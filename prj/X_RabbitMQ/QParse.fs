namespace FSharp.Control.RabbitMQ

open System
open System.Text
open System.Text.RegularExpressions
open FSharp.Control.X_FSharp

module Parse =

  let isQList (x:string) =
    x.Contains("`") || x.Contains(";") || x.Contains(",") || x.Contains(":")

  let isMatchingPattern (x:string) =
    x.Contains("#") || x.Contains("*")

  let hasASymbolLeftOfAssign (x:string) =
    let tmp=x.Trim()
    x.Contains(":") && x.Length>0 && x.[0] <> ':' 

  let toWildcarded (s:string) = 
    s.Replace(".","\.").Replace("*",".*").Replace("?",".*?")  // TODO: is this correct for a question mark?

  let getWildcardSearch0 (L:string list) =
    let sb = StringBuilder()
    L|>List.iteri(fun i x-> if i>0 then sb.Append('|') |> ignore
                            sb.Append(toWildcarded x) |> ignore)
    Regex(sb.ToString())

  let fromQList (x:string) =
    x.Split([|',';'`';'`';' '|],StringSplitOptions.None) 
    |> Seq.distinct
    |> Seq.toList

  let getWildcardSearch (pattern:string) =
    if String.IsNullOrEmpty(pattern) then
      Regex("(^$)")
    else
      let L = fromQList pattern
      getWildcardSearch0 L

  /// nme: parse Q-style key-value lists
  /// dsc: takes a string in a list of statements where each statement is assigning a list to a variable.
  /// or, just a list by itself.
  /// Each element of the list may contain a tag (ie. a ?)  This tuple format is similar to a query string in a Uri.  RabbitMQ also adopts query strings in the format, so we use this Uri convention to add an optional third element to form a tripple)
  /// The tag is used as a thing to select on later
  /// The result is always a nested 
  /// eg. fromQKVs "ax:a,b,c;bx:d,e,f;jx:x,y,z"
  /// eg. fromQKVs "ax:a`b`c,bx:d`e`f,jx:x`y`z,`imm:1,`mand:1,`ttl:60"
  let fromQKVs (x:string) = 
    x.Split([|';'|],StringSplitOptions.RemoveEmptyEntries) 
    |> Array.map(fun x->x.Trim())
    |> Array.map(fun x->
                  let v=x.Split([|':';'/'|],StringSplitOptions.None) |> Array.map(fun (x:string)->x.Trim())  // ie. this will accept "exchange://some.queue.name" as well now.
                  let items = 
                    (if v.Length = 1 then v.[0] else v.[1]).Split([|',';'`';' '|],StringSplitOptions.None)
                    |> Array.map(fun x->(if x.StartsWith("`") then x.Substring(1) else x))
                    |> Seq.distinct
                    |> Seq.toArray
                  if v.Length = 1 then 
                    if hasASymbolLeftOfAssign(x) then
                      (items.[0],[||]) 
                    else
                      ("",items) 
                  else  
                    (v.[0],items)
                )
    // TODO: filter out keys beginning with `

    // TODO: check taht fromQKVs works correctly


    // TODO: pull out string specified attributes
    // note that attributes like `mand could have been specified with the long format
