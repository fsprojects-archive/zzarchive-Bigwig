/// FSharp extension namespace
namespace FSharp.Control.X_FSharp

open System
open System.Collections
open System.Collections.Generic
open System.Text

/// `nyi - not yet implemented exception
exception NYI of string
exception NotASingleton of string

module List= 

  /// get the singleton if the list contains a single element
  let spick (L:'a list) =
    match L with
    | [x] -> x
    | [] -> raise<|KeyNotFoundException()
    | _ -> raise<|NotASingleton("list contains more than one item")
  /// try to get a singleton from the list of one element.  If it is not a singleton, return None
  let trySPick (L:'a list) =
    match L with
    | [x] -> Some(x)
    | _ -> None

  let distinct (L:'a list) =
    L 
    |> Seq.distinct
    |> Seq.toList

  let distinctBy (projection:'a -> 'Key) (L:'a list) =
    L
    |> Seq.distinctBy(projection)
    |> Seq.toList

  let pickOrFail (msg:string) f S = 
    match List.tryPick f S with
    | Some(x) -> x
    | None -> failwith msg

  let singletonOrFail (msg:string) S = 
    S 
    |> trySPick 
    |> (fun x->
          match x with
            | Some(x) -> x
            | None -> failwith msg
       )

module Seq=

  let cast<'a> (L:'a seq) =
    List.map(fun x->x :> obj :?> 'a)

  let spick (S:'a seq) = 
    S 
    |> Seq.toList
    |> List.spick   
  
  let trySPick (S:'a seq) = 
    S 
    |> Seq.toList
    |> List.trySPick 

  let pickOrFailWith (msg:string) f S = 
    match Seq.tryPick f S with
    | Some(x) -> x
    | None -> failwith msg

  let singletonOrFailWith (msg:string) S = 
    S 
    |> trySPick 
    |> (fun x->
          match x with
            | Some(x) -> x
            | None -> failwith msg
       )

/// miscellaneous functions and conversions.
module Map=
  let fromDict dictionary = 
    (dictionary :> seq<_>)
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq
    
  // http://theburningmonk.com/2012/08/f-converting-a-c-dictionary-to-a-map/

module Dict =
  let add (kv:'Key * 'Value) (d:Dictionary<'Key, 'Value>) = 
    d.Add(kv)
    
  let ContainsKey (k:'Key) (d:Dictionary<'Key, 'Value>) =
    d.ContainsKey(k)

  let remove (k:'Key) (d:Dictionary<'Key, 'Value>) =
    d.Remove(k)

  let tryGet (k:'Key) (d:Dictionary<'Key, 'Value>) = 
    match d.TryGetValue(k) with
    | true,value -> Some(value)
    | false,_ -> None

  let count (d:Dictionary<'Key, 'Value>) =
    d.Count

  let isEmpty (d:Dictionary<'Key, 'Value>) =
    d.Count = 0

  let item (k:'Key) (d:Dictionary<'Key, 'Value>) =
    d.[k]

  /// simple representation of contents
  let toJsonString0 (converter:'Key * 'Value -> string * string) (d:IDictionary<'Key, 'Value>) = 
    seq { for kv in d -> (kv.Key,kv.Value) } 
    |> Seq.map(fun (k,v)->converter(k,v))
    |> (fun S->String.Join(",",S))

  let toBangString0 (converter:'Key * 'Value -> string * string) (d:IDictionary<'Key, 'Value>) = 
    let S = 
      seq { for kv in d -> (kv.Key,kv.Value) } 
      |> Seq.map(fun (k,v)->converter(k,v))
    (S |>  Seq.map(fun (x,y)->x) |> (fun x->String.Join("`",x))) + "!" + (S |> Seq.map(fun (x,y)->y) |> (fun y->String.Join(",",y)))

  let toJsonString (d:IDictionary<'Key, 'Value>) = 
    toJsonString0 (fun (k,v)->(sprintf "%A" k, sprintf "%A" v)) d

  let toString (d:IDictionary<'Key, 'Value>) = 
    toBangString0 (fun (k,v)->(sprintf "%A" k, sprintf "%A" v)) d

module IDict = 
  let tryGet (k:'Key) (d:IDictionary<'Key, 'Value>) = 
    match d.TryGetValue(k) with
    | true,value -> Some(value)
    | false,_ -> None

  /// simple representation of contents
  let toJsonString0 (converter:obj * obj -> string * string) (d:IDictionary) = 
    seq { for k in d.Keys -> k } 
    |> Seq.map(fun k->(k,d.Item(k)))
    |> Seq.map(fun (k,v)->converter(k,v))
    |> (fun S->String.Join(",",S))

  let toJsonString (d:IDictionary) = 
    toJsonString0 (fun (k,v)->(sprintf "%A" k, sprintf "%A" v)) d

  let toBangString0 (converter:'Key * 'Value -> string * string) (d:IDictionary) = 
    let S = 
      seq { for k in d.Keys -> k } 
      |> Seq.map(fun k->(k,d.Item(k)))
      |> Seq.map(fun (k,v)->converter(k,v))
    (S |>  Seq.map(fun (x,y)->x) |> (fun x->String.Join("`",x))) + "!" + (S |> Seq.map(fun (x,y)->y) |> (fun y->String.Join(",",y)))

  let toString (d:IDictionary) = 
    toBangString0 (fun (k,v)->(sprintf "%A" k, sprintf "%A" v)) d

  let fromIDictionary (d:IDictionary) =
    let e = Dictionary<string,obj>() :> IDictionary<string,obj>
    for k in d.Keys do
      e.Add(k:?>string,d.[k])
    e


    
module NsDict =
  let split (nskk:string) = 
    match nskk.LastIndexOf('.') with
    | -1 -> ("",nskk)
    | n -> (nskk.Substring(0,n),nskk.Substring(n+1))

  /// is a namespace key 
  let isnskk (nskk:string) =
    if nskk.Length > 0 then (nskk |> Seq.exists(fun x->x='.')) else false

  /// is a namespace key and variable
  let isNsNvar (nskk:string) =
    if nskk.Length > 0 then (isnskk nskk) && not <| (nskk.[nskk.Length-1] = '.') else false

  /// is a namespace key only
  let isNs (nskk:string) = 
    nskk

  let add (kv:string * 'Value) (nsd:Dictionary<string,Dictionary<string,'Value>>) = 
    let nskk,k=split (fst kv)
    let v = snd kv
    let ns = 
      match Dict.tryGet nskk nsd with
      | Some(ns) -> ns
      | None ->
                let ns = Dictionary<string,'Value>()
                Dict.add (nskk,ns) nsd
                ns
    Dict.add (k,v) ns

  let ContainsKey (k:string) (nsd:Dictionary<string,Dictionary<string,'Value>>) =
    let nskk,k=split k
    match Dict.tryGet nskk nsd with
    | Some(ns) -> Dict.ContainsKey k ns
    | None -> false

  let remove (k:string) (nsd:Dictionary<string,Dictionary<string,'Value>>) =
    let nskk,k=split k
    match Dict.tryGet nskk nsd with
    | Some(ns) -> Dict.remove k ns
    | None -> false

  let tryGet (k:string) (nsd:Dictionary<string,Dictionary<string,'Value>>) = 
    let nskk,k=split k
    match Dict.tryGet nskk nsd with
    | Some(ns) -> Dict.tryGet k ns
    | None -> None

  let tryGetNs (nskk:string) (nsd:Dictionary<string, Dictionary<string,'Value>>) = 
    Dict.tryGet nskk nsd 

  let count (nsd:Dictionary<string,Dictionary<string,'Value>>) =
    nsd.Count

  let countNs (nskk:string) (nsd:Dictionary<string, Dictionary<string,'Value>>) =
    match Dict.tryGet nskk nsd with
    | Some(ns) -> Dict.count ns
    | None -> raise <| KeyNotFoundException(sprintf "couldn't find namespace '%s'" nskk)

  let isEmpty (nsd:Dictionary<string,Dictionary<string,'Value>>) =
    Dict.count nsd = 0

  let item (k:string) (nsd:Dictionary<string,Dictionary<string,'Value>>) =
    let nskk,k=split k
    match Dict.tryGet nskk nsd with
    | Some(ns) -> Dict.item k ns
    | None -> raise <| KeyNotFoundException(sprintf "couldn't find namespace '%s'" nskk)

  let itemNs (nskk:string) (nsd:Dictionary<string,Dictionary<string,'Value>>) =
    match Dict.tryGet nskk nsd with
    | Some(ns) -> ns
    | None -> raise <| KeyNotFoundException(sprintf "couldn't find namespace '%s'" nskk)

  /// get the variable, or the dictionary depending on whether the nskk ends in a dot or not.
  /// a.b.c. returns the contents of the dictionary at that address
  /// a.b.c.d returns the contents of a variable.
  let item1 (nskk:string) (nsd:Dictionary<string,Dictionary<string,'Value>>) =
    if isNsNvar nskk then
      let nskk1,vk = split nskk
      [(vk,(item nskk nsd) :> obj :?> 'Value)] // values are always returned in a single element list
    else
      let nskk1,vk = split nskk
      (itemNs nskk1 nsd)
      |> Seq.map(fun (x:KeyValuePair<string,'Value>)->(x.Key,x.Value))
      |> Seq.toList

  /// ensures that even duplicates get added.  
  /// take the k part of kv and treat as a complete namespace
  let add1 (kv:string * 'Value) (nsd:Dictionary<string,Dictionary<string,'Value>>) = 
    let nskk,v = kv
    let nskk,k = split nskk
    let k=String.IsNullOrEmpty k
          |>(function|false->k|true->"")
          |>(fun k->ContainsKey nskk nsd 
                  |>(function
                  |true->k+"-"+Guid.NewGuid().ToString()
                  |false->k))
    add (nskk + "." + k,v) nsd

  let upsert (kv:string * 'Value) (nsd:Dictionary<string,Dictionary<string,'Value>>) = 
    let nskk,v = kv
    let nskk,k = split nskk
    if ContainsKey nskk nsd then
      remove nskk nsd |> ignore
    add (nskk + "." + k,v) nsd
    

  // check if variable already exists
  //let addDup

// Q) should a namespace dictionary be more generic so that the namespace could be any equatable object?
// A) No.  That's a different kind of structure.

