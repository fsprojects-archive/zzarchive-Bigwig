namespace bigwig.X_RabbitMQ

open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Reflection
open System.Diagnostics
open bigwig.X_RabbitMQ
open bigwig.X_FSharp
open RabbitMQ.Client

open Microsoft.FSharp.Reflection

module DefMatch = 

  let (|IsSome|_|) (x:string) = String.IsNullOrEmpty(x) |> (function| false->Some(x) | true->None)

  let setINskk (container:obj) (meta:obj) =
    match meta with
    | :? INskk as INskk -> 
        if String.IsNullOrEmpty INskk.Nskk then
          INskk.Nskk<-NS.getClass container
    | _ -> ()

  let setINskk1 (container:Type) (meta:obj) =
    match meta with
    | :? INskk as INskk -> 
        if String.IsNullOrEmpty INskk.Nskk then
          INskk.Nskk<-NS.getClass1 container
    | _ -> ()

  let O = [typedefof<IOpenDef0<_>>;typedefof<IOpenDef1<_>>;typedefof<IOpenDef2<_>>;typedefof<IOpenDef3<_>>;typedefof<IOpenDef4<_>>;typedefof<IOpenDef5<_>>;typedefof<IOpenDef6<_>>;typedefof<IOpenDef7<_>>;typedefof<IOpenDef8<_>>;typedefof<IOpenDef9<_>>]
  /// add current fromT type to the list of IOpenDef types via the choose marker
  let chooseODefTypes fromT = 
        (List.append [fromT] (Interface.chooseMarker (O,fromT)))

  let chooseOdAttribs0 (T:Type list) =
    T|>List.map(fun x->x.GetCustomAttributes(true)|>Array.map(fun x->x:?>Attribute)|>Array.toList)|>List.concat

  let chooseOdAttribs (fromT) =
    chooseODefTypes fromT |> chooseOdAttribs0 

  let filterAttrib A =
    A |> List.filter(fun x->not<|(x.GetType().Namespace.Contains("Microsoft.FSharp.Core")||x.GetType().Namespace.Contains("System")))

  let filterAsm asm (from : 'a list) =
    from |> List.filter(fun x->x.GetType().Assembly=asm)

  let getIMatchGrp (x:obj) =
    [
     (match x with | :? IMatch0Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch1Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch2Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch3Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch4Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch5Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch6Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch7Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch8Grp as md->Some <| md.Grp | _->None);
     (match x with | :? IMatch9Grp as md->Some <| md.Grp | _->None)
    ] |> List.choose(fun x->x)

  let getIMatchWhen (x:obj) =
    [
     (match x with | :? IMatch0When as md->Some <| md.When | _->None);
     (match x with | :? IMatch1When as md->Some <| md.When | _->None);
     (match x with | :? IMatch2When as md->Some <| md.When | _->None);
     (match x with | :? IMatch3When as md->Some <| md.When | _->None);
     (match x with | :? IMatch4When as md->Some <| md.When | _->None);
     (match x with | :? IMatch5When as md->Some <| md.When | _->None);
     (match x with | :? IMatch6When as md->Some <| md.When | _->None);
     (match x with | :? IMatch7When as md->Some <| md.When | _->None);
     (match x with | :? IMatch8When as md->Some <| md.When | _->None);
     (match x with | :? IMatch9When as md->Some <| md.When | _->None)
    ] |> List.choose(fun x->x)

  /// list of wild cards to regex
  let listWCToRegex searchStr =
    searchStr 
    |> List.map(fun x->Parse.toWildcarded x)
    |> (function 
        | [] -> None 
        | L  -> Some(
                    L |> Seq.distinct |> Seq.toList
                    |> List.map(fun x->
                                       match x with 
                                       | "" -> "(^$|`)" 
                                       | x -> x) 
                    |> (fun x->String.Join("|",x))
                    |> (fun s->Regex(s)))
        )

  /// needs both MatchDef Attributes, and also those defined via interfaces
  /// we merge these definitions here and OR them together into two regex statements, which in turn must both be valid (AND) 
  /// to yield a match
  let makeRegEx1 (searchSpec:string) =
    let KV = searchSpec |> Parse.fromQKVs |> Seq.distinct |> Seq.toList
    let G_RegX = 
      KV 
      |> List.map(fun x->match fst x with | "g" | "grp" | "" -> snd x |> Array.toList | _ -> [] )|> List.concat
      |> listWCToRegex

    let W_RegX = 
      KV 
      |> List.map(fun x->match fst x with | "w" | "when" -> snd x |> Array.toList | _ -> [])|> List.concat
      |> listWCToRegex

    match (G_RegX,W_RegX) with
    | None,None -> failwith "searchSpec: '%s' should not result in an empty search pattern.  Did you tag it correctly as (g,grp) or (n,nme,m,mq)?" searchSpec
    | x -> x

  /// find search specs with other search specs
  let makeRegEx1b (searchSpec:string) (MD:MatchDef list) =

    let gs = 
      match MD|>List.choose(fun x->x.Grp |> (function | IsSome(x) -> Some(x) | _ -> None)) with
      | [] -> None
      | x -> Some(String.Join("g:", x))
    let ws = 
      match MD|>List.choose(fun x->x.When |> (function | IsSome(x) -> Some(x) | _ -> None)) with
      | [] -> None
      | x -> Some(String.Join("ws:", x))

    let s = String.Join(";", [gs;ws] |> List.choose(fun x->x))

    makeRegEx1 s

  /// use search spec to find other search specs that are defined in the object "o" via interface or meta attribute type 
  let makeRegEx2 (searchSpec:string) (o:obj) =

    let Agrp = getIMatchGrp(o) |> List.map(fun x->MatchDef("",x):>Attribute)
    let Awh = getIMatchWhen(o) |> List.map(fun x->MatchDef(x,""):>Attribute)

    let MD=
      o.GetType().GetCustomAttributes(true)
      |>Array.toList
      |>List.map(fun x->x:?>Attribute)
      |>(List.append Agrp)
      |>(List.append Awh)
      |>List.choose(function| :? MatchDef as md->Some(md)| _->None)

    makeRegEx1b searchSpec MD

  let makeRegEx3 (searchSpec:string) (typ:Type) =
    let MD=
      typ.GetCustomAttributes(true)
      |>Array.toList
      |>List.map(fun x->x:?>Attribute)
      |>List.choose(function| :? MatchDef as md->Some(md)| _->None)

    makeRegEx1b searchSpec MD

  type SearchStyle =
  /// include when no interface
  | IncludeNoDef
  /// exclude when no interface
  | ExcludeNoDef

  let inline whenNoIDef style =
    style|>(function | IncludeNoDef -> true | ExcludeNoDef -> false)

  let someRegExIsMatch (r:Regex option) (s:string) =
    match r with
    | None -> false
    | Some(regX) -> regX.IsMatch s

  let choose0 (grx: Regex option) (whrx: Regex option) (L:Attribute list) = 
    L 
    |> List.filter(fun x->                
                      let a=
                        match x:>obj with 
                        | :? IMatchGrp  as g -> someRegExIsMatch grx g.Grp  
                        | _ -> false 
                      let b=
                        match x:>obj with 
                        | :? IMatchWhen as w -> someRegExIsMatch whrx w.When 
                        | _ -> false
                      
                      match grx,whrx with
                      | Some _, Some _ -> a && b
                      | Some _, None -> a
                      | None, Some _ -> b
                      | None, None -> failwith "no search spec provided")

  /// currentNs: current namespace
  /// MD: metta definition attribute list
  /// L: normal attribute list
  /// searchSpec: a spec to select attributes
  let choose (searchSpec:string) (L:Attribute list) =
    let g_regX,w_regX = makeRegEx1 searchSpec
    choose0 g_regX w_regX L

  let choose1 (searchSpec:string) (L:Attribute list) =

    let GrpRegX,WhenRegX = makeRegEx1 searchSpec 

    let R = choose0 GrpRegX WhenRegX L
    R

  /// this is the place where we find a reply-to address based on a specified target object and channel
  let chooseIReplyToT = [typedefof<IReply0>;typedefof<IReply1>;typedefof<IReply2>;typedefof<IReply3>;typedefof<IReply4>;typedefof<IReply5>;typedefof<IReply6>;typedefof<IReply7>;typedefof<IReply8>;typedefof<IReply9>]
  let chooseReplyTo1 (o) = 
    chooseIReplyToT
    |>List.map(fun t->match Interface.tryPickData0(t,o) with | Some(v) -> Some(X_ReplyTo1(FSharpValue.GetTupleFields(v))) | None -> None)
    |>List.choose(fun x->x)

  /// use qlist format grp or when to find meta definitions.  
  /// Also, augment with ReplyTo, as we have the obj reference
  /// currently, a bunch of synonmous tags are defined for
  /// grp: g, grp
  /// when: w, when
  let choose2 (searchSpec:string) (fromObj:obj) =
    fromObj.GetType()
    |> chooseODefTypes
    |> List.map(fun x->chooseOdAttribs x)
    |> List.concat
    |> choose searchSpec
            

  let toSearchSpec (rt:X_ReplyTo1) : string =
    ( (rt.When |> function | IsSome(wh) -> [wh] | _ -> [])
     ,(rt.Grp |> function | IsSome(grp) -> [grp] | _ -> []) )
    ||> List.append      
    |> fun L->String.Join(",",L)

  /// apply searchSpec to walked open def attributes
  let chooseReplyTo (searchSpec:string) (typ:Type) = 
    let GrpRegX,WhenRegX = makeRegEx3 searchSpec typ
    let L = chooseOdAttribs(typ)
            |> List.map (fun t->t.ChooseCustom([typedefof<Attribute>]))
            |> List.concat
            |> List.choose(function | :? MQDef as x->Some x | _ -> None)
            |> List.map(fun x->x:>Attribute)
            |> choose searchSpec
    choose0 GrpRegX WhenRegX L

  /// preconditions: L comes from the current this.  Headers may be provided if we are receiving a message
  let chooseReplyTo2 searchSpec (L:Attribute list) (headers:IBasicProperties option) =
    let inline getFromHdrs (h:IBasicProperties option) = 
      h |> (function | None->[] | Some x->x.IsReplyToPresent() |> (function | false->[] | true->[MQDef(x.ReplyToAddress.ExchangeName,x.ReplyToAddress.RoutingKey):>Attribute]))
    L
    |>List.map(fun x->
                        match x with
                        | :? X_ReplyTo1 as replyTo->
                              match replyTo.Type with
                              | t when t = typedefof<This> -> L |> List.filter(function | :? MQDef -> true | _ -> false)
                              | t when t = typedefof<AddrInMsg> -> getFromHdrs headers |> (function | [] -> failwith "no replyTo definition in received message" | x -> x)
                              | t -> chooseReplyTo searchSpec t
                        | _ -> []
                        )
    |> List.concat
    |> choose1 searchSpec

// attribute groups: ccdef, cxdef, mqdef

// TODO: include attributes on methods and functions

// TODO: blend interface and attributed match statements
// TODO: blend interface and attributed OpenDef statements

// get attributes and also filter out the ones we don't need or want.
// do not worry about the fully recursive definition that might take us to the Register

// TODO: add register lookup?
 
// our match tags are not necessarilly comming from the same place as attribute list

// a selection of groups makes a channel *OR* [original plan] a regex applied to when statements makes a channel
// syntax of g:name (means group), otherwise, it has to be a queue pattern??
// remember that MatchDefs always default to the currentNamespace when they are left undefined
// should group statements

// intersection of Group and MQDefs is (with empty group not a match, and 
// match list: mq: `, some.*.queue; g: `
// mq -> resolves to the current namespace
// some.*.queue -> resolves to regex
// g: ` resolves to not defined - ie. `,"",or null

// we do not search based on namespace keys.  ns -> queue name + wildcard when mq equals empty or `.  So, we ignore INskk._Nskk
// INskk._Nskk comes into play with Register + real-time updated meta should we choose to do this (Clojure may offer some clues about how to do this properly with some of the default bi-temporal structures that that language provides)

// TODO: consider changing attribute lists to arrays 
// TODO: would openDefs work well as attributes?

