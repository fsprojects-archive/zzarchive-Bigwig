namespace bigwig.X_RabbitMQ

open System
open bigwig.X_FSharp
open bigwig.X_RabbitMQ
open RabbitMQ.Client
open System.Collections.Generic
open bigwig.X_RabbitMQ
open bigwig.X_FSharp

module Nskk =
  // TODO: add nskk based searchs to the searchSpec format
  let filter (withTag:string option) (L) = 
    let matcher =
      match withTag with
      | None -> None
      | Some(tag) -> Some(Parse.getWildcardSearch tag)
    L |> List.filter(fun x->match x:>obj with
                            | :? INskk as INskk->matcher|>(function | Some(matcher2) -> matcher2.IsMatch INskk.Nskk | None -> true)
                            | _ -> false)

/// nme: Context Helper
/// def: A context is defined to be any given point in space and/or time. 
/// We add clones, federation, and execution context to this definition of space and/or time.
/// I do not have a mutually exclusive definition of where one ends and another starts suffice it to say
/// that every or element has a tendency to replicate the features of an entire operating system.
///
/// use System.Diagnostics.StackTrace() to get the stacktrace for the current thread.
module DefList =

  let filterIs (ofT:Type) A =
    A|>Array.filter(fun x->ofT.Is1 x)

  /// dsc: pick one element from of type 'ofT from L.
  /// a withTag of None means to ignore the withTag filter and return all elements 
  /// in this instance all elements of type 'ofT must equate to a singleton
  let pickDef<'ofT when 'ofT :> Attribute>(searchSpec : string) (L : Attribute list) =
    L 
    |> TypeList.choose1<'ofT,Attribute> 
    |> List.map(fun x->x :> Attribute)
    |> DefMatch.choose searchSpec
    |> List.map(fun x->x :?> 'ofT)
    |> List.pick(fun x->Some(x))

  /// dsc: try to pick a singleton element, but return None if return is empty (or contains more than one element).
  /// a withTag of None means to ignore the withTag filter and return all elements 
  /// in this instance all elements of type 'ofT must equate to a singleton
  let trySPick<'ofT when 'ofT :> Attribute>(searchSpec : string) (L : Attribute list) =
    L 
    |> TypeList.choose1<'ofT,Attribute> 
    |> List.map(fun x->x :> Attribute)
    |> DefMatch.choose searchSpec
    |> List.map(fun x->x :?> 'ofT)
    |> List.trySPick 

  let spick<'ofT when 'ofT :> Attribute>(searchSpec : string) (L : Attribute list) =
    L 
    |> TypeList.choose1<'ofT,Attribute> 
    |> List.map(fun x->x :> Attribute)
    |> DefMatch.choose searchSpec
    |> List.map(fun x->x :?> 'ofT)
    |> List.spick

  /// dsc: pick n items 'ofT from L
  /// a withTag of None means to ignore the withTag filter and return all elements 
  /// in this instance all elements of type 'ofT must equate to a singleton
  let choose<'ofT when 'ofT :> Attribute>(searchSpec : string) (L : Attribute list) =
    L 
    |> TypeList.choose1<'ofT,Attribute>
    |> List.map(fun x->x :> Attribute)
    |> DefMatch.choose searchSpec
    |> List.map(fun x->x :?> 'ofT)

module AttribList = 
  let fromExtension (o:IAmqpExtension) =
    (o.X_Pairs |> List.map(fun x->x:>Attribute)) 
    @  (o.X_Refs 
    |> Parse.fromQList 
    |> List.map(fun x->NsDict.item x Register.ns))

module DefRT =
  open DefMatch
  open System.Diagnostics
  open System.Reflection

  /// dsc: get run-time context definitions
  let choose (stack:StackTrace) (searchSpec : string) =
    (stack.GetFrame(0).GetMethod() :?> MethodInfo).GetCustomAttributes(true) |> Array.map(fun x->x :?> Attribute)
    |> Array.toList
    |> DefMatch.choose searchSpec
    |> List.map(fun x->x :?> 'ofT)

  /// dsc: get all run-time context definitions
  let chooseAll (stack:StackTrace) (searchSpec : string) =
    let m= stack.GetFrame(0).GetMethod():?>MethodInfo
    let L= m.GetCustomAttributes(true) |> Array.map(fun x->x :?> Attribute) |> Array.toList
    let dtyp= stack.GetFrame(0).GetMethod().DeclaringType
    let L2= dtyp.GetCustomAttributes(true) |> Array.map(fun x->x :?> Attribute) |> Array.toList
    List.append L L2
    |> DefMatch.choose searchSpec
    |> List.map(fun x->x :?> 'ofT)

  /// dsc: gets the current execution contexts methodInfo (which contains additional information)
  let getMethod (stack:StackTrace) =
    stack.GetFrame(0).GetMethod():?>MethodInfo

  /// dsc: gets the current execution context's method name
  let getMethodName (stack:StackTrace) =
    (stack |> getMethod).Name

  let getNsk (stack:StackTrace) =
    (stack |> getMethod).DeclaringType.Namespace


/// resolve definition references
module DefRef =

  let grabDefaultExchange (defaults:PubSubContext option) = 
    match defaults with
    | Some(defaults) ->
        match defaults.ExchangeDefault with
        | null           -> None
        | exchange -> Some(exchange)
    | None -> None

  // dict, publr, consr, xpair, cx, cc
  
  // tags only refer to the first item in the list, and the first layer ie. the T layer

  let getNskkInfo (A:obj) =
    if X_Type.Is1<INskk> A  then
      let INskk = A :?> INskk
      (INskk.Nskk)
    else
      ("")   
  
  let hasRefInfo (A:obj) =
    typedefof<INskk>.Is1 A

  let trygetNskkInfo (A:obj) =
    if typedefof<INskk>.Is1 A  then
      Some(A :?> INskk)
    else
      None

  let emptyLocalSet  = Dictionary<string,Dictionary<string,Attribute>>()


  /// make namespace dictionary from a list of attributes
  /// items that do not have a namespace defined are put in the top-level domain
  let makeNamespaceA (A:Attribute list) =
    let nsd=Dictionary<string,Dictionary<string,Attribute>>()
    A |> List.iter(fun v-> if typedefof<INskk>.Is1 v then
                              let nskk = getNskkInfo(v)
                              NsDict.add (nskk,v) nsd)
    nsd

/// nme: define helper
/// dsc: maps BasicProperties to Attributes and vice versea
module PackXPair =
   
  /// dsc: pack carbon copy xpair in basic properties
  let _cc (cc : XsPair list) (bp : IBasicProperties) =
    let RP = 
      cc        
      |> List.map(fun x->
                if Parse.isQList x.Val then
                  RoutingPair.fromQKVs x.Val
                else
                  [RoutingPair("",x.Val)]
                )
      |> List.concat
    bp.Headers.Add("cc", RP |> List.map(fun x->x.routingKey) |> List.toArray)

  /// dsc: pack blind carbon copy in basic properties
  let bcc (bcc : XsPair list) (bp : IBasicProperties) =
    let RP = 
      bcc        
      |> List.map(fun x->
                if Parse.isQList x.Val then
                  RoutingPair.fromQKVs x.Val
                else
                  [RoutingPair("",x.Val)]
                )
      |> List.concat
    bp.Headers.Add("bcc", RP |> List.map(fun x->x.routingKey) |> List.toArray)

  /// dsc: create basic properties for a message to be published and pack it with XPairs
  let createPubMsgBP (XL : XPair list) (cc : IModel) =
    let bp = cc.CreateBasicProperties()
    _cc (XL |> List.choose(fun x->
                        match x with
                        | :? X_CC as cc -> Some(cc :> XsPair)
                        | _-> None)
           ) bp
    bcc (XL |> List.choose(fun x->
                    match x with
                    | :? X_BCC as cc -> Some(cc :> XsPair)
                    | _-> None)
            ) bp
    XL |> List.iter(fun x->match x with
                           | :? X_CC  -> ()
                           | :? X_BCC -> ()
                           //| :? X_PerMsgTTL as x -> bp.Expiration <- x.Val.ToString()
                           | :? X_TimeStamp as x -> bp.Timestamp <- x.toAmqpTimestamp()
                           | x->bp.Headers.Add(x.Key,x.oVal)
                   )
    bp

  /// dsc: the x-match parameter is for a headers exchange.
  /// We publish into it with certain keys, and then a subscriber can listen for it.
  /// For example, a consumer could listen for real-time notification for particular stocks
  /// create basic properties for subscriber bindings and pack it with XPairs
  let createSubBindingsBP (XL : XPair list) (cc : IModel) =
    let bp = cc.CreateBasicProperties()
    XL |> List.iter(fun x->bp.Headers.Add(x.Key,x.oVal))
    bp

  /// dsc: create exchange basic properties
  let createExchangeBP (XL : XPair list) (cc : IModel) =
    let bp = cc.CreateBasicProperties()
    XL |> List.iter(fun x->bp.Headers.Add(x.Key,x.oVal))
    bp

  /// dsc: create queue basic properties
  let createQueueBP (XL : XPair list) (cc : IModel) =
    let bp = cc.CreateBasicProperties()
    XL |> List.iter(fun x->match x with
                           | :? X_PerQueueMsgTTL as x->bp.Expiration<-x.Val.ToString()
                           | x->bp.Headers.Add(x.Key,x.oVal))
    bp



  /// definition to xpair
  /// assum. : we walk a common base type or interface defining a tree-structure via recursion
  ///             or more specifically 'ret will be some kind of attribute where each element may in turn be followed
  /// mf     : mapping function
  /// 'ret   : the return value from the mf function must be homogenous in some key way
  /// levels : the number of steps that the method may take.  It stops when it gets to zero.  For infinite steps, put in a value of -1
  /// key    : L: local; r: range (as in domain->range); S: Step Into Recursively; nskk: namespace-key-key (ie. a full variable name such as a.b.c.someVariableName)
  ///          R: Registrar
  /// PRECONDITIONS:
  /// • put all attributes in localSet
  /// • filter canditates withTag
  /// • one by one call this function with a multiply candidate
  /// NOTE: ff can be the same as ff

  // Q) how do we specify to grab everything in a namespace?
