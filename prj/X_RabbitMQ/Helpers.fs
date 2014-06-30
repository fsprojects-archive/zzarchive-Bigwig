namespace bigwig.X_RabbitMQ.Meta

open System
open System.Collections.Generic
open RabbitMQ.Client
open bigwig.X_FSharp
open bigwig.X_RabbitMQ
open DefRef

module Bag = 
  let upsert (bag0:Attribute seq) (item:Attribute) =
    let added = ref false
    seq 
      {
        for bagItem in bag0 do
          if bagItem.GetType() = item.GetType() then
            yield item
            added := true
          else 
            yield bagItem
        if not !added then
          yield item
      } 

  let upsertn (bag0:Attribute seq) (items:Attribute list) =
    let mutable bag = bag0
    for item in items do
      bag <- upsert bag item
    bag

  let remove<'a> (bag0:Attribute seq) =
    let typ = typedefof<'a>
    seq {
      for item in bag0 do
        if typ <> item.GetType() then
          yield item
    }

module Helper = 

  /// dsc: create RabbitMQ publication address from string
  let MakeRabbitPAdd0 (exTyp:ExchangeType) (exKey:string) (routingKey:string) = 
    RabbitMQ.Client.PublicationAddress(exTyp.toString(),exKey,routingKey)

  /// dsc: Get PublicationAddress from RoutingPairs and a selector tag
  let RPsToPAddr (routingPairs:RoutingPair list) (tag:string option) =
    let routeTo = 
      match tag with
      | None -> 
          routingPairs |> List.spick
      | Some(tag) ->
          routingPairs |> List.choose(function|x->if x.tag = tag || x.routingKey.EndsWith(tag) then Some(x) else None) |> List.spick
    routeTo

  /// dsc: routing pair to address
  let RPsToAddr1 (routingPairs:RoutingPair list) (tag:string option) =
    let routeTo = RPsToPAddr routingPairs tag
    let _type = 
      match Register.getExchange <| Some routeTo.exchange with
      | None -> raise<|bigwig.X_RabbitMQ.NoSuchExchangeInRegister(sprintf "can not find exchange '%s' in the register" routeTo.exchange)
      | Some exch->exch._type
    RabbitMQ.Client.PublicationAddress(_type.toString(), routeTo.exchange, routeTo.routingKey)

  /// dsc: QList to address
  let QLtoAddr (qList:string) (tag:string option) =
    let RP = RoutingPair.fromQKVs qList 
    RPsToAddr1 RP tag

  /// dsc: queue meta to address
  let QMtoAddr (mq: MQDef) (tag: string option) =
    let RP = mq.GetRoutingPairs()
    RPsToAddr1 RP tag

  /// we keep this scalar for now
  /// glean whatever type of information you want to draw out of this walkthrough.
  [<Obsolete("this can be revisited.  It's more efficient than pick or choose, but caching would be better .")>]
  let mfGet (a:Attribute) = 
    let nskk = trygetNskkInfo a|>(function|Some(x)->(x.Nskk)|_->(""))
    match nskk with
    | null
    | ""                       -> []
    | _ ->
        match a with
        | :? XPair             -> [(nskk,a)]
        | :? PubDef   as publr -> [(nskk,publr:>Attribute)]
        | :? ConsrDef as consr -> [(nskk,consr:>Attribute)]
        | :? CcDef    as cc    -> [(nskk,cc:>Attribute)]
        | :? CxDef    as cx    -> [(nskk,cx:>Attribute)]
        | :? MQDef    as mq    -> [(nskk,mq:>Attribute)]
        | :? RoutingPair       as rk    -> [(nskk,rk:>Attribute)]
        | :? ExDef    as ex    -> [(nskk,ex:>Attribute)]
        | _ -> []
    // three types of output - BasicProperties, RoutingPairs, or ContextHelpers

  let ffGetAll (a:Attribute) = 
    let nskk = trygetNskkInfo a|>(function|Some(x)->(x.Nskk)|_->(""))    
    match nskk, hasRefInfo a with
    | _,false   -> []
    | nskk,true -> 
          match a with 
          |_ -> [(nskk,a)]

  let ffGetConsr (a:Attribute) = 
    let nskk = trygetNskkInfo a|>(function|Some(x)->(x.Nskk)|_->(""))    
    match nskk, hasRefInfo a with
    | _,false   -> []
    | nskk,true -> 
          match a with 
          | :? XPair | :? ConsrDef | :? MQDef | :? RoutingPair | :? ExDef -> [(nskk,a)]
          | _ -> []

  let ffGetPublr (a:Attribute) = 
    let nskk = trygetNskkInfo a|>(function|Some(x)->(x.Nskk)|_->(""))    
    match nskk, hasRefInfo a with
    | _,false   -> []
    | nskk,true -> 
          match a with 
          | :? XPair | :? PubDef | :? MQDef | :? RoutingPair | :? ExDef -> [(nskk,a)]
          | _ -> []

  let mfGetRoutingPairs (a:Attribute) : (string * Attribute) list = 
    let nskk = trygetNskkInfo a|>(function|Some(x)->(x.Nskk)|_->(""))
    match a with
    | :? MQDef    as mq    -> mq.GetRoutingPairs() |> List.map(fun rk->(nskk,rk:>Attribute))
    | :? RoutingPair       as rk    -> [(nskk,rk:>Attribute)] 
    | _ -> []

  let mfGetPublrRK (a:Attribute) : (string * Attribute) list = 
    let nskk = trygetNskkInfo a|>(function|Some(x)->(x.Nskk)|_->(""))
    match a with
    | :? PubDef   as publr -> [(nskk,RoutingPair(publr.exchange,publr.routingKey):>Attribute)]
    | :? MQDef    as mq    -> mq.GetRoutingPairs() |> List.map(fun rk->(nskk,rk:>Attribute))
    | :? RoutingPair       as rk    -> [(nskk,rk:>Attribute)] 
    | _ -> []

  let mfGetConsrRK (a:Attribute) : (string * Attribute) list = 
    let nskk = trygetNskkInfo a|>(function|Some(x)->(x.Nskk)|_->(""))
    match a with
    | :? ConsrDef as consr -> [(nskk,consr:>Attribute)]
    | :? MQDef    as mq    -> mq.GetRoutingPairs() |> List.map(fun rk->(nskk,rk:>Attribute))
    | :? RoutingPair       as rk    -> [(nskk,rk:>Attribute)] 
    | _ -> []

  let mfConsr (a:Attribute) : (string * Attribute) list = 
    []

  let defToXPairT (tag:string) (container:Type) =
    // what if the tag is a full nsk?
    ()

open System
open System.Collections.Generic
open RabbitMQ.Client
open bigwig.X_FSharp
open bigwig.X_RabbitMQ

module BasicProperties =

  let dictToXPair (kvs:IDictionary<string,obj>) =
    seq { 
      for kv in kvs do
          match (kv.Key,kv.Value) with
          | "alternate-exchange",value -> yield X_AlternateExchange(value:?>string) :> XPair
          | "x-message-ttl",value -> yield X_PerMsgTTL(value :?> int64) :> XPair
          //| "x-message-ttl-per-queue",value -> yield X_PerQueueMsgTTL(value:?>string) :> XPair
          | "x-expires",value -> yield X_QueueTTL(value :?> int64) :> XPair
          | "x-dead-letter-exchange",value -> yield X_DeadLetterExchange(value:?>string) :> XPair
          | "x-dead-letter-exchange-routing-key",value -> yield X_DeadLetterExchangeRoutingKey(value:?>string) :> XPair
          | "x-max-length",value -> yield X_MaxLength(value:?>Int32) :> XPair
          | "user-id",value -> yield X_UserId(value:?>string) :> XPair
          | "trust-user-id",value -> yield X_TrustUserId(value:?>string) :> XPair
          //| "application-type",value -> yield X_ApplicationType(value:?>string) :> XPair
          | "cc",value -> yield X_CC(value:?>string) :> XPair
          | "bcc",value -> yield X_BCC(value:?>string) :> XPair
          | "charset",value -> yield X_Charset(value:?>string) :> XPair
          | "encoding-type",value -> yield X_Encoding(value:?>string) :> XPair
          | "content-type",value -> yield X_Content(value:?>string) :> XPair
          | "correlation-id",value -> yield X_CorrelationId(value:?>string) :> XPair
          | "priority",value -> yield X_Priority(value:?>Byte) :> XPair
          | "reply-to",value -> yield X_ReplyTo(value:?>string) :> XPair
          //| "message-id",value -> yield X_MessageId(value:?>string) :> XPair
          | "timestamp",value -> yield X_TimeStamp(value:?>int64) :> XPair
          | "app-id",value -> yield X_AppId(value:?>string) :> XPair
          //| "expiration",value -> yield X_Expiration(value:?>int64) :> XPair
          | "basic.type",value ->  yield X_BasicType(value:?>string) :> XPair
          | "x-match",value ->  yield X_Match(value:?>string) :> XPair
          | "x-reason",value -> yield X_Reason(value:?>string) :> XPair
          | "x-prefetch-count",value -> yield X_PrefetchCount(value:?>string) :> XPair
          | "x-ack-mode",value -> yield X_AckMode(value:?>string) :> XPair
          | "x-delivery-mode",value -> yield X_DeliveryMode(value:?>string) :> XPair
          | "x-reconnect-delay",value -> yield X_ReconnectDelay(value:?>string) :> XPair
          | k,v -> yield X_Argument(k,v) :> XPair
        }

  /// nme: convert basic properties to XPairs
  let toXPair (bp:IBasicProperties) =
    dictToXPair bp.Headers
    |> Seq.toList

  /// from attributes to a communication channel
  let fromA(A:Attribute list) (cc:IModel) =
    for a in A do
        match a with
        | :? QosDef as qos ->
                cc.BasicQos(qos.prefetchSize,qos.prefetchCount,qos._global)
        | _ -> ()

module Cc =

  /// dsc: sets the channel's quality of service
  let setQos (qosDef:QosDef) (cc:IModel) =
    cc.BasicQos(qosDef.prefetchSize, qosDef.prefetchCount, qosDef._global)

  /// attributes to communication channels
  let fromA(A:Attribute list) (cc:IModel) =
    for a in A do
        match a with
        | :? QosDef as qos ->
                cc.BasicQos(qos.prefetchSize,qos.prefetchCount,qos._global)
        | _ -> ()

module Cx = 
  /// attributes to communication channels
  let fromA(A:Attribute list) (cx:IConnection) =
    for a in A do
        match a with
        //| :? QosDef as qos -> setQos qos cc
        | _ -> ()
    ()

module Msg =
  let fromA(A:Attribute list) (bp:IBasicProperties) =
    for a in (A |> List.choose
                (fun a->
                  match a with
                  | :? X_PerMsgTTL -> Some(a)
                  | _ -> None
                )) do
      ()

module Ex = 
  let fromA x = ()

module Mq = 
  let fromA x = ()

//    for xp in xps do
//        match xps with
//          | "alternate-exchange",value -> yield X_AlternateExchange(value:?>string) :> XPair
//          | "x-message-ttl",value -> yield X_PerMsgTTL(value:?>string) :> XPair
//        X_PerQueueMsgTTL(value:?>string) 
//        //| :? X_PerQueueMsgTTL as x->
//        | :? X_PerQueueMsgTTL 
//         X_QueueTTL
//        X_DeadLetterExchange
//        X_DeadLetterExchangeRoutingKey
//        X_MaxLength
//        X_UserId
//        X_TrustUserId
//        X_CC
//        X_BCC
//        X_ApplicationType
//        X_Charset
//        X_Encoding
//        X_Content
//        X_CorrelationId
//        X_Priority
//        X_ReplyTo
//        X_MessageId
//        X_TimeStamp
//        X_AppId
//        X_Expiration
//        X_BasicType
//        X_Match
//        X_Reason
//        X_PrefetchCount
//        X_AckMode
//        X_DeliveryMode
//        X_ReconnectDelay
        //| k,v -> yield X_Argument(k,v) :> XPair
//      }

// mf: a mapping function
//  let gatherDefs (mf:Attribute->'ret list) (t:Type) (withTag:string option) =
//    let A = DefMeta.choose t
//    let lnsd=A|>makeNamespaceA                                                    // [1]
//    A
//    |>DefList.filterByTag withTag                                                 // [2]
//    |>List.map(fun x->gatherDefs0 mf 3 lnsd x)                                    // [3]
//    |>List.concat
//    // [1] make a small namespace out of local definitions-by-decorator
//    // [2] get candidates.  Should withTag be None, then logically we expect the multiplication effect to be against the register (since we have everything already).  Again this is programmer dependent.
//    // [3] recursive gather
//    // NB. consider using .distinct (we have a few list based definitions in our X_FSharp
  
  // should the current set be merged with the register set

  // mf: a mapping function
  // ff: a follow function
  //let rec defToXPair (attrib:obj) (mf:Attribute->'ret option) (ff:Attribute->'ret option) (levels:int) =
