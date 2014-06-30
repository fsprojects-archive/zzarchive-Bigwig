namespace bigwig.X_RabbitMQ

open System
open System.Text
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Text.RegularExpressions
open RabbitMQ.Client
open ServiceStack
open ServiceStack.Text
open ServiceStack.ProtoBuf
open ServiceStack.MsgPack
open bigwig.X_RabbitMQ
open bigwig.X_RabbitMQ.PackXPair
open bigwig.X_FSharp
open FactoryHelper
open DefHelper
open DefRef
open TypeList

exception NoEncodingTypeInMessage of string
exception NoContentTypeInMessage of string
exception NoRouteSpecified of string
exception RoutingPatternNotUnderstood of string

module PubSub =

  let basicPublish (cc:IModel) (exch:string) (routingKey:string) mandatory immediate xpairs body =
    cc.BasicPublish(exch,routingKey,mandatory,immediate,(PackXPair.createPubMsgBP xpairs cc), body)

  let basicPublish1 (cc:IModel) (RP:RoutingPair) mandatory immediate xpairs body =
    basicPublish cc (RP.exchange) (RP.routingKey) mandatory immediate xpairs body

  //  let basicPublish2 (cc:IModel) (pubProps:PubDef) xpairs body =
  //    basicPublish1 cc (pubProps.toRP()) pubProps.mandatory pubProps.immediate xpairs body

  let filterBag (tag:string option) (bag0:Attribute list) = 
    let matcher =
      match tag with
      | None -> None
      | Some(tag) -> Some(Parse.getWildcardSearch tag)
    let bag = bag0 |> List.map(fun x->x:>obj) |> List.choose(function | :? INskk as x->Some(x) | _->None)
    match matcher with
    | None -> bag
    | Some(matcher)->bag|>List.filter(fun x->match x.Nskk with
                                              | "" | null ->true
                                              | ref -> matcher.IsMatch(ref))
    |> List.map(fun x->x:?>Attribute)
  
  let findRoutingPair (rpPattern:string) (RP : RoutingPair list) =
    let rp = RoutingPair.fromQKVs(rpPattern) |> List.spick
    let exrkRegex = Parse.getWildcardSearch rp.exchange 
    let mqrkRegex = Parse.getWildcardSearch rp.routingKey 
    let f = 
      match not<|String.IsNullOrEmpty(rp.exchange), not<|String.IsNullOrEmpty(rp.routingKey) with
      | true,true   -> (fun (x:RoutingPair)->exrkRegex.IsMatch(x.exchange) && mqrkRegex.IsMatch(x.routingKey))
      | true,false  -> (fun (x:RoutingPair)->exrkRegex.IsMatch(x.exchange))
      | false,true  -> (fun (x:RoutingPair)->mqrkRegex.IsMatch(x.routingKey))
      | false,false -> (fun (x:RoutingPair)->true)
    RP |> List.filter(fun x->f(x))

  // exchange://routingKey/some.further.*.search.key.for.attributes
  // ie. first attempt was too ambitious
  let getTagPattern (tagPattern:string) =
    match tagPattern with 
    | null
    | "" ->
      (None,None,None)
    | tagPattern ->
      match tagPattern.Split('/') with
      | [||]    -> (None,None,None)
      | [|a|]   -> (Some(a),None,None)
      | [|a;b|] -> 
          match b.Split(':') with
          | [|c;d|] -> (Some a,Some c,Some d)
          | [|d|]   -> (Some a,None,Some d)
          | [||]    -> (None,None,None)
          | _       -> raise<|RoutingPatternNotUnderstood(sprintf "'%s' not a recognised pattern in some combination of 'tag/exchange:routingKey' pattern" tagPattern)
      | _ -> raise<|RoutingPatternNotUnderstood(sprintf "'%s' not a recognised pattern in some combination of 'tag/exchange:routingKey' pattern" tagPattern)

  let getTagPattern2 (tagPattern:string) =
    match tagPattern with 
    | null
    | "" -> (None,None)
    | tagPattern ->
        match tagPattern.Split([|'/';':'|],StringSplitOptions.RemoveEmptyEntries) with
        | [|a;b|] -> (Some(a),Some(b))
        | [|b|] -> (None,Some(b))
        | [||] -> (None,None)
        | _ -> raise<|RoutingPatternNotUnderstood(sprintf "'%s' not a recognised pattern in some combination of 'exchange:routingKey' pattern" tagPattern)


  exception NotAnXPair of string

  // Q: how do we basic publish in any message transport format?  Use a "byConvention ending to the method? or an additional meta tag?"
  // Q: how do we have defaults for message transport, and exchange.  If we had a publishDefaults attribute, context could be provided by composition.

  // Q: should tag be a tupple.  ie. we use a tag to whittle down the property bag, and a sub-tag to whittle down the routes list
  // A: whittle down the routes with a pattern match against the queue
  // Q: what caching scheme should be employed?
  // tag1/exchangePattern:routePattern
  // if more than one routing key provided, then publish to all
  let send (cc:IModel) (searchSpec:string) (rkSearchSpec:string) (bag0:Attribute list) body =

    let (exPatStr,rkPatStr) = getTagPattern2 rkSearchSpec
    let exPat = exPatStr |> (function|Some(s)->Some(Parse.getWildcardSearch s)|_->None)
    let rkPat = rkPatStr |> (function|Some(s)->Some(Parse.getWildcardSearch s)|_->None)

    /// filtered bag list
    let FB = DefMatch.choose searchSpec bag0

    let inline pick t = TypeList.pick t FB
    let inline tryPick t = TypeList.tryPick t FB
    let inline choose t = TypeList.choose t FB
    let inline strOptToStr (s : string option) = s|>(function| Some(s)->s | _->"")
    
    /// routing pair list
    let RP = 
            choose typedefof<MQDef>
            |> List.map(function | :? MQDef as mqd->mqd.GetRoutingPairs() |> List.map(fun x->(x,mqd.mandatory,mqd.immediate)) | _ ->[])
            |> List.concat
            |> List.filter(fun (x:RoutingPair,mandatory,immediate)->exPat|>(function|Some(ex)->ex.IsMatch(x.exchange)|None->true))
            |> List.filter(fun (x:RoutingPair,mandatory,immediate)->rkPat|>(function|Some(rk)->rk.IsMatch(x.routingKey)|None->true))
    
    /// exchange
    let mutable ex : string  option = None
    /// routing key
    let mutable rk : string option = None
    let mutable mandatory : bool = false
    let mutable immediate : bool = false
    /// carbon copy
    let mutable CC : X_CC list = []

    match RP.Length with 
    | 0 -> failwith "no routes provided"
    | 1 -> let (ex_,rk_,mand_,imm_)  = 
                    RP 
                    |> List.head 
                    |> (fun (x,mand0,imm0)-> (x.exchange,x.routingKey,mand0,imm0))
           ex <- Some ex_
           rk <- Some rk_
           mandatory <- mand_
           immediate <- imm_
    | _ ->  
          let (ex_,mand_,imm_)  = 
               RP 
               |> List.map(fun (x,mandatory,immediate)->(x.exchange, mandatory,immediate))
               |> Seq.distinct 
               |> Seq.filter(fun (x,mand,imm)->not(String.IsNullOrEmpty x)) 
               |> Seq.singletonOrFailWith "must be one and only one exchange defined" 
               |> (fun (x,mand,imm)->Some x,mand,imm)
          ex <- ex_
          mandatory <- mand_
          immediate <- imm_
          CC <- 
               RP 
               |> List.map(fun (x,mandatory,immediate)->x.routingKey) 
               |> Seq.distinct 
               |> Seq.filter(fun x->not(String.IsNullOrEmpty x)) 
               |> Seq.map(fun x->X_CC(x)) 
               |> Seq.toList

    let pd = 
           FB
           |> List.tryPick(function | :? PubDef as pd -> Some pd | _->None) 
           |> (function | None -> PubDef()|Some x->x)

    let rp = RoutingPair(strOptToStr ex, strOptToStr rk)

    let xpairs = 
                FB
                |> List.filter (function | :? XPair->true | _->false)
                |> List.append (CC |> List.map(fun x->x:>Attribute))
                |> List.map(fun x->x:?>XPair)

    basicPublish1 cc rp mandatory immediate xpairs

  let send1 (cc:IModel) (searchSpec:string) (rkSearchSpec:string) (bag0:Attribute list) (msg:'msgTyp) =
    let mutable bag = (DefList.choose<'msgTyp> searchSpec bag0)
    
    let contentMeta = ContentMeta(bag)

    let (B, msgType) = 
            match (contentMeta.ContentType), contentMeta.EncodingType with
            | Some content, encoding ->
                Packer.box (content.ContentType) (content.CharsetType) encoding msg
            | None, _ ->
                failwith "contentType type not specified"

    bag<-Meta.Bag.upsertn bag ((contentMeta.toList()) @ [(msgType :> Attribute)]) |> Seq.toList

    send cc searchSpec rkSearchSpec bag0 B

  let receive (searchSpec:string) (bag:XPair list) (B:byte array) =
    let cm = ContentMeta(searchSpec, bag |> List.map(fun x->x:>Attribute))
    match cm.ContentType, cm.EncodingType, cm.MessageType with
    | Some(contentType), encodingType, Some(messageType) -> 
          Packer.unbox1 (contentType.ContentType) (contentType.CharsetType) encodingType messageType B
    | _ -> raise<|NoContentTypeInMessage("invalid content type")
  
  let receive1<'t> (searchSpec:string) (bag:XPair list) (B:byte array) =
    receive searchSpec (bag @ [X_MessageType(typedefof<'t>)]) B
    