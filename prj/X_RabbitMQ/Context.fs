namespace FSharp.Control.RabbitMQ

open System
open FSharp.Control.RabbitMQ
open FSharp.Control.X_FSharp
open DefHelper

/// NB: Charset attribute get blended into this older form.
type ContentMeta(bag) = 
  let getCharsetType (contentType:X_Content) bag=
    match TypeList.tryPick1<X_Charset,Attribute> bag with
    | Some(charset) -> charset.CharsetType
    | None -> CharsetType.UTF_8
  let getContentType bag = 
    let contentType=TypeList.tryPick1<X_Content,Attribute> bag
    match contentType with
    | None ->()
    | Some(contentType) -> contentType.CharsetType <- getCharsetType contentType bag
    contentType 

  member val ContentType= getContentType bag with get,set
  member val EncodingType =  
    TypeList.tryPick1<X_Encoding,Attribute> bag 
    |> (function | Some(x) -> x.EncodingType | None -> EncodingType.NoEncoding)  
    with get,set
  member val MessageType = TypeList.tryPick1<X_MessageType,Attribute> bag with get,set
  member this.toList() : Attribute list = 
    (this.ContentType |> function | Some(x) -> [x :> XPair] | _->[])
    @([X_Encoding(this.EncodingType) :> XPair])
    @(this.MessageType |> function | Some(x) -> [x :> XPair] | _->[])

  new(searchSpec:string,from:Type) = 
    let bag = DefMatch.choose2 searchSpec from
    ContentMeta(bag)

  new(searchSpec:string,bag) = 
    ContentMeta(DefMatch.choose1 searchSpec bag)

/// nme: Local Definitions;
/// def: groups definitions useful to a queue user from a bag of attributes
//type LocalDefs(searchSpec, bag) as this = 
//  // TODO: should we be gathering attributes based on a few kinds of tags?  in a (tags,target) kind of way where target is consr or publr?
//  let toAttributeList (L:XPair list) = 
//    L |> List.map(fun x->x:>Attribute)
//
//  /// has namespace
//  let hasNs (s : string) =
//    s |> Seq.exists(fun x->x='.')
//
//  /// exchange
//  member val ex = DefList.trySPick<ExDef> searchSpec bag with get,set
//  /// message queue
//  member val mq : MQDef option = DefList.trySPick<MQDef> searchSpec bag with get,set
//
//  /// connection
//  member val cx = DefList.trySPick<CxDef> searchSpec bag with get,set
//  /// communication channel
//  member val cc = DefList.trySPick<CcDef> searchSpec bag with get,set
//  /// quality of service
//  member val qos = DefList.trySPick<QosDef> searchSpec bag with get,set
//  /// consumer
//  member val consr : ConsrDef option = DefList.trySPick<ConsrDef> searchSpec bag with get,set
//  /// publisher
//  member val publr : PubDef option  = DefList.trySPick<PubDef> searchSpec bag with get,set
//
//  /// attributes
//  member val attribs : Attribute list = 
//                                       let mqL = this.mq |> (function|Some(x)->AttribList.fromExtension x|None->[])
//                                       let L = DefList.choose<Attribute> searchSpec bag
//                                       DefList.choose<Attribute> searchSpec (mqL @ L)
//                                       with get,set
//  
//  member val consrAttribs : Attribute list = 
//                                      DefList.choose<Attribute> searchSpec (this.attribs @ (this.consr|>(function|Some(x)->AttribList.fromExtension x|None->[])))
//                                      with get,set
//  
//  member val publrAttribs : Attribute list = 
//                                      DefList.choose<Attribute> searchSpec (this.attribs @ (this.publr|>(function|Some(x)->AttribList.fromExtension x|None->[]))) 
//                                      with get,set
//  member val consrRPairs = DefList.choose<RoutingPair> searchSpec (this.consrAttribs) with get,set
//  member val publrRPairs = DefList.choose<RoutingPair> searchSpec (this.publrAttribs) with get,set
//  new(searchSpec:string,from:Type) = 
//    let bag = DefMatch.choose2 searchSpec from
//    LocalDefs(searchSpec,bag)