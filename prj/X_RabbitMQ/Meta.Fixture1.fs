namespace FSharp.Control.RabbitMQ.Meta.Fixture

open System
open System.Text
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open FSharp.Control.X_FSharp
open FSharp.Control.RabbitMQ

// members and classes may clash in our simple nskk structure.  We do not strictly enforce this.

type RabbitMQ() =
  let x = "x"
  member this.RabbitMQ() = "x"

/// Connection meta store
[<CxDef(HostName = "localhost", Nskk="a.b.RabbitMQ.c")>]
type MyCxDefs() = 
  member val X = ""
  member __.find() =
    let x = CxDef()
    x.HostName <- "localhost"

[<X_AppId("my-app-id")>]
type MyAppDefs() = 
  member this.X="x"

[<X_QueueTTL(0L);X_PerQueueMsgTTL(0L);X_MaxLength(5)>]
type MyMqDefs() = 
  member this.X="x"

[<X_Priority(1);
  X_ReplyTo1(typedefof<This>);
  X_Charset("utf-8"); 
  X_Content("application/x-protobuf"); 
  X_PerMsgTTL(10000L)>]
  // dynamic header values: X_Content; X_Encoding; X_MessageId; X_CorrelationId; X_TimeStamp(); 
type MsgDef1() = 
  let t2=typeof<int>
  let t3=typeof<This>
  let t=typedefof<This>
  member this.X="x"
/// Channel Meta Store
[<CcDef()>]
type MyCcDefs() = 
  member val X = ""  
  member __.find() =
    let x = CcDef()
    x.Nskk <- Some "a.v"

[<MQDef("someExchange:somequeue",Grp="someOtherTag")>]
type ReplyToDefs() = 
  member val X = ""  

// locals with meta
type Publisher() = 
  member val X = ""
  
type Subscriber() =
  member val X = ""

type test() =
  let a = new MQDef()

[<MQDef("myQueueName", "exch:this.is.a.queue.name,another.queue,another.queue.2",Grp = "tag1")>]
[<MQDef("exch:this.is.a.queue.name,another.queue,another.queue.2",Grp = "ch1", durable=true,exclusive=true,noWait=true,passive=true)>]
[<MQDef("this.is.a.queue.name.2")>]
[<MQDef("exch:not.a.good.queue.name")>]
[<MQDef("exch:a.not.a.good.queue.name")>]
[<MQDef("exch:a.not.a.good.queue.name.2", Grp="tag2")>]
[<MQDef("","tag1")>]
[<X_ReplyTo1(typedefof<This>, Grp="tag1")>]
[<X_ReplyTo1(typedefof<AddrInMsg>, Grp="tag1")>]
[<X_ReplyTo1(typedefof<ReplyToDefs>, Grp="tag1", AltGrp="someOtherTag")>]
[<MatchDef(".RabbitMQ.*", Grp="tag1");MatchDef(".RabbitMQ.2.*", Grp="tag2");MatchDef("", Grp="tag3")>]
[<MatchDef(Grp="tag1");MatchDef(Grp="tag2");MatchDef(Grp="tag3")>]
type TestNskkGetter() =     

  let t = 
     let t=MQDef()
     t.durable <- true
     t.exclusive <- true
     t.noWait <- false
     t.passive <- true
     t
  let x1= X_CC("ch1","some.queue.routingKey")
  
  let mutable this_class_nskk_k : string option = None
  let mutable this_get_nskk_k : string option = None

  //--
  interface IOpenDef0<MyCxDefs> 
  interface IOpenDef1<MyCcDefs> 
  interface IOpenDef2<MyAppDefs>
  interface IMatch0When with member val When = "tag1" with get 
  //--
  
  member private this.handleCMeta() = 
    this_class_nskk_k <-
      match this_class_nskk_k with
      | None ->
          let cns=NS.getClass this
          // get and register meta
          Some(cns)
      | x -> x

  member private this.handleMemberMeta(nskk:string option) = 
    match nskk with
    | None -> 
        this.handleCMeta()
        let ns=NS.getMemb this
        // get and register meta
        Some(ns)
    | x -> x

  member this.getm() = 
    NS.getMemb this
  
  member this.get() = 
    this_class_nskk_k <- this.handleMemberMeta this_class_nskk_k

// TODO: consider implementing as a tuple of (nsk, nskType).  nskTypes are: `module`type`member
// TODO: setup register
// TODO: get from local namespace
