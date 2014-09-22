#r"System.dll"
#r"System.Management.dll"
#r"System.Configuration.dll"
#r"bin/debug/protobuf-net.dll"
#r"bin/debug/RabbitMQ.Client.dll"
#r"bin/debug/RabbitMQ.ServiceModel.dll"
#r"bin/debug/ServiceStack.Interfaces.dll"
#r"bin/debug/ServiceStack.ProtoBuf.dll"
#r"bin/debug/ServiceStack.MsgPack.dll"
#r"bin/debug/ServiceStack.Text.dll"
#r"bin/debug/ServiceStack.Client.dll"
#r"bin/debug/FSharp.Control.X_FSharp.dll"
#load"../RabbitMQ/QParse.fs"
#load"../RabbitMQ/IOM.fs"
#load"../RabbitMQ/OM.fs"
#load"../RabbitMQ/Register.fs"
#load"../RabbitMQ/DefMatch.fs"
#load"../RabbitMQ/DefCache.fs"
#load"../RabbitMQ/Def.fs"
#load"../RabbitMQ/Helpers.fs" 
#load"../RabbitMQ/Context.fs"
#load"../RabbitMQ/Factory.fs" 
#load"../RabbitMQ/Packer.fs" 
#load"../RabbitMQ/PubSub.fs" 
#load"../RabbitMQ/X_Model.fs"
#load"Meta.Fixture1.fs"
open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open RabbitMQ.Client
open RabbitMQ.Client.Events
open FSharp.Control.X_FSharp

open FSharp.Control.RabbitMQ.Meta
open FSharp.Control.RabbitMQ

//open FSharp.Control.RabbitMQ.Meta.Fixture
open RabbitMQ.Client.MessagePatterns
open System.Threading
open FSharp.Control.RabbitMQ

module Thread =
  let start x = new Thread(ThreadStart(x))

type MsgCtx(hdrs:IDictionary<string,obj>) as this= 
  let cntTyp : X_Content=  
        match X_Content.tryNew hdrs with 
        | Some(x) -> x 
        | None -> failwith "no content type defined"
  let chsTyp : CharsetType = 
    let ch=
      match cntTyp.CharsetType, (X_Charset.tryNew hdrs) with
      | NoCharset, None -> NoCharset
      | NoCharset, Some(x) -> x.CharsetType
      | x, _ -> x
    match ContentType.isTextBased cntTyp.ContentType, ch with
    | false, _ -> ()
    | true, NoCharset -> failwith "no content type defined"
    | true, _ -> ()
    ch
  member val Cnt = cntTyp
  member val Chs = X_Charset(chsTyp)
  member val Enc =
    match X_Encoding.tryNew hdrs with 
    | Some(x) -> x 
    | None -> X_Encoding(EncodingType.NoEncoding)

module Helper = 
  let failIfNone msg x =
    match x with
    | Some(x) -> x
    | None -> failwith msg

open Helper
open FSharp.Control.RabbitMQ.Meta.Fixture
module testTypeGuidScheme = 
  let testGUID() =
    let tnskk = TestNskkGetter()
    let g1=typedefof<int>.GUID
    let g2=typedefof<string>.GUID
    let tnskk = TestNskkGetter()
    let g3=tnskk.GetType().GUID
    ()

  let testChooseParseAndQKVs() =
    let tnskk = TestNskkGetter()

    let L= 
      tnskk.GetType()
      |> DefMatch.chooseODefTypes
      |> List.map(fun x->DefMatch.chooseOdAttribs x)
      |> List.concat
      |> DefMatch.choose "g:tag1,tag2"

    let L2=DefMatch.choose2 "g:tag1,tag2" tnskk
    
    let L3=DefCache.choose2 "g:tag1,tag2" tnskk

    DefCache.remove "g:tag1,tag2" tnskk |> ignore

    printfn "%A" <| ((L.Length) = (L2.Length))

    printfn "%A" (L.Length)

    let KV = "g:`"    |> Parse.fromQKVs |> Seq.distinct |> Seq.toList
    let KV = ""       |> Parse.fromQKVs |> Seq.distinct |> Seq.toList
    let KV = "g:tag2" |> Parse.fromQKVs |> Seq.distinct |> Seq.toList
    ()

  let testChoose1() = 
    let tnskk = TestNskkGetter()
    let fromT=typedefof<TestNskkGetter>
    let asm = Assembly.GetExecutingAssembly()
    DefMatch.choose2 "g:tag1" tnskk 
    |> List.filter(fun x->x.GetType().Assembly=asm)

  let testChoose2() =
    let tnskk = TestNskkGetter()
    let fromT=typedefof<TestNskkGetter>
    let asm = Assembly.GetExecutingAssembly()
    let L1 = 
              DefMatch.choose2 "w:*.is.a.*" tnskk 
              |> List.filter(fun x->x.GetType().Assembly=asm)         // filter not needed now
    DefMatch.choose2 "w:*.i?.a.*" tnskk |> DefMatch.filterAsm asm     // filter not needed now

  let testChooseAndFilter2b() =
    let tnskk = TestNskkGetter()
    let fromT=typedefof<TestNskkGetter>
    let asm = Assembly.GetExecutingAssembly()
    DefMatch.choose2 "g:tag2" tnskk 
    |> DefMatch.filterAsm asm                                         // not needed now

  let testChooseAndFilter3() =
    let tnskk = TestNskkGetter()
    let fromT=typedefof<TestNskkGetter>
    let asm = Assembly.GetExecutingAssembly()
    DefMatch.choose2 "w:*.not.a.*" tnskk |> DefMatch.filterAsm asm

  let testMakingRegEx4() =
    let tnskk = TestNskkGetter()
    let GrpRegX,WhenRegX = DefMatch.makeRegEx2 "g:`" tnskk
    let GrpRegX,WhenRegX = DefMatch.makeRegEx2 "w:*.is.a.*" tnskk
    let s = "a.is.a.b"
    ()

  type IDelegateEvent<'Del when 'Del :> Delegate> with
    member this.Subscribe handler =
      do this.AddHandler(handler)
      { new IDisposable with 
          member x.Dispose() =
            this.RemoveHandler(handler) }
  
  let testGetReplyToFromMsg() =
    let cxd = Some <| CxDef("localhost")
    let fact = Factory(cxd)
    let cx = fact.Connection
    let ccSvc = fact.OpenChannel(None)
    let cxSvcException=cx.CallbackException.Subscribe(
                            fun args->match args with | A -> printfn "cx Exception: %A" (IDict.toString A.Exception.Data))

    //ccf.openExchange exch
    //ccf.openQueue queue
    
    // a. open by qlist (exchange:queue style), b. open by attribute, c. filter down property bag and combine all routing pairs

    // how do we ignore the queue part of a queue if we aren't creating a queue to consume first?
    
    let unpack0 (content:X_Content) (charset:X_Charset) (encoding: X_Encoding) (msgtype:X_MessageType) (body:byte array)=
      Packer.unbox2 content charset encoding msgtype body

    let unpack1 (msgCtx:MsgCtx) (msgTyp:X_MessageType) B=

      unpack0 msgCtx.Cnt msgCtx.Chs msgCtx.Enc B

    let unpack2 (hdrs:IDictionary<string,obj>) B=
      let msgTyp = failIfNone "no message type" <| X_MessageType.tryNew hdrs
      let msgCtx = MsgCtx hdrs

      unpack0 msgCtx.Cnt msgCtx.Chs msgCtx.Enc msgTyp B

    let unpack3 (e:BasicDeliverEventArgs) =
      let d = e.BasicProperties.Headers
      unpack2 (d) (e.Body) 


    let ccf = fact.OpenCcFactory(None)
    let exch,queue = (ExDef("myTestExchange",ExchangeType.Topic), MQDef("myTestExchange:catch.some.fish",qrouting0=""))

    let consrDef = ConsrDef(exclusive=false)
    let q = MQDef("someExch:someQueue")
    let consr = ccf.openConsumer (Some consrDef) q

    let stopLoop = ref true
    /// event loop
    let loop f (consr:QueueingBasicConsumer) = 
      fun ()-> 
          while not(!stopLoop) do 
            f consr.Model (consr.Queue.Dequeue())

    let mq = MQDef()
    mq.QRouting  <- "exch1:routing.key"
    mq.exclusive <- true
    mq.immediate <- true

    let ccdict = Dictionary<string * string, MQDef>()
    let kv = mq.GetRoutingPairs() |> List.filter(fun x->x.exchange = "" && x.routingKey = "") |> List.spick
    //ccdict.Add((mq.))

    let doSomeWork (cc:IModel) (e:BasicDeliverEventArgs) =
      let msg = unpack3 e
      // cc.BasicPublish() -- what properties should the reply have?  How do we acquire these?
      let mandatory, immediate = false, false
      let B = String.toBytes "[reply] hi"
      let D = Dictionary<string,obj>() :> IDictionary<string,obj>
      let BP = cc.CreateBasicProperties()
      D |> Seq.iter(fun kv-> BP.Headers.Add(kv.Key,kv.Value))
      cc.BasicPublish(e.BasicProperties.ReplyToAddress.ExchangeName, e.BasicProperties.ReplyToAddress.RoutingKey,mandatory,immediate,BP, B )
      cc.ack e
      ()

    // create 25 messages

    let th = 
             consr 
             |> loop doSomeWork 
             |> Thread.start

    let th2 = 
             consr 
             |> loop (fun m e->m.BasicAck(e.DeliveryTag,false))
             |> Thread.start
  
    let th3 = 
             consr
             |> loop (fun m e->m.BasicAck(e.DeliveryTag,false))
             |> Thread.start

    for e in consr.toSeq() do
      // ... do some stuff
      consr.Model.BasicAck(e.DeliveryTag,false)

      // what would this look like if we threw it in an Actor?

    // how do we set the inbox to be of a certain type?

    let worker = 
          MailboxProcessor<BasicDeliverEventArgs>.Start(fun inbox ->
            let rec loop() = async {
              let! e = inbox.Receive() //consr.Queue.Dequeue()
              
              //do printfn "num = %i" num
              return! loop() }
            loop())

    let worker2 = 
          MailboxProcessor<BasicDeliverEventArgs>.Start(fun inbox ->
            let rec loop() = async {
              let e = consr.Queue.Dequeue()
              //let! e = inbox.Receive() //consr.Queue.Dequeue()
              //do printfn "num = %i" num
              return! loop() }
            loop())
    
    let test = consr.Queue.DequeueNoWait(null)
    for e in consr.toSeq() do
      //e.
    //ccf.openExchange consr (queue.GetRoutingPairs())

    Subscriber()

    //let 

    let ccSvcException=ccSvc.CallbackException.Subscribe(
                            fun args-> match args with | A -> printfn "cc exception: (%A), context: (%A)" (IDict.toString A.Exception.Data) (Dict.toString args.Detail))

    let ccCli = fact.OpenChannel(None)
    let ccCliException=ccCli.CallbackException.Subscribe(
                            fun args->match args with | A -> printfn "cc exception: (%A), context: (%A)" (IDict.toString A.Exception.Data) (Dict.toString args.Detail))

    //let doSvc() = 
      //fact.RegisterExchange

    // TODO: how do we use this to reconnect?

    // setup a ping

    //let cx = RabbitMQ.Client.ConnectionFactory
    ()

  let testGetReplyToFromThis() =
    ()

  let testGetReplyToFromAttrib() =
    ()

  let testDealWithExceptions() = 
    ()

  let testGetReplyToFromAll() =
    // should it be possible to get the reply to from all sources if there is more than one attribute?  These are just additional routing keys
    ()

  // TODO: how do you choose between DEV / TEST / PROD?