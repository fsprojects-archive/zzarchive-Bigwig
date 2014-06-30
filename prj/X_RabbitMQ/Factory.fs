namespace bigwig.X_RabbitMQ

open System
open System.IO
open System.Text
open System.Reflection
open System.Collections.Generic
open System.Collections
open Microsoft.FSharp.Reflection
open RabbitMQ
open RabbitMQ.Client
open bigwig.X_RabbitMQ
open bigwig.X_FSharp
open bigwig.X_RabbitMQ.DefHelper
open System.Runtime.CompilerServices

module IAmqpExtensionM =

  let GetArguments (this:bigwig.X_RabbitMQ.IAmqpExtension) (passive:bool) (tag:string) =

    let d = new Dictionary<string,obj>() 

    // gather attributes by context
    let mpairs =
      match passive with
      | false ->
          DefMatch.choose2 tag this
          |> List.choose(fun x->x|>(function| :? XPair as xp -> Some(xp) | _ ->None))
      | _ -> []

    for x_pair in (this.X_Pairs @ mpairs) do
      match x_pair with
      | :? X_BCC
      | :? X_CC  as cc->(cc.oVal :?> string|> Parse.fromQList |> Seq.iter(fun v->d.Add(cc.Key,v)))
      | xp->d.Add(xp.Key,xp.oVal)

    // a context is a dictionary
    
    // the trouble with this namesapce is simply that it doesn't allow collisions

    // split refs, and then look them up either locally in supplied attributes, or in the global namespace
    (this.X_Refs |> (function|null->""|x->x)).Split([|',';' '|],StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList 
    |> List.choose(fun x->NsDict.tryGet x Register.ns) 
    |> List.choose(fun x->if (typedefof<XPair>.Is1 x) then Some(x :?> XPair) else None)
    |> List.iter(fun x->Dict.add (x.Key,x.oVal) d)
    
    d :> IDictionary<string,obj>

  //.this.is.a.name.space`key:1
  
  let GetArguments1 (this:IAmqpExtension) (passive:bool) =
      GetArguments this passive ""

  /// dsc: take basic-properties and put them in xpairs
  let PutArguments (basicProperties:RabbitMQ.Client.IBasicProperties) =
      Meta.BasicProperties.toXPair basicProperties

  let ToIDictionary(this:XPair seq) =
    let d=new Dictionary<string,obj>()
    for kv in this do
      d.Add(kv.Key,kv.oVal)
    d :> IDictionary<string,obj>


// TODO:

  // additional indexes and caches
  // let ByLocale = 
  // let ByAlias =  // an alias is a tag in this context
  //  type DeserializerRegister() =

// REFERENCE:
// • management plugin location: http://localhost:15672/

// KEY CONCEPTS:
// register -> look up -> declare -> bind -> consume
// NB: 
// • Each N-to-1 consumers has a unique queue with topic bindings.  
// • this could be set up with n 1-to-1 direct bindings (Q: what is meant by this?)

exception QueueNotDefined of string
exception ExchangeNotDefined of string
exception ExchangeNotInRegister of string
exception QueueDeclareFailed of string
exception ExchangeDeclareFailed of string

/// dsc: we make connections, channels, exchanges, queues, and bindings
module FactoryHelper =

  let _connectFactory(cxDef : CxDef) =
    let cxf = ConnectionFactory()

    cxf.HostName <- cxDef.HostName |> (function|null->cxf.HostName|x->x)
    cxf.VirtualHost <- cxDef.VirtualHost |> (function|null-> cxf.VirtualHost|x->x)
    cxf.Port <- cxDef.Port |> (function|0->cxf.Port|x->x)
    cxf.UserName <- cxDef.UserName |> (function|null->cxf.UserName|x->x)
    cxf.Password <- cxDef.Password |> (function|null->cxf.Password|x->x)
    //    if meta.Protocol.IsSome then
    //      cf.Protocol <- meta.Protocol.Value
    cxf.RequestedChannelMax <- cxDef.RequestedChannelMax |> (function|0us->cxf.RequestedChannelMax|_->0us)
    cxf.RequestedConnectionTimeout <- cxDef.RequestedConnectionTimeout |> (function|0->cxf.RequestedConnectionTimeout|_->0)
    cxf.RequestedFrameMax <- cxDef.RequestedFrameMax |> (function|0ul->cxf.RequestedFrameMax|_->0ul)
    if not <| String.IsNullOrEmpty cxDef.Uri then
      cxf.Uri <- cxDef.Uri

    if not <| String.IsNullOrEmpty cxDef.Ssl then
      let uri = Uri(cxDef.Ssl)
      if   not <| String.IsNullOrEmpty uri.Query then
        let i = uri.PathAndQuery.IndexOf("?")
        let path = uri.PathAndQuery.Substring(0,i)
        cxf.Ssl <- SslOption(uri.Host,path, (uri.Query = "true" || uri.Query = "enabled"))
      else
        cxf.Ssl <- SslOption(uri.Host)
    cxf
  
  let connect0 (factory : ConnectionFactory) (meta : CxDef option) = 
    if meta.IsSome && meta.Value.MaxRedirects <> 0 then
      factory.CreateConnection(meta.Value.MaxRedirects)
    else
      factory.CreateConnection()

  let isExchangeDeclared (cc:IModel) (ex:string) =
    try 
      cc.ExchangeDeclarePassive(ex)
      true
    with
    | _ -> false

  /// dsc: queue declaration is idempotent, however, so we attempt to declare once
  /// however it might be useful to see if it is really there programatically on ocassion
  /// in terms of "guaranteed" delivery in practice this should not normally be necessary
  let isQueueDeclared (cc:IModel) (mqKey:string) =
    try 
      cc.QueueDeclarePassive(mqKey) |> ignore
      true
    with
    | _ -> false
    
  /// dsc: this passive refers to the request to create the exchange, not the exchange specs themselves which may contain passive=false (ie. assertive)
  let openExchange (cc:IModel) (exDef:ExDef) =
    match exDef.passive with
    | false ->        
        let args = IAmqpExtensionM.GetArguments1 (exDef :> IAmqpExtension) false
        cc.ExchangeDeclare(exDef.name,exDef._type.toString(),durable= exDef.durable,autoDelete= exDef.autoDelete,arguments= args)
        Register.setExchangeOpen(exDef)
    | true -> 
        cc.ExchangeDeclarePassive(exDef.name)
        Register.setExchangeOpen(exDef)
    
  /// dsc: exchange declare by [definition] lookup
  let openExchangeByLU (cc:IModel) exKey =
    let exDefOpt = Register.getExchange(exKey)
    let tmp =
      match exDefOpt with
      | Some(exDef) -> 
          openExchange cc exDef
      | None -> raise <| ExchangeNotInRegister(sprintf "no definition registered for exchange '%s'" exKey.Value)
    Register.setExchangeOpen(exDefOpt.Value)
    tmp

  /// dsc: 
  /// 1. Lookup the register to get queue & exchange definition.
  /// 2. When in passive=false, the (queue,exchange) will be created if they do not exist
  ///    Otherwise, the (queue,exchange) must be created and the method will fail if they are already present
  /// exceptions: QueueNotDefined,  NoExchangeDefined
  let openQueueByLU (cc:IModel) mqKey (passive:bool) =
    //let exchangeMeta = MQ.Register.getExchange(exchange)
    match passive,(Register.getQueue mqKey) with
    | true,Some(mqDef) -> 
        cc.QueueDeclarePassive(mqDef.name)
    | false,Some(mqDef) -> 
        let args = IAmqpExtensionM.GetArguments1 (mqDef :> IAmqpExtension) false
        cc.QueueDeclare(mqKey, mqDef.durable,mqDef.exclusive,mqDef.autoDelete,args)
    | _,_ -> raise<|QueueDeclareFailed("could not declare queue")

  let toStrOption (x:string) = 
    if String.IsNullOrEmpty(x) then None else Some(x)

  /// dsc: should a queue be declared with exclusive status, then a uniqueue queue name will be declared
  /// TODO: how can I create a sensible opportunity to push in a TTL, or other attribute here?
  let openQueue (cc:IModel) (mqDef1:MQDef)=

    // TODO: 
    // • have lazy and active declare settings.  Active means create the queue if it isn't there.  Exclusive means always create but lock to a certain connection.
    // • I am tempted to introduce tristate of passive, proactive, intolerat, exclusive
    // lets go over these definitions again: I thought in the Rabbit documentation that passive meant create it if it isn't there.

    let openResponse = 
      match mqDef1.passive, toStrOption(mqDef1.name) with 
      | true, Some(q) -> 
          cc.QueueDeclarePassive(queue=q)
      | true, None ->  raise<|QueueNotDefined("invalid option.  Must provide a queue name when passive")
      | false, None -> 
          cc.QueueDeclare()  // probably just an un-named RPC client, or an un-named n-1 client (direct or topic).  We assume this to mean not-durable, autoDelete, and exclusive
      | false, Some(q) -> 
          cc.QueueDeclare(queue=mqDef1.name,durable=mqDef1.durable,exclusive=mqDef1.exclusive,autoDelete=mqDef1.autoDelete,arguments=null)  

    // NB. rpc does not require routing.  The routing key can be blank for fanout.
    mqDef1.GetRoutingPairs()
       |> List.iter(fun rp->
                      cc.QueueBind(queue=openResponse.QueueName,exchange=rp.exchange,routingKey=rp.routingKey)
                      Register.setQueueBindingBound mqDef1.name rp
                   )
    openResponse.QueueName

  // cases:
  // 1. get queue information
  //    a. from register if registered
  //    b. from attribute meta if meta is available
  // 2. create exchange if exchange is not yet created
  // 3. create queue if queue is not yet created
  // 4. fail if there are no routing keys defined 

  // other cases:
  // • use the default rabbit queue
  // • use own default queue
  // • use conditions based on the returns of .ConsumerCount, .MessageCount

  /// append routes to the looked up register consrQName -- NB. this does not really make much sense
  let openQueueByDef (cc:IModel) (consrDef:ConsrDef option) (consrQName:string option) (routes:RoutingPair list option)=
    let consrDef = 
      match consrDef with
      | None -> Some<| ConsrDef(noAck= true, exclusive= true, noWait= false, noLocal= false)
      | Some m -> Some m
    
    let mqKey= consrQName |> (function|Some(x)->x|_->"topic.consumer.queue")
    
    let resolveRKeys x =
      x |> (function |None->[]|Some(x)->x |> Seq.distinct |> Seq.toList)

    let mqFromRegister = Register.getQueue(mqKey)
    if mqFromRegister.IsSome then
      if String.IsNullOrEmpty(mqFromRegister.Value.name) then mqFromRegister.Value.name <- mqKey
      mqFromRegister.Value.RoutingPairs <- mqFromRegister.Value.RoutingPairs @ (resolveRKeys routes)

    let mqDef1=MQDef(durable= false, exclusive= true, autoDelete= true, name= mqKey, RoutingPairs= resolveRKeys(routes))
    
    let mqDef1 = mqFromRegister|>(function|None->mqDef1|Some x->x)

    let regRoutingKeys = 
      match mqFromRegister with
      | Some(mqDef1) -> mqDef1.RoutingPairs
      | None -> []    

    let mqDef1 = mqFromRegister|>(function|Some(x)->x|None->mqDef1)
    let uniqueQueueName=openQueue cc mqDef1
    
    Register.setQueueOpen mqDef1
    uniqueQueueName

  /// dsc: check in-memory to find if declared first, then declare
  let assertExchange (cc:IModel) (exDef:ExDef) =
    if not <| (isExchangeDeclared cc exDef.name) then
      openExchange cc exDef 
    exDef

  let assertExchangesForQueue (cc:IModel) (mqDef:MQDef) =
    let exchanges = 
      mqDef.GetRoutingPairs() 
      |> List.map(fun x->x.exchange) 
      |> Seq.distinct 
      |> Seq.toList
    for exchange in exchanges do
      let ExRegEntryOption = Register.getExRegEntry(exchange)
      match ExRegEntryOption with
      | None -> 
          if not <| isExchangeDeclared cc exchange then
            raise <| ExchangeNotDefined(sprintf "exchange '%s' does not exist" exchange)
      | Some(exchange) -> 
          if not <| exchange.IsOpen then
            openExchange cc exchange.Meta
            exchange.IsOpen<-true

  let assertQueue (cc:IModel) (mqDef:MQDef) =
    if not <| (Register.isQueueOpen(mqDef.name)) then    
      assertExchangesForQueue cc mqDef
      openQueue cc mqDef
    else
      null

  let assertQueueByName (cc:IModel) (mqKey:string) =
    match Register.getQueue(mqKey) with
    | Some(mqDef) -> 
        not <| String.IsNullOrEmpty(assertQueue cc mqDef)
    | None -> 
        if isQueueDeclared cc mqKey then 
          let mqDef1=MQDef(name=mqKey, isStub=true)
          Register.setQueueOpen(mqDef1)
          true
        else
          false

  // question: what does this BasicConsume string mean?
  // question: how do we distinguish between a custom queue, and one that already exists with many workers?

  /// has "Next" + IEnumerable foreach functionality
  /// Once created, the Subscription consumes from a queue (using a QueueingBasicConsumer). Received deliveries can be retrieved by calling Next(), or by using the Subscription as an IEnumerator in, for example, a foreach loop.
  /// see: http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v1.7.2/rabbitmq-dotnet-client-1.7.2-client-htmldoc/html/type-RabbitMQ.Client.MessagePatterns.Subscription.html
  let openSubscriber (cc:IModel) (consrDef:ConsrDef option) (queue:MQDef) (openSubQueue:bool) = 
    let noAck = consrDef |> (function | Some(x)->x.noAck | None ->false)
    let queueName = queue.uniqueName |> (function|""|null -> queue.name | x -> x)
    if openSubQueue then
      let uniqueMQName = openQueue cc queue                                   // uniqueName should only be different if "exclusive" is set
      new RabbitMQ.Client.MessagePatterns.Subscription(cc,queueName,noAck)
    else
      new RabbitMQ.Client.MessagePatterns.Subscription(cc,queueName,noAck)

    // the openSubQueue is a little bit weird.  Not sure how this will work out in practice

  let fitsWorkerPattern (q:MQDef) =
    q.passive && not(q.exclusive)

  let fitsSubscriberPattern (q:MQDef) =
    q.exclusive

  /// uses a shared queue for delivery.  There is some knowledge on the server about a basic consumer via ConsumerTag
  /// use queue.deque to get messages
  /// see: http://www.rabbitmq.com/releases/rabbitmq-dotnet-client/v1.4.0/rabbitmq-dotnet-client-1.4.0-net-2.0-htmldoc/type-RabbitMQ.Client.QueueingBasicConsumer.html
  let openConsumer (cc:IModel) (consrDef:ConsrDef option) (queue:MQDef) =
    //let uniqueQName = openQueue cc queue
    let consr= QueueingBasicConsumer(cc)

    let noAck = consrDef |> (function | Some(x)->x.noAck | None ->false)
    let noLocal = consrDef |> (function | Some(x)->x.noLocal | None ->false)
    let noWait = consrDef |> (function | Some(x)->x.noWait | None ->false)
    let exclusive = consrDef |> (function | Some(x)->x.exclusive | None ->false)
    let args = 
          let d : IDictionary<string,obj> = Dictionary<string,obj>() :> IDictionary<string,obj>
          match consrDef with
          | Some(q) -> if q.priority.IsSome then d.Add("x-priority", q.priority.Value)
          | None -> ()
          d
    cc.BasicConsume(queue.name, noAck, queue.name, noLocal, exclusive, args, consr) |> ignore      
    consr  

[<Extension>]
type QueueingBasicConsumerExtension() =
  [<Extension>]
  static member inline toSeq (qbc:QueueingBasicConsumer) = 
    seq {
      while true do
        yield qbc.Queue.Dequeue()
    }

  [<Extension>]
  static member inline toSeq (qbc:QueueingBasicConsumer, ms : int) = 
    seq {
      while true do
        let mutable args : Events.BasicDeliverEventArgs = null
        if qbc.Queue.Dequeue(ms, ref args) then
          yield args
        else
          yield null
    }

    // TODO: for x-priority and other args, integrate this with X_Pair helpers
    // TODO: solve having common definitions for both the service and the consumer
    //       a. attributes in a mutually accessible object in same assembly, or in another assembly

//  let openConsumer (cc:IModel) (consrDef:ConsrDef option) (queue:MQDef) (args:IDictionary<string,obj> option) =
//    let uniqueName = openQueue cc queue                                     // uniqueName should only be different if "exclusive" is set
//    let consr= QueueingBasicConsumer(cc)
//    let args = args |> (function | Some(x)->x | None -> null)
//    cc.BasicConsume(uniqueName, consrDef.Value.noAck, consr) |> ignore      
//    consr


  /// dsc: creates a (N-to-1) consumer
//  let openConsumerByDef (cc:IModel) (consrDef:ConsrDef option) (consrQName:string option) (routes:RoutingPair list option)=
//    let uniqueQName = openQueueByDef cc consrDef consrQName routes
//    let consr= QueueingBasicConsumer(cc)
//
//    let noAck = consrDef |> (function | Some(x)->x.noAck | None ->false)
//    let noLocal = consrDef |> (function | Some(x)->x.noLocal | None ->false)
//    let exclusive = consrDef |> (function | Some(x)->x.exclusive | None ->false)
//
//    cc.BasicConsume(uniqueQName, noAck, consr) |> ignore
//    // add additional properties in here
//    consr
//
//  let openConsumerByDef1to1 (cc:IModel) (consrDef:ConsrDef option) (consrMqNme:string option)=
//    openConsumerByDef cc consrDef consrMqNme None
//
//  let openSubscriberByDef (cc:IModel) (consrDef:ConsrDef option) (consrQName:string option) (routes:RoutingPair list option)=
//    let mqKey = openQueueByDef cc consrDef consrQName routes
//    new RabbitMQ.Client.MessagePatterns.Subscription(cc,mqKey,consrDef.Value.noAck)
//
//  let openSubscriberByDef1to1 (cc:IModel) (consrDef:ConsrDef option) (consrQName:string option)=
//    openSubscriberByDef cc consrDef consrQName

  // what is the difference between a subscriber and a consumer?
  // should consumers or subscribers hit the cache first?  Answer: no
  // 

open FactoryHelper
open RegisterHelper
open Register

type CcFactory(cx0, cc0, autoRegister0) =

  member val cx : IConnection = cx0
  
  member val cc : IModel = cc0

  member val autoRegister : bool = autoRegister0

  member this.RegisterQueue(mqDefs:MQDef list) = 
    Register.RegisterQueues(mqDefs)
  member this.RegisterQueues(mqDef:MQDef) = 
    Register.RegisterQueue(mqDef)

  member this.RegisterExchanges(exchDefs:ExDef list) = 
    Register.RegisterExchanges(exchDefs)
  member this.RegisterExchange(exchDef:ExDef) = 
    Register.RegisterExchange(exchDef)

  member this.openExchangeByLU exKey =
    openExchangeByLU this.cc exKey

  member this.openQueueByLU qNme passive =
    openQueueByLU this.cc qNme passive

  member this.openExchange (exchange:ExDef) =    
    if this.autoRegister then
      if not <| isQueueRegistered exchange.name then
        RegisterExchange exchange |> ignore
    
    openExchange this.cc exchange
  
  /// note: queue does not behave in a immutable way.  ie. queue.uniqueName will get set, and this may be different to queue.name depending upon the context (see rabbit documentation and the passive flag for more information)
  member this.openQueue (queue: MQDef) =
    if this.autoRegister && not <| (queue.noCache || isQueueRegistered queue.name) then
        RegisterQueue queue |> ignore

    if (queue.checkIfAlive) || not<|isQueueOpen queue.name then
      queue.uniqueName <- openQueue this.cc queue

    if this.autoRegister && not <| (queue.noCache) then
        setQueueOpen queue

  member this.openSubscriber (consrDef:ConsrDef option) (queue:MQDef) = 
    FactoryHelper.openSubscriber this.cc consrDef queue true

  member this.openConsumer (consrDef:ConsrDef option) (queue:MQDef) = 
    FactoryHelper.openConsumer this.cc consrDef queue

  //member this.openSubscriber (consrDef:ConsrDef option) (queue:MQDef) = 
    

//  member this.openSubscriberByDef (consrDef:ConsrDef option) (consrQName:string option) = 
//    FactoryHelper.openSubscriberByDef this.cc consrDef consrQName

//  member this.openConsumerByDef (consrDef:ConsrDef option) (consrQName:string option) (routes:RoutingPair list) = 
//    FactoryHelper.openConsumerByDef this.cc consrDef consrQName (Some routes)

//  member this.openConsumer1to1 (consrDef:ConsrDef option) (consrQName:string option) = 
//    // ie. an assumption is made about what the queue name should be
//    FactoryHelper.openConsumer1to1 this.cc consrDef consrQName

  new(cx0, cc0) = CcFactory(cx0,cc0,true)

/// dsc: we make connections, channels, exchanges, queues, and bindings
type Factory(cxDef0 : CxDef option) = 
  // management plugin location: http://localhost:15672/

  let mutable cxDef : CxDef option = cxDef0

  let mutable factory : ConnectionFactory = 
    match cxDef with 
    | None -> null
    | Some(cxDef) -> _connectFactory cxDef

  let mutable cx :  IConnection = 
    match cxDef with
    | None -> null
    | Some(cxDef) -> cxDef.AutoConnect |> (function | true->connect0 factory (Some cxDef) | false -> null)

  member val Connection = cx with get

  member this.OpenConnection() =
    cx <- connect0 factory cxDef
    cx

  member this.OpenChannel() =
    cx.CreateModel()

  member this.OpenChannel(qos : QosDef option) =
    let cc = this.OpenChannel()
    match qos with 
    | Some(qos)->cc.BasicQos(qos.prefetchSize, qos.prefetchCount, qos._global) 
    |_->()
    cc

  member this.OpenCcFactory(qos : QosDef option) =
    let cc = this.OpenChannel()
    match qos with 
    | Some(qos)-> cc.BasicQos(qos.prefetchSize, qos.prefetchCount, qos._global) 
    |_->()
    CcFactory(cx,cc)

  new() = Factory(Option<CxDef>.None)
  new(connMeta0) = Factory(Some(connMeta0))
open RabbitMQ.Client


// example uri:  "amqps://guest:guest@Server2"

// TODO: test serialising each of the classes
// TODO: find ways to resolve all extensions
// TODO: if a definition for the exhange is not defined, do not fail.

// let assertExchangeByLU
// let assertQueueByLU

// Q. how can we unify the different ways of defining and declaring queues, exchanges, and exchange-to-exchange bindings?

// todo: how would a cascade style look?
// todo: create a RegisterThenopenQueue, RegisterThenopenExchange

// exchangeMeta.openQueue

// defined exceptions (no failwith)
  // exception QueueAlreadyDefined of string
  // exception ExchangeAlreadyDefined of string


