namespace FSharp.Control.RabbitMQ

open System.Runtime.CompilerServices
open RabbitMQ.Client
open RabbitMQ.Client.Events
open System

// TODO: currently can't access any of these methods in the type extension format

[<Extension>]
type ModelExtensions() =

  /// a is descended from, or is the same as
  [<Extension>]
  static member inline ack (m:IModel,e:BasicDeliverEventArgs) =
    m.BasicAck(e.DeliveryTag,false)

  [<Extension>]
  static member inline nack (m:IModel,e:BasicDeliverEventArgs,requeue) =
    m.BasicNack(e.DeliveryTag,false,requeue)

