module OM

open System

/// [def] a policy is somewhat similar to the arguments applied in exchange.declare and queue.declare
/// except that they are applied automatically without the involvement of the client application, 
/// and they can change at any time. [1] Note that the set of features which can be controlled by policy 
/// is not the same as the set of features which can be controlled by arguments.
/// [2] Each exchange or queue will have at most one policy matching
type Policy() = 
  inherit Attribute()
  member val pattern = "" with get,set
  /// a json document
  member val definition = "" with get,set
  member val priority = 0 with get,set
  member val applyTo = "" with get,set

