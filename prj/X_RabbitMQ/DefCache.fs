namespace bigwig.X_RabbitMQ

open System
open System.Collections.Generic
open bigwig.X_RabbitMQ
open bigwig.X_FSharp

module DefCache =

  let cache = Dictionary<Guid * string, Attribute list>()

  let choose2 (searchSpec:string) (fromObj) =
    let key = ((fromObj.GetType().GUID), searchSpec)
    match Dict.tryGet key cache with
    | Some(v) -> v
    | None -> 
        let L = DefMatch.choose2 searchSpec fromObj
        Dict.add (key,L) cache
        L
  
  let remove (searchSpec:string) (fromObj) = 
    let key = ((fromObj.GetType().GUID), searchSpec)
    if cache.ContainsKey(key) then
      cache.Remove(key)
    else
      false

