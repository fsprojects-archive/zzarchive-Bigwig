namespace FSharp.Control.X_FSharp

open System
open FSharp.Control.X_FSharp
open FSharp.Control.X_FSharp.List
open Microsoft.FSharp.Reflection

module Interface =

  // may discover implemented interface, but it does not retrieve interface values

  /// get <T> from a list of interface markers
  /// where ofT = typedefof<abc<_>>
  let chooseMarker(ofT:Type list,fromT:Type) =
    fromT.GetInterfaces() 
    |> Array.choose(fun (x:Type)->
                      if ofT |> List.exists(fun oft->x.DeclaringType = oft.DeclaringType) then
                        match x.GenericTypeArguments with
                        | [|x|] -> Some(x)
                        | _ -> None
                      else
                        None
                    )
    |> Array.toList

  /// get <T,U> from a list of interface markers
  let chooseMarker2(ofT:Type list,fromT:Type) =
    fromT.GetInterfaces() 
    |> Array.choose(fun (x:Type)->
                      if ofT |> List.exists(fun oft->x.DeclaringType = oft.DeclaringType) then
                        match x.GenericTypeArguments with
                        | [|x;y|] -> Some(x,y)
                        | _ -> None
                      else
                        None
                    )
    |> Array.toList
  /// get <T,U,V> from a list of interface markers
  let chooseMarker3(ofT:Type list,fromT:Type) =
    fromT.GetInterfaces() 
    |> Array.choose(fun (x:Type)->
                      if ofT |> List.exists(fun oft->x.DeclaringType = oft.DeclaringType) then
                        match x.GenericTypeArguments with
                        | [|x;y;z|] -> Some(x,y,z)
                        | _ -> None
                      else
                        None
                    )
    |> Array.toList

  let chooseMarker1G<'ofT,'fromT> =
    chooseMarker([typedefof<'ofT>],typedefof<'fromT>)

  // TODO: F# currently does not support multiple implementations of the same interface with different generic types 

  /// get monadic
  let chooseMarker1G_2<'ofT,'ofT2,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>],typedefof<'fromT>)
  /// get monadic
  let chooseMarker1G_3<'ofT,'ofT2,'ofT3,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>;typedefof<'ofT3>],typedefof<'fromT>)
  /// get monadic
  let chooseMarker1G_4<'ofT,'ofT2,'ofT3,'ofT4,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>;typedefof<'ofT3>;typedefof<'ofT4>],typedefof<'fromT>)
  /// get monadic
  let chooseMarker1G_5<'ofT,'ofT2,'ofT3,'ofT4,'ofT5,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>;typedefof<'ofT3>;typedefof<'ofT4>;typedefof<'ofT5>],typedefof<'fromT>)
  /// get monadic
  let chooseMarker1G_6<'ofT,'ofT2,'ofT3,'ofT4,'ofT5,'ofT6,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>;typedefof<'ofT3>;typedefof<'ofT4>;typedefof<'ofT5>;typedefof<'ofT6>],typedefof<'fromT>)
  /// get monadic
  let chooseMarker1G_7<'ofT,'ofT2,'ofT3,'ofT4,'ofT5,'ofT6,'ofT7,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>;typedefof<'ofT3>;typedefof<'ofT4>;typedefof<'ofT5>;typedefof<'ofT6>;typedefof<'ofT7>],typedefof<'fromT>)
  /// get monadic
  let chooseMarker1G_8<'ofT,'ofT2,'ofT3,'ofT4,'ofT5,'ofT6,'ofT7,'ofT8,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>;typedefof<'ofT3>;typedefof<'ofT4>;typedefof<'ofT5>;typedefof<'ofT6>;typedefof<'ofT7>;typedefof<'ofT8>],typedefof<'fromT>)
  /// get monadic
  let chooseMarker1G_9<'ofT,'ofT2,'ofT3,'ofT4,'ofT5,'ofT6,'ofT7,'ofT8,'ofT9,'fromT> =
    chooseMarker([typedefof<'ofT>;typedefof<'ofT2>;typedefof<'ofT3>;typedefof<'ofT4>;typedefof<'ofT5>;typedefof<'ofT6>;typedefof<'ofT7>;typedefof<'ofT8>;typedefof<'ofT9>],typedefof<'fromT>)

  /// get dyadic
  let chooseMarker2G<'ofT,'fromT> =
    chooseMarker2([typedefof<'ofT>],typedefof<'fromT>)

  /// get triadic
  let chooseMarker3G<'ofT,'fromT> =
    chooseMarker3([typedefof<'ofT>],typedefof<'fromT>)


//module Interface = 
  let tryPick0(a : Type, o) =
    o.GetType().GetInterfaces()  
    |> Array.filter(fun x->x=a) 
    |> Array.tryPick(fun x->Some x)

  let tryPick<'a>(o) =
    tryPick0(typedefof<'a>,o)
  
  /// conditionally get interface values from a type when the shape of the type is not yet known
  /// we know no more than a given type may have certain interface marker generics and/or values through an interface implementation
  /// 't: the interface you are interested in
  /// o: the object you suspect may hold an interface implementation
  let tryPickData0(a : Type, o) =
    let i = tryPick0(a, o)
    match i with
    | Some(i) ->
        let T = if i.IsGenericType then i.GetGenericArguments() else [||]     // generic type
        let imap=o.GetType().GetInterfaceMap(i)                               // get mapping between the interface and the instance
        let O = (imap.InterfaceMethods |> Array.map(fun x->x.ReturnType))     // getter return types
        let t = FSharpType.MakeTupleType(T |> Array.append O)                 // make combined tuple type of generics + getter returns
        let V = 
              imap.InterfaceMethods 
              |> Array.filter(fun x->x.ReturnType <> typedefof<System.Void>)
              |> Array.filter(fun x->x.GetParameters().Length = 0)
              |> Array.map(fun x->x.Invoke(o,null))                           // instance 
        let v= FSharpValue.MakeTuple(V, t)                                    // make the the tuple itself
        Some v
    | None -> None 

  let tryPickData<'a>(o) =
    tryPickData0(typedefof<'a>,o)