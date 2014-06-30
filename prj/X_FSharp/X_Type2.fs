namespace bigwig.X_FSharp

open System
open bigwig.X_FSharp
open bigwig.X_FSharp.List
open System.Runtime.CompilerServices

[<Extension>]
type X_Type2() =

  /// get all attributes from 'from of type 'ofT
  [<Extension>]
  static member inline choose<'ofT>(typ:Type) =
    typ.GetCustomAttributes(true)
    |> List.ofArray
    |> List.filter(fun x-> x.Is1<'ofT>())
    |> List.map(fun x->x :?> 'ofT)

  /// get the first element of type 'a from 'src list
  [<Extension>]
  static member inline tryPick<'ofT>(typ : Type) =
    typ.GetCustomAttributes(true)
    |> List.ofArray
    |> List.filter(fun x->x.Is1<'ofT>())
    |> List.map(fun x->x :?> 'ofT)
    |> List.tryPick(fun x->Some x)

  [<Extension>]
  static member inline spick<'ofT>(typ : Type) =
    typ.GetCustomAttributes(true)
    |> List.ofArray
    |> List.filter(fun x->x.Is1<'ofT>())
    |> List.map(fun x->x :?> 'ofT)
    |> List.spick

  [<Extension>]
  static member inline pick<'ofT>(typ : Type) =
    typ.GetCustomAttributes(true)
    |> List.ofArray
    |> List.filter(fun x-> x.Is1<'ofT>())
    |> List.map(fun x->x :?> 'ofT)
    |> List.pick(fun x->Some(x))

  [<Extension>]
  static member inline trySPick<'ofT>(typ : Type) =
    typ.GetCustomAttributes(true)
    |> List.ofArray
    |> List.filter(fun x-> x.Is1<'ofT>())
    |> List.map(fun x->x :?> 'ofT)
    |> trySPick
