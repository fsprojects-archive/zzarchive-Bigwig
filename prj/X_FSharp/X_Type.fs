namespace bigwig.X_FSharp

open System
open bigwig.X_FSharp.List
open System.Runtime.CompilerServices

// TODO: currently can't access any of these methods in the type extension format

[<Extension>]
type X_Type() =

  /// a is descended from, or is the same as
  [<Extension>]
  static member inline Is (xt:Type, ofYt:Type) = 
    if ofYt.IsInterface then
      ofYt.IsAssignableFrom(xt)
    else      
      xt=ofYt || xt.IsSubclassOf(ofYt)

  [<Extension>]
  static member inline Is (xt:Type, L:Type list) = 
    L|>List.exists(fun x->X_Type.Is(xt,x))

  [<Extension>]
  static member inline Is (X:Type list, L:'a list) = 
    X|>List.filter(fun x->X_Type.Is(x.GetType(),L|>List.map(fun x->x.GetType())))
      
  [<Extension>]
  static member inline Is1 (ofYt:Type,x) = 
    X_Type.Is (x.GetType(), ofYt)

  /// a is descended from, or is the same as
  [<Extension>]
  static member inline Is<'xt,'ofYt>() = 
    X_Type.Is(typedefof<'xt>,typedefof<'ofYt>)

  [<Extension>]
  static member inline Is<'ofYt>(x:Type) = 
    X_Type.Is(x,typedefof<'ofYt>)

  [<Extension>]
  static member inline Is1<'ofYt>(x) = 
    X_Type.Is(x.GetType(),typedefof<'ofYt>)

  [<Extension>]
  static member inline IsExact(xt:Type,yt:Type) = 
    xt = yt

  /// must be an exact match
  [<Extension>]
  static member inline IsExact<'y>(x) = 
    x.GetType() = typedefof<'y>

  [<Extension>]
  static member inline TryAs<'ty>(x) =
    try 
      Some(x :> obj :?> 'ty)
    with
      | _->None

  [<Extension>]
  static member inline ChooseCustom(containerTyp:Type,L:'Ltyp list) =
    containerTyp.GetCustomAttributes(true)
    |> Array.toList
    |> List.filter(fun x->L|>List.exists(fun y->X_Type.Is(x.GetType(), y)))

  [<Extension>]
  static member inline ChooseCustom(container:obj,L:'ofLtyp list) =
    let containerTyp = container.GetType()
    X_Type.ChooseCustom(containerTyp,L)
