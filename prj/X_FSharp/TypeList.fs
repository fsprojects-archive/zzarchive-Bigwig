namespace FSharp.Control.X_FSharp

open System
open FSharp.Control.X_FSharp

module TypeList =

  let (|IsExactType1|_|) (x : obj) (yt : Type) = 
    if X_Type.IsExact(x.GetType(), yt) then 
      Some(x) 
    else 
      None

  /// get all attributes from 'from of type 'ofT
  let choose (ofT : Type) (L:'a list) =
    L 
    |> List.filter(fun x-> ofT.Is1 x)

  let pick (ofT : Type) fromL =
    fromL
    |> choose ofT
    |> List.pick(fun x->Some <| x)

  let tryPick ofT fromL =
    fromL
    |> choose ofT 
    |> List.tryPick(fun x->Some <| (x:> obj :?> 'ofT))

  let spick ofT fromL =
    fromL
    |> choose ofT 
    |> List.map(fun x->x:> obj :?> 'ofT)
    |> List.spick

  let trySPick ofT fromL =
    fromL
    |> choose ofT 
    |> List.map(fun x->x:> obj :?> 'ofT)
    |> List.trySPick  

  /// get all attributes from 'from of type 'ofT
  let choose1<'ofT,'from>(L : 'from list) =
    let ofParent = typedefof<'ofT>
    L 
    |> List.filter(fun x-> ofParent.Is1 x)
    |> List.map(fun x->x :> obj :?> 'ofT)  

  let pick1<'ofT,'from>(fromL : 'from list) =
    fromL
    |> choose1<'ofT,'from> 
    |> List.pick(fun x->Some(x:> obj :?> 'ofT))

  /// get the first element of type 'a from 'src list
  let tryPick1<'ofT,'from>(fromL : 'from list) =
    choose1<'ofT,'from> fromL
    |> List.tryPick(fun x->Some(x:> obj :?> 'ofT))

  let spick1<'ofT,'from>(fromL : 'from list) =
    fromL
    |> choose1<'ofT,'from> 
    |> List.map(fun x->x:> obj :?> 'ofT)
    |> List.spick

  let trySPick1<'ofT,'from>(fromL : 'from list) =
    fromL
    |> choose1<'ofT,'from> 
    |> List.map(fun x->x:> obj :?> 'ofT)
    |> List.trySPick

  // REFERENCE:
  // information on the C# "is":  http://msdn.microsoft.com/en-us/library/scekt9xw.aspx
  // http://stackoverflow.com/questions/4963160/how-to-determine-if-a-type-implements-an-interface-with-c-sharp-reflection
  // typeof(IMyInterface).IsAssignableFrom(typeof(MyType))
  // typeof(MyType).GetInterfaces().Contains(typeof(IMyInterface))


