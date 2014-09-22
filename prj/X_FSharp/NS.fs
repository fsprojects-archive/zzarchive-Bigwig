namespace FSharp.Control.X_FSharp

open System
open System.Diagnostics
open System.Reflection

/// namespace helper module
module NS = 
  /// drops off the FSI module at the front if necessary
  let fullName1 (x:Type) = 
    match (x.FullName) with
    | x when x.StartsWith("FSI_") -> x.Substring(x.IndexOf('.')+1)
    | x -> x

  /// fullname from object instance
  let fullName (x:obj) = 
    fullName1 (x.GetType())

  /// get namespace from type and method info
  let getMemb0 (t:Type) (mi:MethodInfo) =
    sprintf "%s.%s" (fullName t) (mi.Name)

  /// get ns from method info
  let getMemb1 (x:obj) (mi:MethodInfo) =
    sprintf "%s.%s" (fullName x) (mi.Name)

  /// get ns from stack trace
  let getMemb2 (x:obj) (st:StackTrace) =
    let mi = st.GetFrame(0).GetMethod():?>MethodInfo
    getMemb1 x mi

  /// use to get the current namespace, where the method is also a dotted part of it.
  let inline getMemb (x:obj) =
    let mi = StackTrace().GetFrame(0).GetMethod():?>MethodInfo
    getMemb1 x mi

  let inline getClass (x:obj) =
    fullName x

  let inline getClass1 (x:Type) =
    fullName1 x
    