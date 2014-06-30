namespace clxio.X_RabbitMQ.Meta.Fixture

#r"System.dll"
#r"System.Management.dll"
#r"System.Configuration.dll"
#r"bin/debug/RabbitMQ.Client.dll"
#r"bin/debug/ServiceStack.Client.dll"
#r"bin/debug/clxio.X_FSharp.dll"
#load"QParse.fs"
#load"IOM.fs"
#load"OM.fs"
#load"Register.fs"
#load"DefMatch.fs"
#load"Def.fs"
#load"Context.fs"
open System
open System.Text
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open clxio.X_FSharp
open clxio.X_RabbitMQ
open Microsoft.FSharp.Reflection

type Test() = 
  let x = "x"
  interface IReply0 with
    member val Type = typedefof<int> with get
    member val Grp = "someGrp"  : string with get 
    member val When : string = "someWhen" with get 
    // ie. the reply-to type may need different Grp/When selectors
    member val AltGrp : string option = None with get 
    member val AltWhen : string option = None with get 

module A = 
  
//  let chooseIReplyToT = [typedefof<IReply0>;typedefof<IReply1>;typedefof<IReply2>;typedefof<IReply3>;typedefof<IReply4>;typedefof<IReply5>;typedefof<IReply6>;typedefof<IReply7>;typedefof<IReply8>;typedefof<IReply9>]
//  let chooseReplyTo1 (oa) = 
//    chooseIReplyToT
//    |>List.map(fun t->match Interface.tryPickData0(t,o:>obj) with | Some(v) -> Some(X_ReplyTo1(FSharpValue.GetTupleFields(v))) | None -> None)
//    |>List.choose(fun x->x)

  let o = Test()
  DefMatch.chooseReplyTo1 o
  |> List.map(fun x->x:>Attribute)
  |> DefMatch.choose "g:someGrp"
  let b = (o :> IReply0)



