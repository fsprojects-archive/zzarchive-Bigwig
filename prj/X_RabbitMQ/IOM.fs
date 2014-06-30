namespace bigwig.X_RabbitMQ

open System

// when gets applied to the namespace key (nskk)
// when gets applied to MQ for MQDefs, and to Nskk directly for everything else.  Note: empty MQDefs default to the namespace they are defined in.  This is adequate for now

type IMatchGrp = interface abstract member Grp : string with get end
type IMatch0Grp = interface abstract member Grp : string with get end
type IMatch1Grp = interface abstract member Grp : string with get end
type IMatch2Grp = interface abstract member Grp : string with get end
type IMatch3Grp = interface abstract member Grp : string with get end
type IMatch4Grp = interface abstract member Grp : string with get end
type IMatch5Grp = interface abstract member Grp : string with get end
type IMatch6Grp = interface abstract member Grp : string with get end
type IMatch7Grp = interface abstract member Grp : string with get end
type IMatch8Grp = interface abstract member Grp : string with get end
type IMatch9Grp = interface abstract member Grp : string with get end

type IMatchWhen = interface abstract member When : string with get end
type IMatch0When = interface abstract member When : string with get end
type IMatch1When = interface abstract member When : string with get end
type IMatch2When = interface abstract member When : string with get end
type IMatch3When = interface abstract member When : string with get end
type IMatch4When = interface abstract member When : string with get end
type IMatch5When = interface abstract member When : string with get end
type IMatch6When = interface abstract member When : string with get end
type IMatch7When = interface abstract member When : string with get end
type IMatch8When = interface abstract member When : string with get end
type IMatch9When = interface abstract member When : string with get end

type IMatchNskk = interface abstract member Nskk : string with get end

/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef0<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef1<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef2<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef3<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef4<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef5<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef6<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef7<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef8<'t> = interface end
/// import defintions in 't into the current objects meta list / dictionary
type IOpenDef9<'t> = interface end

// currently not implemented, but a cleaner way to implement a qualifier for meta pointed to through the IMatch convention

/// when selected we go to the type specified and pull out the corresponding MQDef
/// 't could be a container
//type IReply0<'t> = interface 
type IReply0 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  // ie. the reply-to type may need different Grp/When selectors
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply1<'t> = interface 
type IReply1 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply2<'t> = interface 
type IReply2 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply3<'t> = interface 
type IReply3 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply4<'t> = interface 
type IReply4 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply5<'t> = interface 
type IReply5 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply6<'t> = interface 
type IReply6 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply7<'t> = interface 
type IReply7 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply8<'t> = interface 
type IReply8 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end
//type IReply9<'t> = interface 
type IReply9 = interface 
  abstract member Type : Type with get
  abstract member Grp : string with get 
  abstract member When : string with get 
  abstract member AltGrp : string option with get 
  abstract member AltWhen : string option with get 
end

/// ie. IReply0<This>
/// NB. the default action when not "This" or "AddrInMsg" is to send to those denoted at the site 't when the selector grp & when are applied
type This()=
  member this.X = "X"

/// ie. IReply0<AddrInMsg>
/// NB. the default action when not "This" or "AddrInMsg" is to send to those denoted at the site 't when the selector grp & when are applied
type AddrInMsg()=
  member this.X = "X"

/// ie. IReply0<AddrInEntity>
//this is the default action.  If not AddrInMsg &
//type AddrInEntity<'t>()=
//  member this.X = "X"

/// Namespace-key-key
/// NB. Ref was intended as a grouping device
/// lazy group specifier could be .some.group1.
/// so if not present, we assume the ns to be the namespace + name of the class
type INskk = 
  /// A Namespace key,var key
  abstract member Nskk : string with get,set
  /// a key or search key for a nskk value
  //abstract member _tag : string with get,set

exception PreconditionFail of string
type IPrecondition = 
  abstract member Pass : unit -> unit

