open System
open System.Reflection

let t=typeof<int> //.GUID.ToByteArray().Length

// Assembly.GetExecutingAssembly()

Type.GetType(t.FullName)

//let resolveAsm (asmName:AssemblyName)=Assembly.GetExecutingAssembly()) = Assembly.GetExecutingAssembly()

let resolveAsm2 (asmName:AssemblyName)=Assembly.GetExecutingAssembly()

let resolveType (asm:Assembly,name:string,flag:bool)=typeof<int>

//let resolveType2 = Func<Assembly,string,bool,Type>(fun asmNme nme case->typeof<int>)
//let resolveType3 = (fun asmNme nme case->typeof<int>)
let resolveType4 asmNme nme case=typeof<int>
let resolveType2 (asm:Assembly) (nme:string) (case:bool) =typeof<int> 
//,Type>(fun (asmNme:Assembly,nme:string,case:bool)->typeof<int>)

Type.GetType(t.Name,resolveAsm2 ,resolveType2).Name
t.Name


"01234567890123456"

//Type.GetTypeCode(t)

// see TypeCode enum

//t.GUID
Type.GetTypeFromCLSID(t.GUID).Name
//= typeof<int>

Type.GetTypeHandle(t)

Type.GetTypeFromProgID

Type.GetTypeFromProgID(t.GUID.ToString())

t.FullName
t.Name