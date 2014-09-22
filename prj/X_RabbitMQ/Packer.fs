namespace FSharp.Control.RabbitMQ

open System
open System.Text
open System.Reflection
open ServiceStack
open ServiceStack.Text
open ServiceStack.ProtoBuf
open ServiceStack.MsgPack
open FSharp.Control.RabbitMQ
open FSharp.Control.X_FSharp
open System.IO.Compression
open System.IO

exception NoEncodingDefined of string
exception NoCharactersetDefined of string
exception NoContentTypeDefined of string

module String = 
  let toBytes(str : string) =
    let bytes = [|for i=0 to (str.Length * sizeof<char>) do yield byte(0)|]
    System.Buffer.BlockCopy(str.ToCharArray(), 0, bytes, 0, bytes.Length)
    bytes

  let fromBytes(bytes : byte array) =
    let chars = [|for i = 0 to bytes.Length / sizeof<char> do yield byte(0)|]
    System.Buffer.BlockCopy(bytes, 0, chars, 0, bytes.Length)
    string(chars)
 
module Charset = 
  let encode (charsetType:CharsetType) (s:string) =
    match charsetType with
    | RAW -> String.toBytes s
    | UTF_7 -> System.Text.UTF7Encoding.UTF7.GetBytes(s)
    | UTF_8 -> System.Text.UTF8Encoding.UTF8.GetBytes(s)
    | UTF_16 -> System.Text.UnicodeEncoding.Unicode.GetBytes(s)
    | UTF_32 -> System.Text.UTF32Encoding.UTF32.GetBytes(s)
    | CharsetType.NoCharset -> raise <| NoCharactersetDefined("text is encoded with a type of encoding")

  let decode (charsetType:CharsetType) (B:byte array) =
    match charsetType with
    | RAW -> String.fromBytes B
    | UTF_7 -> System.Text.UTF7Encoding.UTF7.GetString(B)
    | UTF_8 -> System.Text.UTF8Encoding.UTF8.GetString(B)
    | UTF_16 -> System.Text.UnicodeEncoding.Unicode.GetString(B)
    | UTF_32 -> System.Text.UTF32Encoding.UTF32.GetString(B)
    | CharsetType.NoCharset -> raise <| NoCharactersetDefined("text is encoded with a type of encoding")
    // see "typically looks like" in https://www.w3.org/International/O-HTTP-charset
    // http://www.unicode.org/faq/utf_bom.html

module Compression = 

  let inflate (encoding:EncodingType) (B:byte array) =
    match encoding with
    | NoEncoding -> B
    | GZip -> 
        use dec = new GZipStream(new MemoryStream(B), CompressionMode.Decompress)
        dec.ToBytes()

  let deflate (encoding:EncodingType) (B:byte array) =
    match encoding with
    | NoEncoding -> B
    | GZip -> 
        use enc = new GZipStream(new MemoryStream(B), CompressionMode.Compress)
        enc.ToBytes()

/// dsc: we try to handle all message encoding and format types in a meta based way using open standards (http-style mime, compression, charsets via headers)
/// Everybody seems to be borrowing from each other in this regard.
/// This may not be the most compact, or efficient way of doing this, but the overhead is low, and the more important thing at this stage
/// of the project is to have a flexible and robust approach to a bunch of unknown transport method and protocol based requirements.
module Packer = 
  let box (contentType:ContentType) (charsetType:CharsetType) (encoding:EncodingType) (msg:'msgTyp) = 
    let bytes = 
      match contentType with
      | JSON     -> Charset.encode charsetType <| msg.ToJson()
      | JSV      -> Charset.encode charsetType <| msg.ToJsv()
      | CSV      -> Charset.encode charsetType <| msg.ToCsv()
      | ProtoBuf -> msg.ToProtoBuf()
      | MsgPack  -> msg.ToMsgPack()
      | NoContentType -> raise<|NoContentTypeDefined("no content type defined")
      |> Compression.deflate encoding
    
    (bytes, X_MessageType(msg.GetType()))

  let unbox<'a> (contentType:ContentType) (charsetType:CharsetType) (encoding:EncodingType) (msg:byte array) =
    
    match contentType with
    | JSON     -> (msg |> Compression.inflate encoding |> Charset.decode charsetType).FromJson<'a>()
    | JSV      -> (msg |> Compression.inflate encoding |> Charset.decode charsetType).FromJsv<'a>()
    | CSV      -> raise<|NYI("CSV deserializer not implemented yet")
    | ProtoBuf -> (msg |> Compression.inflate encoding).FromProtoBuf<'a>()
    | MsgPack  -> (msg |> Compression.inflate encoding).FromMsgPack<'a>()
    | _ -> raise<|NoContentTypeDefined("unrecognised content encoding type")

  type Unboxer() = 
    member this.Unbox<'a> (contentType:ContentType) (charsetType:CharsetType) (encoding:EncodingType) (msg:byte array) =
      unbox<'a> contentType charsetType encoding msg

  let unboxer= Unboxer()

  let unbox1 (content:ContentType) (charset:CharsetType) (encoding:EncodingType) (msgType:X_MessageType) (B:byte array) =
    let typ = Type.GetType(msgType.TypeKey)
    let m = typedefof<Unboxer>.GetMethod("Unbox").MakeGenericMethod([|typ|])
    B |> (fun x->m.Invoke(unboxer,[|(content:>obj);(charset:>obj);(encoding:>obj);(x:>obj)|]))

  let unbox2 (content:X_Content) (charset:X_Charset) (encoding:X_Encoding) (msgType:X_MessageType) (B:byte array) =
    B |> unbox1 content.ContentType (CharsetType.merge content.CharsetType charset.CharsetType) encoding.EncodingType msgType


  // TODO: create a proxy stash for all created generated methods

  // TODO: create a MEF