namespace Aardvark.Data

open Aardvark.Base
open Aardvark.Data
open Aardvark.Base.Rendering
open System
open Aardvark.Import.JS
open Microsoft.FSharp.Collections
open System
open Aardvark.SceneGraph

type Cell(x : int64, y : int64, z : int64, e : int) =
    let bb, center =
        let d = 2.0 ** float e
        let isCenteredAtOrigin = x = System.Int64.MaxValue && y = System.Int64.MaxValue && z = System.Int64.MaxValue
        let min = if isCenteredAtOrigin then V3d(-0.5 * d, -0.5 * d, -0.5 * d) else V3d(float x * d, float y * d, float z * d)
        Box3d(min, min + V3d(d, d, d)), min + 0.5 * V3d(d, d, d)
    
    member __.X = x
    member __.Y = y
    member __.Z = z
    member __.E = e
    member x.BoundingBox = bb
    member x.Center = center

    override __.ToString() = sprintf "%d_%d_%d_%d" x y z e

type Stream (data : ArrayBuffer) =
    let mutable offset = 0
    let view = DataView.Create(data)


    member x.ReadGuid() =
        let res = System.Guid(unbox<byte[]> (Uint8Array.Create(data, offset, 16)))
        offset <- offset + 16
        res

    member x.ReadInt8() =
        let res = view.getInt8(offset)
        offset <- offset + 1
        res 

    member x.ReadInt16() =
        let res = view.getInt16(offset, true)
        offset <- offset + 2
        res

    member x.ReadInt32() =
        let res = view.getInt32(offset, true)
        offset <- offset + 4
        res

    member x.ReadInt64() =
        let l = view.getUint32(offset, true)    |> uint64
        let h = view.getUint32(offset+4, true)  |> uint64
        offset <- offset + 8
        (h <<< 32) ||| l |> int64
            
        
    member x.ReadFloat32() =
        let res = view.getFloat32(offset, true)
        offset <- offset + 4
        res
        
    member x.ReadFloat64() =
        let res = view.getFloat64(offset, true)
        offset <- offset + 8
        res
    member x.ReadC3b() =
        let r = view.getUint8(offset) |> uint32
        let g = view.getUint8(offset+1) |> uint32
        let b = view.getUint8(offset+2) |> uint32
        offset <- offset + 3
        (r <<< 16) ||| (g <<< 8) ||| b
        
    
    member x.ReadGuidArray(cnt : int) =
        let arr = Array.zeroCreate cnt
        for i in 0 .. cnt - 1 do arr.[i] <- x.ReadGuid()
        arr

    member x.ReadInt8Array(cnt : int) =
        let res = Int8Buffer(data, offset, cnt)
        offset <- offset + cnt 
        res
    member x.ReadInt16Array(cnt : int) =
        let res = Int16Buffer(data, offset, cnt)
        offset <- offset + cnt * 2
        res
    member x.ReadInt32Array(cnt : int) =
        let res = Int32Buffer(data, offset, cnt)
        offset <- offset + cnt * 4
        res
    member x.ReadFloat64Array(cnt : int) =
        let res = Float64Buffer(data, offset, cnt)
        offset <- offset + cnt * 8
        res
    member x.ReadC3bArray(cnt : int) =
        let res = C3bBuffer(data, offset, cnt)
        offset <- offset + 3 * cnt 
        res

    member x.ReadC4bArray(cnt : int) =
        let res = C4bBuffer(data, offset, cnt)
        offset <- offset + 4 * cnt 
        res

    member x.ReadIntArray(cnt : int) =
        let res = Int32Buffer(data, offset, cnt)
        offset <- offset + 4 * cnt 
        res

    member x.ReadV2iArray(cnt : int) =
        let res = V2iBuffer(data, offset, cnt)
        offset <- offset + 8 * cnt 
        res

    member x.ReadV3iArray(cnt : int) =
        let res = V3iBuffer(data, offset, cnt)
        offset <- offset + 12 * cnt 
        res
        
    member x.ReadV4iArray(cnt : int) =
        let res = V4iBuffer(data, offset, cnt)
        offset <- offset + 16 * cnt 
        res
    
    member x.ReadFloat32Array(cnt : int) =
        let res = Float32Buffer(data, offset, cnt)
        offset <- offset + 4 * cnt 
        res

    member x.ReadV2fArray(cnt : int) =
        let res = V2fBuffer(data, offset, cnt)
        offset <- offset + 8 * cnt 
        res

    member x.ReadV3fArray(cnt : int) =
        let res = V3fBuffer(data, offset, cnt)
        offset <- offset + 12 * cnt 
        res
        
        
    member x.ReadV3dArray(cnt : int) =
        let res = V3dBuffer(data, offset, cnt)
        offset <- offset + 24 * cnt 
        res
    member x.ReadV4fArray(cnt : int) =
        let res = V4fBuffer(data, offset, cnt)
        offset <- offset + 16 * cnt 
        res


    member x.ReadV3f() =
        let a = x.ReadFloat32()
        let b = x.ReadFloat32()
        let c = x.ReadFloat32()
        V3d(float a, float b, float c)
        
    member x.ReadV3d() =
        let a = x.ReadFloat64()
        let b = x.ReadFloat64()
        let c = x.ReadFloat64()
        V3d(a, b, c)

    member x.ReadBox3f() =
        let min = x.ReadV3f()
        let max = x.ReadV3f()
        Box3d(min, max)

    member x.ReadBox3d() =
        let min = x.ReadV3d()
        let max = x.ReadV3d()
        Box3d(min, max)

module DurableDataCodec =

    /////////////////////////////////////////////////////////////////////////
    // Encoding


    //let private encoders = Dict<Guid, Stream -> obj -> unit>(Unchecked.hash, Unchecked.equals)
    
    //let private encodeByteArray (stream : Stream) buffer = stream.Write(buffer, 0, buffer.Length)
    //let private encodeInt8 (stream : IStream) (x : int8) = stream.Write x
    //let private encodeInt16 (stream : IStream) (x : int16) = stream.Write x
    //let private encodeInt32 (stream : IStream) (x : int32) = stream.Write x
    //let private encodeInt64 (stream : IStream) (x : int64) = stream.Write x
    //let private encodeFloat32 (stream : IStream) (x : float32) = stream.Write x
    //let private encodeFloat64 (stream : IStream) (x : float) = stream.Write x
    //let private encodeGuid (stream : IStream) (x : Guid) = stream.Write x

    //let encode (stream : IStream) (key : Durable.Def) (value : obj) =
    //    if key.Type = Guid.Empty then
    //        let encode = encoders.[key.Id]
    //        encode stream value
    //    else
    //        let encode = encoders.[key.Type]
    //        encodeGuid stream key.Id
    //        encode stream value

    //encoders.[Durable.Primitives.guid.Id] <- fun stream o -> encodeGuid stream (o :?> Guid)
    //encoders.[Durable.Primitives.Int32.Id] <- fun stream o -> encodeInt32 stream (o :?> int32)
    //encoders.[Durable.Primitives.Int64.Id] <- fun stream o -> encodeInt64 stream (o :?> int64)
    //encoders.[Durable.Primitives.Float32.Id] <- fun stream o -> encodeFloat32 stream (o :?> float32)
    //encoders.[Durable.Primitives.Float64.Id] <- fun stream o -> encodeFloat64 stream (o :?> float)
    //encoders.[Durable.Primitives.V3f.Id] <- fun stream o -> stream.Write (o :?> V3f)
    //encoders.[Durable.Primitives.V3d.Id] <- fun stream o -> stream.Write (o :?> V3d)
    
    //encoders.[Durable.Primitives.Cell.Id] <- fun stream o ->
    //    let x = o :?> Cell
    //    encodeInt64 stream x.X
    //    encodeInt64 stream x.Y
    //    encodeInt64 stream x.Z
    //    encodeInt32 stream x.Exponent

    //encoders.[Durable.Primitives.Box3f.Id] <- fun stream o -> stream.Write (o :?> Box3f)
    //encoders.[Durable.Primitives.Box3d.Id] <- fun stream o -> stream.Write (o :?> Box3d)

    //encoders.[Durable.Primitives.DurableMap.Id] <- fun stream o ->
    //    let x = o :?> Map<Durable.Def, obj>
    //    // write number of elements
    //    encodeInt32 stream x.Count
    //    // write entries
    //    x |> Map.iter (encode stream)

    //let private genArrayEncoder<'a when 'a : unmanaged> () = fun (stream : IStream) (o : obj) ->
    //    let xs = o :?> 'a[]
    //    stream.Write(xs, 0, xs.Length)
    //    //let sizeInBytes = sizeof<'a> * xs.Length
    //    //use p = fixed xs
    //    //let q = (NativePtr.toNativeInt p).ToPointer()
    //    //let span = ReadOnlySpan(q, sizeInBytes)

    //    //// write number of elements
    //    //encodeInt32 stream xs.Length
    //    //// write elements
    //    //stream.Write(span)
        
    //encoders.[Durable.Primitives.GuidArray.Id]     <- genArrayEncoder<Guid> ()
    //encoders.[Durable.Primitives.Int8Array.Id]     <- genArrayEncoder<int8> ()
    //encoders.[Durable.Primitives.Int16Array.Id]    <- genArrayEncoder<int16> ()
    //encoders.[Durable.Primitives.Int32Array.Id]    <- genArrayEncoder<int32> ()
    //encoders.[Durable.Primitives.Int64Array.Id]    <- genArrayEncoder<int64> ()
    //encoders.[Durable.Primitives.Float32Array.Id]  <- genArrayEncoder<float32> ()
    //encoders.[Durable.Primitives.Float64Array.Id]  <- genArrayEncoder<float> ()
    //encoders.[Durable.Primitives.V3fArray.Id]      <- genArrayEncoder<V3f> ()
    //encoders.[Durable.Primitives.V3dArray.Id]      <- genArrayEncoder<V3d> ()
    //encoders.[Durable.Primitives.C3bArray.Id]      <- genArrayEncoder<C3b> ()
        
    
    /////////////////////////////////////////////////////////////////////////
    // Decoding

    let private decoders = Dict<Guid, Stream -> obj>(Unchecked.hash, Unchecked.equals)

    let private decodeInt8 (stream : Stream) : int8 = stream.ReadInt8()
    let private decodeInt16 (stream : Stream) : int16 = stream.ReadInt16()
    let private decodeInt32 (stream : Stream) : int32 = stream.ReadInt32()
    let private decodeInt64 (stream : Stream) : int64 = stream.ReadInt64()
    let private decodeFloat32 (stream : Stream) : float32 = stream.ReadFloat32()
    let private decodeFloat64 (stream : Stream) : float = stream.ReadFloat64()
    let private decodeGuid (stream : Stream) : Guid = stream.ReadGuid()
    let private decodeV3f (stream : Stream) : V3d = stream.ReadV3f()
    let private decodeV3d (stream : Stream) : V3d = stream.ReadV3d()

    let decode (stream : Stream) : Durable.Def * obj =
        let key = decodeGuid stream
        let def = Durable.get key
        if def.Type = Guid.Empty then
            let decode = decoders.[key]
            (def, decode stream)
        else
            let decode = decoders.[def.Type]
            (def, decode stream)

    let decodeMap (stream : Stream) : Durable.Def * Map<Durable.Def, obj> =
        let (def, o) = decode stream
        (def, o :?> Map<Durable.Def, obj>) 

    decoders.[Durable.Primitives.guid.Id] <- fun stream -> (decodeGuid stream) :> obj
    decoders.[Durable.Primitives.Int8.Id] <- fun stream -> (decodeInt8 stream) :> obj
    decoders.[Durable.Primitives.Int16.Id] <- fun stream -> (decodeInt16 stream) :> obj
    decoders.[Durable.Primitives.Int32.Id] <- fun stream -> (decodeInt32 stream) :> obj
    decoders.[Durable.Primitives.Int64.Id] <- fun stream -> (decodeInt64 stream) :> obj
    decoders.[Durable.Primitives.Float32.Id] <- fun stream -> (decodeFloat32 stream) :> obj
    decoders.[Durable.Primitives.Float64.Id] <- fun stream -> (decodeFloat64 stream) :> obj
    decoders.[Durable.Primitives.V3f.Id] <- fun stream -> decodeV3f stream :> obj
    decoders.[Durable.Primitives.V3d.Id] <- fun stream -> decodeV3d stream :> obj
    decoders.[Durable.Primitives.Cell.Id] <- fun stream -> Cell(decodeInt64 stream, decodeInt64 stream, decodeInt64 stream, decodeInt32 stream) :> obj
    decoders.[Durable.Primitives.Box3f.Id] <- fun stream -> stream.ReadBox3f() :> obj
    decoders.[Durable.Primitives.Box3d.Id] <- fun stream -> stream.ReadBox3d() :> obj

    decoders.[Durable.Primitives.DurableMap.Id] <- fun stream ->
        let mutable map = Map.empty
        // read number of elements
        let count = decodeInt32 stream
        // read entries
        for i in 0..count-1 do
            let (def, o) = decode stream
            map <- map |> Map.add def o
        
        map :> obj

    let private genArrayDecoder (inner : Stream -> int -> 'a) (stream : Stream) =
        let count = decodeInt32 stream
        inner stream count :> obj
    //    let xs = Array.zeroCreate<'a> count
    //    let s = stream.Read(xs, 0, xs.Length)
    //    let sizeInBytes = sizeof<'a> * count
    //    if s <> sizeInBytes then failwith "Invariant 42c79483-4b9f-4ba8-b8fc-3074a7aa7d5c."
    //    xs :> obj
    //    //use p = fixed xs
    //    //let q = (NativePtr.toNativeInt p).ToPointer()
    //    //let span = Span(q, sizeInBytes)
    //    //let s = stream.Read(span)
    //    //if s <> sizeInBytes then failwith "Invariant 42c79483-4b9f-4ba8-b8fc-3074a7aa7d5c."
    //    //xs :> obj

    decoders.[Durable.Primitives.GuidArray.Id]     <- genArrayDecoder <| fun s -> s.ReadGuidArray
    decoders.[Durable.Primitives.Int8Array.Id]     <- genArrayDecoder <| fun s -> s.ReadInt8Array
    decoders.[Durable.Primitives.Int16Array.Id]    <- genArrayDecoder <| fun s -> s.ReadInt16Array
    decoders.[Durable.Primitives.Int32Array.Id]    <- genArrayDecoder <| fun s -> s.ReadInt32Array
    //decoders.[Durable.Primitives.Int64Array.Id]    <- genArrayDecoder <| fun s -> s.ReadInt64Array
    decoders.[Durable.Primitives.Float32Array.Id]  <- genArrayDecoder <| fun s -> s.ReadFloat32Array
    decoders.[Durable.Primitives.Float64Array.Id]  <- genArrayDecoder <| fun s -> s.ReadFloat64Array
    decoders.[Durable.Primitives.V3fArray.Id]      <- genArrayDecoder <| fun s -> s.ReadV3fArray
    decoders.[Durable.Primitives.V3dArray.Id]      <- genArrayDecoder <| fun s -> s.ReadV3dArray
    decoders.[Durable.Primitives.C3bArray.Id]      <- genArrayDecoder <| fun s -> s.ReadC3bArray