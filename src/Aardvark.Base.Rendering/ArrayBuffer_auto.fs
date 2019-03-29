namespace Aardvark.Base.Rendering
open Fable.Import.JS
open Aardvark.Base

type Int8Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Int8Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 1
    static member PrimitiveType = Int(true, 8)
    member x.Length = length
    member x.Item
        with get(i : int) =
            int8(store.[i])
        and set(i : int) (v : int8) =
            store.[i] <- float v
    new(cnt : int) = Int8Buffer(ArrayBuffer.Create(float (1 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> int8) = 
        let res = Int8Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : int8) = 
        let res = Int8Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Int8Buffer(cnt)
    static member ofArray (arr : int8[]) = 
        let res = Int8Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<int8>) = Int8Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<int8>) = Int8Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Int8Buffer(arr, byteOffset + 1*start, count)
    interface IArrayBuffer with
        member x.ElementType = Int(true, 8)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<int8> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : int8) = x.[i] <- v
type Int16Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Int16Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 2
    static member PrimitiveType = Int(true, 16)
    member x.Length = length
    member x.Item
        with get(i : int) =
            int16(store.[i])
        and set(i : int) (v : int16) =
            store.[i] <- float v
    new(cnt : int) = Int16Buffer(ArrayBuffer.Create(float (2 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> int16) = 
        let res = Int16Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : int16) = 
        let res = Int16Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Int16Buffer(cnt)
    static member ofArray (arr : int16[]) = 
        let res = Int16Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<int16>) = Int16Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<int16>) = Int16Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Int16Buffer(arr, byteOffset + 2*start, count)
    interface IArrayBuffer with
        member x.ElementType = Int(true, 16)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<int16> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : int16) = x.[i] <- v
type Int32Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Int32Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 4
    static member PrimitiveType = Int(true, 32)
    member x.Length = length
    member x.Item
        with get(i : int) =
            int32(store.[i])
        and set(i : int) (v : int32) =
            store.[i] <- float v
    new(cnt : int) = Int32Buffer(ArrayBuffer.Create(float (4 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> int32) = 
        let res = Int32Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : int32) = 
        let res = Int32Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Int32Buffer(cnt)
    static member ofArray (arr : int32[]) = 
        let res = Int32Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<int32>) = Int32Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<int32>) = Int32Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Int32Buffer(arr, byteOffset + 4*start, count)
    interface IArrayBuffer with
        member x.ElementType = Int(true, 32)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<int32> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : int32) = x.[i] <- v
type Uint8Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Uint8Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 1
    static member PrimitiveType = Int(false, 8)
    member x.Length = length
    member x.Item
        with get(i : int) =
            uint8(store.[i])
        and set(i : int) (v : uint8) =
            store.[i] <- float v
    new(cnt : int) = Uint8Buffer(ArrayBuffer.Create(float (1 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> uint8) = 
        let res = Uint8Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : uint8) = 
        let res = Uint8Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Uint8Buffer(cnt)
    static member ofArray (arr : uint8[]) = 
        let res = Uint8Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<uint8>) = Uint8Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<uint8>) = Uint8Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Uint8Buffer(arr, byteOffset + 1*start, count)
    interface IArrayBuffer with
        member x.ElementType = Int(false, 8)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<uint8> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : uint8) = x.[i] <- v
type Uint16Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Uint16Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 2
    static member PrimitiveType = Int(false, 16)
    member x.Length = length
    member x.Item
        with get(i : int) =
            uint16(store.[i])
        and set(i : int) (v : uint16) =
            store.[i] <- float v
    new(cnt : int) = Uint16Buffer(ArrayBuffer.Create(float (2 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> uint16) = 
        let res = Uint16Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : uint16) = 
        let res = Uint16Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Uint16Buffer(cnt)
    static member ofArray (arr : uint16[]) = 
        let res = Uint16Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<uint16>) = Uint16Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<uint16>) = Uint16Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Uint16Buffer(arr, byteOffset + 2*start, count)
    interface IArrayBuffer with
        member x.ElementType = Int(false, 16)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<uint16> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : uint16) = x.[i] <- v
type Uint32Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Uint32Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 4
    static member PrimitiveType = Int(false, 32)
    member x.Length = length
    member x.Item
        with get(i : int) =
            uint32(store.[i])
        and set(i : int) (v : uint32) =
            store.[i] <- float v
    new(cnt : int) = Uint32Buffer(ArrayBuffer.Create(float (4 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> uint32) = 
        let res = Uint32Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : uint32) = 
        let res = Uint32Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Uint32Buffer(cnt)
    static member ofArray (arr : uint32[]) = 
        let res = Uint32Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<uint32>) = Uint32Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<uint32>) = Uint32Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Uint32Buffer(arr, byteOffset + 4*start, count)
    interface IArrayBuffer with
        member x.ElementType = Int(false, 32)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<uint32> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : uint32) = x.[i] <- v
type Float32Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 4
    static member PrimitiveType = Float 32
    member x.Length = length
    member x.Item
        with get(i : int) =
            store.[i]
        and set(i : int) (v : float) =
            store.[i] <- float v
    new(cnt : int) = Float32Buffer(ArrayBuffer.Create(float (4 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> float) = 
        let res = Float32Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : float) = 
        let res = Float32Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Float32Buffer(cnt)
    static member ofArray (arr : float[]) = 
        let res = Float32Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<float>) = Float32Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<float>) = Float32Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Float32Buffer(arr, byteOffset + 4*start, count)
    interface IArrayBuffer with
        member x.ElementType = Float 32
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<float> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : float) = x.[i] <- v
type Float64Buffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float64Array.Create(arr, float byteOffset, float length)
    static member ElementSize = 8
    static member PrimitiveType = Float 64
    member x.Length = length
    member x.Item
        with get(i : int) =
            store.[i]
        and set(i : int) (v : float) =
            store.[i] <- float v
    new(cnt : int) = Float64Buffer(ArrayBuffer.Create(float (8 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> float) = 
        let res = Float64Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : float) = 
        let res = Float64Buffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        Float64Buffer(cnt)
    static member ofArray (arr : float[]) = 
        let res = Float64Buffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<float>) = Float64Buffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<float>) = Float64Buffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = Float64Buffer(arr, byteOffset + 8*start, count)
    interface IArrayBuffer with
        member x.ElementType = Float 64
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<float> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : float) = x.[i] <- v
type V2iBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Int32Array.Create(arr, float byteOffset, float (2 * length))
    static member ElementSize = 8
    static member PrimitiveType = Vec(Int(true, 32), 2)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 2 * i
            V2i(int32(store.[i + 0]), int32(store.[i + 1]))
        and set(i : int) (v : V2i) =
            let i = 2 * i
            store.[i+0] <- float v.X
            store.[i+1] <- float v.Y
    new(cnt : int) = V2iBuffer(ArrayBuffer.Create(float (8 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> V2i) = 
        let res = V2iBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : V2i) = 
        let res = V2iBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        V2iBuffer(cnt)
    static member ofArray (arr : V2i[]) = 
        let res = V2iBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<V2i>) = V2iBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<V2i>) = V2iBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = V2iBuffer(arr, byteOffset + 8*start, count)
    interface IArrayBuffer with
        member x.ElementType = Vec(Int(true, 32), 2)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<V2i> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : V2i) = x.[i] <- v
type V3iBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Int32Array.Create(arr, float byteOffset, float (3 * length))
    static member ElementSize = 12
    static member PrimitiveType = Vec(Int(true, 32), 3)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 3 * i
            V3i(int32(store.[i + 0]), int32(store.[i + 1]), int32(store.[i + 2]))
        and set(i : int) (v : V3i) =
            let i = 3 * i
            store.[i+0] <- float v.X
            store.[i+1] <- float v.Y
            store.[i+2] <- float v.Z
    new(cnt : int) = V3iBuffer(ArrayBuffer.Create(float (12 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> V3i) = 
        let res = V3iBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : V3i) = 
        let res = V3iBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        V3iBuffer(cnt)
    static member ofArray (arr : V3i[]) = 
        let res = V3iBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<V3i>) = V3iBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<V3i>) = V3iBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = V3iBuffer(arr, byteOffset + 12*start, count)
    interface IArrayBuffer with
        member x.ElementType = Vec(Int(true, 32), 3)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<V3i> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : V3i) = x.[i] <- v
type V4iBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Int32Array.Create(arr, float byteOffset, float (4 * length))
    static member ElementSize = 16
    static member PrimitiveType = Vec(Int(true, 32), 4)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 4 * i
            V4i(int32(store.[i + 0]), int32(store.[i + 1]), int32(store.[i + 2]), int32(store.[i + 3]))
        and set(i : int) (v : V4i) =
            let i = 4 * i
            store.[i+0] <- float v.X
            store.[i+1] <- float v.Y
            store.[i+2] <- float v.Z
            store.[i+3] <- float v.W
    new(cnt : int) = V4iBuffer(ArrayBuffer.Create(float (16 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> V4i) = 
        let res = V4iBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : V4i) = 
        let res = V4iBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        V4iBuffer(cnt)
    static member ofArray (arr : V4i[]) = 
        let res = V4iBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<V4i>) = V4iBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<V4i>) = V4iBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = V4iBuffer(arr, byteOffset + 16*start, count)
    interface IArrayBuffer with
        member x.ElementType = Vec(Int(true, 32), 4)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<V4i> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : V4i) = x.[i] <- v
type V2fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (2 * length))
    static member ElementSize = 8
    static member PrimitiveType = Vec(Float 32, 2)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 2 * i
            V2d(store.[i + 0], store.[i + 1])
        and set(i : int) (v : V2d) =
            let i = 2 * i
            store.[i+0] <- v.X
            store.[i+1] <- v.Y
    new(cnt : int) = V2fBuffer(ArrayBuffer.Create(float (8 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> V2d) = 
        let res = V2fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : V2d) = 
        let res = V2fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        V2fBuffer(cnt)
    static member ofArray (arr : V2d[]) = 
        let res = V2fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<V2d>) = V2fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<V2d>) = V2fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = V2fBuffer(arr, byteOffset + 8*start, count)
    interface IArrayBuffer with
        member x.ElementType = Vec(Float 32, 2)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<V2d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : V2d) = x.[i] <- v
type V3fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (3 * length))
    static member ElementSize = 12
    static member PrimitiveType = Vec(Float 32, 3)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 3 * i
            V3d(store.[i + 0], store.[i + 1], store.[i + 2])
        and set(i : int) (v : V3d) =
            let i = 3 * i
            store.[i+0] <- v.X
            store.[i+1] <- v.Y
            store.[i+2] <- v.Z
    new(cnt : int) = V3fBuffer(ArrayBuffer.Create(float (12 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> V3d) = 
        let res = V3fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : V3d) = 
        let res = V3fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        V3fBuffer(cnt)
    static member ofArray (arr : V3d[]) = 
        let res = V3fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<V3d>) = V3fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<V3d>) = V3fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = V3fBuffer(arr, byteOffset + 12*start, count)
    interface IArrayBuffer with
        member x.ElementType = Vec(Float 32, 3)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<V3d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : V3d) = x.[i] <- v
type V4fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (4 * length))
    static member ElementSize = 16
    static member PrimitiveType = Vec(Float 32, 4)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 4 * i
            V4d(store.[i + 0], store.[i + 1], store.[i + 2], store.[i + 3])
        and set(i : int) (v : V4d) =
            let i = 4 * i
            store.[i+0] <- v.X
            store.[i+1] <- v.Y
            store.[i+2] <- v.Z
            store.[i+3] <- v.W
    new(cnt : int) = V4fBuffer(ArrayBuffer.Create(float (16 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> V4d) = 
        let res = V4fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : V4d) = 
        let res = V4fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        V4fBuffer(cnt)
    static member ofArray (arr : V4d[]) = 
        let res = V4fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<V4d>) = V4fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<V4d>) = V4fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = V4fBuffer(arr, byteOffset + 16*start, count)
    interface IArrayBuffer with
        member x.ElementType = Vec(Float 32, 4)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<V4d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : V4d) = x.[i] <- v
type M22fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (4 * length))
    static member ElementSize = 16
    static member PrimitiveType = Mat(Float 32, 2, 2)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 4 * i
            M22d(store.[i + 0], store.[i + 1], store.[i + 2], store.[i + 3])
        and set(i : int) (v : M22d) =
            let i = 4 * i
            store.[i+0] <- v.M00
            store.[i+1] <- v.M01
            store.[i+2] <- v.M10
            store.[i+3] <- v.M11
    new(cnt : int) = M22fBuffer(ArrayBuffer.Create(float (16 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> M22d) = 
        let res = M22fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : M22d) = 
        let res = M22fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        M22fBuffer(cnt)
    static member ofArray (arr : M22d[]) = 
        let res = M22fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<M22d>) = M22fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<M22d>) = M22fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = M22fBuffer(arr, byteOffset + 16*start, count)
    interface IArrayBuffer with
        member x.ElementType = Mat(Float 32, 2, 2)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<M22d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : M22d) = x.[i] <- v
type M23fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (6 * length))
    static member ElementSize = 24
    static member PrimitiveType = Mat(Float 32, 2, 3)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 6 * i
            M23d(store.[i + 0], store.[i + 1], store.[i + 2], store.[i + 3], store.[i + 4], store.[i + 5])
        and set(i : int) (v : M23d) =
            let i = 6 * i
            store.[i+0] <- v.M00
            store.[i+1] <- v.M01
            store.[i+2] <- v.M02
            store.[i+3] <- v.M10
            store.[i+4] <- v.M11
            store.[i+5] <- v.M12
    new(cnt : int) = M23fBuffer(ArrayBuffer.Create(float (24 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> M23d) = 
        let res = M23fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : M23d) = 
        let res = M23fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        M23fBuffer(cnt)
    static member ofArray (arr : M23d[]) = 
        let res = M23fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<M23d>) = M23fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<M23d>) = M23fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = M23fBuffer(arr, byteOffset + 24*start, count)
    interface IArrayBuffer with
        member x.ElementType = Mat(Float 32, 2, 3)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<M23d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : M23d) = x.[i] <- v
type M33fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (9 * length))
    static member ElementSize = 36
    static member PrimitiveType = Mat(Float 32, 3, 3)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 9 * i
            M33d(store.[i + 0], store.[i + 1], store.[i + 2], store.[i + 3], store.[i + 4], store.[i + 5], store.[i + 6], store.[i + 7], store.[i + 8])
        and set(i : int) (v : M33d) =
            let i = 9 * i
            store.[i+0] <- v.M00
            store.[i+1] <- v.M01
            store.[i+2] <- v.M02
            store.[i+3] <- v.M10
            store.[i+4] <- v.M11
            store.[i+5] <- v.M12
            store.[i+6] <- v.M20
            store.[i+7] <- v.M21
            store.[i+8] <- v.M22
    new(cnt : int) = M33fBuffer(ArrayBuffer.Create(float (36 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> M33d) = 
        let res = M33fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : M33d) = 
        let res = M33fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        M33fBuffer(cnt)
    static member ofArray (arr : M33d[]) = 
        let res = M33fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<M33d>) = M33fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<M33d>) = M33fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = M33fBuffer(arr, byteOffset + 36*start, count)
    interface IArrayBuffer with
        member x.ElementType = Mat(Float 32, 3, 3)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<M33d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : M33d) = x.[i] <- v
type M34fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (12 * length))
    static member ElementSize = 48
    static member PrimitiveType = Mat(Float 32, 3, 4)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 12 * i
            M34d(store.[i + 0], store.[i + 1], store.[i + 2], store.[i + 3], store.[i + 4], store.[i + 5], store.[i + 6], store.[i + 7], store.[i + 8], store.[i + 9], store.[i + 10], store.[i + 11])
        and set(i : int) (v : M34d) =
            let i = 12 * i
            store.[i+0] <- v.M00
            store.[i+1] <- v.M01
            store.[i+2] <- v.M02
            store.[i+3] <- v.M03
            store.[i+4] <- v.M10
            store.[i+5] <- v.M11
            store.[i+6] <- v.M12
            store.[i+7] <- v.M13
            store.[i+8] <- v.M20
            store.[i+9] <- v.M21
            store.[i+10] <- v.M22
            store.[i+11] <- v.M23
    new(cnt : int) = M34fBuffer(ArrayBuffer.Create(float (48 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> M34d) = 
        let res = M34fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : M34d) = 
        let res = M34fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        M34fBuffer(cnt)
    static member ofArray (arr : M34d[]) = 
        let res = M34fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<M34d>) = M34fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<M34d>) = M34fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = M34fBuffer(arr, byteOffset + 48*start, count)
    interface IArrayBuffer with
        member x.ElementType = Mat(Float 32, 3, 4)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<M34d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : M34d) = x.[i] <- v
type M44fBuffer(arr : ArrayBuffer, byteOffset : int, length : int) =
    let store = Float32Array.Create(arr, float byteOffset, float (16 * length))
    static member ElementSize = 64
    static member PrimitiveType = Mat(Float 32, 4, 4)
    member x.Length = length
    member x.Item
        with get(i : int) =
            let i = 16 * i
            M44d(store.[i + 0], store.[i + 1], store.[i + 2], store.[i + 3], store.[i + 4], store.[i + 5], store.[i + 6], store.[i + 7], store.[i + 8], store.[i + 9], store.[i + 10], store.[i + 11], store.[i + 12], store.[i + 13], store.[i + 14], store.[i + 15])
        and set(i : int) (v : M44d) =
            let i = 16 * i
            store.[i+0] <- v.M00
            store.[i+1] <- v.M01
            store.[i+2] <- v.M02
            store.[i+3] <- v.M03
            store.[i+4] <- v.M10
            store.[i+5] <- v.M11
            store.[i+6] <- v.M12
            store.[i+7] <- v.M13
            store.[i+8] <- v.M20
            store.[i+9] <- v.M21
            store.[i+10] <- v.M22
            store.[i+11] <- v.M23
            store.[i+12] <- v.M30
            store.[i+13] <- v.M31
            store.[i+14] <- v.M32
            store.[i+15] <- v.M33
    new(cnt : int) = M44fBuffer(ArrayBuffer.Create(float (64 * cnt)), 0, cnt)
    static member init (cnt : int) (creator : int -> M44d) = 
        let res = M44fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- creator i
        res
    static member create (cnt : int) (value : M44d) = 
        let res = M44fBuffer(cnt)
        for i in 0 .. cnt - 1 do res.[i] <- value
        res
    static member zeroCreate (cnt : int) = 
        M44fBuffer(cnt)
    static member ofArray (arr : M44d[]) = 
        let res = M44fBuffer(arr.Length)
        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]
        res
    static member ofSeq (arr : seq<M44d>) = M44fBuffer.ofArray (Seq.toArray arr)
    static member ofList (arr : list<M44d>) = M44fBuffer.ofArray (List.toArray arr)
    member x.Sub(start : int, count : int) = M44fBuffer(arr, byteOffset + 64*start, count)
    interface IArrayBuffer with
        member x.ElementType = Mat(Float 32, 4, 4)
        member x.Length = x.Length
        member x.Buffer = arr
        member x.ByteOffset = byteOffset
        member x.View = store |> unbox<ArrayBufferView>
        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer
    interface IArrayBuffer<M44d> with
        member x.Item
            with get(i : int) = x.[i]
            and set(i : int) (v : M44d) = x.[i] <- v
