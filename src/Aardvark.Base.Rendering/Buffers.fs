﻿namespace Aardvark.Base.Rendering

open Fable.Import.JS
open Aardvark.Base.Incremental

type IArrayBuffer =
    abstract member ElementType : PrimitiveType
    abstract member Length : int
    abstract member Buffer : ArrayBuffer
    abstract member ByteOffset : int
    abstract member Sub : start : int * cnt : int -> IArrayBuffer
    abstract member View : ArrayBufferView

type IArrayBuffer<'a> =
    inherit IArrayBuffer
    abstract member Item : int -> 'a with get, set


type IBuffer = interface end

type HostBuffer(data : IArrayBuffer) =
    interface IBuffer
    member x.Data = data


type BufferView =
    {
        buffer  : IMod<IBuffer>
        offset  : int
        typ     : PrimitiveType
    }

module BufferView =
    let inline ofArray<'a when 'a :> IArrayBuffer and 'a : (static member PrimitiveType : PrimitiveType) > (arr : IMod<'a>) =
        let t = (^a : (static member PrimitiveType : PrimitiveType) ())
        
        {
            buffer  = arr |> Mod.map (fun a -> HostBuffer a :> IBuffer)
            offset  = 0
            typ     = t
        }