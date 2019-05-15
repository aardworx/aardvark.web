namespace Aardvark.Base.Rendering

open Aardvark.Import.JS
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
    abstract member Get : int -> 'a
    abstract member Set : int * 'a -> unit
    //abstract member Item : int -> 'a with get, set


type IBuffer = interface end
type ITexture = interface end

type FileTexture(url : string) =
    interface ITexture
    member x.Url = url


type HostBuffer(data : IArrayBuffer) =
    interface IBuffer
    member x.Data = data

type PromiseBuffer(promise : Promise<IBuffer>) =
    interface IBuffer
    member x.Promise = promise


type BufferView =
    {
        buffer  : IMod<IBuffer>
        offset  : int
        typ     : PrimitiveType
    }

module BufferView =

    let private cache = WeakMap.Create() |> unbox<WeakMap<IMod, IMod<IBuffer>>>

    let toBuffer<'a when 'a :> IArrayBuffer> (m : IMod<'a>) =
        let v = cache.get(m)
        if unbox v then 
            v
        else
            let v = m |> Mod.map (fun a -> HostBuffer a :> IBuffer)
            cache.set(m, v) |> ignore
            v


    let ofArray<'a when 'a :> IArrayBuffer> (arr : IMod<'a>) =
        let t = arr.GetValue(AdaptiveToken.Top).ElementType
        
        {
            buffer  = toBuffer arr //|> Mod.map (fun a -> HostBuffer a :> IBuffer)
            offset  = 0
            typ     = t
        }