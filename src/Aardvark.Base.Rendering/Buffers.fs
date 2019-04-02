namespace Aardvark.Base.Rendering

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


    let inline ofArray<'a when 'a :> IArrayBuffer and 'a : (static member PrimitiveType : PrimitiveType) > (arr : IMod<'a>) =
        let t = (^a : (static member PrimitiveType : PrimitiveType) ())
        
        {
            buffer  = toBuffer arr //|> Mod.map (fun a -> HostBuffer a :> IBuffer)
            offset  = 0
            typ     = t
        }