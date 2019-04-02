namespace Aardvark.Rendering.WebGL

open Aardvark.Base
open Aardvark.Base.Incremental

open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Rendering
open Fable.Core

module private Blit =
    open Fable.Core.JsInterop

    let getShaderType (template : obj) =
        match template with
        | :? float -> Float 64
        | :? bool -> Bool
        | :? V2d -> Vec(Float 64, 2)
        | :? V3d -> Vec(Float 64, 3)
        | :? V4d -> Vec(Float 64, 4)
        | :? V2i -> Vec(Int(true, 32), 2)
        | :? V3i -> Vec(Int(true, 32), 3)
        | :? V4i -> Vec(Int(true, 32), 4)
        | :? M22d -> Mat(Float 64, 2, 2)
        | :? M23d -> Mat(Float 64, 2, 3)
        | :? M24d -> Mat(Float 64, 2, 4)
        | :? M32d -> Mat(Float 64, 3, 2)
        | :? M33d -> Mat(Float 64, 3, 3)
        | :? M34d -> Mat(Float 64, 3, 4)
        | :? M42d -> Mat(Float 64, 4, 2)
        | :? M43d -> Mat(Float 64, 4, 3)
        | :? M44d -> Mat(Float 64, 4, 4)
        | :? Trafo3d -> Trafo
        | _ -> failwithf "[GL] bad primitive: %A" template

    let (|Scalar|_|) (t : PrimitiveType) =
        match t with
        | Bool -> Some ()
        | Float _ -> Some ()
        | Int _ -> Some ()
        | _ -> None
            
   
    [<AutoOpen>]
    module private Helpers = 
        let inline number (v : obj) =
            if unbox v then unbox v
            else 0.0

    let toFloatArray (t : PrimitiveType) =
        match t with
        | Scalar -> fun (value : obj) -> Float32Array.``of`` [| number value|]
        | Vec(Float _ ,2) -> fun (value : obj) -> let value = unbox<V2d> value in Float32Array.``of`` [| value.X; value.Y |]
        | Vec(Float _ ,3) -> fun (value : obj) -> let value = unbox<V3d> value in Float32Array.``of`` [| value.X; value.Y; value.Z |]
        | Vec(Float _ ,4) -> fun (value : obj) -> let value = unbox<V4d> value in Float32Array.``of`` [| value.X; value.Y; value.Z; value.W |]
        | Vec(Int _ ,2) -> fun (value : obj) -> let value = unbox<V2i> value in Float32Array.``of`` [| float value.X; float value.Y |]
        | Vec(Int _ ,3) -> fun (value : obj) -> let value = unbox<V3i> value in Float32Array.``of`` [| float value.X; float value.Y; float value.Z |]
        | Vec(Int _ ,4) -> fun (value : obj) -> let value = unbox<V4i> value in Float32Array.``of`` [| float value.X; float value.Y; float value.Z; float value.W |]
        | Mat(_,2,2) -> fun (value : obj) -> (unbox<M22d> value).ToFloat32Array()
        | Mat(_,2,3) -> fun (value : obj) -> (unbox<M23d> value).ToFloat32Array()
        | Mat(_,2,4) -> fun (value : obj) -> (unbox<M24d> value).ToFloat32Array()
        | Mat(_,3,2) -> fun (value : obj) -> (unbox<M32d> value).ToFloat32Array()
        | Mat(_,3,3) -> fun (value : obj) -> (unbox<M33d> value).ToFloat32Array()
        | Mat(_,3,4) -> fun (value : obj) -> (unbox<M34d> value).ToFloat32Array()
        | Mat(_,4,2) -> fun (value : obj) -> (unbox<M42d> value).ToFloat32Array()
        | Mat(_,4,3) -> fun (value : obj) -> (unbox<M43d> value).ToFloat32Array()
        | Mat(_,4,4) -> fun (value : obj) -> (unbox<M44d> value).ToFloat32Array()
        | Trafo -> fun (value : obj) -> (unbox<Trafo3d> value).Forward.ToFloat32Array()
        | _ -> failwith "bad type"

    let rec getBlit (buffer : bool) (src : PrimitiveType) (dst : PrimitiveType) =
        match src, dst with
            | Bool, Bool                -> 4, fun (view : DataView) (offset : int) (value : obj) -> view.setUint32(float offset, (if unbox value then 1.0 else 0.0), true)
            | Scalar, Bool              -> 4, fun (view : DataView) (offset : int) (value : obj) -> view.setUint32(float offset, number value, true)
            | Scalar, Int(false, 8)     -> 1, fun (view : DataView) (offset : int) (value : obj) -> view.setUint8(float offset, number value)
            | Scalar, Int(false, 16)    -> 2, fun (view : DataView) (offset : int) (value : obj) -> view.setUint16(float offset, number value, true)
            | Scalar, Int(false, 32)    -> 4, fun (view : DataView) (offset : int) (value : obj) -> view.setUint32(float offset, number value, true)
            | Scalar, Int(true, 8)      -> 1, fun (view : DataView) (offset : int) (value : obj) -> view.setInt8(float offset, number value)
            | Scalar, Int(true, 16)     -> 2, fun (view : DataView) (offset : int) (value : obj) -> view.setInt16(float offset, number value, true)
            | Scalar, Int(true, 32)     -> 4, fun (view : DataView) (offset : int) (value : obj) -> view.setInt32(float offset, number value, true)
            | Scalar, Float 32          -> 4, fun (view : DataView) (offset : int) (value : obj) -> view.setFloat32(float offset, number value, true)
            | Scalar, Float 64          -> 8, fun (view : DataView) (offset : int) (value : obj) -> view.setFloat64(float offset, number value, true)

            | Vec(tSrc, _), Vec(tDst, dDst) ->
                let toArray = toFloatArray src
                let dstSize, writer = getBlit buffer tSrc tDst
                dDst * dstSize, fun (view : DataView) (offset : int) (value : obj) ->
                    
                    let mutable off = offset
                    let arr = toArray value
                    for i in 0 .. int arr.length - 1 do
                        writer view off arr.[i]
                        off <- off + dstSize
                    
            | Scalar, Vec(tDst, _) -> getBlit buffer src tDst
            | Vec _, Scalar -> getBlit buffer src (Vec(dst, 1))

            | Trafo, Mat _ ->
                let s, w = getBlit buffer (Mat(Float 64, 4, 4)) dst
                s, fun (view : DataView) (offset : int) (value : obj) -> w view offset (unbox<M44d> value?forward)
                
            | Mat(s, 3, 3), Mat(d, 3, 3) when buffer ->
                let s, w = getBlit buffer (Mat(s, 3, 4)) (Mat(d, 3, 4))
                s, fun (view : DataView) (offset : int) (value : obj) -> w view offset (M34d (unbox<M33d> value))

            | Mat(Scalar, r0, c0), Mat(Float 32, r1, c1) when r0 = r1 && c0 = c1 ->
                let size = 4 * r1 * c1
                let toArray = toFloatArray dst
                size, fun (view : DataView) (offset : int) (value : obj) -> 
                    let dst = Float32Array.Create(view.buffer, view.byteOffset + float offset, float (r0 * c0))
                    dst.set(unbox (toArray value))

            | _ ->
                failwithf "[GL] bad uniform conversion form %A to %A" src dst
                    

type UniformBuffer(ctx : Context, handle : WebGLBuffer, layout : UniformBlockInfo) =
    inherit Resource(ctx)

    let mutable store = Uint8Array.Create(float layout.size)
    let view = DataView.Create(store.buffer)

    member x.Handle = handle
    member x.Layout = layout

    member x.GetWriterTemplate(name : string, template : obj) =
        match Map.tryFind name layout.fieldsByName with
        | Some f ->
            let srcType = Blit.getShaderType template
            let _,blit = Blit.getBlit true srcType f.typ
            fun value -> blit view f.offset value
        | None ->
            fun value -> Log.warn "[GL] unknown field: %s" name

    member x.GetWriter(name : string, m : IMod) =
        let writer = ref None
        Mod.custom (fun t ->
            let v = m.GetValueObj t
            let write = 
                match !writer with
                | Some w -> w
                | None ->
                    let w = x.GetWriterTemplate(name, v)
                    writer := Some w
                    w
            write v
        )

    member x.SetField(name : string, value : obj) =
        x.GetWriterTemplate(name, value) value
        
    member x.Upload() =
        ctx.GL.bindBuffer(ctx.GL.UNIFORM_BUFFER, handle)
        ctx.GL.bufferSubData(ctx.GL.UNIFORM_BUFFER, 0.0, U2.Case2 store.buffer)
        ctx.GL.bindBuffer(ctx.GL.UNIFORM_BUFFER, null)

    override x.Destroy() =
        Log.debug "destroy uniformbuffer"
        store <- null
        ctx.GL.deleteBuffer handle

type UniformLocation(ctx : Context, typ : PrimitiveType) =
    inherit Resource(ctx)

    let store = Uint8Array.Create(float (PrimitiveType.size typ))
    let view = DataView.Create(store.buffer)

    member x.GetWriterTemplate(template : obj) =
        let srcType = Blit.getShaderType template
        let _,blit = Blit.getBlit false srcType typ
        fun value -> blit view 0 value
        
    member x.GetWriter(m : IMod) =
        let writer = ref None
        Mod.custom (fun t ->
            let v = m.GetValueObj t
            let write = 
                match !writer with
                | Some w -> w
                | None ->
                    let w = x.GetWriterTemplate(v)
                    writer := Some w
                    w
            write v
        )

    member x.Store = store

    override x.Destroy() = ()

[<AutoOpen>]
module UniformBufferImpl =
    
    type Context with
        member x.CreateUniformBuffer(layout : UniformBlockInfo)  =
            Log.debug "create uniformbuffer"
            let handle = x.GL.createBuffer()
            x.GL.bindBuffer(x.GL.UNIFORM_BUFFER, handle)
            x.GL.bufferData(x.GL.UNIFORM_BUFFER, U3.Case1 (float layout.size), x.GL.DYNAMIC_DRAW)
            x.GL.bindBuffer(x.GL.UNIFORM_BUFFER, null)
            UniformBuffer(x, handle, layout)

        member x.CreateUniformLocation(typ : PrimitiveType) =
            Log.debug "create uniformlocation"
            UniformLocation(x, typ)
