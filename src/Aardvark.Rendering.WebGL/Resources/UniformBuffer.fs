namespace Aardvark.Rendering.WebGL

open Aardvark.Base
open Aardvark.Base.Incremental

open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Rendering
open Fable.Core
open Microsoft.FSharp.Reflection

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
                    

    open System
    module private Array =
        let choosei (f : int -> 'a -> Option<'b>) (arr : 'a[]) =
            let res = System.Collections.Generic.List<'b>()
            for i in 0 .. arr.Length - 1 do
                match f i arr.[i] with
                | Some r -> res.Add r
                | None -> ()
            Seq.toArray res


    let private primitiveTypes =
        HMap.ofList [
            "System.Boolean", Bool
            "System.Byte", Int(false, 8)
            "System.SByte", Int(true, 8)
            "System.UInt16", Int(false, 16)
            "System.Int16", Int(true, 16)
            "System.UInt32", Int(false, 32)
            "System.Int32", Int(true, 32)
            "System.UInt64", Int(false, 64)
            "System.Int64", Int(true, 64)
            "System.Single", Float 32
            "System.Double", Float 64

            "Aardvark.Base.V2i", Vec(Int(true, 32), 2)
            "Aardvark.Base.V3i", Vec(Int(true, 32), 3)
            "Aardvark.Base.V4i", Vec(Int(true, 32), 4)
            "Aardvark.Base.V2d", Vec(Float 64, 2)
            "Aardvark.Base.V3d", Vec(Float 64, 3)
            "Aardvark.Base.V4d", Vec(Float 64, 4)

            
            "Aardvark.Base.M22d", Mat(Float 64, 2, 2)
            "Aardvark.Base.M23d", Mat(Float 64, 2, 3)
            "Aardvark.Base.M24d", Mat(Float 64, 2, 4)
            "Aardvark.Base.M32d", Mat(Float 64, 3, 2)
            "Aardvark.Base.M33d", Mat(Float 64, 3, 3)
            "Aardvark.Base.M34d", Mat(Float 64, 3, 4)
            "Aardvark.Base.M42d", Mat(Float 64, 4, 2)
            "Aardvark.Base.M43d", Mat(Float 64, 4, 3)
            "Aardvark.Base.M44d", Mat(Float 64, 4, 4)
            "Aardvark.Base.Trafo3d", Trafo
        ]

    let (|Union|Record|Primitive|Other|) (t : Type) =
        if FSharpType.IsUnion t then
            Union (FSharpType.GetUnionCases t |> List.ofArray)
        elif FSharpType.IsRecord t then
            Record (FSharpType.GetRecordFields t |> List.ofArray)
        else
            match HMap.tryFind t.FullName primitiveTypes with
            | Some prim -> Primitive prim   
            | _ -> Other t

    let rec getBlitTyped (buffer : bool) (src : Type) (dst : PrimitiveType) =
        match src, dst with
        | Primitive prim, dst ->
            getBlit buffer prim dst

        | Record srcFields, Struct(_size, dstFields) ->
            let srcFieldsByName = srcFields |> List.mapi (fun i f -> f.Name, (f, i)) |> Map.ofList
            let writers =
                dstFields |> List.toArray |> FSharp.Collections.Array.choose (fun dst ->
                    match Map.tryFind dst.name srcFieldsByName with
                    | Some (src, index) ->
                        let _, blit = getBlitTyped buffer src.PropertyType dst.typ
                        Some (index, fun view off value -> blit view (off + dst.offset) value)
                    | None ->
                        None
                )
                
            let blit (view : DataView) (offset : int) (value : obj) =
                let values = FSharpValue.GetRecordFields(value)
                for (i, write) in writers do
                    write view offset values.[i]

            0, blit

        | Record _, _ -> failwithf "[GL] unexpected record %A value for field %A" src dst

        | Union cases, Struct(_size, fields) ->
            let fieldsByName = fields |> List.map (fun f -> f.name, f) |> Map.ofList
            match Map.tryFind "tag" fieldsByName with
            | Some tagField -> 
                let writers =
                    cases |> List.choose (fun c ->
                        let writeFields = 
                            c.GetFields() |> Array.choosei (fun i f ->
                                let name = sprintf "%s_%s" c.Name f.Name
                                match Map.tryFind name fieldsByName with
                                | Some dst ->
                                    let _, blit = getBlitTyped buffer f.PropertyType dst.typ
                                    Some (i, fun view off value -> blit view (off + dst.offset) value)
                                | None ->
                                    None
                            )
                        if writeFields.Length > 0 then
                            Some (c.Tag, writeFields)
                        else
                            None
                    )
                    |> Map.ofList

                let _,blitTag = getBlit buffer (Int(true, 32)) tagField.typ

                let blit (view : DataView) (offset : int) (value : obj) =
                    let case, fields = FSharpValue.GetUnionFields(value, src)
                    blitTag view (offset + tagField.offset) case.Tag
                    match Map.tryFind case.Tag writers with
                    | Some writes ->
                        for (i, write) in writes do
                            write view offset fields.[i]
                    | None ->
                        ()



                0, blit
            | _ ->
                failwith "bad union target"
            
        | Union _, _ -> failwithf "[GL] unexpected union %A value for field %A" src dst

        | Other _, _ -> failwithf "[GL] unexpected type %A value for field %A" src dst


[<AllowNullLiteral>]
type UniformBufferSlot(parent : UniformBufferStore, handle : WebGLBuffer, store : Uint8Array, index : int, offset : int, layout : UniformBlockInfo) =
    
    let mutable next : UniformBufferSlot = null
    let view = DataView.Create(store.buffer, float offset)

    member x.Index = index
    member x.Handle = handle
    member x.Offset = offset
    member x.Size = layout.size
    member x.Layout = layout
    member x.Next
        with get() = next
        and set n = next <- n

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
        let vt = m.ValueType

        if vt.IsGenericParameter then
            Log.warn "[GL] uniform value %s does not have type-information" name
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
                parent.Dirty()
            )
        else
            match Map.tryFind name layout.fieldsByName with
            | Some f ->
                let _, blit = Blit.getBlitTyped true vt f.typ
                Mod.custom (fun t ->
                    let v = m.GetValueObj t
                    blit view f.offset v
                    parent.Dirty()
                )
            | None ->
                Mod.custom (fun _ -> Log.warn "[GL] unknown field: %s" name)

    member x.Free() = parent.Free x

and UniformBufferStore(parent : UniformBufferManager, ctx : Context, layout : UniformBlockInfo, capacity : int) =
    let gl = ctx.GL

    let virtualSize = 
        let align = gl.getParameter(gl.UNIFORM_BUFFER_OFFSET_ALIGNMENT) |> unbox<int>
        if layout.size % align = 0 then layout.size
        else (layout.size / align + 1) * align
        

    let buffer =
        let b = gl.createBuffer()
        gl.bindBuffer(gl.UNIFORM_BUFFER, b)
        gl.bufferData(gl.UNIFORM_BUFFER, U3.Case1 (float capacity * float virtualSize), gl.DYNAMIC_DRAW)
        gl.bindBuffer(gl.UNIFORM_BUFFER, null)
        b

    let store = 
        Uint8Array.Create(float (capacity * virtualSize))

    let mutable dirty = false
    let mutable count = 0
    let mutable free : UniformBufferSlot = null

    let tryAlloc(x : UniformBufferStore) =
        if unbox free then
            let n = free
            free <- n.Next
            n.Next <- null
            Some n
        elif count < capacity then
            let i = count
            let n = UniformBufferSlot(x, buffer, store, i, i * virtualSize, layout)
            count <- count + 1
            Some n
        else
            None

    let free(slot : UniformBufferSlot) =
        slot.Next <- free
        free <- slot

    member x.Upload() = 
        if dirty then
            dirty <- false
            gl.bindBuffer(gl.UNIFORM_BUFFER, buffer)
            gl.bufferSubData(gl.UNIFORM_BUFFER, 0.0, U2.Case2 store.buffer)
            gl.bindBuffer(gl.UNIFORM_BUFFER, null)

    member x.Dirty() =
        if not dirty then
            dirty <- true
            parent.Dirty x

    member x.TryAlloc() = tryAlloc x
    member x.Free(s : UniformBufferSlot) = free s

and UniformBufferManager(ctx : Context, layout : UniformBlockInfo) =
    let stores = System.Collections.Generic.List<UniformBufferStore>()

    static let dirty = System.Collections.Generic.List<UniformBufferStore>()

    member x.Layout = layout

    member x.Dirty(s : UniformBufferStore) =
        dirty.Add s


    member x.Alloc() =
        let mutable res = None
        use mutable e = stores.GetEnumerator()
        while not (unbox res) && e.MoveNext() do
            res <- e.Current.TryAlloc()

        match res with
        | None ->
            let store = UniformBufferStore(x, ctx, layout, 128)
            stores.Add store
            store.TryAlloc().Value
        | Some r -> 
            r

    static member UploadAll() =
        for d in dirty do d.Upload()
        dirty.Clear()


type UniformBuffer(ctx : Context, handle : WebGLBuffer, layout : UniformBlockInfo) =
    inherit Resource(ctx)

    let mutable store = Uint8Array.Create(float layout.size)
    let view = DataView.Create(store.buffer)

    member x.Handle = handle
    member x.Layout = layout
    
    member x.Offset = 0
    member x.Size = layout.size

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
        let vt = m.ValueType

        if vt.IsGenericParameter then
            Log.warn "[GL] uniform value %s does not have type-information" name
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
        else
            match Map.tryFind name layout.fieldsByName with
            | Some f ->
                let _, blit = Blit.getBlitTyped true vt f.typ
                Mod.custom (fun t ->
                    let v = m.GetValueObj t
                    blit view f.offset v
                )
            | None ->
                Mod.custom (fun _ -> Log.warn "[GL] unknown field: %s" name)
                
                

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
        let vt = m.ValueType

        if vt.IsGenericParameter then
            Log.warn "[GL] uniform value %s does not have type-information" name
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
        else
            let _, blit = Blit.getBlitTyped false vt typ
            Mod.custom (fun t ->
                let v = m.GetValueObj t
                blit view 0 v
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
