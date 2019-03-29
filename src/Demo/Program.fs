module Program

open System
open Aardvark.Base
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Incremental

module Log =
    open Fable.Import.JS


    let start fmt = Printf.kprintf (fun str -> console.group str) fmt
    let stop () = console.groupEnd()
    let line fmt = Printf.kprintf (fun str -> console.log(str)) fmt
    let warn fmt = Printf.kprintf (fun str -> console.warn(str)) fmt
    let error fmt = Printf.kprintf (fun str -> console.error(str)) fmt


type ShaderType =
    | Float of bits : int
    | Int of signed : bool * bits : int
    | Bool

    | Vec of ShaderType * int
    | Mat of ShaderType * int * int
    | Trafo


module DefaultSemantic =
    let Positions = "Positions"
    let Colors = "Colors"
    let Depth = "Depth"


type WebGLVertexArrayObject =
    interface end

type WebGL2RenderingContext =
    inherit WebGLRenderingContext
    abstract member UNIFORM_BUFFER : float
    abstract member ACTIVE_UNIFORM_BLOCKS : float

    abstract member UNIFORM_BLOCK_BINDING : float
    abstract member UNIFORM_BLOCK_DATA_SIZE : float
    abstract member UNIFORM_BLOCK_ACTIVE_UNIFORMS : float
    abstract member UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES : float

    abstract member UNIFORM_TYPE : float
    abstract member UNIFORM_SIZE : float
    abstract member UNIFORM_BLOCK_INDEX : float
    abstract member UNIFORM_OFFSET : float
    abstract member UNIFORM_ARRAY_STRIDE : float
    abstract member UNIFORM_MATRIX_STRIDE : float
    abstract member UNIFORM_IS_ROW_MAJOR : float



    abstract member bindBufferRange : target : float * index : float * buffer : WebGLBuffer * offset : float * size : float -> unit
    abstract member bindBufferBase : target : float * index : float * buffer : WebGLBuffer -> unit

    abstract member getActiveUniformBlockName : WebGLProgram * float -> string
    abstract member getActiveUniformBlockParameter : WebGLProgram * float * float -> obj
    abstract member getActiveUniforms : WebGLProgram * Uint32Array * float -> obj
    abstract member uniformBlockBinding : WebGLProgram * float * float -> unit
    abstract member getFragDataLocation : WebGLProgram * string -> float

    abstract member createVertexArray : unit -> WebGLVertexArrayObject
    abstract member deleteVertexArray : WebGLVertexArrayObject -> unit
    abstract member bindVertexArray : WebGLVertexArrayObject -> unit

type Context(gl : WebGL2RenderingContext) =
    let mutable id = 1

    member x.GL = gl

    member x.NewId() = 
        let r = id
        id <- r + 1
        r

[<AbstractClass>]
type Resource(ctx : Context) =
    let id = ctx.NewId()
    let mutable refCount = 1
    
    member x.Context = ctx

    member x.Acquire() = 
        refCount <- refCount + 1

    member x.Release() = 
        refCount <- refCount - 1
        if refCount = 0 then x.Destroy()


    abstract member Destroy : unit -> unit

    member x.Id = id
    override x.GetHashCode() = id
    override x.Equals o =
        match o with
        | :? Resource as o -> id = o.Id
        | _ -> false

    
    interface IComparable with
        member x.CompareTo o =
            match o with
            | :? Resource as o -> compare id o.Id
            | _ -> failwith "uncomparable"



[<AutoOpen>]
module FramebufferSignatureImpl =
    type FramebufferSignature(ctx : Context, colors : Map<int, string>, depth : bool) =
        inherit Resource(ctx)

        

        member x.Colors = colors
        member x.Depth = depth
        override x.Destroy() = ()

    type Context with


        member x.CreateFramebufferSignature (attachments : Set<string>) =
            let depth =
                Set.contains DefaultSemantic.Depth attachments
            let colors =
                attachments
                |> Set.remove DefaultSemantic.Depth
                |> Seq.sortWith(fun l r -> 
                    if l = DefaultSemantic.Colors then
                        if r = DefaultSemantic.Colors then 0
                        else -1
                    elif r = DefaultSemantic.Colors then
                        1
                    else
                        compare l r 
                )
                |> Seq.mapi (fun i n -> i,n)
                |> Map.ofSeq

            FramebufferSignature(x, colors, depth)

        member x.DefaultFramebufferSignature =
            x.CreateFramebufferSignature(Set.ofList [DefaultSemantic.Colors; DefaultSemantic.Depth])

[<AutoOpen>]
module ProgramImpl = 
    module ShaderType =
        let ofGLType (gl : WebGL2RenderingContext) (t : float) =
            if t = gl.BOOL then Bool
            elif t = gl.BYTE then Int(true, 8)
            elif t = gl.UNSIGNED_BYTE then Int(false, 8)
            elif t = gl.SHORT then Int(true, 16)
            elif t = gl.UNSIGNED_SHORT then Int(false, 16)
            elif t = gl.INT then Int(true, 32)
            elif t = gl.UNSIGNED_INT then Int(false, 32)
            elif t = gl.FLOAT then Float(32)

            elif t = gl.INT_VEC2 then Vec(Int(true, 32), 2)
            elif t = gl.INT_VEC3 then Vec(Int(true, 32), 3)
            elif t = gl.INT_VEC4 then Vec(Int(true, 32), 4)

            elif t = gl.BOOL_VEC2 then Vec(Bool, 2)
            elif t = gl.BOOL_VEC3 then Vec(Bool, 3)
            elif t = gl.BOOL_VEC4 then Vec(Bool, 4)

            elif t = gl.FLOAT_VEC2 then Vec(Float 32, 2)
            elif t = gl.FLOAT_VEC3 then Vec(Float 32, 3)
            elif t = gl.FLOAT_VEC4 then Vec(Float 32, 4)
            
            elif t = gl.FLOAT_MAT2 then Mat(Float 32, 2, 2)
            elif t = gl.FLOAT_MAT3 then Mat(Float 32, 3, 3)
            elif t = gl.FLOAT_MAT4 then Mat(Float 32, 4, 4)

            else failwithf "invalid type: %A" t

    type Parameter =
        {
            name    : string
            typ     : ShaderType
            size    : int
        }

    type UniformField =
        {
            name        : string
            offset      : int
            stride      : int
            size        : int
            rowMajor    : bool
            typ         : ShaderType
        }

    type UniformBlockInfo =
        {
            index           : int
            name            : string
            size            : int
            fields          : list<UniformField>
            fieldsByName    : Map<string, UniformField>
        }

    type ProgramInterface =
        {
            attributes      : Map<int, Parameter>
            uniformBlocks   : Map<int, UniformBlockInfo>
        }

    type WebGL2RenderingContext with
        member x.GetAttributes(p : WebGLProgram) =
            Map.ofList [
                let cnt = x.getProgramParameter(p, x.ACTIVE_ATTRIBUTES) |> unbox<int>
                for ai in 0 .. cnt - 1 do
                    let att = x.getActiveAttrib(p, float ai)
                    if unbox att then
                        let loc = x.getAttribLocation(p, att.name) |> int
                        let t = ShaderType.ofGLType x att.``type``
                        yield loc, { name = att.name; typ = t; size = int att.size }
            ]

        member x.GetUniformBlocks(p : WebGLProgram) =
            let cnt = x.getProgramParameter(p, x.ACTIVE_UNIFORM_BLOCKS) |> unbox<int>
            Map.ofList [
                for bi in 0 .. cnt - 1 do
                    let name = x.getActiveUniformBlockName(p, float bi)
                    let indices = x.getActiveUniformBlockParameter(p, float bi, x.UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES) |> unbox<Uint32Array>
                    let size = x.getActiveUniformBlockParameter(p, float bi, x.UNIFORM_BLOCK_DATA_SIZE) |> unbox<int>

                    let offsets     = x.getActiveUniforms(p, indices, x.UNIFORM_OFFSET) |> unbox<Uint32Array>
                    let sizes       = x.getActiveUniforms(p, indices, x.UNIFORM_SIZE) |> unbox<Uint32Array>
                    let strides     = x.getActiveUniforms(p, indices, x.UNIFORM_ARRAY_STRIDE) |> unbox<Uint32Array>
                    let rowMajors   = x.getActiveUniforms(p, indices, x.UNIFORM_IS_ROW_MAJOR) |> unbox<bool[]>
                    let types       = x.getActiveUniforms(p, indices, x.UNIFORM_TYPE) |> unbox<Uint32Array>
                    
                    x.uniformBlockBinding(p, float bi, float bi)
                    let fields =
                        List.init (int indices.length) (fun i ->
                            let off = offsets.[i] |> unbox<int>
                            let size = sizes.[i] |> unbox<int>
                            let stride = strides.[i] |> unbox<int>
                            let rowMajor = rowMajors.[i] |> unbox<bool>
                            let t = types.[i] |> ShaderType.ofGLType x
                            let r = x.getActiveUniform(p, indices.[i])
                            { offset = off; stride = stride; size = size; rowMajor = rowMajor; name = r.name; typ = t }
                        )
                        |> List.sortBy (fun f -> f.offset)

                    yield bi, { index = bi; size = size; name = name; fields = fields; fieldsByName = fields |> Seq.map (fun f -> f.name, f) |> Map.ofSeq }
            ]

        member x.FindOutputLocation(p : WebGLProgram, name : string) =
            let names = [name; name + "Out"; "fs_" + name]
            names |> List.tryPick (fun name ->
                let v = x.getFragDataLocation(p, name) |> unbox<int>
                if v >= 0 then Some v
                else None
            )

        member x.GetProgramInterface(signature : FramebufferSignature, p : WebGLProgram) =  
            let valid = 
                signature.Colors |> Map.forall (fun slot name ->
                    match x.FindOutputLocation(p, name) with
                    | Some loc when loc = slot -> true
                    | None -> console.warn (sprintf "[GL] program does not contain output: %s" name); false
                    | Some l -> console.warn (sprintf "[GL] program location for %s is %d (expected %d)" name l slot);false
                )
            if valid then
                Some {
                    attributes = x.GetAttributes(p)
                    uniformBlocks = x.GetUniformBlocks(p)
                }
            else
                None
            
    let private versionRx = System.Text.RegularExpressions.Regex @"\#version.*(\r\n|\r|\n)"

    type Program(ctx : Context, handle : WebGLProgram, iface : ProgramInterface) =
        inherit Resource(ctx)
        member x.Handle = handle
        member x.Interface = iface

        override x.Destroy() = ctx.GL.deleteProgram handle

    type Context with

        member x.CompileShader (stage : float, code : string) =
            let def =
                if stage = x.GL.VERTEX_SHADER then "VERTEX"
                elif stage = x.GL.FRAGMENT_SHADER then "FRAGMENT"
                else "UNKNOWN"

            let code = versionRx.Replace(code, fun m -> m.Value + "#define " + def + "\r\n")

            let shader = x.GL.createShader(stage)
            x.GL.shaderSource(shader, code)
            x.GL.compileShader(shader)
    
            let s = x.GL.getShaderParameter(shader, x.GL.COMPILE_STATUS) |> unbox<int>
            if s = 0 then
                let log = x.GL.getShaderInfoLog(shader)
                console.warn s
                console.warn log
                None
            else
                Some shader

        member x.CreateProgram(signature : FramebufferSignature, code : string) =
            match x.CompileShader(x.GL.VERTEX_SHADER, code), x.CompileShader(x.GL.FRAGMENT_SHADER, code) with
            | Some vs, Some fs ->
                let p = x.GL.createProgram()
                x.GL.attachShader(p, vs)
                x.GL.attachShader(p, fs)
                x.GL.linkProgram(p)

                let status = x.GL.getProgramParameter(p, x.GL.LINK_STATUS) |> unbox<int>
                if status <> 0 then
                    match x.GL.GetProgramInterface(signature, p) with
                    | Some iface -> 
                        Some(Program(x, p, iface))
                    | None ->
                        x.GL.deleteProgram p
                        None
                else
                    let log = x.GL.getProgramInfoLog(p)
                    console.warn log
                    x.GL.deleteProgram p
                    None
            | _ ->
                None

[<AutoOpen>]  
module BufferImpl = 

    type IArray =
        abstract member shaderType : ShaderType
        abstract member buffer : ArrayBuffer

    type IntArray(len : int) =
        let store = Int32Array.Create(float len)
    
        static member ShaderType = Int(true, 32)
        member x.Length = len
        member x.Item
            with get(i : int) = int store.[i]
            and set (i : int) (value : int) = store.[i] <- float value
    
        interface IArray with
            member x.buffer = store.buffer
            member x.shaderType = Int(true, 32)

        static member init (count : int) (f : int -> int) =
            let res = IntArray(count)
            for i in 0 .. count - 1 do
                res.[i] <- f i
            res
    
        static member ofArray (arr : int[]) =
            let res = IntArray(arr.Length)
            for i in 0 .. arr.Length - 1 do
                res.[i] <- arr.[i]
            res

    type V2fArray(len : int) =
        let store = Float32Array.Create(float (2 * len))
        
        static member ShaderType = Vec(Float 32, 2)
        member x.Length = len
        member x.Item
            with get(i : int) =
                let i = 2 * i
                V2d(store.[i], store.[i+1])
            and set (i : int) (value : V2d) =
                let i = 2 * i
                store.[i] <- value.X
                store.[i+1] <- value.Y
        
        interface IArray with
            member x.buffer = store.buffer
            member x.shaderType = Vec(Float 32, 2)

        static member init (count : int) (f : int -> V2d) =
            let res = V2fArray(count)
            for i in 0 .. count - 1 do
                res.[i] <- f i
            res
        
        static member ofArray (arr : V2d[]) =
            let res = V2fArray(arr.Length)
            for i in 0 .. arr.Length - 1 do
                res.[i] <- arr.[i]
            res

    type V3fArray(len : int) =
        let store = Float32Array.Create(float (3 * len))
        
        static member ShaderType = Vec(Float 32, 3)
        member x.Length = len
        member x.Item
            with get(i : int) =
                let i = 3 * i
                V3d(store.[i], store.[i+1], store.[i+2])
            and set (i : int) (value : V3d) =
                let i = 3 * i
                store.[i] <- value.X
                store.[i+1] <- value.Y
                store.[i+2] <- value.Z
            
        interface IArray with
            member x.buffer = store.buffer
            member x.shaderType = Vec(Float 32, 3)
        
        static member init (count : int) (f : int -> V3d) =
            let res = V3fArray(count)
            for i in 0 .. count - 1 do
                res.[i] <- f i
            res
        
        static member ofArray (arr : V3d[]) =
            let res = V3fArray(arr.Length)
            for i in 0 .. arr.Length - 1 do
                res.[i] <- arr.[i]
            res

    type V4fArray(len : int) =
        let store = Float32Array.Create(float (4 * len))
    
        static member ShaderType = Vec(Float 32, 4)
        member x.Length = len
        member x.Item
            with get(i : int) =
                let i = 4 * i
                V4d(store.[i], store.[i+1], store.[i+2], store.[i+3])
            and set (i : int) (value : V4d) =
                let i = 4 * i
                store.[i] <- value.X
                store.[i+1] <- value.Y
                store.[i+2] <- value.Z
                store.[i+3] <- value.W
                    
        interface IArray with
            member x.buffer = store.buffer
            member x.shaderType = Vec(Float 32, 4)


    type IBuffer = interface end

    type HostBuffer(data : ArrayBuffer) =
        interface IBuffer
        member x.Data = data

        new(arr : IArray) = HostBuffer(arr.buffer)

    type Buffer(ctx : Context, handle : WebGLBuffer) =
        inherit Resource(ctx)
        member x.Handle = handle

        override x.Destroy() = ctx.GL.deleteBuffer(handle)
        override x.ToString() = string handle

        interface IBuffer

    type Context with

        member x.DeleteBuffer(b : Buffer) =
            b.Release()

        member x.CreateBuffer(target : float, data : IBuffer) =
            match data with
            | :? HostBuffer as data ->
                let b = x.GL.createBuffer()
                x.GL.bindBuffer(target, b)
                x.GL.bufferData(target, U3.Case3 data.Data, x.GL.STATIC_DRAW)
                Buffer(x, b)

            | :? Buffer as b ->
                b.Acquire()
                b

            | _ ->
                failwith "bad buffer"

        member x.CreateBuffer (target : float, arr : IArray) =
            x.CreateBuffer(target, HostBuffer arr.buffer)


[<AutoOpen>]
module UniformBufferImpl =
    

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

        let (|Scalar|_|) (t : ShaderType) =
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

        let toFloatArray (t : ShaderType) =
            match t with
            | Scalar -> fun (value : obj) -> Float32Array.``of`` [| number value|]
            | Vec(_,2) -> fun (value : obj) -> Float32Array.``of`` [| number value?X; number value?Y |]
            | Vec(_,3) -> fun (value : obj) -> Float32Array.``of`` [| number value?X; number value?Y; number value?Z |]
            | Vec(_,3) -> fun (value : obj) -> Float32Array.``of`` [| number value?X; number value?Y; number value?Z; number value?W |]

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

        let rec getBlit (src : ShaderType) (dst : ShaderType) =
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
                    let dstSize, writer = getBlit tSrc tDst
                    dDst * dstSize, fun (view : DataView) (offset : int) (value : obj) ->
                        let mutable off = offset
                        let arr = toArray value
                        for i in 0 .. int arr.length - 1 do
                            writer view off arr.[i]
                            off <- off + dstSize
                    
                | Scalar, Vec(tDst, _) -> getBlit src tDst
                | Vec _, Scalar -> getBlit src (Vec(dst, 1))

                | Trafo, Mat _ ->
                    let s, w = getBlit (Mat(Float 64, 4, 4)) dst
                    s, fun (view : DataView) (offset : int) (value : obj) -> w view offset (unbox<M44d> value?forward)
                
                | Mat(s, 3, 3), Mat(d, 3, 3) ->
                    let s, w = getBlit (Mat(s, 3, 4)) (Mat(d, 3, 4))
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
                let _,blit = Blit.getBlit srcType f.typ
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
            store <- null
            ctx.GL.deleteBuffer handle


    type Context with
        member x.CreateUniformBuffer(layout : UniformBlockInfo)  =
            let handle = x.GL.createBuffer()
            x.GL.bindBuffer(x.GL.UNIFORM_BUFFER, handle)
            x.GL.bufferData(x.GL.UNIFORM_BUFFER, U3.Case1 (float layout.size), x.GL.DYNAMIC_DRAW)
            x.GL.bindBuffer(x.GL.UNIFORM_BUFFER, null)
            UniformBuffer(x, handle, layout)

[<AutoOpen>]
module Resources =
    
    type IResourceToken =
        inherit IDisposable
        abstract member Context : Context

    type IResource =
        inherit IAdaptiveObject
        abstract member Acquire : unit -> unit
        abstract member Release : unit -> unit
        abstract member ReleaseAll : unit -> unit
        abstract member GetHandleObj : AdaptiveToken -> obj

    type IResource<'a> =
        inherit IResource
        abstract member GetHandle : AdaptiveToken -> 'a

    type ResourceCache(ctx : Context) =
        let hash (l : list<obj>) =
            l |> List.fold (fun h m -> HashCode.Combine(h, Unchecked.hash m)) 0

        let rec equals (l : list<obj>) (r : list<obj>) =
            match l, r with
            | [], [] -> true
            | l :: ls, r :: rs -> Unchecked.equals l r && equals ls rs
            | _ -> false

        let store = Dict<list<obj>, IResource>(hash, equals)
        
        member x.Context = ctx

        member internal x.Remove (key : list<obj>) =
            store.Remove key |> ignore

        member x.GetOrCreate<'a, 'b when 'a :> IResource<'b>>(deps : list<obj>, creator : IResourceToken -> 'a) =
            let resource = 
                store.GetOrCreate(deps, fun deps ->
                    creator(new ResourceCacheEntry(x, deps) :> IResourceToken) :> IResource
                )

            unbox<IResource<'b>> resource

        member x.Clear() =
            for (k,v) in Seq.toList store do v.ReleaseAll()
            store.Clear()

    and private ResourceCacheEntry (cache : ResourceCache, key : list<obj>) =
        interface IResourceToken with
            member x.Dispose() = cache.Remove key
            member x.Context = cache.Context


    [<AbstractClass>]
    type AbstractResource<'a>(entry : IDisposable) =
        inherit AdaptiveObject()

        let mutable handle = None
        let mutable refCount = 0

        abstract member Create : AdaptiveToken -> 'a
        abstract member Update : AdaptiveToken * 'a -> 'a
        abstract member Destroy : 'a -> unit

        override x.Kind = "Resource"

        member x.Acquire() =
            refCount <- refCount + 1

        member x.ReleaseAll() =
            entry.Dispose()
            refCount <- 0
            match handle with
            | Some h -> 
                x.Destroy h
                handle <- None
            | None ->
                ()


        member x.Release() =
            refCount <- refCount - 1
            if refCount = 0 then
                match handle with
                | Some h -> 
                    x.Destroy h
                    handle <- None
                    entry.Dispose()
                | None -> 
                    ()

        member x.GetHandle(t) =
            x.EvaluateAlways t (fun t ->
                if refCount <= 0 then failwith "updating unreferenced resource"

                match handle with
                | Some h ->
                    if x.OutOfDate then 
                        let hh = x.Update(t, h)
                        handle <- Some hh
                        hh
                    else
                        h
                | None ->
                    let h = x.Create t
                    handle <- Some h
                    h
            )


        interface IResource with
            member x.Acquire() = x.Acquire()
            member x.Release() = x.Release()
            member x.ReleaseAll() = x.ReleaseAll()
            member x.GetHandleObj(t) = x.GetHandle(t) :> obj

        interface IResource<'a> with
            member x.GetHandle(t) = x.GetHandle(t)



    type BufferResource(token : IResourceToken, target : float, data : IMod<IBuffer>) =
        inherit AbstractResource<Buffer>(token)

        override x.Create(t) =
            let data = data.GetValue t
            token.Context.CreateBuffer(target, data)

        override x.Update(t, b) =
            let data = data.GetValue t
            let n = token.Context.CreateBuffer(target, data)
            b.Destroy()
            n

        override x.Destroy b =
            b.Destroy()

    type UniformBufferResource(token : IResourceToken, layout : UniformBlockInfo, tryGetUniform : string -> Option<IMod>) =
        inherit AbstractResource<UniformBuffer>(token)

        let mutable write = Mod.constant ()

        override x.Create(t) =
            let b = token.Context.CreateUniformBuffer(layout)

            let writers = 
                layout.fields |> List.choose (fun f ->
                    match tryGetUniform f.name with
                    | Some value ->
                        let writer = b.GetWriter(f.name, value)
                        Some writer
                    | None ->
                        None
                )

            if not (List.isEmpty writers) then
                write <- 
                    Mod.custom (fun t ->
                        for w in writers do w.GetValue t
                        b.Upload()
                    )

            write.GetValue t

            b

        override x.Update(t, b) =
            write.GetValue t
            b
            
        override x.Destroy b =
            write <- Mod.constant ()
            b.Destroy()



    type ResourceManager(ctx : Context) =
        
        let bufferCache = ResourceCache(ctx)
        let indexBufferCache = ResourceCache(ctx)
        let uniformBufferCache = ResourceCache(ctx)
        let programCache = Dict<FramebufferSignature * string, Option<Program>>(Unchecked.hash, Unchecked.equals)

        member x.Context = ctx

        member x.CreateUniformBuffer(block : UniformBlockInfo, tryGetUniform : string -> Option<IMod>) =
            let values = 
                block.fields |> List.map (fun f ->
                    match tryGetUniform f.name with
                    | Some m -> m
                    | None -> failwithf "[GL] could not get uniform: %s" f.name
                )
            let key = (block :> obj) :: unbox values
            uniformBufferCache.GetOrCreate(key, fun token ->
                UniformBufferResource(token, block, tryGetUniform)
            )

        member x.CreateBuffer(data : IMod<IBuffer>) =
            bufferCache.GetOrCreate([data], fun token ->
                BufferResource(token, ctx.GL.ARRAY_BUFFER, data)
            )
            
        member x.CreateIndexBuffer(data : IMod<IBuffer>) =
            indexBufferCache.GetOrCreate([data], fun token ->
                BufferResource(token, ctx.GL.ELEMENT_ARRAY_BUFFER, data)
            )


        member x.CreateProgram(signature : FramebufferSignature, code : string) =
            let program = 
                programCache.GetOrCreate((signature, code), fun (signature, code) ->
                    let program = ctx.CreateProgram(signature, code)
                    match program with
                    | Some p -> p.Acquire()
                    | None -> ()
                    program
                )
            match program with
            | Some p -> p
            | None -> failwith "[GL] could not compile program"

        member x.Dispose() =
            bufferCache.Clear()
            uniformBufferCache.Clear()
            for (_,p) in programCache do 
                match p with
                | Some p -> p.Destroy()
                | None -> ()
            programCache.Clear()

        interface IDisposable with
            member x.Dispose() = x.Dispose()


    type PrimitiveTopology =
        | PointList
        | LineList
        | LineStrip
        | TriangleList
        | TriangleStrip
        


    type DrawCall =
        {
            first           : int
            faceVertexCount : int
            instanceCount   : int
        }

    type PipelineState =
        {
            shader          : string
            uniforms        : Map<string, IMod>
        }

    type BufferView =
        {
            buffer  : IMod<IBuffer>
            offset  : int
            typ     : ShaderType
        }

    module BufferView =
        let inline ofArray<'a when 'a :> IArray and 'a : (static member ShaderType : ShaderType) > (arr : IMod<'a>) =
            let t = (^a : (static member ShaderType : ShaderType) ())
            
            {
                buffer  = arr |> Mod.map (fun a -> HostBuffer a :> IBuffer)
                offset  = 0
                typ     = t
            }


    type RenderObject =
        {
            pipeline        : PipelineState
            vertexBuffers   : Map<string, BufferView>
            indexBuffer     : Option<BufferView>
            mode            : PrimitiveTopology
            call            : IMod<DrawCall>
        }
        
    type VertexAttrib =
        {
            size    : int
            typ     : float
            norm    : bool
            stride  : int
            offset  : int
        }

    module VertexAttrib =
        let rec ofType (gl : WebGL2RenderingContext) (t : ShaderType) =
            match t with
            | ShaderType.Bool           -> [ { size = 1; typ = gl.BOOL; norm = false; stride = 0; offset = 0 } ]
            | ShaderType.Int(true, 8)   -> [ { size = 1; typ = gl.BYTE; norm = false; stride = 0; offset = 0 }  ]
            | ShaderType.Int(false, 8)  -> [ { size = 1; typ = gl.UNSIGNED_BYTE; norm = false; stride = 0; offset = 0 } ]
            | ShaderType.Int(true, 16)  -> [ { size = 1; typ = gl.SHORT; norm = false; stride = 0; offset = 0 } ]
            | ShaderType.Int(false, 16) -> [ { size = 1; typ = gl.UNSIGNED_SHORT; norm = false; stride = 0; offset = 0 } ]
            | ShaderType.Int(true, 32)  -> [ { size = 1; typ = gl.INT; norm = false; stride = 0; offset = 0 } ]
            | ShaderType.Int(false, 32) -> [ { size = 1; typ = gl.UNSIGNED_INT; norm = false; stride = 0; offset = 0 } ]
            | ShaderType.Float(32)      -> [ { size = 1; typ = gl.FLOAT; norm = false; stride = 0; offset = 0 } ]

            | ShaderType.Vec(inner, d) ->
                match ofType gl inner with
                | [t] -> [ { t with size = d } ]
                | _ -> failwithf "[GL] bad vector type: %A" t
            | ShaderType.Mat(inner, r, c) ->
                match ofType gl inner with
                | [t] -> failwith "[GL] matrix attributes not implemented"
                | _ -> failwithf "[GL] bad matrix type: %A" t
            | _ ->
                failwithf "[GL] bad attribute type: %A" t

    type IndexInfo =
        {
            typ : float
            offset : int
            size : int
        }

    type PreparedRenderObject =
        {
            program             : Program
            uniformBuffers      : Map<int, IResource<UniformBuffer>>
            vertexBuffers       : Map<int, IResource<Buffer> * list<VertexAttrib>>
            indexBuffer         : Option<IResource<Buffer> * IndexInfo>
            mode                : float
            call                : IMod<DrawCall>
        }

    module PreparedRenderObject =

        let update (t : AdaptiveToken) (o : PreparedRenderObject) =
            o.uniformBuffers |> Map.iter (fun _ b -> b.GetHandle(t) |> ignore)
            o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.GetHandle(t) |> ignore)
            o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.GetHandle t |> ignore)
            o.call.GetValue(t) |> ignore

        let acquire (o : PreparedRenderObject) =
            o.program.Acquire()
            o.uniformBuffers |> Map.iter (fun _ b -> b.Acquire())
            o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.Acquire())
            o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.Acquire())
            
        let release (o : PreparedRenderObject) =
            o.program.Release()
            o.uniformBuffers |> Map.iter (fun _ b -> b.Release())
            o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.Release())
            o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.Release())


        let render (o : PreparedRenderObject) =
            let gl = o.program.Context.GL


            gl.useProgram(o.program.Handle)
            
            // bind uniforms
            for (id, b) in Map.toSeq o.uniformBuffers do
                let b = b.GetHandle(AdaptiveToken.Top)
                gl.bindBufferBase(gl.UNIFORM_BUFFER, float id, b.Handle)

            // bind buffers
            for (id, (b, atts)) in Map.toSeq o.vertexBuffers do
                let b = b.GetHandle(AdaptiveToken.Top)
                gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
                let mutable id = id
                for att in atts do
                    gl.enableVertexAttribArray(float id)
                    gl.vertexAttribPointer(float id, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                    id <- id + 1
                gl.bindBuffer(gl.ARRAY_BUFFER, null)

            
            let call = o.call.GetValue AdaptiveToken.Top
            match o.indexBuffer with
            | Some (ib, info) ->
                let ib = ib.GetHandle(AdaptiveToken.Top)
                gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
                gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))
            | None ->
                gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)



    type ResourceManager with
        member x.Prepare(signature : FramebufferSignature, o : RenderObject) =
            let gl = x.Context.GL

            let program = x.CreateProgram(signature, o.pipeline.shader)

            let uniformBuffers = 
                program.Interface.uniformBlocks |> Map.map (fun index block ->
                    x.CreateUniformBuffer(block, fun n -> Map.tryFind n o.pipeline.uniforms)
                )

            let vertexBuffers =
                program.Interface.attributes |> Map.map (fun index p ->
                    match Map.tryFind p.name o.vertexBuffers with
                    | Some b ->
                        let buffer = x.CreateBuffer(b.buffer)
                        let atts = VertexAttrib.ofType gl b.typ |> List.map (fun a -> { a with offset = a.offset + b.offset })

                        buffer, atts

                    | None ->
                        failwithf "[GL] could not get vertex attribute %s" p.name
                )

            let vertexAttributes =
                program.Interface.attributes |> Map.map (fun index p ->
                    match Map.tryFind p.name o.vertexBuffers with
                    | Some b ->
                        b.typ, b.offset
                    | None ->
                        failwithf "[GL] could not get vertex attribute %s" p.name
                            
                )

            let indexBuffer =
                match o.indexBuffer with
                | Some view ->
                    let b = x.CreateIndexBuffer(view.buffer)
                    let info =
                        match view.typ with
                        | Int(_, 32) -> { offset = view.offset; size = 4; typ = gl.UNSIGNED_INT }
                        | Int(_, 16) -> { offset = view.offset; size = 2; typ = gl.UNSIGNED_SHORT }
                        | Int(_, 8) -> { offset = view.offset; size = 1; typ = gl.UNSIGNED_BYTE }
                        | t -> failwithf "[GL] bad index type: %A" t
                    Some (b,info)
                | None ->
                    None

            let mode =
                match o.mode with
                | PrimitiveTopology.PointList -> gl.POINTS
                | PrimitiveTopology.LineList -> gl.LINES
                | PrimitiveTopology.LineStrip -> gl.LINE_STRIP
                | PrimitiveTopology.TriangleList -> gl.TRIANGLES
                | PrimitiveTopology.TriangleStrip -> gl.TRIANGLE_STRIP


            {
                program             = program
                uniformBuffers      = uniformBuffers
                indexBuffer         = indexBuffer
                vertexBuffers       = vertexBuffers
                mode                = mode
                call                = o.call
            }





//type WebGLRenderingContext with
//    member x.UNIFORM_BUFFER = 35345.0

//    [<Emit("$0.bindBufferRange($1, $2, $3, $4, $5)")>]
//    member x.bindBufferRange(target : float, index : float, buffer : WebGLBuffer, offset : float, size : float) = jsNative

type IRenderTask =  
    inherit System.IDisposable
    abstract member Run : V2i -> unit

module RenderTask =
    let empty = 
        { new IRenderTask with
            member x.Run _ = ()
            member x.Dispose() = ()
        }


let testy() =
    let d = Dict<Option<IMod<int> * int>, int>(Unchecked.hash, Unchecked.equals)
   
    let a = Some (Mod.constant 1, 0)
    let b = Some (Mod.constant 2, 0)
    let aa = Some (Mod.constant 1, 0)
    let c = Some (Mod.init 10 :> IMod<_>, 0)

    d.[a] <- 1
    d.[b] <- 2
    d.[c] <- 3
    Log.start "initial"
    Log.line "aa: %A" (d.TryGetValue aa)
    Log.line "a:  %A" (d.TryGetValue a)
    Log.line "b:  %A" (d.TryGetValue b)
    Log.line "c:  %A" (d.TryGetValue c)
    Log.stop()

    Log.start "aa <- 100"
    d.[aa] <- 100
    Log.line "aa: %A" (d.TryGetValue aa)
    Log.line "a:  %A" (d.TryGetValue a)
    Log.line "b:  %A" (d.TryGetValue b)
    Log.line "c:  %A" (d.TryGetValue c)
    Log.warn "%A" (Seq.toList d)
    Log.stop()

    Log.start "remove(c): %A" (d.Remove c)
    
    Log.line "%A" (Seq.toList d)
    Log.stop()

    let set = DictSet(Unchecked.hash, Unchecked.equals)
    Log.start "add"
    Log.line "add(a):  %A" (set.Add a)
    Log.line "add(aa): %A" (set.Add aa)
    Log.line "add(b):  %A" (set.Add b)
    Log.line "add(c):  %A" (set.Add c)
    Log.line "count: %d" set.Count
    Log.line "set: %A" (Seq.toList set)

    Log.stop()

    Log.start "remove"
    Log.line "rem(aa): %A" (set.Remove aa)
    Log.line "rem(a):  %A" (set.Remove a)
    Log.line "rem(b):  %A" (set.Remove b)
    Log.line "rem(c):  %A" (set.Remove c)
    Log.line "count: %d" set.Count
    Log.line "set: %A" (Seq.toList set)
    Log.stop()

    //let s = cset [1..10]

    //let test = s |> ASet.choose (fun v -> if v % 2 = 0 then Some (v / 2) else None)

    //let r = test.GetReader()
    //console.warn (r.GetOperations(AdaptiveToken.Top) |> sprintf "%A")
    //console.warn (r.State |> Seq.toList |> sprintf "%A")

    //transact (fun () -> s.Add 99 |> ignore)
    //console.warn (r.GetOperations(AdaptiveToken.Top) |> sprintf "%A")
    //console.warn (r.State |> Seq.toList |> sprintf "%A")

    
    //transact (fun () -> s.Add 100 |> ignore)
    //console.warn (r.GetOperations(AdaptiveToken.Top) |> sprintf "%A")
    //console.warn (r.State  |> Seq.toList |> sprintf "%A")

    //let m = Mod.ofPromise (seop())
    //console.warn(Mod.force m)
    
    //console.warn(Mod.force m)
    //setTimeout (fun () -> console.warn(Mod.force m)) 1000 |> ignore


[<EntryPoint>]
let main argv =
    //testy()

    let shader =
        """#version 300 es

            #ifdef VERTEX

            uniform View {
                float scale;
                mat4 MVP;
            };

            layout(location = 0) in vec3 pos;
            layout(location = 1) in vec2 tc;
            out vec2 cc;
            void main() {
                gl_Position = vec4(scale * pos, 1.0) * MVP;
                cc = tc;
            }
            #endif

            #ifdef FRAGMENT
            precision highp float;
            precision highp int;
            in vec2 cc;
            layout(location = 0) out vec4 Colors;
            void main() {
                Colors = vec4(cc.x,cc.y,1,1);
            }
            #endif
        """
    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then
            
            
            
            
            let canvas = document.createElement_canvas()
            canvas.tabIndex <- 1.0
            document.body.appendChild(canvas) |> ignore
            document.body.style.margin <- "0"
            document.body.style.padding <- "0"

            canvas.style.width <- "100%"
            canvas.style.height <- "100%"
            
            let mouse = new Aardvark.Rendering.Mouse(canvas) :> Aardvark.Rendering.IMouse
            let keyboard = new Aardvark.Rendering.Keyboard(canvas)  :> Aardvark.Rendering.IKeyboard
            let time = Mod.custom (fun _ -> performance.now() / 1000.0)
            let size = Mod.init (V2i.II)




            let initial = CameraView.lookAt (V3d.III * 6.0) V3d.Zero V3d.OOI
            let cam = Aardvark.Application.DefaultCameraController.control mouse keyboard time initial
            let anim = keyboard.IsDown(Aardvark.Rendering.Keys.Space)
            let angle =
                Mod.integrate 0.0 time [
                    anim |> Mod.map (fun a ->
                        if a then 
                            time |> Mod.stepTime (fun _ dt o -> o + 0.1 * dt)
                        else
                            AFun.create id
                    )
                ]



            let model = angle |> Mod.map Trafo3d.RotationZ
            let view = cam |> Mod.map CameraView.viewTrafo
            let proj = size |> Mod.map (fun s ->  Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
            let modelViewProj =
                Mod.custom (fun t ->
                    let m = model.GetValue(t)
                    let v = view.GetValue(t)
                    let p = proj.GetValue(t)
                    m * v * p
                
                )

            
            let pos =   
                V3fArray.ofArray [|
                    V3d(-1.0, -1.0, 0.0)
                    V3d(1.0, -1.0, 0.0)
                    V3d(1.0, 1.0, 0.0)
                    V3d(-1.0, 1.0, 0.0)
                |]
                
                
            let tc =   
                V2fArray.ofArray [|
                    V2d(0.0, 0.0)
                    V2d(1.0, 0.0)
                    V2d(1.0, 1.0)
                    V2d(0.0, 1.0)
                |]

            let index =
                IntArray.ofArray [|
                    0; 1; 2
                    0; 2; 3
                |]

            let object =
                {
                    pipeline = 
                        { 
                            shader = shader 
                            uniforms =
                                Map.ofList [
                                    "scale", Mod.constant 1.0 :> IMod
                                    "MVP", modelViewProj :> IMod
                                ]
                        }
                    vertexBuffers =
                        Map.ofList [
                            "pos", BufferView.ofArray (Mod.constant pos)
                            "tc", BufferView.ofArray (Mod.constant tc)
                        ]
                    indexBuffer = Some <|  BufferView.ofArray (Mod.constant index)
                    mode = PrimitiveTopology.TriangleList
                    call = Mod.constant { faceVertexCount = 6; first = 0; instanceCount = 1 }
                }
            



            let gl = canvas.getContext("webgl2") |> unbox<WebGL2RenderingContext>
            let ctx = Context(gl)
            let manager = new ResourceManager(ctx)
            let prep = manager.Prepare(ctx.DefaultFramebufferSignature, object)
            PreparedRenderObject.acquire prep

            let objects = [prep]



            let mutable baseTime = performance.now()
            let mutable frameCount = 0

            let render = ref (fun (v : float) -> ())
            let caller =
                { new AdaptiveObject() with
                    override x.MarkObj() = 
                        window.requestAnimationFrame(!render) |> ignore
                        true
                    override x.Kind = "RenderView"
                }

            render := fun _ ->
                let rect = canvas.getBoundingClientRect()
                transact (fun () -> size.Value <- V2i(int rect.width, int rect.height))
                caller.EvaluateAlways AdaptiveToken.Top (fun token ->

                    if canvas.width <> rect.width then canvas.width <- rect.width
                    if canvas.height <> rect.height then canvas.height <- rect.height
                    //console.log(sprintf "%.0fx%.0f" rect.width rect.height)
                    gl.viewport(0.0, 0.0, rect.width, rect.height)
                    gl.clearColor(0.0, 0.0, 0.0, 1.0)
                    gl.clearDepth(1.0)
                    gl.clear(float (int gl.COLOR_BUFFER_BIT ||| int gl.DEPTH_BUFFER_BIT))
                    for prep in objects do
                        PreparedRenderObject.update token prep

                    for prep in objects do
                        PreparedRenderObject.render prep


                    frameCount <- frameCount + 1
                    if frameCount > 100 then
                        let n = performance.now()
                        let fps = 1000.0 * float frameCount / (n - baseTime)
                        console.log fps
                        baseTime <- n
                        frameCount <- 0

                    //setTimeout render 16 |> ignore
                )
                transact (fun () -> time.MarkOutdated())

            !render 0.0

    )



    0 // return an integer exit code
