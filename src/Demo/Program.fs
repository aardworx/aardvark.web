module Program

open System
open Aardvark.Base
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Incremental

type Sepp<'a> =
    struct
        val mutable public A : 'a
        new(a) = { A = a }
    end


type V2fArray(len : int) =
    let store = Float32Array.Create(float (2 * len))

    member x.length = len

    member x.Item
        with get(i : int) = 
            let b = 2 * i
            V2d(store.[b], store.[b + 1])
        
        and set(i : int) (v : V2d) = 
            let b = 2 * i
            store.[b] <- v.X
            store.[b+1] <- v.Y

    member x.SetByIndex (value : int -> V2d) =
        for i in 0 .. len - 1 do
            x.[i] <- value i


let test (s : Sepp<int>) =
    let mutable s2 = s
    s2.A <- 10
    s2


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
    type ShaderType =
        | Float of bits : int
        | Int of signed : bool * bits : int
        | Bool

        | Vec of ShaderType * int
        | Mat of ShaderType * int * int

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

                    let fields =
                        List.init (int indices.length) (fun i ->
                            let off = offsets.[i] |> unbox<int>
                            let size = sizes.[i] |> unbox<int>
                            let stride = strides.[i] |> unbox<int>
                            let rowMajor = rowMajors.[i] |> unbox<bool>
                            let t = types.[i] |> ShaderType.ofGLType x
                            x.uniformBlockBinding(p, float i, float i)
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
        abstract member buffer : ArrayBuffer

    type V2fArray(len : int) =
        let store = Float32Array.Create(float (2 * len))

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

        member x.CreateBuffer(data : IBuffer) =
            match data with
            | :? HostBuffer as data ->
                let b = x.GL.createBuffer()
                x.GL.bindBuffer(x.GL.ARRAY_BUFFER, b)
                x.GL.bufferData(x.GL.ARRAY_BUFFER, U3.Case3 data.Data, x.GL.STATIC_DRAW)
                Buffer(x, b)

            | :? Buffer as b ->
                b.Acquire()
                b

            | _ ->
                failwith "bad buffer"

        member x.CreateBuffer (arr : IArray) =
            x.CreateBuffer(HostBuffer arr.buffer)


[<AutoOpen>]
module UniformBufferImpl =
    
    type UniformBuffer(ctx : Context, handle : WebGLBuffer, layout : UniformBlockInfo) =
        inherit Resource(ctx)

        let mutable store = Uint8Array.Create(float layout.size)
        let view = DataView.Create(store.buffer)

        let writeNumber (t : ShaderType) (offset : int) (value : float) =
            match t with
            | Int(false, 8) -> view.setUint8(float offset, value)
            | Int(false, 16) -> view.setUint16(float offset, value)
            | Int(false, 32) -> view.setUint32(float offset, value)
            | Int(true, 8) -> view.setInt8(float offset, value)
            | Int(true, 16) -> view.setInt16(float offset, value)
            | Int(true, 32) -> view.setInt32(float offset, value)
            | Float 32 -> view.setFloat32(float offset, value)
            | Float 64 -> view.setFloat64(float offset, value)
            | Bool -> view.setInt32(float offset, value)
            | _ -> ()
            
        let getNumberWriter (t : ShaderType) (offset : int) =
            match t with
            | Int(false, 8) -> fun value -> view.setUint8(float offset, value)
            | Int(false, 16) -> fun value -> view.setUint16(float offset, value)
            | Int(false, 32) -> fun value -> view.setUint32(float offset, value)
            | Int(true, 8) -> fun value -> view.setInt8(float offset, value)
            | Int(true, 16) -> fun value -> view.setInt16(float offset, value)
            | Int(true, 32) -> fun value -> view.setInt32(float offset, value)
            | Float 32 -> fun value -> view.setFloat32(float offset, value)
            | Float 64 -> fun value -> view.setFloat64(float offset, value)
            | Bool -> fun value -> view.setInt32(float offset, value)
            | _ -> ignore


        member x.Handle = handle
        member x.Layout = layout

        member x.GetWriterTemplate(name : string, template : obj) =
            match Map.tryFind name layout.fieldsByName with
            | Some f ->
                match template with
                | :? bool -> 
                    let writer = getNumberWriter f.typ f.offset
                    fun value -> writer (if unbox value then 1.0 else 0.0)
                | :? float ->
                    let writer = getNumberWriter f.typ f.offset
                    fun value -> writer (unbox value)
                | :? M44d -> 
                    match f.typ with
                    | Mat(Float 32, 4, 4) ->
                        fun value ->
                            let m = unbox<M44d> value
                            Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | Mat(Float 32, 3, 3) ->
                        fun value ->
                            let m = M33d (unbox<M44d> value)
                            Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | Mat(Float 32, 2, 2) ->
                        fun value ->
                            let m = M33d (unbox<M44d> value)
                            Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | _ ->
                        fun value -> console.warn "bad matrix type"
                | :? Trafo3d -> 
                    match f.typ with
                    | Mat(Float 32, 4, 4) ->
                        fun value ->
                            let m = (unbox<Trafo3d> value).Forward
                            Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | Mat(Float 32, 3, 3) ->
                        fun value ->
                            let m = M33d ((unbox<Trafo3d> value).Forward)
                            Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | Mat(Float 32, 2, 2) ->
                        fun value ->
                            let m = M33d ((unbox<Trafo3d> value).Forward)
                            Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | _ ->
                        fun value -> console.warn "bad matrix type"
                    //let a = Float32Array.Create(store.buffer, ) //.set(unbox<ArrayLike<float>> (m.ToFloat32Array()), float f.offset)
                    //failwith ""
                | _ -> 
                    fun value -> console.warn (sprintf "bad input: %A" value)
                    
            | None ->
                ignore

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
            match Map.tryFind name layout.fieldsByName with
            | Some f ->
                match value with
                | :? bool as value -> writeNumber f.typ f.offset (if value then 1.0 else 0.0)
                | :? float as value -> writeNumber f.typ f.offset value
                | :? M44d as m -> 
                    match f.typ with
                    | Mat(Float 32, 4, 4) ->
                        Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | Mat(Float 32, 3, 3) ->
                        let m = M34d m
                        Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | Mat(Float 32, 2, 2) ->
                        let m = M22d m
                        Float32Array.Create(store.buffer, float f.offset).set(unbox<ArrayLike<float>> (m.ToFloat32Array()))
                    | _ ->
                        console.warn "bad matrix type"
                    //let a = Float32Array.Create(store.buffer, ) //.set(unbox<ArrayLike<float>> (m.ToFloat32Array()), float f.offset)
                    //failwith ""
                | _ -> console.warn (sprintf "bad input: %A" value)
                    
            | None ->
                ()

            ()
        
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

    let a = Mod.init 10
    let b = Mod.init 11
    let s = cset [a :> IMod<_>; b :> IMod<_>]
    let test = s |> ASet.chooseM (Mod.map ((+) 5 >> Some))
    
    let r = test.GetReader()
    console.warn (r.GetOperations(AdaptiveToken.Top) |> sprintf "op: %A")
    console.warn (r.State |> Seq.toList |> sprintf "st: %A")

    transact (fun () -> a.Value <- 100)
    console.warn (r.GetOperations(AdaptiveToken.Top) |> sprintf "op: %A")
    console.warn (r.State |> Seq.toList |> sprintf "st: %A")
    
    transact (fun () -> s.Remove a |> ignore)
    console.warn (r.GetOperations(AdaptiveToken.Top) |> sprintf "op: %A")
    console.warn (r.State |> Seq.toList |> sprintf "st: %A")

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

    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then
            let canvas = document.createElement_canvas()
            canvas.tabIndex <- 1.0
            document.body.appendChild(canvas) |> ignore
            document.body.style.margin <- "0"
            document.body.style.padding <- "0"

            canvas.style.width <- "100%"
            canvas.style.height <- "100%"
            
            let gl = canvas.getContext("webgl2") |> unbox<WebGL2RenderingContext>
            let ctx = Context(gl)


            let pos =   
                V3fArray.ofArray [|
                    V3d(-1.0, -1.0, 0.0)
                    V3d(1.0, -1.0, 0.0)
                    V3d(1.0, 1.0, 0.0)

                    V3d(-1.0, -1.0, 0.0)
                    V3d(1.0, 1.0, 0.0)
                    V3d(-1.0, 1.0, 0.0)
                |]
                
                
            let tc =   
                V2fArray.ofArray [|
                    V2d(0.0, 0.0)
                    V2d(1.0, 0.0)
                    V2d(1.0, 1.0)
                             
                    V2d(0.0, 0.0)
                    V2d(1.0, 1.0)
                    V2d(0.0, 1.0)
                |]
                

            let b = ctx.CreateBuffer pos
            let tcb = ctx.CreateBuffer tc

            //let b = gl.createBuffer()
            //gl.bindBuffer(gl.ARRAY_BUFFER, b)
            //gl.bufferData(gl.ARRAY_BUFFER, U3.Case3 pos.buffer, gl.STATIC_DRAW)
            //gl.bindBuffer(gl.ARRAY_BUFFER, null)

            let shader =
                """#version 300 es

                    #ifdef VERTEX

                    uniform View {
                        mat4 MVP;
                        //float scale;
                    };

                    layout(location = 0) in vec3 pos;
                    layout(location = 1) in vec2 tc;
                    out vec2 cc;
                    void main() {
                        gl_Position = vec4(pos, 1.0) * MVP;
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

            let p = ctx.CreateProgram(ctx.DefaultFramebufferSignature, shader)

            let (_,ub) = p.Value.Interface.uniformBlocks |> Map.toSeq |> Seq.head
            let uniform = ctx.CreateUniformBuffer(ub)

            uniform.SetField("scale", 1.0)
            uniform.SetField("MVP", M44d.Identity)
            uniform.Upload()
            console.error(string p.Value.Interface)

            //let set = System.Collections.Generic.HashSet<obj>()

            //do 
            //    let a = obj()
            //    let b = obj()

            //    console.log (set.Add a)
            //    console.log (set.Add b)
            //    console.log (set.Remove a)
            //    console.log (set.Remove b)
            //    console.log (set.Remove a)
            //    console.log (set.Remove b)


            //let q = System.Collections.Generic.List<int>()
            //let cmp = compare
            //q.HeapEnqueue(cmp, 10)
            //q.HeapEnqueue(cmp, 4)
            //q.HeapEnqueue(cmp, 2)
            //q.HeapEnqueue(cmp, 123)
            //q.HeapEnqueue(cmp, 311)
            //q.HeapEnqueue(cmp, 12)
            //q.HeapEnqueue(cmp, 3)
            //q.HeapEnqueue(cmp, 1999)


            //while q.Count > 0 do
            //    console.warn(q.HeapDequeue(cmp))

            let m = M44d.RotationZ(0.3)

            //let ub = gl.createBuffer()
            //gl.bindBuffer(gl.UNIFORM_BUFFER, ub)
            //gl.bufferData(gl.UNIFORM_BUFFER, U3.Case3 (m.ToFloat32Array().buffer), gl.DYNAMIC_DRAW)
            //gl.bindBuffer(gl.UNIFORM_BUFFER, null)

            //let vao = gl.createVertexArray()
            //do
            //    let i = Index.after Index.zero
            //    let j = Index.after i
            //    let b = Index.between i j
            //    console.warn (i < b)
            //    console.warn (b < j)
            //    console.warn (i < j)
            //    console.warn ((i = b))
            //    console.warn ((b = j))
            //    console.warn ((i = j))
            //    console.warn ((i > b))
            //    console.warn ((b > j))
            //    console.warn ((i > j))

            //let view = new RenderView(canvas)
            //view.Dispose()
            let mouse = new Aardvark.Rendering.Mouse(canvas) :> Aardvark.Rendering.IMouse
            let keyboard = new Aardvark.Rendering.Keyboard(canvas)  :> Aardvark.Rendering.IKeyboard

            let time = Mod.custom (fun _ -> performance.now() / 1000.0)
            let initial = CameraView.lookAt (V3d.III * 6.0) V3d.Zero V3d.OOI
            let cam = Aardvark.Application.DefaultCameraController.control mouse keyboard time initial

            
            let v = cam.GetValue(AdaptiveToken.Top) |> CameraView.viewTrafo

            let anim = keyboard.IsDown(Aardvark.Rendering.Keys.Space)
            let size = Mod.init (V2i.II)

            
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

            
            let writer = 
                let writeMVP = uniform.GetWriter("MVP", modelViewProj)
                Mod.custom (fun t ->
                    writeMVP.GetValue t
                    uniform.Upload()
                )
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
                caller.EvaluateAlways AdaptiveToken.Top (fun token ->
                    let t = performance.now() / 1000.0
 
                
                    let rect = canvas.getBoundingClientRect()

                
                    writer.GetValue token
                    //gl.bindBuffer(gl.UNIFORM_BUFFER, ub)
                    //gl.bufferSubData(gl.UNIFORM_BUFFER, 0.0, U2.Case2 (mvp.Forward.ToFloat32Array().buffer))
                    //gl.bindBuffer(gl.UNIFORM_BUFFER, null)



                    if canvas.width <> rect.width then canvas.width <- rect.width
                    if canvas.height <> rect.height then canvas.height <- rect.height
                    //console.log(sprintf "%.0fx%.0f" rect.width rect.height)
                    gl.viewport(0.0, 0.0, rect.width, rect.height)
                    gl.clearColor(0.0, 0.0, 0.0, 1.0)
                    gl.clearDepth(1.0)
                    gl.clear(float (int gl.COLOR_BUFFER_BIT ||| int gl.DEPTH_BUFFER_BIT))

                    gl.useProgram(p.Value.Handle)
                    gl.enableVertexAttribArray(0.0)
                    gl.enableVertexAttribArray(1.0)

                    gl.bindBufferBase(gl.UNIFORM_BUFFER, 0.0, uniform.Handle)

                    gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
                    gl.vertexAttribPointer(0.0, 3.0, gl.FLOAT, false, 0.0, 0.0)
                    gl.bindBuffer(gl.ARRAY_BUFFER, tcb.Handle)
                    gl.vertexAttribPointer(1.0, 2.0, gl.FLOAT, false, 0.0, 0.0)

                    gl.drawArrays(gl.TRIANGLES, 0.0, 6.0)

                    gl.bindBuffer(gl.ARRAY_BUFFER, null)
                    gl.bindBufferBase(gl.UNIFORM_BUFFER, 0.0, null)

                    gl.disableVertexAttribArray(1.0)
                    gl.disableVertexAttribArray(0.0)
                    gl.useProgram(null)


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
