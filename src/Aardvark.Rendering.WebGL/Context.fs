namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Fable.Import.JS
open Fable.Import.Browser


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
        if refCount = 0 then 
            x.Destroy()


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


