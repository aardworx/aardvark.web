namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Aardvark.Import.JS
open Aardvark.Import.Browser


type WebGLVertexArrayObject =
    interface end

type WebGLSync =
    interface end

type WebGLSampler =
    interface end

type WebGL2RenderingContext =
    inherit WebGLRenderingContext
    abstract member SRGB_EXT : float
    abstract member SRGB_ALPHA_EXT : float
    abstract member SRGB8_ALPHA8_EXT : float
    abstract member FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT : float

    abstract member UNIFORM_BUFFER : float
    abstract member ACTIVE_UNIFORM_BLOCKS : float
    abstract member UNIFORM_BUFFER_OFFSET_ALIGNMENT : float

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

    abstract member TEXTURE_COMPARE_FUNC : float
    abstract member TEXTURE_BASE_LEVEL : float
    abstract member TEXTURE_COMPARE_MODE : float
    abstract member TEXTURE_MAX_LEVEL : float
    abstract member TEXTURE_MAX_LOD : float
    abstract member TEXTURE_MIN_LOD : float
    abstract member TEXTURE_WRAP_R : float
    abstract member COMPARE_REF_TO_TEXTURE : float


    abstract member createSampler : unit -> WebGLSampler
    abstract member deleteSampler : WebGLSampler -> unit
    abstract member isSampler : WebGLSampler -> bool
    abstract member bindSampler : unit : float * sampler : WebGLSampler -> unit
    abstract member getSamplerParameter : WebGLSampler * float -> obj
    abstract member samplerParameteri : WebGLSampler * float * float -> unit
    abstract member samplerParameterf : WebGLSampler * float * float -> unit

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

    abstract member texImage2D : target : float * level : float * internalformat : float * width : float * height : float * border : float * format : float * typ: float * data : obj -> unit

    abstract member SYNC_GPU_COMMANDS_COMPLETE : float
    abstract member SYNC_FLUSH_COMMANDS_BIT : float

    abstract member ALREADY_SIGNALED : float
    abstract member TIMEOUT_EXPIRED : float
    abstract member CONDITION_SATISFIED : float
    abstract member WAIT_FAILED : float
    abstract member SYNC_STATUS : float
    abstract member UNSIGNALED : float
    abstract member SIGNALED : float
    abstract member SYNC_CONDITION : float
    abstract member SYNC_FLAGS : float
    abstract member SYNC_FENCE : float

    abstract member MAX_CLIENT_WAIT_TIMEOUT_WEBGL : float

    abstract member fenceSync : float * float -> WebGLSync
    abstract member deleteSync : WebGLSync -> unit
    abstract member isSync : WebGLSync -> bool
    abstract member clientWaitSync : WebGLSync * float * float -> float
    abstract member waitSync : WebGLSync * float * float -> unit
    abstract member getSyncParameter : WebGLSync * float -> float

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


