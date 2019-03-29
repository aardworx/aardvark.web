namespace Aardvark.Base.Rendering

open Fable.Import.JS
open Aardvark.Base
open Aardvark.Base.Incremental
open FSharp.Collections

type PrimitiveType =
    | Float of bits : int
    | Int of signed : bool * bits : int
    | Bool

    | Vec of PrimitiveType * int
    | Mat of PrimitiveType * int * int
    | Trafo
    

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

module DefaultSemantic =
    let Positions = "Positions"
    let Colors = "Colors"
    let Depth = "Depth"


type IFramebufferSignature =
    abstract member Colors : Map<int, string>
    abstract member Depth : bool

type IBuffer = interface end

type HostBuffer(data : IArrayBuffer) =
    interface IBuffer
    member x.Data = data




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


type RenderObject =
    {
        pipeline        : PipelineState
        vertexBuffers   : Map<string, BufferView>
        indexBuffer     : Option<BufferView>
        mode            : PrimitiveTopology
        call            : IMod<DrawCall>
    }
    

type IRenderTask =
    inherit IAdaptiveObject
    inherit System.IDisposable
    abstract member Run : AdaptiveToken -> unit
    
[<AbstractClass>]
type AbstractRenderTask() =
    inherit AdaptiveObject()
    override x.Kind = "RenderTask"

    abstract member Render : AdaptiveToken -> unit
    abstract member Release : unit -> unit

    interface IRenderTask with
        member x.Run t =
            x.EvaluateAlways t (fun t ->
                x.Render t
            )
        member x.Dispose() = 
            x.Release()

    


module RenderTask =
    type private EmptyTask() =
        inherit ConstantObject()
        override x.Kind = "RenderTask"
        interface IRenderTask with
            member x.Run _ = ()
            member x.Dispose() = ()

    type private NoDisposeTask(render : AdaptiveToken -> unit) =
        inherit AdaptiveObject()
        override x.Kind = "RenderTask"
        interface IRenderTask with
            member x.Run t =
                x.EvaluateAlways t (fun t ->
                    render t
                )
            member x.Dispose() = 
                ()

    let empty =  new EmptyTask() :> IRenderTask
    
    
    let custom (f : AdaptiveToken -> unit) =  new NoDisposeTask(f) :> IRenderTask
    
    