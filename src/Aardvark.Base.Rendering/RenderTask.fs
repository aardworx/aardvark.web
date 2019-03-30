namespace Aardvark.Base.Rendering

open Aardvark.Base.Incremental
open FSharp.Collections

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
    
    