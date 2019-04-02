namespace Aardvark.Base.Rendering

open Aardvark.Base.Incremental
open FSharp.Collections
open Fable.Import.JS

type IRenderTask =
    inherit IAdaptiveObject
    inherit System.IDisposable
    abstract member Run : AdaptiveToken -> Promise<unit>
    
[<AbstractClass>]
type AbstractRenderTask() =
    inherit AdaptiveObject()
    override x.Kind = "RenderTask"

    abstract member Render : AdaptiveToken -> Promise<unit>
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
            member x.Run _ = Promise.resolve ()
            member x.Dispose() = ()

    type private NoDisposeTask(render : AdaptiveToken -> Promise<unit>) =
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
    
    
    let custom (f : AdaptiveToken -> Promise<unit>) =  new NoDisposeTask(f) :> IRenderTask
    
    