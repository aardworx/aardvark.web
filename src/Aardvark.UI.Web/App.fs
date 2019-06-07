namespace Aardvark.UI

open Aardvark.Import.Browser
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

type Unpersist<'model, 'mmodel> =
    {
        create : 'model -> 'mmodel
        update : 'mmodel -> 'model -> unit
    }

module Unpersist =
    let inline instance< ^a, ^b when (^a or ^b) : (static member Create : 'a -> 'b) and (^a or ^b) : (static member Update : ^b * ^a -> unit)> =
        {
            create = fun m -> ((^a or ^b) : (static member Create : 'a -> 'b) (m))
            update = fun m v -> ((^a or ^b) : (static member Update : ^b * ^a -> unit) (m, v))
        }

type App<'model, 'mmodel, 'msg> =
    {
        initial     : 'model
        update      : 'model -> 'msg -> 'model
        view        : 'mmodel -> DomNode<'msg>
        unpersist   : Unpersist<'model, 'mmodel>
    }

module App =
    open Aardvark.UI
    open Aardvark.Import.JS

    type LivingUpdater<'msg>(inner : Updater.NodeUpdater<'msg>) =
        inherit AdaptiveObject()

        let mutable alive = true

        override x.Kind = "Callback"

        override x.MarkObj() =
            if alive then 
                setTimeout (fun () -> x.Update AdaptiveToken.Top) 0 |> ignore
            true

        member x.Update(t : AdaptiveToken) =
            if alive then 
                x.EvaluateAlways t (fun t -> 
                    inner.Update t
                )
        member x.Dispose() =
            alive <- false
            inner.RemoveOutput x


    let run (parent : HTMLElement) (app : App<'model, 'mmodel, 'msg>) =
        let mutable model = app.initial
        let mm = app.unpersist.create model
        let view = app.view mm

        let scope = 
            {
                Updater.emit = fun msgs ->
                    model <- msgs |> Seq.fold app.update model
                    transact (fun () ->
                        app.unpersist.update mm model
                    )
            }

        let updater = LivingUpdater(Aardvark.UI.DomNode.newUpdater parent scope view)
        updater.Update(AdaptiveToken.Top)
        
        { new System.IDisposable with
            member x.Dispose() =
                updater.Dispose()
                let all = FSharp.Collections.Array.init (int parent.children.length) (fun i -> parent.children.[i])
                all |> FSharp.Collections.Array.iter (fun c -> c.remove())
        }