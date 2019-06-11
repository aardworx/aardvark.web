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

type MutableApp<'mmodel, 'msg>(model : 'mmodel, kill : System.IDisposable, emit : seq<'msg> -> unit) =
    
    let callbacks = System.Collections.Generic.List<Result<unit, obj> -> unit>()
    let mutable exit = 
        Prom.create( fun success _error -> 
            callbacks.Add success
        )

    member x.Exit = exit

    member x.Cancel() = 
        kill.Dispose()
        for c in callbacks do c (Ok ())
        callbacks.Clear()
        exit <- Prom.value (Ok ())
        
    member x.Faulted(err : obj) = 
        kill.Dispose()
        for c in callbacks do c (Error err)
        callbacks.Clear()
        exit <- Prom.value (Error err)

    member x.Model = model
    member x.Emit msgs = emit msgs



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


    let private fix (create : Lazy<'a> -> 'a) =
        let ref = ref Unchecked.defaultof<'a>
        ref := create (lazy (!ref ))
        !ref

    let run (parent : HTMLElement) (app : App<'model, 'mmodel, 'msg>) =
        let old = parent.innerHTML
        let mutable model = app.initial
        let mm = app.unpersist.create model
        let view = app.view mm

        fix (fun (self : Lazy<MutableApp<_,_>>) ->
            let scope = 
                {
                    Updater.emit = fun msgs ->
                        try
                            model <- msgs |> Seq.fold app.update model
                            transact (fun () ->
                                app.unpersist.update mm model
                            )
                        with e ->
                            self.Value.Faulted e
                }

            let updater = LivingUpdater(Aardvark.UI.DomNode.newUpdater parent scope view)
            updater.Update(AdaptiveToken.Top)
        
            let kill = 
                { new System.IDisposable with
                    member x.Dispose() =
                        updater.Dispose()
                        parent.innerHTML <- old
                }
            new MutableApp<_,_>(mm, kill, scope.emit)
        )