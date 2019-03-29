namespace Aardvark.Application

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Fable.Import.Browser
open Aardvark.Rendering.WebGL


type RenderControl(canvas : HTMLCanvasElement) =
    let gl = canvas.getContext("webgl2") |> unbox<WebGL2RenderingContext>
    let ctx = Context(gl)
    let manager = new ResourceManager(ctx)

    let signature = ctx.DefaultFramebufferSignature

    let keyboard = new Keyboard(canvas)
    let mouse = new Mouse(canvas)
    let size = 
        let rect = canvas.getBoundingClientRect()
        Mod.init (V2i(int rect.width, int rect.height))

    let time = Mod.custom (fun _ -> performance.now() / 1000.0)

    let mutable task = RenderTask.empty

    let render = ref (fun (v : float) -> ())
    let caller =
        { new AdaptiveObject() with
            override x.MarkObj() = 
                window.requestAnimationFrame(!render) |> ignore
                true
            override x.Kind = 
                "RenderControl"
        }

    do 
        let mutable frameCount = 0
        let mutable baseTime = performance.now()
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
                task.Run(token)


                frameCount <- frameCount + 1
                if frameCount > 100 then
                    let n = performance.now()
                    let fps = 1000.0 * float frameCount / (n - baseTime)
                    console.log fps
                    baseTime <- n
                    frameCount <- 0

            )
            transact (fun () -> time.MarkOutdated())

        !render 0.0


    member x.Context = ctx
    member x.Manager = manager
    member x.FramebufferSignature = signature
    member x.Keyboard = keyboard :> IKeyboard
    member x.Mouse = mouse :> IMouse
    member x.Size = size :> IMod<_>
    member x.Time = time 

    member x.RenderTask
        with get() = task
        and set t =
            task.Dispose()
            task <- t
            transact (fun () -> caller.MarkOutdated())


