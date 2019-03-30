﻿namespace Aardvark.Application

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Fable.Import.Browser
open Aardvark.Rendering.WebGL
open Fable.Import.JS
open Fable.Core


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
    let mutable showFps = true
    let mutable fps = 0.0

    let overlay =
        let div = document.createElement_div()
        canvas.parentElement.appendChild(div) |> ignore
        div.style.position <- "absolute"
        div.style.right <- "5px"
        div.style.bottom <- "5px"
        div.style.color <- "white"
        div.style.fontSize <- "2em"
        div.style.fontFamily <- "Consolas"
        div
        
    let mutable frameCount = 0
    let mutable overlayVisible = false
    let mutable baseTime = performance.now()
    let hideOverlay() = 
        if overlayVisible then
            overlayVisible <- false
            overlay.style.display <- "none"
            frameCount <- 0
            
    let mutable hide = setTimeout hideOverlay 300


    let mutable inRender = false
    let render = ref (fun (v : float) -> ())
    let caller =
        { new AdaptiveObject() with
            override x.MarkObj() = 
                if not inRender then
                    clearTimeout hide
                    if not overlayVisible then
                        overlayVisible <- true
                        overlay.style.display <- "block"
                        overlay.innerText <- ""
                        baseTime <- performance.now()

                window.requestAnimationFrame(!render) |> ignore
                true
            override x.Kind = 
                "RenderControl"
        }

    let rec checkSize() =
        let rect = canvas.getBoundingClientRect()
        let s = V2i(int rect.width, int rect.height)
        if s <> size.Value then
            transact (fun () -> 
                size.Value <- s
                caller.MarkOutdated()
            )
        setTimeout checkSize 100 |> ignore
            

    do 
        checkSize()
        render := fun _ ->
            inRender <- true
            let rect = canvas.getBoundingClientRect()
            let s = V2i(int rect.width, int rect.height)
            if s <> size.Value then transact (fun () -> size.Value <- s)
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
                    fps <- 1000.0 * float frameCount / (n - baseTime)
                    console.log fps
                    baseTime <- n
                    frameCount <- 0
                    if showFps then overlay.innerText <- sprintf "%.1ffps" fps
                //if showFps then
                //    ctx2d.textAlign <- "right"
                //    ctx2d.font <- "Consolas"
                //    ctx2d.strokeStyle <- (U3.Case1 "color: white")
                //    ctx2d.strokeText(sprintf "%.1ffps" fps, rect.width, rect.height, 100.0)

            )
            transact (fun () -> time.MarkOutdated())
            inRender <- false
            clearTimeout hide
            hide <- setTimeout hideOverlay 300

        !render 0.0


    member x.Context = ctx
    member x.Manager = manager
    member x.FramebufferSignature = signature
    member x.Keyboard = keyboard :> IKeyboard
    member x.Mouse = mouse :> IMouse
    member x.Size = size :> IMod<_>
    member x.Time = time 

    member x.ShowFps
        with get() = showFps
        and set v = showFps <- v

    member x.RenderTask
        with get() = task
        and set t =
            task.Dispose()
            task <- t
            transact (fun () -> caller.MarkOutdated())

