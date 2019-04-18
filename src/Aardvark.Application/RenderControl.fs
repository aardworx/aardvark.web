namespace Aardvark.Application

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Fable.Import.Browser
open Aardvark.Rendering.WebGL
open Fable.Import.JS
open Fable.Core
open Fable.Core.JsInterop


type ContextAttribs =
     {
        alpha : bool
        antialias : bool
        depth : bool
        powerPreference : string
        preserveDrawingBuffer : bool
        stencil : bool
     }

type EXT_sRGB =
    abstract member SRGB_EXT : float
    abstract member SRGB_ALPHA_EXT : float
    abstract member SRGB8_ALPHA8_EXT : float
    abstract member FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT : float

[<AutoOpen>]
module Exts = 
    [<Emit("Element.ALLOW_KEYBOARD_INPUT")>]
    let ALLOW_KEYBOARD_INPUT : float = jsNative

type RenderControl(canvas : HTMLCanvasElement) =

    let config =
        {
            alpha = false
            antialias = true
            depth = true
            powerPreference = "high-performance"
            preserveDrawingBuffer = true
            stencil = true
        }

    let gl = canvas.getContext("webgl2", config) |> unbox<WebGL2RenderingContext>
    let gl =
        if unbox gl then gl
        else canvas.getContext("webgl", config) |> unbox<WebGL2RenderingContext>
    let ctx = Context(gl)
    let manager = new ResourceManager(ctx)

    do
        //let e = gl.getFramebufferAttachmentParameter(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT) |> unbox<float>
        //if e = gl.LINEAR then Log.error "linear"
        //elif e = gl.SRGB_EXT then Log.error "srgb"
        //else Log.error "strange: %A" e

        canvas.addEventListener("webglcontextlost", EventListenerOrEventListenerObject.Case1 (fun _ -> Log.warn "context lost"))
        canvas.addEventListener("webglcontextrestored", EventListenerOrEventListenerObject.Case1 (fun _ -> Log.warn "context restored"))
        let dbgRenderInfo = gl.getExtension("WEBGL_debug_renderer_info") |> unbox<WEBGL_debug_renderer_info>
        if unbox dbgRenderInfo then
          let name = gl.getParameter(dbgRenderInfo.UNMASKED_RENDERER_WEBGL) |> unbox<string>
          let vendor = gl.getParameter(dbgRenderInfo.UNMASKED_VENDOR_WEBGL) |> unbox<string>
          Log.line "%s %s" vendor name

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

    
    let mutable rafap = false
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

                if not rafap then
                    window.requestAnimationFrame(!render) |> ignore
                true
            override x.Kind = 
                "RenderControl"
        }

    do 
        
        //canvas.addEventListener_pointerdown(fun e ->
        //    if unbox e.pointerType = "touch" then
        //        overlay.innerHTML <- "RAFAP"
        //        overlay.style.display <- "block"

        //        if not rafap then transact caller.MarkOutdated
        //        rafap <- not rafap
        //)

        let alt = keyboard.IsDown Keys.LeftAlt
        keyboard.KeyDown(Keys.Return).Add(fun () ->
            if document?fullscreenElement || document?mozFullScreenElement || document?webkitFullscreenElement || document?msFullscreenElement then
                if document?exitFullscreen then document?exitFullscreen()
                elif document?msExitFullscreen then document?msExitFullscreen()
                elif document?mozCancelFullScreen then document?mozCancelFullScreen()
                elif document?webkitExitFullscreen then document?webkitExitFullscreen()
            else
                if canvas?requestFullscreen then canvas?requestFullscreen()
                elif canvas?msRequestFullscreen then canvas?msRequestFullscreen()
                elif canvas?mozRequestFullScreen then canvas?mozRequestFullScreen()
                elif canvas?webkitRequestFullscreen then canvas?webkitRequestFullscreen(ALLOW_KEYBOARD_INPUT)
        )


        canvas.addEventListener("dblclick", EventListenerOrEventListenerObject.Case1 (fun e -> 
            if document?fullscreenElement || document?mozFullScreenElement || document?webkitFullscreenElement || document?msFullscreenElement then
                if document?exitFullscreen then document?exitFullscreen()
                elif document?msExitFullscreen then document?msExitFullscreen()
                elif document?mozCancelFullScreen then document?mozCancelFullScreen()
                elif document?webkitExitFullscreen then document?webkitExitFullscreen()
            else
                if canvas?requestFullscreen then canvas?requestFullscreen()
                elif canvas?msRequestFullscreen then canvas?msRequestFullscreen()
                elif canvas?mozRequestFullScreen then canvas?mozRequestFullScreen()
                elif canvas?webkitRequestFullscreen then canvas?webkitRequestFullscreen(ALLOW_KEYBOARD_INPUT)
            e.preventDefault(); e.stopPropagation()
        ), true)
               
        canvas.addEventListener("touchstart", EventListenerOrEventListenerObject.Case1 (fun e -> 
            if document?fullscreenElement || document?mozFullScreenElement || document?webkitFullscreenElement || document?msFullscreenElement then
                if document?exitFullscreen then document?exitFullscreen()
                elif document?msExitFullscreen then document?msExitFullscreen()
                elif document?mozCancelFullScreen then document?mozCancelFullScreen()
                elif document?webkitExitFullscreen then document?webkitExitFullscreen()
            else
                if canvas?requestFullscreen then canvas?requestFullscreen()
                elif canvas?msRequestFullscreen then canvas?msRequestFullscreen()
                elif canvas?mozRequestFullScreen then canvas?mozRequestFullScreen()
                elif canvas?webkitRequestFullscreen then canvas?webkitRequestFullscreen(ALLOW_KEYBOARD_INPUT)
                elif canvas?webkitRequestFullScreen then canvas?webkitRequestFullScreen()
            //if document?fullscreenElement || document?mozFullScreenElement || document?webkitFullscreenElement then
            //    if document?cancelFullScreen then document?cancelFullScreen()
            //    elif document?mozCancelFullScreen then document?mozCancelFullScreen()
            //    elif document?webkitCancelFullScreen then document?webkitCancelFullScreen()
            //else
            //    if canvas?requestFullscreen then canvas?requestFullscreen()
            //    elif canvas?webkitRequestFullscreen then canvas?webkitRequestFullscreen()
            //    elif canvas?mozRequestFullScreen then canvas?mozRequestFullScreen()
            e.preventDefault(); e.stopPropagation()
        ), true)
        
        
        

        let ctrl = keyboard.IsDown(Keys.LeftCtrl)
        keyboard.KeyDown(Keys.End).Add (fun () ->
            if ctrl.GetValue(AdaptiveToken.Top) then
                if not rafap then transact caller.MarkOutdated
                rafap <- not rafap
        )

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
            let pp = 
                caller.EvaluateAlways AdaptiveToken.Top (fun token ->

                    if canvas.width <> rect.width then canvas.width <- rect.width
                    if canvas.height <> rect.height then canvas.height <- rect.height
                    //console.log(sprintf "%.0fx%.0f" rect.width rect.height)
                    gl.viewport(0.0, 0.0, rect.width, rect.height)
                    gl.clearColor(0.0, 0.0, 0.0, 1.0)
                    gl.clearDepth(1.0)
                    gl.clear(float (int gl.COLOR_BUFFER_BIT ||| int gl.DEPTH_BUFFER_BIT))
                    let p = task.Run(token)

                    p.``then``(fun () ->
                        frameCount <- frameCount + 1
                        if frameCount > 100 then
                            let n = performance.now()
                            fps <- 1000.0 * float frameCount / (n - baseTime)
                            console.log fps
                            baseTime <- n
                            frameCount <- 0
                            if showFps then overlay.innerText <- sprintf "%.1ffps" fps
                    )
                    //if showFps then
                    //    ctx2d.textAlign <- "right"
                    //    ctx2d.font <- "Consolas"
                    //    ctx2d.strokeStyle <- (U3.Case1 "color: white")
                    //    ctx2d.strokeText(sprintf "%.1ffps" fps, rect.width, rect.height, 100.0)

                )
            pp.``then``(fun () ->
                transact (fun () -> time.MarkOutdated())
                inRender <- false
                clearTimeout hide
                hide <- setTimeout hideOverlay 300
                if rafap then 
                    setTimeout (fun () -> !render 0.0) 0 |> ignore //window.requestAnimationFrame(!render) |> ignore

            ) |> ignore
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


