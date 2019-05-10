namespace Aardvark.Application

open Aardvark.SceneGraph
open Aardvark.Import.Browser
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL

type ShowConfig =
    {
        antialias : bool
        scene : Aardvark.Application.RenderControl -> ISg
    }
[<AutoOpen>]
module Utilities =
    
    let show (config : ShowConfig) =

        document.addEventListener_readystatechange(fun e ->
            if document.readyState = "complete" then
                let canvas = document.createElement_canvas()
                canvas.tabIndex <- 1.0
                document.body.appendChild(canvas) |> ignore
                document.body.style.margin <- "0"
                document.body.style.padding <- "0"
                canvas.style.width <- "100%"
                canvas.style.height <- "100%"
        
                let control = new Aardvark.Application.RenderControl(canvas, config.antialias, false)

                let initial = CameraView.lookAt (V3d(6.0, 5.0, 4.0)) V3d.Zero V3d.OOI
                let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial
                let view = cam |> Mod.map CameraView.viewTrafo
                let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 70.0 0.1 1000.0 (float s.X / float s.Y) |> Frustum.projTrafo)

                let sg = 
                    config.scene control
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj

                let objects = sg.RenderObjects()
                let task() = new RenderTask(control.FramebufferSignature, control.Manager, objects) :> IRenderTask

                control.RenderTask <- task()
        )

