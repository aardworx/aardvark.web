module Program

open System
open Aardvark.Base
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL
open Aardvark.SceneGraph

[<EntryPoint>]
let main argv =

    let shader =
        """#version 300 es

            #ifdef VERTEX

            uniform View {
                float scale;
                mat4 ModelViewProjTrafo;
            };

            layout(location = 0) in vec3 Positions;
            layout(location = 1) in vec2 DiffuseColorCoordinates;
            out vec2 cc;
            void main() {
                gl_Position = vec4(scale * Positions, 1.0) * ModelViewProjTrafo;
                cc = DiffuseColorCoordinates;
            }
            #endif

            #ifdef FRAGMENT
            precision highp float;
            precision highp int;
            in vec2 cc;
            layout(location = 0) out vec4 Colors;
            void main() {
                Colors = vec4(cc.x,cc.y,1,1);
            }
            #endif
        """


    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then
            let canvas = document.createElement_canvas()
            canvas.tabIndex <- 1.0
            document.body.appendChild(canvas) |> ignore
            document.body.style.margin <- "0"
            document.body.style.padding <- "0"
            canvas.style.width <- "100%"
            canvas.style.height <- "100%"
            
            let control = new Aardvark.Application.RenderControl(canvas)




            let initial = CameraView.lookAt (V3d.III * 6.0) V3d.Zero V3d.OOI
            let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial
            let anim = control.Keyboard.IsDown(Aardvark.Application.Keys.Space)
            let angle =
                Mod.integrate 0.0 control.Time [
                    anim |> Mod.map (fun a ->
                        if a then 
                            control.Time |> Mod.stepTime (fun _ dt o -> o + 0.1 * dt)
                        else
                            AFun.create id
                    )
                ]

            let model = angle |> Mod.map Trafo3d.RotationZ
            let view = cam |> Mod.map CameraView.viewTrafo
            let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
            //let modelViewProj =
            //    Mod.custom (fun t ->
            //        let m = model.GetValue(t)
            //        let v = view.GetValue(t)
            //        let p = proj.GetValue(t)
            //        m * v * p
                
            //    )

            
            let pos = V3fList()
            pos.Add(V3d(-1.0, -1.0, 0.0))
            pos.Add(V3d(1.0, -1.0, 0.0))
            pos.Add(V3d(1.0, 1.0, 0.0))
            pos.Add(V3d(-1.0, 1.0, 0.0))

            //let pos =   
            //    V3fBuffer.ofArray [|
            //        V3d(-1.0, -1.0, 0.0)
            //        V3d(1.0, -1.0, 0.0)
            //        V3d(1.0, 1.0, 0.0)
            //        V3d(-1.0, 1.0, 0.0)
            //    |]
                
                
            let tc =   
                V2fBuffer.ofArray [|
                    V2d(0.0, 0.0)
                    V2d(1.0, 0.0)
                    V2d(1.0, 1.0)
                    V2d(0.0, 1.0)
                |]

            let index =
                Int32Buffer.ofArray [|
                    0; 1; 2
                    0; 2; 3
                |]

            let sg =
                Sg.draw PrimitiveTopology.TriangleList
                |> Sg.index index
                |> Sg.vertexAttribute DefaultSemantic.Positions pos
                |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates tc
                |> Sg.uniform "scale" (Mod.constant 1.0)
                |> Sg.trafo model
                |> Sg.viewTrafo view
                |> Sg.projTrafo proj
                |> Sg.shader shader

            let sg =
                let rand = System.Random()
                Sg.ofList [
                    for i in 0 .. 200 do
                        let phi = rand.NextDouble() * Constant.PiTimesTwo
                        let r = rand.NextDouble() * 10.0

                        let t = V3d(cos phi, sin phi, 0.1 * (rand.NextDouble() - 0.5)) * r
                        yield sg |> Sg.trafo (Mod.constant <| Trafo3d.Translation t)
                ]


            let objects = sg.RenderObjects()


            let task() = 
                let prep = objects |> ASet.map (fun o -> control.Manager.Prepare(control.FramebufferSignature, o))

                let reader = prep.GetReader()
                { new AbstractRenderTask() with
                    member x.Render(token) =
                        for op in reader.GetOperations token do
                            match op with
                            | Add(_,o) -> PreparedRenderObject.acquire o
                            | Rem(_,o) -> PreparedRenderObject.release o

                        for prep in reader.State do
                            PreparedRenderObject.update token prep

                        for prep in reader.State do
                            PreparedRenderObject.render prep
                    member x.Release() =
                        for o in reader.State do PreparedRenderObject.release o
                        reader.Dispose()
                }


            let mutable active = true

            control.Keyboard.DownWithRepeats.Add(fun k ->
                match k with
                | Aardvark.Application.Keys.Return ->
                    if active then
                        active <- false
                        control.RenderTask <- RenderTask.empty
                    else
                        active <- true
                        control.RenderTask <- task()
                | _ ->
                    ()
            )


            control.RenderTask <- task()
    )



    0 // return an integer exit code
