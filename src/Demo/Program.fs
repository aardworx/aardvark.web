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

[<EntryPoint>]
let main argv =

    let shader =
        """#version 300 es

            #ifdef VERTEX

            uniform View {
                float scale;
                mat4 MVP;
            };

            layout(location = 0) in vec3 pos;
            layout(location = 1) in vec2 tc;
            out vec2 cc;
            void main() {
                gl_Position = vec4(scale * pos, 1.0) * MVP;
                cc = tc;
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
            let modelViewProj =
                Mod.custom (fun t ->
                    let m = model.GetValue(t)
                    let v = view.GetValue(t)
                    let p = proj.GetValue(t)
                    m * v * p
                
                )

            
            let pos =   
                V3fBuffer.ofArray [|
                    V3d(-1.0, -1.0, 0.0)
                    V3d(1.0, -1.0, 0.0)
                    V3d(1.0, 1.0, 0.0)
                    V3d(-1.0, 1.0, 0.0)
                |]
                
                
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

            let object =
                {
                    pipeline = 
                        { 
                            shader = shader 
                            uniforms =
                                Map.ofList [
                                    "scale", Mod.constant 1.0 :> IMod
                                    "MVP", modelViewProj :> IMod
                                ]
                        }
                    vertexBuffers =
                        Map.ofList [
                            "pos", BufferView.ofArray (Mod.constant pos)
                            "tc", BufferView.ofArray (Mod.constant tc)
                        ]
                    indexBuffer = Some <|  BufferView.ofArray (Mod.constant index)
                    mode = PrimitiveTopology.TriangleList
                    call = Mod.constant { faceVertexCount = 6; first = 0; instanceCount = 1 }
                }
            


            let task = 
                let prep = control.Manager.Prepare(control.FramebufferSignature, object)
                let objects = [prep]
                for o in objects do PreparedRenderObject.acquire o
                { new AbstractRenderTask() with
                    member x.Render(token) =
                        for prep in objects do
                            PreparedRenderObject.update token prep

                        for prep in objects do
                            PreparedRenderObject.render prep
                    member x.Release() =
                        for o in objects do PreparedRenderObject.release o
                }

            control.RenderTask <- task
    )



    0 // return an integer exit code
