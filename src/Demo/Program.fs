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

type IRenderTask =  
    inherit System.IDisposable
    abstract member Run : V2i -> unit

module RenderTask =
    let empty = 
        { new IRenderTask with
            member x.Run _ = ()
            member x.Dispose() = ()
        }



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
            
            let mouse = new Aardvark.Application.Mouse(canvas) :> Aardvark.Application.IMouse
            let keyboard = new Aardvark.Application.Keyboard(canvas)  :> Aardvark.Application.IKeyboard
            let time = Mod.custom (fun _ -> performance.now() / 1000.0)
            let size = Mod.init (V2i.II)




            let initial = CameraView.lookAt (V3d.III * 6.0) V3d.Zero V3d.OOI
            let cam = Aardvark.Application.DefaultCameraController.control mouse keyboard time initial
            let anim = keyboard.IsDown(Aardvark.Application.Keys.Space)
            let angle =
                Mod.integrate 0.0 time [
                    anim |> Mod.map (fun a ->
                        if a then 
                            time |> Mod.stepTime (fun _ dt o -> o + 0.1 * dt)
                        else
                            AFun.create id
                    )
                ]



            let model = angle |> Mod.map Trafo3d.RotationZ
            let view = cam |> Mod.map CameraView.viewTrafo
            let proj = size |> Mod.map (fun s ->  Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo)
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
            



            let gl = canvas.getContext("webgl2") |> unbox<Aardvark.Rendering.WebGL.WebGL2RenderingContext>
            let ctx = Context(gl)
            let manager = new ResourceManager(ctx)
            let prep = manager.Prepare(ctx.DefaultFramebufferSignature, object)
            PreparedRenderObject.acquire prep

            let objects = [prep]



            let mutable baseTime = performance.now()
            let mutable frameCount = 0

            let render = ref (fun (v : float) -> ())
            let caller =
                { new AdaptiveObject() with
                    override x.MarkObj() = 
                        window.requestAnimationFrame(!render) |> ignore
                        true
                    override x.Kind = "RenderView"
                }

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
                    for prep in objects do
                        PreparedRenderObject.update token prep

                    for prep in objects do
                        PreparedRenderObject.render prep


                    frameCount <- frameCount + 1
                    if frameCount > 100 then
                        let n = performance.now()
                        let fps = 1000.0 * float frameCount / (n - baseTime)
                        console.log fps
                        baseTime <- n
                        frameCount <- 0

                    //setTimeout render 16 |> ignore
                )
                transact (fun () -> time.MarkOutdated())

            !render 0.0

    )



    0 // return an integer exit code
