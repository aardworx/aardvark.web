module Program

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Rendering.WebGL
open Aardvark.Import.Browser

module FShadeTest =
    open FShade
    type Vertex = 
        { 
            [<Position>] pos : V4d
            [<Color>] c : V4d
            [<Semantic("WorldPos")>] wp : V4d
            [<Normal>] n : V3d 
            [<TexCoord>] tc : V2d 
        }
        
    type MyRecord = { ambient : float; diffuse : float }
    type MyUnion =
        | A of MyRecord
        | B of float


    type UniformScope with
        member x.Ambient : MyUnion = uniform?Ambient

    let sammy =
        sampler2d {
            texture uniform?DiffuseColorTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
        
    let constantColor (c : V4d) (v : Vertex) =
        vertex {
            return { v with c = c }
        }

    let trafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            return { v with pos = uniform.ViewProjTrafo * wp; wp = wp; n = Vec.normalize (uniform.NormalMatrix * v.n) }
        }

    let diffuseTexture (v : Vertex) =
        fragment {
            return sammy.Sample(v.tc)
        }

    [<GLSLIntrinsic("mix({0}, {1}, {2})")>]
    let lerp (a : 'a) (b : 'a) (t : float) = onlyInShaderCode "mix"

    let simpleLight (v : Vertex) =
        fragment {
            match uniform.Ambient with 
            | A a -> 
                let d = 
                    let dir = Vec.normalize (uniform.CameraLocation - v.wp.XYZ)
                    let n = Vec.normalize v.n
                    Vec.dot n dir
                let ambient = lerp 0.0 1.0 a.ambient
                return V4d((ambient + a.diffuse * (1.0 - ambient) * d) * v.c.XYZ, v.c.W)
            | B f ->
                return f * v.c
        }



[<EntryPoint>]
let main argv =

    FShade.SimpleSample.run()

    //document.addEventListener_readystatechange(fun e ->
    //    if document.readyState = "complete" then

    //        //console.error (typeof<int[]>.GetElementType())

    //        let canvas = document.createElement_canvas()
    //        canvas.tabIndex <- 1.0
    //        document.body.appendChild(canvas) |> ignore
    //        document.body.style.margin <- "0"
    //        document.body.style.padding <- "0"
    //        canvas.style.width <- "100%"
    //        canvas.style.height <- "100%"
            
    //        let control = new Aardvark.Application.RenderControl(canvas, true)

    //        let initial = CameraView.lookAt (V3d(6.0, 6.0, 4.0)) V3d.Zero V3d.OOI
    //        let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial
    //        let anim = Mod.constant true //control.Keyboard.IsDown(Aardvark.Application.Keys.Space)
    //        let angle =
    //            Mod.integrate 0.0 control.Time [
    //                anim |> Mod.map (fun a ->
    //                    if a then 
    //                        control.Time |> Mod.stepTime (fun _ dt o -> o + 0.1 * dt)
    //                    else
    //                        AFun.create id
    //                )
    //            ]


    //        let view = cam |> Mod.map CameraView.viewTrafo
    //        let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 70.0 0.1 1000.0 (float s.X / float s.Y) |> Frustum.projTrafo)




    //        let sphere =
    //            Sg.sphere 2
    //            |> Sg.trafo (Mod.constant (Trafo3d.Scale(0.5)))

    //            |> Sg.viewTrafo view
    //            |> Sg.projTrafo proj
    //            |> Sg.uniform "DiffuseColorTexture" (Mod.constant (FileTexture "pattern.jpg" :> ITexture))
                
    //        let sphere2 =
    //            Sg.sphere 4
    //            |> Sg.trafo (Mod.constant (Trafo3d.Scale(0.5) ))

    //            |> Sg.viewTrafo view
    //            |> Sg.projTrafo proj
    //            |> Sg.uniform "DiffuseColorTexture" (Mod.constant (FileTexture "cliffs_color.jpg" :> ITexture))
                    
    //        let box =
    //            Sg.box Box3d.Unit
    //            |> Sg.trafo (Mod.constant (Trafo3d.Translation(-0.5, -0.5, -0.5)))

    //            |> Sg.viewTrafo view
    //            |> Sg.projTrafo proj
    //            |> Sg.uniform "DiffuseColorTexture" (Mod.constant (FileTexture "test.jpg" :> ITexture))

    //        let rand = System.Random()
    //        let sett =
    //            let s = 2
    //            cset [
    //                for x in -s/2 .. (s - s/2 - 1) do
    //                    for y in -s/2 .. (s - s/2 - 1) do
    //                        for z in -s/2 .. (s - s/2 - 1) do
    //                            let r = rand.NextDouble()
                                
    //                            let sg =
    //                                if r > 0.6 then sphere2
    //                                elif r > 0.3 then sphere
    //                                else box

    //                            let t = V3d(float x, float y, float z) * 1.5
    //                            let axis = V3d(rand.NextDouble() * 20.0 - 10.0, rand.NextDouble() * 20.0 - 10.0, rand.NextDouble() * 20.0 - 10.0)|> Vec.normalize

    //                            let speed = rand.NextDouble() * 40.0 - 20.0
    //                            let model = angle |> Mod.map (fun a -> Trafo3d.Rotation(axis, speed * a))
    //                            let s = 
    //                                sg 
    //                                |> Sg.trafo model
    //                                |> Sg.trafo (Mod.constant <| Trafo3d.Translation t)
    //                            yield s
    //            ]

    //        let mode = Mod.init (FShadeTest.A { FShadeTest.ambient = 0.1; FShadeTest.diffuse = 0.9 })

    //        let sg =
    //            Sg.set sett
    //            |> Sg.effect [
    //                FShade.Effect.ofFunction (FShadeTest.constantColor V4d.IOOI)
    //                FShade.Effect.ofFunction FShadeTest.trafo
    //                FShade.Effect.ofFunction FShadeTest.diffuseTexture
    //                FShade.Effect.ofFunction FShadeTest.simpleLight
    //            ]
    //            |> Sg.uniform "Ambient" mode


    //        let objects = sg.RenderObjects()
    //        let task() = new RenderTask(control.FramebufferSignature, control.Manager, objects) :> IRenderTask

    //        let mutable active = true
    //        control.Keyboard.DownWithRepeats.Add(fun k ->
    //            match k with
    //            | Aardvark.Application.Keys.Space ->
    //                transact (fun () ->
    //                    match mode.Value with
    //                    | FShadeTest.A _ -> mode.Value <- FShadeTest.B 1.2
    //                    | FShadeTest.B _ -> mode.Value <- FShadeTest.A { FShadeTest.ambient = 0.1; FShadeTest.diffuse = 0.9 }
    //                )
    //            | Aardvark.Application.Keys.Delete ->
    //                if active then
    //                    active <- false
    //                    control.RenderTask <- RenderTask.empty
    //                else
    //                    active <- true
    //                    control.RenderTask <- task()
    //            | Aardvark.Application.Keys.X ->
                    
    //                if sett.Count > 0 then
    //                    //transact (fun () -> sett.Clear())
    //                    let e = sett |> Seq.item (rand.Next sett.Count)
    //                    transact (fun () -> sett.Remove e |> ignore)
    //            | _ ->
    //                ()
    //        )


    //        control.RenderTask <- task()
    //)
    0
