namespace Aardvark.Application

open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Base.Incremental

module DefaultCameraController =

    let controlWSAD (k : IKeyboard) (time : IMod<float>) =   
        let w = k.IsDown Keys.W 
        let s = k.IsDown Keys.S
        let a = k.IsDown Keys.A 
        let d = k.IsDown Keys.D 

        let moveX = 
            Mod.map2 (fun l r ->
                if l && not r then -V2i.IO
                elif r && not l then V2i.IO
                else V2i.Zero
            ) a d

        let moveY = 
            Mod.map2 (fun f b ->
                if f && not b then V2i.OI
                elif b && not f then -V2i.OI
                else V2i.Zero
            ) w s

        let move = Mod.map2 (+) moveX moveY

        move |> Mod.map (fun m ->
            if m <> V2i.Zero then
                time |> Mod.stepTime (fun t dt (cam : CameraView)  ->
                    let dt = if dt > 10.0 then 0.0 else dt

                    let direction = float m.X * cam.Right + float m.Y * cam.Forward
                    let delta = 1.2 * dt * direction

                    cam.WithLocation(cam.Location + delta)
                )
            else
                AFun.create id
        )

    let controlLookAround (m : IMouse) =
        let down = m.IsDown(MouseButtons.Left)
        let location = m.Position |> Mod.map (fun pp -> pp.Position)

        down |> Mod.map (fun d ->
            if d then
                location |> Mod.step (fun op delta (cam : CameraView) ->
                    let trafo =
                        M44d.Rotation(cam.Right, float delta.Y * -0.01) *
                        M44d.Rotation(cam.Sky, float delta.X * -0.01)

                    let newForward = trafo.TransformDir cam.Forward |> Vec.normalize
                    cam.WithForward(newForward)

                ) 
            else
                AFun.create id
        )


    //let controlOrbitAround (m : IMouse) (center : IMod<V3d>) =
    //    let down = m.IsDown(MouseButtons.Left)
    //    let location = m.Position |> Mod.map (fun pp -> pp.Position)

    //    adaptive {
    //        let! d = down
    //        let! center' = center
    //        if d then
    //            return location |> Mod.step (fun op delta (cam : CameraView) ->
    //                let trafo =
    //                    M44d.Rotation(cam.Right, float delta.Y * -0.01) *
    //                    M44d.Rotation(cam.Sky, float delta.X * -0.01)

    //                let newLocation = trafo.TransformDir cam.Location
    //                let tempcam = cam.WithLocation newLocation
    //                let newForward = center' - newLocation |> Vec.normalize

    //                tempcam.WithForward newForward
    //            ) 
    //        else
    //            return AdaptiveFunc.Identity
    //    }

    let controlPan (m : IMouse) =
        let down = m.IsDown(MouseButtons.Middle)
        let location = m.Position |> Mod.map (fun pp -> pp.Position)

        down |> Mod.map (fun d ->
            if d then
                location |> Mod.step (fun p delta (cam : CameraView) ->

                    let step = 0.05 * (cam.Down * float delta.Y + cam.Right * float delta.X)

                    cam.WithLocation(cam.Location + step)

                )
            else
                AFun.create id
        ) 

    let controlZoom (m : IMouse) =
        let down = m.IsDown(MouseButtons.Right)
        let location = m.Position |> Mod.map (fun pp -> pp.Position)

        down |> Mod.map (fun d ->
            if d then
                location |> Mod.step (fun p delta (cam : CameraView) ->
                    let step = -0.05 * (cam.Forward * float delta.Y)
                    cam.WithLocation(cam.Location + step)

                )
            else
                AFun.create id
        ) 

    let controllScroll (m : IMouse) (time : IMod<float>) =
        let active = Mod.init false
        
        let speed = ref 0.0
        let s = m.Scroll.Subscribe(fun d ->
            speed := !speed + d * 120.0 
            if not active.Value then
                transact (fun () -> Mod.change active true)
        )

        active |> Mod.map (fun a ->
            if a then
                time |> Mod.step (fun t dt (cam : CameraView) ->
                    let v = !speed * 0.004 ** dt
                    speed := v

                    let df = v * dt
                    let direction = 0.10 * (cam.Forward * df)

                    if abs v < 0.1 then
                        transact (fun () -> active.Value <- false)
                        //Mod.changeAfterEvaluation active false

                    cam.WithLocation(cam.Location + direction)

                )
            else
                AFun.create id
        )

    let control (mouse : IMouse) (keyboard : IKeyboard) (time : IMod<float>) (cam : CameraView) : IMod<CameraView> =
        Mod.integrate cam time [
            controlWSAD keyboard time
            controlLookAround mouse
            controlPan mouse
            controlZoom mouse
            controllScroll mouse time
        ]
