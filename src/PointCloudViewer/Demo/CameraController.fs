namespace Example

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.UI

module Integrator = 

    let inline private dbl (one) = one + one

    let inline rungeKutta (f : ^t -> ^a -> ^da) (y0 : ^a) (h : ^t) : ^a =
        let twa : ^t = dbl LanguagePrimitives.GenericOne
        let half : ^t = LanguagePrimitives.GenericOne / twa
        let hHalf = h * half

        let k1 = h * f LanguagePrimitives.GenericZero y0
        let k2 = h * f hHalf (y0 + k1 * half)
        let k3 = h * f hHalf (y0 + k2 * half)
        let k4 = h * f h (y0 + k3)
        let sixth = LanguagePrimitives.GenericOne / (dbl twa + twa)
        y0 + (k1 + twa*k2 + twa*k3 + k4) * sixth

    let inline euler (f : ^t -> ^a -> ^da) (y0 : ^a) (h : ^t) : ^a=
        y0 + h * f LanguagePrimitives.GenericZero y0

    let rec integrate (maxDt : float) (f : 'm -> float -> 'm) (m0 : 'm) (dt : float) =
        if dt <= maxDt then
            f m0 dt
        else
            integrate maxDt f (f m0 maxDt) (dt - maxDt) 




[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OrbitState =
    let clamp (min : float) (max : float) (value : float) =
        if value > max then max
        elif value < min then min
        else value

    let withView (s : OrbitState) =
        let ct = cos s.theta
        let l = 
            V3d(
                cos s.phi * ct * s.radius + s.center.X,
                sin s.phi * ct * s.radius + s.center.Y,
                sin s.theta * s.radius + s.center.Z
            )
        //let l = V2d(s.phi, s.theta).CartesianFromSpherical() * s.radius + s.center
        { s with view = CameraView.lookAt l s.center s.sky }

    let create (center : V3d) (phi : float) (theta : float) (r : float) =
        let thetaRange = V2d(-Constant.PiHalf + 0.0001, Constant.PiHalf - 0.0001)
        let radiusRange = V2d(0.1, 40000.0)

        let r = clamp radiusRange.X radiusRange.Y r
        let theta = clamp thetaRange.X thetaRange.Y theta

        withView {
            sky     = V3d.OOI
            center  = center
            phi     = phi
            theta   = theta
            radius  = r

            targetPhi = phi
            targetTheta = theta
            targetRadius = r
            targetCenter = center

            lastTap = None
            dragStarts = MapExt.empty
            dragEnters = MapExt.empty
            lastRender = None
            view = Unchecked.defaultof<_>

            radiusRange = radiusRange
            thetaRange = thetaRange
            moveSensitivity = 0.5
            zoomSensitivity = 1.0
            speed = 0.3
        }



type OrbitMessage =
    | Tap of id : float * pos : V2i
    | PointerDown of id : float * isTouch : bool * pos : V2i
    | PointerUp of  id : float * isTouch : bool * V2i
    | PointerMove of id : float * isTouch : bool * V2i
    | Wheel of V2d


    | Rendered
    | SetTargetCenter of V3d
    | SetTargetPhi of float
    | SetTargetTheta of float
    | SetTargetRadius of float



module OrbitController = 
    let time() = Aardvark.Import.Browser.performance.now() |> MicroTime.FromMilliseconds

    let private clamp (min : float) (max : float) (value : float) =
        if value > max then max
        elif value < min then min
        else value

    let initial = OrbitState.create V3d.Zero Constant.PiQuarter Constant.PiQuarter 4.0

    let rec update (model : OrbitState) (msg : OrbitMessage) =
        match msg with
        | Tap(id, pos) ->
            let n = time()
            match model.lastTap with
            | Some (ot, op) ->
                if (n - ot).TotalMilliseconds < 400.0 && (pos - op).Length < 40.0 then
                    
                    Aardvark.Import.Browser.document.getElementsByClassName("label").[0].innerHTML <- sprintf "dbl %.0f: %A" id pos
            | None ->
                ()
            { model with lastTap = Some (n, pos) }
            //Aardvark.Import.Browser.document.getElementsByClassName("label").[0].innerHTML <- sprintf "%.0f: %A" id pos

            //model

        | SetTargetPhi tphi ->
            OrbitState.withView { model with targetPhi = tphi }
        
        | SetTargetTheta ttheta ->
            OrbitState.withView { model with targetPhi = clamp model.thetaRange.X model.thetaRange.Y ttheta }
        
        | SetTargetRadius tr ->
            OrbitState.withView { model with targetRadius = clamp model.radiusRange.X model.radiusRange.Y tr }

        | SetTargetCenter tc ->
            OrbitState.withView { model with targetCenter = tc }
        

        | PointerDown(id, isTouch, p) ->
            let s = MapExt.add id p model.dragStarts
            { model with 
                dragStarts = s
                dragEnters = MapExt.add id (p, time()) model.dragEnters
                lastRender = None 
            }

        | PointerUp(id, isTouch, p) ->
            
            let s = MapExt.remove id model.dragStarts

            let model = 
                match MapExt.tryFind id model.dragEnters with
                | Some (op, s) ->
                    let n = time()
                    let dt = n - s
                    if dt.TotalMilliseconds < 400.0 && (p - op).Length < 20.0 then
                        update model (Tap(id, p))
                    else
                        model
                | None ->
                    model



            { model with 
                dragStarts = s; 
                dragEnters = MapExt.remove id model.dragEnters
                lastRender = None 
            }

        | Wheel delta ->
            OrbitState.withView { model with targetRadius = clamp model.radiusRange.X model.radiusRange.Y (model.targetRadius + delta.Y * model.zoomSensitivity) }

        | PointerMove(id, isTouch, p) ->
            let down = model.dragStarts.Count


            match down with
            | 1 -> 
                match MapExt.tryFind id model.dragStarts with
                | Some(start) ->
                    let delta = p - start
                    let dphi = float delta.X * -0.01 * model.moveSensitivity
                    let dtheta = float delta.Y * 0.01 * model.moveSensitivity
            
                    OrbitState.withView 
                        { model with
                            dragStarts = MapExt.add id p model.dragStarts
                            targetPhi = model.targetPhi + dphi
                            targetTheta = clamp model.thetaRange.X model.thetaRange.Y (model.targetTheta + dtheta)
                        }
                | None ->
                    model
            | 2 when isTouch ->
                match MapExt.tryFind id model.dragStarts with
                | Some op ->
                    let np = p
                    let otherId, otherPos = model.dragStarts |> MapExt.toSeq |> Seq.find (fun (k,_) -> k <> id)

                    let scale = Vec.length (np - otherPos) / Vec.length (op - otherPos)
                    
                    let r = clamp model.radiusRange.X model.radiusRange.Y (model.targetRadius / scale)

                    OrbitState.withView 
                        { model with
                            dragStarts = MapExt.add id p model.dragStarts
                            targetRadius = r
                        }

                | None ->
                    model
            | _ ->
                model
        | Rendered ->
            let dphi = model.targetPhi - model.phi
            let dtheta = model.targetTheta - model.theta
            let dradius = model.targetRadius - model.radius
            let dcenter = model.targetCenter - model.center

            let now = time()
            let dt =
                match model.lastRender with
                | Some last -> (now - last)
                | None -> MicroTime.Zero
            
            let delta = model.speed * dt.TotalSeconds / 0.05
            let part = if dt.TotalSeconds > 0.0 then clamp 0.0 1.0 delta else 0.0
            let model = { model with lastRender = Some now }

            let model = 
                if abs dphi > 0.0 then
                    if Fun.IsTiny(dphi, 1E-4) then
                        Log.warn "phi done"
                        OrbitState.withView { model with phi = model.targetPhi }
                    else
                        OrbitState.withView { model with phi = model.phi + part * dphi }
                else
                    model

            let model = 
                if abs dtheta > 0.0 then
                    if Fun.IsTiny(dtheta, 1E-4) then
                        Log.warn "theta done"
                        OrbitState.withView { model with theta = model.targetTheta }
                    else
                        OrbitState.withView { model with theta  = model.theta + part * dtheta }
                else
                    model

            let model = 
                if abs dradius > 0.0 then
                    if Fun.IsTiny(dradius, 1E-4) then
                        Log.warn "radius done"
                        OrbitState.withView { model with radius = model.targetRadius }
                    else
                        OrbitState.withView { model with radius  = model.radius + part * dradius }
                else
                    model

            let model = 
                if dcenter.Length > 0.0 then
                    if dcenter.Length < 1E-4 then
                        Log.warn "center done"
                        OrbitState.withView { model with center = model.targetCenter }
                    else
                        OrbitState.withView { model with center  = model.center + part * dcenter }
                else
                    model

            model

    open Aardvark.Import.JS
    open Aardvark.Import.Browser

    let getAttributes (model : MOrbitState) (f : OrbitMessage -> 'msg) =
        let down = model.dragEnters |> Mod.map (MapExt.isEmpty >> not)
        attributes {
            //yield style "touch-action: none"
            yield always <| mousedown (fun e -> 
                    let target = unbox<HTMLCanvasElement> e.target
                    e.preventDefault()
                    e.stopPropagation()
                    PointerDown(e.which, false, V2i(int e.clientX, int e.clientY))|> f
                )
            yield always <| mouseup (fun e -> 
                    let target = unbox<HTMLElement> e.target
                    e.preventDefault()
                    e.stopPropagation()
                    PointerUp(e.which, false, V2i(int e.clientX, int e.clientY))|> f
                )
            yield always <| mousemove (fun e -> 
                e.preventDefault()
                e.stopPropagation()
                PointerMove(e.which, false, V2i(int e.clientX, int e.clientY)) |> f
            )
            yield always <| rendered (fun _ -> Rendered |> f)
            yield always <| wheel (fun e -> Wheel (V2d(0.0, -e.wheelDeltaY / 120.0)) |> f)

            yield always <| touchstart (fun e ->
                e.preventDefault()
                e.stopPropagation()
                seq {
                    for i in 0 .. int e.touches.length - 1 do
                        let t = e.touches.[i]
                        yield PointerDown(t.identifier, true, V2i(int t.clientX, int t.clientY)) |> f
                }
            )
            yield always <| touchend (fun e ->
                e.preventDefault()
                e.stopPropagation()
                seq {
                    for i in 0 .. int e.changedTouches.length - 1 do
                        let t = e.changedTouches.[i]
                        yield PointerUp(t.identifier, true, V2i(int t.clientX, int t.clientY)) |> f
                }
            )

            yield always <| touchmove (fun e ->
                e.preventDefault()
                e.stopPropagation()
                seq {
                    for i in 0 .. int e.touches.length - 1 do
                        let t = e.touches.[i]
                        yield PointerMove(t.identifier, true, V2i(int t.clientX, int t.clientY)) |> f
                }
            )
        }
       
    let inline control (model : MOrbitState) attributes (mapping : OrbitMessage -> 'msg) (scene : Aardvark.Application.RenderControl -> ISg) =
        let a = AttributeMap.ofList attributes
        let att = AttributeMap.union [a; getAttributes model mapping]
        DomNode.Render(att, scene)



module FreeFlyController =
    open Aardvark.Base.Incremental.Operators
                        

    let initial =
        {
            view = CameraView.lookAt (6.0 * V3d.III) V3d.Zero V3d.OOI
                                    
            orbitCenter = None
            stash = None
            sensitivity = 1.0
            panFactor  = 0.01
            zoomFactor = 0.01
            rotationFactor = 0.01            
            dolly = false

            lastTime = None
            moveVec = V3d.Zero
            rotateVec = V3d.Zero
            dragStart = V2i.Zero
            movePos = V2i.Zero
            look = false; zoom = false; pan = false                    
            forward = false; backward = false; left = false; right = false
            isWheel = false;
            moveSpeed = 0.0
            scrollSensitivity = 0.8
            scrolling = false

            freeFlyConfig = FreeFlyConfig.initial

            targetJump = None

            targetPhiTheta = V2d.Zero
            targetZoom = 0.0
            targetDolly = 0.0
            animating = false
            targetPan = V2d.Zero
            panSpeed = 0.0
        }

    let md (f : float) (state : CameraControllerState) =
        (state.freeFlyConfig.dollyConstant + abs f * exp (state.freeFlyConfig.dollyDamping )) * float (sign f)
    let rd (f : float) (state : CameraControllerState) =
        (state.freeFlyConfig.jumpAtConstant + abs f * state.freeFlyConfig.jumpAtDamping) * float (sign f)
    
    let rd3 (v : V3d) (state : CameraControllerState) =
        let rd v = rd v state
        V3d(rd v.X, rd v.Y, rd v.Z)

    let vectorFromTo (p : V3d) (q : V3d) (state : CameraControllerState) =
        rd3 (p - q) state

    //let angleFromTo (x1 : V3d) (y1 : V3d) (z1 : V3d) (x2 : V3d) (y2 : V3d) (z2 : V3d) (state : CameraControllerState) =
    //    let aa = (Rot3d.FromFrame(x2,y2,z2)).ToAngleAxis()
    //    Rot3d.FromAngleAxis(rd3 aa state)

    //let transformRot (cv : CameraView) (r : Rot3d) =
    //    if r.GetEulerAngles().AllSmallerOrEqual(1E-9) then
    //        cv
    //    else
    //        let fw = r.TransformDir cv.Forward
    //        let up = r.TransformDir cv.Up
    //        (cv.WithForward fw).WithUp up
        

    type CameraMotion = { dWorldPos : V3d; dPos : V3d; dRot : V3d; dWorldForward : V3d; dWorldUp : V3d; dMoveSpeed : float; dZoom : float; dPan : V2d; dDolly : float } with
        static member Zero = { dWorldPos = V3d.Zero; dPos = V3d.Zero; dRot = V3d.Zero; dWorldForward = V3d.Zero; dWorldUp = V3d.Zero; dMoveSpeed = 0.0; dZoom = 0.0; dPan = V2d.Zero; dDolly = 0.0 }

        static member (+) (cam : CameraView, motion : CameraMotion) =
            let cam = 
                cam.WithLocation(
                    cam.Location +
                    motion.dPos.X * cam.Right +
                    motion.dPos.Y * cam.Up +
                    motion.dPos.Z * cam.Forward
                )

            let cam = 
                cam.WithLocation(
                    cam.Location +
                    motion.dPan.X * cam.Right  +
                    motion.dPan.Y * cam.Up 
                )
            
            let cam =
                cam.WithLocation(
                    cam.Location +
                    motion.dDolly * cam.Forward
                )

            let cam =
                cam.WithLocation(
                    cam.Location +
                    motion.dZoom * cam.Forward
                )
            
            let cam =
                let trafo =
                    M44d.Rotation(cam.Right, motion.dRot.X) *
                    M44d.Rotation(cam.Sky, motion.dRot.Y) *
                    M44d.Rotation(cam.Forward, motion.dRot.Z)
                    
                if trafo = M44d.Identity then cam
                else
                    let forbidden = V3d.OOI |> Vec.normalize
                    
                    let newForward = trafo.TransformDir cam.Forward |> Vec.normalize

                    let newForward =
                        if abs (Vec.dot forbidden newForward) > 0.99 then
                            let trafo2 =
                                M44d.Rotation(cam.Right, 0.0) *
                                M44d.Rotation(cam.Sky, motion.dRot.Y) *
                                M44d.Rotation(cam.Forward, motion.dRot.Z)
                    
                            trafo2.TransformDir cam.Forward |> Vec.normalize
                        else
                            newForward

                    cam.WithForward newForward
                    
            let cam = 

                if Fun.IsTiny(motion.dWorldForward.Length,(1E-4)) &&
                   Fun.IsTiny(motion.dWorldUp.Length,(1E-4)) &&
                   Fun.IsTiny(motion.dWorldPos.Length,(1E-4))
                then
                    cam
                else
                    //let location = cam.Location + motion.dWorldPos
                    //let center = cam.Location + cam.Forward + motion.dWorldForward
                    //let sky = 
                    //    let upPoint = cam.Location + cam.Up + motion.dWorldUp
                    //    (upPoint - cam.Location).Normalized
                        
                    //CameraView.lookAt location center sky

                    let location = cam.Location + motion.dWorldPos
                    let centerPoint = cam.Location + cam.Forward + motion.dWorldForward
                    let upPoint = cam.Location + cam.Up + motion.dWorldUp
                        
                    let fw = (centerPoint - location).Normalized
                    let up = (upPoint - location).Normalized
                    let ri = Vec.cross fw up |> Vec.normalize

                    //gram schmidt
                    let up1 = up - Vec.dot fw up * fw |> Vec.normalize
                    let ri1 = ri - Vec.dot fw ri * fw - Vec.dot up1 ri * up1 |> Vec.normalize

                    CameraView(V3d.OOI,location,fw,up1,ri1)


            cam

        static member (+) (l : CameraMotion, r : CameraMotion) =
            // TODO: correct?
            let trafo =
                M44d.Rotation(V3d.IOO, l.dRot.X) *
                M44d.Rotation(V3d.OIO, l.dRot.Y) *
                M44d.Rotation(V3d.OOI, l.dRot.Z)

            {
                dWorldPos = l.dWorldPos + r.dWorldPos
                dPos = l.dPos + trafo.TransformDir r.dPos
                dRot = l.dRot + r.dRot
                dWorldForward = l.dWorldForward + r.dWorldForward
                dWorldUp = l.dWorldUp + r.dWorldUp
                dMoveSpeed = l.dMoveSpeed + r.dMoveSpeed
                dZoom = l.dZoom + r.dZoom
                dPan = l.dPan + r.dPan
                dDolly = l.dDolly + r.dDolly
            }

        static member (*) (motion : CameraMotion, f : float) =
            { dWorldPos = motion.dWorldPos * f; dPos = motion.dPos * f; dRot = motion.dRot * f; dWorldForward = motion.dWorldForward * f; dWorldUp = motion.dWorldUp * f; dMoveSpeed = motion.dMoveSpeed * f; dZoom = motion.dZoom * f; dPan = motion.dPan * f; dDolly = motion.dDolly * f}

        static member (*) (f : float, motion : CameraMotion) = motion * f
            
        static member (+) (state : CameraControllerState, motion : CameraMotion) =
            let clamping (max : float) (v : float) =
                if max < 0.0 then
                    if v < max then max
                    else v
                else
                    if v > max then max
                    else v
                
            let clampedV3d (origin : V3d) (dtarget : V3d) (dendpoint : V3d) =
                let dir = dtarget.Normalized
                //let ray = Ray3d(origin, dir)
                let tO = Vec.dot dtarget dir //ray.GetTOfProjectedPoint (origin + dtarget)
                let tS = Vec.dot dendpoint dir //ray.GetTOfProjectedPoint (origin + dendpoint)
                if tS > tO then 
                    dtarget
                else 
                    dendpoint

            let dWorldPos = 
                let loc = CameraView.location state.view
                match state.targetJump with
                | Some jump ->
                    clampedV3d loc (CameraView.location jump - loc) (motion.dWorldPos)
                | None ->
                    V3d.Zero

            let dWorldForward =
                let fwp = (CameraView.location state.view) + (CameraView.forward state.view)
                match state.targetJump with
                | Some jump ->
                    clampedV3d fwp (CameraView.location jump + CameraView.forward jump - fwp) (motion.dWorldForward)
                | None ->
                    V3d.Zero
                    
            let dWorldUp =
                let upp = (CameraView.location state.view) + (CameraView.up state.view)
                match state.targetJump with
                | Some jump ->
                    clampedV3d upp (CameraView.location jump + CameraView.up jump - upp) (motion.dWorldUp)
                | None ->
                    V3d.Zero

            let motion = 
                { motion with 
                    dRot = V3d(clamping state.targetPhiTheta.Y motion.dRot.X, clamping state.targetPhiTheta.X motion.dRot.Y , motion.dRot.Z)
                    dWorldPos = dWorldPos
                    dWorldForward = dWorldForward
                    dWorldUp = dWorldUp
                }
                
            let tj = 
                state.targetJump |> Option.bind ( fun j ->
                    if 
                       Vec.length ((CameraView.location j) - (CameraView.location state.view)) < 1E-4 &&
                       Vec.length ((CameraView.location j + CameraView.forward j) - (CameraView.location state.view + CameraView.forward state.view)) > 1E-4 &&
                       Vec.length ((CameraView.location j + CameraView.up j) -      (CameraView.location state.view + CameraView.up state.view)     ) > 1E-4 then
                        None
                    elif (state.pan || state.look || state.dolly || (abs state.targetZoom > 0.05)) || not (Fun.IsTiny state.rotateVec.X && Fun.IsTiny state.rotateVec.Y && Fun.IsTiny state.rotateVec.Z) then
                        None
                    else
                        Some j
                )

            { state with 
                view = state.view + { motion with dRot = motion.dRot + state.rotateVec }
                moveSpeed = state.moveSpeed + motion.dMoveSpeed 
                targetPhiTheta = state.targetPhiTheta - V2d(motion.dRot.Y, motion.dRot.X)
                targetZoom = state.targetZoom - motion.dZoom
                targetPan = state.targetPan - motion.dPan
                targetDolly = state.targetDolly - motion.dDolly
                targetJump = tj
            }

    type Message =     
        | Down of button : MouseButtons * pos : V2i
        | Up of button : MouseButtons
        | Wheel of V2d
        | Move of V2i
        | KeyDown of key : Keys
        | KeyUp of key : Keys
        | Blur
        | Rendered
        | JumpTo of CameraView
        | Rotate of degrees : float
        //| MoveMovStick of TouchStickState
        //| ReleaseMovStick
        //| MoveRotStick of TouchStickState
        //| ReleaseRotStick
        | Nop

    let initial' (dist:float) =
        { initial with view = CameraView.lookAt (dist * V3d.III) V3d.Zero V3d.OOI }

    //let sw = System.Diagnostics.Stopwatch()
    //do sw.Start()


    let now() = Aardvark.Import.Browser.performance.now() / 1000.0

    let withTime (model : CameraControllerState) =
        { model with lastTime = Some (now()); view = model.view.WithLocation(model.view.Location) }

    let dummyChange (model : CameraControllerState) =
        { model with view = model.view.WithLocation(model.view.Location) }

    let startAnimation (model : CameraControllerState) =
        if not model.animating then
            { model with 
                view = model.view.WithLocation model.view.Location
                animating = true
                lastTime = Some (now())
            }
        else
            model

    let stopAnimation (model : CameraControllerState) =
        if model.animating then
            { model with 
                animating = false
                lastTime = None
            }
        else
            model
    
    let exp x = Math.Pow(Math.E, x)
    
    type V3d with
        member x.AllTiny =
            Fun.IsTiny x.X && Fun.IsTiny x.Y && Fun.IsTiny x.Z

    let update (model : CameraControllerState) (message : Message) =
        match message with
            | Nop -> model
            | Blur ->
                { model with 
                    lastTime = None
                    moveVec = V3d.Zero
                    dragStart = V2i.Zero
                    look = false; zoom = false; pan = false                    
                    forward = false; backward = false; left = false; right = false
                }
            | Rendered ->
                let now = now()

                let move (state : CameraControllerState) =
                    if state.moveVec.AllTiny |> not then
                        { CameraMotion.Zero with
                            dPos = state.moveVec * exp state.freeFlyConfig.moveSensitivity
                        }
                    else
                        CameraMotion.Zero

                let pan (state : CameraControllerState) =
                    if state.targetPan.Length > 0.05 then
                        let tt = (state.freeFlyConfig.panConstant + abs state.targetPan.X * exp (state.freeFlyConfig.panDamping )) * float (sign state.targetPan.X)
                        let tu = (state.freeFlyConfig.panConstant + abs state.targetPan.Y * exp (state.freeFlyConfig.panDamping )) * float (sign state.targetPan.Y)
                        { CameraMotion.Zero with
                            dPan = V2d(tt,tu)
                        }
                    else
                        CameraMotion.Zero
                
                let dolly (state : CameraControllerState) =
                    if abs state.targetDolly > 0.05 then
                        let dd = (state.freeFlyConfig.dollyConstant + abs state.targetDolly * exp (state.freeFlyConfig.dollyDamping )) * float (sign state.targetDolly)
                        { CameraMotion.Zero with
                            dDolly = dd
                        }
                    else
                        CameraMotion.Zero

                let zoom (state : CameraControllerState) =
                    if abs state.targetZoom > 0.05 then
                        let dd = (state.freeFlyConfig.zoomConstant + abs state.targetZoom * exp (state.freeFlyConfig.zoomDamping )) * float (sign state.targetZoom)
                        { CameraMotion.Zero with
                            dZoom = dd
                        }
                    else
                        CameraMotion.Zero

                let look (state : CameraControllerState) =
                    if state.targetPhiTheta <> V2d.Zero then
                    
                        let rr = (state.freeFlyConfig.lookAtConstant + abs state.targetPhiTheta.Y * state.freeFlyConfig.lookAtDamping) * float (sign (state.targetPhiTheta.Y))
                        let ru = (state.freeFlyConfig.lookAtConstant + abs state.targetPhiTheta.X * state.freeFlyConfig.lookAtDamping) * float (sign (state.targetPhiTheta.X))

                        { CameraMotion.Zero with
                            dRot = V3d(rr, ru, 0.0)
                        }
                    else
                        CameraMotion.Zero
                
                let jump (state : CameraControllerState) =
                    match state.targetJump with
                    | None -> 
                        CameraMotion.Zero,true
                    | Some cv ->
                        let dpos = 
                            let tgt = (CameraView.location cv) - (CameraView.location model.view)
                            rd3 tgt state
                            
                        let dWorldForward = 
                            let tgt = (CameraView.location cv + CameraView.forward cv) - (CameraView.location model.view + CameraView.forward model.view)
                            rd3 tgt state

                        let dWorldUp = 
                            let tgt = (CameraView.location cv + CameraView.up cv) - (CameraView.location model.view + CameraView.up model.view)
                            rd3 tgt state
                        
                        let dl = Fun.IsTiny(dpos.Length,1E-4)
                        let df = Fun.IsTiny(dWorldForward.Length,1E-4)
                        let du = Fun.IsTiny(dWorldUp.Length,1E-4)
                        
                        let shouldNotAnimate = dl && df && du

                        { CameraMotion.Zero with
                            dWorldPos = dpos
                            dWorldForward = dWorldForward
                            dWorldUp =      dWorldUp
                        }, shouldNotAnimate

                let model = 
                    match model.lastTime with
                        | Some last ->
                            let dt = now - last
                            let step = Integrator.rungeKutta (fun t s -> move s + look s + pan s + dolly s + zoom s + (jump s |> fst))

                            Integrator.integrate 0.0166666 step model dt

                        | None -> 
                            model
                     
                if model.rotateVec.AllTiny && model.moveVec.AllTiny && model.targetPhiTheta = V2d.Zero && (model.targetPan.Length <= 0.05) && (abs model.targetDolly <= 0.05) && (abs model.targetZoom <= 0.05) && (jump model |> snd) then
                    stopAnimation model
                else
                    let model = 
                        { model with lastTime = Some now; }
                    if model.animating then 
                        { model with lastTime = Some now; } |> dummyChange 
                    else model 
            
            | Rotate deg ->
                { model with view = model.view.WithUp(M44d.Rotation(model.view.Forward, deg).TransformDir model.view.Up) }

            | KeyDown Keys.W ->
                if not model.forward then
                    startAnimation { model with forward = true; moveVec = model.moveVec + V3d.OOI }
                else
                    model

            | KeyUp Keys.W ->
                if model.forward then
                    startAnimation { model with forward = false; moveVec = model.moveVec - V3d.OOI }
                else
                    model

            | KeyDown Keys.S ->
                if not model.backward then
                    startAnimation { model with backward = true; moveVec = model.moveVec - V3d.OOI }
                else
                    model

            | KeyUp Keys.S ->
                if model.backward then
                    startAnimation { model with backward = false; moveVec = model.moveVec + V3d.OOI }
                else
                    model

            | Wheel delta ->
                startAnimation 
                    { model with
                        targetZoom = model.targetZoom + (float delta.Y) * model.freeFlyConfig.zoomMouseWheelSensitivity
                    }

            | KeyDown Keys.A ->
                if not model.left then
                    startAnimation { model with left = true; moveVec = model.moveVec - V3d.IOO }
                else
                    model

            | KeyUp Keys.A ->
                if model.left then
                    startAnimation { model with left = false; moveVec = model.moveVec + V3d.IOO }
                else
                    model


            | KeyDown Keys.D ->
                if not model.right then
                    startAnimation { model with right = true; moveVec = model.moveVec + V3d.IOO }
                else
                    model

            | KeyUp Keys.D ->
                if model.right then
                    startAnimation { model with right = false; moveVec = model.moveVec - V3d.IOO }
                else
                    model

            | KeyDown _ | KeyUp _ ->
                model

            | Down(button,pos) ->
                let model = withTime { model with dragStart = pos }
                match button with
                    | MouseButtons.Left -> { model with look = true }
                    | MouseButtons.Middle -> { model with pan = true }
                    | MouseButtons.Right -> { model with dolly = true }
                    | _ -> model

            | Up button ->
                match button with
                    | MouseButtons.Left -> { model with look = false }
                    | MouseButtons.Middle -> { model with pan = false }
                    | MouseButtons.Right -> { model with dolly = false }
                    | _ -> model   
                    
            | JumpTo cv ->
                startAnimation { model with targetJump = Some cv }

            | Move pos  ->
                let delta = pos - model.dragStart

                let look model = 
                    if model.look then
                        let deltaAngle = V2d(float delta.X * model.freeFlyConfig.lookAtMouseSensitivity, float delta.Y * model.freeFlyConfig.lookAtMouseSensitivity)
                    
                        startAnimation 
                            { model with 
                                dragStart = pos
                                targetPhiTheta = model.targetPhiTheta + deltaAngle 
                            }
                    else model

                let pan model =
                    if model.pan then
                        startAnimation 
                            { model with
                                targetPan = model.targetPan + (V2d(float delta.X,float -delta.Y)) * model.freeFlyConfig.panMouseSensitivity
                                dragStart = pos
                            }
                    else 
                        model

                let dolly model =
                    if model.dolly then
                        startAnimation 
                            { model with
                                targetDolly = model.targetDolly + (float -delta.Y) * model.freeFlyConfig.dollyMouseSensitivity
                                dragStart = pos
                            }
                    else 
                        model
                    
                { model with dragStart = pos }
                    |> look
                    |> pan
                    |> dolly


    open Aardvark.Import.Browser

    let attributes (state : MCameraControllerState) (f : Message -> 'msg) = 
        attributes {
            yield always (blur (fun () -> f Blur))
            yield always <| pointerdown (fun pe -> 
                    let target = unbox<HTMLElement> pe.target
                    target.focus()
                    target.setPointerCapture(pe.pointerId)
                    Down(unbox pe.which, V2i(int pe.clientX, int pe.clientY)) |> f
                )
            yield onlyWhen (state.look %|| state.pan %|| state.dolly %|| state.zoom) <| pointerup (fun pe -> (unbox<HTMLElement> pe.target).releasePointerCapture(pe.pointerId); pe.preventDefault(); pe.stopPropagation(); Up(unbox pe.which) |> f)
            yield always (contextmenu (fun e -> e.preventDefault(); f Nop))
            yield always (keydown (fun e -> KeyDown (Keys.ofEvent e) |> f))
            yield always (keyup (fun e -> KeyUp (Keys.ofEvent e) |> f))      
            yield always <| wheel (fun e -> e.preventDefault(); f <| Wheel (V2d(0.0, e.wheelDeltaY / 80.0)))

            yield onlyWhen 
                    (state.look %|| state.pan %|| state.dolly %|| state.zoom) 
                    (pointermove (fun e -> if e.pointerType = ("mouse" :> obj) then f (Move (V2i(int e.clientX, int e.clientY))) else f Nop ))
            yield always <| rendered (fun _ -> f Rendered)
            //yield always <| onTouchStickMove "leftstick" (fun stick -> MoveMovStick stick |> f)
            //yield always <| onTouchStickMove "ritestick" (fun stick -> MoveRotStick stick |> f)
            //yield always <| onTouchStickStop "leftstick" (fun _ -> ReleaseMovStick |> f)
            //yield always <| onTouchStickStop "ritestick" (fun _ -> ReleaseRotStick |> f)
        }

            //| MoveMovStick s ->
            //    let s = scaleStick model.freeFlyConfig.touchScalesExponentially 2.5 s
            //    let pos = V2d(s.distance * cos(s.angle * Constant.RadiansPerDegree), s.distance * -sin(s.angle * Constant.RadiansPerDegree))
            //    startAnimation { model with moveVec = V3d(pos.X,0.0,pos.Y) }
            //| ReleaseMovStick ->
            //    startAnimation { model with moveVec = V3d(0.0,0.0,0.0) }   
                
            //| MoveRotStick s ->
            //    let s = scaleStick model.freeFlyConfig.touchScalesExponentially 0.75 s
            //    let pos = V2d(s.distance * cos(s.angle * Constant.RadiansPerDegree), s.distance * sin(s.angle * Constant.RadiansPerDegree))

            //    startAnimation { model with rotateVec = V3d(-pos.Y,-pos.X,0.0) * 0.01 }
            //| ReleaseRotStick ->
            //    startAnimation { model with rotateVec = V3d(0.0,0.0,0.0) }
