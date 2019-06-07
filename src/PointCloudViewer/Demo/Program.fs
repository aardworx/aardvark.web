module Program

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Rendering.WebGL
open Aardvark.Import.Browser
open Aardvark.Data
open Microsoft.FSharp.Collections


type RenderCommand =
    | Render of aset<RenderObject>
    | Clear of colors : amap<string, V4d> * depth : IMod<Option<float>> * stencil : IMod<Option<int>>
    | Unordered of aset<RenderCommand>


module FShadeTest =
    open FShade

    
    type SimpleVertex = 
        { 
            [<Position>] pos : V4d
            [<Color>] c : V4d
            [<Semantic("WorldPos")>] wp : V4d
            [<Normal>] n : V3d 
            [<TexCoord>] tc : V2d 
        }

    type Vertex = 
        { 
            [<Position>] pos : V4d
            [<Color>] c : V4d
            [<Semantic("WorldPos")>] wp : V4d
            [<Normal>] n : V3d 
            [<TexCoord>] tc : V2d 
            [<PointSize>] s : float
            [<PointCoord>] pc : V2d
            [<FragCoord>] fc : V4d
            depthRange : float
        }
        
    type MyRecord = { ambient : float; diffuse : float }
    type MyUnion =
        | A of MyRecord
        | B of float


    type UniformScope with
        member x.Ambient : MyUnion = uniform?Ambient
        member x.ShowColor : bool = uniform?ShowColor

    let sammy =
        sampler2d {
            texture uniform?DiffuseColorTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
    let diffuseTexture (v : Vertex) =
        fragment {
            return sammy.Sample(v.tc)
        }
        
    let constantColor (c : V4d) (v : Vertex) =
        vertex {
            return { v with c = c }
        }

    let trafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            return { 
                v with 
                    pos = uniform.ViewProjTrafo * wp
                    wp = wp
                    n = Vec.normalize (uniform.NormalMatrix * v.n)
                    s = 1.0
            }
        }

    let depthVertex (v : Vertex) =
        vertex {
            
            let wp = uniform.ModelTrafo * v.pos
            let vp = uniform.ModelViewTrafo * v.pos
            let pos = uniform.ProjTrafo * vp

            let pixelSize = 8.0 * (float uniform.ViewportSize.X / 1300.0)
            let ndcRadius = pixelSize / V2d uniform.ViewportSize

            let pp = pos.XYZ / pos.W
            let ppx = uniform.ProjTrafoInv * V4d(pp + V3d(ndcRadius.X, 0.0, 0.0), 1.0)
            let ppy = uniform.ProjTrafoInv * V4d(pp + V3d(0.0, ndcRadius.Y, 0.0), 1.0)
            let vpx = ppx / ppx.W
            let vpy = ppy / ppy.W
            let vr = 0.5 * (Vec.length (vpx - vp) + Vec.length (vpy - vp))

            let ppz = uniform.ProjTrafo * (vp - V4d(0.0, 0.0, vr, 0.0))
            let ppz = ppz.XYZ / ppz.W

            let depthRange = abs (ppz.Z - pp.Z)

            return {
                v with
                    pos = pos
                    wp = wp
                    depthRange = depthRange
                    n = Vec.normalize (uniform.NormalMatrix * v.n)
                    s = pixelSize
            }
        }

    type Fragment =
        {
            [<Color>] c : V4d
            [<Depth>] d : float
        }



    let circularPoint (v : Vertex) =
        fragment {  
            let c = 2.0 * v.pc - V2d.II
            let f = Vec.dot c c - 1.0
            if f > 0.0 then discard()

            let z = sqrt (-f)
            let n = V3d(c, z)

            let newDepth = v.fc.Z - v.depthRange * z


            let c =
                if uniform.ShowColor then v.c.XYZ
                else V3d.III

            let c = V4d((0.5 + 0.5 * z) * c, 1.0)
            return { c = c; d = newDepth }

            //let sn = 0.5 * (V3d(c, sqrt (1.0 - l2)) + V3d.III)
            //return V4d(sn, 1.0)

            //let n = Vec.normalize v.n
            //let c = uniform.CameraLocation - v.wp.XYZ |> Vec.normalize
            //let d = abs (Vec.dot n c)

            //return V4d((0.2 + 0.8 * d) * V3d.III, 1.0)
        }

    [<GLSLIntrinsic("mix({0}, {1}, {2})")>]
    let lerp (a : 'a) (b : 'a) (t : float) = onlyInShaderCode "mix"

    let simpleLight (v : Vertex) =
        fragment {
            //match uniform.Ambient with 
            //| A a -> 
            let d = 
                let dir = Vec.normalize (uniform.CameraLocation - v.wp.XYZ)
                let n = Vec.normalize v.n
                Vec.dot n dir
            let ambient = 0.3 //lerp 0.0 1.0 a.ambient
            return V4d((ambient + (1.0 - ambient) * d) * v.c.XYZ, v.c.W)
            //| B f ->
            //    return f * v.c
        }

    let bla (v : Vertex) =
        fragment {  
            let c : V4d = uniform?Color
            return v.c * (1.0  - c.W) + c * c.W
        }

module Normal16 =
    let private sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let private clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )

    let decode (v : uint16) : V3d =
        let e = V2d(float (v >>> 8) / 255.0, float (v &&& 0xFFus) / 255.0) * 2.0 - V2d.II
        let v = V3d(e, 1.0 - abs e.X - abs e.Y)
        if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
        else v |> Vec.normalize

    let encode (v : V3d) : uint16 =
        let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
        let p = 
            if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
            else clamp p
        
        let x0 = floor ((p.X * 0.5 + 0.5) * 255.0) |> int
        let y0 = floor ((p.Y * 0.5 + 0.5) * 255.0) |> int

        let mutable bestDot = 0.0
        let mutable best = 0us

        for dx in 0 .. 1 do
            for dy in 0 .. 1 do
                let e = uint16 (((x0 + dx) <<< 8) ||| (y0 + dy))
                let vv = decode e
                let d = Vec.dot vv v
                if d > bestDot then
                    bestDot <- d
                    best <- e

        best

module Normal32 =
    let private sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let private clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )

    let decode (v : uint32) : V3d =
        let e = V2d(float (v >>> 16) / 65535.0, float (v &&& 0xFFFFu) / 65535.0) * 2.0 - V2d.II
        let v = V3d(e, 1.0 - abs e.X - abs e.Y)
        if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
        else v |> Vec.normalize

    let encode (v : V3d) : uint32 =
        let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
        let p = 
            if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
            else clamp p
        
        let x0 = floor ((p.X * 0.5 + 0.5) * 65535.0) |> int
        let y0 = floor ((p.Y * 0.5 + 0.5) * 65535.0) |> int

        let mutable bestDot = 0.0
        let mutable best = 0u

        for dx in 0 .. 1 do
            for dy in 0 .. 1 do
                let e = uint32 (((x0 + dx) <<< 16) ||| (y0 + dy))
                let vv = decode e
                let d = Vec.dot vv v
                if d > bestDot then
                    bestDot <- d
                    best <- e

        best


module Time =
    let timed (f : unit -> int) =
        #if FABLE_QUOTATIONS
        let start = performance.now()
        let iter = f()
        let dt = performance.now() - start
        console.log(sprintf "took %.3fµs" (1000.0 * dt / float iter))
        #else
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let iter = f()
        sw.Stop()
        printfn "took %.3fµs" (1000.0 * sw.Elapsed.TotalMilliseconds / float iter)
        #endif


module Lod =
    open Aardvark.Base.Management

    type Stats =
        {
            totalNodes      : float
            visibleNodes    : float
            totalPoints     : float
            visiblePoints   : float

        }

        static member Zero = { totalNodes = 0.0; visibleNodes = 0.0; totalPoints = 0.0; visiblePoints = 0.0 }

        static member (+) (l : Stats, r : Stats) =
            {
                totalNodes = l.totalNodes + r.totalNodes
                visibleNodes = l.visibleNodes + r.visibleNodes
                totalPoints = l.totalPoints + r.totalPoints
                visiblePoints = l.visiblePoints + r.visiblePoints
            }

    type RunningMean(capacity : int) =
        let mutable sum = 0.0
        let mutable values = Array.zeroCreate<float> capacity
        let mutable index = 0
        let mutable count = 0

        member x.Add(value : float) =
            if count < capacity then
                sum <- sum + value
                values.[index] <- value
                index <- (index + 1) % capacity
                count <- count + 1
            else
                let rem = values.[index]
                values.[index] <- value
                sum <- sum + (value - rem)
                index <- (index + 1) % capacity
                count <- count + 1

        member x.Count =
            count

        member x.Average =
            if count = 0 then 1.0
            else sum / float count

    type TreeReader2(urls : aset<string>, emitStats : Stats -> unit, control : Aardvark.Application.RenderControl, state : TraversalState, time : IMod<float>) as this =
        inherit AbstractReader<hdeltaset<IRenderObject>>(HDeltaSet.monoid)

        let manager = control.Manager
        let urlReader = urls.GetReader()

        let pipeline = 
            let template = 
                Sg.draw PrimitiveTopology.PointList
                |> Sg.vertexAttribute DefaultSemantic.Positions (V3fBuffer.zeroCreate 1)
                |> Sg.vertexAttribute DefaultSemantic.Colors (C3bBuffer.zeroCreate 1)
            let obj = template.RenderObjects state |> ASet.toList |> List.head |> unbox<RenderObject>
            manager.PreparePipeline(control.FramebufferSignature, obj.pipeline)

        let mutable alive = true
        let mutable queue = AtomicQueue.empty
        let mutable initial = true

        let w = Worker.Create "worker.js"

        let rec sendCam() =
            if alive then 
                let view = Mod.force state.viewTrafo
                let proj = Mod.force state.projTrafo
                w.postMessage (Command.UpdateCamera(view, proj))
                Aardvark.Import.JS.setTimeout sendCam 50 |> ignore

        

        let gl = pipeline.program.Context.GL

        let attTypes =
            pipeline.program.Interface.attributes |> Map.map (fun slot att ->
                if att.name = DefaultSemantic.Positions then Types.Vec(Types.Float 32, 3)
                elif att.name = DefaultSemantic.Colors then Types.Vec(Types.Int(false, 8), 3)
                else failwith ""
            )

        let atts =
            attTypes |> Map.map (fun _ t -> VertexAttrib.ofType gl t)

        let attSizes =
            attTypes |> Map.map (fun _ t -> Types.PrimitiveType.size t)

        let mem = 
            let b = 
                if gl.IsGL2 then Memory.webgl2 gl.STATIC_DRAW gl
                else Memory.webgl gl.STATIC_DRAW gl
            let mem = 
                {
                    malloc = fun s -> attSizes |> Map.map (fun _ e -> b.malloc (e * s))
                    mfree = fun p s -> p |> Map.iter (fun i p -> let e = attSizes.[i] in b.mfree p (s*e) )
                    mrealloc = fun p o n -> p |> Map.map (fun i p -> let e = attSizes.[i] in b.mrealloc p (o*e) (n*e) )
                    mcopy = fun _ _ _ _ _ -> failwith ""
                }
            new Aardvark.Base.Management.ChunkedMemoryManager<Map<int, WebGLBuffer>>(mem, 4 <<< 20)

        let read =
            let entries =
                pipeline.program.Interface.attributes |> Map.map (fun id att ->
                    if att.name = DefaultSemantic.Positions then fun (n : Octnode) -> HostBuffer n.PositionsLocal
                    elif att.name = DefaultSemantic.Colors then fun (n : Octnode) -> HostBuffer n.Colors
                    else failwith ""
                )
            fun o -> 
                let n = Octnode(Unchecked.defaultof<_>, System.Guid.Empty, None, 0, V3d.Zero, o)
                let e = entries |> Map.map (fun _ f -> f n)
                n.PointCountCell, e

        let copyTarget =
            if gl.IsGL2 then gl.COPY_WRITE_BUFFER
            else gl.ARRAY_BUFFER

        let alloc (o : Map<Durable.Def, obj>) =
            let gl = manager.Context.GL
            let cnt, buffers = read o
            let slot = mem.Alloc cnt
            slot.Memory.Value |> Map.iter (fun id b ->
                let e = attSizes.[id]
                let data = buffers.[id]
                gl.bindBuffer(copyTarget, b)
                gl.bufferSubData(copyTarget, float (e * slot.Offset), Fable.Core.U2.Case1 data.Data.View)
            )
            gl.bindBuffer(copyTarget, null)
            slot

        let free (slot : Block<_>) =
            mem.Free slot

        let slotCache = Dict<System.Guid, Block<_> * V3d * Octnode>(Unchecked.hash, Unchecked.equals)

        let calls = Dict<nref<Map<int, WebGLBuffer>>, Dict<DrawCall, V3d * Octnode>>(Unchecked.hash, Unchecked.equals)

        let addCall (call : Block<_>) (rootCenter : V3d) (node :  Octnode) =
            let set = calls.GetOrCreate(call.Memory, fun _ -> Dict(Unchecked.hash, Unchecked.equals))
            set.[{ first = call.Offset; faceVertexCount = call.Size; instanceCount = 1 }] <- (rootCenter, node)

        let removeCall(call : Block<_>) =
            match calls.TryGetValue call.Memory with
            | Some set ->
                if set.Remove { first = call.Offset; faceVertexCount = call.Size; instanceCount = 1 } then
                    if set.Count = 0 then calls.Remove call.Memory |> ignore
                    true
                else
                    false
            | None ->
                false

        let mvp = TraversalState.modelViewProjTrafo state
        let mv = TraversalState.modelViewTrafo state
        
        let inst = gl.getExtension("WEBGL_multi_draw_instanced") |> unbox<WEBGL_multi_draw_instanced>
        let renderTime = RunningMean(5)

        let mutable last = -1.0


        

        let run =
            let maxQuality = 4.0
            let render = 
                if unbox inst then
                    fun (calls : Dict<DrawCall, V3d * Octnode>) ->
                        let mvp = Mod.force mvp
                        let mutable count = 0
                        let mutable offsets = Array.zeroCreate calls.Count
                        let mutable counts = Array.zeroCreate calls.Count
                        let mutable instanceCounts = Array.zeroCreate calls.Count

                        let mutable pointCount = 0.0
                        let mutable visiblePointCount = 0.0
                        let mutable nodeCount = 0.0
                        let mutable visibleNodeCount = 0.0
                        
                        for (call, (rootCenter, node)) in calls do
                            let bb = node.BoundingBox
                            let bb = Box3d(bb.Min - rootCenter, bb.Max - rootCenter)
                            
                            nodeCount <- nodeCount + 1.0
                            pointCount <- pointCount + float node.PointCountCell
                            if bb.IntersectsViewProj mvp then
                                let mv = Mod.force mv
                                let q = node.Quality(rootCenter, mv)
                                let mutable fvc = call.faceVertexCount
                                if q < 0.0 then
                                    let pp = node.PercentageOfParent
                                    let k = log pp / maxQuality
                                    let rel = exp (k * q)
                                    let part = int (float node.PointCountCell * rel)
                                    fvc <- max 0 part

                                if fvc > 0 then
                                    offsets.[count] <- call.first
                                    counts.[count] <- fvc
                                    instanceCounts.[count] <- 1
                                    count <- count + 1
                                    visibleNodeCount <- visibleNodeCount + 1.0
                                    visiblePointCount <- visiblePointCount + float fvc

                        if count > 0 then
                            inst.multiDrawArraysInstancedWEBGL(gl.POINTS, offsets, 0, counts, 0, instanceCounts, 0, count)

                        { totalNodes = nodeCount; visibleNodes = visibleNodeCount; totalPoints = pointCount; visiblePoints = visiblePointCount}
                else
                    fun (calls : Dict<DrawCall, V3d * Octnode>) -> 
                        
                        let mutable pointCount = 0.0
                        let mutable visiblePointCount = 0.0
                        let mutable nodeCount = 0.0
                        let mutable visibleNodeCount = 0.0
                        
                        let mvp = Mod.force mvp
                        for (call, (rootCenter, node)) in calls do
                            let bb = node.BoundingBox
                            let bb = Box3d(bb.Min - rootCenter, bb.Max - rootCenter)
                            
                            nodeCount <- nodeCount + 1.0
                            pointCount <- pointCount + float call.faceVertexCount
                            if bb.IntersectsViewProj mvp then
                                let mv = Mod.force mv
                                let q = node.Quality(rootCenter, mv)
                                let mutable fvc = call.faceVertexCount
                                if q < 0.0 then
                                    let pp = node.PercentageOfParent
                                    
                                    let k = log pp / maxQuality
                                    let rel = exp (k * q)
                                    let part = int (float node.PointCountCell * rel)
                                    fvc <- max 0 part

                                if fvc > 0 then
                                    gl.drawArrays(gl.POINTS, float call.first, float fvc)
                                    visibleNodeCount <- visibleNodeCount + 1.0
                                    visiblePointCount <- visiblePointCount + float fvc
                        
                        { totalNodes = nodeCount; visibleNodes = visibleNodeCount; totalPoints = pointCount; visiblePoints = visiblePointCount}

            fun () ->
                let mutable stats = Stats.Zero
                for mem, calls in calls do
                    mem.Value |> Map.iter (fun id b ->
                        let atts = atts.[id]
                        gl.bindBuffer(gl.ARRAY_BUFFER, b)
                        let mutable mid = id
                        for att in atts do
                            gl.enableVertexAttribArray(float mid)
                            gl.vertexAttribPointer(float mid, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                            mid <- mid + 1
                        gl.bindBuffer(gl.ARRAY_BUFFER, null)
                    )

                    let s = render calls
                    stats <- stats + s

                gl.flush()
                gl.finish()
                emitStats stats
                let now = performance.now()
                if last >= 0.0 then 
                    let dt = now - last
                    renderTime.Add dt
                last <- now

        let command =
            { new PreparedCommand(manager) with
                member x.Compile(t) =
                    [|
                        match t with
                        | Some prev -> yield! Compiler.updatePipelineState prev.ExitState pipeline
                        | None ->  yield! Compiler.setPipelineState pipeline
                        yield run
                    |]
                    
                member x.ExitState = pipeline
                member x.Acquire() = PreparedPipelineState.acquire pipeline
                member x.Release() = PreparedPipelineState.release pipeline
                member x.Update t = PreparedPipelineState.update t pipeline
                member x.Resources = PreparedPipelineState.resources pipeline
            }

        let mutable rootCenter = V3d.Zero

        do sendCam()
        do w.onmessage <- fun e ->
            let msg = unbox<Reply> e.data
            match msg with 
            | Reply.SetRootCenter(_, c) ->
                rootCenter <- c
                ()
            | Reply.Perform (ops) ->
                let test = 
                    ops 
                    |> Seq.map (fun (k,vs) -> k, Operation.map (fun kvs -> DurableDataCodec.decodeMap (Stream(kvs)) |> snd) vs)
                    |> AtomicOperation.ofSeq

                queue <- AtomicQueue.enqueue test queue
                transact (fun () -> this.MarkOutdated())
            | _ ->
                ()

        override x.Kind = "SetReader"

        override x.Compute(token : AdaptiveToken) =
            let start = performance.now()
            let elapsed() = performance.now() - start

            for op in urlReader.GetOperations token do
                match op with
                | Add(_,url) -> w.postMessage (Command.Add(url))
                | Rem(_,url) -> w.postMessage (Command.Remove(url))
                ()


            while elapsed() < 0.5 * renderTime.Average && not (AtomicQueue.isEmpty queue) do
                let op, rest = AtomicQueue.dequeue queue
                queue <- rest
                
                op.ops |> Seq.iter (fun (el, op) ->
                    match op with
                    | Nop ->    
                        ()
                    | Deactivate ->
                        match slotCache.TryGetValue el with
                        | Some (slot, _, _) -> removeCall slot |> ignore
                        | None -> ()
                    | Free _ ->
                        match slotCache.TryRemove el with
                        | Some (slot, _, _)-> 
                            removeCall slot |> ignore
                            free slot
                        | None -> 
                            ()
                    | Activate ->
                        match slotCache.TryGetValue el with
                        | Some (slot, rootCenter, box) -> addCall slot rootCenter box |> ignore
                        | None -> ()

                    | Alloc (v,a) ->
                        match slotCache.TryGetValue el with
                        | Some (slot, rootCenter, box) -> 
                            if a > 0 then addCall slot rootCenter box |> ignore
                        | None ->
                            let bb = Octnode(Unchecked.defaultof<_>, System.Guid.Empty, None, 0, V3d.Zero, v) //v.[Durable.Octree.BoundingBoxExactGlobal] |> unbox<Box3d>
                            //let bb = Box3d(bb.Min - rootCenter, bb.Max - rootCenter)
                            let slot = alloc v
                            slotCache.[el] <- (slot, rootCenter, bb)
                            if a > 0 then addCall slot rootCenter bb |> ignore
                )

                manager.Context.GL.flush()
                manager.Context.GL.finish()



            if not (AtomicQueue.isEmpty queue) then
                let _ = time.GetValue token
                ()
        
            if initial then
                initial <- false
                command :> IRenderObject |> Add |> HDeltaSet.single
            else
                HDeltaSet.empty

        override x.Release() =
            alive <- false
            w.terminate()
            //commandCache |> Seq.iter (fun (cmd,_) -> cmd.Release())
            //commandCache.Clear()
            //cache.Clear()
            //delayed <- HDeltaSet.empty
            //state <- HRefSet.empty
            initial <- true
            queue <- AtomicQueue.empty
            PreparedPipelineState.release pipeline
            ()

    type TreeSg(ctrl : Aardvark.Application.RenderControl, urls : aset<string>, ?emitStats : (Stats -> unit)) =
        interface ISg with
            member x.RenderObjects(state) =
                ASet.create (fun () -> new TreeReader2(urls, defaultArg emitStats ignore, ctrl, state, ctrl.Time))

    let sg<'a> ctrl (urls : aset<string>) : ISg =
        TreeSg(ctrl, urls) :> ISg


open Aardvark.Base.Management
open Fable.Core

module Octbuild =

    let test (update : string -> unit) =

        let load (file : File) =
            
            let progress = document.createElement_div()
            document.body.appendChild(progress) |> ignore
            progress.style.position <- "fixed"
            progress.style.right <- "10pt"
            progress.style.bottom <- "10pt"
            progress.style.fontFamily <- "Consolas"
            progress.style.zIndex <- "10000"
            progress.style.background <- "white"
            progress.style.color <- "black"
            progress.style.webkitUserSelect <- "none"
            progress.style.cursor <- "no-drop"
            let setMessage fmt = Printf.kprintf (fun str -> progress.innerHTML <- str) fmt
        
            let data = 
                PointCloudImporter.Import.Pts(file)
        
            let config =
                { 
                    PointCloudImporter.overwrite = false
                    PointCloudImporter.compress = false
                    PointCloudImporter.splitLimit = 32768
                    PointCloudImporter.store = file.name
                    PointCloudImporter.progress = fun totalSize size time ->
                        let time = MicroTime.FromMilliseconds time
                        let r = min 1.0 (size / totalSize)
                        let eta = (time / r) - time
                        setMessage "%s: %A: %.2f%% (eta: %A)" file.name time (100.0 * r) eta
        
                }
        
                    
        
            let run =
                promise {
                    try
                        let import = PointCloudImporter.import config data
                        progress.addEventListener_click (fun _ -> import.cancel())
                        let! pointCount = import
                        
                        progress.style.cursor <- ""
                        update config.store
                        setMessage "imported <a href=\"./?local=%s\">%s</a> with %.0f points" config.store config.store pointCount
        
                        window.history.replaceState("", "", sprintf "./?local=%s" config.store)

                        Aardvark.Import.JS.setTimeout (fun () -> progress.remove()) 1000 |> ignore
                    with e ->
                        setMessage "%A" e
                        progress.style.cursor <- ""
                        Aardvark.Import.JS.setTimeout (fun () -> progress.remove()) 2000 |> ignore
                }
        
            ()

        let dropzone = document.createElement_div()
        dropzone.style.position <- "fixed"
        dropzone.style.height <- "100vh"
        dropzone.style.width <- "100vw"
        dropzone.style.background <- "rgb(80,80,80)"
        dropzone.style.pointerEvents <- "none"
        dropzone.style.opacity <- "0"
        dropzone.style.display <- "flex"
        dropzone.style.alignItems <- "center"
        dropzone.style.justifyContent <- "center"
        dropzone.style.fontSize <- "6em"
        dropzone.style.color <- "white"
        dropzone.style.fontFamily <- "monospace"
        document.body.appendChild(dropzone) |> ignore

        document.addEventListener_dragleave(fun e ->
            dropzone.style.opacity <- "0"
            e.preventDefault()
        )

        document.addEventListener_dragover(fun e ->
            dropzone.style.opacity <- "0.5"
            e.preventDefault()
        )

        //document.addEventListener_dragend(fun e ->
        //    dropzone.style.opacity <- "0"
        //    e.preventDefault()
            
        //)

        document.addEventListener_drop(fun e ->
            if e.dataTransfer.files.length > 0 then
                let f = e.dataTransfer.files.[0]
                if f.name.EndsWith ".pts" then
                    load e.dataTransfer.files.[0]
                    dropzone.style.opacity <- "0"
                else
                    let o = dropzone.style.background
                    dropzone.style.background <- "#800000"
                    dropzone.style.opacity <- "0.8"
                    dropzone.innerText <- sprintf "could not load %s" f.name
                    let restore () =
                        dropzone.style.background <- o
                        dropzone.innerText <- ""
                        dropzone.style.opacity <- "0"
                    Aardvark.Import.JS.setTimeout restore 2000 |> ignore

            e.preventDefault()
        )


type IDBInfo =
    abstract member name : string
    abstract member version : float

type IDBFactory with
    [<Emit("$0.databases")>]
    member x.databases : unit -> Aardvark.Import.JS.Promise<IDBInfo[]> = jsNative

module UITest =
    open System
    open Aardvark.UI
    open Aardvark.UI.Generic
    open Example

    let fix (create : Lazy<'a> -> 'a) =
        let ref = ref Unchecked.defaultof<'a>
        ref := create (lazy (!ref ))

    type Message = 
        | Stop
        | Reset 
        | Append of string
        | Prepend of string
        | Toggle

    let initial = 
        { 
            elements = PList.ofList [ "YEAH" ]
            box = true 
        }

    let update (shutdown : Lazy<IDisposable>) (m : TestModel) (msg : Message) =
        match msg with
        | Stop ->
            shutdown.Value.Dispose()
            initial
        | Toggle -> 
            { m with box = not m.box }
        | Reset ->
            initial
        | Append element -> 
            { m with elements = PList.append element m.elements }
        | Prepend element -> 
            { m with elements = PList.prepend element m.elements }

    let view (m : MTestModel) =
        div [style "color: white"] [
            button [clazz "ui basic yellow button"; click (fun _ -> Toggle) ] (m.box |> Mod.map (function true -> "sphere" | false -> "box"))
            button [clazz "ui basic green button"; click (fun _ -> performance.now() |> MicroTime.FromMilliseconds |> string |> Prepend) ] "prepend"
            button [clazz "ui basic green button"; click (fun _ -> performance.now() |> MicroTime.FromMilliseconds |> string |> Append) ] "append"
            button [clazz "ui basic red button"; click (fun _ -> Reset) ] "reset"
            
            button [clazz "ui basic red button"; click (fun _ -> Stop) ] "stop"

            div [] (m.elements |> AList.map (fun v -> div [clazz "ui basic inverted label"] v))

            div [] (m.box |> Mod.map (fun box ->
                [
                    let withCam (sg : ISg) (control : Aardvark.Application.RenderControl) =
                        let initial = CameraView.lookAt (V3d(1.0, 6.0, 4.0)) V3d.Zero V3d.OOI
                        let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial

                        let view = cam |> Mod.map (fun v -> v |> CameraView.viewTrafo)
                        let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 70.0 1.0 100000.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                        sg
                        |> Sg.viewTrafo view
                        |> Sg.projTrafo proj

                    if box then 
                        yield Aardvark.UI.DomNode.Render(
                            AttributeMap.ofList [style "width: 100%; height: 100%; tab-index: 0"; clazz "hugo"], 
                            Sg.box Box3d.Unit
                                |> Sg.shader {
                                    do! FShadeTest.trafo
                                    do! FShadeTest.constantColor V4d.IIII
                                    do! FShadeTest.simpleLight
                                }
                                |> withCam
                        )
                    else
                        yield Aardvark.UI.DomNode.Render(
                            AttributeMap.ofList [style "width: 100%; height: 100%; tab-index: 0"], 
                            Sg.sphere 3
                                |> Sg.shader {
                                    do! FShadeTest.trafo
                                    do! FShadeTest.constantColor V4d.IIII
                                    do! FShadeTest.simpleLight
                                }
                                |> withCam
                        )
                        
                ]
            ))

        ]

    let rec run() =
        fix (fun shutdown ->
            App.run document.body { 
                initial = initial
                update = update shutdown
                view = view
                unpersist = Unpersist.instance
            }
        ) 
       

[<EntryPoint>]
let main argv =
    let query = 
        window.location.search.Split([| '&'; '?' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun str -> str.Split([| '=' |]))
        |> Array.choose (fun kvp -> if kvp.Length = 2 then Some (kvp.[0], kvp.[1]) else None)
        |> Map.ofArray

    let url =
        match Map.tryFind "blob" query with
        | Some id -> "./" + id + "/{0}"
        | None -> 
            match Map.tryFind "local" query with
            | Some l -> "local://" + l
            | None -> "./navvis/{0}"

    let largestr (unit : string) (v : float) =
        let a = abs v
        if a = 0.0 then "0" + unit
        elif a >= 1000000000.0 then sprintf "%.3fG%s" (v / 1000000000.0) unit
        elif a >= 1000000.0 then sprintf "%.2fM%s" (a / 1000000.0) unit
        elif a >= 1000.0 then sprintf "%.1fk%s" (a / 1000.0) unit
        elif a >= 1.0 then sprintf "%.0f%s" a unit
        elif a >= 0.001 then sprintf "%.0fm%s" (a * 1000.0) unit
        elif a >= 0.000001 then sprintf "%.0fu%s" (a * 1000000.0) unit
        elif a >= 0.000000001 then sprintf "%.0fn%s" (a * 1000000000.0) unit
        else string v

   
    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then

            UITest.run()
            if false then
                let select = document.getElementById "clouds" |> unbox<HTMLSelectElement>
                if unbox indexedDB.databases then
                    indexedDB.databases().``then``(fun dbs -> 
                        select.innerHTML <- ""
                        dbs |> Array.iter (fun db ->
                            let name = db.name
                            if unbox name then
                                let e = document.createElement_option()
                                e.value <- "local://" + name
                                e.innerText <- name
                                select.appendChild e |> ignore
                        )
                        let e = document.createElement_option()
                        e.value <- "navvis"
                        e.innerText <- "navvis"
                        select.appendChild e |> ignore

                        match Map.tryFind "local" query with
                        | Some q -> 
                            select.value <- "local://" + q
                        | None ->
                            select.value <- "navvis"

                    ) |> ignore
                else
                    let select = document.getElementById "clouds"
                    select.parentElement.remove()

                let canvas = document.getElementById "target" |> unbox<HTMLCanvasElement>
                canvas.tabIndex <- 1.0
            

                canvas.addEventListener_click(fun _ ->
                    canvas.focus()
                )

                let control = new Aardvark.Application.RenderControl(canvas, false, true, ClearColor = V4d.OOOO)
                let initial = CameraView.lookAt (V3d(6.0, 6.0, 4.0)) V3d.Zero V3d.OOI
                let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial
                let color = Mod.init true

                let view = cam |> Mod.map (fun v -> v |> CameraView.viewTrafo)
                let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 70.0 1.0 100000.0 (float s.X / float s.Y) |> Frustum.projTrafo)


                control.Keyboard.DownWithRepeats.Add (fun k ->
                    match k with
                    | Aardvark.Application.Keys.V -> transact (fun () -> color.Value <- not color.Value)
                    | _ -> ()
                )

                let url = Mod.init url


                let set (u : string) =
                    if u.StartsWith "local://" then
                        let name = u.Substring 8
                        transact (fun () -> url.Value <- u)
                        window.history.replaceState("", "", sprintf "./?local=%s" name)
                    else   
                        transact (fun () -> url.Value <- "./" + u + "/{0}")
                        window.history.replaceState("", "", sprintf "./?blob=%s" u)
                    
                select.addEventListener_change(fun e ->
                    set select.value
                )


                let l = mlist<int> PList.empty

                let test = l |> AList.map (fun v -> 2*v) |> AList.sort
                let r = test.GetReader()

                Log.line "%A" l.Value
                r.GetOperations(AdaptiveToken.Top) |> Log.line "%A"
                r.State |> Log.line "%A"

                transact (fun () -> l.Update (PList.ofList [1;3]))
            
                Log.line "%A" l.Value
                r.GetOperations(AdaptiveToken.Top) |> Log.line "%A"
                r.State |> Log.line "%A"
            
                transact (fun () -> l.Update (PList.append 15 l.Value))
            
                Log.line "%A" l.Value
                r.GetOperations(AdaptiveToken.Top) |> Log.line "%A"
                r.State |> Log.line "%A"
            
                transact (fun () -> l.Update (PList.insertAt 1 7 l.Value))
            
                Log.line "%A" l.Value
                r.GetOperations(AdaptiveToken.Top) |> Log.line "%A"
                r.State |> Log.line "%A"

                console.warn (PList.ofList [1;2;3])


                Octbuild.test(fun store ->
                    let u = "local://" + store
                    transact (fun () -> url.Value <- u)
                    let e = document.createElement_option()
                    e.value <- u
                    e.innerText <- store
                    select.appendChild e |> ignore
                    select.value <- store
                
                    set u

                )

                //for b in 0 .. 8 .. 1000 do
                //    let mutable t = Index.zero
                //    for i in 1 .. b do
                //        t <- Index.after t

                //    let mutable bla = t

                //    let rep = 20
                //    let mutable sum = 0.0
                //    for i in -1 .. rep do
                //        let t0 = performance.now()
                //        let iter = 20000
                //        for i in 1 .. iter do
                //            bla <- Index.after t
                //        if i > 0 then
                //            let dt = (performance.now() - t0) / float iter
                //            sum <- sum + dt
                //    Log.line "%d: %.5fus" b (1000.0 * sum / float rep)

                //console.warn "done"
                let set = ASet.ofModSingle url

                let emitStats (s : Lod.Stats) =
                    let cnt = document.getElementById "pointCount"
                    cnt.innerText <- sprintf "%s (%s)" (largestr "" s.totalPoints) (largestr "" s.visiblePoints)
                    let cnt = document.getElementById "nodeCount"
                    cnt.innerText <- sprintf "%s (%s)" (largestr "" s.totalNodes) (largestr "" s.visibleNodes)

                let sg =
                    Lod.TreeSg(control, set, emitStats) :> ISg
                    |> Sg.shader {
                        do! FShadeTest.depthVertex
                        do! FShadeTest.circularPoint
                    }
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj
                    |> Sg.uniform "ViewportSize" control.Size
                    |> Sg.uniform "ShowColor" color
                let objects = sg.RenderObjects()
                let task() = new RenderTask(control.FramebufferSignature, control.Manager, objects) :> IRenderTask

                control.RenderTask <- task()
    )
    0
