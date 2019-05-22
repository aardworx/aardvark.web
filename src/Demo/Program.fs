﻿module Program

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
                let n = Octnode(Unchecked.defaultof<_>, System.Guid.Empty, 0, V3d.Zero, o)
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

        let slotCache = Dict<System.Guid, Block<_> * Box3d>(Unchecked.hash, Unchecked.equals)

        let calls = Dict<nref<Map<int, WebGLBuffer>>, Dict<DrawCall, Box3d>>(Unchecked.hash, Unchecked.equals)

        let addCall (call : Block<_>) (box :  Box3d) =
            let set = calls.GetOrCreate(call.Memory, fun _ -> Dict(Unchecked.hash, Unchecked.equals))
            set.[{ first = call.Offset; faceVertexCount = call.Size; instanceCount = 1 }] <- box

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
        
        let inst = gl.getExtension("WEBGL_multi_draw_instanced") |> unbox<WEBGL_multi_draw_instanced>
        let renderTime = RunningMean(5)

        let mutable last = -1.0


        

        let run =
            let render = 
                if unbox inst then
                    fun (calls : Dict<DrawCall, Box3d>) ->
                        let mvp = Mod.force mvp
                        let mutable count = 0
                        let mutable offsets = Array.zeroCreate calls.Count
                        let mutable counts = Array.zeroCreate calls.Count
                        let mutable instanceCounts = Array.zeroCreate calls.Count

                        let mutable pointCount = 0.0
                        let mutable visiblePointCount = 0.0
                        let mutable nodeCount = 0.0
                        let mutable visibleNodeCount = 0.0
                        
                        for (call, bounds) in calls do
                            nodeCount <- nodeCount + 1.0
                            pointCount <- pointCount + float call.faceVertexCount
                            if bounds.IntersectsViewProj mvp then
                                if call.faceVertexCount > 0 then
                                    offsets.[count] <- call.first
                                    counts.[count] <- call.faceVertexCount
                                    instanceCounts.[count] <- 1
                                    count <- count + 1
                                    visibleNodeCount <- visibleNodeCount + 1.0
                                    visiblePointCount <- visiblePointCount + float call.faceVertexCount

                        emitStats { totalNodes = nodeCount; visibleNodes = visibleNodeCount; totalPoints = pointCount; visiblePoints = visiblePointCount}

                        if count > 0 then
                            inst.multiDrawArraysInstancedWEBGL(gl.POINTS, offsets, 0, counts, 0, instanceCounts, 0, count)
                else
                    fun (calls : Dict<DrawCall, Box3d>) -> 
                        
                        let mutable pointCount = 0.0
                        let mutable visiblePointCount = 0.0
                        let mutable nodeCount = 0.0
                        let mutable visibleNodeCount = 0.0
                        
                        let mvp = Mod.force mvp
                        for (call, bounds) in calls do
                            nodeCount <- nodeCount + 1.0
                            pointCount <- pointCount + float call.faceVertexCount
                            if bounds.IntersectsViewProj mvp then
                                gl.drawArrays(gl.POINTS, float call.first, float call.faceVertexCount)
                                visibleNodeCount <- visibleNodeCount + 1.0
                                visiblePointCount <- visiblePointCount + float call.faceVertexCount
                        
                        emitStats { totalNodes = nodeCount; visibleNodes = visibleNodeCount; totalPoints = pointCount; visiblePoints = visiblePointCount}

            fun () ->
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

                    render calls
                gl.flush()
                gl.finish()
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
                        | Some (slot, _) -> removeCall slot |> ignore
                        | None -> ()
                    | Free _ ->
                        match slotCache.TryRemove el with
                        | Some (slot, _)-> 
                            removeCall slot |> ignore
                            free slot
                        | None -> 
                            ()
                    | Activate ->
                        match slotCache.TryGetValue el with
                        | Some (slot, box) -> addCall slot box |> ignore
                        | None -> ()

                    | Alloc (v,a) ->
                        match slotCache.TryGetValue el with
                        | Some (slot, box) -> 
                            if a > 0 then addCall slot box |> ignore
                        | None ->
                            let bb = Octnode(Unchecked.defaultof<_>, System.Guid.Empty, 0, V3d.Zero, v).BoundingBox //v.[Durable.Octree.BoundingBoxExactGlobal] |> unbox<Box3d>
                            let bb = Box3d(bb.Min - rootCenter, bb.Max - rootCenter)
                            let slot = alloc v
                            slotCache.[el] <- (slot, bb)
                            if a > 0 then addCall slot bb |> ignore
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
    //open Aardvark.Import.JS

    //[<Emit("$0.charCodeAt($1)")>]
    //let charCodeAt (str : string) (i : float) : float = jsNative

    //type System.String with
    //    member inline x.charCodeAt(i : float) = charCodeAt x i
    

     
    //type Message<'a> = 
    //    | Progress of totalSize : float * read : float * time : float
    //    | Chunk of 'a
    //    | Done

    //let private lineBreak = [| '\n'; '\r'; char 0 |]

    //let readLines (chunkSize : int) (init : unit -> 's) (parse : 's -> string -> unit) (emit : Message<'s> -> Promise<'a>) (b : Blob) : Promise<'a> =
    //    let nextNewLine (str : string) (o : int) =
    //        let fst = str.IndexOfAny(lineBreak, o)
    //        let mutable n = fst
    //        if n >= 0 then
    //            let mutable next = n + 1
    //            while next < str.Length && (let c = str.charCodeAt(float next) in c = 10.0  || c = 13.0 || c = 0.0) do
    //                n <- n + 1
    //            fst, n
    //        else
    //            -1, -1

    //    let parseLines (str : string) =
    //        let state = init()
    //        let mutable cnt = 0
    //        let mutable o = 0
    //        let mutable ns, ne = nextNewLine str o
    //        while ns >= 0 do
    //            let line = str.Substring(o, ns - o)
    //            parse state line
    //            o <- ne + 1
    //            let (a,b) = nextNewLine str o
    //            ns <- a
    //            ne <- b
    //            cnt <- cnt + 1

    //        if cnt > 0 then 
    //            emit (Chunk state) |> Prom.map (fun _ -> o)
    //        else
    //            Prom.value o

    //    let startTime = performance.now()
    //    let rec readChunk (b : Blob) (start : float) (size : float) : Promise<'a> =
    //        let s = min (b.size - start) size
    //        if s <= 0.0 then
    //            emit Done
    //        else
    //            //Log.line "chunk %.0f %.0f" start (start + s)
    //            let chunk = b.slice(start, start + s)
    //            let r = FileReader.Create()

    //            let readCurrent =
    //                Promise.Create(fun success error ->
    //                    r.addEventListener_load(fun e ->
    //                        let str = Fable.Core.JsInterop.(?) e.target "result" |> unbox<string>
    //                        success str
    //                    )
    //                    r.readAsText chunk
    //                ) |> unbox<Promise<string>>

    //            promise {
    //                let! str = readCurrent
    //                let! off = parseLines(str)
    //                if off = 0 then
    //                    return! readChunk b start (2.0 * size)
    //                else
    //                    let oo = start + float off
    //                    //Log.warn "offset: %.0f" oo
    //                    let! _ = emit(Progress(b.size, oo, performance.now() - startTime))
    //                    return! readChunk b oo size
                        
    //            }

    //            //r.addEventListener_load(fun e ->
    //            //    let str = Fable.Core.JsInterop.(?) e.target "result" |> unbox<string>
    //            //    parseLines(str).``then``(fun off ->
    //            //        if off = 0 then
    //            //            readChunk b start (2.0 * size)
    //            //        else
    //            //            let oo = start + float off
    //            //            emit(Progress(b.size, oo, performance.now() - startTime)).``then`` (fun _ ->
    //            //                readChunk b oo size
    //            //            ) 
    //            //    )
    //            //)
    //            //r.readAsText chunk


    //    readChunk b 0.0 (float chunkSize)

    //module Pts =
    
    //    // -15283.527316 246958.380835 412.436068 -1964 110 114 100
    //    type Token =
    //        | Skip      = 0
    //        | PositionX = 1
    //        | PositionY = 2
    //        | PositionZ = 3
    //        | RedByte   = 4
    //        | GreenByte = 5
    //        | BlueByte  = 6

        
    //    type MyRef<'a> = { mutable Value : 'a }


    //    let inline private nextToken (str : string) (offset : MyRef<float>) =
    //        let mutable o = offset.Value
    //        while o < float str.Length && str.charCodeAt o = 32.0 do o <- o + 1.0

    //        let n = str.IndexOf(' ', unbox<int> o) |> float 
    //        if n >= 0.0 then
    //            //let mutable next = n + 1.0
    //            //while next < float str.Length && str.charCodeAt next = 32.0 do
    //            //    n <- n + 1.0
    //            //    next <- next + 1.0
    //            offset.Value <- o
    //            n - o
    //        elif o < float str.Length then
    //            offset.Value <- o
    //            float str.Length - o
    //        else
    //            -1.0
                
    //    //let charCodeAt (str : string) (i : int) : int = int str.[i]
        
    //    //type System.String with
    //    //    member inline x.charCodeAt(i : int) = charCodeAt x i
        
    //    let fastParseFloat (str : string) (o : float) (len : float) =
    //        let mutable sign = true
    //        let mutable v = 0.0
    //        let mutable i = 0.0
    //        let mutable si = o

    //        let mutable exp = 1.0
    //        let mutable iexp = 0.1

    //        while i < len do
    //            let c = charCodeAt str si
    //            if c = 45.0 then 
    //                sign <- not sign
    //            elif c = 46.0 then 
    //                v <- v * iexp
    //                exp <- 0.1
    //                iexp <- 0.0
    //            else 
    //                let o = c - 48.0
    //                v <- v + exp * o
    //                exp <- exp * 0.1
    //                if iexp <> 0.0 then iexp <- iexp * 10.0


    //            i <- i + 1.0
    //            si <- si + 1.0

    //        if sign then v
    //        else -v

    //    let inline private tryParseDouble (str : string) (o : float) (l : float) =
    //        let v =  parseFloat (str.Substring(unbox<int> o, unbox<int> l))
    //        if System.Double.IsNaN v then
    //            None
    //        else
    //            Some v

    //    let inline private tryParseByte (str : string) (o : float) (l : float) =
    //        let v = parseFloat (str.Substring(unbox<int> o, unbox<int> l)) 
    //        if System.Double.IsNaN v then
    //            None
    //        else
    //            Some (byte v)

    //    let parseLine (fmt : array<Token>) (ps : V3dList, cs : Uint8List, bb : Box3d) (line : string) =
    //        let mutable px = 0.0
    //        let mutable py = 0.0
    //        let mutable pz = 0.0
    //        let mutable r = 0uy
    //        let mutable g = 0uy
    //        let mutable b = 0uy

    //        let mutable error = false
    //        let mutable i = 0
    //        let o = { Value = 0.0 }
    //        let mutable len = nextToken line o
            
    //        while not error && i < fmt.Length && len >= 0.0 do
    //            let s = o.Value
    //            match fmt.[i] with

    //            | Token.PositionX ->
    //                match tryParseDouble line s len with
    //                | Some v -> px <- v
    //                | None -> error <- true

    //            | Token.PositionY ->
    //                match tryParseDouble line s len  with
    //                | Some v -> py <- v
    //                | None -> error <- true

    //            | Token.PositionZ ->
    //                match tryParseDouble line s len with
    //                | Some v -> pz <- v
    //                | None -> error <- true

    //            | Token.RedByte ->
    //                match tryParseByte line s len with
    //                | Some b -> r <- b
    //                | None -> error <- true
                    
    //            | Token.GreenByte ->
    //                match tryParseByte line s len with
    //                | Some b -> g <- b
    //                | None -> error <- true

    //            | Token.BlueByte ->
    //                match tryParseByte line s len with
    //                | Some v -> b <- v
    //                | None -> error <- true
    //            | _ ->
    //                ()

    //            o.Value <- o.Value + len
    //            len <- nextToken line o
    //            i <- i + 1


    //        if not error && i = fmt.Length then
    //            bb.ExtendBy(px, py, pz)
    //            ps.Add(px, py, pz)
    //            cs.Add r; cs.Add g; cs.Add b

    //open Microsoft.FSharp.Collections

    //let newGuid() = System.Guid.NewGuid()

    //let parseGuid (str : string) =
    //    let g = System.Guid str
    //    g


    //type LocalOctnode(db : LocalDatabase, id : System.Guid, splitLimit : int, m : Map<Durable.Def, obj>) =
    //    static member pickle (m : LocalOctnode) =
    //        let s = Stream()
    //        DurableDataCodec.encode s Durable.Octree.Node (m.Data :> obj)
    //        s.ToArrayBuffer()   

    //    static member unpickle (db : LocalDatabase) (splitLimit : int) (name : string) (m : ArrayBuffer) =
    //        let id = parseGuid name
    //        assert (string id = name)
    //        let data = DurableDataCodec.decode (Stream m) |> snd |> unbox<Map<Durable.Def, obj>>
    //        LocalOctnode(db, id, splitLimit, data)


    //    member x.Cell = m.[Durable.Octree.Cell] |> unbox<Cell>
    //    member x.Id = id
    //    member x.Database = db
    //    member x.Data = m

        
    //    member x.PositionsLocal = m.[Durable.Octree.PositionsLocal3f] |> unbox<V3fBuffer>
    //    member x.Colors = m.[Durable.Octree.Colors3b] |> unbox<C3bBuffer>

    //    member x.PointCountCell = 
    //        match Map.tryFind Durable.Octree.PointCountCell m with
    //        | Some c -> unbox<int> c
    //        | None -> 0

    //    member x.SubNodes =
    //        match Map.tryFind Durable.Octree.SubnodesGuids m with
    //        | Some ids ->
    //            ids |> unbox<System.Guid[]> |> FSharp.Collections.Array.map (fun g ->
    //                if g = System.Guid.Empty then
    //                    None
    //                else
    //                    let r = db.GetRef(string g, LocalOctnode.pickle, LocalOctnode.unpickle db splitLimit)
    //                    Some r
    //            ) |> Some
    //        | None ->
    //            None

    //    static member Build (db : LocalDatabase, cell : Cell, splitLimit : int, ps : IArrayBuffer<V3d>, cs : IArrayBuffer<uint8>) : Promise<Option<LocalOctnode>> =
    //        promise {
    //            if ps.Length > 0 then
    //                let id = System.Guid.NewGuid()

    //                if ps.Length > splitLimit then
    //                    let subPos = FSharp.Collections.Array.init 8 (fun _ -> V3dList())
    //                    let subCs = FSharp.Collections.Array.init 8 (fun _ -> Uint8List())
    //                    let center = cell.Center
    //                    //Log.warn "cell: %A: %d" cell ps.Length
    //                    for i in 0 .. ps.Length-1 do
    //                        let pt = ps.Get i
    //                        let index =
    //                            (if pt.X >= center.X then 4 else 0) |||
    //                            (if pt.Y >= center.Y then 2 else 0) |||
    //                            (if pt.Z >= center.Z then 1 else 0)

    //                        let r = cs.Get (3*i+0)
    //                        let g = cs.Get (3*i+1)
    //                        let b = cs.Get (3*i+2)

    //                        subPos.[index].Add(pt)
    //                        subCs.[index].Add(r)
    //                        subCs.[index].Add(g)
    //                        subCs.[index].Add(b)
    

                        

    //                    let subNodeIds = FSharp.Collections.Array.zeroCreate 8
    //                    for i in 0 .. subNodeIds.Length - 1 do
    //                        let c = cell.GetChild(i)
    //                        let! n = LocalOctnode.Build(db, c, splitLimit, subPos.[i], subCs.[i])
    //                        match n with
    //                        | Some n when n.Id <> System.Guid.Empty ->
    //                            let! r = db.GetRef(string n.Id, LocalOctnode.pickle, LocalOctnode.unpickle db splitLimit)
    //                            r.Write n
    //                            subNodeIds.[i] <- parseGuid r.Name
    //                        | _ -> 
    //                            subNodeIds.[i] <- System.Guid.Empty
    //                    let center = cell.Center
    //                    let data = 
    //                        Map.ofList [ 
    //                            Durable.Octree.Cell, cell :> obj
    //                            Durable.Octree.SubnodesGuids, subNodeIds :> obj
    //                        ]

    //                    return LocalOctnode(db, id, splitLimit, data) |> Some

    //                else
    //                    let center = cell.Center
    //                    let local = V3fBuffer.init ps.Length (fun i -> ps.Get i - center)
    //                    let cs = cs :> IArrayBuffer
    //                    let data = 
    //                        Map.ofList [ 
    //                            Durable.Octree.Cell, cell :> obj
    //                            Durable.Octree.PointCountCell, ps.Length :> obj
    //                            Durable.Octree.PositionsLocal3f, local :> obj
    //                            Durable.Octree.Colors3b, C3bBuffer(cs.Buffer, cs.ByteOffset, cs.Length / 3) :> obj
    //                        ]

    //                    return LocalOctnode(db, id, splitLimit, data) |> Some
    //            else
    //                return None
    //        }

    //    member x.AddContained(bb : Box3d, ps : IArrayBuffer<V3d>, cs : IArrayBuffer<uint8>) =
    //        promise {
    //            match x.SubNodes with
    //            | Some ns ->
    //                let nns = FSharp.Collections.Array.zeroCreate ns.Length
    //                for i in 0 .. ns.Length - 1 do
    //                    match ns.[i] with
    //                    | Some p ->
    //                        let! r = p
    //                        nns.[i] <- Some r
    //                    | None ->
    //                        nns.[i] <- None
    //                let ns = nns

    //                let subPos = FSharp.Collections.Array.init 8 (fun _ -> V3dList())
    //                let subCs = FSharp.Collections.Array.init 8 (fun _ -> Uint8List())
    //                let center = x.Cell.Center

    //                if (bb.Min.X > center.X || bb.Max.X < center.X) &&
    //                   (bb.Min.Y > center.Y || bb.Max.Y < center.Y) && 
    //                   (bb.Min.Z > center.Z || bb.Max.Z < center.Z) then
    //                    let pt = bb.Min
    //                    let index =
    //                        (if pt.X >= center.X then 4 else 0) |||
    //                        (if pt.Y >= center.Y then 2 else 0) |||
    //                        (if pt.Z >= center.Z then 1 else 0)
                            
    //                    let mutable changed = false
    //                    match ns.[index] with
    //                    | Some n ->
    //                        let! o = n.Read()
    //                        let! res = o.AddContained(bb, ps, cs)
    //                        if res <> o then n.Write res
    //                    | None ->
    //                        match! LocalOctnode.Build(db, x.Cell.GetChild index, splitLimit, ps, cs) with
    //                        | Some v when v.Id <> System.Guid.Empty ->
    //                            let! r = db.GetRef(string v.Id, LocalOctnode.pickle, LocalOctnode.unpickle db splitLimit)
    //                            r.Write v
    //                            ns.[index] <- Some r
    //                            changed <- true
    //                        | _ ->
    //                            ()


    //                    if changed then
    //                        let ids =
    //                            ns |> FSharp.Collections.Array.map (fun n ->
    //                                match n with
    //                                | Some n -> parseGuid n.Name
    //                                | None -> System.Guid.Empty
    //                            )


    //                        return LocalOctnode(db, id, splitLimit, Map.add Durable.Octree.SubnodesGuids (ids :> obj) m)
    //                    else
    //                        return x
    //                else
    //                    //Log.warn "cell: %A: %d" x.Cell ps.Length
    //                    for i in 0 .. ps.Length-1 do
    //                        let pt = ps.Get i
    //                        let index =
    //                            (if pt.X >= center.X then 4 else 0) |||
    //                            (if pt.Y >= center.Y then 2 else 0) |||
    //                            (if pt.Z >= center.Z then 1 else 0)

    //                        let r = cs.Get (3*i+0)
    //                        let g = cs.Get (3*i+1)
    //                        let b = cs.Get (3*i+2)

    //                        subPos.[index].Add(pt)
    //                        subCs.[index].Add(r)
    //                        subCs.[index].Add(g)
    //                        subCs.[index].Add(b)
    
    //                    let mutable changed = false
    //                    let ns = FSharp.Collections.Array.copy ns
    //                    for i in 0 .. ns.Length - 1 do
    //                        match ns.[i] with
    //                        | Some n ->
    //                            let! o = n.Read()
    //                            let! v = o.AddContained(Box3d subPos.[i], subPos.[i], subCs.[i])
    //                            if v <> o then n.Write(v)

    //                        | None ->
    //                            let v = LocalOctnode.Build (db, x.Cell.GetChild i, splitLimit, subPos.[i], subCs.[i])
    //                            match! v with
    //                            | Some v when v.Id <> System.Guid.Empty ->
    //                                let! r = db.GetRef(string v.Id, LocalOctnode.pickle, LocalOctnode.unpickle db splitLimit)
    //                                r.Write v
    //                                ns.[i] <- Some r
    //                                changed <- true
    //                            | _ ->
    //                                ()
    //                    if changed then
    //                        let ids =
    //                            ns |> FSharp.Collections.Array.map (fun n ->
    //                                match n with
    //                                | Some n -> parseGuid n.Name
    //                                | None -> System.Guid.Empty
    //                            )


    //                        return LocalOctnode(db, id, splitLimit, Map.add Durable.Octree.SubnodesGuids (ids :> obj) m)
    //                    else
    //                        return x
    //            | None ->
    //                let old = x.PointCountCell
    //                let cnt = old + ps.Length
    //                if cnt > splitLimit then
    //                    let subPos = FSharp.Collections.Array.init 8 (fun _ -> V3dList())
    //                    let subCs = FSharp.Collections.Array.init 8 (fun _ -> Uint8List())
    //                    let center = x.Cell.Center
    //                    //Log.warn "cell: %A: %d" x.Cell ps.Length
    //                    for i in 0 .. ps.Length-1 do
    //                        let pt = ps.Get i
    //                        let index =
    //                            (if pt.X >= center.X then 4 else 0) |||
    //                            (if pt.Y >= center.Y then 2 else 0) |||
    //                            (if pt.Z >= center.Z then 1 else 0)

    //                        let r = cs.Get (3*i+0)
    //                        let g = cs.Get (3*i+1)
    //                        let b = cs.Get (3*i+2)

    //                        subPos.[index].Add(pt)
    //                        subCs.[index].Add(r)
    //                        subCs.[index].Add(g)
    //                        subCs.[index].Add(b)


    //                    let subNodeIds =
    //                        FSharp.Collections.Array.init 8 (fun i ->
    //                            let c = x.Cell.GetChild(i)
    //                            let n = LocalOctnode.Build(db, c, splitLimit, subPos.[i], subCs.[i])
    //                            match n with
    //                            | Some n when n.Id <> System.Guid.Empty ->
    //                                let r = db.GetRef(string n.Id, LocalOctnode.pickle, LocalOctnode.unpickle db splitLimit)
    //                                r.Write n
    //                                parseGuid r.Name
    //                            | _ -> 
    //                                System.Guid.Empty
    //                        )
    //                    let center = x.Cell.Center
    //                    let data = 
    //                        Map.ofList [ 
    //                            Durable.Octree.Cell, x.Cell :> obj
    //                            Durable.Octree.SubnodesGuids, subNodeIds :> obj
    //                        ]

    //                    return LocalOctnode(db, id, splitLimit, data)
    //                else
    //                    let center = x.Cell.Center

    //                    let o = x.PositionsLocal
    //                    let local = V3fBuffer.init cnt (fun i -> if i < o.Length then o.[i] else ps.Get (i - o.Length) - center)

    //                    let oc = 
    //                        let b = (x.Colors) :> IArrayBuffer
    //                        Uint8Array.Create(b.Buffer, b.ByteOffset, b.Length * 3)
                            
    //                    let colors = C3bBuffer.zeroCreate cnt
    //                    let cs = Uint8Array.Create(cs.Buffer, cs.ByteOffset, cs.Length)
                        
    //                    Uint8Array.Create((colors :> IArrayBuffer).Buffer, 0, oc.length).set(unbox oc)
    //                    Uint8Array.Create((colors :> IArrayBuffer).Buffer, oc.length).set(unbox cs)
                    
    //                    let data = 
    //                        m 
    //                        |> Map.add Durable.Octree.PositionsLocal3f (local :> obj)
    //                        |> Map.add Durable.Octree.Colors3b (colors :> obj)
    //                        |> Map.add Durable.Octree.PointCountCell (cnt :> obj)

    //                    return LocalOctnode(db, id, splitLimit, data)
    //        }

    //    member x.Wrap(other : Cell) =
    //        let cell = x.Cell


    //        if cell = other then
    //            Prom.value x
    //        else
    //            promise {
                    
    //                let! targetCells =
    //                    if cell.IsCenteredAtOrigin then 
    //                        match x.SubNodes with
    //                        | Some ns -> ns |> FSharp.Collections.Array.choose (function Some a -> a.Read() |> Some | None -> None) |> Prom.all
    //                        | None -> Prom.value Seq.empty
    //                    else
    //                        Prom.value (Seq.singleton x)
                    
    //                let targetCells = Seq.toArray targetCells
    //                let overallBounds = targetCells |> FSharp.Collections.Array.fold (fun b c -> Box3d.Union(b, c.Cell.BoundingBox)) Box3d.Invalid
    //                let parentCell = Cell overallBounds
                    

    //                let rec build (depth : int) (c : Cell) (children : array<LocalOctnode>) =
    //                    assert (children |> Seq.forall (fun cc -> c.BoundingBox.Contains cc.Cell.BoundingBox))
    //                    if depth > 30 then 
    //                        Log.warn "hinig %d %A %A" depth c (children |> FSharp.Collections.Array.map (fun c -> string c.Cell))
    //                        None
    //                    else
    //                        let dst = targetCells |> FSharp.Collections.Array.tryFind (fun n -> n.Cell = c)
    //                        match dst with
    //                        | Some d -> 
    //                            Some d
    //                        | None -> 
    //                            if children.Length = 0 then
    //                                None
    //                            else
                                    
    //                                let i = 
    //                                    let cc = c.Center
    //                                    targetCells |> FSharp.Collections.Array.groupBy (fun n -> 
    //                                        let nc = n.Cell.Center
    //                                        (if nc.X > cc.X then 4 else 0) |||
    //                                        (if nc.Y > cc.Y then 2 else 0) |||
    //                                        (if nc.Z > cc.Z then 1 else 0)
    //                                    ) 

    //                                let ns : array<Option<DbRef<LocalOctnode>>> = FSharp.Collections.Array.zeroCreate 8
    //                                for (i, cs) in i do
    //                                    match build (depth + 1) (c.GetChild i) cs with
    //                                    | Some x when x.Id <> System.Guid.Empty ->
    //                                        let r = db.GetRef(string x.Id, LocalOctnode.pickle, LocalOctnode.unpickle db splitLimit)
    //                                        r.Write x
    //                                        ns.[i] <- Some r
    //                                    | _ ->
    //                                        ()
                        
                        
    //                                let ids =
    //                                    ns |> FSharp.Collections.Array.map (fun n ->
    //                                        match n with
    //                                        | Some n -> parseGuid n.Name
    //                                        | None -> System.Guid.Empty
    //                                    )
    //                                let data = 
    //                                    Map.ofList [ 
    //                                        Durable.Octree.Cell, x.Cell :> obj
    //                                        Durable.Octree.SubnodesGuids, ids :> obj
    //                                    ]

    //                                LocalOctnode(db, id, splitLimit, data) |> Some


    //                return (build 0 parentCell targetCells).Value
    //            }


    //type LocalOctree(db : LocalDatabase, splitLimit : int) =
    
    //    static let containingCell (ps : IArrayBuffer<V3d>) =
    //        let bb = Box3d ps
    //        let c = Cell bb
    //        let wasted = c.BoundingBox.Volume / bb.Volume
    //        console.warn(wasted)
    //        if c.BoundingBox.Contains bb.Min && c.BoundingBox.Contains bb.Max then
    //            c
    //        else    
    //            failwith "asdasdasdad"
    //    let mutable root : Option<LocalOctnode> = None

    //    member x.Root = root

    //    member x.Add(ps : IArrayBuffer<V3d>, cs : IArrayBuffer<uint8>) =
    //        if ps.Length > 0 then
    //            promise {
    //                match root with
    //                | Some r ->
    //                    let bb = Box3d ps
    //                    let other = Cell(bb)
    //                    let! r = r.Wrap(other)
    //                    let! n = r.AddContained(bb, ps, cs)
    //                    root <- Some n
    //                | None -> 
    //                    root <- LocalOctnode.Build(db, containingCell ps, splitLimit, ps, cs)
    //            }
    //        else
    //            Prom.value()

    //open Microsoft.FSharp.Collections

    //let testBlobStore(ps : IArrayBuffer<V3d>, cs : IArrayBuffer<uint8>) =
    //    promise {
    //        let! db = LocalDatabase.connect "yippie"
    //        let oct = LocalOctree(db, 8291)
    //        do! oct.Add(ps, cs)

    //        do! db.Delete()
    //    }
        
    //module InCore =
    //    type Octnode(db : LocalDatabase, id : System.Guid, cell : Cell, splitLimit : int, zip : bool) =

    //        static let ref (db : LocalDatabase) (splitLimit : int) (zip : bool) (n : Octnode) = db.Ref(string n.Id, n, Octnode.pickle, Octnode.unpickle db splitLimit zip)

    //        let mutable children : Option<DbRef<Octnode>>[] = [||]
    //        let mutable data : Map<Durable.Def, obj> = Map.empty

    //        let mutable bounds = Box3d.Invalid
    //        let mutable cellCount = 0
    //        let mutable totalCount = 0.0
            
    //        static let createList (def : Durable.Def) (cap : int) =
    //            if def = Durable.Octree.PositionsLocal3f then V3fList(cap) :> obj
    //            elif def = Durable.Octree.Colors3b then Uint8List(3 * cap) :> obj
    //            else failwithf "bad attribute: %A" def
                
    //        static let append (posOffset : V3d) (def : Durable.Def) (idx : IArrayBuffer<int>) (src : obj) (dst : obj) =
    //            if def = Durable.Octree.PositionsLocal3f then
    //                let src = unbox<IArrayBuffer<V3d>> src
    //                let dst = unbox<V3fList> dst
    //                for i in 0 .. idx.Length - 1 do
    //                    let s = src.Get (idx.Get i)
    //                    dst.Add (s.X + posOffset.X, s.Y + posOffset.Y, s.Z + posOffset.Z)
    //            elif def = Durable.Octree.Colors3b then
    //                let src = unbox<IArrayBuffer<uint8>> src
    //                let dst = unbox<Uint8List> dst
    //                for i in 0 .. idx.Length - 1 do
    //                    let i = 3 * idx.Get i
    //                    dst.Add (src.Get i)
    //                    dst.Add (src.Get (i + 1))
    //                    dst.Add (src.Get (i + 2))
    //            else
    //                failwithf "bad attribute: %A" def

    //        static let partition (cell : Cell) (bb : Box3d) (index : IArrayBuffer<int>) (ps : IArrayBuffer<V3d>) =
    //            let center = cell.Center
    //            if (bb.Min.X >= center.X || bb.Max.X <= center.X) &&
    //               (bb.Min.Y >= center.Y || bb.Max.Y <= center.Y) && 
    //               (bb.Min.Z >= center.Z || bb.Max.Z <= center.Z) then

    //                let o = cell.GetOctant bb.Center
    //                FSharp.Collections.Array.init 8 (fun i ->
    //                    if i = o then
    //                        let index = 
    //                            match index with
    //                            | :? Int32List as o -> o
    //                            | _ ->
    //                                let l = Int32List()
    //                                l.AddRange index
    //                                l
    //                        bb, index
    //                    else
    //                        Box3d.Invalid, Int32List()
    //                )

    //            else

    //                let ri = FSharp.Collections.Array.init 8 (fun _ -> Box3d.Invalid, Int32List(index.Length))
    //                let mutable ci = 0
    //                for i in 0 .. index.Length - 1 do   
    //                    let index = index.Get i
    //                    let p = ps.Get index
    //                    let o = cell.GetOctant p
    //                    let (bb, r) = ri.[o]
    //                    bb.ExtendBy p
    //                    r.Add index
    //                    ci <- ci + 1
    //                ri

    //        static member pickle (n : Octnode) =
    //            let children : Option<DbRef<Octnode>>[] = n.Children
    //            let subnodes = FSharp.Collections.Array.zeroCreate children.Length
    //            for i in 0 .. children.Length - 1 do
    //                let c = children.[i]
    //                match c with
    //                | Some c ->     
    //                    subnodes.[i] <- System.Guid c.Name
    //                | None ->
    //                    subnodes.[i] <- System.Guid.Empty

    //            let cell : Cell = n.Cell
    //            let data : Map<Durable.Def, obj> = n.Data
    //            let bounds = n.BoundingBox

    //            let localps = 
    //                if n.CellCount > 0 then
    //                    let ps = data.[Durable.Octree.PositionsLocal3f] |> unbox<IArrayBuffer<V3d>>
    //                    V3fBuffer(ps.Buffer, ps.ByteOffset, ps.Length)
    //                else
    //                    V3fBuffer(0)
                        
                        
    //            let localcs = 
    //                if n.CellCount > 0 then
    //                     let cs = data.[Durable.Octree.Colors3b] |> unbox<IArrayBuffer<uint8>>
    //                     C3bBuffer(cs.Buffer, cs.ByteOffset, cs.Length / 3)
    //                else
    //                     C3bBuffer(0)

    //            let data =
    //                Map.ofArray [|
    //                    yield Durable.Octree.Cell, cell :> obj
    //                    yield Durable.Octree.PointCountCell, n.CellCount |> int :> obj
    //                    yield Durable.Octree.TotalPointCount, n.TotalPointCount :> obj
    //                    yield Durable.Octree.BoundingBoxExactGlobal, bounds :> obj
    //                    yield Durable.Octree.Colors3b, localcs :> obj
    //                    yield Durable.Octree.PositionsLocal3f, localps :> obj
    //                    if subnodes.Length > 0 then
    //                        yield Durable.Octree.SubnodesGuids, subnodes :> obj
    //                |]

    //            let stream = Stream()
    //            DurableDataCodec.encode stream Durable.Octree.Node (data :> obj)
    //            let res = stream.ToArrayBuffer()
    //            let res =   
    //                if n.Zip then pako.deflate(Uint8Array.Create res).buffer
    //                else res
    //            res

    //        static member unpickle (db : LocalDatabase) (splitLimit : int) (zip : bool) (name : string) (data : ArrayBuffer) : Promise<Octnode> =
    //            let data = 
    //                if zip then pako.inflate(Uint8Array.Create data).buffer
    //                else data
    //            let stream = Stream(data)
    //            let _def, data = DurableDataCodec.decode stream
    //            let data = unbox<Map<Durable.Def, obj>> data
    //            let id = System.Guid name
    //            let n = Octnode(db, id, unbox data.[Durable.Octree.Cell], splitLimit, zip)
    //            n.SetData(data) |> Prom.map (fun () -> n)

    //        member x.Zip : bool = zip

    //        member private x.SetData(d : Map<Durable.Def, obj>) : Promise<unit> =
    //            promise {
    //                bounds <- unbox d.[Durable.Octree.BoundingBoxExactGlobal]
    //                cellCount <- unbox d.[Durable.Octree.PointCountCell]
    //                totalCount <- unbox d.[Durable.Octree.TotalPointCount]

    //                match Map.tryFind Durable.Octree.SubnodesGuids d with
    //                | Some ids ->
    //                    let ids = unbox<System.Guid[]> ids
    //                    children <- FSharp.Collections.Array.zeroCreate ids.Length
    //                    for i in 0 .. ids.Length - 1 do
    //                        let id = ids.[i]
    //                        if id = System.Guid.Empty then 
    //                            children.[i] <- None
    //                        else
    //                            let! r = db.GetRef(string id, Octnode.pickle, Octnode.unpickle db splitLimit zip)
    //                            children.[i] <- Some r

    //                | None ->
    //                    children <- [||]
    //                    ()

    //                let atts = 
    //                    d                    
    //                    |> Map.remove Durable.Octree.BoundingBoxExactGlobal
    //                    |> Map.remove Durable.Octree.PointCountCell
    //                    |> Map.remove Durable.Octree.MinTreeDepth
    //                    |> Map.remove Durable.Octree.SubnodesGuids


    //                for (def, att) in Map.toSeq atts do
    //                    if def = Durable.Octree.PositionsLocal3f then
    //                        let att = unbox<V3fBuffer> att
    //                        let dst = V3fList(Fun.NextPowerOfTwo att.Length)
    //                        dst.AddRange att
    //                        data <- Map.add Durable.Octree.PositionsLocal3f (dst :> obj) data
    //                    elif def = Durable.Octree.Colors3b then
    //                        let att = unbox<C3bBuffer> att
    //                        let src = Uint8Buffer((att :> IArrayBuffer).Buffer, (att :> IArrayBuffer).ByteOffset, att.Length * 3)
    //                        let dst = Uint8List(Fun.NextPowerOfTwo (att.Length * 3))
    //                        dst.AddRange src
    //                        data <- Map.add Durable.Octree.Colors3b (dst :> obj) data
    //            }

    //        member x.Id : System.Guid = id

    //        static member Build(db : LocalDatabase, cell : Cell, splitLimit : int, zip : bool, bb : Box3d, index : IArrayBuffer<int>, pb : IArrayBuffer<V3d>, atts : Map<Durable.Def, obj>) : Option<Promise<DbRef<Octnode>>> =
    //            if index.Length <= 0 then
    //                None
    //            else
    //                let id = System.Guid.NewGuid()
    //                let c = Octnode(db, id, cell, splitLimit, zip)
    //                c.AddContained(bb, index, pb, atts) |> Prom.bind (fun v ->
    //                    (ref db splitLimit zip c)
    //                ) |> Some
                    
    //        member x.CellCount = cellCount
    //        member x.Data = data
    //        member x.TotalPointCount 
    //            with get() = totalCount
    //            and set v = totalCount <- v
    //        member x.Cell = cell
    //        member x.Children
    //            with get() = children
    //            and set c = children <- c

    //        member x.BoundingBox 
    //            with get() = bounds
    //            and set b = bounds <- b

    //        member x.ForceSplit() : Promise<unit> =
    //            promise {
    //                if cellCount > 0 then
    //                    let center = cell.Center
    //                    let localps = data.[Durable.Octree.PositionsLocal3f] |> unbox<V3fList>
    //                    let ps = V3dBuffer.init localps.Count (fun i -> localps.[i] + center)

    //                    let is = Int32Buffer.init ps.Length (fun i -> i)
    //                    let atts = data |> Map.add Durable.Octree.PositionsLocal3f (ps :> obj) // that's a lie
    //                    cellCount <- 0
    //                    data <- Map.empty


    //                    let part = partition cell bounds is ps
    //                    let cc = FSharp.Collections.Array.zeroCreate part.Length
    //                    for i in 0 .. part.Length - 1 do
    //                        let (bb, idx) = part.[i]
    //                        match Octnode.Build(db, cell.GetChild i, splitLimit, zip, bb, idx, ps, atts) with
    //                        | Some p ->
    //                            let! r = p 
    //                            cc.[i] <- Some r
    //                        | None ->
    //                            cc.[i] <- None


    //                    children <- cc
    //                else
    //                    children <- FSharp.Collections.Array.zeroCreate 8
    //            }

    //        member x.AddContained(bb : Box3d, idx : IArrayBuffer<int>, pb : IArrayBuffer<V3d>, atts : Map<Durable.Def, obj>) : Promise<unit> =
    //            promise {
    //                if idx.Length > 0 then
    //                    if children.Length = 0 then
    //                        let newCnt = cellCount + idx.Length
    //                        if newCnt > splitLimit then
    //                            do! x.ForceSplit()
    //                            do! x.AddContained(bb, idx, pb, atts)

    //                        else
    //                            totalCount <- totalCount + float idx.Length
    //                            bounds.ExtendBy bb.Min
    //                            bounds.ExtendBy bb.Max
    //                            cellCount <- newCnt

    //                            let offset = -cell.Center
    //                            for (def, att) in Map.toSeq atts do
    //                                match Map.tryFind def data with
    //                                | Some dst ->   
    //                                    append offset def idx att dst
    //                                | None ->
    //                                    let l = createList def splitLimit
    //                                    append offset def idx att l
    //                                    data <- Map.add def l data
                                    
    //                    else
    //                        totalCount <- totalCount + float idx.Length
    //                        bounds.ExtendBy bb.Min
    //                        bounds.ExtendBy bb.Max

    //                        let sub = partition cell bb idx pb
    //                        for ci in 0 .. sub.Length - 1 do
    //                            let (bb, idx) = sub.[ci]
    //                            match children.[ci] with
    //                            | None -> 
    //                                match Octnode.Build(db, cell.GetChild ci, splitLimit, zip, bb, idx, pb, atts) with
    //                                | Some r ->
    //                                    let! r = r
    //                                    children.[ci] <- Some r
    //                                | None ->
    //                                    children.[ci] <- None
    //                            | Some c ->
    //                                let! v = c.Read()
    //                                do! v.AddContained(bb, idx, pb, atts)
    //                                c.Write v
    //            }
            
    //        member x.BuildLod(r : DbRef<Octnode>) : Promise<unit>=    
    //            promise {
    //                let children = x.Children
    //                if children.Length > 0 then
    //                    let children = children |> FSharp.Collections.Array.choose (fun i -> i)
    //                    let values = FSharp.Collections.Array.zeroCreate children.Length
    //                    for i in 0 .. children.Length - 1 do 
    //                        let c = children.[i]
    //                        let! r = c.Read()
    //                        values.[i] <- r
    //                        do! r.BuildLod(c)

    //                    let mutable count = 0
    //                    for c in values do
    //                        let cnt = float splitLimit * (c.TotalPointCount / totalCount) |> int
    //                        let cc = c.CellCount

    //                        let index = 
    //                            if cnt >= cc then
    //                                Int32Buffer.init cc (fun i -> i) :> IArrayBuffer<int>
    //                            else
    //                                let indices = Int32List(cnt + 1)
    //                                let step = float cc / float cnt
    //                                let mutable s = 0.0
    //                                let mutable c = 0
    //                                while c < cnt do
    //                                    indices.Add (int s)
    //                                    s <- s + step
    //                                    c <- c + 1
    //                                indices :> IArrayBuffer<int>

    //                        let offset = c.Cell.Center - cell.Center
    //                        for (def, att) in Map.toSeq c.Data do
    //                            match Map.tryFind def data with
    //                            | Some dst ->   
    //                                append offset def index att dst
    //                            | None ->
    //                                let l = createList def splitLimit
    //                                append offset def index att l
    //                                data <- Map.add def l data

    //                        count <- count + index.Length

    //                    Log.line "%A: %d" cell count
    //                    cellCount <- count
    //                    r.Write x
    //            }
            
    //        static member BuildWithSubtrees(db : LocalDatabase, splitLimit : int, zip : bool, cell : Cell, ts : array<Cell * DbRef<Octnode>>) : Option<Promise<DbRef<Octnode>>> =
    //            if ts.Length = 0 then
    //                None
    //            elif ts.Length = 1 && fst ts.[0] = cell then
    //                Some (Prom.value (snd ts.[0]))
    //            else
    //                promise {
    //                    let id = System.Guid.NewGuid()
    //                    let n = Octnode(db, id, cell, splitLimit, zip)
    //                    let groups = ts |> FSharp.Collections.Array.groupBy (fun (c, _) -> cell.GetOctant c.Center)
    //                    let children = FSharp.Collections.Array.zeroCreate 8
    //                    let mutable total = 0.0
    //                    let mutable bounds = Box3d.Invalid
    //                    for (i, g) in groups do
    //                        match Octnode.BuildWithSubtrees(db, splitLimit, zip, cell.GetChild i, g) with
    //                        | Some node ->
    //                            let! node = node
    //                            let! n = node.Read()
    //                            total <- total + n.TotalPointCount
    //                            bounds.ExtendBy(n.BoundingBox.Min)
    //                            bounds.ExtendBy(n.BoundingBox.Max)
    //                            children.[i] <- Some node
    //                        | None ->
    //                            children.[i] <- None

    //                    n.TotalPointCount <- total
    //                    n.BoundingBox <- bounds
    //                    n.Children <- children
    //                    return! ref db splitLimit zip n
    //                } |> Some

    //    type Octree(db : LocalDatabase, splitLimit : int, zip : bool) =
    //        let mutable root : Option<Octnode> = None
            
    //        member x.Root = root

    //        member x.Add(bb : Box3d, ps : IArrayBuffer<V3d>, cs : IArrayBuffer<uint8>) =
    //            promise {
    //                if ps.Length > 0 then
    //                    let idx = Int32Buffer.init ps.Length id

    //                    let atts = 
    //                        Map.ofList [
    //                            Durable.Octree.PositionsLocal3f, ps :> obj // that's a lie
    //                            Durable.Octree.Colors3b, cs :> obj
    //                        ]

    //                    match root with
    //                    | Some r -> 
    //                        if r.Cell.BoundingBox.Contains bb then
    //                            do! r.AddContained(bb, idx, ps, atts)
    //                        else    
    //                            let mutable subNodes = [||]
    //                            let c = r.Cell
    //                            if c.IsCenteredAtOrigin then
    //                                if r.Children.Length = 0 then 
    //                                    do! r.ForceSplit()
    //                                subNodes <- r.Children |> FSharp.Collections.Array.mapi (fun o v -> match v with | Some v -> Some (c.GetChild o, v) | _ -> None) |>  FSharp.Collections.Array.choose id
    //                            else
    //                                let! r = db.Ref(string r.Id, r, Octnode.pickle, Octnode.unpickle db splitLimit zip)
    //                                subNodes <- [| c, r |]

    //                            let bounds = subNodes |> FSharp.Collections.Array.fold (fun b (c,_) -> Box3d.Union(b, c.BoundingBox)) bb
    //                            let cell = Cell bounds
    //                            let newRoot = Octnode.BuildWithSubtrees(db, splitLimit, zip, cell, subNodes)
    //                            let mutable res : Option<Octnode> = None
    //                            match newRoot with
    //                            | Some r -> 
    //                                let! r = r
    //                                let! o = r.Read()
    //                                do! o.AddContained(bb, idx, ps, atts)
    //                                r.Write o
    //                                res <- Some o
    //                            | None -> 
    //                                match Octnode.Build(db, cell, splitLimit, zip, bb, idx, ps, atts) with
    //                                | Some r ->
    //                                    let! r = r
    //                                    let! r = r.Read()
    //                                    res <- Some r
    //                                | None ->
    //                                    res <- None

    //                            root <- res

    //                    | None ->
    //                        let c = Cell bb
    //                        match Octnode.Build(db, c, splitLimit, zip, bb, idx, ps, atts) with
    //                        | Some r ->
    //                            let! r = r
    //                            let! r = r.Read()
    //                            root <- Some r
    //                        | None ->
    //                            root <- None
    //            }
            
    //        member x.BuildLod() : Promise<unit> =
    //            match root with
    //            | Some r -> 
    //                promise {
    //                    let! rootRef = db.Ref(string id, r, Octnode.pickle, Octnode.unpickle db splitLimit zip)
    //                    do! r.BuildLod(rootRef)
    //                }
    //            | None -> 
    //                Prom.value ()

    //        member x.Persist() =
    //            promise {
    //                match root with
    //                | Some r ->
    //                    let id = r.Id
    //                    let bb = r.BoundingBox
    //                    let rootInfo = 
    //                        Fable.Core.JsInterop.createObj [
    //                            "RootId", string id :> obj
    //                            "PointCount", r.TotalPointCount :> obj
    //                            "Bounds", Fable.Core.JsInterop.createObj [
    //                                "Min", Fable.Core.JsInterop.createObj [
    //                                    "X", bb.Min.X :> obj
    //                                    "Y", bb.Min.Y :> obj
    //                                    "Z", bb.Min.Z :> obj
    //                                ]
    //                                "Max", Fable.Core.JsInterop.createObj [
    //                                    "X", bb.Max.X :> obj
    //                                    "Y", bb.Max.Y :> obj
    //                                    "Z", bb.Max.Z :> obj
    //                                ]
    //                            ]
    //                            "Cell", Fable.Core.JsInterop.createObj [
    //                                "X", float r.Cell.X :> obj
    //                                "Y", float r.Cell.Y :> obj
    //                                "Z", float r.Cell.Z :> obj
    //                                "E", r.Cell.E :> obj
    //                            ]

    //                            "GZipped", zip :> obj
    //                        ]
    //                    let str = JSON.stringify rootInfo

    //                    let! rootRef = db.Ref(string id, r, Octnode.pickle, Octnode.unpickle db splitLimit zip)
    //                    rootRef.Write r
    //                    do! db.Set("root.json", str)
    //                | None ->
    //                    ()
    //            }








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
