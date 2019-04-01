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
open Fable.Core.JsInterop


[<AllowNullLiteral>]
type CodeFragment() =
    let store = ArrayBuffer.Create(100.0)
    let mutable prev : CodeFragment = null
    let mutable next : CodeFragment = null

    member x.AddCall(code : int, [<ParamArray>] args : obj[]) =
        ()

    member x.Prev
        with get() = prev
        and set p = prev <- p
        
    member x.Next
        with get() = next
        and set p = next <- p

        
[<AllowNullLiteral>]
type ILinked =
    abstract member Next : ILinked with get, set
    abstract member Prev : ILinked with get, set
    abstract member Update : AdaptiveToken -> unit

[<AllowNullLiteral>]
type TrieNode<'k, 'a>(dirty : DictSet<ILinked>, nodeKey : Option<'k>, parent : TrieNode<'k, 'a>, create : 'a -> ILinked) =
    let mutable prev : TrieNode<'k, 'a> = null
    let mutable next : TrieNode<'k, 'a> = null

    let mutable firstChild : TrieNode<'k, 'a>  = null
    let mutable lastChild : TrieNode<'k, 'a>  = null
    let children = Dict<'k, TrieNode<'k, 'a>>(Unchecked.hash, Unchecked.equals)
    
    let mutable firstValue : ILinked = null
    let mutable lastValue : ILinked = null

    member x.LastValue = lastValue

    member x.Prev
        with get() = prev
        and set p = prev <- p
        
    member x.Next
        with get() = next
        and set p = next <- p
        
    member x.FirstChild
        with get() = firstChild
        and set p = firstChild <- p
            
    member x.LastChild
        with get() = lastChild
        and set p = lastChild <- p

    member x.Parent = parent

    member x.Last =
        if unbox lastChild then
            if unbox lastChild then lastChild.Last
            elif unbox lastValue then lastValue
            elif unbox prev then prev.Last
            else null
        else
            if unbox lastValue then lastValue
            elif unbox prev then prev.Last
            else null

    member x.First =
        if unbox firstValue then firstValue
        elif unbox firstChild then firstChild.First
        elif unbox next then next.First
        elif unbox parent then parent.LastValue
        else null

    member x.Add(keys : list<'k>, value : 'a) =
        match keys with
        | [] -> 
            let prev =
                if unbox lastValue then lastValue
                elif unbox prev then prev.Last
                elif unbox parent then parent.Last
                else null

            let next =
                if children.Count > 0 then firstChild.First
                elif unbox next then next.First
                else null


            let l = create(value)
            dirty.Add l |> ignore
            if unbox next then dirty.Add next |> ignore
            l.Prev <- prev
            l.Next <- next
            
            if not (unbox lastValue) then firstValue <- l
            lastValue <- l

            if unbox prev then prev.Next <- l
            if unbox next then next.Prev <- l

            l
        | k :: ks ->
            match children.TryGetValue k with
            | Some c -> c.Add(ks, value)
            | None -> 
                let c = TrieNode<'k, 'a>(dirty, Some k, x, create, Prev = lastChild, Next = null)
                let res = c.Add(ks, value)
                children.[k] <- c

                if unbox lastChild then lastChild.Next <- c
                else firstChild <- c
                lastChild <- c

                res

    member x.Remove(keys : list<'k>) =
        match keys with
        | [] ->
            let f = x.First
            let l = x.Last
            if unbox f then 
                if unbox l.Next then dirty.Add l.Next |> ignore

                let mutable c = f
                while c <> l.Next do
                    dirty.Remove(c) |> ignore
                    c <- c.Next

                if unbox f.Prev then f.Prev.Next <- l.Next
                if unbox l.Next then l.Next.Prev <- f.Prev

            if unbox prev then prev.Next <- next
            else parent.FirstChild <- next

            if unbox next then next.Prev <- prev
            else parent.LastChild <- prev

            true

        | k :: ks ->
            match children.TryGetValue k with
            | Some c ->
                if c.Remove(ks) then
                    children.Remove k |> ignore
                    children.Count = 0
                else
                    false
            | None ->
                false


[<AllowNullLiteral>]
type Fragment(value : int) =
    let mutable code = "none"
    let mutable prev : Fragment = null
    let mutable next : Fragment = null
    
    member x.Value = value
    member x.Code = code

    override x.ToString() =
        x.Code

    member x.Prev
        with get() = prev
        and set (p : Fragment) = prev <- p
            
    member x.Next
        with get() = next
        and set p = next <- p

    interface ILinked with
        member x.Update(t) =
            if unbox x.Prev then code <- sprintf "step(%d,%d)" x.Prev.Value value
            else code <- sprintf "init %d" value
        

        member x.Prev
            with get() = x.Prev :> ILinked
            and set p = x.Prev <- unbox p

        member x.Next
            with get() = x.Next :> ILinked
            and set p = x.Next <- unbox p



type Trie<'k, 'a>(create : 'a -> ILinked) =
    let dirty = DictSet<ILinked>(Unchecked.hash, Unchecked.equals)
    let mutable root = TrieNode<'k, 'a>(dirty, None, null, create)

    member x.First = root.First
    member x.Last = root.Last


    member x.Add(keys : list<'k>, value : 'a) =
        root.Add(keys, value)

    member x.Remove(keys : list<'k>) =
        if root.Remove keys then
            root <- TrieNode<'k, 'a>(dirty, None, null, create)

    member x.Update(token : AdaptiveToken) =
        for d in dirty do 
            if unbox d then
                d.Update(token)
        dirty.Clear()


module Compiler =
    let compile (o : PreparedRenderObject) =
        let gl = o.program.Context.GL

        [
            //Log.startCollapsed "init(%d)" o.id
            //Log.line "set program"
            yield fun () -> gl.useProgram(o.program.Handle)
            
            //Log.line "set depthMode"
            match !o.depthMode.Handle with
            | Some m ->
                yield fun () -> 
                    gl.enable(gl.DEPTH_TEST)
                    gl.depthFunc(m)
            | None ->
                yield fun () -> 
                    gl.disable(gl.DEPTH_TEST)


            // bind uniforms
            for (id, b) in Map.toSeq o.uniformBuffers do
                //Log.line "set UB %d" id
                yield fun () -> 
                    let b = !b.Handle
                    gl.bindBufferBase(gl.UNIFORM_BUFFER, float id, b.Handle)

            // bind buffers
            for (id, (b, atts)) in Map.toSeq o.vertexBuffers do
                //Log.line "set VB %d" id
                yield fun () -> 
                    let b = !b.Handle
                    gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
                    let mutable mid = id
                    for att in atts do
                        let id = mid
                        gl.enableVertexAttribArray(float id)
                        gl.vertexAttribPointer(float id, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                        mid <- mid + 1
                    gl.bindBuffer(gl.ARRAY_BUFFER, null)

            match o.indexBuffer with
            | Some (ib, info) ->
                yield fun () ->
                    let call = !o.call.Handle
                    let ib = !ib.Handle
                    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
                    gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))
            | None ->
                yield fun () ->
                    let call = !o.call.Handle
                    gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)

            //Log.stop()
        ]

    let compileDiff (prev : PreparedRenderObject) (o : PreparedRenderObject) =
        let gl = o.program.Context.GL

        [
            //Log.startCollapsed "compile(%d, %d)" prev.id o.id
            if prev.program.Handle <> o.program.Handle then
                //Log.line "set program"
                yield fun () -> gl.useProgram(o.program.Handle)

            if prev.depthMode.Handle <> o.depthMode.Handle then
                //Log.line "set depthMode"
                match !o.depthMode.Handle with
                | Some m ->
                    yield fun () -> 
                        gl.enable(gl.DEPTH_TEST)
                        gl.depthFunc(m)
                | None ->
                    yield fun () -> 
                        gl.disable(gl.DEPTH_TEST)
            
            // bind uniforms
            for (id, b) in Map.toSeq o.uniformBuffers do
                match Map.tryFind id prev.uniformBuffers with
                | Some ob when ob.Handle = b.Handle -> 
                    ()
                | _ -> 
                    //Log.line "set UB %d" id
                    yield fun () -> 
                        let b = !b.Handle
                        gl.bindBufferBase(gl.UNIFORM_BUFFER, float id, b.Handle)

            // bind buffers
            for (id, (b, atts)) in Map.toSeq o.vertexBuffers do
                match Map.tryFind id prev.vertexBuffers with
                | Some (ob,oa) when ob.Handle = b.Handle && oa = atts ->
                    ()
                | _ -> 
                    //Log.line "set VB %d" id
                    yield fun () -> 
                        let b = !b.Handle
                        gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
                        let mutable id = id
                        for att in atts do
                            gl.enableVertexAttribArray(float id)
                            gl.vertexAttribPointer(float id, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                            id <- id + 1
                        gl.bindBuffer(gl.ARRAY_BUFFER, null)



            
            match o.indexBuffer with
            | Some (ib, info) ->
                yield fun () ->
                    let call = !o.call.Handle
                    let ib = !ib.Handle
                    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
                    gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))
            | None ->
                yield fun () ->
                    let call = !o.call.Handle
                    gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)

            //Log.stop()
        ]

[<AllowNullLiteral>]
type RenderObjectFragment(o : PreparedRenderObject) =
    let mutable prev : RenderObjectFragment = null
    let mutable next : RenderObjectFragment = null
    let mutable execute : list<unit -> unit> = []
    let mutable prevChanged = true


    member x.Object = o

    member x.RunSingle() =
        for f in execute do f() 
        

    member x.Run() =
        let mutable c = x
        while unbox c do 
            c.RunSingle()
            c <- c.Next

    member x.Prev
        with get() = prev
        and set p = 
            if p <> prev then
                prevChanged <- true
                prev <- p
            
    member x.Next
        with get() = next
        and set p = next <- p

    member x.Update(t) =
        if prevChanged then
            prevChanged <- false
            if unbox prev then
                let prev = prev.Object
                execute <- Compiler.compileDiff prev o
            else
                execute <- Compiler.compile o

    interface ILinked with
        member x.Update(t) = 
            x.Update t

        member x.Prev
            with get() = x.Prev :> ILinked
            and set p = x.Prev <- unbox p

        member x.Next
            with get() = x.Next :> ILinked
            and set p = x.Next <- unbox p



type RenderTask(signature : FramebufferSignature, manager : ResourceManager, objects : aset<RenderObject>) =
    inherit DirtyTrackingAdaptiveObject<IResource>("Resource")

    let trie = Trie<obj, PreparedRenderObject>(fun o -> RenderObjectFragment(o) :> ILinked)

    let getKey (o : PreparedRenderObject) =
        [
            o.program.Handle :> obj
            o.id :> obj
        ]




    let preparedObjects = objects |> ASet.map (fun o -> manager.Prepare(signature, o))
    let reader = preparedObjects.GetReader()
    let allResources = Dict<IResource, int>(Unchecked.hash, Unchecked.equals)

    let addResource (r : IResource) =
        let isNew = ref false
        allResources.Alter(r, fun o ->
            match o with
            | Some o -> Some (o+1)
            | None -> 
                isNew := true
                Some 1
        )
        !isNew

    let removeResource (r : IResource) =
        let isDead = ref false
        allResources.Alter(r, fun o ->
            match o with
            | None
            | Some 1 -> 
                isDead := true
                None
            | Some o ->
                Some (o - 1)
        )
        !isDead

    override x.Kind = "RenderTask"

    member x.Run(token : AdaptiveToken) =
        x.EvaluateAlways' token (fun token dirty ->
            let mutable dirty = HRefSet.ofSeq dirty

            let ops = reader.GetOperations token
            for o in ops do
                match o with
                | Add(_,o) ->
                    PreparedRenderObject.acquire o
                    for r in PreparedRenderObject.resources o do
                        if addResource r then
                            dirty <- HRefSet.add r dirty
                    trie.Add(getKey o, o) |> ignore
                | Rem(_,o) ->
                    trie.Remove(getKey o) |> ignore
                    for r in PreparedRenderObject.resources o do
                        if removeResource r then
                            dirty <- HRefSet.remove r dirty
                    PreparedRenderObject.release o

            if dirty.Count > 0 then
                for d in dirty do
                    d.Update token |> ignore

            trie.Update(token)
            let entry = trie.First |> unbox<RenderObjectFragment>
            entry.Run()
        )

    member x.Dispose() =
        for o in reader.State do
            PreparedRenderObject.release o

        reader.Dispose()
        allResources.Clear()

    interface IRenderTask with
        member x.Dispose() = x.Dispose()
        member x.Run t = x.Run t




[<EntryPoint>]
let main argv =
    let create (v : int) = 
        Fragment(v) :> ILinked

    let node = Trie<int, int>(create)

    node.Add([0], 0) |> ignore
    node.Add([0;1], 1) |> ignore
    node.Add([0;2], 2) |> ignore
    node.Add([1;2], 3) |> ignore
    node.Add([1;2], 4) |> ignore
    node.Add([1;2;3], 5) |> ignore
    node.Update(AdaptiveToken.Top)

    let w = node.Remove [0;1]
    node.Update(AdaptiveToken.Top)
    //Log.warn "remove %A" w
    let mutable c = unbox<Fragment> node.First
    while unbox c do
        Log.warn "%A" c.Code
        assert (if unbox c.Next then c.Next.Prev = c else true)
        c <- c.Next


    let shader =
        """#version 300 es

            #ifdef VERTEX

            uniform PerModel {
                mat4 ModelTrafo;
            };
            uniform PerView {
                float scale;
                mat4 ViewProjTrafo;
            };

            layout(location = 0) in vec3 Positions;
            layout(location = 1) in vec2 DiffuseColorCoordinates;
            out vec2 cc;
            void main() {
                gl_Position = (vec4(scale * Positions, 1.0) * ModelTrafo) * ViewProjTrafo;
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
            let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 60.0 0.1 1000.0 (float s.X / float s.Y) |> Frustum.projTrafo)
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
                let s = 11
                let rand = System.Random()
                Sg.ofList [
                    for x in -s/2 .. s/2 do
                        for y in -s/2 .. s/2 do
                            for z in -s/2 .. s/2 do
                                let phi = rand.NextDouble() * Constant.PiTimesTwo
                                let r = rand.NextDouble() * 10.0

                                let t = V3d(float x, float y, float z) * 3.0
                                yield sg |> Sg.trafo (Mod.constant <| Trafo3d.Translation t)
                ]


            let objects = sg.RenderObjects()


            let task() = 
                new RenderTask(control.FramebufferSignature, control.Manager, objects) :> IRenderTask
                //let prep = objects |> ASet.map (fun o -> control.Manager.Prepare(control.FramebufferSignature, o))

                //let reader = prep.GetReader()
                //{ new AbstractRenderTask() with
                //    member x.Render(token) =
                //        for op in reader.GetOperations token do
                //            match op with
                //            | Add(_,o) -> PreparedRenderObject.acquire o
                //            | Rem(_,o) -> PreparedRenderObject.release o

                //        for prep in reader.State do
                //            PreparedRenderObject.update token prep

                //        for prep in reader.State do
                //            PreparedRenderObject.render token prep
                //    member x.Release() =
                //        for o in reader.State do PreparedRenderObject.release o
                //        reader.Dispose()
                //}


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
