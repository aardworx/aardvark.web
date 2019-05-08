namespace Aardvark.Rendering.WebGL



open System
open Aardvark.Base
open Fable.Core
open Aardvark.Import.Browser
open Aardvark.Import.JS
open FSharp.Collections
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL
open Fable.Core.JsInterop
  
[<AllowNullLiteral>]
type ILinked =
    abstract member Next : ILinked with get, set
    abstract member Prev : ILinked with get, set
    abstract member Update : AdaptiveToken -> unit

[<AllowNullLiteral>]
type TrieNode<'k, 'a>(dirty : DictSet<ILinked>, parent : TrieNode<'k, 'a>, create : 'a -> ILinked) =
    let mutable value : Option<ILinked> = None

    let mutable prev : TrieNode<'k, 'a> = null
    let mutable next : TrieNode<'k, 'a> = null

    let mutable lastChild : TrieNode<'k, 'a> = null
    let mutable firstChild : TrieNode<'k, 'a> = null

    let children = Dict<'k, TrieNode<'k, 'a>>(Unchecked.hash, Unchecked.equals)

    member x.First : ILinked =
        match value with
        | Some v -> v
        | None ->
            if unbox firstChild then firstChild.First
            else null

    member x.Last : ILinked =
        if unbox lastChild then lastChild.Last
        elif FSharp.Core.Option.isSome value then value.Value
        else null

    member x.Prev
        with get() = prev
        and set p = prev <- p
        
    member x.Next
        with get() = next
        and set p = next <- p

    member x.Add(keys : list<'k>, v : 'a, left : TrieRef<'k, 'a>, right : TrieRef<'k, 'a>) =
        match keys with
        | [] ->
            match value with
            | Some v ->
                let prev = v.Prev
                let next = v.Next
                if unbox prev then prev.Next <- next
                if unbox next then next.Prev <- prev
                value <- None
            | _ ->
                ()

            let next =
                if unbox firstChild then firstChild.First
                else right.First

            let prev = 
                left.Last

            let l = create v
            l.Prev <- prev
            l.Next <- next

            dirty.Add l |> ignore
            if unbox prev then 
                prev.Next <- l
            if unbox next then 
                next.Prev <- l
                dirty.Add next |> ignore

            value <- Some l

        | k :: ks ->

            match children.TryGetValue k with
            | Some c ->
                let l = 
                    if unbox c.Prev then Trie c.Prev
                    elif FSharp.Core.Option.isSome value then Value value.Value
                    else left

                let r =
                    if unbox c.Next then Trie c.Next
                    else right

                c.Add(ks, v, l, r)
            | None ->
                let c = TrieNode(dirty, x, create)
                let l = 
                    if unbox lastChild then Trie lastChild
                    elif FSharp.Core.Option.isSome value then Value value.Value
                    else left
                    
                let r =
                    right
                c.Add(ks, v, l, r)



                if unbox lastChild then lastChild.Next <- c
                else firstChild <- c
                c.Prev <- lastChild
                lastChild <- c
                assert(unbox lastChild && unbox firstChild)
                children.[k] <- c

    member x.Remove(keys : list<'k>) =
        match keys with
        | [] -> 
            let l = x.First
            let r = x.Last

            let mutable c = l
            while c <> r.Next do
                dirty.Remove c |> ignore
                c <- c.Next

            if unbox l.Prev then l.Prev.Next <- r.Next
            if unbox r.Next then 
                dirty.Add r.Next |> ignore
                r.Next.Prev <- l.Prev


            true
        | k :: ks ->
            match children.TryGetValue k with
            | Some c ->
                if c.Remove ks then
                    children.Remove k |> ignore
                    if unbox c.Prev then c.Prev.Next <- c.Next
                    else firstChild <- c.Next

                    if unbox c.Next then c.Next.Prev <- c.Prev
                    else lastChild <- c.Prev

                    if children.Count > 0 || FSharp.Core.Option.isSome value then false
                    else true
                else
                    false
            | None ->
                false

and TrieRef<'k, 'a> =
    | Nothing
    | Trie of TrieNode<'k, 'a>
    | Value of ILinked

    member x.First = 
        match x with
        | Nothing -> null
        | Trie t -> t.First
        | Value v -> v

    member x.Last = 
        match x with
        | Nothing -> null
        | Trie t -> t.Last
        | Value v -> v

type Trie<'k, 'a>(create : 'a -> ILinked) =
    let dirty = DictSet<ILinked>(Unchecked.hash, Unchecked.equals)
    let mutable root = TrieNode<'k, 'a>(dirty, null, create)

    member x.First = root.First
    member x.Last = root.Last


    member x.Add(keys : list<'k>, value : 'a) =
        root.Add(keys, value, Nothing, Nothing)

    member x.Remove(keys : list<'k>) =
        if root.Remove keys then
            root <- TrieNode<'k, 'a>(dirty, null, create)

    member x.Update(token : AdaptiveToken) =
        for d in dirty do 
            if unbox d then
                d.Update(token)
        dirty.Clear()


module Compiler =

    let private setUniform (gl : WebGL2RenderingContext) (typ : PrimitiveType) (loc : WebGLUniformLocation) (r : nref<UniformLocation>) =
        match typ with
        | Bool -> fun () -> gl.uniform1iv(loc, Int32Array.Create(r.Value.Store.buffer, 0, 1))

        | Float _ -> fun () -> gl.uniform1fv(loc, Float32Array.Create(r.Value.Store.buffer, 0, 1))
        | Vec (Float _, 2) -> fun () -> gl.uniform2fv(loc, Float32Array.Create(r.Value.Store.buffer, 0, 2))
        | Vec (Float _, 3) -> fun () -> gl.uniform3fv(loc, Float32Array.Create(r.Value.Store.buffer, 0, 3))
        | Vec (Float _, 4) -> fun () -> gl.uniform4fv(loc, Float32Array.Create(r.Value.Store.buffer, 0, 4))

        | Int _ -> fun () -> gl.uniform1iv(loc, Int32Array.Create(r.Value.Store.buffer, 0, 1))
        | Vec (Int _, 2) -> fun () -> gl.uniform2iv(loc, Int32Array.Create(r.Value.Store.buffer, 0, 2))
        | Vec (Int _, 3) -> fun () -> gl.uniform3iv(loc, Int32Array.Create(r.Value.Store.buffer, 0, 3))
        | Vec (Int _, 4) -> fun () -> gl.uniform4iv(loc, Int32Array.Create(r.Value.Store.buffer, 0, 4))

        | Mat (Float _, 2, 2) -> fun () -> gl.uniformMatrix2fv(loc, false, Float32Array.Create(r.Value.Store.buffer, 0, 4))
        | Mat (Float _, 3, 3) -> fun () -> gl.uniformMatrix3fv(loc, false, Float32Array.Create(r.Value.Store.buffer, 0, 9))
        | Mat (Float _, 4, 4) -> fun () -> gl.uniformMatrix4fv(loc, false, Float32Array.Create(r.Value.Store.buffer, 0, 16))

        | _ -> failwithf "bad uniform type : %A" typ


    let setPipelineState (o : PreparedPipelineState) =
        let gl = o.program.Context.GL
        [|
            //Log.start "init(%d)" o.id
            //Log.line "set program"
            yield fun () -> gl.useProgram(o.program.Handle)
            
            //Log.line "set depthMode"
            match o.depthMode.Handle.Value with
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
                    let b = b.Handle.Value
                    gl.bindBufferRange(gl.UNIFORM_BUFFER, float id, b.Handle, float b.Offset, float b.Size)
                    
            for (id, (loc, tex, sam)) in Map.toSeq o.samplers do
                let r = tex.Handle
                match sam with
                | Some sam ->
                    yield fun () ->
                        gl.activeTexture(gl.TEXTURE0 + float id)
                        gl.uniform1i(loc, float id)
                        gl.bindTexture(gl.TEXTURE_2D, r.Value.Handle)
                        gl.bindSampler(float id, sam.Handle.Value.Handle)

                | None -> 
                    yield fun () ->
                        gl.activeTexture(gl.TEXTURE0 + float id)
                        gl.uniform1i(loc, float id)
                        gl.bindTexture(gl.TEXTURE_2D, r.Value.Handle)

            for (loc, (typ,r)) in o.uniforms do
                yield setUniform gl typ loc r.Handle

        
        |]
        


    let updatePipelineState (prev : PreparedPipelineState) (o : PreparedPipelineState) =
        let gl = o.program.Context.GL
        [|
            //Log.start "compile(%d, %d)" prev.id o.id
            if prev.program.Handle <> o.program.Handle then
                //Log.line "set program"
                yield fun () -> gl.useProgram(o.program.Handle)

            if prev.depthMode.Handle <> o.depthMode.Handle then
                //Log.line "set depthMode"
                yield fun () -> 
                    match o.depthMode.Handle.Value with
                    | Some m ->
                        gl.enable(gl.DEPTH_TEST)
                        gl.depthFunc(m)
                    | None ->
                        gl.disable(gl.DEPTH_TEST)
                    
            // bind uniforms
            for (id, b) in Map.toSeq o.uniformBuffers do
                match Map.tryFind id prev.uniformBuffers with
                | Some ob when ob = b -> 
                    ()
                | _ -> 
                    //Log.line "set UB %d" id
                    yield fun () -> 
                        let b = b.Handle.Value
                        gl.bindBufferRange(gl.UNIFORM_BUFFER, float id, b.Handle, float b.Offset, float b.Size)

            let eq (l : Option<IResource<Sampler>>) (r : Option<IResource<Sampler>>) =
                match l, r with
                | Some l, Some r -> l.Handle = r.Handle
                | None, None -> true
                | _ -> false

            for (id, (loc, tex, sam)) in Map.toSeq o.samplers do
                match Map.tryFind id prev.samplers with
                | Some (ol,ot,os) when ol = loc && ot.Handle = tex.Handle && eq os sam ->
                    ()
                | _ -> 
                    let r = tex.Handle
                    match sam with
                    | Some sam ->
                        yield fun () ->
                            gl.activeTexture(gl.TEXTURE0 + float id)
                            gl.uniform1i(loc, float id)
                            gl.bindTexture(gl.TEXTURE_2D, r.Value.Handle)
                            gl.bindSampler(float id, sam.Handle.Value.Handle)

                    | None -> 
                        yield fun () ->
                            gl.activeTexture(gl.TEXTURE0 + float id)
                            gl.uniform1i(loc, float id)
                            gl.bindTexture(gl.TEXTURE_2D, r.Value.Handle)
            
            for (loc, (typ,r)) in o.uniforms do
                match HMap.tryFind loc prev.uniforms with
                | Some (tt, tr) when typ = tt && tr.Handle = r.Handle ->
                    ()
                | _ -> 
                    yield setUniform gl typ loc r.Handle

        |]
        

    let render (o : PreparedRenderObject) =
        let gl = o.pipeline.program.Context.GL

        [|
            // bind buffers
            for (id, (b, atts)) in Map.toSeq o.vertexBuffers do
                //Log.line "set VB %d" id
                yield fun () -> 
                    let b = b.Handle.Value
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
                let h = o.call.Handle
                let ib = ib.Handle
                yield fun () ->
                    let call = h.Value
                    let ib = ib.Value
                    if unbox ib && unbox call then
                        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
                        gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))
            | None ->
                let h = o.call.Handle
                yield fun () ->
                    let call = h.Value
                    if unbox call then
                        gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)

            //Log.stop()
        |]

    let compile (o : PreparedRenderObject) =
        let gl = o.pipeline.program.Context.GL

        [|
            yield! setPipelineState o.pipeline
            yield! render o
        |]

    let compileDiff (prev : PreparedRenderObject) (o : PreparedRenderObject) =
        let gl = o.pipeline.program.Context.GL

        [|
            yield! updatePipelineState prev.pipeline o.pipeline
          
            // bind buffers
            for (id, (b, atts)) in Map.toSeq o.vertexBuffers do
                match Map.tryFind id prev.vertexBuffers with
                | Some (ob,oa) when ob.Handle = b.Handle && oa = atts ->
                    ()
                | _ -> 
                    //Log.line "set VB %d" id
                    yield fun () -> 
                        let b = b.Handle.Value
                        gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
                        let mutable id = id
                        for att in atts do
                            gl.enableVertexAttribArray(float id)
                            gl.vertexAttribPointer(float id, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                            id <- id + 1
                        gl.bindBuffer(gl.ARRAY_BUFFER, null)


            
            match prev.indexBuffer, o.indexBuffer with
            | Some (ob,oi), Some (ib, info) when ib.Handle = ob.Handle && oi = info ->
                
                yield fun () ->
                    let call = o.call.Handle.Value
                    gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))

            | _, None ->
                yield fun () ->
                    let call = o.call.Handle.Value
                    gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)
                    

            | _,  Some (ib, info) ->
                //Log.line "set IB"
                yield fun () ->
                    let call = o.call.Handle.Value
                    let ib = ib.Handle.Value
                    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
                    gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))

            //Log.stop()
        |]


type RenderObjectCommand(manager : ResourceManager, prep : PreparedRenderObject) =
    inherit PreparedCommand(manager)

    member x.Object = prep

    override x.Resources = PreparedRenderObject.resources prep
    override x.ExitState = prep.pipeline
    override x.Acquire() = PreparedRenderObject.acquire prep
    override x.Release() = PreparedRenderObject.release prep
    override x.Update(token : AdaptiveToken) = PreparedRenderObject.update token prep
    override x.Compile(prev : Option<PreparedCommand>) = 
        match prev with
        | Some prev ->
            match prev with
            | :? RenderObjectCommand as prev -> Compiler.compileDiff prev.Object prep
            | _ -> 
                FSharp.Collections.Array.append
                    (Compiler.updatePipelineState prev.ExitState prep.pipeline)
                    (Compiler.render prep)
        | None ->
            Compiler.compile prep

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PreparedCommand =
    let ofRenderObject (manager : ResourceManager) (signature : FramebufferSignature) (obj : IRenderObject) =
        match obj with
        | :? RenderObject as o ->
            match manager.Prepare(signature, o) with
            | Some p -> RenderObjectCommand(manager, p) :> PreparedCommand |> Some
            | _ -> None
        | :? PreparedRenderObject as o ->
            RenderObjectCommand(manager, o) :> PreparedCommand |> Some

        | :? PreparedCommand as o ->
            o |> Some

        | _ ->
            failwithf "bad renderobject: %A" obj


[<AllowNullLiteral>]
type RenderObjectFragment(o : PreparedCommand) =
    let mutable prev : RenderObjectFragment = null
    let mutable next : RenderObjectFragment = null
    let mutable execute : array<unit -> unit> = [||]
    let mutable prevChanged = true


    member x.Object = o

    member x.RunSingle() =
        for f in execute do f() 
        

    member x.Run() =
        let mutable count = 0
        let mutable c = x
        while unbox c do 
            c.RunSingle()
            count <- count + 1
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
                execute <- o.Compile(Some prev)
            else
                execute <- o.Compile(None)

    interface ILinked with
        member x.Update(t) = 
            x.Update t

        member x.Prev
            with get() = x.Prev :> ILinked
            and set p = x.Prev <- unbox p

        member x.Next
            with get() = x.Next :> ILinked
            and set p = x.Next <- unbox p

[<AutoOpen>]
module SymbolHelpers =

    [<Emit("Symbol($0)")>]
    let newSym(str : string) : Symbol = jsNative

    [<Emit(@"delete $0[$1]")>]
    let delete (o : obj) (str : Symbol) : unit = jsNative

type RenderTask(signature : FramebufferSignature, manager : ResourceManager, objects : aset<IRenderObject>) =
    inherit DirtyTrackingAdaptiveObject<IResource>("Resource")
    
    let mutable inRender = true
    let trie = Trie<obj, PreparedCommand>(fun o -> RenderObjectFragment(o) :> ILinked)

    let getKey (o : PreparedCommand) =
        [
            o.ExitState.program :> obj
            //o.vertexBuffers |> Map.toList |> List.map (fun (i,(b,d)) -> i, b.Handle, d) :> obj
            o.Id :> obj
        ]

    let preparedObjects = 
        objects |> ASet.choose (fun o -> 
            PreparedCommand.ofRenderObject manager signature o
            //match o with
            //| :? RenderObject as o -> manager.Prepare(signature, o)
            //| _ -> Some (unbox<PreparedRenderObject> o)
        )

    let reader = preparedObjects.GetReader()
    let allResources = Dict<IResource, int>(Unchecked.hash, Unchecked.equals)

    let sym = newSym "RenderTasky"
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

    let removeResource (x : RenderTask) (r : IResource) =
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
        if !isDead then
            r.RemoveOutput x
            true
        else
            false

    override x.Kind = "RenderTask"

    member x.Run(token : AdaptiveToken) =
        x.EvaluateAlways' token (fun token dirty ->
            let mutable dirty = HRefSet.ofSeq dirty
            let ops = reader.GetOperations token


            inRender <- true
            for o in ops do
                match o with
                | Add(_,o) ->
                    o.Acquire()
                    let res = System.Collections.Generic.List<IResource>()
                    for r in o.Resources do
                        if addResource r then
                            res.Add r
                            ()
                            //dirty <- HRefSet.add r dirty

                    o?(sym) <- true

                    res |> Seq.map (fun r -> r.Update token) |> Prom.all |> Prom.map (fun _ ->
                        if o?(sym) then
                            trie.Add(getKey o, o) |> ignore
                            if not inRender then transact x.MarkOutdated
                    ) |> ignore

                    
                | Rem(_,o) ->
                    delete o sym
                    trie.Remove(getKey o) |> ignore
                    for r in o.Resources do
                        if removeResource x r then
                            dirty <- HRefSet.remove r dirty
                    o.Release()

            inRender <- false

            let promises = 
                if dirty.Count > 0 then
                    dirty |> Seq.map (fun d -> d.Update token) |> Prom.all |> unbox<Promise<unit>>
                else
                    Prom.value ()

            promises.``then``(fun () ->
                UniformBufferManager.UploadAll()
                trie.Update(token)
                let entry = trie.First |> unbox<RenderObjectFragment>
                entry.Run()
            )
        )

    member x.Dispose() =
        for o in reader.State do
            o.Release()

        reader.Dispose()
        allResources.Clear()

    interface IRenderTask with
        member x.Dispose() = x.Dispose()
        member x.Run t = x.Run t
