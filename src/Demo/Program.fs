module Program

open System
open Aardvark.Base
open Fable.Core
open Aardvark.Import.Browser
open Aardvark.Import.JS
open FSharp.Collections
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL
open Aardvark.SceneGraph
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

    let compile (o : PreparedRenderObject) =
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

            match o.indexBuffer with
            | Some (ib, info) ->
                yield fun () ->
                    let call = o.call.Handle.Value
                    let ib = ib.Handle.Value
                    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
                    gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))
            | None ->
                yield fun () ->
                    let call = o.call.Handle.Value
                    gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)

            //Log.stop()
        |]

    let compileDiff (prev : PreparedRenderObject) (o : PreparedRenderObject) =
        let gl = o.program.Context.GL

        //compile o

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


[<AllowNullLiteral>]
type RenderObjectFragment(o : PreparedRenderObject) =
    let mutable prev : RenderObjectFragment = null
    let mutable next : RenderObjectFragment = null
    let mutable execute : array<unit -> unit> = [||]
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

[<AutoOpen>]
module Helpers =

    [<Emit("Symbol($0)")>]
    let newSym(str : string) : Symbol = jsNative

    [<Emit(@"delete $0[$1]")>]
    let delete (o : obj) (str : Symbol) : unit = jsNative

type RenderTask(signature : FramebufferSignature, manager : ResourceManager, objects : aset<RenderObject>) =
    inherit DirtyTrackingAdaptiveObject<IResource>("Resource")
    
    let mutable inRender = true
    let trie = Trie<obj, PreparedRenderObject>(fun o -> RenderObjectFragment(o) :> ILinked)

    let getKey (o : PreparedRenderObject) =
        [
            o.vertexBuffers |> Map.toList |> List.map (fun (i,(b,d)) -> i, b.Handle, d) :> obj
            o.program :> obj
            o.id :> obj
        ]

    let preparedObjects = objects |> ASet.choose (fun o -> manager.Prepare(signature, o))
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
                    PreparedRenderObject.acquire o
                    for r in PreparedRenderObject.resources o do
                        if addResource r then
                            ()
                            //dirty <- HRefSet.add r dirty

                    o?(sym) <- true

                    (PreparedRenderObject.update token o).``then``(fun () ->
                        if o?(sym) then
                            trie.Add(getKey o, o) |> ignore
                            if not inRender then transact x.MarkOutdated
                    ) |> ignore
                    
                | Rem(_,o) ->
                    delete o sym
                    trie.Remove(getKey o) |> ignore
                    for r in PreparedRenderObject.resources o do
                        if removeResource x r then
                            dirty <- HRefSet.remove r dirty
                    PreparedRenderObject.release o

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
            PreparedRenderObject.release o

        reader.Dispose()
        allResources.Clear()

    interface IRenderTask with
        member x.Dispose() = x.Dispose()
        member x.Run t = x.Run t


module FShadeTest =
    open FShade
    
    type TexCoord() = inherit SemanticAttribute("DiffuseColorCoordinates")
    type Normal() = inherit SemanticAttribute("Normals")

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
        member x.CameraLocation : V3d = uniform?PerView?CameraLocation
        member x.NormalMatrix : M33d = uniform?PerModel?NormalMatrix
        member x.ModelTrafo : M44d = uniform?PerModel?ModelTrafo
        member x.ViewProjTrafo : M44d = uniform?PerView?ViewProjTrafo
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
    

    //console.error typedefof<int * int * int>


    //let urls =
    //    [
    //        "http://localhost:8080/bundle.js"
    //        "http://localhost:8080/index.html"
    //        //"http://localhost:8080/asd.html"
    //    ]

    //let test =
    //    future {
    //        try
    //            let start = performance.now()
    //            let! data = urls |> List.chooseFuture Future.tryFetchBuffer
    //            let dt = performance.now() - start
    //            Log.warn "took: %A" dt
    //            let total = data |> List.sumBy (fun b -> int b.byteLength)
    //            return (dt, List.length data, total)
    //        with e ->
    //            Log.error "error: %A" e
    //            return (0.0, 0, 0)
    //    }

    //test.Continue(Log.line "success: %A", Log.error "error: %s")



    //let test = 
    //    set |> ASet.mapPromise (fun url ->
    //        promise {
    //            try
    //                let! a = Prom.fetchBuffer url
    //                return a
    //            with e ->
    //                Log.warn "err: %A" e
    //                return ArrayBuffer.Create (10.0)
    //        }
    //    )

    //let callback = ref id

    //let r = test.GetReader()
    //let o =
    //    { new AdaptiveObject() with
    //        member x.Kind = "Dummy"
    //        member x.MarkObj() = 
    //            setTimeout !callback 10 |> ignore
    //            true
    //    }
    //callback := fun () ->
    //    o.EvaluateAlways AdaptiveToken.Top (fun token ->
    //        let ops = r.GetOperations token
    //        ops |> HDeltaSet.iter (fun d ->
    //            match d with
    //            | Add(_,v) -> Log.warn "add %A" v.byteLength
    //            | Rem(_,v) -> Log.warn "rem %A" v.byteLength
    //        )
    //    )

    //callback.Value()

    //let rem() =
    //    transact (fun () ->
    //        set.Remove "http://localhost:8080/bundle.js" |> ignore
    //    )

    //setTimeout rem 2000 |> ignore



    //let shader =
    //    """#version 300 es
        
    //        precision highp float;
    //        precision highp int;

    //        uniform PerModel {
    //            mat3 NormalMatrix;
    //            mat4 ModelTrafo;
    //        };
    //        uniform PerView {
    //            mat4 ViewProjTrafo;
    //            vec3 CameraLocation;
    //        };

    //        uniform sampler2D Tex;

    //        #ifdef VERTEX
    //        layout(location = 0) in vec3 Positions;
    //        layout(location = 1) in vec3 Normals;
    //        layout(location = 2) in vec2 DiffuseColorCoordinates;
    //        out vec3 fs_Normals; 
    //        out vec4 fs_WorldPosition; 
    //        out vec2 fs_DiffuseColorCoordinates;

    //        void main() {
    //            vec4 wp = vec4(Positions, 1.0) * ModelTrafo;
    //            gl_Position = wp * ViewProjTrafo;
    //            fs_Normals = normalize(Normals * NormalMatrix);
    //            fs_WorldPosition = wp;
    //            fs_DiffuseColorCoordinates = DiffuseColorCoordinates;
    //        }
    //        #endif

    //        #ifdef FRAGMENT
    //        in vec3 fs_Normals; 
    //        in vec4 fs_WorldPosition; 
    //        in vec2 fs_DiffuseColorCoordinates;
    //        layout(location = 0) out vec4 Colors;
    //        void main() {
    //            vec3 v = normalize(CameraLocation);
    //            float diff = abs(dot(normalize(fs_Normals), normalize(CameraLocation - fs_WorldPosition.xyz)));

    //            vec4 color = texture(Tex, fs_DiffuseColorCoordinates, -0.5);


    //            Colors = vec4(vec3(0.05,0.05,0.05) + color.xyz * diff * 0.95, 1);
    //        }
    //        #endif
    //    """
        
    //let shader100 =
    //    """#version 100
            
    //        precision highp float;
    //        precision highp int;

    //        uniform mat4 ModelTrafo;
    //        uniform mat3 NormalMatrix;
    //        uniform mat4 ViewProjTrafo;
    //        uniform vec3 CameraLocation;
    //        uniform sampler2D Tex;

    //        #ifdef VERTEX
    //        attribute vec3 Positions;
    //        attribute vec3 Normals;
    //        attribute vec2 DiffuseColorCoordinates;
    //        varying vec3 fs_Normals; 
    //        varying vec4 fs_WorldPosition; 
    //        varying vec2 fs_DiffuseColorCoordinates;

    //        void main() {
    //            vec4 wp = vec4(Positions, 1.0) * ModelTrafo;
    //            gl_Position = wp * ViewProjTrafo;
    //            fs_Normals = normalize(Normals * NormalMatrix);
    //            fs_WorldPosition = wp;
    //            fs_DiffuseColorCoordinates = DiffuseColorCoordinates;
    //        }
    //        #endif

    //        #ifdef FRAGMENT
    //        varying vec3 fs_Normals; 
    //        varying vec4 fs_WorldPosition; 
    //        varying vec2 fs_DiffuseColorCoordinates;
                
    //        void main() {
    //            vec3 v = normalize(CameraLocation);
    //            float diff = abs(dot(normalize(fs_Normals), normalize(CameraLocation - fs_WorldPosition.xyz)));

    //            vec4 color = texture2D(Tex, fs_DiffuseColorCoordinates, -0.5);


    //            gl_FragColor = vec4(vec3(0.05,0.05,0.05) + color.xyz * diff * 0.95, 1);
    //        }
    //        #endif
    //    """


    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then

            //console.error (typeof<int[]>.GetElementType())

            let canvas = document.createElement_canvas()
            canvas.tabIndex <- 1.0
            document.body.appendChild(canvas) |> ignore
            document.body.style.margin <- "0"
            document.body.style.padding <- "0"
            canvas.style.width <- "100%"
            canvas.style.height <- "100%"
            
            let control = new Aardvark.Application.RenderControl(canvas)

            let initial = CameraView.lookAt (V3d(6.0, 6.0, 4.0)) V3d.Zero V3d.OOI
            let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial
            let anim = Mod.constant true //control.Keyboard.IsDown(Aardvark.Application.Keys.Space)
            let angle =
                Mod.integrate 0.0 control.Time [
                    anim |> Mod.map (fun a ->
                        if a then 
                            control.Time |> Mod.stepTime (fun _ dt o -> o + 0.1 * dt)
                        else
                            AFun.create id
                    )
                ]

            //let asdasd =
            //    Mod.integrate 0.0 control.Time [
            //        control.Time |> Mod.stepTime (fun _ dt o -> o + 0.1 * dt) |> Mod.constant
            //    ]

            //let cam = 
            //    Mod.map2 (fun a (cam : CameraView) -> 
            //        let t = Trafo3d.RotationZ(a)

            //        cam.WithLocation(t.Forward.TransformPos cam.Location)
            //    ) asdasd cam

            let view = cam |> Mod.map CameraView.viewTrafo
            let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 70.0 0.1 1000.0 (float s.X / float s.Y) |> Frustum.projTrafo)




            let sphere =
                Sg.sphere 2
                |> Sg.trafo (Mod.constant (Trafo3d.Scale(0.5)))

                |> Sg.viewTrafo view
                |> Sg.projTrafo proj
                |> Sg.uniform "DiffuseColorTexture" (Mod.constant (FileTexture "pattern.jpg" :> ITexture))
                
            let sphere2 =
                Sg.sphere 4
                |> Sg.trafo (Mod.constant (Trafo3d.Scale(0.5) ))

                |> Sg.viewTrafo view
                |> Sg.projTrafo proj
                |> Sg.uniform "DiffuseColorTexture" (Mod.constant (FileTexture "cliffs_color.jpg" :> ITexture))
                    
            let box =
                Sg.box Box3d.Unit
                |> Sg.trafo (Mod.constant (Trafo3d.Translation(-0.5, -0.5, -0.5)))

                |> Sg.viewTrafo view
                |> Sg.projTrafo proj
                |> Sg.uniform "DiffuseColorTexture" (Mod.constant (FileTexture "test.jpg" :> ITexture))

            let rand = System.Random()
            let sett =
                let s = 2
                cset [
                    for x in -s/2 .. (s - s/2 - 1) do
                        for y in -s/2 .. (s - s/2 - 1) do
                            for z in -s/2 .. (s - s/2 - 1) do
                                let r = rand.NextDouble()
                                
                                let sg =
                                    if r > 0.6 then sphere2
                                    elif r > 0.3 then sphere
                                    else box

                                let t = V3d(float x, float y, float z) * 1.5
                                let axis = V3d(rand.NextDouble() * 20.0 - 10.0, rand.NextDouble() * 20.0 - 10.0, rand.NextDouble() * 20.0 - 10.0)|> Vec.normalize

                                let speed = rand.NextDouble() * 40.0 - 20.0
                                let model = angle |> Mod.map (fun a -> Trafo3d.Rotation(axis, speed * a))
                                let s = 
                                    sg 
                                    |> Sg.trafo model
                                    |> Sg.trafo (Mod.constant <| Trafo3d.Translation t)
                                yield s
                ]

            let mode = Mod.init (FShadeTest.A { FShadeTest.ambient = 0.1; FShadeTest.diffuse = 0.9 })

            let sg =
                Sg.set sett
                |> Sg.effect [
                    FShade.Effect.ofFunction (FShadeTest.constantColor V4d.IOOI)
                    FShade.Effect.ofFunction FShadeTest.trafo
                    FShade.Effect.ofFunction FShadeTest.diffuseTexture
                    FShade.Effect.ofFunction FShadeTest.simpleLight
                ]
                |> Sg.uniform "Ambient" mode


            let objects = sg.RenderObjects()

            //let test = <@ (1 + 2) * 3 @>
            //console.warn(string test)
            //let test = MapExt.ofList [1,2;3,4]
            //let res = FShade.Imperative.ModuleCompiler.compile (failwith "") (failwith "")


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
                | Aardvark.Application.Keys.Space ->
                    transact (fun () ->
                        match mode.Value with
                        | FShadeTest.A _ -> mode.Value <- FShadeTest.B 1.2
                        | FShadeTest.B _ -> mode.Value <- FShadeTest.A { FShadeTest.ambient = 0.1; FShadeTest.diffuse = 0.9 }
                    )
                | Aardvark.Application.Keys.Delete ->
                    if active then
                        active <- false
                        control.RenderTask <- RenderTask.empty
                    else
                        active <- true
                        control.RenderTask <- task()
                | Aardvark.Application.Keys.X ->
                    
                    if sett.Count > 0 then
                        //transact (fun () -> sett.Clear())
                        let e = sett |> Seq.item (rand.Next sett.Count)
                        transact (fun () -> sett.Remove e |> ignore)
                | _ ->
                    ()
            )


            control.RenderTask <- task()
    )



    0 // return an integer exit code
