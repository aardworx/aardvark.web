namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open FSharp.Collections
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL

type IResourceToken =
    inherit IDisposable
    abstract member Context : Context

type IResource =
    inherit IAdaptiveObject
    abstract member Acquire : unit -> unit
    abstract member Release : unit -> unit
    abstract member ReleaseAll : unit -> unit
    abstract member GetHandleObj : AdaptiveToken -> obj

type IResource<'a> =
    inherit IResource
    abstract member GetHandle : AdaptiveToken -> 'a

type ResourceCache(ctx : Context) =
    let hash (l : list<obj>) =
        l |> List.fold (fun h m -> HashCode.Combine(h, Unchecked.hash m)) 0

    let rec equals (l : list<obj>) (r : list<obj>) =
        match l, r with
        | [], [] -> true
        | l :: ls, r :: rs -> Unchecked.equals l r && equals ls rs
        | _ -> false

    let store = Dict<list<obj>, IResource>(hash, equals)
       
    member x.Context = ctx

    member internal x.Remove (key : list<obj>) =
        store.Remove key |> ignore

    member x.GetOrCreate<'a, 'b when 'a :> IResource<'b>>(deps : list<obj>, creator : IResourceToken -> 'a) =
        let resource = 
            store.GetOrCreate(deps, fun deps ->
                creator(new ResourceCacheEntry(x, deps) :> IResourceToken) :> IResource
            )

        unbox<IResource<'b>> resource

    member x.Clear() =
        for (k,v) in Seq.toList store do v.ReleaseAll()
        store.Clear()

and private ResourceCacheEntry (cache : ResourceCache, key : list<obj>) =
    interface IResourceToken with
        member x.Dispose() = cache.Remove key
        member x.Context = cache.Context


[<AbstractClass>]
type AbstractResource<'a>(entry : IDisposable) =
    inherit AdaptiveObject()

    let mutable handle = None
    let mutable refCount = 0

    abstract member Create : AdaptiveToken -> 'a
    abstract member Update : AdaptiveToken * 'a -> 'a
    abstract member Destroy : 'a -> unit

    override x.Kind = "Resource"

    member x.Acquire() =
        refCount <- refCount + 1

    member x.ReleaseAll() =
        entry.Dispose()
        refCount <- 0
        match handle with
        | Some h -> 
            x.Destroy h
            handle <- None
        | None ->
            ()


    member x.Release() =
        refCount <- refCount - 1
        if refCount = 0 then
            match handle with
            | Some h -> 
                x.Destroy h
                handle <- None
                entry.Dispose()
            | None -> 
                ()

    member x.GetHandle(t) =
        x.EvaluateAlways t (fun t ->
            if refCount <= 0 then failwith "updating unreferenced resource"

            match handle with
            | Some h ->
                if x.OutOfDate then 
                    let hh = x.Update(t, h)
                    handle <- Some hh
                    hh
                else
                    h
            | None ->
                let h = x.Create t
                handle <- Some h
                h
        )


    interface IResource with
        member x.Acquire() = x.Acquire()
        member x.Release() = x.Release()
        member x.ReleaseAll() = x.ReleaseAll()
        member x.GetHandleObj(t) = x.GetHandle(t) :> obj

    interface IResource<'a> with
        member x.GetHandle(t) = x.GetHandle(t)


type BufferResource(token : IResourceToken, target : float, data : IMod<IBuffer>) =
    inherit AbstractResource<Buffer>(token)

    override x.Create(t) =
        let data = data.GetValue t
        token.Context.CreateBuffer(target, data)

    override x.Update(t, b) =
        let data = data.GetValue t
        let n = token.Context.CreateBuffer(target, data)
        b.Destroy()
        n

    override x.Destroy b =
        b.Destroy()

type UniformBufferResource(token : IResourceToken, layout : UniformBlockInfo, tryGetUniform : string -> Option<IMod>) =
    inherit AbstractResource<UniformBuffer>(token)

    let mutable write = Mod.constant ()

    override x.Create(t) =
        let b = token.Context.CreateUniformBuffer(layout)

        let writers = 
            layout.fields |> List.choose (fun f ->
                match tryGetUniform f.name with
                | Some value ->
                    let writer = b.GetWriter(f.name, value)
                    Some writer
                | None ->
                    None
            )

        if not (List.isEmpty writers) then
            write <- 
                Mod.custom (fun t ->
                    for w in writers do w.GetValue t
                    b.Upload()
                )

        write.GetValue t

        b

    override x.Update(t, b) =
        write.GetValue t
        b
           
    override x.Destroy b =
        write <- Mod.constant ()
        b.Destroy()


type ResourceManager(ctx : Context) =
       
    let bufferCache = ResourceCache(ctx)
    let indexBufferCache = ResourceCache(ctx)
    let uniformBufferCache = ResourceCache(ctx)
    let programCache = Dict<FramebufferSignature * string, Option<Program>>(Unchecked.hash, Unchecked.equals)

    member x.Context = ctx

    member x.CreateUniformBuffer(block : UniformBlockInfo, tryGetUniform : string -> Option<IMod>) =
        let values = 
            block.fields |> List.map (fun f ->
                match tryGetUniform f.name with
                | Some m -> m
                | None -> failwithf "[GL] could not get uniform: %s" f.name
            )
        let key = (block :> obj) :: unbox values
        uniformBufferCache.GetOrCreate(key, fun token ->
            UniformBufferResource(token, block, tryGetUniform)
        )

    member x.CreateBuffer(data : IMod<IBuffer>) =
        bufferCache.GetOrCreate([data], fun token ->
            BufferResource(token, ctx.GL.ARRAY_BUFFER, data)
        )
           
    member x.CreateIndexBuffer(data : IMod<IBuffer>) =
        indexBufferCache.GetOrCreate([data], fun token ->
            BufferResource(token, ctx.GL.ELEMENT_ARRAY_BUFFER, data)
        )


    member x.CreateProgram(signature : FramebufferSignature, code : string) =
        let program = 
            programCache.GetOrCreate((signature, code), fun (signature, code) ->
                let program = ctx.CreateProgram(signature, code)
                match program with
                | Some p -> p.Acquire()
                | None -> ()
                program
            )
        match program with
        | Some p -> p
        | None -> failwith "[GL] could not compile program"

    member x.Dispose() =
        bufferCache.Clear()
        uniformBufferCache.Clear()
        for (_,p) in programCache do 
            match p with
            | Some p -> p.Destroy()
            | None -> ()
        programCache.Clear()

    interface IDisposable with
        member x.Dispose() = x.Dispose()
