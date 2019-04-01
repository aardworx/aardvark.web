﻿namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open FSharp.Collections
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL

type nref<'a>(v : 'a) =
    let id = newId()
    let mutable value = v

    member x.Id = id

    member x.GetValue() = value

    member x.Value
        with get() = value
        and set v = value <- v

    override x.GetHashCode() = id
    override x.Equals o =
        match o with
        | :? nref<obj> as o -> o.Id = id
        | _ -> false

//[<AutoOpen>]
//module Assign =
//    let inline (:=) (a : ^a) (b : ^b) = ((^a) : (member set_Value : ^b -> unit) (a, b))
//    let inline (!) (l : ^a) = ((^a) : (member get_Value : unit -> ^b) (l))

type IResourceToken =
    inherit IDisposable
    abstract member Context : Context

type IResource =
    inherit IAdaptiveObject
    abstract member ResourceKind : string
    abstract member Acquire : unit -> unit
    abstract member Release : unit -> unit
    abstract member ReleaseAll : unit -> unit
    abstract member Update : AdaptiveToken -> unit

type IResource<'a> =
    inherit IResource
    abstract member Handle : nref<'a>
    //abstract member GetHandle : AdaptiveToken -> 'a

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

    let mutable hasHandle = false
    let handle = nref Unchecked.defaultof<'a>
    let mutable refCount = 0

    abstract member ResourceKind : string
    abstract member CreateRes : AdaptiveToken -> 'a
    abstract member UpdateRes : AdaptiveToken * 'a -> 'a
    abstract member DestroyRes : 'a -> unit

    override x.Kind = "Resource"

    member x.Acquire() =
        refCount <- refCount + 1

    member x.ReleaseAll() =
        entry.Dispose()
        refCount <- 0
        if hasHandle then
            hasHandle <- false
            x.DestroyRes handle.Value
            handle.Value <- Unchecked.defaultof<_>
        


    member x.Release() =
        refCount <- refCount - 1
        if refCount = 0 then
            if hasHandle then
                hasHandle <- false
                x.DestroyRes handle.Value
                handle.Value <- Unchecked.defaultof<_>
                entry.Dispose()

    member x.Update(t) =
        x.EvaluateAlways t (fun t ->
            if refCount <= 0 then failwith "updating unreferenced resource"

            if hasHandle then
                if x.OutOfDate then 
                    let hh = x.UpdateRes(t, handle.Value)
                    handle.Value <- hh
            else
                let h = x.CreateRes t
                handle.Value <- h
                hasHandle <- true
        )


    interface IResource with
        member x.ResourceKind = x.ResourceKind
        member x.Acquire() = x.Acquire()
        member x.Release() = x.Release()
        member x.ReleaseAll() = x.ReleaseAll()
        member x.Update(t) = x.Update t

    interface IResource<'a> with
        member x.Handle = handle
        //member x.GetHandle(t) = x.GetHandle(t)


type BufferResource(token : IResourceToken, target : float, data : IMod<IBuffer>) =
    inherit AbstractResource<Buffer>(token)

    override x.ResourceKind = "Buffer"

    override x.CreateRes(t) =
        let data = data.GetValue t
        token.Context.CreateBuffer(target, data)

    override x.UpdateRes(t, b) =
        let data = data.GetValue t
        let n = token.Context.CreateBuffer(target, data)
        b.Destroy()
        n

    override x.DestroyRes b =
        b.Destroy()

type UniformBufferResource(token : IResourceToken, layout : UniformBlockInfo, tryGetUniform : string -> Option<IMod>) =
    inherit AbstractResource<UniformBuffer>(token)

    let mutable write = Mod.constant ()
    
    override x.ResourceKind = "UniformBuffer"
    override x.CreateRes(t) =
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

    override x.UpdateRes(t, b) =
        write.GetValue t
        b
           
    override x.DestroyRes b =
        write <- Mod.constant ()
        b.Destroy()


type ResourceManager(ctx : Context) =
       
    let noToken =
        { new IResourceToken with
            member x.Dispose() = ()
            member x.Context = ctx
        }

    let bufferCache = ResourceCache(ctx)
    let indexBufferCache = ResourceCache(ctx)
    let uniformBufferCache = ResourceCache(ctx)
    let depthModeCache = ResourceCache(ctx)
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

    member x.CreateDepthMode(mode : IMod<DepthTestMode>) =
        let create (m : DepthTestMode) =
            let gl = x.Context.GL
            match m with
            | DepthTestMode.None -> None
            | DepthTestMode.Less -> Some gl.LESS
            | DepthTestMode.LessOrEqual -> Some gl.LEQUAL
            | DepthTestMode.Greater -> Some gl.GREATER
            | DepthTestMode.GreaterOrEqual -> Some gl.GEQUAL
            | DepthTestMode.Equal -> Some gl.EQUAL
            | DepthTestMode.NotEqual -> Some gl.NOTEQUAL
            | DepthTestMode.Always -> Some gl.ALWAYS
            | DepthTestMode.Never -> Some gl.NEVER
            | _ -> None

        depthModeCache.GetOrCreate([mode], fun token ->
            { new AbstractResource<Option<float>>(token) with
                member x.ResourceKind = "DepthTestMode"
                member x.UpdateRes(t,_) = create (mode.GetValue t)
                member x.CreateRes(t) = create (mode.GetValue t)
                member x.DestroyRes(_) = ()
            }
        )

    member x.CreateDrawCall(call : IMod<DrawCall>) =
        { new AbstractResource<DrawCall>(noToken) with
            member x.ResourceKind = "DrawCall"
            member x.UpdateRes(t,_) = call.GetValue t
            member x.CreateRes(t) = call.GetValue t
            member x.DestroyRes(_) = ()
        }


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
