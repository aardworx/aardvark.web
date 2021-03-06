﻿namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open FSharp.Collections
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL
open Aardvark.Import.JS



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

type IResourceToken =
    inherit IDisposable
    abstract member Context : Context

type IResource =
    inherit IAdaptiveObject
    abstract member ResourceKind : string
    abstract member Acquire : unit -> unit
    abstract member Release : unit -> unit
    abstract member ReleaseAll : unit -> unit
    abstract member Update : AdaptiveToken -> Promise<unit>

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
                let r = creator(new ResourceCacheEntry(x, deps) :> IResourceToken) :> IResource
                r
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

    let handle = nref Unchecked.defaultof<'a>
    let mutable promise : Option<Promise<unit>> = None
    let mutable refCount = 0

    abstract member ResourceKind : string
    abstract member CreateRes : AdaptiveToken -> Promise<'a>
    abstract member UpdateRes : AdaptiveToken * 'a -> Promise<'a>
    abstract member DestroyRes : 'a -> unit

    override x.Kind = "Resource"

    member x.Acquire() =
        refCount <- refCount + 1

    member x.ReleaseAll() =
        entry.Dispose()
        refCount <- 0
        match promise with
        | Some p ->
            p.``then``(fun () ->
                x.DestroyRes handle.Value |> ignore
                handle.Value <- Unchecked.defaultof<_>
            ) |> ignore
            promise <- None
        | None ->
            x.DestroyRes handle.Value |> ignore
            handle.Value <- Unchecked.defaultof<_>
        


    member x.Release() =
        refCount <- refCount - 1
        if refCount = 0 then
            entry.Dispose()
            match promise with
            | Some p ->
                p.``then``(fun () ->
                    x.DestroyRes handle.Value |> ignore
                    handle.Value <- Unchecked.defaultof<_>
                ) |> ignore
                promise <- None
            | None ->
                x.DestroyRes handle.Value |> ignore
                handle.Value <- Unchecked.defaultof<_>
            

    member x.Update(t) =
        x.EvaluateAlways t (fun t ->
            if refCount <= 0 then 
                Log.error "updating unreferenced resource"
                Prom.value ()
            else
                match promise with
                | Some p ->
                    if x.OutOfDate then
                        let np = 
                            p |> Prom.bind (fun () ->
                                x.UpdateRes(t, handle.Value).``then`` (fun h ->
                                    handle.Value <- h
                                )
                            )
                        promise <- Some np
                        np
                    else
                        p
                | None ->
                    if x.OutOfDate then
                        let h = x.CreateRes t
                        let p = h.``then``(fun h -> handle.Value <- h)
                        promise <- Some p
                        p
                    else
                        Prom.value ()
                        //failwith "strange"
                


            //if hasHandle then
            //    if x.OutOfDate then 
            //        match promise with
            //        | Some p -> 
            //            let np = 
            //                p |> Prom.bind (fun () ->
            //                    x.UpdateRes(t, handle.Value).``then`` (fun h ->
            //                        handle.Value <- h
            //                    )
            //                )
            //            promise <- Some np
            //            np
            //        | None ->
            //            let hh = x.UpdateRes(t, handle.Value)
            //            let p =
            //                hh.``then``(fun hh ->
            //                    handle.Value <- hh
            //                )
            //            promise <- Some p
            //            p
            //    else
            //        Prom.value ()
            //else
            //    if x.OutOfDate then 
            //        let h = x.CreateRes t
            //        h.``then``(fun h ->
            //            handle.Value <- h
            //            hasHandle <- true
            //        )
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
        b.Release()
        n

    override x.DestroyRes b =
        b.Release()

type TextureResource(token : IResourceToken, tex : IMod<ITexture>) =
    inherit AbstractResource<Texture>(token)

    override x.ResourceKind = "Texture"

    override x.CreateRes(t) =
        let data = tex.GetValue t
        token.Context.CreateTexture(data, None)

    override x.UpdateRes(t, b) =
        let data = tex.GetValue t
        let n = token.Context.CreateTexture(data, None)
        b.Release()
        n

    override x.DestroyRes b =
        b.Release()

type SamplerResource(token : IResourceToken, tex : IMod<FShade.SamplerState>) =
    inherit AbstractResource<Sampler>(token)

    override x.ResourceKind = "Sampler"

    override x.CreateRes(t) =
        let data = tex.GetValue t
        token.Context.CreateSampler(data) |> Prom.value

    override x.UpdateRes(t, b) =
        let data = tex.GetValue t
        let n = token.Context.CreateSampler(data)
        b.Release()
        n |> Prom.value

    override x.DestroyRes b =
        b.Release()

type SampledTextureResource(token : IResourceToken, tex : IMod<ITexture>, sammy : FShade.SamplerState) =
    inherit AbstractResource<Texture>(token)

    override x.ResourceKind = "Texture"

    override x.CreateRes(t) =
        let data = tex.GetValue t
        token.Context.CreateTexture(data, Some sammy)

    override x.UpdateRes(t, b) =
        let data = tex.GetValue t
        let n = token.Context.CreateTexture(data, Some sammy)
        b.Release()
        n

    override x.DestroyRes b =
        b.Release()

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

        Prom.value b

    override x.UpdateRes(t, b) =
        write.GetValue t
        Prom.value b
           
    override x.DestroyRes b =
        write <- Mod.constant ()
        b.Destroy()


type UniformBufferSlotResource(token : IResourceToken, man : UniformBufferManager, tryGetUniform : string -> Option<IMod>) =
    inherit AbstractResource<UniformBufferSlot>(token)

    let mutable write = Mod.constant ()
    
    override x.ResourceKind = "UniformBufferSlot"
    override x.CreateRes(t) =
        let b = man.Alloc()

        let writers = 
            man.Layout.fields |> List.choose (fun f ->
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
                )

        write.GetValue t
        Prom.value b

    override x.UpdateRes(t, b) =
        write.GetValue t
        Prom.value b
           
    override x.DestroyRes b =
        write <- Mod.constant ()
        b.Free()


type UniformLocationResource(token : IResourceToken, typ : Types.PrimitiveType, value : IMod) =
    inherit AbstractResource<UniformLocation>(token)
    
    let mutable write = Mod.constant ()
    
    override x.ResourceKind = "UniformLocation"
    override x.CreateRes(t) =
        let b = token.Context.CreateUniformLocation(typ)
        write <- b.GetWriter(value)
        write.GetValue t
        
        Prom.value b
        
    override x.UpdateRes(t, b) =
        write.GetValue t
        Prom.value b
                   
    override x.DestroyRes b =
        write <- Mod.constant ()
        b.Destroy()

type ResourceManager(ctx : Context) =
    let isGL2 = ctx.GL.IsGL2

    let noToken =
        { new IResourceToken with
            member x.Dispose() = ()
            member x.Context = ctx
        }

    let bufferCache = ResourceCache(ctx)
    let textureCache = ResourceCache(ctx)
    let samplerCache = ResourceCache(ctx)
    let sampledTextureCache = ResourceCache(ctx)
    let indexBufferCache = ResourceCache(ctx)
    let uniformBufferCache = ResourceCache(ctx)
    let uniformBufferSlotCache = ResourceCache(ctx)
    let uniformLocationCache = ResourceCache(ctx)
    let depthModeCache = ResourceCache(ctx)
    let programCache = Dict<FramebufferSignature * string, Option<Program>>(Unchecked.hash, Unchecked.equals)

    let ubManagers = Dict<UniformBlockInfo, UniformBufferManager>(Unchecked.hash, Unchecked.equals)

    member x.IsGL2 = isGL2
    member x.Context = ctx

    member x.CreateUniformBufferSlot(block : UniformBlockInfo, tryGetUniform : string -> Option<IMod>) =
        let values = 
            block.fields |> List.map (fun f ->
                match tryGetUniform f.name with
                | Some m -> m
                | None -> failwithf "[GL] could not get uniform: %s" f.name
            )

        let key = (block :> obj) :: unbox values
        uniformBufferSlotCache.GetOrCreate(key, fun token ->
            let man = ubManagers.GetOrCreate(block, fun b -> UniformBufferManager(ctx, b))
            UniformBufferSlotResource(token, man, tryGetUniform)
        )
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
           
    member x.CreateSampledTexture(data : IMod<ITexture>, sam : FShade.SamplerState) =
        let a = data
        let b = sam
        sampledTextureCache.GetOrCreate([data; sam], fun token ->
            SampledTextureResource(token, a, b)
        )

    member x.CreateTexture(data : IMod<ITexture>) =
        let a = data
        textureCache.GetOrCreate([data], fun token ->
            TextureResource(token, a)
        )
        
    member x.CreateSampler(data : IMod<FShade.SamplerState>) =
        let a = data
        textureCache.GetOrCreate([data], fun token ->
            SamplerResource(token, a)
        )
                  
    member x.CreateIndexBuffer(data : IMod<IBuffer>) =
        indexBufferCache.GetOrCreate([data], fun token ->
            BufferResource(token, ctx.GL.ELEMENT_ARRAY_BUFFER, data)
        )
    member x.CreateUniformLocation(typ : Types.PrimitiveType, data : IMod) =
        uniformLocationCache.GetOrCreate([typ; data], fun token ->
            UniformLocationResource(token, typ, data)
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
                member x.UpdateRes(t,_) = create (mode.GetValue t) |> Prom.value
                member x.CreateRes(t) = create (mode.GetValue t) |> Prom.value
                member x.DestroyRes(_) = ()
            }
        )

    member x.CreateDrawCall(call : IMod<DrawCall>) =
        { new AbstractResource<DrawCall>(noToken) with
            member x.ResourceKind = "DrawCall"
            member x.UpdateRes(t,_) = call.GetValue t |> Prom.value
            member x.CreateRes(t) = call.GetValue t |> Prom.value
            member x.DestroyRes(_) = ()
        } :> IResource<_>


    member x.CreateProgram(signature : FramebufferSignature, code : string, iface : FShade.GLSL.GLSLProgramInterface) =
        let program = 
            programCache.GetOrCreate((signature, code), fun (signature, code) ->
                let program = ctx.CreateProgram(signature, code, iface)
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
