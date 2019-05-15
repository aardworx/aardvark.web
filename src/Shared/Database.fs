namespace Aardvark.Data


open Aardvark.Import.JS
open Microsoft.FSharp.Collections
open System
open Aardvark.Base

type pako =
    abstract member deflate : Uint8Array -> Uint8Array 
    abstract member inflate : Uint8Array -> Uint8Array

[<AutoOpen>]
module GlobalThings =
    open Fable.Core
    let [<Import("*", "pako")>] pako : pako = jsNative


type IBlobStore =
    abstract member GetString : file : string -> Promise<string>
    abstract member Get : file : string -> Promise<ArrayBuffer>

type HttpBlobStore(urlFormat : string) =
    member x.Get(file : string) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchBuffer url
    member x.GetString(file : string) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchString url
        
    interface IBlobStore with
        member x.Get(file) = x.Get file
        member x.GetString(file) = x.GetString file




[<AllowNullLiteral>]
type LruNode<'k, 'v>(key : 'k, value : Promise<'v>) =
    let mutable prev : LruNode<'k, 'v> = null
    let mutable next : LruNode<'k, 'v> = null
    
    member x.Prev
        with get() = prev
        and set p = prev <- p

    member x.Next
        with get() = next
        and set n = next <- n

    member x.Key = key
    member x.Value = value

type LruCache<'k, 'v>(capacity : float, hash : 'k -> int, equals : 'k -> 'k -> bool, ?destroy : Promise<'v> -> Promise<unit>) =
    let nodes = Dict<'k, LruNode<'k, 'v>>(hash, equals)

    let mutable first : LruNode<'k, 'v> = null
    let mutable last : LruNode<'k, 'v> = null
    let mutable currentCount = 0.0
    let mutable lastOp = Prom.value()

    let moveToFront (node : LruNode<'k, 'v>) =
        if unbox node.Prev then
            node.Prev.Next <- node.Next
            if unbox node.Next then node.Next.Prev <- node.Prev
            else last <- node.Prev

            node.Prev <- null
            node.Next <- first
            if unbox first then first.Prev <- node
            else last <- node
            first <- node
        
    let clean (capacity : float) =
        let p = 
            promise {
                do! lastOp
                while unbox last && currentCount > capacity do
                    let n = last
                    nodes.Remove n.Key |> ignore
                    if unbox n.Prev then n.Prev.Next <- null
                    else first <- null

                    // destroy the node
                    assert (not (unbox n.Next))
                    last <- n.Prev
                    n.Prev <- null

                    match destroy with
                    | Some d -> 
                        do! d n.Value
                    | None -> 
                        ()

                    currentCount <- currentCount - 1.0
            }
        lastOp <- p
        p
        //lastOp <- Prom.all all |> unbox<Promise<unit>>

    member x.Use (key : 'k, value : 'v) =
        let node = 
            nodes.GetOrCreate(key, fun key ->
                let n = LruNode(key, Prom.value value)

                if unbox first then first.Prev <- n
                else last <- n // empty

                n.Next <- first
                first <- n
                currentCount <- currentCount + 1.0
                n
            )

        moveToFront node

    member x.Remove (key : 'k) =
        match nodes.TryRemove key with
        | Some node ->
            let p = node.Prev
            let n = node.Next

            if unbox p then p.Next <- n
            else first <- n
            if unbox n then n.Prev <- p
            else last <- p
            currentCount <- currentCount - 1.0
        | None ->
            ()

    member x.GetOrCreate(key : 'k, creator : 'k -> Promise<'v>) =
        promise {
            let node = 
                nodes.GetOrCreate(key, fun key ->
                    let value = creator key
                    let n = LruNode(key, value)

                    if unbox first then first.Prev <- n
                    else last <- n // empty

                    n.Next <- first
                    first <- n
                    currentCount <- currentCount + 1.0
                    n
                )

            moveToFront node
            do! clean(capacity)
            let! v = node.Value
            return v
        }

    
    member x.GetOrCreateValue(key : 'k, creator : 'k -> 'v) =
        let node = 
            nodes.GetOrCreate(key, fun key ->
                let value = creator key
                let n = LruNode(key, Prom.value value)

                if unbox first then first.Prev <- n
                else last <- n // empty

                n.Next <- first
                first <- n
                currentCount <- currentCount + 1.0
                n
            )

        moveToFront node
        clean(capacity) |> ignore
        let v = Fable.Core.JsInterop.(?) node.Value "resolved" |> unbox<'v>
        v

    member x.Clear() =
        clean 0.0 |> Prom.map (fun () ->
            nodes.Clear()
            first <- null
            last <- null
            currentCount <- 0.0
        )

    member x.TryGet(key : 'k) =
        match nodes.TryGetValue key with
        | Some node ->
            moveToFront node
            Some node.Value
        | None ->
            None

    member x.Get(key : 'k) =
        match nodes.TryGetValue key with
        | Some node ->
            moveToFront node
            node.Value
        | None ->
            failwithf "[LRU] cannot get unknown key %A" key






type Database(store : IBlobStore, cacheCapacity : float) =
    let cache = LruCache<string, obj>(cacheCapacity, Unchecked.hash, Unchecked.equals)

    member x.GetString(file : string) =
        cache.GetOrCreate(file, fun file ->
            store.GetString file |> unbox
        ) |> unbox<Promise<string>>

    member x.Get(file : string, gzip : bool, repair : Durable.Def -> obj -> obj) =
        cache.GetOrCreate(file, fun file ->
            store.Get file |> Prom.map (fun data ->
                let data =
                    if gzip then pako.inflate(Uint8Array.Create data).buffer
                    else data

                let s = Aardvark.Data.Stream(data)
                let (def, o) = Aardvark.Data.DurableDataCodec.decode s
                if def = Durable.Octree.Node then
                    let o = o |> unbox |> Map.add Durable.Octree.Buffer (data :> obj)
                    let o = repair def o
                    (def, o) :> obj
                else
                    let o = repair def o
                    (def, o) :> obj
            )
        ) |> unbox<Promise<Durable.Def * obj>>

    member inline x.TryGet<'a>(file : string, gzip : bool, repair : Durable.Def -> obj -> obj) =
        x.Get(file, gzip, repair) |> Prom.map (fun (def,o) ->
            match o with
            | :? 'a as o -> Some o
            | _ -> None
        )

    new(urlFormat : string, cacheCapacity : float) = new Database(HttpBlobStore(urlFormat), cacheCapacity)