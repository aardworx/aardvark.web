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

type LruCache<'k, 'v>(capacity : float, hash : 'k -> int, equals : 'k -> 'k -> bool) =
    let nodes = Dict<'k, LruNode<'k, 'v>>(hash, equals)

    let mutable first : LruNode<'k, 'v> = null
    let mutable last : LruNode<'k, 'v> = null
    let mutable currentCount = 0.0

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
        
    let clean () =
        while unbox last && currentCount > capacity do
            let n = last
            nodes.Remove n.Key |> ignore
            if unbox n.Prev then n.Prev.Next <- null
            else first <- null

            // destroy the node
            assert (not (unbox n.Next))
            last <- n.Prev
            n.Prev <- null

            currentCount <- currentCount - 1.0


    member x.GetOrCreate(key : 'k, creator : 'k -> Promise<'v>) =
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
        clean()
        node.Value

    member x.Clear() =
        nodes.Clear()
        first <- null
        last <- null
        currentCount <- 0.0

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






type Database(urlFormat : string, cacheCapacity : float) =
    let cache = LruCache<string, obj>(cacheCapacity, Unchecked.hash, Unchecked.equals)

    member x.GetString(file : string) =
        cache.GetOrCreate(file, fun file ->
            let url = System.String.Format(urlFormat, file)
            Prom.fetchBuffer url |> Prom.map (fun data ->
                let arr = Uint8Array.Create(data, 0, data.byteLength)
                System.Text.Encoding.UTF8.GetString (unbox<byte[]> arr) :> obj
            )
        ) |> unbox<Promise<string>>

    member x.Get(file : string, gzip : bool, repair : Durable.Def -> obj -> obj) =
        cache.GetOrCreate(file, fun file ->
            let url = System.String.Format(urlFormat, file)
            Prom.fetchBuffer url |> Prom.map (fun data ->
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

