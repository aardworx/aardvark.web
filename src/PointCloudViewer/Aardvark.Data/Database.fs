namespace Aardvark.Data

open Aardvark.Base
open Aardvark.Import.JS
open Microsoft.FSharp.Collections


[<AutoOpen>]
module CompressionThings =
    open Fable.Core

    type pako =
        abstract member deflate : Uint8Array -> Uint8Array 
        abstract member inflate : Uint8Array -> Uint8Array

    let [<Import("*", "pako")>] pako : pako = jsNative

[<AllowNullLiteral>]
type QuerablePromise<'a> =
    inherit Promise<'a>
    abstract member tryGetError : unit -> Option<obj>
    abstract member tryGetResult : unit -> Option<'a>

module QuerablePromise =

    let value (v : 'a) =
        let res = Promise.resolve(v)
        Fable.Core.JsInterop.(?<-) res "tryGetError" (fun () -> None)
        Fable.Core.JsInterop.(?<-) res "tryGetResult" (fun () -> Some v)
        unbox<QuerablePromise<'a>> res

    let ofPromise (p : Promise<'a>) : QuerablePromise<'a> =
        if Fable.Core.JsInterop.(?) p "tryGetResult" then
            unbox p
        else
            let mutable error = None
            let mutable result = None

            let resolve (v : 'a) =
                result <- Some v
                v
            let reject (o : obj) : 'a =
                error <- Some o
                raise (unbox o)

            let res = p.``then``(resolve, reject)

            Fable.Core.JsInterop.(?<-) res "tryGetError" (fun () -> error)
            Fable.Core.JsInterop.(?<-) res "tryGetResult" (fun () -> result)

            unbox<QuerablePromise<'a>> res



type Database(store : IBlobStore, ?cacheCapacity : float, ?compressed : bool) =
    let cacheCapacity = defaultArg cacheCapacity 2048.0
    let compressed = defaultArg compressed false
    let cache = LruCache<string, DbRef>(cacheCapacity, Unchecked.hash, Unchecked.equals, Prom.bind (fun r -> r.Spill()))

    static let deflate (data : ArrayBuffer) = pako.deflate(Uint8Array.Create data).buffer
    static let inflate (data : ArrayBuffer) = pako.inflate(Uint8Array.Create data).buffer

    static let pickleDurable (d : Durable.Def, data : obj) =
        let s = Stream()
        DurableDataCodec.encode s d data
        s.ToArrayBuffer() |> Prom.value

    static let unpickleDurable (_name : string) (data : ArrayBuffer) =
        let s = Stream(data)
        let t = DurableDataCodec.decode s
        Prom.value t

    member x.Flush() =
        cache.Clear()

    member x.Close() =
        promise {
            do! cache.Clear()
            do! store.Close()
        }

    member internal x.Cache = cache

    member x.Compressed = compressed

    member x.Store = store

    member x.GetStringRef(file : string) =
        cache.GetOrCreate(file, fun file ->
            let write (store : IBlobStore) (v : string) = 
                store.SetString(file, v) 

            let read (store : IBlobStore) (_name : string) = 
                store.GetString(file) 

            DbRef<string>(x, file, None, write, read) :> DbRef |> Prom.value
        ) |> unbox<Promise<DbRef<string>>>
        
    member x.StringRef(file : string, value : string) =
        x.GetStringRef file |> Prom.map (fun r ->
            r.Write value
            r
        )

    member x.GetRef(file : string, pickle : 'a -> Promise<ArrayBuffer>, unpickle : string -> ArrayBuffer -> Promise<'a>) =
        cache.GetOrCreate(file, fun file ->
            let write (store : IBlobStore) (v : 'a) = 
                pickle v |> Prom.bind (fun data -> 
                    let data = if compressed then deflate data else data
                    store.Set(file, data)
                )
            let read (store : IBlobStore) (name : string) =
                store.Get(file) |> Prom.bind (fun data ->
                    let data = if compressed then inflate data else data
                    unpickle name data
                )
            DbRef<'a>(x, file, None, write, read) :> DbRef |> Prom.value
        ) |> unbox<Promise<DbRef<'a>>>

    member x.Ref(file : string, value : 'a, pickle : 'a -> Promise<ArrayBuffer>, unpickle : string -> ArrayBuffer -> Promise<'a>) =
        x.GetRef(file, pickle, unpickle) |> Prom.map (fun r ->
            r.Write value
            r
        )

    member x.GetDurableRef(file : string) =
        x.GetRef(file, pickleDurable, unpickleDurable)
        
    member x.DurableRef(file : string, def : Durable.Def, value : obj) =
        x.Ref(file, (def, value), pickleDurable, unpickleDurable)

    member x.ReadDurable(file : string, repair : Durable.Def -> obj -> obj) : Promise<Durable.Def * obj> =
        let read (name : string) (data : ArrayBuffer) =
            promise {
                let! (def, value) = unpickleDurable name data

                let value = 
                    if def.Type = Durable.Primitives.DurableMap.Id then
                        Map.add Durable.Octree.Buffer (data :> obj) (unbox value) :> obj
                    else
                        value

                return def, repair def value
            }
        x.GetRef(file, pickleDurable, read) |> Prom.bind (fun r -> r.Read())
        
    member x.WriteDurable(file : string, def : Durable.Def, data : obj) : Promise<unit> =
        x.GetRef(file, pickleDurable, unpickleDurable) |> Prom.map (fun r -> r.Write((def,data)))

and [<AbstractClass>] DbRef internal (db : Database, name : string) =
    member x.Database : Database = db
    member x.Name : string = name
    abstract member Spill : unit -> Promise<unit>
    abstract member Size : Option<int>

and DbRef<'a> internal (db : Database, name : string, cache : Option<'a>, write : IBlobStore -> 'a -> Promise<unit>, read : IBlobStore -> string -> Promise<'a>, ?sizeof : 'a -> int) =
    inherit DbRef(db, name)

    let sizeof = defaultArg sizeof (fun _ -> 1)
    let mutable dirty = match cache with | Some _ -> true | None -> false
    let mutable cache : QuerablePromise<'a> = match cache with | Some v -> QuerablePromise.value v | _ -> null

    override x.Size =
        if unbox cache then
            match cache.tryGetResult() with
            | Some r -> Some (sizeof r)
            | None -> None
        else
            None

    override x.Spill() : Promise<unit> =
        if dirty && unbox cache then
            dirty <- false
            let v = cache
            cache <- null
            promise {
                let! value = v
                do! write db.Store value
            }
        else
            Prom.value ()

    member x.Read() : Promise<'a> =
        db.Cache.Use(name, x)
        if unbox cache then
            cache :> Promise<_>
        else
            let res = read db.Store name |> QuerablePromise.ofPromise
            cache <- res
            res :> Promise<_>

    member x.Write(value : 'a) : unit =
        db.Cache.Use(name, x)
        cache <- QuerablePromise.value value
        dirty <- true


