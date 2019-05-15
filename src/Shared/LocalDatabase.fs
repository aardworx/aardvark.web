namespace Aardvark.Data

open Fable.Core
open Aardvark.Base
open Aardvark.Import.JS
open Aardvark.Import.Browser

type LocalDatabase internal(refCache : int, name : string, tableName : string, db : IDBDatabase) =
    let mutable closed = false
    let refCache = LruCache<string, DbRef>(float refCache, Unchecked.hash, Unchecked.equals, fun r -> r |> Prom.bind (fun r -> r.Spill()))


    let mutable last = Prom.value ()
    let ret (write : bool) (f : IDBObjectStore -> Promise<'a>) =
        let v = 
            promise {
                do! last
                let trans = db.transaction(U2.Case1 tableName, (if write then "readwrite" else "readonly"))
                let s = trans.objectStore tableName
                return! f s
            }
        last <- unbox v
        v


    member x.Wait() =
        last

    member x.Close() = 
        promise {
            do! last
            if not closed then
                do! refCache.Clear() 
                db.close()
                closed <- true
        }
     

    member x.Delete() : Promise<unit> = 
        ret true <| fun store ->
            Prom.create (fun success error ->
                x.Close().``then`` (fun () ->
                    let request = indexedDB.deleteDatabase name
                    request.addEventListener_error(fun e ->
                        error e.error
                    )
                    request.addEventListener_success(fun e ->
                        success ()
                    )
                ) |> ignore
            )
        

    member x.TryGet(file : string) : Promise<Option<obj>> =
        ret false <| fun store ->
            Prom.create (fun success error ->
                if closed then 
                    error "closed"
                else
                    let request = store.get(file)
                    request.addEventListener_error(fun e ->
                        success None
                    )
                    request.addEventListener_success(fun e ->
                        success (Some request.result)
                    )
            )

    member x.Get(file : string) =
        ret false <| fun store ->
            Prom.create (fun success error ->
                if closed then 
                    error "closed"
                else
                    let request = store.get(file)
                    request.addEventListener_error(fun e ->
                        error e.error
                    )
                    request.addEventListener_success(fun e ->
                        success request.result
                    )
            )

    member x.Set(file : string, value : obj) =
        ret true <| fun store ->
            Prom.create (fun success error ->
                if closed then 
                    error "closed"
                else
                    let request = store.put(value, file)
                    request.addEventListener_error(fun e ->
                        error e.error
                    )
                    request.addEventListener_success(fun e ->
                        success ()
                    )
            )

    member x.Add(file : string, value : obj) =
        ret true <| fun store ->
            Prom.create (fun success error ->
                if closed then 
                    error "closed"
                else
                    let request = store.add(value, file)
                    request.addEventListener_error(fun e ->
                        error e.error
                    )
                    request.addEventListener_success(fun e ->
                        success ()
                    )
            )
    member x.Delete(file : string) =
        ret true <| fun store ->
            Prom.create (fun success error ->
                if closed then 
                    error "closed"
                else
                    let request = store.delete(file)
                    request.addEventListener_error(fun e ->
                        error e.error
                    )
                    request.addEventListener_success(fun e ->
                        success ()
                    )
            )

    member x.Clear() =
        ret true <| fun store ->
            Prom.create (fun success error ->
                if closed then 
                    error "closed"
                else
                    let request = store.clear()
                    request.addEventListener_error(fun e ->
                        error e.error
                    )
                    request.addEventListener_success(fun e ->
                        success ()
                    )
            )

    member x.Exists (file : string) =
        x.TryGet(file) |> Prom.map (function Some _ -> true | None -> false)

    member x.GetRef<'a>(name : string, pickle : 'a -> ArrayBuffer, unpickle : string -> ArrayBuffer -> Promise<'a>) : Promise<DbRef<'a>> =
        let res = 
            refCache.GetOrCreate(name, fun _ ->
                DbRef<'a>(x, refCache, name, None, pickle, unpickle name) :> DbRef |> Prom.value
            ) 
        unbox<Promise<DbRef<'a>>> res
        //if not (unbox res) then assert(false)
        //unbox<DbRef<'a>> res
    member x.Ref<'a>(name : string, value : 'a, pickle : 'a -> ArrayBuffer, unpickle : string -> ArrayBuffer -> Promise<'a>) =
        let res = 
            refCache.GetOrCreate(name, fun _ ->
                DbRef<'a>(x, refCache, name, Some value, pickle, unpickle name) :> DbRef |> Prom.value
            ) 
        unbox<Promise<DbRef<'a>>> res

    interface IBlobStore with
        member x.Get(file) = x.Get(file) |> unbox<Promise<ArrayBuffer>>
        member x.GetString(file) = x.Get(file) |> unbox<Promise<string>>

and DbRef =
    abstract member Spill : unit -> Promise<unit>

and DbRef<'a>(db : LocalDatabase, cache : LruCache<string, DbRef>, file : string, value : Option<'a>, pickle : ('a -> ArrayBuffer), unpickle : (ArrayBuffer -> Promise<'a>)) =
    let mutable pending = Prom.value()
    let mutable value = value
    let mutable dirty = match value with | Some _ -> true | None -> false

    let ret (f : unit -> Promise<'x>) =
        let v = pending |> Prom.bind(fun () -> db.Wait() |> Prom.bind f)
        pending <- unbox v
        v
    
    member x.Name = file

    member x.Cache = cache

    member x.Delete() =
        value <- None
        dirty <- false
        cache.Remove file
        db.Delete file


    member x.TryRead() =
        cache.Use(file, x)
        match value with
        | Some v -> 
            Prom.value (Some v)
        | None ->
            ret <| fun () ->
                promise {
                    match! db.TryGet file with
                    | Some r ->
                        let! (v : 'a) = unpickle (unbox r)
                        value <- Some v
                        return Some v
                    | None ->
                        return None
                }

    member x.Read() =
        if file = string System.Guid.Empty then failwith "asdasdsa"
        cache.Use(file, x)
        match value with
        | Some v -> 
            Prom.value v
        | None ->
            ret <| fun () ->
                promise {
                    match! db.TryGet file with
                    | Some r ->
                        let! (v : 'a) = unpickle (unbox r)
                        value <- Some v
                        return v
                    | None ->
                        return failwithf "bad file: %A" file
                }

    member x.Write(v : 'a) =
        cache.Use(file, x)
        value <- Some v
        dirty <- true

    member x.Spill() =
        if dirty then
            dirty <- false
            match value with
            | Some v -> 
                value <- None
                ret <| fun () ->
                    db.Set(file, pickle v)
            | None -> 
                Prom.value ()
        else
            Prom.value ()
    
    interface DbRef with
        member x.Spill() = x.Spill()

module LocalDatabase =
    open Microsoft.FSharp.Collections

    [<Literal>]
    let Version = 1.0

    module private Upgrade = 
        let private ofList (l : list<float * float * (IDBDatabase -> IDBTransaction -> unit)>) =
            l 
            |> List.groupBy (fun (s,_,_) -> s)
            |> List.map (fun (s,r) -> s, r |> List.map (fun (_,e,u) -> e, u) |> MapExt.ofList)
            |> MapExt.ofList

        let private upgrades =
            ofList [
                0.0, 1.0, fun db trans ->
                    db.createObjectStore("blobs") |> ignore
            ]
    
        let rec tryUpgrade (s : float) (e : float) (db : IDBDatabase) (trans : IDBTransaction) =
            if s = e then
                true
            else
                match MapExt.tryFind s upgrades with
                | Some upgrades ->
                    let (l, s, _) = MapExt.split e upgrades
                    match s with
                    | Some u -> 
                        u db trans
                        true
                    | None ->
                        match MapExt.toSeqBack l |> Seq.tryHead with
                        | Some (k, u) ->
                            u db trans
                            tryUpgrade k e db trans
                        | None ->
                            trans.abort()
                            db.close()
                            false
                    
                | None ->
                    trans.abort()
                    db.close()
                    false


    let destroy (name : string) =
        Prom.create (fun success error ->
            let request = indexedDB.deleteDatabase name
            request.addEventListener_error(fun e ->
                error e.error
            )
            request.addEventListener_success(fun e ->
                success ()
            )

        )

    type StorageEstimate  =
        abstract member quota : float
        abstract member usage : float

    type StorageManager  =
        abstract member estimate : unit -> Promise<StorageEstimate>
        abstract member persist : unit -> Promise<bool>
        abstract member persisted : unit -> Promise<bool>

    type Navigator with
        [<Emit("$0.storage")>]
        member x.storage : StorageManager = jsNative


    let connect (refCache : int) (name : string) =
        Prom.create (fun success error ->
            let conn = indexedDB.``open``(name, Version)
            
            if unbox navigator.storage then
                navigator.storage.estimate().``then``(fun e ->
                    Log.start "[IndexedDB] info"
                    Log.line "used:  %A (%.2f%%)"(Mem (int64 e.usage)) (100.0 * e.usage / e.quota)
                    Log.line "quota: %A"(Mem (int64 e.quota))
                    Log.stop()
                ) |> ignore

            conn.addEventListener_error(fun e ->
                let msg = sprintf "connection error: %A" conn.error
                error msg
            )

            conn.addEventListener_blocked (fun e ->
                console.warn "blocked"
            )

            conn.addEventListener_success (fun e ->
                let db = conn.result |> unbox<IDBDatabase>
                if unbox db then
                    success <| LocalDatabase(refCache, name, "blobs", db)
                else
                    error "no database"
            )

            conn.addEventListener_upgradeneeded (fun e ->
                let old = e.oldVersion
                let db = conn.result |> unbox<IDBDatabase>
                let trans = conn.transaction

                if Upgrade.tryUpgrade old Version db trans then
                    let prom =
                        Prom.create (fun success error ->
                            trans.addEventListener_complete (fun _ -> success())
                            trans.addEventListener_error (fun _ -> error "trans")
                            trans.addEventListener_abort (fun _ -> error "trans")
                        )

                    prom.``then`` (fun () -> success <| LocalDatabase(refCache, name, "blobs", db)) |> ignore
                else
                    error (sprintf "cannot upgrade db version %.0f" old)


            )
        
        )
        
    let inline clear (store : LocalDatabase) = store.Clear()
    let inline delete (name : string) (store : LocalDatabase) = store.Delete name
    let inline exists (name : string) (store : LocalDatabase) = store.Exists name
    let inline get (name : string) (store : LocalDatabase) = store.Get(name)
    let inline set (name : string) (value : 'a) (store : LocalDatabase) = store.Set(name, value :> obj)
    let inline tryGet (name : string) (store : LocalDatabase) = store.TryGet(name)
    