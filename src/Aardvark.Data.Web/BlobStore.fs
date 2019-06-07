namespace Aardvark.Data

open Fable.Core
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Base

type IBlobStore =
    abstract member CanWrite : bool
    abstract member GetString : file : string -> Promise<string>
    abstract member Get : file : string -> Promise<ArrayBuffer>
    abstract member Set : file : string * value : ArrayBuffer -> Promise<unit>
    abstract member SetString : file : string * value : string -> Promise<unit>
    abstract member Close : unit -> Promise<unit>
    abstract member Delete : unit -> Promise<unit>

type HttpBlobStore(urlFormat : string) =
    member x.Get(file : string) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchBuffer url

    member x.GetString(file : string) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchString url

    member x.Set(_file, _data) : Promise<unit> = failwith "cannot write"
    member x.SetString(_file, _data) : Promise<unit> = failwith "cannot write"
        
    interface IBlobStore with
        member x.CanWrite = false
        member x.Set(file, data) = x.Set(file, data)
        member x.SetString(file, data) = x.SetString(file, data)
        member x.Get(file) = x.Get file
        member x.GetString(file) = x.GetString file
        member x.Close() = Prom.value ()
        member x.Delete() = Prom.value ()

type LocalBlobStore internal(name : string, tableName : string, db : IDBDatabase) =
    let mutable closed = false

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

    member x.GetFiles() =
        ret false <| fun store ->
            Prom.create (fun success error ->
                if closed then 
                    error "closed"
                else
                    let request = store.openCursor()
                    let res = System.Collections.Generic.List<string>()

                    request.addEventListener_error(fun e ->
                        error e.error
                    )
                    request.addEventListener_success(fun e ->
                        let cursor : IDBCursor = Fable.Core.JsInterop.(?) e.target "result"
                        if unbox cursor then 
                            if unbox cursor.key then  
                                match cursor.key with
                                | :? string as key -> res.Add key
                                | _ ->  ()
                                cursor.``continue``()
                            else
                                success (unbox<string[]> res)
                        else
                            success (unbox<string[]> res)
                    )
            )

    interface IBlobStore with
        member x.CanWrite = true
        member x.Get(file) = x.Get(file) |> unbox<Promise<ArrayBuffer>>
        member x.GetString(file) = x.Get(file) |> unbox<Promise<string>>
        member x.Set(file, data) = x.Set(file, data)
        member x.SetString(file, data) = x.Set(file, data)
        member x.Close() = x.Close()
        member x.Delete() = x.Delete()

module LocalBlobStore = 
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


    let connect (name : string) =
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
                    success <| LocalBlobStore(name, "blobs", db)
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

                    prom.``then`` (fun () -> success <| LocalBlobStore(name, "blobs", db)) |> ignore
                else
                    error (sprintf "cannot upgrade db version %.0f" old)


            )
        
        )
        
    let inline clear (store : LocalBlobStore) = store.Clear()
    let inline delete (name : string) (store : LocalBlobStore) = store.Delete name
    let inline exists (name : string) (store : LocalBlobStore) = store.Exists name
    let inline get (name : string) (store : LocalBlobStore) = store.Get(name)
    let inline set (name : string) (value : 'a) (store : LocalBlobStore) = store.Set(name, value :> obj)
    let inline tryGet (name : string) (store : LocalBlobStore) = store.TryGet(name)
 
module BlobStore =  
    let local (name : string) = LocalBlobStore.connect name |> unbox<Promise<IBlobStore>>
    let get (url : string) = 
        if url.StartsWith "local://" then
            url.Substring(8) |> local
        else
            HttpBlobStore(url) :> IBlobStore |> Prom.value

    let inline readString (file : string) (store : IBlobStore) = store.GetString file
    let inline read (file : string) (store : IBlobStore) = store.Get file
    let inline writeString (file : string) (value : string) (store : IBlobStore) = store.SetString(file, value)
    let inline write (file : string) (data : ArrayBuffer) (store : IBlobStore) = store.Set(file, data)
