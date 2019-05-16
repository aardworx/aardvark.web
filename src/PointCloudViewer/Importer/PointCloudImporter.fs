module PointCloudImporter

open Fable.Core
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Base
open Aardvark.Data

type Command =
    | Import of file : File * storeName : string * overwrite : bool * format : Ascii.Token[] * splitLimit : int * compress : bool

type Reply =
    | Error of file : File * json : string
    | Progress of file : File * totalSize : float * size : float * time : float
    | Done of file : File * pointCount : float

    member x.File =
        match x with
        | Error(f,_) -> f
        | Progress(f,_,_,_) -> f
        | Done(f,_) -> f



type PointCloudInfo =
    {
        RootId      : System.Guid
        PointCount  : float
        Bounds      : Box3d
        Cell        : Cell
    }

module PointCloudInfo =
    open Fable.Core.JsInterop

    let tryOfJson (json : string) =
        try
            let o = JSON.parse json

            if o?RootId && o?Bounds && o?Cell then
                let res = 
                    {
                        RootId = o?RootId
                        PointCount = o?PointCount
                        Bounds = Box3d(V3d(o?Bounds?Min?X, o?Bounds?Min?Y, o?Bounds?Min?Z), V3d(o?Bounds?Max?X, o?Bounds?Max?Y, o?Bounds?Max?Z))
                        Cell = Cell(o?Cell?X, o?Cell?Y, o?Cell?Z, o?Cell?E)
                    }
                Some res
            else
                None

        with _ ->
            None




let tryGetPointCloudInfo (store : IBlobStore) =
    promise {
        try
            let! root = store.GetString "root.json"
            return PointCloudInfo.tryOfJson root
        with _ ->
            return None
    }

let execute (postMessage : obj -> unit) (cmd : Command) =
    match cmd with
    | Import(file, storeName, overwrite, format, splitLimit, compress) ->
        promise {
            if overwrite then 
                do! LocalBlobStore.destroy storeName

            let! store = BlobStore.local storeName
            match! tryGetPointCloudInfo store with
            | Some info ->
                do! store.Close()
                postMessage (Reply.Done(file, info.PointCount))
            | None -> 
                try
                    let db = Database(store, compressed = compress)

                    let progress (totalSize : float) (size : float) (time : float) =
                        postMessage (Reply.Progress(file, totalSize, size, time))

                    let cfg =
                        {   
                            Ascii.overwrite = false
                            Ascii.format = format
                            Ascii.splitLimit = splitLimit
                            Ascii.progress = progress
                        }

                    let! tree = file |> MutableOctree.importAscii cfg db
                    let pointCount = 
                        match tree.Root with
                        | Some r -> r.TotalPointCount
                        | None -> 0.0
                
                    do! db.Close()
                    postMessage (Reply.Done(file, pointCount))
                with e ->
                    do! store.Delete()
                    postMessage (Reply.Error (file, JSON.stringify e))
        } |> ignore

let importAscii (cfg : Ascii.ImportConfig) (file : File) =
    Promise.Create (fun success error ->
        let w = Worker.Create("importer.js")
        let storeName = file.name

        w.addEventListener_message(fun msg ->
            let msg = unbox<Reply> msg.data
            if msg.File.name = file.name then
                match msg with
                | Reply.Error(_,e) -> 
                    error (JSON.parse e)

                | Reply.Progress (_, t, s, time) -> 
                    cfg.progress t s time

                | Reply.Done(_, cnt) -> 
                    w.terminate()
                    success storeName
        )

        w.postMessage (Command.Import(file, storeName, cfg.overwrite, cfg.format, cfg.splitLimit, false))
    ) |> unbox<Promise<string>>