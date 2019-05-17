module PointCloudImporter

open Fable.Core
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Base
open Aardvark.Data

[<RequireQualifiedAccess>]
type Import =
    | Ascii of file : File * format : Ascii.Token[]

    static member Pts(file : File) =
        Import.Ascii(file, [| Ascii.Token.PositionX; Ascii.Token.PositionY; Ascii.Token.PositionZ; Ascii.Token.Skip; Ascii.Token.BlueByte; Ascii.Token.GreenByte; Ascii.Token.RedByte |])
        
    static member Xyz(file : File) =
        Import.Ascii(file, [| Ascii.Token.PositionX; Ascii.Token.PositionY; Ascii.Token.PositionZ |])

    static member XyzRgb(file : File) =
        Import.Ascii(file, [| Ascii.Token.PositionX; Ascii.Token.PositionY; Ascii.Token.PositionZ; Ascii.Token.BlueByte; Ascii.Token.GreenByte; Ascii.Token.RedByte |])
    
type ImportConfig =
    {
        store       : string
        overwrite   : bool
        splitLimit  : int
        compress    : bool
        progress    : float -> float -> float -> unit
    }
    
[<RequireQualifiedAccess>]
type Command =
    | Import of config : ImportConfig * data : Import
    
[<RequireQualifiedAccess>]
type Reply =
    | Error of name : string * json : string
    | Progress of name : string * totalSize : float * size : float * time : float
    | Done of name : string * pointCount : float

    member x.Name =
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
    | Command.Import(config, cmd) ->
        let name = config.store
        promise {
            if config.overwrite then 
                do! LocalBlobStore.destroy config.store

            let! store = BlobStore.local config.store
            match! tryGetPointCloudInfo store with
            | Some info ->
                do! store.Close()
                postMessage (Reply.Done(name, info.PointCount))
            | None -> 
                try
                    let db = Database(store, compressed = config.compress)

                    let progress (totalSize : float) (size : float) (time : float) =
                        postMessage (Reply.Progress(name, totalSize, size, time))

                    let! tree = 
                        match cmd with
                        | Import.Ascii(file, format) ->
                            let cfg =
                                {   
                                    Ascii.overwrite = false
                                    Ascii.format = format
                                    Ascii.splitLimit = config.splitLimit
                                    Ascii.progress = progress
                                }

                            file |> MutableOctree.importAscii cfg db



                    let pointCount = 
                        match tree.Root with
                        | Some r -> r.TotalPointCount
                        | None -> 0.0
                
                    do! db.Flush()
                    do! db.Close()
                    postMessage (Reply.Done(name, pointCount))
                with e ->
                    do! store.Delete()
                    postMessage (Reply.Error (name, JSON.stringify e))
        } |> ignore


type CancelablePromise<'a> =
    inherit Promise<'a>
    abstract member cancel : unit -> unit

module CancelablePromise =


    let ofPromise (p : unit -> (unit -> unit) * Promise<'a>) =
        let cancel, p = p()
        Fable.Core.JsInterop.(?<-) p "cancel" cancel
        p |> unbox<CancelablePromise<'a>>

    let create (conts : ('a -> unit) -> (obj -> unit) -> obj) =
        let mutable kill = fun () -> ()
        let res = 
            Promise.Create(fun success error ->
                let cancel = conts (fun v -> success (v :> obj)) error
                kill <- unbox<unit -> unit> cancel
            ) |> unbox<Promise<'a>>
        Fable.Core.JsInterop.(?<-) res "cancel" (fun () -> kill())
        res |> unbox<CancelablePromise<'a>>


let import (cfg : ImportConfig) (data : Import) =
    
    CancelablePromise.create (fun success error ->
        let w = Worker.Create("importer.js")
        let storeName = cfg.store
        let mutable finished = false

        let success v =
            if not finished then
                w.terminate()
                finished <- true
                success v
            
        let error v =
            if not finished then
                w.terminate()
                finished <- true
                let r = indexedDB.deleteDatabase(cfg.store)
                r.addEventListener_success(fun _ ->
                    error v
                )
                r.addEventListener_error(fun _ ->
                    error v
                )

        w.addEventListener_message(fun msg ->
            let msg = unbox<Reply> msg.data
            if msg.Name = storeName then
                match msg with
                | Reply.Error(_,e) -> 
                    error (JSON.parse e)

                | Reply.Progress (_, t, s, time) -> 
                    cfg.progress t s time

                | Reply.Done(_, cnt) -> 
                    success cnt
        )

        let rcfg = { cfg with progress = Unchecked.defaultof<_> }
        w.postMessage (Command.Import(rcfg, data))

        (fun () -> error "cancel") :> obj
    ) 