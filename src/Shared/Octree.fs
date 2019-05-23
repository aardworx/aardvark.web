namespace Aardvark.Data

open Aardvark.Import.JS
open System
open Aardvark.Base
open Aardvark.Data
open Aardvark.Data.Durable
open Microsoft.FSharp.Collections


module HeatMap =
    let private heatMapColors =
        let fromInt (i : int) =
            V3d(
                float (byte ((i >>> 16) &&& 0xFF)) / 255.0,
                float (byte ((i >>> 8) &&& 0xFF)) / 255.0,
                float (byte (i &&& 0xFF)) / 255.0
            )

        Array.map fromInt [|
            0x1639fa
            0x2050fa
            0x3275fb
            0x459afa
            0x55bdfb
            0x67e1fc
            0x72f9f4
            0x72f8d3
            0x72f7ad
            0x71f787
            0x71f55f
            0x70f538
            0x74f530
            0x86f631
            0x9ff633
            0xbbf735
            0xd9f938
            0xf7fa3b
            0xfae238
            0xf4be31
            0xf29c2d
            0xee7627
            0xec5223
            0xeb3b22
        |]

    let color (tc : float) =
        let tc = max 0.0 (min 1.0 tc)
        let fid = tc * float heatMapColors.Length - 0.5

        let id = int (floor fid)
        if id < 0 then 
            heatMapColors.[0]
        elif id >= heatMapColors.Length - 1 then
            heatMapColors.[heatMapColors.Length - 1]
        else
            let c0 = heatMapColors.[id]
            let c1 = heatMapColors.[id + 1]
            let t = fid - float id
            (c0 * (1.0 - t) + c1 * t)





[<AutoOpen>]
module private OctHelpers = 
    let inline tryGet<'a> (def : Def) (m : Map<Def, obj>) =
        match Map.tryFind def m with
        //| Some (:? 'a as o) -> Some o
        //| Some o -> Log.warn "bad type for %A %A" def o; None
        | Some o -> Some (o :?> 'a)
        | _ -> None
    
    let inline get<'a> (def : Def) (m : Map<Def, obj>) =
        match Map.tryFind def m with
        | Some o -> o :?> 'a
        //| Some (:? 'a as o) -> o
        //| Some o -> Log.warn "bad type for %A %A" def o; failwithf "[Octree] could not get %s" def.Name
        | _ -> failwithf "[Octree] could not get %s" def.Name
     
     
    let repair (rootCenter : V3d) (def : Durable.Def) (o : obj) =
        if def = Durable.Octree.Node then
            let o = unbox<Map<Durable.Def, obj>> o


            match Map.tryFind Durable.Octree.PositionsLocal3f o with
            | Some pos -> 
                let cell = o.[Durable.Octree.Cell] |> unbox<Cell>
                let offset = cell.Center - rootCenter
                let pos = unbox<V3fBuffer> pos
                let float = Float32Array.Create((pos :> IArrayBuffer).Buffer, (pos :> IArrayBuffer).ByteOffset, pos.Length * 3)
                for i in 0 .. 3 .. float.length - 3 do
                    float.[i+0] <- float.[i+0] + float32 offset.X
                    float.[i+1] <- float.[i+1] + float32 offset.Y
                    float.[i+2] <- float.[i+2] + float32 offset.Z
            | None ->
                ()
                
            match Map.tryFind Durable.Octree.Colors3b o with
            | Some col -> 
                let col = unbox<C3bBuffer> col
                let rgb = Uint8Array.Create((col :> IArrayBuffer).Buffer, (col :> IArrayBuffer).ByteOffset, col.Length * 3)
                for i in 0 .. 3 .. rgb.length - 3 do
                    let t = rgb.[i+0]
                    rgb.[i+0] <- rgb.[i+2]
                    rgb.[i+2] <- t
            | None ->
                ()
        o

type Octnode(db : Database, dbid : Guid, level : int, rootCenter : V3d, m : Map<Def, obj>) =
    let bounds =
        match tryGet<Box3d> Octree.BoundingBoxExactGlobal m with
        | Some box -> box
        | None ->
            let cell = get<Cell> Octree.Cell m
            match tryGet<Box3d> Octree.BoundingBoxExactLocal m with
            | Some box -> Box3d(box.Min + cell.Center, box.Max + cell.Center)
            | None -> cell.BoundingBox

    member x.Data = m

    override x.GetHashCode() = dbid.GetHashCode()
    override x.Equals o =
        match o with
        | :? Octnode as o -> dbid = o.Id
        | _ -> false

    override x.ToString() =
        sprintf "%A(%d)" dbid level

    member x.Id = dbid 

    member x.TryPositionsLocal = tryGet<V3fBuffer> Octree.PositionsLocal3f m

    member x.TryNormals = 
        match tryGet<V3fBuffer> Octree.Normals3f m with
        | Some v -> Some v
        | None ->
            match tryGet<Int8Buffer> Octree.Normals3sb m with
            | Some b ->
                let v3 = V3fBuffer.init b.Length (fun i -> V3d(float b.[3*i+0] / 127.0, float b.[3*i+1] / 127.0, float b.[3*i+2] / 127.0).Normalized)
                Some v3
            | None ->
                None

    member x.TryColors = tryGet<C3bBuffer> Octree.Colors3b m 

    member x.TryMinTreeDepth = tryGet<int> Octree.MinTreeDepth m
    member x.TryMaxTreeDepth = tryGet<int> Octree.MaxTreeDepth m
    member x.TryAvgPointDistance = tryGet<float32> Octree.AveragePointDistance m
    member x.TryAvgPointDistanceStdDev = tryGet<float32> Octree.AveragePointDistanceStdDev m
    member x.TryPointCountCell = tryGet<int> Octree.PointCountCell m
    member x.TryCell = tryGet<Cell> Octree.Cell m

    //member x.SetOffset(offset : V3d) =
    //    //if currentOffset <> offset then
    //    let off = offset - currentOffset
    //    currentOffset <- offset
    //    let pos : V3fBuffer = x.PositionsLocal
    //    let float = Float32Array.Create((pos :> IArrayBuffer).Buffer, (pos :> IArrayBuffer).ByteOffset, pos.Length * 3)
    //    for i in 0 .. 3 .. float.length - 3 do
    //        float.[i+0] <- float.[i+0] + float32 off.X
    //        float.[i+1] <- float.[i+1] + float32 off.Y
    //        float.[i+2] <- float.[i+2] + float32 off.Z


    member x.PositionsLocal =
        match x.TryPositionsLocal with
        | Some v -> v
        | None -> failwith "[Octree] could not get positions"
        
    member x.Normals =
        match x.TryNormals with
        | Some v -> v
        | None -> failwith "[Octree] could not get normals"
        
    member x.Colors =
        match x.TryColors with
        | Some c -> 
            //let bits = Uint8Array.Create((c :> IArrayBuffer).Buffer, (c :> IArrayBuffer).ByteOffset)

            //let depth = x.MaxTreeDepth

            //for i in 0 .. 3 .. bits.length - 2 do
            //    let r = float bits.[i]   / 255.0
            //    let g = float bits.[i+1] / 255.0
            //    let b = float bits.[i+2] / 255.0
            //    let c = 0.5 * (HeatMap.color (float depth / 7.0) + V3d(r,g,b))

            //    bits.[i] <- byte (255.0 * c.X)
            //    bits.[i+1] <- byte (255.0 * c.Y)
            //    bits.[i+2] <- byte (255.0 * c.Z)
            //    ()


            c
        | None -> failwith "[Octree] could not get colors"


    member x.MinTreeDepth = get<int> Octree.MinTreeDepth m
    member x.MaxTreeDepth = get<int> Octree.MaxTreeDepth m
    member x.AvgPointDistance = get<float32> Octree.AveragePointDistance m
    member x.AvgPointDistanceStdDev = get<float32> Octree.AveragePointDistanceStdDev m
    member x.PointCountCell = 
        match tryGet<int> Octree.PointCountCell m with
        | Some cnt -> cnt
        | None -> x.PositionsLocal.Length
    member x.Cell = get<Cell> Octree.Cell m
    member x.BoundingBox = bounds
    member x.SubNodeIds =
        match tryGet<Guid[]> Octree.SubnodesGuids m with
        | Some arr -> arr
        | None -> [||]

    member x.SubNodes =
        let promises = 
            x.SubNodeIds |> Array.map (fun id ->
                if id <> Guid.Empty then
                    let prom = 
                        db.ReadDurable(string id, repair rootCenter) |> Prom.map (fun (def, data) ->
                            if def <> Octree.Node then 
                                Log.warn "unexpected data: %A" def
                                Unchecked.defaultof<_>
                            else
                                let d = unbox<Map<Def, obj>> data
                                Octnode(db, id, level + 1, rootCenter, d)
                        )
                    Some prom
                else
                    None
            )
        promises

    member x.GetNodes(level : int) =
        if level <= 0 then
            Prom.value ([|x|])
        elif x.SubNodeIds.Length > 0 then
            x.SubNodes |> Array.choose id |> Prom.all |> Prom.bind (fun cs ->
                cs |> Seq.map (fun n -> n.GetNodes(level-1)) |> Prom.all |> Prom.map (Seq.concat >> Seq.toArray)
            )
        else
            Prom.value ([|x|])

type Octree(store : IBlobStore) =
    let mutable center = V3d.Zero
    let mutable stddev = 1.0

    let root = 
        store.GetString("root.json") |> Prom.bind (fun str ->
            let obj = JSON.parse str
            //console.warn obj
            let id : Guid = Fable.Core.JsInterop.(?) obj "RootId"
            let gzip : bool = Fable.Core.JsInterop.(?) obj "GZipped"

            let cent = Fable.Core.JsInterop.(?) obj "Centroid"
            let centroidDev = Fable.Core.JsInterop.(?) obj "CentroidStdDev"

            let bounds = 
                let b = Fable.Core.JsInterop.(?) obj "Bounds"
                let l = Fable.Core.JsInterop.(?) b "Min"
                let h = Fable.Core.JsInterop.(?) b "Max"
                Box3d(
                    V3d(Fable.Core.JsInterop.(?) l "X", Fable.Core.JsInterop.(?) l "Y", Fable.Core.JsInterop.(?) l "Z"),
                    V3d(Fable.Core.JsInterop.(?) h "X", Fable.Core.JsInterop.(?) h "Y", Fable.Core.JsInterop.(?) h "Z")
                )


            if unbox cent then
                center <- V3d(Fable.Core.JsInterop.(?) cent "X", Fable.Core.JsInterop.(?) cent "Y", Fable.Core.JsInterop.(?) cent "Z")
                stddev <- centroidDev
            else
                center <- bounds.Center
                stddev <- max bounds.Size.X (max bounds.Size.Y bounds.Size.Z)

            let db = Database(store, compressed = gzip)
            db.ReadDurable(string id, repair center) |> Prom.map (fun (def, data) ->
                if def <> Octree.Node then 
                    Log.warn "unexpected data: %A" def
                    Unchecked.defaultof<_>
                else
                    Octnode(db, id, 0, center, unbox<Map<Def, obj>> data)

            )
        )

    member x.Center = center
    member x.StdDev = stddev
    member x.Root = root
    
    member x.GetNodes(level : int) =
        root |> Prom.bind (fun r -> r.GetNodes(level))
