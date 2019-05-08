namespace Aardvark.Data

open Aardvark.Import.Browser
open Aardvark.Import.JS
open System
open Aardvark.Base
open Aardvark.Data
open Aardvark.Data.Durable
open Microsoft.FSharp.Collections
open Aardvark.SceneGraph
open Aardvark.Base.Rendering


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
        
type Octnode(db : Database, gzip: bool, dbid : Guid, level : int, m : Map<Def, obj>) =
    let mutable colorsRepaired = false
    let mutable currentOffset = V3d.Zero
    let mutable subNodes : Option<Promise<Octnode>>[] = null
    
    let bounds =
        match tryGet<Box3d> Octree.BoundingBoxExactGlobal m with
        | Some box -> box
        | None ->
            let cell = get<Cell> Octree.Cell m
            match tryGet<Box3d> Octree.BoundingBoxExactLocal m with
            | Some box -> Box3d(box.Min + cell.Center, box.Max + cell.Center)
            | None -> cell.BoundingBox


    let repairColors3 (b : C3bBuffer) =  
        if not colorsRepaired then
            colorsRepaired <- true
            let rgbs = Uint8Array.Create((b :> IArrayBuffer).Buffer, (b :> IArrayBuffer).ByteOffset, b.Length * 3)
            let mutable o = 0
            for i in 0 .. b.Length - 1 do
                let b = rgbs.[o+0]
                rgbs.[o+0] <- rgbs.[o+2]
                rgbs.[o+2] <- b
                o <- o + 3
        b

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

    member x.TryColors = tryGet<C3bBuffer> Octree.Colors3b m |> FSharp.Core.Option.map repairColors3

    member x.TryMinTreeDepth = tryGet<int> Octree.MinTreeDepth m
    member x.TryMaxTreeDepth = tryGet<int> Octree.MaxTreeDepth m
    member x.TryAvgPointDistance = tryGet<float32> Octree.AveragePointDistance m
    member x.TryAvgPointDistanceStdDev = tryGet<float32> Octree.AveragePointDistanceStdDev m
    member x.TryPointCountCell = tryGet<int> Octree.PointCountCell m
    member x.TryCell = tryGet<Cell> Octree.Cell m

    member x.SetOffset(offset : V3d) =
        //if currentOffset <> offset then
        let off = offset - currentOffset
        currentOffset <- offset
        let pos : V3fBuffer = x.PositionsLocal
        let float = Float32Array.Create((pos :> IArrayBuffer).Buffer, (pos :> IArrayBuffer).ByteOffset, pos.Length * 3)
        for i in 0 .. 3 .. float.length - 3 do
            float.[i+0] <- float.[i+0] + float32 off.X
            float.[i+1] <- float.[i+1] + float32 off.Y
            float.[i+2] <- float.[i+2] + float32 off.Z


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
    member x.PointCountCell = get<int> Octree.PointCountCell m
    member x.Cell = get<Cell> Octree.Cell m
    member x.BoundingBox = bounds
    member x.SubNodeIds =
        match tryGet<Guid[]> Octree.SubnodesGuids m with
        | Some arr -> arr
        | None -> [||]

    member x.SubNodes =
        match subNodes with
        | null ->
            let promises = 
                x.SubNodeIds |> Array.map (fun id ->
                    if id <> Guid.Empty then
                        let prom = 
                            db.Get(string id, gzip) |> Prom.map (fun (def, data) ->
                                if def <> Octree.Node then 
                                    Log.warn "unexpected data: %A" def
                                    Unchecked.defaultof<_>
                                else
                                    let d = unbox<Map<Def, obj>> data
                                    Octnode(db, gzip, id, level + 1, d)
                            )
                        Some prom
                    else
                        None
                )
            subNodes <- promises
            promises
        | ps ->
            ps

    member x.GetNodes(level : int) =
        if level <= 0 then
            Prom.value ([|x|])
        elif x.SubNodeIds.Length > 0 then
            x.SubNodes |> Array.choose id |> Prom.all |> Prom.bind (fun cs ->
                cs |> Seq.map (fun n -> n.GetNodes(level-1)) |> Prom.all |> Prom.map (Seq.concat >> Seq.toArray)
            )
        else
            Prom.value ([|x|])

type Octree(db : Database) =
    let mutable center = V3d.Zero
    let mutable stddev = 1.0

    let root = 
        db.GetString("root.json") |> Prom.bind (fun str ->
            let obj = JSON.parse str
            console.warn obj
            let id : Guid = Fable.Core.JsInterop.(?) obj "RootId"
            let gzip : bool = Fable.Core.JsInterop.(?) obj "GZipped"

            let cent = Fable.Core.JsInterop.(?) obj "Centroid"
            let centroidDev = Fable.Core.JsInterop.(?) obj "CentroidStdDev"


            db.Get(string id, gzip) |> Prom.map (fun (def, data) ->
                if def <> Octree.Node then 
                    Log.warn "unexpected data: %A" def
                    Unchecked.defaultof<_>
                else
                    let d = unbox<Map<Def, obj>> data
                    let root = Octnode(db, gzip, id, 0, d)

                    
                    if unbox cent then
                        center <- V3d(Fable.Core.JsInterop.(?) cent "X", Fable.Core.JsInterop.(?) cent "Y", Fable.Core.JsInterop.(?) cent "Z")
                        stddev <- centroidDev
                    else
                        let s = (root.BoundingBox.Size.X + root.BoundingBox.Size.Y + root.BoundingBox.Size.Z) / 3.0
                        center <- root.BoundingBox.Center
                        stddev <- s / 2.0
                    root

            )
        )

    member x.Center = center
    member x.StdDev = stddev
    member x.Root = root
    
    member x.GetNodes(level : int) =
        root |> Prom.bind (fun r -> r.GetNodes(level))
