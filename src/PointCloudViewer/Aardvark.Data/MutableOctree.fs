namespace Aardvark.Data

open Aardvark.Import.JS
open Aardvark.Base
open Microsoft.FSharp.Collections
open Aardvark.Data

type MutableOctnode(db : Database, id : System.Guid, cell : Cell, splitLimit : int) =

    static let ref (db : Database) (splitLimit : int) (n : MutableOctnode) = db.Ref(string n.Id, n, MutableOctnode.pickle, MutableOctnode.unpickle db splitLimit)

    let mutable children : Option<DbRef<MutableOctnode>>[] = [||]
    let mutable data : Map<Durable.Def, obj> = Map.empty

    let mutable bounds = Box3d.Invalid
    let mutable cellCount = 0
    let mutable totalCount = 0.0
    
    static let createList (def : Durable.Def) (cap : int) =
        if def = Durable.Octree.PositionsLocal3f then V3fList(cap) :> obj
        elif def = Durable.Octree.Colors3b then Uint8List(3 * cap) :> obj
        else failwithf "bad attribute: %A" def
        
    static let append (posOffset : V3d) (def : Durable.Def) (idx : IArrayBuffer<int>) (src : obj) (dst : obj) =
        if def = Durable.Octree.PositionsLocal3f then
            let src = unbox<IArrayBuffer<V3d>> src
            let dst = unbox<V3fList> dst
            for i in 0 .. idx.Length - 1 do
                let s = src.Get (idx.Get i)
                dst.Add (s.X + posOffset.X, s.Y + posOffset.Y, s.Z + posOffset.Z)
        elif def = Durable.Octree.Colors3b then
            let src = unbox<IArrayBuffer<uint8>> src
            let dst = unbox<Uint8List> dst
            for i in 0 .. idx.Length - 1 do
                let i = 3 * idx.Get i
                dst.Add (src.Get i)
                dst.Add (src.Get (i + 1))
                dst.Add (src.Get (i + 2))
        else
            failwithf "bad attribute: %A" def

    static let partition (cell : Cell) (bb : Box3d) (index : IArrayBuffer<int>) (ps : IArrayBuffer<V3d>) =
        let center = cell.Center
        if (bb.Min.X >= center.X || bb.Max.X <= center.X) &&
           (bb.Min.Y >= center.Y || bb.Max.Y <= center.Y) && 
           (bb.Min.Z >= center.Z || bb.Max.Z <= center.Z) then

            let o = cell.GetOctant bb.Center
            FSharp.Collections.Array.init 8 (fun i ->
                if i = o then
                    let index = 
                        match index with
                        | :? Int32List as o -> o
                        | _ ->
                            let l = Int32List()
                            l.AddRange index
                            l
                    bb, index
                else
                    Box3d.Invalid, Int32List()
            )

        else

            let ri = FSharp.Collections.Array.init 8 (fun _ -> Box3d.Invalid, Int32List(index.Length))
            let mutable ci = 0
            for i in 0 .. index.Length - 1 do   
                let index = index.Get i
                let p = ps.Get index
                let o = cell.GetOctant p
                let (bb, r) = ri.[o]
                bb.ExtendBy p
                r.Add index
                ci <- ci + 1
            ri

    static member pickle (n : MutableOctnode) =
        let children : Option<DbRef<MutableOctnode>>[] = n.Children
        let subnodes = FSharp.Collections.Array.zeroCreate children.Length
        for i in 0 .. children.Length - 1 do
            let c = children.[i]
            match c with
            | Some c ->     
                subnodes.[i] <- System.Guid c.Name
            | None ->
                subnodes.[i] <- System.Guid.Empty

        let cell : Cell = n.Cell
        let data : Map<Durable.Def, obj> = n.Data
        let bounds = n.BoundingBox

        let localps = 
            if n.CellCount > 0 then
                let ps = data.[Durable.Octree.PositionsLocal3f] |> unbox<IArrayBuffer<V3d>>
                V3fBuffer(ps.Buffer, ps.ByteOffset, ps.Length)
            else
                V3fBuffer(0)
                
                
        let localcs = 
            if n.CellCount > 0 then
                 let cs = data.[Durable.Octree.Colors3b] |> unbox<IArrayBuffer<uint8>>
                 C3bBuffer(cs.Buffer, cs.ByteOffset, cs.Length / 3)
            else
                 C3bBuffer(0)

        let data =
            Map.ofArray [|
                yield Durable.Octree.Cell, cell :> obj
                yield Durable.Octree.PointCountCell, n.CellCount |> int :> obj
                yield Durable.Octree.TotalPointCount, n.TotalPointCount :> obj
                yield Durable.Octree.BoundingBoxExactGlobal, bounds :> obj
                yield Durable.Octree.Colors3b, localcs :> obj
                yield Durable.Octree.PositionsLocal3f, localps :> obj
                if subnodes.Length > 0 then
                    yield Durable.Octree.SubnodesGuids, subnodes :> obj
            |]

        let stream = Stream()
        DurableDataCodec.encode stream Durable.Octree.Node (data :> obj)
        let res = stream.ToArrayBuffer()
        Prom.value res

    static member unpickle (db : Database) (splitLimit : int)  (name : string) (data : ArrayBuffer) : Promise<MutableOctnode> =
        let stream = Stream(data)
        let _def, data = DurableDataCodec.decode stream
        let data = unbox<Map<Durable.Def, obj>> data
        let id = System.Guid name
        let n = MutableOctnode(db, id, unbox data.[Durable.Octree.Cell], splitLimit)
        n.SetData(data) |> Prom.map (fun () -> n)

    member private x.SetData(d : Map<Durable.Def, obj>) : Promise<unit> =
        promise {
            bounds <- unbox d.[Durable.Octree.BoundingBoxExactGlobal]
            cellCount <- unbox d.[Durable.Octree.PointCountCell]
            totalCount <- unbox d.[Durable.Octree.TotalPointCount]

            match Map.tryFind Durable.Octree.SubnodesGuids d with
            | Some ids ->
                let ids = unbox<System.Guid[]> ids
                children <- FSharp.Collections.Array.zeroCreate ids.Length
                for i in 0 .. ids.Length - 1 do
                    let id = ids.[i]
                    if id = System.Guid.Empty then 
                        children.[i] <- None
                    else
                        let! r = db.GetRef(string id, MutableOctnode.pickle, MutableOctnode.unpickle db splitLimit)
                        children.[i] <- Some r

            | None ->
                children <- [||]

            let atts = 
                d                    
                |> Map.remove Durable.Octree.BoundingBoxExactGlobal
                |> Map.remove Durable.Octree.PointCountCell
                |> Map.remove Durable.Octree.MinTreeDepth
                |> Map.remove Durable.Octree.SubnodesGuids


            for (def, att) in Map.toSeq atts do
                if def = Durable.Octree.PositionsLocal3f then
                    let att = unbox<V3fBuffer> att
                    let dst = V3fList(Fun.NextPowerOfTwo att.Length)
                    dst.AddRange att
                    data <- Map.add Durable.Octree.PositionsLocal3f (dst :> obj) data
                elif def = Durable.Octree.Colors3b then
                    let att = unbox<C3bBuffer> att
                    let src = Uint8Buffer((att :> IArrayBuffer).Buffer, (att :> IArrayBuffer).ByteOffset, att.Length * 3)
                    let dst = Uint8List(Fun.NextPowerOfTwo (att.Length * 3))
                    dst.AddRange src
                    data <- Map.add Durable.Octree.Colors3b (dst :> obj) data
        }

    member x.Id : System.Guid = id

    static member Build(db : Database, cell : Cell, splitLimit : int, bb : Box3d, index : IArrayBuffer<int>, pb : IArrayBuffer<V3d>, atts : Map<Durable.Def, obj>) : Option<Promise<DbRef<MutableOctnode>>> =
        if index.Length <= 0 then
            None
        else
            let id = System.Guid.NewGuid()
            let c = MutableOctnode(db, id, cell, splitLimit)
            c.AddContained(bb, index, pb, atts) |> Prom.bind (fun v ->
                (ref db splitLimit c)
            ) |> Some
            
    member x.CellCount = cellCount
    member x.Data = data
    member x.TotalPointCount 
        with get() = totalCount
        and set v = totalCount <- v

    member x.Cell = cell

    member x.Children
        with get() = children
        and set c = children <- c

    member x.BoundingBox 
        with get() = bounds
        and set b = bounds <- b

    member x.ForceSplit() : Promise<unit> =
        promise {
            if cellCount > 0 then
                let center = cell.Center
                let localps = data.[Durable.Octree.PositionsLocal3f] |> unbox<V3fList>
                let ps = V3dBuffer.init localps.Count (fun i -> localps.[i] + center)

                let is = Int32Buffer.init ps.Length (fun i -> i)
                let atts = data |> Map.add Durable.Octree.PositionsLocal3f (ps :> obj) // that's a lie
                cellCount <- 0
                data <- Map.empty


                let part = partition cell bounds is ps
                let cc = FSharp.Collections.Array.zeroCreate part.Length
                for i in 0 .. part.Length - 1 do
                    let (bb, idx) = part.[i]
                    match MutableOctnode.Build(db, cell.GetChild i, splitLimit, bb, idx, ps, atts) with
                    | Some p ->
                        let! r = p 
                        cc.[i] <- Some r
                    | None ->
                        cc.[i] <- None


                children <- cc
            else
                children <- FSharp.Collections.Array.zeroCreate 8
        }

    member x.AddContained(bb : Box3d, idx : IArrayBuffer<int>, pb : IArrayBuffer<V3d>, atts : Map<Durable.Def, obj>) : Promise<unit> =
        promise {
            if idx.Length > 0 then
                if children.Length = 0 then
                    let newCnt = cellCount + idx.Length
                    if newCnt > splitLimit then
                        do! x.ForceSplit()
                        do! x.AddContained(bb, idx, pb, atts)

                    else
                        totalCount <- totalCount + float idx.Length
                        bounds.ExtendBy bb.Min
                        bounds.ExtendBy bb.Max
                        cellCount <- newCnt

                        let offset = -cell.Center
                        for (def, att) in Map.toSeq atts do
                            match Map.tryFind def data with
                            | Some dst ->   
                                append offset def idx att dst
                            | None ->
                                let l = createList def splitLimit
                                append offset def idx att l
                                data <- Map.add def l data
                            
                else
                    totalCount <- totalCount + float idx.Length
                    bounds.ExtendBy bb.Min
                    bounds.ExtendBy bb.Max

                    let sub = partition cell bb idx pb
                    for ci in 0 .. sub.Length - 1 do
                        let (bb, idx) = sub.[ci]
                        match children.[ci] with
                        | None -> 
                            match MutableOctnode.Build(db, cell.GetChild ci, splitLimit, bb, idx, pb, atts) with
                            | Some r ->
                                let! r = r
                                children.[ci] <- Some r
                            | None ->
                                children.[ci] <- None
                        | Some c ->
                            let! v = c.Read()
                            do! v.AddContained(bb, idx, pb, atts)
                            c.Write v
        }
    
    member x.BuildLod(r : DbRef<MutableOctnode>) : Promise<unit>=    
        promise {
            let children = x.Children
            if children.Length > 0 then
                let children = children |> FSharp.Collections.Array.choose (fun i -> i)
                let values = FSharp.Collections.Array.zeroCreate children.Length
                for i in 0 .. children.Length - 1 do 
                    let c = children.[i]
                    let! r = c.Read()
                    values.[i] <- r
                    do! r.BuildLod(c)

                let mutable count = 0
                for c in values do
                    let cnt = float splitLimit * (c.TotalPointCount / totalCount) |> int
                    let cc = c.CellCount

                    let index = 
                        if cnt >= cc then
                            Int32Buffer.init cc (fun i -> i) :> IArrayBuffer<int>
                        else
                            let indices = Int32List(cnt + 1)
                            let step = float cc / float cnt
                            let mutable s = 0.0
                            let mutable c = 0
                            while c < cnt do
                                indices.Add (int s)
                                s <- s + step
                                c <- c + 1
                            indices :> IArrayBuffer<int>

                    let offset = c.Cell.Center - cell.Center
                    for (def, att) in Map.toSeq c.Data do
                        match Map.tryFind def data with
                        | Some dst ->   
                            append offset def index att dst
                        | None ->
                            let l = createList def splitLimit
                            append offset def index att l
                            data <- Map.add def l data

                    count <- count + index.Length

                Log.line "%A: %d" cell count
                cellCount <- count
                r.Write x
        }
    
    static member BuildWithSubtrees(db : Database, splitLimit : int, cell : Cell, ts : array<Cell * DbRef<MutableOctnode>>) : Option<Promise<DbRef<MutableOctnode>>> =
        if ts.Length = 0 then
            None
        elif ts.Length = 1 && fst ts.[0] = cell then
            Some (Prom.value (snd ts.[0]))
        else
            promise {
                let id = System.Guid.NewGuid()
                let n = MutableOctnode(db, id, cell, splitLimit)
                let groups = ts |> FSharp.Collections.Array.groupBy (fun (c, _) -> cell.GetOctant c.Center)
                let children = FSharp.Collections.Array.zeroCreate 8
                let mutable total = 0.0
                let mutable bounds = Box3d.Invalid
                for (i, g) in groups do
                    match MutableOctnode.BuildWithSubtrees(db, splitLimit, cell.GetChild i, g) with
                    | Some node ->
                        let! node = node
                        let! n = node.Read()
                        total <- total + n.TotalPointCount
                        bounds.ExtendBy(n.BoundingBox.Min)
                        bounds.ExtendBy(n.BoundingBox.Max)
                        children.[i] <- Some node
                    | None ->
                        children.[i] <- None

                n.TotalPointCount <- total
                n.BoundingBox <- bounds
                n.Children <- children
                return! ref db splitLimit n
            } |> Some

type MutableOctree(db : Database, splitLimit : int) =
    let mutable root : Option<MutableOctnode> = None
    
    member x.Root = root

    member x.Add(bb : Box3d, ps : IArrayBuffer<V3d>, cs : IArrayBuffer<uint8>) =
        promise {
            if ps.Length > 0 then
                let idx = Int32Buffer.init ps.Length id

                let atts = 
                    Map.ofList [
                        Durable.Octree.PositionsLocal3f, ps :> obj // that's a lie
                        Durable.Octree.Colors3b, cs :> obj
                    ]

                match root with
                | Some r -> 
                    if r.Cell.BoundingBox.Contains bb then
                        do! r.AddContained(bb, idx, ps, atts)
                    else    
                        let mutable subNodes = [||]
                        let c = r.Cell
                        if c.IsCenteredAtOrigin then
                            if r.Children.Length = 0 then 
                                do! r.ForceSplit()
                            subNodes <- r.Children |> FSharp.Collections.Array.mapi (fun o v -> match v with | Some v -> Some (c.GetChild o, v) | _ -> None) |>  FSharp.Collections.Array.choose id
                        else
                            let! r = db.Ref(string r.Id, r, MutableOctnode.pickle, MutableOctnode.unpickle db splitLimit)
                            subNodes <- [| c, r |]

                        let bounds = subNodes |> FSharp.Collections.Array.fold (fun b (c,_) -> Box3d.Union(b, c.BoundingBox)) bb
                        let cell = Cell bounds
                        let newRoot = MutableOctnode.BuildWithSubtrees(db, splitLimit, cell, subNodes)
                        let mutable res : Option<MutableOctnode> = None
                        match newRoot with
                        | Some r -> 
                            let! r = r
                            let! o = r.Read()
                            do! o.AddContained(bb, idx, ps, atts)
                            r.Write o
                            res <- Some o
                        | None -> 
                            match MutableOctnode.Build(db, cell, splitLimit, bb, idx, ps, atts) with
                            | Some r ->
                                let! r = r
                                let! r = r.Read()
                                res <- Some r
                            | None ->
                                res <- None

                        root <- res

                | None ->
                    let c = Cell bb
                    match MutableOctnode.Build(db, c, splitLimit, bb, idx, ps, atts) with
                    | Some r ->
                        let! r = r
                        let! r = r.Read()
                        root <- Some r
                    | None ->
                        root <- None
        }
    
    member x.BuildLod() : Promise<unit> =
        match root with
        | Some r -> 
            promise {
                let! rootRef = db.Ref(string id, r, MutableOctnode.pickle, MutableOctnode.unpickle db splitLimit)
                do! r.BuildLod(rootRef)
            }
        | None -> 
            Prom.value ()

    member x.Persist() =
        promise {
            match root with
            | Some r ->
                let id = r.Id
                let bb = r.BoundingBox
                let rootInfo = 
                    Fable.Core.JsInterop.createObj [
                        "RootId", string id :> obj
                        "PointCount", r.TotalPointCount :> obj
                        "Bounds", Fable.Core.JsInterop.createObj [
                            "Min", Fable.Core.JsInterop.createObj [
                                "X", bb.Min.X :> obj
                                "Y", bb.Min.Y :> obj
                                "Z", bb.Min.Z :> obj
                            ]
                            "Max", Fable.Core.JsInterop.createObj [
                                "X", bb.Max.X :> obj
                                "Y", bb.Max.Y :> obj
                                "Z", bb.Max.Z :> obj
                            ]
                        ]
                        "Cell", Fable.Core.JsInterop.createObj [
                            "X", float r.Cell.X :> obj
                            "Y", float r.Cell.Y :> obj
                            "Z", float r.Cell.Z :> obj
                            "E", r.Cell.E :> obj
                        ]

                        "GZipped", db.Compressed :> obj
                    ]
                let str = JSON.stringify rootInfo

                let! rootRef = db.Ref(string id, r, MutableOctnode.pickle, MutableOctnode.unpickle db splitLimit)
                rootRef.Write r
                do! db.Store.SetString("root.json", str)
            | None ->
                ()
        }





module Ascii =
    open Fable.Core

    [<AutoOpen>]
    module private Helpers = 
        [<Emit("$0.charCodeAt($1)")>]
        let charCodeAt (str : string) (i : float) : float = jsNative

        type System.String with
            member inline x.charCodeAt(i : float) = charCodeAt x i

    // -15283.527316 246958.380835 412.436068 -1964 110 114 100
    type Token =
        | Skip      = 0
        | PositionX = 1
        | PositionY = 2
        | PositionZ = 3
        | RedByte   = 4
        | GreenByte = 5
        | BlueByte  = 6

    type ImportConfig =
        {
            overwrite   : bool
            format      : Token[]
            splitLimit  : int
            progress    : float -> float -> float -> unit
        }
    
    type private MyRef<'a> = { mutable Value : 'a }


    let inline private nextToken (str : string) (offset : MyRef<float>) =
        let mutable o = offset.Value
        while o < float str.Length && str.charCodeAt o = 32.0 do o <- o + 1.0

        let n = str.IndexOf(' ', unbox<int> o) |> float 
        if n >= 0.0 then
            //let mutable next = n + 1.0
            //while next < float str.Length && str.charCodeAt next = 32.0 do
            //    n <- n + 1.0
            //    next <- next + 1.0
            offset.Value <- o
            n - o
        elif o < float str.Length then
            offset.Value <- o
            float str.Length - o
        else
            -1.0
            
    //let charCodeAt (str : string) (i : int) : int = int str.[i]
    
    //type System.String with
    //    member inline x.charCodeAt(i : int) = charCodeAt x i
    
    let fastParseFloat (str : string) (o : float) (len : float) =
        let mutable sign = true
        let mutable v = 0.0
        let mutable i = 0.0
        let mutable si = o

        let mutable exp = 1.0
        let mutable iexp = 0.1

        while i < len do
            let c = charCodeAt str si
            if c = 45.0 then 
                sign <- not sign
            elif c = 46.0 then 
                v <- v * iexp
                exp <- 0.1
                iexp <- 0.0
            else 
                let o = c - 48.0
                v <- v + exp * o
                exp <- exp * 0.1
                if iexp <> 0.0 then iexp <- iexp * 10.0


            i <- i + 1.0
            si <- si + 1.0

        if sign then v
        else -v

    let inline private tryParseDouble (str : string) (o : float) (l : float) =
        let v =  parseFloat (str.Substring(unbox<int> o, unbox<int> l))
        if System.Double.IsNaN v then
            None
        else
            Some v

    let inline private tryParseByte (str : string) (o : float) (l : float) =
        let v = parseFloat (str.Substring(unbox<int> o, unbox<int> l)) 
        if System.Double.IsNaN v then
            None
        else
            Some (byte v)

    let parseLine (fmt : array<Token>) (ps : V3dList, cs : Uint8List, bb : Box3d) (line : string) =
        let mutable px = 0.0
        let mutable py = 0.0
        let mutable pz = 0.0
        let mutable r = 0uy
        let mutable g = 0uy
        let mutable b = 0uy

        let mutable error = false
        let mutable i = 0
        let o = { Value = 0.0 }
        let mutable len = nextToken line o
        
        while not error && i < fmt.Length && len >= 0.0 do
            let s = o.Value
            match fmt.[i] with

            | Token.PositionX ->
                match tryParseDouble line s len with
                | Some v -> px <- v
                | None -> error <- true

            | Token.PositionY ->
                match tryParseDouble line s len  with
                | Some v -> py <- v
                | None -> error <- true

            | Token.PositionZ ->
                match tryParseDouble line s len with
                | Some v -> pz <- v
                | None -> error <- true

            | Token.RedByte ->
                match tryParseByte line s len with
                | Some b -> r <- b
                | None -> error <- true
                
            | Token.GreenByte ->
                match tryParseByte line s len with
                | Some b -> g <- b
                | None -> error <- true

            | Token.BlueByte ->
                match tryParseByte line s len with
                | Some v -> b <- v
                | None -> error <- true
            | _ ->
                ()

            o.Value <- o.Value + len
            len <- nextToken line o
            i <- i + 1


        if not error && i = fmt.Length then
            bb.ExtendBy(px, py, pz)
            ps.Add(px, py, pz)
            cs.Add r; cs.Add g; cs.Add b


    let defaultConfig =
        {
            overwrite   = false
            format      = [| Token.PositionX; Token.PositionY; Token.PositionZ; Token.Skip; Token.BlueByte; Token.GreenByte; Token.RedByte |]
            splitLimit  = 32768
            progress    = fun _ _ _ -> ()
        }

module MutableOctree =
    open Aardvark.Import.Browser

    let importAscii (cfg : Ascii.ImportConfig) (db : Database) (blob : Blob) =
        promise {
            let startTime = performance.now()
            let res = MutableOctree(db, cfg.splitLimit) 
            let emit (msg : Blob.ReadLineMessage<V3dList * Uint8List * Box3d>) =
                match msg with
                | Blob.Done ->
                    promise {
                        do! res.BuildLod()
                        do! res.Persist()
                        do! db.Flush()
                        let took = performance.now() - startTime |> MicroTime.FromMilliseconds
                        Log.line "took: %A" took
                        return res
                    }
                | Blob.Progress(totalSize, size, time) ->
                    cfg.progress totalSize size time
                    Prom.value res

                | Blob.Chunk(ps, cs, bb) ->
                    res.Add(bb, ps, cs) |> Prom.map (fun () -> res)
        
            let chunkSize = 4 <<< 20
            let expectedCount = Fun.NextPowerOfTwo (chunkSize / 50)
            let init() = V3dList(expectedCount), Uint8List(3 * expectedCount), Box3d.Invalid
                            
            return! Aardvark.Data.Blob.readLines chunkSize init (Ascii.parseLine cfg.format) emit blob
        }



