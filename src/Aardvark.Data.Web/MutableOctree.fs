namespace Aardvark.Data

open Aardvark.Import.JS
open Aardvark.Base
open Microsoft.FSharp.Collections
open Aardvark.Data


type RKdTree =
    {
        perm    : Int32Array
        axis    : Int32Array
        radius  : Float64Array
    }

module RKdTree =


    [<AutoOpen>]
    module private Helpers = 

        let get_x = fun (v : V3d) -> v.X
        let get_y = fun (v : V3d) -> v.Y
        let get_z = fun (v : V3d) -> v.Z

        let set_x = fun (v : V3d) (x : float) -> V3d(x, v.Y, v.Z)
        let set_y = fun (v : V3d) (y : float) -> V3d(v.X, y, v.Z)
        let set_z = fun (v : V3d) (z : float) -> V3d(v.X, v.Y, z)

        let gets = [| get_x; get_y; get_z; get_x |]
        let sets = [| set_x; set_y; set_z; set_x |]

        let c_insertionMedianThreshold = 7
        let compares (pos : IArrayBuffer<V3d>) =
            [|
                fun (l : int) (r : int) -> compare (pos.Get(l).X) (pos.Get(r).X)
                fun (l : int) (r : int) -> compare (pos.Get(l).Y) (pos.Get(r).Y)
                fun (l : int) (r : int) -> compare (pos.Get(l).Z) (pos.Get(r).Z)
                fun (l : int) (r : int) -> compare (pos.Get(l).X) (pos.Get(r).X)
            |]


    let get (i : int) (v : V3d) = gets.[i &&& 3] v
    let set (i : int) (v : V3d) (a : float) = sets.[i &&& 3] v a

    type IArrayBuffer<'a> with
        member a.QuickMedian(cmp : 'a -> 'a -> int, l : int, r : int, countSub1 : int, med : int) =
            let mutable countSub1 = countSub1
            let mutable l = l
            let mutable r = r
            let mutable ret = false
            while not ret && countSub1 >= c_insertionMedianThreshold do
                let sixth = (1 + countSub1) / 6
                let e1 = l + sixth
                let e5 = r - sixth
                let e3 = (l + r) >>> 1
                let e4 = e3 + sixth
                let e2 = e3 - sixth

                if cmp (a.Get(e1)) (a.Get(e2)) > 0 then ( let t = (a.Get(e1)) in a.Set(e1, a.Get(e2)); a.Set(e2, t) )
                if cmp (a.Get(e4)) (a.Get(e5)) > 0 then ( let t = (a.Get(e4)) in a.Set(e4, a.Get(e5)); a.Set(e5, t) )
                if cmp (a.Get(e1)) (a.Get(e3)) > 0 then ( let t = (a.Get(e1)) in a.Set(e1, a.Get(e3)); a.Set(e3, t) )
                if cmp (a.Get(e2)) (a.Get(e3)) > 0 then ( let t = (a.Get(e2)) in a.Set(e2, a.Get(e3)); a.Set(e3, t) )
                if cmp (a.Get(e1)) (a.Get(e4)) > 0 then ( let t = (a.Get(e1)) in a.Set(e1, a.Get(e4)); a.Set(e4, t) )
                if cmp (a.Get(e3)) (a.Get(e4)) > 0 then ( let t = (a.Get(e3)) in a.Set(e3, a.Get(e4)); a.Set(e4, t) )
                if cmp (a.Get(e2)) (a.Get(e5)) > 0 then ( let t = (a.Get(e2)) in a.Set(e2, a.Get(e5)); a.Set(e5, t) )
                if cmp (a.Get(e2)) (a.Get(e3)) > 0 then ( let t = (a.Get(e2)) in a.Set(e2, a.Get(e3)); a.Set(e3, t) )
                if cmp (a.Get(e4)) (a.Get(e5)) > 0 then ( let t = (a.Get(e4)) in a.Set(e4, a.Get(e5)); a.Set(e5, t) )

                let mutable p1 = a.Get e2
                a.Set(e2, a.Get(l))
                let mutable p2 = a.Get e4
                a.Set(e4, a.Get(r))

                let mutable lo = l + 1;
                let mutable hi = r - 1;

                let pivotsDiffer = cmp p1 p2 <> 0

                if pivotsDiffer then
                    let mutable i = lo
                    while i <= hi do
                        let mutable ai = a.Get(i)
                        if cmp ai p1 < 0 then ( a.Set(i, a.Get(lo)); a.Set(lo, ai); lo <- lo + 1 )
                        elif cmp ai p2 > 0 then
                            while cmp (a.Get(hi)) p2 > 0 && i < hi do
                                hi <- hi - 1
                            a.Set(i, a.Get(hi))
                            a.Set(hi, ai)
                            hi <- hi - 1
                            ai <- a.Get i
                            if cmp ai p1 < 0 then 
                                a.Set(i, a.Get lo)
                                a.Set(lo, ai)
                                lo <- lo + 1
                        i <- i + 1
                else
                    let mutable i = lo
                    while i <= hi do
                        let mutable ai = a.Get i
                        if cmp ai p1 = 0 then ()
                        elif cmp ai p1 < 0 then ( a.Set(i, a.Get lo); a.Set(lo, ai); lo <- lo + 1 )
                        else
                            while cmp (a.Get hi) p1 > 0 do
                                hi <- hi - 1
                            a.Set(i, a.Get hi)
                            a.Set(hi, ai)
                            hi <- hi - 1
                            ai <- a.Get i
                            if cmp ai p1 < 0 then 
                                a.Set(i, a.Get lo)
                                a.Set(lo, ai)
                                lo <- lo + 1

                        i <- i + 1

                a.Set(l, a.Get(lo - 1)); a.Set(lo - 1, p1)
                a.Set(r, a.Get(hi + 1)); a.Set(hi + 1, p2)

                let cl = lo - 2 - l
                let cr = r - hi - 2

                if pivotsDiffer then
                    if lo < e1 && e5 < hi then
                        if med <= lo - 2 then
                            r <- lo - 2
                            countSub1 <- cl
                        elif med >= hi + 2 then 
                            l <- hi + 2
                            countSub1 <- cr
                        else
                            while cmp (a.Get lo) p1 = 0 do
                                lo <- lo + 1

                            let mutable i = lo + 1
                            while i <= hi do
                                if cmp (a.Get i) p1 = 0 then
                                    p1 <- a.Get i
                                    a.Set(i, a.Get lo)
                                    a.Set(lo,p1)
                                    lo <- lo + 1
                                i <- i + 1
                            while cmp (a.Get hi) p2 = 0 do
                                hi <- hi - 1

                            let mutable i = hi - 1
                            while i >= lo do
                                if cmp (a.Get i) p2 = 0 then
                                    p2 <- a.Get i
                                    a.Set(i, a.Get hi)
                                    a.Set(hi, p2)
                                    hi <- hi - 1
                                i <- i - 1
                            if (med < lo || med > hi) then ret <- true
                            l <- lo
                            r <- hi
                            countSub1 <- hi - lo

                    else
                        if med <= lo - 2 then (r <- lo - 2; countSub1 <- cl )
                        elif (med >= hi + 2) then (l <- hi + 2; countSub1 <- cr )
                        elif (med >= lo && med <= hi) then ( l <- lo; r <- hi; countSub1 <- hi - lo )
                        else ret <- true
                else
                    if (med <= lo - 2) then ( r <- lo - 2; countSub1 <- cl )
                    elif (med >= hi + 2) then ( l <- hi + 2; countSub1 <- cr )
                    else ret <- true
                

            if not ret then
                let mutable i = l + 1
                while i <= r do
                    let mutable ai = a.Get i
                        
                    let mutable j = i - 1;
                    while j >= l && cmp ai (a.Get j) < 0 do
                        a.Set(j + 1, a.Get j)
                        j <- j - 1
                    a.Set(j + 1, ai)
                    i <- i + 1

        member inline x.QuickMedian(cmp : 'a -> 'a -> int, beginIncl : int, endExcl : int, med : int) =
            x.QuickMedian(cmp, beginIncl, endExcl - 1, endExcl - beginIncl - 1, med)
    type V3d with
        member this.MajorDim =
            let x = abs this.X
            let y = abs this.Y
            let z = abs this.Z

            if x > y then
                if z > y then 2
                else 1
            else
                if z > x then 2
                else 0

    let rec private balance (eps : float) (cmps : array<int -> int -> int>) (data : RKdTree) (perm : Int32Buffer) (positions : IArrayBuffer<V3d>)
                             (top : int) (left : int) (row : int) (s : int) (e : int) (box : Box3d) =

        let mutable left = left
        let mutable row = row

        if row <= 0 then
            left <- left >>> 1
            row <- System.Int32.MaxValue

        if left = 0 then
            data.perm.[top] <- perm.[s]

        else
            let mutable mid = s - 1 + left + min left row
            let dim = box.Size.MajorDim
            data.axis.[top] <- dim
                

            perm.QuickMedian(cmps.[dim], s, e, mid)
                
            let splitPoint = positions.Get(perm.[mid])
            let vm = get dim splitPoint
            let mutable lbox = box
            let mutable rbox = box
            lbox <- Box3d(lbox.Min, set dim lbox.Max (float vm))
            rbox <- Box3d(set dim rbox.Min (float vm), rbox.Max)
            let mutable r2 = 0.0
            for i in s .. e - 1 do
                let d = Vec.lengthSquared (positions.Get(perm.[i]) - splitPoint)
                r2 <- max r2 (float d)

            data.perm.[top] <- perm.[mid]
            data.radius.[top] <- sqrt r2 + eps

            //let t1 = 2 * top + 1
            //let lh = left >>> 1
            if s < mid then
                balance eps cmps data perm positions (2 * top + 1) (left >>> 1) row s mid lbox

            mid <- mid + 1

            if mid < e then
                balance eps cmps data perm positions (2 * top + 2) (left >>> 1) (row - left) mid e rbox
    

    let build (eps : float) (positions : IArrayBuffer<V3d>) =

        let axis = Int32Array.Create (positions.Length / 2)
        let radius = Float64Array.Create (positions.Length / 2)
        let perm = FSharp.Collections.Array.create positions.Length System.Int32.MaxValue |> unbox<Int32Array>

        let bounds = Box3d(positions)
        let data = { perm = perm; axis = axis; radius = radius }

        let size = positions.Length
        let p2 = Fun.PrevPowerOfTwo size
        let row = size + 1 - p2
        let left = p2 >>> 1
        let tperm = Int32Buffer.init positions.Length id
        balance eps (compares positions) data tperm positions 0 left row 0 size bounds
        data


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
                match Map.tryFind Durable.Octree.AveragePointDistance data with
                | Some v -> 
                    yield Durable.Octree.AveragePointDistance, v
                | None ->
                    ()
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
                |> Map.remove Durable.Octree.AveragePointDistance
                |> Map.remove Durable.Octree.AveragePointDistanceStdDev

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
    
    member x.BuildLod(r : DbRef<MutableOctnode>, rel : float) : Promise<int * int>=    
        promise {
            let children = x.Children
            let mutable minDepth = System.Int32.MaxValue
            let mutable maxDepth = System.Int32.MinValue

            if children.Length > 0 then
                let children = children |> FSharp.Collections.Array.choose (fun i -> i)
                let values = FSharp.Collections.Array.zeroCreate children.Length


                for i in 0 .. children.Length - 1 do 
                    let c = children.[i]
                    let! r = c.Read()
                    values.[i] <- r
                    let rel = r.TotalPointCount / totalCount
                    let! (dMin, dMax) = r.BuildLod(c, rel)
                    minDepth <- min dMin minDepth
                    maxDepth <- max dMax maxDepth 

                let mutable count = 0
                for c in values do
                    let cnt = float splitLimit * (c.TotalPointCount / totalCount) |> int
                    let cc = c.CellCount

                    let index = Int32Buffer.init (min cc cnt) (fun i -> i) :> IArrayBuffer<int>
                    let offset = c.Cell.Center - cell.Center

                    let cData =
                        c.Data
                        |> Map.remove Durable.Octree.MinTreeDepth
                        |> Map.remove Durable.Octree.MaxTreeDepth
                        |> Map.remove Durable.Octree.PointCountCell
                        |> Map.remove Durable.Octree.TotalPointCount
                        |> Map.remove Durable.Octree.AveragePointDistance
                        |> Map.remove Durable.Octree.AveragePointDistanceStdDev
                        |> Map.remove Durable.Octree.Buffer

                    for (def, att) in Map.toSeq cData do
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
                data <- Map.add Durable.Octree.MinTreeDepth (minDepth :> obj) data
                data <- Map.add Durable.Octree.MaxTreeDepth (maxDepth :> obj) data
                

                //let perm = tree.perm
                //let pos = V3fBuffer.init perm.length (fun i -> pos.[perm.[i]])
                //data <- Map.add Durable.Octree.PositionsLocal3f (pos :> obj) data
                //r.Write x
            else
                minDepth <- 0
                maxDepth <- 0




            let pos = data.[Durable.Octree.PositionsLocal3f] |> unbox<IArrayBuffer<V3d>>
            let col = data.[Durable.Octree.Colors3b] |> unbox<IArrayBuffer<uint8>>
            //let col = Uint8Array.Create((col :> IArrayBuffer).Buffer, (col :> IArrayBuffer).ByteOffset, pos.Count * 3)
            let t = Aardvark.Import.Browser.performance.now()
            let tree = RKdTree.build 1E-6 pos
            let dt = Aardvark.Import.Browser.performance.now() - t
            Log.warn "kd (%d): %.3fms" pos.Length dt

            let pp = V3fBuffer.zeroCreate tree.perm.length
            let cc = Uint8Array.Create (tree.perm.length * 3)
            do
                for i in 0 .. tree.perm.length - 1 do
                    let ii = tree.perm.[i]
                    pp.[i] <- pos.Get ii

                    let di = 3 * i
                    let ci = 3 * ii
                    cc.[di+0] <- col.Get (ci+0) 
                    cc.[di+1] <- col.Get (ci+1) 
                    cc.[di+2] <- col.Get (ci+2) 
                    
            data <- Map.add Durable.Octree.AveragePointDistance (float32 rel :> obj) data
            data <- Map.add Durable.Octree.PositionsLocal3f (pp :> obj) data
            data <- Map.add Durable.Octree.Colors3b (Uint8Buffer(cc.buffer, cc.byteOffset, cc.length) :> obj) data

            r.Write x

            return (minDepth, maxDepth)
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
    //let mutable sum = V3d.Zero
    //let mutable sumSq = V3d.Zero
    //let mutable cnt = 0.0

    member x.Root = root

    member x.Add(bb : Box3d, ps : IArrayBuffer<V3d>, cs : IArrayBuffer<uint8>) =
        promise {
            if ps.Length > 0 then
                let idx = Int32Buffer.init ps.Length id

                //for i in 0 .. ps.Length - 1 do
                //    let p = ps.Get i
                //    sum <- sum + p
                //    sumSq <- sumSq + p*p
                //    cnt <- cnt + 1.0

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
                let! _ = r.BuildLod(rootRef, 1.0)
                ()
            }
        | None -> 
            Prom.value ()

    member x.Persist(centroid : V3d, stddev : float) =
        promise {
            match root with
            | Some r ->
                let id = r.Id
                let bb = r.BoundingBox

                //let centroid = sum / cnt
                //let var = sumSq / cnt - centroid * centroid
                //let stddev = sqrt (var.X + var.Y + var.Z) //V3d(sqrt var.X, sqrt var.Y, sqrt var.Z) |> Vec.length

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
                        "Centroid", Fable.Core.JsInterop.createObj [
                            "X", centroid.X :> obj
                            "Y", centroid.Y :> obj
                            "Z", centroid.Z :> obj
                        ]

                        "CentroidStdDev", stddev :> obj

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


    let arr = [| ' '; ',' |]

    let inline private nextToken (str : string) (offset : MyRef<float>) =
        let mutable o = offset.Value
        while o < float str.Length && (str.charCodeAt o = 32.0 || str.charCodeAt o = 44.0) do o <- o + 1.0

        let n = str.IndexOfAny(arr, unbox<int> o) |> float 
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

    type Stats =
        {
            mutable sumX : float
            mutable sumY : float
            mutable sumZ : float
            mutable sumSqX : float
            mutable sumSqY : float
            mutable sumSqZ : float
            mutable count : float
        }

        static member Empty = { sumX = 0.0; sumY = 0.0; sumZ = 0.0; sumSqX = 0.0; sumSqY = 0.0; sumSqZ = 0.0; count = 0.0 }


    let parseLine (fmt : array<Token>) (ps : V3dList, cs : Uint8List, bb : Box3d, s : Stats) (line : string) =
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
            s.sumX <- s.sumX + px
            s.sumY <- s.sumY + py
            s.sumZ <- s.sumZ + pz
            s.sumSqX <- s.sumSqX + px * px
            s.sumSqY <- s.sumSqY + py * py
            s.sumSqZ <- s.sumSqZ + pz * pz
            s.count <- s.count + 1.0

            bb.ExtendBy(px, py, pz)
            ps.Add(px, py, pz)
            cs.Add r; cs.Add g; cs.Add b

    [<Emit("parseInt($0)")>]
    let parseInt(str : string) : int = jsNative

    let tryGuessFormat (line : string) =
        let parts = line.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)

        let kind =
            parts |> FSharp.Collections.Array.map (fun p ->
                let v = parseFloat p
                if System.Double.IsNaN (unbox v) then
                    None
                else
                    if v = float (int v) then
                        if v >= 0.0 then Some ("uint", float v)
                        else Some ("int", float v)
                    else
                        Some ("float", v)
            )

        let floats = [| Token.PositionX; Token.PositionY; Token.PositionZ |]
        let uints = [| Token.BlueByte; Token.GreenByte; Token.RedByte |]

        let fmt = System.Collections.Generic.List<Token>()

        let add (index : int) (t : Token) =
            while fmt.Count < index do
                fmt.Add Token.Skip
            fmt.Add t

        let mutable floatIndex = 0
        let mutable uintIndex = 0
        for i in 0 .. kind.Length - 1 do
            match kind.[i] with
            | Some ("float",_) ->
                if floatIndex < floats.Length then 
                    add i floats.[floatIndex]
                    floatIndex <- floatIndex + 1
            | Some ("uint", v) when v <= 255.0 ->
                if uintIndex < uints.Length then 
                    add i uints.[uintIndex]
                    uintIndex <- uintIndex + 1
            | _ ->
                ()
        if floatIndex = floats.Length then 
            let arr = fmt.ToArray()
            console.warn arr
            Some arr
        else 
            console.warn "bad format"
            None






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
            let stats = Ascii.Stats.Empty
            let parseLine = ref Unchecked.defaultof<_>
            parseLine :=
                if cfg.format.Length = 0 then
                    fun state line ->
                        match Ascii.tryGuessFormat line with
                        | Some fmt -> 
                            parseLine := Ascii.parseLine fmt
                            !parseLine state line
                        | None ->
                            ()
                else 
                    Ascii.parseLine cfg.format
            let startTime = performance.now()
            let res = MutableOctree(db, cfg.splitLimit) 
            let emit (msg : Blob.ReadLineMessage<V3dList * Uint8List * Box3d * Ascii.Stats>) =
                match msg with
                | Blob.Done ->
                    promise {
                        do! res.BuildLod()

                        
                        let centroid = V3d(stats.sumX, stats.sumY, stats.sumZ) / stats.count
                        let var = V3d(stats.sumSqX, stats.sumSqY, stats.sumSqZ) / stats.count - centroid * centroid
                        let stddev = sqrt (var.X + var.Y + var.Z)

                        do! res.Persist(centroid, stddev)
                        do! db.Flush()
                        let took = performance.now() - startTime |> MicroTime.FromMilliseconds
                        Log.line "took: %A" took
                        return res
                    }
                | Blob.Progress(totalSize, size, time) ->
                    cfg.progress totalSize size time
                    Prom.value res

                | Blob.Chunk(ps, cs, bb, s) ->
                    res.Add(bb, ps, cs) |> Prom.map (fun () -> res)
        
            let chunkSize = 4 <<< 20
            let expectedCount = Fun.NextPowerOfTwo (chunkSize / 50)

            let init() = V3dList(expectedCount), Uint8List(3 * expectedCount), Box3d.Invalid, stats
                            


            return! Aardvark.Data.Blob.readLines chunkSize init (fun s l -> !parseLine s l) emit blob

        }



