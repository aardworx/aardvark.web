namespace Aardvark.Base


type Box3d(bmin : V3d, bmax : V3d) =
    let mutable bmin = bmin
    let mutable bmax = bmax

    member x.Min = bmin
    member x.Max = bmax

    member x.Center = 0.5 * (bmin + bmax)

    member x.Size = bmax - bmin

    member x.ExtendBy(v : V3d) =
        bmin <- V3d(min bmin.X v.X, min bmin.Y v.Y, min bmin.Z v.Z)
        bmax <- V3d(max bmax.X v.X, max bmax.Y v.Y, max bmax.Z v.Z)

        
    member __.ExtendBy(x : float, y : float, z : float) =
        bmin <- V3d(min bmin.X x, min bmin.Y y, min bmin.Z z)
        bmax <- V3d(max bmax.X x, max bmax.Y y, max bmax.Z z)

    member x.GetMinMaxInDirection(dir : V3d) =
        let mutable xmin = System.Double.NegativeInfinity
        let mutable ymin = System.Double.NegativeInfinity
        let mutable zmin = System.Double.NegativeInfinity
        let mutable xmax = System.Double.PositiveInfinity
        let mutable ymax = System.Double.PositiveInfinity
        let mutable zmax = System.Double.PositiveInfinity

        if dir.X >= 0.0 then 
            xmin <- bmin.X
            xmax <- bmax.X
        else
            xmin <- bmax.X
            xmax <- bmin.X
            
        if dir.Y >= 0.0 then 
            ymin <- bmin.Y
            ymax <- bmax.Y
        else
            ymin <- bmax.Y
            ymax <- bmin.Y
            
        if dir.Z >= 0.0 then 
            zmin <- bmin.Z
            zmax <- bmax.Z
        else
            zmin <- bmax.Z
            zmax <- bmin.Z

        V3d(xmin, ymin, zmin), V3d(xmax, ymax, zmax)
        
    member x.GetMinInDirection(dir : V3d) =
        V3d(
            (if dir.X >= 0.0 then bmin.X else bmax.X),
            (if dir.Y >= 0.0 then bmin.Y else bmax.Y),
            (if dir.Z >= 0.0 then bmin.Z else bmax.Z)
        )
    member x.GetMaxInDirection(dir : V3d) =
        V3d(
            (if dir.X >= 0.0 then bmax.X else bmin.X),
            (if dir.Y >= 0.0 then bmax.Y else bmin.Y),
            (if dir.Z >= 0.0 then bmax.Z else bmin.Z)
        )
        
    member internal x.GetMinInDirection4(dir : V3d) =
        V4d(
            (if dir.X >= 0.0 then bmin.X else bmax.X),
            (if dir.Y >= 0.0 then bmin.Y else bmax.Y),
            (if dir.Z >= 0.0 then bmin.Z else bmax.Z),
            1.0
        )
    member internal x.GetMaxInDirection4(dir : V3d) =
        V4d(
            (if dir.X >= 0.0 then bmax.X else bmin.X),
            (if dir.Y >= 0.0 then bmax.Y else bmin.Y),
            (if dir.Z >= 0.0 then bmax.Z else bmin.Z),
            1.0
        )

    member x.GetMaxPlaneHeight(plane : V4d) =
        (if plane.X >= 0.0 then bmax.X else bmin.X) * plane.X +
        (if plane.Y >= 0.0 then bmax.Y else bmin.Y) * plane.Y +
        (if plane.Z >= 0.0 then bmax.Z else bmin.Z) * plane.Z +
        plane.W



    member x.IntersectsViewProj (vp : Trafo3d) =
        let r0 = vp.Forward.R0
        let r1 = vp.Forward.R1
        let r2 = vp.Forward.R2
        let r3 = vp.Forward.R3

        let inline plane (v : V4d) = v / v.XYZ.Length

        let planes =
            [|
                plane (r3 + r0)
                plane (r3 - r0)
                plane (r3 + r1)
                plane (r3 - r1)
                plane (r3 + r2)
                plane (r3 - r2)
            |]

        planes |> Array.forall (fun plane ->
            x.GetMaxPlaneHeight(plane) >= 0.0
        )

        ////left
        //let plane = r3 + r0
        //let max = x.GetMaxInDirection4(plane.XYZ)
        //if (max.Dot(plane) < 0.0) then false
        //else
        //    //right
        //    let plane = r3 - r0
        //    let max = x.GetMaxInDirection4(plane.XYZ)
        //    if (max.Dot(plane) < 0.0) then false
        //    else
        //        //top
        //        let plane = r3 + r1
        //        let max = x.GetMaxInDirection4(plane.XYZ)
        //        if (max.Dot(plane) < 0.0) then false
        //        else
        //            //bottom
        //            let plane = r3 - r1
        //            let max = x.GetMaxInDirection4(plane.XYZ)
        //            if (max.Dot(plane) < 0.0) then false
        //            else
        //                //near
        //                let plane = r3 + r2;
        //                let max = x.GetMaxInDirection4(plane.XYZ)
        //                if (max.Dot(plane) < 0.0) then false
        //                else
        //                    //far
        //                    let plane = r3 - r2
        //                    let max = x.GetMaxInDirection4(plane.XYZ)
        //                    if (max.Dot(plane) < 0.0) then false
        //                    else true

    member x.Contains(v : V3d) =
        v.AllGreaterOrEqual x.Min && v.AllSmallerOrEqual x.Max

    member x.Contains(b : Box3d) =
        x.Contains b.Min && x.Contains b.Max

    member x.Volume =
        let s = x.Size
        s.X * s.Y * s.Z

    static member FromMinAndSize(min : V3d, size : V3d) =
        Box3d(min, min + size)

    static member Unit =
        Box3d(V3d.Zero, V3d.III)
        
    static member Invalid =
        Box3d(V3d(System.Double.PositiveInfinity, System.Double.PositiveInfinity, System.Double.PositiveInfinity), V3d(System.Double.NegativeInfinity, System.Double.NegativeInfinity, System.Double.NegativeInfinity) )
    static member Union(l : Box3d, r : Box3d) =
        Box3d(
            V3d(min l.Min.X r.Min.X, min l.Min.Y r.Min.Y, min l.Min.Z r.Min.Z),
            V3d(max l.Max.X r.Max.X, max l.Max.Y r.Max.Y, max l.Max.Z r.Max.Z)
        )
            
    new (ps : IArrayBuffer<V3d>) =
        let mutable bmin = V3d(System.Double.PositiveInfinity, System.Double.PositiveInfinity, System.Double.PositiveInfinity)
        let mutable bmax = V3d(System.Double.NegativeInfinity, System.Double.NegativeInfinity, System.Double.NegativeInfinity)
        for i in 0 .. ps.Length - 1 do
            let p = ps.Get i
            bmin <- V3d(min bmin.X p.X, min bmin.Y p.Y, min bmin.Z p.Z)
            bmax <- V3d(max bmax.X p.X, max bmax.Y p.Y, max bmax.Z p.Z)
        Box3d(bmin, bmax)

type Triangle3d(p0 : V3d, p1 : V3d, p2 : V3d) =
    member x.P0 = p0
    member x.P1 = p1
    member x.P2 = p2



type Cell(x : int64, y : int64, z : int64, e : int) =
    let bb, center =
        let d = 2.0 ** float e
        let isCenteredAtOrigin = x = System.Int64.MaxValue && y = System.Int64.MaxValue && z = System.Int64.MaxValue
        let min = if isCenteredAtOrigin then V3d(-0.5 * d, -0.5 * d, -0.5 * d) else V3d(float x * d, float y * d, float z * d)
        Box3d(min, min + V3d(d, d, d)), min + 0.5 * V3d(d, d, d)
    
    member __.X = x
    member __.Y = y
    member __.Z = z
    member __.E = e
    member x.BoundingBox = bb
    member x.Center = center

    member x.GetOctant (p : V3d) =
        (if p.X > center.X then 4 else 0) |||
        (if p.Y > center.Y then 2 else 0) |||
        (if p.Z > center.Z then 1 else 0)

    member __.GetChild(i : int) : Cell =
        let isCenteredAtOrigin = x = System.Int64.MaxValue && y = System.Int64.MaxValue && z = System.Int64.MaxValue
        if isCenteredAtOrigin then
            Cell(
                (if (i &&& 4) = 0 then -1L else 0L),
                (if (i &&& 2) = 0 then -1L else 0L),
                (if (i &&& 1) = 0 then -1L else 0L),
                e - 1
            )
        else
            let u = int64 (i &&& 4) >>> 2
            let v = int64 (i &&& 2) >>> 1
            let w = int64 (i &&& 1)
            Cell(2L*x + u, 2L*y + v, 2L*z + w, e - 1)

    override __.GetHashCode() =
        Aardvark.Base.HashCode.Combine(x.GetHashCode(), y.GetHashCode(), z.GetHashCode(), e.GetHashCode())

    override __.Equals o =
        match o with
        | :? Cell as o -> x = o.X && y = o.Y && z = o.Z && e = o.E
        | _ -> false


    override __.ToString() = sprintf "%d_%d_%d_%d" x y z e

    member __.IsCenteredAtOrigin =
        x = System.Int64.MaxValue && y = System.Int64.MaxValue && z = System.Int64.MaxValue

    

    new (b : Box3d) =
        let s = b.Size
        let s = max s.X (max s.Y s.Z)
        let mutable e = int (Fun.Log2 s)
        let floor3 (v : V3d) = V3d(floor v.X, floor v.Y, floor v.Z)

        if (b.Min.X <= 0.0 && b.Max.X >= 0.0) || (b.Min.Y <= 0.0 && b.Max.Y >= 0.0) || (b.Min.Z <= 0.0 && b.Max.Z >= 0.0) then
            let mutable d = 2.0 ** float (e-1)
            while b.Max.AnyGreater (V3d.III * d) || b.Min.AnySmaller(-V3d.III * d) do
                e <- e + 1
                d <- d * 2.0
            
            Cell(System.Int64.MaxValue, System.Int64.MaxValue, System.Int64.MaxValue, e)
        else
            let mutable d = 2.0 ** float e
            let mutable minCell = floor3 (b.Min / d) 
            let mutable maxCell = floor3 (b.Max / d) 
            while minCell <> maxCell do
                d <- d * 2.0
                e <- e + 1
                minCell <- floor3 (b.Min / d) 
                maxCell <- floor3 (b.Max / d) 

            Cell(int64 minCell.X, int64 minCell.Y, int64 minCell.Z, e)

