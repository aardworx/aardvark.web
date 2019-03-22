namespace Aardvark.Base

open Aardvark.Base

type Trafo3d(forward : M44d, backward : M44d) =
    member x.Forward = forward
    member x.Backward = backward

    member x.Inverse = Trafo3d(backward, forward)

    static member (*) (l : Trafo3d, r : Trafo3d) = Trafo3d(r.Forward * l.Forward, l.Backward * r.Backward)
    static member Identity = Trafo3d(M44d.Identity, M44d.Identity)
    static member Translation (t : V3d) = Trafo3d(M44d.Translation t, M44d.Translation -t)
    static member Translation (x : float, y : float, z : float) = Trafo3d.Translation(V3d(x,y,z))
    static member Rotation (axis : V3d, angle : float) = Trafo3d(M44d.Rotation(axis, angle), M44d.Rotation(axis, -angle))
    static member RotationX (angle : float) = Trafo3d(M44d.RotationX(angle), M44d.RotationX(-angle))
    static member RotationY (angle : float) = Trafo3d(M44d.RotationY(angle), M44d.RotationY(-angle))
    static member RotationZ (angle : float) = Trafo3d(M44d.RotationZ(angle), M44d.RotationZ(-angle))
    static member Scale (scale : V3d) = Trafo3d(M44d.Scale scale, M44d.Scale (1.0 / scale))
    static member Scale (sx : float, sy : float, sz : float) = Trafo3d.Scale(V3d(sx,sy,sz))
    static member Scale (s : float) = Trafo3d.Scale(V3d(s,s,s))
    static member FromBasis (xAxis : V3d, yAxis : V3d, zAxis : V3d, origin : V3d) =
        let fw =
            M44d(
                xAxis.X, yAxis.X, zAxis.X, origin.X,
                xAxis.Y, yAxis.Y, zAxis.Y, origin.Y,
                xAxis.Z, yAxis.Z, zAxis.Z, origin.Z,
                0.0, 0.0, 0.0, 1.0
            )
        Trafo3d(fw, fw.Inverse)

    new (forward : M44d) = Trafo3d(forward, forward.Inverse)
    override x.ToString() = sprintf "[%s, %s]" (string forward) (string backward)
    override x.GetHashCode() = forward.GetHashCode()
    override x.Equals o =
        match o with
        | :? Trafo3d as o -> forward = o.Forward
        | _ -> false

    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? Trafo3d as o -> compare forward o.Forward
            | _ -> failwith "uncomparable"