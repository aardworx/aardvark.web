namespace Aardvark.Base

open Aardvark.Base
open Fable.Core
open Aardvark.Import.JS

type M22d(m00 : float, m01 : float, m10 : float, m11 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m10 = m10
    let mutable m11 = m11
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member x.Rows = 2
    member x.Cols = 2
    member x.R0 : V2d = V2d(m00, m01)
    member x.R1 : V2d = V2d(m10, m11)
    member x.C0 : V2d = V2d(m00, m10)
    member x.C1 : V2d = V2d(m01, m11)
    static member Zero = M22d( 0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M22d( 1.0 ,  0.0 ,  0.0 ,  1.0 )
    static member FromCols(c0 : V2d, c1 : V2d) : M22d = 
        M22d(
            c0.X, c1.X, 
            c0.Y, c1.Y
        )
    static member FromRows(r0 : V2d, r1 : V2d) : M22d = 
        M22d(
            r0.X, r0.Y, 
            r1.X, r1.Y
        )
    static member (*) (l : M22d, r : M22d) : M22d =
        M22d(
            l.M00 * r.M00 + l.M01 * r.M10,
            l.M00 * r.M01 + l.M01 * r.M11,

            l.M10 * r.M00 + l.M11 * r.M10,
            l.M10 * r.M01 + l.M11 * r.M11

        )
    static member (*) (l : M22d, r : M23d) : M23d =
        M23d(
            l.M00 * r.M00 + l.M01 * r.M10,
            l.M00 * r.M01 + l.M01 * r.M11,
            l.M00 * r.M02 + l.M01 * r.M12,

            l.M10 * r.M00 + l.M11 * r.M10,
            l.M10 * r.M01 + l.M11 * r.M11,
            l.M10 * r.M02 + l.M11 * r.M12

        )
    static member (+) (l : M22d, r : M22d) : M22d = 
        M22d(
            l.M00 + r.M00, l.M01 + r.M01, 
            l.M10 + r.M10, l.M11 + r.M11
        )
    static member (-) (l : M22d, r : M22d) : M22d = 
        M22d(
            l.M00 - r.M00, l.M01 - r.M01, 
            l.M10 - r.M10, l.M11 - r.M11
        )
    static member (*) (l : M22d, r : float) : M22d = 
        M22d(
            l.M00 * r, l.M01 * r, 
            l.M10 * r, l.M11 * r
        )
    static member (*) (l : float, r : M22d) : M22d = 
        M22d(
            l * r.M00, l * r.M01, 
            l * r.M10, l * r.M11
        )
    static member (*) (l : M22d, r : V2d) : V2d = 
        V2d(
            l.M00 * r.X + l.M01 * r.Y, 
            l.M10 * r.X + l.M11 * r.Y
        )
    member x.Transposed : M22d =
        M22d(
            m00, m10, 
            m01, m11
        )
    interface NoSRTP.IHasTransposed<M22d> with member x.Transposed = x.Transposed
    member x.Det : float = m00 * m11 - m01 * m10
    interface NoSRTP.IHasDet<float> with member x.Det = x.Det
    new(o : M23d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(o : M24d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(o : M32d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(o : M33d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(o : M34d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(o : M42d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(o : M43d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(o : M44d) = 
        M22d(
            o.M00, o.M01, 
            o.M10, o.M11
        )
    new(arr : Float32Array) = 
        M22d(
            float arr.[0], float arr.[1], 
            float arr.[2], float arr.[3]
        )
    new(arr : Float64Array) = 
        M22d(
            arr.[0], arr.[1], 
            arr.[2], arr.[3]
        )
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01
        arr.[index + 2] <- float32 m10; arr.[index + 3] <- float32 m11
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01
        arr.[index + 2] <- m10; arr.[index + 3] <- m11
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(4)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01
        arr.[2] <- float32 m10; arr.[3] <- float32 m11
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(4)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr
    member x.Inverse : M22d = 
        let lu = x.ToFloat64Array()
        let inv = Float64Array.Create(4)
        let perm = Microsoft.FSharp.Collections.Array.zeroCreate 2
        lu.LuFactorize(0, 1, 2, perm)
        lu.LuInverse(0, 1, 2, perm, inv, 0, 1, 2)
        M22d(inv)
    interface NoSRTP.IHasInverse<M22d> with member x.Inverse = x.Inverse
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m10.GetHashCode(), m11.GetHashCode())
    override __.Equals(o) = match o with | :? M22d as o -> m00 = o.M00 && m01 = o.M01 && m10 = o.M10 && m11 = o.M11 | _ -> false
    override __.ToString() = sprintf "[[%f, %f], [%f, %f]]" m00 m01 m10 m11
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M22d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                else compare m11 o.M11
            | _ -> failwith "uncomparable"
and M23d(m00 : float, m01 : float, m02 : float, m10 : float, m11 : float, m12 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m02 = m02
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m12 = m12
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M02
        with get() : float = m02
        and set(v : float) : unit = m02 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M12
        with get() : float = m12
        and set(v : float) : unit = m12 <- v
    member x.Rows = 2
    member x.Cols = 3
    member x.R0 : V3d = V3d(m00, m01, m02)
    member x.R1 : V3d = V3d(m10, m11, m12)
    member x.C0 : V2d = V2d(m00, m10)
    member x.C1 : V2d = V2d(m01, m11)
    member x.C2 : V2d = V2d(m02, m12)
    static member Zero = M23d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M23d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 )
    static member FromCols(c0 : V2d, c1 : V2d, c2 : V2d) : M23d = 
        M23d(
            c0.X, c1.X, c2.X, 
            c0.Y, c1.Y, c2.Y
        )
    static member FromRows(r0 : V3d, r1 : V3d) : M23d = 
        M23d(
            r0.X, r0.Y, r0.Z, 
            r1.X, r1.Y, r1.Z
        )
    static member (*) (l : M23d, r : M32d) : M22d =
        M22d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21

        )
    static member (*) (l : M23d, r : M33d) : M23d =
        M23d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22

        )
    static member (*) (l : M23d, r : M34d) : M24d =
        M24d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22,
            l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22,
            l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23

        )
    static member (+) (l : M23d, r : M23d) : M23d = 
        M23d(
            l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, 
            l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12
        )
    static member (-) (l : M23d, r : M23d) : M23d = 
        M23d(
            l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, 
            l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12
        )
    static member (*) (l : M23d, r : float) : M23d = 
        M23d(
            l.M00 * r, l.M01 * r, l.M02 * r, 
            l.M10 * r, l.M11 * r, l.M12 * r
        )
    static member (*) (l : float, r : M23d) : M23d = 
        M23d(
            l * r.M00, l * r.M01, l * r.M02, 
            l * r.M10, l * r.M11, l * r.M12
        )
    static member (*) (l : M23d, r : V3d) : V2d = 
        V2d(
            l.M00 * r.X + l.M01 * r.Y + l.M02 * r.Z, 
            l.M10 * r.X + l.M11 * r.Y + l.M12 * r.Z
        )
    member x.Transposed : M32d =
        M32d(
            m00, m10, 
            m01, m11, 
            m02, m12
        )
    interface NoSRTP.IHasTransposed<M32d> with member x.Transposed = x.Transposed
    new(o : M22d) = 
        M23d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 
        )
    new(o : M24d) = 
        M23d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12
        )
    new(o : M32d) = 
        M23d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 
        )
    new(o : M33d) = 
        M23d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12
        )
    new(o : M34d) = 
        M23d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12
        )
    new(o : M42d) = 
        M23d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 
        )
    new(o : M43d) = 
        M23d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12
        )
    new(o : M44d) = 
        M23d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12
        )
    new(arr : Float32Array) = 
        M23d(
            float arr.[0], float arr.[1], float arr.[2], 
            float arr.[3], float arr.[4], float arr.[5]
        )
    new(arr : Float64Array) = 
        M23d(
            arr.[0], arr.[1], arr.[2], 
            arr.[3], arr.[4], arr.[5]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01; arr.[index + 2] <- float32 m02
        arr.[index + 3] <- float32 m10; arr.[index + 4] <- float32 m11; arr.[index + 5] <- float32 m12
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01; arr.[index + 2] <- m02
        arr.[index + 3] <- m10; arr.[index + 4] <- m11; arr.[index + 5] <- m12
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(6)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01; arr.[2] <- float32 m02
        arr.[3] <- float32 m10; arr.[4] <- float32 m11; arr.[5] <- float32 m12
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(6)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m02.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m12.GetHashCode())
    override __.Equals(o) = match o with | :? M23d as o -> m00 = o.M00 && m01 = o.M01 && m02 = o.M02 && m10 = o.M10 && m11 = o.M11 && m12 = o.M12 | _ -> false
    override __.ToString() = sprintf "[[%f, %f, %f], [%f, %f, %f]]" m00 m01 m02 m10 m11 m12
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M23d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m02 o.M02; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                else compare m12 o.M12
            | _ -> failwith "uncomparable"
and M24d(m00 : float, m01 : float, m02 : float, m03 : float, m10 : float, m11 : float, m12 : float, m13 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m02 = m02
    let mutable m03 = m03
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m12 = m12
    let mutable m13 = m13
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M02
        with get() : float = m02
        and set(v : float) : unit = m02 <- v
    member __.M03
        with get() : float = m03
        and set(v : float) : unit = m03 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M12
        with get() : float = m12
        and set(v : float) : unit = m12 <- v
    member __.M13
        with get() : float = m13
        and set(v : float) : unit = m13 <- v
    member x.Rows = 2
    member x.Cols = 4
    member x.R0 : V4d = V4d(m00, m01, m02, m03)
    member x.R1 : V4d = V4d(m10, m11, m12, m13)
    member x.C0 : V2d = V2d(m00, m10)
    member x.C1 : V2d = V2d(m01, m11)
    member x.C2 : V2d = V2d(m02, m12)
    member x.C3 : V2d = V2d(m03, m13)
    static member Zero = M24d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M24d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 )
    static member FromCols(c0 : V2d, c1 : V2d, c2 : V2d, c3 : V2d) : M24d = 
        M24d(
            c0.X, c1.X, c2.X, c3.X, 
            c0.Y, c1.Y, c2.Y, c3.Y
        )
    static member FromRows(r0 : V4d, r1 : V4d) : M24d = 
        M24d(
            r0.X, r0.Y, r0.Z, r0.W, 
            r1.X, r1.Y, r1.Z, r1.W
        )
    static member (*) (l : M24d, r : M43d) : M23d =
        M23d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32

        )
    static member (*) (l : M24d, r : M44d) : M24d =
        M24d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32,
            l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23 + l.M03 * r.M33,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32,
            l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23 + l.M13 * r.M33

        )
    static member (+) (l : M24d, r : M24d) : M24d = 
        M24d(
            l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, l.M03 + r.M03, 
            l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12, l.M13 + r.M13
        )
    static member (-) (l : M24d, r : M24d) : M24d = 
        M24d(
            l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, l.M03 - r.M03, 
            l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12, l.M13 - r.M13
        )
    static member (*) (l : M24d, r : float) : M24d = 
        M24d(
            l.M00 * r, l.M01 * r, l.M02 * r, l.M03 * r, 
            l.M10 * r, l.M11 * r, l.M12 * r, l.M13 * r
        )
    static member (*) (l : float, r : M24d) : M24d = 
        M24d(
            l * r.M00, l * r.M01, l * r.M02, l * r.M03, 
            l * r.M10, l * r.M11, l * r.M12, l * r.M13
        )
    static member (*) (l : M24d, r : V4d) : V2d = 
        V2d(
            l.M00 * r.X + l.M01 * r.Y + l.M02 * r.Z + l.M03 * r.W, 
            l.M10 * r.X + l.M11 * r.Y + l.M12 * r.Z + l.M13 * r.W
        )
    member x.Transposed : M42d =
        M42d(
            m00, m10, 
            m01, m11, 
            m02, m12, 
            m03, m13
        )
    interface NoSRTP.IHasTransposed<M42d> with member x.Transposed = x.Transposed
    new(o : M22d) = 
        M24d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 
        )
    new(o : M23d) = 
        M24d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 
        )
    new(o : M32d) = 
        M24d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 
        )
    new(o : M33d) = 
        M24d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 
        )
    new(o : M34d) = 
        M24d(
            o.M00, o.M01, o.M02, o.M03, 
            o.M10, o.M11, o.M12, o.M13
        )
    new(o : M42d) = 
        M24d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 
        )
    new(o : M43d) = 
        M24d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 
        )
    new(o : M44d) = 
        M24d(
            o.M00, o.M01, o.M02, o.M03, 
            o.M10, o.M11, o.M12, o.M13
        )
    new(arr : Float32Array) = 
        M24d(
            float arr.[0], float arr.[1], float arr.[2], float arr.[3], 
            float arr.[4], float arr.[5], float arr.[6], float arr.[7]
        )
    new(arr : Float64Array) = 
        M24d(
            arr.[0], arr.[1], arr.[2], arr.[3], 
            arr.[4], arr.[5], arr.[6], arr.[7]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01; arr.[index + 2] <- float32 m02; arr.[index + 3] <- float32 m03
        arr.[index + 4] <- float32 m10; arr.[index + 5] <- float32 m11; arr.[index + 6] <- float32 m12; arr.[index + 7] <- float32 m13
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01; arr.[index + 2] <- m02; arr.[index + 3] <- m03
        arr.[index + 4] <- m10; arr.[index + 5] <- m11; arr.[index + 6] <- m12; arr.[index + 7] <- m13
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(8)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01; arr.[2] <- float32 m02; arr.[3] <- float32 m03
        arr.[4] <- float32 m10; arr.[5] <- float32 m11; arr.[6] <- float32 m12; arr.[7] <- float32 m13
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(8)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m02.GetHashCode(), m03.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m12.GetHashCode(), m13.GetHashCode())
    override __.Equals(o) = match o with | :? M24d as o -> m00 = o.M00 && m01 = o.M01 && m02 = o.M02 && m03 = o.M03 && m10 = o.M10 && m11 = o.M11 && m12 = o.M12 && m13 = o.M13 | _ -> false
    override __.ToString() = sprintf "[[%f, %f, %f, %f], [%f, %f, %f, %f]]" m00 m01 m02 m03 m10 m11 m12 m13
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M24d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m02 o.M02; a <> 0) then a
                elif (a <- compare m03 o.M03; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                elif (a <- compare m12 o.M12; a <> 0) then a
                else compare m13 o.M13
            | _ -> failwith "uncomparable"
and M32d(m00 : float, m01 : float, m10 : float, m11 : float, m20 : float, m21 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m20 = m20
    let mutable m21 = m21
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M20
        with get() : float = m20
        and set(v : float) : unit = m20 <- v
    member __.M21
        with get() : float = m21
        and set(v : float) : unit = m21 <- v
    member x.Rows = 3
    member x.Cols = 2
    member x.R0 : V2d = V2d(m00, m01)
    member x.R1 : V2d = V2d(m10, m11)
    member x.R2 : V2d = V2d(m20, m21)
    member x.C0 : V3d = V3d(m00, m10, m20)
    member x.C1 : V3d = V3d(m01, m11, m21)
    static member Zero = M32d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M32d( 1.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 )
    static member FromCols(c0 : V3d, c1 : V3d) : M32d = 
        M32d(
            c0.X, c1.X, 
            c0.Y, c1.Y, 
            c0.Z, c1.Z
        )
    static member FromRows(r0 : V2d, r1 : V2d, r2 : V2d) : M32d = 
        M32d(
            r0.X, r0.Y, 
            r1.X, r1.Y, 
            r2.X, r2.Y
        )
    static member (*) (l : M32d, r : M22d) : M32d =
        M32d(
            l.M00 * r.M00 + l.M01 * r.M10,
            l.M00 * r.M01 + l.M01 * r.M11,

            l.M10 * r.M00 + l.M11 * r.M10,
            l.M10 * r.M01 + l.M11 * r.M11,

            l.M20 * r.M00 + l.M21 * r.M10,
            l.M20 * r.M01 + l.M21 * r.M11

        )
    static member (*) (l : M32d, r : M23d) : M33d =
        M33d(
            l.M00 * r.M00 + l.M01 * r.M10,
            l.M00 * r.M01 + l.M01 * r.M11,
            l.M00 * r.M02 + l.M01 * r.M12,

            l.M10 * r.M00 + l.M11 * r.M10,
            l.M10 * r.M01 + l.M11 * r.M11,
            l.M10 * r.M02 + l.M11 * r.M12,

            l.M20 * r.M00 + l.M21 * r.M10,
            l.M20 * r.M01 + l.M21 * r.M11,
            l.M20 * r.M02 + l.M21 * r.M12

        )
    static member (+) (l : M32d, r : M32d) : M32d = 
        M32d(
            l.M00 + r.M00, l.M01 + r.M01, 
            l.M10 + r.M10, l.M11 + r.M11, 
            l.M20 + r.M20, l.M21 + r.M21
        )
    static member (-) (l : M32d, r : M32d) : M32d = 
        M32d(
            l.M00 - r.M00, l.M01 - r.M01, 
            l.M10 - r.M10, l.M11 - r.M11, 
            l.M20 - r.M20, l.M21 - r.M21
        )
    static member (*) (l : M32d, r : float) : M32d = 
        M32d(
            l.M00 * r, l.M01 * r, 
            l.M10 * r, l.M11 * r, 
            l.M20 * r, l.M21 * r
        )
    static member (*) (l : float, r : M32d) : M32d = 
        M32d(
            l * r.M00, l * r.M01, 
            l * r.M10, l * r.M11, 
            l * r.M20, l * r.M21
        )
    static member (*) (l : M32d, r : V2d) : V3d = 
        V3d(
            l.M00 * r.X + l.M01 * r.Y, 
            l.M10 * r.X + l.M11 * r.Y, 
            l.M20 * r.X + l.M21 * r.Y
        )
    member x.Transposed : M23d =
        M23d(
            m00, m10, m20, 
            m01, m11, m21
        )
    interface NoSRTP.IHasTransposed<M23d> with member x.Transposed = x.Transposed
    new(o : M22d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
             0.0 ,  0.0 
        )
    new(o : M23d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
             0.0 ,  0.0 
        )
    new(o : M24d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
             0.0 ,  0.0 
        )
    new(o : M33d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21
        )
    new(o : M34d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21
        )
    new(o : M42d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21
        )
    new(o : M43d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21
        )
    new(o : M44d) = 
        M32d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21
        )
    new(arr : Float32Array) = 
        M32d(
            float arr.[0], float arr.[1], 
            float arr.[2], float arr.[3], 
            float arr.[4], float arr.[5]
        )
    new(arr : Float64Array) = 
        M32d(
            arr.[0], arr.[1], 
            arr.[2], arr.[3], 
            arr.[4], arr.[5]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01
        arr.[index + 2] <- float32 m10; arr.[index + 3] <- float32 m11
        arr.[index + 4] <- float32 m20; arr.[index + 5] <- float32 m21
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01
        arr.[index + 2] <- m10; arr.[index + 3] <- m11
        arr.[index + 4] <- m20; arr.[index + 5] <- m21
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(6)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01
        arr.[2] <- float32 m10; arr.[3] <- float32 m11
        arr.[4] <- float32 m20; arr.[5] <- float32 m21
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(6)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr.[4] <- m20; arr.[5] <- m21
        arr
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m20.GetHashCode(), m21.GetHashCode())
    override __.Equals(o) = match o with | :? M32d as o -> m00 = o.M00 && m01 = o.M01 && m10 = o.M10 && m11 = o.M11 && m20 = o.M20 && m21 = o.M21 | _ -> false
    override __.ToString() = sprintf "[[%f, %f], [%f, %f], [%f, %f]]" m00 m01 m10 m11 m20 m21
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M32d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                elif (a <- compare m20 o.M20; a <> 0) then a
                else compare m21 o.M21
            | _ -> failwith "uncomparable"
and M33d(m00 : float, m01 : float, m02 : float, m10 : float, m11 : float, m12 : float, m20 : float, m21 : float, m22 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m02 = m02
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m12 = m12
    let mutable m20 = m20
    let mutable m21 = m21
    let mutable m22 = m22
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M02
        with get() : float = m02
        and set(v : float) : unit = m02 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M12
        with get() : float = m12
        and set(v : float) : unit = m12 <- v
    member __.M20
        with get() : float = m20
        and set(v : float) : unit = m20 <- v
    member __.M21
        with get() : float = m21
        and set(v : float) : unit = m21 <- v
    member __.M22
        with get() : float = m22
        and set(v : float) : unit = m22 <- v
    member x.Rows = 3
    member x.Cols = 3
    member x.R0 : V3d = V3d(m00, m01, m02)
    member x.R1 : V3d = V3d(m10, m11, m12)
    member x.R2 : V3d = V3d(m20, m21, m22)
    member x.C0 : V3d = V3d(m00, m10, m20)
    member x.C1 : V3d = V3d(m01, m11, m21)
    member x.C2 : V3d = V3d(m02, m12, m22)
    static member Zero = M33d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M33d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 )
    static member FromCols(c0 : V3d, c1 : V3d, c2 : V3d) : M33d = 
        M33d(
            c0.X, c1.X, c2.X, 
            c0.Y, c1.Y, c2.Y, 
            c0.Z, c1.Z, c2.Z
        )
    static member FromRows(r0 : V3d, r1 : V3d, r2 : V3d) : M33d = 
        M33d(
            r0.X, r0.Y, r0.Z, 
            r1.X, r1.Y, r1.Z, 
            r2.X, r2.Y, r2.Z
        )
    static member Rotation(angle : float) : M33d = 
        let c = cos angle
        let s = sin angle
        M33d(
            c, -s,  0.0 , 
            s, c,  0.0 , 
             0.0 ,  0.0 ,  1.0 
        )
    static member Scale(scale : V2d) : M33d = 
        M33d(
            scale.X,  0.0 ,  0.0 , 
             0.0 , scale.Y,  0.0 , 
             0.0 ,  0.0 ,  1.0 
        )
    static member Scale(scale : float) : M33d = 
        M33d(
            scale,  0.0 ,  0.0 , 
             0.0 , scale,  0.0 , 
             0.0 ,  0.0 ,  1.0 
        )
    static member Translation(offset : V2d) : M33d = 
        M33d(
             1.0 ,  0.0 , offset.X, 
             0.0 ,  1.0 , offset.Y, 
             0.0 ,  0.0 ,  1.0 
        )
    member x.TransformPos(pos : V2d) : V2d = 
        V2d(
            m00 * pos.X + m01 * pos.Y + m02, 
            m10 * pos.X + m11 * pos.Y + m12
        )
    member x.TransformDir(pos : V2d) : V2d = 
        V2d(
            m00 * pos.X + m01 * pos.Y, 
            m10 * pos.X + m11 * pos.Y
        )
    member x.TransformPosProj(pos : V2d) : V2d = 
        let w = m20 * pos.X + m21 * pos.Y + m22
        V2d(
            (m00 * pos.X + m01 * pos.Y + m02) / w, 
            (m10 * pos.X + m11 * pos.Y + m12) / w
        )
    static member (*) (l : M33d, r : M32d) : M32d =
        M32d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21

        )
    static member (*) (l : M33d, r : M33d) : M33d =
        M33d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22

        )
    static member (*) (l : M33d, r : M34d) : M34d =
        M34d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22,
            l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22,
            l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22,
            l.M20 * r.M03 + l.M21 * r.M13 + l.M22 * r.M23

        )
    static member (+) (l : M33d, r : M33d) : M33d = 
        M33d(
            l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, 
            l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12, 
            l.M20 + r.M20, l.M21 + r.M21, l.M22 + r.M22
        )
    static member (-) (l : M33d, r : M33d) : M33d = 
        M33d(
            l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, 
            l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12, 
            l.M20 - r.M20, l.M21 - r.M21, l.M22 - r.M22
        )
    static member (*) (l : M33d, r : float) : M33d = 
        M33d(
            l.M00 * r, l.M01 * r, l.M02 * r, 
            l.M10 * r, l.M11 * r, l.M12 * r, 
            l.M20 * r, l.M21 * r, l.M22 * r
        )
    static member (*) (l : float, r : M33d) : M33d = 
        M33d(
            l * r.M00, l * r.M01, l * r.M02, 
            l * r.M10, l * r.M11, l * r.M12, 
            l * r.M20, l * r.M21, l * r.M22
        )
    static member (*) (l : M33d, r : V3d) : V3d = 
        V3d(
            l.M00 * r.X + l.M01 * r.Y + l.M02 * r.Z, 
            l.M10 * r.X + l.M11 * r.Y + l.M12 * r.Z, 
            l.M20 * r.X + l.M21 * r.Y + l.M22 * r.Z
        )
    member x.Transposed : M33d =
        M33d(
            m00, m10, m20, 
            m01, m11, m21, 
            m02, m12, m22
        )
    interface NoSRTP.IHasTransposed<M33d> with member x.Transposed = x.Transposed
    member x.Det : float = 
        m00 * m11 * m22 + m01 * m12 * m20 + m02 * m10 * m21 - 
        m20 * m11 * m02 - m21 * m12 * m00 - m22 * m10 * m01
    interface NoSRTP.IHasDet<float> with member x.Det = x.Det
    new(o : M22d) = 
        M33d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 , 
             0.0 ,  0.0 ,  1.0 
        )
    new(o : M23d) = 
        M33d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
             0.0 ,  0.0 ,  1.0 
        )
    new(o : M24d) = 
        M33d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
             0.0 ,  0.0 ,  1.0 
        )
    new(o : M32d) = 
        M33d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 , 
            o.M20, o.M21,  1.0 
        )
    new(o : M34d) = 
        M33d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
            o.M20, o.M21, o.M22
        )
    new(o : M42d) = 
        M33d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 , 
            o.M20, o.M21,  1.0 
        )
    new(o : M43d) = 
        M33d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
            o.M20, o.M21, o.M22
        )
    new(o : M44d) = 
        M33d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
            o.M20, o.M21, o.M22
        )
    new(arr : Float32Array) = 
        M33d(
            float arr.[0], float arr.[1], float arr.[2], 
            float arr.[3], float arr.[4], float arr.[5], 
            float arr.[6], float arr.[7], float arr.[8]
        )
    new(arr : Float64Array) = 
        M33d(
            arr.[0], arr.[1], arr.[2], 
            arr.[3], arr.[4], arr.[5], 
            arr.[6], arr.[7], arr.[8]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01; arr.[index + 2] <- float32 m02
        arr.[index + 3] <- float32 m10; arr.[index + 4] <- float32 m11; arr.[index + 5] <- float32 m12
        arr.[index + 6] <- float32 m20; arr.[index + 7] <- float32 m21; arr.[index + 8] <- float32 m22
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01; arr.[index + 2] <- m02
        arr.[index + 3] <- m10; arr.[index + 4] <- m11; arr.[index + 5] <- m12
        arr.[index + 6] <- m20; arr.[index + 7] <- m21; arr.[index + 8] <- m22
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(9)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01; arr.[2] <- float32 m02
        arr.[3] <- float32 m10; arr.[4] <- float32 m11; arr.[5] <- float32 m12
        arr.[6] <- float32 m20; arr.[7] <- float32 m21; arr.[8] <- float32 m22
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(9)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr.[6] <- m20; arr.[7] <- m21; arr.[8] <- m22
        arr
    member x.Inverse : M33d = 
        let lu = x.ToFloat64Array()
        let inv = Float64Array.Create(9)
        let perm = Microsoft.FSharp.Collections.Array.zeroCreate 3
        lu.LuFactorize(0, 1, 3, perm)
        lu.LuInverse(0, 1, 3, perm, inv, 0, 1, 3)
        M33d(inv)
    interface NoSRTP.IHasInverse<M33d> with member x.Inverse = x.Inverse
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m02.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m12.GetHashCode(), m20.GetHashCode(), m21.GetHashCode(), m22.GetHashCode())
    override __.Equals(o) = match o with | :? M33d as o -> m00 = o.M00 && m01 = o.M01 && m02 = o.M02 && m10 = o.M10 && m11 = o.M11 && m12 = o.M12 && m20 = o.M20 && m21 = o.M21 && m22 = o.M22 | _ -> false
    override __.ToString() = sprintf "[[%f, %f, %f], [%f, %f, %f], [%f, %f, %f]]" m00 m01 m02 m10 m11 m12 m20 m21 m22
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M33d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m02 o.M02; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                elif (a <- compare m12 o.M12; a <> 0) then a
                elif (a <- compare m20 o.M20; a <> 0) then a
                elif (a <- compare m21 o.M21; a <> 0) then a
                else compare m22 o.M22
            | _ -> failwith "uncomparable"
and M34d(m00 : float, m01 : float, m02 : float, m03 : float, m10 : float, m11 : float, m12 : float, m13 : float, m20 : float, m21 : float, m22 : float, m23 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m02 = m02
    let mutable m03 = m03
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m12 = m12
    let mutable m13 = m13
    let mutable m20 = m20
    let mutable m21 = m21
    let mutable m22 = m22
    let mutable m23 = m23
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M02
        with get() : float = m02
        and set(v : float) : unit = m02 <- v
    member __.M03
        with get() : float = m03
        and set(v : float) : unit = m03 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M12
        with get() : float = m12
        and set(v : float) : unit = m12 <- v
    member __.M13
        with get() : float = m13
        and set(v : float) : unit = m13 <- v
    member __.M20
        with get() : float = m20
        and set(v : float) : unit = m20 <- v
    member __.M21
        with get() : float = m21
        and set(v : float) : unit = m21 <- v
    member __.M22
        with get() : float = m22
        and set(v : float) : unit = m22 <- v
    member __.M23
        with get() : float = m23
        and set(v : float) : unit = m23 <- v
    member x.Rows = 3
    member x.Cols = 4
    member x.R0 : V4d = V4d(m00, m01, m02, m03)
    member x.R1 : V4d = V4d(m10, m11, m12, m13)
    member x.R2 : V4d = V4d(m20, m21, m22, m23)
    member x.C0 : V3d = V3d(m00, m10, m20)
    member x.C1 : V3d = V3d(m01, m11, m21)
    member x.C2 : V3d = V3d(m02, m12, m22)
    member x.C3 : V3d = V3d(m03, m13, m23)
    static member Zero = M34d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M34d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 )
    static member FromCols(c0 : V3d, c1 : V3d, c2 : V3d, c3 : V3d) : M34d = 
        M34d(
            c0.X, c1.X, c2.X, c3.X, 
            c0.Y, c1.Y, c2.Y, c3.Y, 
            c0.Z, c1.Z, c2.Z, c3.Z
        )
    static member FromRows(r0 : V4d, r1 : V4d, r2 : V4d) : M34d = 
        M34d(
            r0.X, r0.Y, r0.Z, r0.W, 
            r1.X, r1.Y, r1.Z, r1.W, 
            r2.X, r2.Y, r2.Z, r2.W
        )
    static member RotationZ(angle : float) : M34d = 
        let c = cos angle
        let s = sin angle
        M34d(c, s,  0.0 , -s, c,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 )
    static member RotationY(angle : float) : M34d = 
        let c = cos angle
        let s = sin angle
        M34d(c,  0.0 , s,  0.0 ,  1.0 ,  0.0 , -s,  0.0 , c,  0.0 ,  0.0 ,  0.0 )
    static member RotationX(angle : float) : M34d = 
        let c = cos angle
        let s = sin angle
        M34d( 1.0 ,  0.0 ,  0.0 ,  0.0 , c, s,  0.0 , -s, c,  0.0 ,  0.0 ,  0.0 )
    static member Rotation(axis : V3d, angle : float) : M34d =
        let axis = axis.Normalized
        let n1 = axis.X
        let n2 = axis.Y
        let n3 = axis.Z
        let n12 = axis.X * axis.Y
        let n13 = axis.X * axis.Z
        let n23 = axis.Y * axis.Z
        let c = cos angle
        let s = sin angle
        let ci = 1.0 - c
        M34d(
            c+n1*n1*ci, n12*ci+n3*s, n13*ci-n2*s,  0.0 , 
            n12*ci-n3*s, c+n2*n2*ci, n23*ci+n1*s,  0.0 , 
            n13*ci+n2*s, n23*ci-n1*s, c+n3*n3*ci,  0.0 
        )
    static member ViewTrafo(location : V3d, right : V3d, up : V3d, normal : V3d) =
        M34d(
            right.X, right.Y, right.Z, -location.Dot(right), 
            up.X, up.Y, up.Z, -location.Dot(up), 
            normal.X, normal.Y, normal.Z, -location.Dot(normal)
        )
    static member Scale(scale : V3d) : M34d = 
        M34d(
            scale.X,  0.0 ,  0.0 ,  0.0 , 
             0.0 , scale.Y,  0.0 ,  0.0 , 
             0.0 ,  0.0 , scale.Z,  0.0 
        )
    static member Scale(scale : float) : M34d = 
        M34d(
            scale,  0.0 ,  0.0 ,  0.0 , 
             0.0 , scale,  0.0 ,  0.0 , 
             0.0 ,  0.0 , scale,  0.0 
        )
    static member Translation(offset : V3d) : M34d = 
        M34d(
             1.0 ,  0.0 ,  0.0 , offset.X, 
             0.0 ,  1.0 ,  0.0 , offset.Y, 
             0.0 ,  0.0 ,  1.0 , offset.Z
        )
    member x.TransformPos(pos : V3d) : V3d = 
        V3d(
            m00 * pos.X + m01 * pos.Y + m02 * pos.Z + m03, 
            m10 * pos.X + m11 * pos.Y + m12 * pos.Z + m13, 
            m20 * pos.X + m21 * pos.Y + m22 * pos.Z + m23
        )
    member x.TransformDir(pos : V3d) : V3d = 
        V3d(
            m00 * pos.X + m01 * pos.Y + m02 * pos.Z, 
            m10 * pos.X + m11 * pos.Y + m12 * pos.Z, 
            m20 * pos.X + m21 * pos.Y + m22 * pos.Z
        )
    static member (*) (l : M34d, r : M43d) : M33d =
        M33d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20 + l.M23 * r.M30,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21 + l.M23 * r.M31,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22 + l.M23 * r.M32

        )
    static member (*) (l : M34d, r : M44d) : M34d =
        M34d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32,
            l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23 + l.M03 * r.M33,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32,
            l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23 + l.M13 * r.M33,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20 + l.M23 * r.M30,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21 + l.M23 * r.M31,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22 + l.M23 * r.M32,
            l.M20 * r.M03 + l.M21 * r.M13 + l.M22 * r.M23 + l.M23 * r.M33

        )
    static member (+) (l : M34d, r : M34d) : M34d = 
        M34d(
            l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, l.M03 + r.M03, 
            l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12, l.M13 + r.M13, 
            l.M20 + r.M20, l.M21 + r.M21, l.M22 + r.M22, l.M23 + r.M23
        )
    static member (-) (l : M34d, r : M34d) : M34d = 
        M34d(
            l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, l.M03 - r.M03, 
            l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12, l.M13 - r.M13, 
            l.M20 - r.M20, l.M21 - r.M21, l.M22 - r.M22, l.M23 - r.M23
        )
    static member (*) (l : M34d, r : float) : M34d = 
        M34d(
            l.M00 * r, l.M01 * r, l.M02 * r, l.M03 * r, 
            l.M10 * r, l.M11 * r, l.M12 * r, l.M13 * r, 
            l.M20 * r, l.M21 * r, l.M22 * r, l.M23 * r
        )
    static member (*) (l : float, r : M34d) : M34d = 
        M34d(
            l * r.M00, l * r.M01, l * r.M02, l * r.M03, 
            l * r.M10, l * r.M11, l * r.M12, l * r.M13, 
            l * r.M20, l * r.M21, l * r.M22, l * r.M23
        )
    static member (*) (l : M34d, r : V4d) : V3d = 
        V3d(
            l.M00 * r.X + l.M01 * r.Y + l.M02 * r.Z + l.M03 * r.W, 
            l.M10 * r.X + l.M11 * r.Y + l.M12 * r.Z + l.M13 * r.W, 
            l.M20 * r.X + l.M21 * r.Y + l.M22 * r.Z + l.M23 * r.W
        )
    member x.Transposed : M43d =
        M43d(
            m00, m10, m20, 
            m01, m11, m21, 
            m02, m12, m22, 
            m03, m13, m23
        )
    interface NoSRTP.IHasTransposed<M43d> with member x.Transposed = x.Transposed
    new(o : M22d) = 
        M34d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 , 
             0.0 ,  0.0 ,  1.0 ,  0.0 
        )
    new(o : M23d) = 
        M34d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 , 
             0.0 ,  0.0 ,  1.0 ,  0.0 
        )
    new(o : M24d) = 
        M34d(
            o.M00, o.M01, o.M02, o.M03, 
            o.M10, o.M11, o.M12, o.M13, 
             0.0 ,  0.0 ,  1.0 ,  0.0 
        )
    new(o : M32d) = 
        M34d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 , 
            o.M20, o.M21,  1.0 ,  0.0 
        )
    new(o : M33d) = 
        M34d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 , 
            o.M20, o.M21, o.M22,  0.0 
        )
    new(o : M42d) = 
        M34d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 , 
            o.M20, o.M21,  1.0 ,  0.0 
        )
    new(o : M43d) = 
        M34d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 , 
            o.M20, o.M21, o.M22,  0.0 
        )
    new(o : M44d) = 
        M34d(
            o.M00, o.M01, o.M02, o.M03, 
            o.M10, o.M11, o.M12, o.M13, 
            o.M20, o.M21, o.M22, o.M23
        )
    new(arr : Float32Array) = 
        M34d(
            float arr.[0], float arr.[1], float arr.[2], float arr.[3], 
            float arr.[4], float arr.[5], float arr.[6], float arr.[7], 
            float arr.[8], float arr.[9], float arr.[10], float arr.[11]
        )
    new(arr : Float64Array) = 
        M34d(
            arr.[0], arr.[1], arr.[2], arr.[3], 
            arr.[4], arr.[5], arr.[6], arr.[7], 
            arr.[8], arr.[9], arr.[10], arr.[11]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.UpperLeftM33() : M33d = M33d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01; arr.[index + 2] <- float32 m02; arr.[index + 3] <- float32 m03
        arr.[index + 4] <- float32 m10; arr.[index + 5] <- float32 m11; arr.[index + 6] <- float32 m12; arr.[index + 7] <- float32 m13
        arr.[index + 8] <- float32 m20; arr.[index + 9] <- float32 m21; arr.[index + 10] <- float32 m22; arr.[index + 11] <- float32 m23
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01; arr.[index + 2] <- m02; arr.[index + 3] <- m03
        arr.[index + 4] <- m10; arr.[index + 5] <- m11; arr.[index + 6] <- m12; arr.[index + 7] <- m13
        arr.[index + 8] <- m20; arr.[index + 9] <- m21; arr.[index + 10] <- m22; arr.[index + 11] <- m23
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(12)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01; arr.[2] <- float32 m02; arr.[3] <- float32 m03
        arr.[4] <- float32 m10; arr.[5] <- float32 m11; arr.[6] <- float32 m12; arr.[7] <- float32 m13
        arr.[8] <- float32 m20; arr.[9] <- float32 m21; arr.[10] <- float32 m22; arr.[11] <- float32 m23
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(12)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr.[8] <- m20; arr.[9] <- m21; arr.[10] <- m22; arr.[11] <- m23
        arr
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m02.GetHashCode(), m03.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m12.GetHashCode(), m13.GetHashCode(), m20.GetHashCode(), m21.GetHashCode(), m22.GetHashCode(), m23.GetHashCode())
    override __.Equals(o) = match o with | :? M34d as o -> m00 = o.M00 && m01 = o.M01 && m02 = o.M02 && m03 = o.M03 && m10 = o.M10 && m11 = o.M11 && m12 = o.M12 && m13 = o.M13 && m20 = o.M20 && m21 = o.M21 && m22 = o.M22 && m23 = o.M23 | _ -> false
    override __.ToString() = sprintf "[[%f, %f, %f, %f], [%f, %f, %f, %f], [%f, %f, %f, %f]]" m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M34d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m02 o.M02; a <> 0) then a
                elif (a <- compare m03 o.M03; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                elif (a <- compare m12 o.M12; a <> 0) then a
                elif (a <- compare m13 o.M13; a <> 0) then a
                elif (a <- compare m20 o.M20; a <> 0) then a
                elif (a <- compare m21 o.M21; a <> 0) then a
                elif (a <- compare m22 o.M22; a <> 0) then a
                else compare m23 o.M23
            | _ -> failwith "uncomparable"
and M42d(m00 : float, m01 : float, m10 : float, m11 : float, m20 : float, m21 : float, m30 : float, m31 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m20 = m20
    let mutable m21 = m21
    let mutable m30 = m30
    let mutable m31 = m31
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M20
        with get() : float = m20
        and set(v : float) : unit = m20 <- v
    member __.M21
        with get() : float = m21
        and set(v : float) : unit = m21 <- v
    member __.M30
        with get() : float = m30
        and set(v : float) : unit = m30 <- v
    member __.M31
        with get() : float = m31
        and set(v : float) : unit = m31 <- v
    member x.Rows = 4
    member x.Cols = 2
    member x.R0 : V2d = V2d(m00, m01)
    member x.R1 : V2d = V2d(m10, m11)
    member x.R2 : V2d = V2d(m20, m21)
    member x.R3 : V2d = V2d(m30, m31)
    member x.C0 : V4d = V4d(m00, m10, m20, m30)
    member x.C1 : V4d = V4d(m01, m11, m21, m31)
    static member Zero = M42d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M42d( 1.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member FromCols(c0 : V4d, c1 : V4d) : M42d = 
        M42d(
            c0.X, c1.X, 
            c0.Y, c1.Y, 
            c0.Z, c1.Z, 
            c0.W, c1.W
        )
    static member FromRows(r0 : V2d, r1 : V2d, r2 : V2d, r3 : V2d) : M42d = 
        M42d(
            r0.X, r0.Y, 
            r1.X, r1.Y, 
            r2.X, r2.Y, 
            r3.X, r3.Y
        )
    static member (*) (l : M42d, r : M22d) : M42d =
        M42d(
            l.M00 * r.M00 + l.M01 * r.M10,
            l.M00 * r.M01 + l.M01 * r.M11,

            l.M10 * r.M00 + l.M11 * r.M10,
            l.M10 * r.M01 + l.M11 * r.M11,

            l.M20 * r.M00 + l.M21 * r.M10,
            l.M20 * r.M01 + l.M21 * r.M11,

            l.M30 * r.M00 + l.M31 * r.M10,
            l.M30 * r.M01 + l.M31 * r.M11

        )
    static member (*) (l : M42d, r : M23d) : M43d =
        M43d(
            l.M00 * r.M00 + l.M01 * r.M10,
            l.M00 * r.M01 + l.M01 * r.M11,
            l.M00 * r.M02 + l.M01 * r.M12,

            l.M10 * r.M00 + l.M11 * r.M10,
            l.M10 * r.M01 + l.M11 * r.M11,
            l.M10 * r.M02 + l.M11 * r.M12,

            l.M20 * r.M00 + l.M21 * r.M10,
            l.M20 * r.M01 + l.M21 * r.M11,
            l.M20 * r.M02 + l.M21 * r.M12,

            l.M30 * r.M00 + l.M31 * r.M10,
            l.M30 * r.M01 + l.M31 * r.M11,
            l.M30 * r.M02 + l.M31 * r.M12

        )
    static member (+) (l : M42d, r : M42d) : M42d = 
        M42d(
            l.M00 + r.M00, l.M01 + r.M01, 
            l.M10 + r.M10, l.M11 + r.M11, 
            l.M20 + r.M20, l.M21 + r.M21, 
            l.M30 + r.M30, l.M31 + r.M31
        )
    static member (-) (l : M42d, r : M42d) : M42d = 
        M42d(
            l.M00 - r.M00, l.M01 - r.M01, 
            l.M10 - r.M10, l.M11 - r.M11, 
            l.M20 - r.M20, l.M21 - r.M21, 
            l.M30 - r.M30, l.M31 - r.M31
        )
    static member (*) (l : M42d, r : float) : M42d = 
        M42d(
            l.M00 * r, l.M01 * r, 
            l.M10 * r, l.M11 * r, 
            l.M20 * r, l.M21 * r, 
            l.M30 * r, l.M31 * r
        )
    static member (*) (l : float, r : M42d) : M42d = 
        M42d(
            l * r.M00, l * r.M01, 
            l * r.M10, l * r.M11, 
            l * r.M20, l * r.M21, 
            l * r.M30, l * r.M31
        )
    static member (*) (l : M42d, r : V2d) : V4d = 
        V4d(
            l.M00 * r.X + l.M01 * r.Y, 
            l.M10 * r.X + l.M11 * r.Y, 
            l.M20 * r.X + l.M21 * r.Y, 
            l.M30 * r.X + l.M31 * r.Y
        )
    member x.Transposed : M24d =
        M24d(
            m00, m10, m20, m30, 
            m01, m11, m21, m31
        )
    interface NoSRTP.IHasTransposed<M24d> with member x.Transposed = x.Transposed
    new(o : M22d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
             0.0 ,  0.0 , 
             0.0 ,  0.0 
        )
    new(o : M23d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
             0.0 ,  0.0 , 
             0.0 ,  0.0 
        )
    new(o : M24d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
             0.0 ,  0.0 , 
             0.0 ,  0.0 
        )
    new(o : M32d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21, 
             0.0 ,  0.0 
        )
    new(o : M33d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21, 
             0.0 ,  0.0 
        )
    new(o : M34d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21, 
             0.0 ,  0.0 
        )
    new(o : M43d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21, 
            o.M30, o.M31
        )
    new(o : M44d) = 
        M42d(
            o.M00, o.M01, 
            o.M10, o.M11, 
            o.M20, o.M21, 
            o.M30, o.M31
        )
    new(arr : Float32Array) = 
        M42d(
            float arr.[0], float arr.[1], 
            float arr.[2], float arr.[3], 
            float arr.[4], float arr.[5], 
            float arr.[6], float arr.[7]
        )
    new(arr : Float64Array) = 
        M42d(
            arr.[0], arr.[1], 
            arr.[2], arr.[3], 
            arr.[4], arr.[5], 
            arr.[6], arr.[7]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01
        arr.[index + 2] <- float32 m10; arr.[index + 3] <- float32 m11
        arr.[index + 4] <- float32 m20; arr.[index + 5] <- float32 m21
        arr.[index + 6] <- float32 m30; arr.[index + 7] <- float32 m31
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01
        arr.[index + 2] <- m10; arr.[index + 3] <- m11
        arr.[index + 4] <- m20; arr.[index + 5] <- m21
        arr.[index + 6] <- m30; arr.[index + 7] <- m31
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(8)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01
        arr.[2] <- float32 m10; arr.[3] <- float32 m11
        arr.[4] <- float32 m20; arr.[5] <- float32 m21
        arr.[6] <- float32 m30; arr.[7] <- float32 m31
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(8)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr.[4] <- m20; arr.[5] <- m21
        arr.[6] <- m30; arr.[7] <- m31
        arr
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m20.GetHashCode(), m21.GetHashCode(), m30.GetHashCode(), m31.GetHashCode())
    override __.Equals(o) = match o with | :? M42d as o -> m00 = o.M00 && m01 = o.M01 && m10 = o.M10 && m11 = o.M11 && m20 = o.M20 && m21 = o.M21 && m30 = o.M30 && m31 = o.M31 | _ -> false
    override __.ToString() = sprintf "[[%f, %f], [%f, %f], [%f, %f], [%f, %f]]" m00 m01 m10 m11 m20 m21 m30 m31
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M42d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                elif (a <- compare m20 o.M20; a <> 0) then a
                elif (a <- compare m21 o.M21; a <> 0) then a
                elif (a <- compare m30 o.M30; a <> 0) then a
                else compare m31 o.M31
            | _ -> failwith "uncomparable"
and M43d(m00 : float, m01 : float, m02 : float, m10 : float, m11 : float, m12 : float, m20 : float, m21 : float, m22 : float, m30 : float, m31 : float, m32 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m02 = m02
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m12 = m12
    let mutable m20 = m20
    let mutable m21 = m21
    let mutable m22 = m22
    let mutable m30 = m30
    let mutable m31 = m31
    let mutable m32 = m32
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M02
        with get() : float = m02
        and set(v : float) : unit = m02 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M12
        with get() : float = m12
        and set(v : float) : unit = m12 <- v
    member __.M20
        with get() : float = m20
        and set(v : float) : unit = m20 <- v
    member __.M21
        with get() : float = m21
        and set(v : float) : unit = m21 <- v
    member __.M22
        with get() : float = m22
        and set(v : float) : unit = m22 <- v
    member __.M30
        with get() : float = m30
        and set(v : float) : unit = m30 <- v
    member __.M31
        with get() : float = m31
        and set(v : float) : unit = m31 <- v
    member __.M32
        with get() : float = m32
        and set(v : float) : unit = m32 <- v
    member x.Rows = 4
    member x.Cols = 3
    member x.R0 : V3d = V3d(m00, m01, m02)
    member x.R1 : V3d = V3d(m10, m11, m12)
    member x.R2 : V3d = V3d(m20, m21, m22)
    member x.R3 : V3d = V3d(m30, m31, m32)
    member x.C0 : V4d = V4d(m00, m10, m20, m30)
    member x.C1 : V4d = V4d(m01, m11, m21, m31)
    member x.C2 : V4d = V4d(m02, m12, m22, m32)
    static member Zero = M43d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M43d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 )
    static member FromCols(c0 : V4d, c1 : V4d, c2 : V4d) : M43d = 
        M43d(
            c0.X, c1.X, c2.X, 
            c0.Y, c1.Y, c2.Y, 
            c0.Z, c1.Z, c2.Z, 
            c0.W, c1.W, c2.W
        )
    static member FromRows(r0 : V3d, r1 : V3d, r2 : V3d, r3 : V3d) : M43d = 
        M43d(
            r0.X, r0.Y, r0.Z, 
            r1.X, r1.Y, r1.Z, 
            r2.X, r2.Y, r2.Z, 
            r3.X, r3.Y, r3.Z
        )
    static member (*) (l : M43d, r : M32d) : M42d =
        M42d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21,

            l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20,
            l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21

        )
    static member (*) (l : M43d, r : M33d) : M43d =
        M43d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22,

            l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20,
            l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21,
            l.M30 * r.M02 + l.M31 * r.M12 + l.M32 * r.M22

        )
    static member (*) (l : M43d, r : M34d) : M44d =
        M44d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22,
            l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22,
            l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22,
            l.M20 * r.M03 + l.M21 * r.M13 + l.M22 * r.M23,

            l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20,
            l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21,
            l.M30 * r.M02 + l.M31 * r.M12 + l.M32 * r.M22,
            l.M30 * r.M03 + l.M31 * r.M13 + l.M32 * r.M23

        )
    static member (+) (l : M43d, r : M43d) : M43d = 
        M43d(
            l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, 
            l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12, 
            l.M20 + r.M20, l.M21 + r.M21, l.M22 + r.M22, 
            l.M30 + r.M30, l.M31 + r.M31, l.M32 + r.M32
        )
    static member (-) (l : M43d, r : M43d) : M43d = 
        M43d(
            l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, 
            l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12, 
            l.M20 - r.M20, l.M21 - r.M21, l.M22 - r.M22, 
            l.M30 - r.M30, l.M31 - r.M31, l.M32 - r.M32
        )
    static member (*) (l : M43d, r : float) : M43d = 
        M43d(
            l.M00 * r, l.M01 * r, l.M02 * r, 
            l.M10 * r, l.M11 * r, l.M12 * r, 
            l.M20 * r, l.M21 * r, l.M22 * r, 
            l.M30 * r, l.M31 * r, l.M32 * r
        )
    static member (*) (l : float, r : M43d) : M43d = 
        M43d(
            l * r.M00, l * r.M01, l * r.M02, 
            l * r.M10, l * r.M11, l * r.M12, 
            l * r.M20, l * r.M21, l * r.M22, 
            l * r.M30, l * r.M31, l * r.M32
        )
    static member (*) (l : M43d, r : V3d) : V4d = 
        V4d(
            l.M00 * r.X + l.M01 * r.Y + l.M02 * r.Z, 
            l.M10 * r.X + l.M11 * r.Y + l.M12 * r.Z, 
            l.M20 * r.X + l.M21 * r.Y + l.M22 * r.Z, 
            l.M30 * r.X + l.M31 * r.Y + l.M32 * r.Z
        )
    member x.Transposed : M34d =
        M34d(
            m00, m10, m20, m30, 
            m01, m11, m21, m31, 
            m02, m12, m22, m32
        )
    interface NoSRTP.IHasTransposed<M34d> with member x.Transposed = x.Transposed
    new(o : M22d) = 
        M43d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 , 
             0.0 ,  0.0 ,  1.0 , 
             0.0 ,  0.0 ,  0.0 
        )
    new(o : M23d) = 
        M43d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
             0.0 ,  0.0 ,  1.0 , 
             0.0 ,  0.0 ,  0.0 
        )
    new(o : M24d) = 
        M43d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
             0.0 ,  0.0 ,  1.0 , 
             0.0 ,  0.0 ,  0.0 
        )
    new(o : M32d) = 
        M43d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 , 
            o.M20, o.M21,  1.0 , 
             0.0 ,  0.0 ,  0.0 
        )
    new(o : M33d) = 
        M43d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
            o.M20, o.M21, o.M22, 
             0.0 ,  0.0 ,  0.0 
        )
    new(o : M34d) = 
        M43d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
            o.M20, o.M21, o.M22, 
             0.0 ,  0.0 ,  0.0 
        )
    new(o : M42d) = 
        M43d(
            o.M00, o.M01,  0.0 , 
            o.M10, o.M11,  0.0 , 
            o.M20, o.M21,  1.0 , 
            o.M30, o.M31,  0.0 
        )
    new(o : M44d) = 
        M43d(
            o.M00, o.M01, o.M02, 
            o.M10, o.M11, o.M12, 
            o.M20, o.M21, o.M22, 
            o.M30, o.M31, o.M32
        )
    new(arr : Float32Array) = 
        M43d(
            float arr.[0], float arr.[1], float arr.[2], 
            float arr.[3], float arr.[4], float arr.[5], 
            float arr.[6], float arr.[7], float arr.[8], 
            float arr.[9], float arr.[10], float arr.[11]
        )
    new(arr : Float64Array) = 
        M43d(
            arr.[0], arr.[1], arr.[2], 
            arr.[3], arr.[4], arr.[5], 
            arr.[6], arr.[7], arr.[8], 
            arr.[9], arr.[10], arr.[11]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.UpperLeftM33() : M33d = M33d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01; arr.[index + 2] <- float32 m02
        arr.[index + 3] <- float32 m10; arr.[index + 4] <- float32 m11; arr.[index + 5] <- float32 m12
        arr.[index + 6] <- float32 m20; arr.[index + 7] <- float32 m21; arr.[index + 8] <- float32 m22
        arr.[index + 9] <- float32 m30; arr.[index + 10] <- float32 m31; arr.[index + 11] <- float32 m32
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01; arr.[index + 2] <- m02
        arr.[index + 3] <- m10; arr.[index + 4] <- m11; arr.[index + 5] <- m12
        arr.[index + 6] <- m20; arr.[index + 7] <- m21; arr.[index + 8] <- m22
        arr.[index + 9] <- m30; arr.[index + 10] <- m31; arr.[index + 11] <- m32
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(12)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01; arr.[2] <- float32 m02
        arr.[3] <- float32 m10; arr.[4] <- float32 m11; arr.[5] <- float32 m12
        arr.[6] <- float32 m20; arr.[7] <- float32 m21; arr.[8] <- float32 m22
        arr.[9] <- float32 m30; arr.[10] <- float32 m31; arr.[11] <- float32 m32
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(12)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr.[6] <- m20; arr.[7] <- m21; arr.[8] <- m22
        arr.[9] <- m30; arr.[10] <- m31; arr.[11] <- m32
        arr
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m02.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m12.GetHashCode(), m20.GetHashCode(), m21.GetHashCode(), m22.GetHashCode(), m30.GetHashCode(), m31.GetHashCode(), m32.GetHashCode())
    override __.Equals(o) = match o with | :? M43d as o -> m00 = o.M00 && m01 = o.M01 && m02 = o.M02 && m10 = o.M10 && m11 = o.M11 && m12 = o.M12 && m20 = o.M20 && m21 = o.M21 && m22 = o.M22 && m30 = o.M30 && m31 = o.M31 && m32 = o.M32 | _ -> false
    override __.ToString() = sprintf "[[%f, %f, %f], [%f, %f, %f], [%f, %f, %f], [%f, %f, %f]]" m00 m01 m02 m10 m11 m12 m20 m21 m22 m30 m31 m32
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M43d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m02 o.M02; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                elif (a <- compare m12 o.M12; a <> 0) then a
                elif (a <- compare m20 o.M20; a <> 0) then a
                elif (a <- compare m21 o.M21; a <> 0) then a
                elif (a <- compare m22 o.M22; a <> 0) then a
                elif (a <- compare m30 o.M30; a <> 0) then a
                elif (a <- compare m31 o.M31; a <> 0) then a
                else compare m32 o.M32
            | _ -> failwith "uncomparable"
and M44d(m00 : float, m01 : float, m02 : float, m03 : float, m10 : float, m11 : float, m12 : float, m13 : float, m20 : float, m21 : float, m22 : float, m23 : float, m30 : float, m31 : float, m32 : float, m33 : float) =
    let mutable m00 = m00
    let mutable m01 = m01
    let mutable m02 = m02
    let mutable m03 = m03
    let mutable m10 = m10
    let mutable m11 = m11
    let mutable m12 = m12
    let mutable m13 = m13
    let mutable m20 = m20
    let mutable m21 = m21
    let mutable m22 = m22
    let mutable m23 = m23
    let mutable m30 = m30
    let mutable m31 = m31
    let mutable m32 = m32
    let mutable m33 = m33
    member __.M00
        with get() : float = m00
        and set(v : float) : unit = m00 <- v
    member __.M01
        with get() : float = m01
        and set(v : float) : unit = m01 <- v
    member __.M02
        with get() : float = m02
        and set(v : float) : unit = m02 <- v
    member __.M03
        with get() : float = m03
        and set(v : float) : unit = m03 <- v
    member __.M10
        with get() : float = m10
        and set(v : float) : unit = m10 <- v
    member __.M11
        with get() : float = m11
        and set(v : float) : unit = m11 <- v
    member __.M12
        with get() : float = m12
        and set(v : float) : unit = m12 <- v
    member __.M13
        with get() : float = m13
        and set(v : float) : unit = m13 <- v
    member __.M20
        with get() : float = m20
        and set(v : float) : unit = m20 <- v
    member __.M21
        with get() : float = m21
        and set(v : float) : unit = m21 <- v
    member __.M22
        with get() : float = m22
        and set(v : float) : unit = m22 <- v
    member __.M23
        with get() : float = m23
        and set(v : float) : unit = m23 <- v
    member __.M30
        with get() : float = m30
        and set(v : float) : unit = m30 <- v
    member __.M31
        with get() : float = m31
        and set(v : float) : unit = m31 <- v
    member __.M32
        with get() : float = m32
        and set(v : float) : unit = m32 <- v
    member __.M33
        with get() : float = m33
        and set(v : float) : unit = m33 <- v
    member x.Rows = 4
    member x.Cols = 4
    member x.R0 : V4d = V4d(m00, m01, m02, m03)
    member x.R1 : V4d = V4d(m10, m11, m12, m13)
    member x.R2 : V4d = V4d(m20, m21, m22, m23)
    member x.R3 : V4d = V4d(m30, m31, m32, m33)
    member x.C0 : V4d = V4d(m00, m10, m20, m30)
    member x.C1 : V4d = V4d(m01, m11, m21, m31)
    member x.C2 : V4d = V4d(m02, m12, m22, m32)
    member x.C3 : V4d = V4d(m03, m13, m23, m33)
    static member Zero = M44d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M44d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 )
    static member FromCols(c0 : V4d, c1 : V4d, c2 : V4d, c3 : V4d) : M44d = 
        M44d(
            c0.X, c1.X, c2.X, c3.X, 
            c0.Y, c1.Y, c2.Y, c3.Y, 
            c0.Z, c1.Z, c2.Z, c3.Z, 
            c0.W, c1.W, c2.W, c3.W
        )
    static member FromRows(r0 : V4d, r1 : V4d, r2 : V4d, r3 : V4d) : M44d = 
        M44d(
            r0.X, r0.Y, r0.Z, r0.W, 
            r1.X, r1.Y, r1.Z, r1.W, 
            r2.X, r2.Y, r2.Z, r2.W, 
            r3.X, r3.Y, r3.Z, r3.W
        )
    static member RotationZ(angle : float) : M44d = 
        let c = cos angle
        let s = sin angle
        M44d(c, s,  0.0 ,  0.0 , -s, c,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 )
    static member RotationY(angle : float) : M44d = 
        let c = cos angle
        let s = sin angle
        M44d(c,  0.0 , s,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 , -s,  0.0 , c,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 )
    static member RotationX(angle : float) : M44d = 
        let c = cos angle
        let s = sin angle
        M44d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 , c, s,  0.0 ,  0.0 , -s, c,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 )
    static member Rotation(axis : V3d, angle : float) : M44d =
        let axis = axis.Normalized
        let n1 = axis.X
        let n2 = axis.Y
        let n3 = axis.Z
        let n12 = axis.X * axis.Y
        let n13 = axis.X * axis.Z
        let n23 = axis.Y * axis.Z
        let c = cos angle
        let s = sin angle
        let ci = 1.0 - c
        M44d(
            c+n1*n1*ci, n12*ci+n3*s, n13*ci-n2*s,  0.0 , 
            n12*ci-n3*s, c+n2*n2*ci, n23*ci+n1*s,  0.0 , 
            n13*ci+n2*s, n23*ci-n1*s, c+n3*n3*ci,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    static member ViewTrafo(location : V3d, right : V3d, up : V3d, normal : V3d) =
        M44d(
            right.X, right.Y, right.Z, -location.Dot(right), 
            up.X, up.Y, up.Z, -location.Dot(up), 
            normal.X, normal.Y, normal.Z, -location.Dot(normal), 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    static member Scale(scale : V3d) : M44d = 
        M44d(
            scale.X,  0.0 ,  0.0 ,  0.0 , 
             0.0 , scale.Y,  0.0 ,  0.0 , 
             0.0 ,  0.0 , scale.Z,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    static member Scale(scale : float) : M44d = 
        M44d(
            scale,  0.0 ,  0.0 ,  0.0 , 
             0.0 , scale,  0.0 ,  0.0 , 
             0.0 ,  0.0 , scale,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    static member Translation(offset : V3d) : M44d = 
        M44d(
             1.0 ,  0.0 ,  0.0 , offset.X, 
             0.0 ,  1.0 ,  0.0 , offset.Y, 
             0.0 ,  0.0 ,  1.0 , offset.Z, 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    member x.TransformPos(pos : V3d) : V3d = 
        V3d(
            m00 * pos.X + m01 * pos.Y + m02 * pos.Z + m03, 
            m10 * pos.X + m11 * pos.Y + m12 * pos.Z + m13, 
            m20 * pos.X + m21 * pos.Y + m22 * pos.Z + m23
        )
    member x.TransformDir(pos : V3d) : V3d = 
        V3d(
            m00 * pos.X + m01 * pos.Y + m02 * pos.Z, 
            m10 * pos.X + m11 * pos.Y + m12 * pos.Z, 
            m20 * pos.X + m21 * pos.Y + m22 * pos.Z
        )
    member x.TransformPosProj(pos : V3d) : V3d = 
        let w = m30 * pos.X + m31 * pos.Y + m32 * pos.Z + m33
        V3d(
            (m00 * pos.X + m01 * pos.Y + m02 * pos.Z + m03) / w, 
            (m10 * pos.X + m11 * pos.Y + m12 * pos.Z + m13) / w, 
            (m20 * pos.X + m21 * pos.Y + m22 * pos.Z + m23) / w
        )
    static member (*) (l : M44d, r : M43d) : M43d =
        M43d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20 + l.M23 * r.M30,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21 + l.M23 * r.M31,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22 + l.M23 * r.M32,

            l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20 + l.M33 * r.M30,
            l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21 + l.M33 * r.M31,
            l.M30 * r.M02 + l.M31 * r.M12 + l.M32 * r.M22 + l.M33 * r.M32

        )
    static member (*) (l : M44d, r : M44d) : M44d =
        M44d(
            l.M00 * r.M00 + l.M01 * r.M10 + l.M02 * r.M20 + l.M03 * r.M30,
            l.M00 * r.M01 + l.M01 * r.M11 + l.M02 * r.M21 + l.M03 * r.M31,
            l.M00 * r.M02 + l.M01 * r.M12 + l.M02 * r.M22 + l.M03 * r.M32,
            l.M00 * r.M03 + l.M01 * r.M13 + l.M02 * r.M23 + l.M03 * r.M33,

            l.M10 * r.M00 + l.M11 * r.M10 + l.M12 * r.M20 + l.M13 * r.M30,
            l.M10 * r.M01 + l.M11 * r.M11 + l.M12 * r.M21 + l.M13 * r.M31,
            l.M10 * r.M02 + l.M11 * r.M12 + l.M12 * r.M22 + l.M13 * r.M32,
            l.M10 * r.M03 + l.M11 * r.M13 + l.M12 * r.M23 + l.M13 * r.M33,

            l.M20 * r.M00 + l.M21 * r.M10 + l.M22 * r.M20 + l.M23 * r.M30,
            l.M20 * r.M01 + l.M21 * r.M11 + l.M22 * r.M21 + l.M23 * r.M31,
            l.M20 * r.M02 + l.M21 * r.M12 + l.M22 * r.M22 + l.M23 * r.M32,
            l.M20 * r.M03 + l.M21 * r.M13 + l.M22 * r.M23 + l.M23 * r.M33,

            l.M30 * r.M00 + l.M31 * r.M10 + l.M32 * r.M20 + l.M33 * r.M30,
            l.M30 * r.M01 + l.M31 * r.M11 + l.M32 * r.M21 + l.M33 * r.M31,
            l.M30 * r.M02 + l.M31 * r.M12 + l.M32 * r.M22 + l.M33 * r.M32,
            l.M30 * r.M03 + l.M31 * r.M13 + l.M32 * r.M23 + l.M33 * r.M33

        )
    static member (+) (l : M44d, r : M44d) : M44d = 
        M44d(
            l.M00 + r.M00, l.M01 + r.M01, l.M02 + r.M02, l.M03 + r.M03, 
            l.M10 + r.M10, l.M11 + r.M11, l.M12 + r.M12, l.M13 + r.M13, 
            l.M20 + r.M20, l.M21 + r.M21, l.M22 + r.M22, l.M23 + r.M23, 
            l.M30 + r.M30, l.M31 + r.M31, l.M32 + r.M32, l.M33 + r.M33
        )
    static member (-) (l : M44d, r : M44d) : M44d = 
        M44d(
            l.M00 - r.M00, l.M01 - r.M01, l.M02 - r.M02, l.M03 - r.M03, 
            l.M10 - r.M10, l.M11 - r.M11, l.M12 - r.M12, l.M13 - r.M13, 
            l.M20 - r.M20, l.M21 - r.M21, l.M22 - r.M22, l.M23 - r.M23, 
            l.M30 - r.M30, l.M31 - r.M31, l.M32 - r.M32, l.M33 - r.M33
        )
    static member (*) (l : M44d, r : float) : M44d = 
        M44d(
            l.M00 * r, l.M01 * r, l.M02 * r, l.M03 * r, 
            l.M10 * r, l.M11 * r, l.M12 * r, l.M13 * r, 
            l.M20 * r, l.M21 * r, l.M22 * r, l.M23 * r, 
            l.M30 * r, l.M31 * r, l.M32 * r, l.M33 * r
        )
    static member (*) (l : float, r : M44d) : M44d = 
        M44d(
            l * r.M00, l * r.M01, l * r.M02, l * r.M03, 
            l * r.M10, l * r.M11, l * r.M12, l * r.M13, 
            l * r.M20, l * r.M21, l * r.M22, l * r.M23, 
            l * r.M30, l * r.M31, l * r.M32, l * r.M33
        )
    static member (*) (l : M44d, r : V4d) : V4d = 
        V4d(
            l.M00 * r.X + l.M01 * r.Y + l.M02 * r.Z + l.M03 * r.W, 
            l.M10 * r.X + l.M11 * r.Y + l.M12 * r.Z + l.M13 * r.W, 
            l.M20 * r.X + l.M21 * r.Y + l.M22 * r.Z + l.M23 * r.W, 
            l.M30 * r.X + l.M31 * r.Y + l.M32 * r.Z + l.M33 * r.W
        )
    member x.Transposed : M44d =
        M44d(
            m00, m10, m20, m30, 
            m01, m11, m21, m31, 
            m02, m12, m22, m32, 
            m03, m13, m23, m33
        )
    interface NoSRTP.IHasTransposed<M44d> with member x.Transposed = x.Transposed
    member x.Det : float = 
        m33 * (
            m00 * m11 * m22 + m01 * m12 * m20 + m02 * m10 * m21 - 
            m20 * m11 * m02 - m21 * m12 * m00 - m22 * m10 * m01
        ) - 
        m32 * (
           m00 * m11 * m23 + m01 * m13 * m20 + m03 * m10 * m21 -
           m20 * m11 * m03 - m21 * m13 * m00 - m23 * m10 * m01
        ) + 
        m31 * (
           m00 * m12 * m23 + m02 * m13 * m20 + m03 * m10 * m22 - 
           m20 * m12 * m03 - m22 * m13 * m00 - m23 * m10 * m02
        ) - 
        m30 * (
           m01 * m12 * m23 + m02 * m13 * m21 + m03 * m11 * m22 -
           m21 * m12 * m03 - m22 * m13 * m01 - m23 * m11 * m02
        )
    interface NoSRTP.IHasDet<float> with member x.Det = x.Det
    new(o : M22d) = 
        M44d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 , 
             0.0 ,  0.0 ,  1.0 ,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    new(o : M23d) = 
        M44d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 , 
             0.0 ,  0.0 ,  1.0 ,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    new(o : M24d) = 
        M44d(
            o.M00, o.M01, o.M02, o.M03, 
            o.M10, o.M11, o.M12, o.M13, 
             0.0 ,  0.0 ,  1.0 ,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    new(o : M32d) = 
        M44d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 , 
            o.M20, o.M21,  1.0 ,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    new(o : M33d) = 
        M44d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 , 
            o.M20, o.M21, o.M22,  0.0 , 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    new(o : M34d) = 
        M44d(
            o.M00, o.M01, o.M02, o.M03, 
            o.M10, o.M11, o.M12, o.M13, 
            o.M20, o.M21, o.M22, o.M23, 
             0.0 ,  0.0 ,  0.0 ,  1.0 
        )
    new(o : M42d) = 
        M44d(
            o.M00, o.M01,  0.0 ,  0.0 , 
            o.M10, o.M11,  0.0 ,  0.0 , 
            o.M20, o.M21,  1.0 ,  0.0 , 
            o.M30, o.M31,  0.0 ,  1.0 
        )
    new(o : M43d) = 
        M44d(
            o.M00, o.M01, o.M02,  0.0 , 
            o.M10, o.M11, o.M12,  0.0 , 
            o.M20, o.M21, o.M22,  0.0 , 
            o.M30, o.M31, o.M32,  1.0 
        )
    new(arr : Float32Array) = 
        M44d(
            float arr.[0], float arr.[1], float arr.[2], float arr.[3], 
            float arr.[4], float arr.[5], float arr.[6], float arr.[7], 
            float arr.[8], float arr.[9], float arr.[10], float arr.[11], 
            float arr.[12], float arr.[13], float arr.[14], float arr.[15]
        )
    new(arr : Float64Array) = 
        M44d(
            arr.[0], arr.[1], arr.[2], arr.[3], 
            arr.[4], arr.[5], arr.[6], arr.[7], 
            arr.[8], arr.[9], arr.[10], arr.[11], 
            arr.[12], arr.[13], arr.[14], arr.[15]
        )
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.UpperLeftM33() : M33d = M33d(x)
    member x.CopyTo(arr : Float32Array, index : int) = 
        arr.[index + 0] <- float32 m00; arr.[index + 1] <- float32 m01; arr.[index + 2] <- float32 m02; arr.[index + 3] <- float32 m03
        arr.[index + 4] <- float32 m10; arr.[index + 5] <- float32 m11; arr.[index + 6] <- float32 m12; arr.[index + 7] <- float32 m13
        arr.[index + 8] <- float32 m20; arr.[index + 9] <- float32 m21; arr.[index + 10] <- float32 m22; arr.[index + 11] <- float32 m23
        arr.[index + 12] <- float32 m30; arr.[index + 13] <- float32 m31; arr.[index + 14] <- float32 m32; arr.[index + 15] <- float32 m33
    member x.CopyTo(arr : Float64Array, index : int) = 
        arr.[index + 0] <- m00; arr.[index + 1] <- m01; arr.[index + 2] <- m02; arr.[index + 3] <- m03
        arr.[index + 4] <- m10; arr.[index + 5] <- m11; arr.[index + 6] <- m12; arr.[index + 7] <- m13
        arr.[index + 8] <- m20; arr.[index + 9] <- m21; arr.[index + 10] <- m22; arr.[index + 11] <- m23
        arr.[index + 12] <- m30; arr.[index + 13] <- m31; arr.[index + 14] <- m32; arr.[index + 15] <- m33
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(16)
        arr.[0] <- float32 m00; arr.[1] <- float32 m01; arr.[2] <- float32 m02; arr.[3] <- float32 m03
        arr.[4] <- float32 m10; arr.[5] <- float32 m11; arr.[6] <- float32 m12; arr.[7] <- float32 m13
        arr.[8] <- float32 m20; arr.[9] <- float32 m21; arr.[10] <- float32 m22; arr.[11] <- float32 m23
        arr.[12] <- float32 m30; arr.[13] <- float32 m31; arr.[14] <- float32 m32; arr.[15] <- float32 m33
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(16)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr.[8] <- m20; arr.[9] <- m21; arr.[10] <- m22; arr.[11] <- m23
        arr.[12] <- m30; arr.[13] <- m31; arr.[14] <- m32; arr.[15] <- m33
        arr
    member x.Inverse : M44d = 
        let lu = x.ToFloat64Array()
        let inv = Float64Array.Create(16)
        let perm = Microsoft.FSharp.Collections.Array.zeroCreate 4
        lu.LuFactorize(0, 1, 4, perm)
        lu.LuInverse(0, 1, 4, perm, inv, 0, 1, 4)
        M44d(inv)
    interface NoSRTP.IHasInverse<M44d> with member x.Inverse = x.Inverse
    override __.GetHashCode() = HashCode.Combine(m00.GetHashCode(), m01.GetHashCode(), m02.GetHashCode(), m03.GetHashCode(), m10.GetHashCode(), m11.GetHashCode(), m12.GetHashCode(), m13.GetHashCode(), m20.GetHashCode(), m21.GetHashCode(), m22.GetHashCode(), m23.GetHashCode(), m30.GetHashCode(), m31.GetHashCode(), m32.GetHashCode(), m33.GetHashCode())
    override __.Equals(o) = match o with | :? M44d as o -> m00 = o.M00 && m01 = o.M01 && m02 = o.M02 && m03 = o.M03 && m10 = o.M10 && m11 = o.M11 && m12 = o.M12 && m13 = o.M13 && m20 = o.M20 && m21 = o.M21 && m22 = o.M22 && m23 = o.M23 && m30 = o.M30 && m31 = o.M31 && m32 = o.M32 && m33 = o.M33 | _ -> false
    override __.ToString() = sprintf "[[%f, %f, %f, %f], [%f, %f, %f, %f], [%f, %f, %f, %f], [%f, %f, %f, %f]]" m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? M44d as o ->
                let mutable a = 0
                if (a <- compare m00 o.M00; a <> 0) then a
                elif (a <- compare m01 o.M01; a <> 0) then a
                elif (a <- compare m02 o.M02; a <> 0) then a
                elif (a <- compare m03 o.M03; a <> 0) then a
                elif (a <- compare m10 o.M10; a <> 0) then a
                elif (a <- compare m11 o.M11; a <> 0) then a
                elif (a <- compare m12 o.M12; a <> 0) then a
                elif (a <- compare m13 o.M13; a <> 0) then a
                elif (a <- compare m20 o.M20; a <> 0) then a
                elif (a <- compare m21 o.M21; a <> 0) then a
                elif (a <- compare m22 o.M22; a <> 0) then a
                elif (a <- compare m23 o.M23; a <> 0) then a
                elif (a <- compare m30 o.M30; a <> 0) then a
                elif (a <- compare m31 o.M31; a <> 0) then a
                elif (a <- compare m32 o.M32; a <> 0) then a
                else compare m33 o.M33
            | _ -> failwith "uncomparable"
