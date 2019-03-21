namespace Aardvark.Base

open Aardvark.Base
open Fable.Core
open Fable.Import.JS

type M22d(m00 : float, m10 : float, m01 : float, m11 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M01 : float = m01
    member __.M11 : float = m11
    member x.R0 : V2d = V2d(m00, m01)
    member x.R1 : V2d = V2d(m10, m11)
    member x.C0 : V2d = V2d(m00, m10)
    member x.C1 : V2d = V2d(m01, m11)
    static member Zero = M22d( 0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M22d( 1.0 ,  0.0 ,  0.0 ,  1.0 )
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
    member x.Det : float = m00 * m11 - m01 * m10
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
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(4.0)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(4.0)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr
and M23d(m00 : float, m10 : float, m01 : float, m11 : float, m02 : float, m12 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M02 : float = m02
    member __.M12 : float = m12
    member x.R0 : V3d = V3d(m00, m01, m02)
    member x.R1 : V3d = V3d(m10, m11, m12)
    member x.C0 : V2d = V2d(m00, m10)
    member x.C1 : V2d = V2d(m01, m11)
    member x.C2 : V2d = V2d(m02, m12)
    static member Zero = M23d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M23d( 1.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 )
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(6.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(6.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr
and M24d(m00 : float, m10 : float, m01 : float, m11 : float, m02 : float, m12 : float, m03 : float, m13 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M02 : float = m02
    member __.M12 : float = m12
    member __.M03 : float = m03
    member __.M13 : float = m13
    member x.R0 : V4d = V4d(m00, m01, m02, m03)
    member x.R1 : V4d = V4d(m10, m11, m12, m13)
    member x.C0 : V2d = V2d(m00, m10)
    member x.C1 : V2d = V2d(m01, m11)
    member x.C2 : V2d = V2d(m02, m12)
    member x.C3 : V2d = V2d(m03, m13)
    static member Zero = M24d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M24d( 1.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(8.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(8.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr
and M32d(m00 : float, m10 : float, m20 : float, m01 : float, m11 : float, m21 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M20 : float = m20
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M21 : float = m21
    member x.R0 : V2d = V2d(m00, m01)
    member x.R1 : V2d = V2d(m10, m11)
    member x.R2 : V2d = V2d(m20, m21)
    member x.C0 : V3d = V3d(m00, m10, m20)
    member x.C1 : V3d = V3d(m01, m11, m21)
    static member Zero = M32d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M32d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 )
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(6.0)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr.[4] <- m20; arr.[5] <- m21
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(6.0)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr.[4] <- m20; arr.[5] <- m21
        arr
and M33d(m00 : float, m10 : float, m20 : float, m01 : float, m11 : float, m21 : float, m02 : float, m12 : float, m22 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M20 : float = m20
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M21 : float = m21
    member __.M02 : float = m02
    member __.M12 : float = m12
    member __.M22 : float = m22
    member x.R0 : V3d = V3d(m00, m01, m02)
    member x.R1 : V3d = V3d(m10, m11, m12)
    member x.R2 : V3d = V3d(m20, m21, m22)
    member x.C0 : V3d = V3d(m00, m10, m20)
    member x.C1 : V3d = V3d(m01, m11, m21)
    member x.C2 : V3d = V3d(m02, m12, m22)
    static member Zero = M33d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M33d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 )
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
    member x.Det : float = 
        m00 * m11 * m22 + m01 * m12 * m20 + m02 * m10 * m21 - 
        m20 * m11 * m02 - m21 * m12 * m00 - m22 * m10 * m01
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(9.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr.[6] <- m20; arr.[7] <- m21; arr.[8] <- m22
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(9.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr.[6] <- m20; arr.[7] <- m21; arr.[8] <- m22
        arr
and M34d(m00 : float, m10 : float, m20 : float, m01 : float, m11 : float, m21 : float, m02 : float, m12 : float, m22 : float, m03 : float, m13 : float, m23 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M20 : float = m20
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M21 : float = m21
    member __.M02 : float = m02
    member __.M12 : float = m12
    member __.M22 : float = m22
    member __.M03 : float = m03
    member __.M13 : float = m13
    member __.M23 : float = m23
    member x.R0 : V4d = V4d(m00, m01, m02, m03)
    member x.R1 : V4d = V4d(m10, m11, m12, m13)
    member x.R2 : V4d = V4d(m20, m21, m22, m23)
    member x.C0 : V3d = V3d(m00, m10, m20)
    member x.C1 : V3d = V3d(m01, m11, m21)
    member x.C2 : V3d = V3d(m02, m12, m22)
    member x.C3 : V3d = V3d(m03, m13, m23)
    static member Zero = M34d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M34d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 )
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.UpperLeftM33() : M33d = M33d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(12.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr.[8] <- m20; arr.[9] <- m21; arr.[10] <- m22; arr.[11] <- m23
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(12.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr.[8] <- m20; arr.[9] <- m21; arr.[10] <- m22; arr.[11] <- m23
        arr
and M42d(m00 : float, m10 : float, m20 : float, m30 : float, m01 : float, m11 : float, m21 : float, m31 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M20 : float = m20
    member __.M30 : float = m30
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M21 : float = m21
    member __.M31 : float = m31
    member x.R0 : V2d = V2d(m00, m01)
    member x.R1 : V2d = V2d(m10, m11)
    member x.R2 : V2d = V2d(m20, m21)
    member x.R3 : V2d = V2d(m30, m31)
    member x.C0 : V4d = V4d(m00, m10, m20, m30)
    member x.C1 : V4d = V4d(m01, m11, m21, m31)
    static member Zero = M42d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M42d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 )
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(8.0)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr.[4] <- m20; arr.[5] <- m21
        arr.[6] <- m30; arr.[7] <- m31
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(8.0)
        arr.[0] <- m00; arr.[1] <- m01
        arr.[2] <- m10; arr.[3] <- m11
        arr.[4] <- m20; arr.[5] <- m21
        arr.[6] <- m30; arr.[7] <- m31
        arr
and M43d(m00 : float, m10 : float, m20 : float, m30 : float, m01 : float, m11 : float, m21 : float, m31 : float, m02 : float, m12 : float, m22 : float, m32 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M20 : float = m20
    member __.M30 : float = m30
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M21 : float = m21
    member __.M31 : float = m31
    member __.M02 : float = m02
    member __.M12 : float = m12
    member __.M22 : float = m22
    member __.M32 : float = m32
    member x.R0 : V3d = V3d(m00, m01, m02)
    member x.R1 : V3d = V3d(m10, m11, m12)
    member x.R2 : V3d = V3d(m20, m21, m22)
    member x.R3 : V3d = V3d(m30, m31, m32)
    member x.C0 : V4d = V4d(m00, m10, m20, m30)
    member x.C1 : V4d = V4d(m01, m11, m21, m31)
    member x.C2 : V4d = V4d(m02, m12, m22, m32)
    static member Zero = M43d( 0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 )
    static member Identity = M43d( 1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  1.0 ,  0.0 )
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.UpperLeftM33() : M33d = M33d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(12.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr.[6] <- m20; arr.[7] <- m21; arr.[8] <- m22
        arr.[9] <- m30; arr.[10] <- m31; arr.[11] <- m32
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(12.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02
        arr.[3] <- m10; arr.[4] <- m11; arr.[5] <- m12
        arr.[6] <- m20; arr.[7] <- m21; arr.[8] <- m22
        arr.[9] <- m30; arr.[10] <- m31; arr.[11] <- m32
        arr
and M44d(m00 : float, m10 : float, m20 : float, m30 : float, m01 : float, m11 : float, m21 : float, m31 : float, m02 : float, m12 : float, m22 : float, m32 : float, m03 : float, m13 : float, m23 : float, m33 : float) =
    member __.M00 : float = m00
    member __.M10 : float = m10
    member __.M20 : float = m20
    member __.M30 : float = m30
    member __.M01 : float = m01
    member __.M11 : float = m11
    member __.M21 : float = m21
    member __.M31 : float = m31
    member __.M02 : float = m02
    member __.M12 : float = m12
    member __.M22 : float = m22
    member __.M32 : float = m32
    member __.M03 : float = m03
    member __.M13 : float = m13
    member __.M23 : float = m23
    member __.M33 : float = m33
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
    member x.UpperLeftM22() : M22d = M22d(x)
    member x.UpperLeftM33() : M33d = M33d(x)
    member x.ToFloat32Array() : Float32Array =
        let arr = Float32Array.Create(16.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr.[8] <- m20; arr.[9] <- m21; arr.[10] <- m22; arr.[11] <- m23
        arr.[12] <- m30; arr.[13] <- m31; arr.[14] <- m32; arr.[15] <- m33
        arr
    member x.ToFloat64Array() : Float64Array =
        let arr = Float64Array.Create(16.0)
        arr.[0] <- m00; arr.[1] <- m01; arr.[2] <- m02; arr.[3] <- m03
        arr.[4] <- m10; arr.[5] <- m11; arr.[6] <- m12; arr.[7] <- m13
        arr.[8] <- m20; arr.[9] <- m21; arr.[10] <- m22; arr.[11] <- m23
        arr.[12] <- m30; arr.[13] <- m31; arr.[14] <- m32; arr.[15] <- m33
        arr
