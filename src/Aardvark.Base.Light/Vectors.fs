namespace Aardvark.Base

open System
open Aardvark.Base


type V2i(x : int, y : int) =
    member __.X = x
    member __.Y = y
    member __.Dot(o : V2i) = x * o.X + y * o.Y

    member x.Type = "V2i"

    static member Zero = V2i(0,0)
    static member OO = V2i(0,0)
    static member OI = V2i(0,1)
    static member IO = V2i(1,0)
    static member II = V2i(1,1)
    
    static member (~-) (l : V2i) = V2i(-l.X, -l.Y)
    static member (+) (l : V2i, r : V2i) = V2i(l.X + r.X, l.Y + r.Y)
    static member (-) (l : V2i, r : V2i) = V2i(l.X - r.X, l.Y - r.Y)
    static member (*) (l : V2i, r : V2i) = V2i(l.X * r.X, l.Y * r.Y)
    static member (/) (l : V2i, r : V2i) = V2i(l.X / r.X, l.Y / r.Y)
    static member (*) (l : V2i, r : int) = V2i(l.X * r, l.Y * r)
    static member (*) (l : int, r : V2i) = V2i(l * r.X, l * r.Y)
    static member (/) (l : V2i, r : int) = V2i(l.X / r, l.Y / r)
    static member (/) (l : int, r : V2i) = V2i(l / r.X, l / r.Y)
    
    
    override __.GetHashCode() = HashCode.Combine(x.GetHashCode(), y.GetHashCode())
    override __.Equals o =
        match o with
        | :? V2i as o -> x = o.X && y = o.Y
        | _ -> false
    override __.ToString() = sprintf "[%d, %d]" x y

    interface IComparable with
        member __.CompareTo o =
            match o with
            | :? V2i as o ->
                let a = compare x o.X
                if a = 0 then compare y o.Y
                else a
            | _ ->
                failwith ""


type V2d(x : float, y : float) =
    member __.X = x
    member __.Y = y
    member __.Dot(o : V2d) = x * o.X + y * o.Y
    
    member x.Type = "V2d"

    static member Zero  = V2d(0.0,0.0)
    static member OO    = V2d(0.0,0.0)
    static member OI    = V2d(0.0,1.0)
    static member IO    = V2d(1.0,0.0)
    static member II    = V2d(1.0,1.0)
        
    static member (~-) (l : V2d) = V2d(-l.X, -l.Y)
    static member (+) (l : V2d, r : V2d) = V2d(l.X + r.X, l.Y + r.Y)
    static member (-) (l : V2d, r : V2d) = V2d(l.X - r.X, l.Y - r.Y)
    static member (*) (l : V2d, r : V2d) = V2d(l.X * r.X, l.Y * r.Y)
    static member (/) (l : V2d, r : V2d) = V2d(l.X / r.X, l.Y / r.Y)
    static member (*) (l : V2d, r : float) = V2d(l.X * r, l.Y * r)
    static member (*) (l : float, r : V2d) = V2d(l * r.X, l * r.Y)
    static member (/) (l : V2d, r : float) = V2d(l.X / r, l.Y / r)
    static member (/) (l : float, r : V2d) = V2d(l / r.X, l / r.Y)
    


    override __.GetHashCode() = HashCode.Combine(x.GetHashCode(), y.GetHashCode())
    override __.Equals o =
        match o with
        | :? V2d as o -> x = o.X && y = o.Y
        | _ -> false
    override __.ToString() = sprintf "[%f, %f]" x y


    interface IComparable with
        member __.CompareTo o =
            match o with
            | :? V2d as o ->
                let a = compare x o.X
                if a = 0 then compare y o.Y
                else a
            | _ ->
                failwith ""


