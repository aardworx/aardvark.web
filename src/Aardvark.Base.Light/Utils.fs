namespace Aardvark.Base

open System

type HashCode =
    static member Combine (a : uint32, b : uint32) =
        a ^^^ b + 0x9e3779b9u + (a <<< 6) + (a >>> 2) |> int

    static member Combine (a : int, b : int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int

    static member Combine (a : int, b : int, [<ParamArray>] rest : int[]) =
        let mutable a = HashCode.Combine(a,b)
        for r in rest do    
            a <- HashCode.Combine(a, r)
        a
