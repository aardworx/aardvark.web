namespace Aardvark.Base

open System
open System.Runtime.CompilerServices
open Fable.Core
open Fable.Import.JS

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

[<AutoOpen>]
module CoreExtensions =

    [<Emit("Object.getPrototypeOf($0).constructor.name")>]
    let private tt (o : obj) : string = failwith ""
    type System.Object with
        member x.GetTypeName() : string = tt x


[<AbstractClass; Sealed; Extension>]
type ArrayExtensions private() =
    [<Extension>]
    static member LuFactorize(alu : Float64Array, a0 : int, ax : int, ay : int, p : int[]) =
        for i in 0 .. p.Length - 1 do p.[i] <- i
        let n = p.Length

        let mutable ak = a0
        let mutable a_k = 0

        for k in 0 .. n - 2 do
            let mutable pi = k
            let mutable pivot = alu.[ak + a_k]
            let mutable absPivot = abs pivot
            let mutable aik = ak + ay + a_k
            for i in k + 1 .. n - 1 do
                let value = alu.[aik]
                let absValue = abs value
                if absValue > absPivot then
                    pivot <- value
                    absPivot <- absValue
                    pi <- i
                aik <- aik + ay

            if absPivot < 1E-10 then
                failwith "cannot factorize"

            if pi <> k then
                let api = a0 + pi * ay
                let t = p.[pi]
                p.[pi] <- p.[k]
                p.[k] <- t

                let mutable apii = api
                let mutable aki = ak
                for i in 0 .. n - 1 do
                    let t = alu.[apii]
                    alu.[apii] <- alu.[aki]
                    alu.[aki] <- t

                    apii <- apii + ax
                    aki <- aki + ax

            let mutable aj = ak + ay
            let mutable ajk = aj + a_k
            for j in k + 1 .. n - 1 do
                let factor = alu.[ajk] / pivot
                alu.[ajk] <- factor

                let mutable aji = ajk + ax
                let mutable aki = ak + a_k + ax
                for i in k+1 .. n-1 do
                    alu.[aji] <- alu.[aji] - alu.[aki] * factor
                    aki <- aki + ax
                    aji <- aji + ax

                aj <- aj + ay
                ajk <- ajk + ay

            ak <- ak + ay
            a_k <- a_k + ax
    
    [<Extension>]
    static member LuInverse(lu : Float64Array, l0 : int, lx : int, ly : int, p : int[], x : Float64Array, x0 : int, xx : int, xy : int) =
        let n = p.Length
        let mutable xj = x0
        for j in 0 .. n - 1 do
            let mutable xji = xj
            for i in 0 .. n - 1 do
                x.[xji] <- (if p.[j] = i then 1.0 else 0.0)
                xji <- xji + xx
            xj <- xj + xy


        let mutable xk = x0 + xy
        let mutable lk = l0 + ly
        for k in 1 .. n - 1 do
            let mutable xj = x0
            let mutable lkj = lk
            for j in 0 .. k-1 do
                let mutable xki = xk
                let mutable xji = xj
                for i in 0 .. n - 1 do
                    x.[xki] <- x.[xki] - lu.[lkj] * x.[xji]
                    xki <- xki + xx
                    xji <- xji + xx

                xj <- xj + xy
                lkj <- lkj + lx
                ()
            xk <- xk + xy
            lk <- lk + ly


        let mutable k = n - 1
        let mutable xk = x0 + k * xy
        let mutable lk = l0 + k * ly
        let mutable lkk = lk + k * lx
        while k >= 0 do
            let mutable lkj = lkk + lx
            let mutable xj = xk + xy
            for j in k+1 .. n - 1 do
                let mutable xki = xk
                let mutable xji = xj
                for i in 0 .. n - 1 do
                    x.[xki] <- x.[xki] - lu.[lkj] * x.[xji]
                    xki <- xki + xx
                    xji <- xji + xx
                lkj <- lkj + lx
                xj <- xj + xy

            let factor = lu.[lkk] // storing the inverse would increase the mean error
            let mutable xki = xk
            for i in 0 .. n - 1 do
                x.[xki] <- x.[xki] / factor
                xki <- xki + xx


            k <- k - 1
            xk <- xk - xy
            lk <- lk - ly
            lkk <- lkk - ly - lx
