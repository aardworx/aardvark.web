module Program

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Rendering.WebGL
open Aardvark.Import.Browser
open Aardvark.Data
open Microsoft.FSharp.Collections


type RenderCommand =
    | Render of aset<RenderObject>
    | Clear of colors : amap<string, V4d> * depth : IMod<Option<float>> * stencil : IMod<Option<int>>
    | Unordered of aset<RenderCommand>


module FShadeTest =
    open FShade

    
    type SimpleVertex = 
        { 
            [<Position>] pos : V4d
            [<Color>] c : V4d
            [<Semantic("WorldPos")>] wp : V4d
            [<Normal>] n : V3d 
            [<TexCoord>] tc : V2d 
        }

    type Vertex = 
        { 
            [<Position>] pos : V4d
            [<Color>] c : V4d
            [<Semantic("WorldPos")>] wp : V4d
            [<Normal>] n : V3d 
            [<TexCoord>] tc : V2d 
            [<PointSize>] s : float
            [<PointCoord>] pc : V2d
            [<FragCoord>] fc : V4d
            depthRange : float
        }
        
    type MyRecord = { ambient : float; diffuse : float }
    type MyUnion =
        | A of MyRecord
        | B of float


    type UniformScope with
        member x.Ambient : MyUnion = uniform?Ambient
        member x.ShowColor : bool = uniform?ShowColor

    let sammy =
        sampler2d {
            texture uniform?DiffuseColorTexture
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinMagMipLinear
        }
    let diffuseTexture (v : Vertex) =
        fragment {
            return sammy.Sample(v.tc)
        }
        
    let constantColor (c : V4d) (v : Vertex) =
        vertex {
            return { v with c = c }
        }

    let trafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            return { 
                v with 
                    pos = uniform.ViewProjTrafo * wp
                    wp = wp
                    n = Vec.normalize (uniform.NormalMatrix * v.n)
                    s = 1.0
            }
        }

    let depthVertex (v : Vertex) =
        vertex {
            
            let wp = uniform.ModelTrafo * v.pos
            let vp = uniform.ModelViewTrafo * v.pos
            let pos = uniform.ProjTrafo * vp

            let pixelSize = 7.0 * (float uniform.ViewportSize.X / 1300.0)
            let ndcRadius = pixelSize / V2d uniform.ViewportSize

            let pp = pos.XYZ / pos.W
            let ppx = uniform.ProjTrafoInv * V4d(pp + V3d(ndcRadius.X, 0.0, 0.0), 1.0)
            let ppy = uniform.ProjTrafoInv * V4d(pp + V3d(0.0, ndcRadius.Y, 0.0), 1.0)
            let vpx = ppx / ppx.W
            let vpy = ppy / ppy.W
            let vr = 0.5 * (Vec.length (vpx - vp) + Vec.length (vpy - vp))

            let ppz = uniform.ProjTrafo * (vp - V4d(0.0, 0.0, vr, 0.0))
            let ppz = ppz.XYZ / ppz.W

            let depthRange = abs (ppz.Z - pp.Z)

            return {
                v with
                    pos = pos
                    wp = wp
                    depthRange = depthRange
                    n = Vec.normalize (uniform.NormalMatrix * v.n)
                    s = pixelSize
            }
        }

    type Fragment =
        {
            [<Color>] c : V4d
            [<Depth>] d : float
        }



    let circularPoint (v : Vertex) =
        fragment {  
            let c = 2.0 * v.pc - V2d.II
            let f = Vec.dot c c - 1.0
            if f > 0.0 then discard()

            let z = sqrt (-f)
            let n = V3d(c, z)

            let newDepth = v.fc.Z - v.depthRange * z


            let c =
                if uniform.ShowColor then v.c.XYZ
                else V3d.III

            let c = V4d((0.6 + 0.4 * z) * c, 1.0)
            return c //{ c = c; d = newDepth }

            //let sn = 0.5 * (V3d(c, sqrt (1.0 - l2)) + V3d.III)
            //return V4d(sn, 1.0)

            //let n = Vec.normalize v.n
            //let c = uniform.CameraLocation - v.wp.XYZ |> Vec.normalize
            //let d = abs (Vec.dot n c)

            //return V4d((0.2 + 0.8 * d) * V3d.III, 1.0)
        }

    [<GLSLIntrinsic("mix({0}, {1}, {2})")>]
    let lerp (a : 'a) (b : 'a) (t : float) = onlyInShaderCode "mix"

    let simpleLight (v : Vertex) =
        fragment {
            match uniform.Ambient with 
            | A a -> 
                let d = 
                    let dir = Vec.normalize (uniform.CameraLocation - v.wp.XYZ)
                    let n = Vec.normalize v.n
                    Vec.dot n dir
                let ambient = lerp 0.0 1.0 a.ambient
                return V4d((ambient + a.diffuse * (1.0 - ambient) * d) * v.c.XYZ, v.c.W)
            | B f ->
                return f * v.c
        }

type largeuint(data : byte[]) =
    static let ceilDiv8 (v : int) =
        if v < 0 then 0
        elif v &&& 7 = 0 then v / 8
        else 1 + v / 8

    static let trim (arr : byte[]) =
        let mutable off = 0
        while off < arr.Length && arr.[off] = 0uy do
            off <- off + 1
        if off > 0 then Array.skip off arr
        else arr

    let data = trim data
    member x.Data = data
    member x.Bits = 8 * data.Length

    static member Zero = largeuint([||])
    static member One = largeuint([| 1uy |])

    new (v : int) =
        assert(v >= 0)
        largeuint [| byte (v >>> 24); byte (v >>> 16); byte (v >>> 8); byte v |]

    override x.ToString() = 
        data |> Seq.mapi (fun i v -> if i = 0 then sprintf "%X" v else sprintf "%02X" v) |> String.concat "" |> sprintf "0x%s"

    static member (<<<) (l : largeuint, r : int) : largeuint =
        if r = 0 then l
        elif r < 0 then l >>> -r
        else
            let maxBits = 8 * l.Data.Length + r
            let res = Array.zeroCreate (ceilDiv8 maxBits)
            let shift = r &&& 7
            if shift = 0 then
                let mutable ri = res.Length - 1 - (r >>> 3)
                let mutable li = l.Data.Length - 1
                while li >= 0 && ri >= 0 do 
                    res.[ri] <- l.Data.[li]
                    ri <- ri - 1
                    li <- li - 1
                largeuint res

            else
                let mutable ri = res.Length - 1 - (r >>> 3)
                let mutable li = l.Data.Length - 1
                let mutable c = 0uy
                while li >= 0 && ri >= 0 do 
                    res.[ri] <- (l.Data.[li] <<< shift) ||| c
                    c <- l.Data.[li] >>> (8-shift)
                    ri <- ri - 1
                    li <- li - 1

                if ri >= 0 && c <> 0uy then
                    res.[ri] <- c

                largeuint res


    static member (>>>) (l : largeuint, r : int) =
        if r = 0 then l
        elif r < 0 then l <<< -r
        else
            let maxBits = 8 * l.Data.Length - r
            let res = Array.zeroCreate (ceilDiv8 maxBits)
            let shift = r &&& 7
            if shift = 0 then
                let mutable ri = 0
                let mutable li = 0
                while li < l.Data.Length && ri < res.Length do 
                    res.[ri] <- l.Data.[li]
                    ri <- ri + 1
                    li <- li + 1
                
                largeuint res
                
            else
                let mask = (1uy <<< shift) - 1uy
                let mutable ri = 0
                let mutable li = 0
                let mutable c = 0uy
                while li < l.Data.Length && ri < res.Length do 
                    res.[ri] <- (l.Data.[li] >>> shift) ||| c
                    c <- (l.Data.[li] &&& mask) <<< (8 - shift)
                    ri <- ri + 1
                    li <- li + 1
                

                largeuint res



    static member (+) (l : largeuint, r : largeuint) : largeuint =
        let bits = 1 + max l.Bits r.Bits
        let res = Array.zeroCreate (ceilDiv8 bits)

        let mutable li = l.Data.Length-1
        let mutable ri = r.Data.Length-1
        let mutable oi = res.Length-1
        let mutable c = 0uy
        while li >= 0 && ri >= 0 do
            let v = int l.Data.[li] + int r.Data.[ri] + int c
            res.[oi] <- byte v
            c <- if v > 255 then 1uy else 0uy
            li <- li - 1
            ri <- ri - 1
            oi <- oi - 1

        while li >= 0 do
            let v = int l.Data.[li] + int c
            res.[oi] <- byte v
            li <- li - 1
            oi <- oi - 1
            c <- if v > 255 then 1uy else 0uy

        while ri >= 0 do
            let v = int r.Data.[ri] + int c
            res.[oi] <- byte v
            ri <- ri - 1
            oi <- oi - 1
            c <- if v > 255 then 1uy else 0uy

        while oi >= 0 do
            let v = int c
            res.[oi] <- byte v
            c <- if v > 255 then 1uy else 0uy
            oi <- oi - 1

        largeuint res

    static member DistanceIsOne(l : largeuint, r : largeuint) : bool =
        // TODO: faster implementation!!!
        let c = compare l r
        if c > 0 then 
            l = r + largeuint.One
        elif c < 0 then
            r = l + largeuint.One
        else
            false

    member x.Equals(o : bigint) =
        let arr = o.ToByteArray() |> Array.rev |> trim
        arr.Length = data.Length && Array.forall2 (=) arr data


    override x.GetHashCode() =
        let combine a b =
            uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int
        data |> Array.fold (fun c v -> combine c (Unchecked.hash v)) 0

    override x.Equals o =
        match o with
        | :? largeuint as o -> data.Length = o.Data.Length && Array.forall2 (=) data o.Data
        | _ -> false

    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? largeuint as o ->
                let c = compare data.Length o.Data.Length
                if c <> 0 then c
                else
                    let rec compareArray (i : int) (l : byte[]) (r : byte[]) =
                        if i < l.Length then 
                            let c = compare l.[i] r.[i]
                            if c <> 0 then c
                            else compareArray (i+1) l r
                        else
                            0
                    compareArray 0 data o.Data
            | _ ->
                failwith "uncomparable"

type largeuint32(data : uint32[]) =
    static let ceilDiv32 (v : int) =
        if v < 0 then 0
        elif v &&& 31 = 0 then v  /32
        else 1 + v / 32

    static let trim (arr : uint32[]) =
        let mutable off = 0
        while off < arr.Length && arr.[off] = 0u do
            off <- off + 1
        if off > 0 then Array.skip off arr
        else arr

    let data = trim data
    member x.Data = data
    member x.Bits = 32 * data.Length

    static member Zero = largeuint32([||])
    static member One = largeuint32([| 1u |])

    new (v : int) =
        assert(v >= 0)
        largeuint32 [| uint32 v |]

    override x.ToString() = 
        data |> Seq.mapi (fun i v -> if i = 0 then sprintf "%X" v else sprintf "%08X" v) |> String.concat "" |> sprintf "0x%s"

    static member (<<<) (l : largeuint32, r : int) : largeuint32 =
        if r = 0 then l
        elif r < 0 then l >>> -r
        else
            let maxBits = 32 * l.Data.Length + r
            let res = Array.zeroCreate (ceilDiv32 maxBits)
            let shift = r &&& 31
            if shift = 0 then
                let mutable ri = res.Length - 1 - (r >>> 5)
                let mutable li = l.Data.Length - 1
                while li >= 0 && ri >= 0 do 
                    res.[ri] <- l.Data.[li]
                    ri <- ri - 1
                    li <- li - 1
                largeuint32 res

            else
                let mutable ri = res.Length - 1 - (r >>> 5)
                let mutable li = l.Data.Length - 1
                let mutable c = 0u
                while li >= 0 && ri >= 0 do 
                    res.[ri] <- (l.Data.[li] <<< shift) ||| c
                    c <- l.Data.[li] >>> (32-shift)
                    ri <- ri - 1
                    li <- li - 1

                if ri >= 0 && c <> 0u then
                    res.[ri] <- c

                largeuint32 res


    static member (>>>) (l : largeuint32, r : int) =
        if r = 0 then l
        elif r < 0 then l <<< -r
        else
            let maxBits = 32 * l.Data.Length - r
            let res = Array.zeroCreate (ceilDiv32 maxBits)
            let shift = r &&& 31
            if shift = 0 then
                let mutable ri = 0
                let mutable li = 0
                while li < l.Data.Length && ri < res.Length do 
                    res.[ri] <- l.Data.[li]
                    ri <- ri + 1
                    li <- li + 1
                
                largeuint32 res
                
            else
                let mask = (1u <<< shift) - 1u
                let mutable ri = 0
                let mutable li = 0
                let mutable c = 0u
                while li < l.Data.Length && ri < res.Length do 
                    res.[ri] <- (l.Data.[li] >>> shift) ||| c
                    c <- (l.Data.[li] &&& mask) <<< (32 - shift)
                    ri <- ri + 1
                    li <- li + 1
                

                largeuint32 res



    static member (+) (l : largeuint32, r : largeuint32) : largeuint32 =
        let bits = 1 + max l.Bits r.Bits
        let res = Array.zeroCreate (ceilDiv32 bits)

        let mutable li = l.Data.Length-1
        let mutable ri = r.Data.Length-1
        let mutable oi = res.Length-1
        let mutable c = 0u
        while li >= 0 && ri >= 0 do
            let v = float l.Data.[li] + float r.Data.[ri] + float c

            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            li <- li - 1
            ri <- ri - 1
            oi <- oi - 1

        while li >= 0 do
            let v = float l.Data.[li] + float c
            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            li <- li - 1
            oi <- oi - 1

        while ri >= 0 do
            let v = float r.Data.[ri] + float c
            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            ri <- ri - 1
            oi <- oi - 1

        while oi >= 0 do
            let v = float c
            c <- if v > 4294967295.0 then 1u else 0u
            res.[oi] <- uint32 v
            oi <- oi - 1

        largeuint32 res

    static member (-) (l : largeuint32, r : largeuint32) : largeuint32 =
        let bits = 1 + max l.Bits r.Bits
        let res = Array.zeroCreate (ceilDiv32 bits)

        let mutable li = l.Data.Length-1
        let mutable ri = r.Data.Length-1
        let mutable oi = res.Length-1
        let mutable c = 1u
        while li >= 0 && ri >= 0 do
            let v = float l.Data.[li] + float ~~~r.Data.[ri] + float c

            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            li <- li - 1
            ri <- ri - 1
            oi <- oi - 1

        while li >= 0 do
            let v = float l.Data.[li] + float c
            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            li <- li - 1
            oi <- oi - 1

        while ri >= 0 do
            let v = float ~~~r.Data.[ri] + float c
            res.[oi] <- uint32 v
            c <- if v > 4294967295.0 then 1u else 0u
            ri <- ri - 1
            oi <- oi - 1

        while oi >= 0 do
            let v = float c
            c <- if v > 4294967295.0 then 1u else 0u
            res.[oi] <- uint32 v
            oi <- oi - 1

        largeuint32 res
    static member DistanceIsOne(l : largeuint32, r : largeuint32) : bool =
        // TODO: faster implementation!!!
        let c = compare l r
        if c > 0 then 
            // 0FF
            // 100

            // 101001
            // 101000
            
            // 101000
            // 100111
            // 001111
            
            // 101001
            // 101000
            
            // 010110 // 1
            // 101000

            //     01







            l = r + largeuint32.One
        elif c < 0 then
            r = l + largeuint32.One
        else
            false

    override x.GetHashCode() =
        let combine a b =
            uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int
        data |> Array.fold (fun c v -> combine c (Unchecked.hash v)) 0

    override x.Equals o =
        match o with
        | :? largeuint32 as o -> data.Length = o.Data.Length && Array.forall2 (=) data o.Data
        | _ -> false

    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? largeuint32 as o ->
                let c = compare data.Length o.Data.Length
                if c <> 0 then c
                else
                    let rec compareArray (i : int) (l : uint32[]) (r : uint32[]) =
                        if i < l.Length then 
                            let c = compare l.[i] r.[i]
                            if c <> 0 then c
                            else compareArray (i+1) l r
                        else
                            0
                    compareArray 0 data o.Data
            | _ ->
                failwith "uncomparable"

type System.Numerics.BigInteger with
    static member DistanceIsOne(l : bigint, r : bigint) =
        l = r + bigint.One || r = l + bigint.One

type bla = largeuint32

module Normal16 =
    let sgn (v : V2d) = V2d((if v.X >= 0.0 then 1.0 else -1.0), (if v.Y >= 0.0 then 1.0 else -1.0))
    let clamp (v : V2d) =
        V2d(
            (if v.X < -1.0 then -1.0 elif v.X > 1.0 then 1.0 else v.X),
            (if v.Y < -1.0 then -1.0 elif v.Y > 1.0 then 1.0 else v.Y)
        )
    let encode (v : V3d) : uint16 =
        let p = v.XY * (1.0 / (abs v.X + abs v.Y + abs v.Z))
        let p = 
            if v.Z <= 0.0 then clamp (V2d(1.0 - abs p.Y, 1.0 - abs p.X) * sgn p)
            else clamp p
        
        (uint16 ((p.X * 0.5 + 0.5) * 255.0) <<< 8) |||
        (uint16 ((p.Y * 0.5 + 0.5) * 255.0))


    let decode (v : uint16) : V3d =
        let e = V2d(float (v >>> 8) / 255.0, float (v &&& 0xFFus) / 255.0) * 2.0 - V2d.II
        let v = V3d(e, 1.0 - abs e.X - abs e.Y)
        if v.Z < 0.0 then V3d(V2d(1.0 - abs v.Y, 1.0 - abs v.X) * sgn v.XY, v.Z) |> Vec.normalize
        else v |> Vec.normalize



[<Struct; CustomEquality; CustomComparison>]
type Time private(number : bla, dexp : int) =
    
    member private x.Number = number
    member private x.DenomiatorExp = dexp

    static member Zero = Time(bla.Zero, 0)
    static member One = Time(bla.One, 0)

    override x.ToString() =
        let denomiator = bla.One <<< dexp
        sprintf "%A / %A" number denomiator
        //let mutable d,r = bigint.DivRem(number, denomiator)
        //let mutable str = sprintf "%A." d
        //if r.IsZero then 
        //    str + "0"
        //else
        //    let mutable digits = 0
        //    while digits < 30 && not r.IsZero do
        //        r <- r * bigint 10
        //        let (d1, r1) = bigint.DivRem(r, denomiator)
        //        str <- str + sprintf "%A" d1
        //        r <- r1
        //        digits <- digits + 1
        //    str

    override x.GetHashCode() =
        let a = number.GetHashCode() 
        let b = dexp.GetHashCode()
        uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int

    override x.Equals o =
        match o with
        | :? Time as o -> number = o.Number && dexp = o.DenomiatorExp
        | _ -> false

    static member Between(l : Time, r : Time) =
        let le = l.DenomiatorExp
        let re = r.DenomiatorExp
        let c = compare le re
        let mutable a = Unchecked.defaultof<_>
        let mutable b = Unchecked.defaultof<_>
        let mutable e = 0
        if c < 0 then
            a <- l.Number <<< (re - le)
            b <- r.Number
            e <- re

        elif c > 0 then
            a <- l.Number
            b <- r.Number <<< (le - re)
            e <- le
            
        else
            a <- l.Number
            b <- r.Number
            e <- le

        //if d.IsZero then failwith "equal Times"
        if bla.DistanceIsOne(a,b) then
            Time(a + b, e + 1)
        else
            Time((a + b) >>> 1, e)
        
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? Time as o -> 
                if dexp < o.DenomiatorExp then
                    let a = number <<< (o.DenomiatorExp - dexp)
                    let b = o.Number
                    compare a b
                elif o.DenomiatorExp < dexp then
                    let a = number
                    let b = o.Number <<< (dexp - o.DenomiatorExp)
                    compare a b
                else
                    compare number o.Number
            | _ ->
                failwith "uncomparable"

module Time =
    let timed (f : unit -> int) =
        #if FABLE_QUOTATIONS
        let start = performance.now()
        let iter = f()
        let dt = performance.now() - start
        console.log(sprintf "took %.3fµs" (1000.0 * dt / float iter))
        #else
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let iter = f()
        sw.Stop()
        printfn "took %.3fµs" (1000.0 * sw.Elapsed.TotalMilliseconds / float iter)
        #endif

    let test() =
        let rand = System.Random()

        let a = largeuint32 3 - largeuint32 18 + largeuint32 18
        console.warn (string a)

        //for i in 1 .. 1000 do
        //    let shift = rand.Next(20)
        //    let s2 = rand.Next(shift)

        //    let v = rand.Next()
        //    let a = (bigint v <<< shift) >>> s2
        //    let b = (largeuint v <<< shift) >>> s2
        //    let sa = sprintf "0x%s" (a.ToString("X")) 
        //    let sb = b.ToString()

        //    if not (b.Equals a) then 
                
        //        failwithf "bad(%d): %A <<< %d -> %A vs %A" i v shift sa sb





        let a = Time.Zero
        let b = Time.One
        
        timed ( fun () ->
            let mutable l = a
            let mutable h = b
        
            let iter = 10000
            for i in 1 .. iter do
                h <- Time.Between(l,h)
            iter
        )
        let mutable l = a
        let mutable h = b
        for i in 1 .. 10000 do
            let n = Time.Between(l,h)
            if n >= h || n <= l then failwithf "bad: %A %A -> %A" l h n
            if rand.NextDouble() > 0.5 then l <- n
            else h <- n


        
        let a = Time.Zero
        let b = Time.One
        
        for i in 1 .. 10 do
            timed ( fun () ->
                let mutable l = a
                let mutable h = b
        
                let iter = 10000
                for i in 1 .. iter do
                    h <- Time.Between(l,h)
                iter
            )




module Lod =
    open Aardvark.Base.Management

    type TreeReader2(url : string, control : Aardvark.Application.RenderControl, t : TraversalState, rootCenter : V3d, time : IMod<float>) as this =
        inherit AbstractReader<hdeltaset<IRenderObject>>(HDeltaSet.monoid)

        let manager = control.Manager

        let pipeline = 
            let template = 
                Sg.draw PrimitiveTopology.PointList
                |> Sg.vertexAttribute DefaultSemantic.Positions (V3fBuffer.zeroCreate 1)
                |> Sg.vertexAttribute DefaultSemantic.Colors (C3bBuffer.zeroCreate 1)
            let obj = template.RenderObjects t |> ASet.toList |> List.head |> unbox<RenderObject>
            manager.PreparePipeline(control.FramebufferSignature, obj.pipeline)

        let mutable alive = true
        let mutable queue = AtomicQueue.empty
        let mutable initial = true

        let w = Worker.Create "worker.js"

        let rec sendCam() =
            if alive then 
                let view = Mod.force t.viewTrafo
                let proj = Mod.force t.projTrafo
                w.postMessage (Command.UpdateCamera(view, proj))
                Aardvark.Import.JS.setTimeout sendCam 50 |> ignore

        

        let gl = pipeline.program.Context.GL

        let attTypes =
            pipeline.program.Interface.attributes |> Map.map (fun slot att ->
                if att.name = DefaultSemantic.Positions then Vec(Float 32, 3)
                elif att.name = DefaultSemantic.Colors then Vec(Int(false, 8), 3)
                else failwith ""
            )

        let atts =
            attTypes |> Map.map (fun _ t -> VertexAttrib.ofType gl t)

        let attSizes =
            attTypes |> Map.map (fun _ t -> PrimitiveType.size t)

        let mem = 
            let b = 
                if gl.IsGL2 then Memory.webgl2 gl.STATIC_DRAW gl
                else Memory.webgl gl.STATIC_DRAW gl
            let mem = 
                {
                    malloc = fun s -> attSizes |> Map.map (fun _ e -> b.malloc (e * s))
                    mfree = fun p s -> p |> Map.iter (fun i p -> let e = attSizes.[i] in b.mfree p (s*e) )
                    mrealloc = fun p o n -> p |> Map.map (fun i p -> let e = attSizes.[i] in b.mrealloc p (o*e) (n*e) )
                    mcopy = fun _ _ _ _ _ -> failwith ""
                }
            new Aardvark.Base.Management.ChunkedMemoryManager<Map<int, WebGLBuffer>>(mem, 4 <<< 20)

        let read =
            let entries =
                pipeline.program.Interface.attributes |> Map.map (fun id att ->
                    if att.name = DefaultSemantic.Positions then fun (n : Octnode) -> HostBuffer n.PositionsLocal
                    elif att.name = DefaultSemantic.Colors then fun (n : Octnode) -> HostBuffer n.Colors
                    else failwith ""
                )
            fun o -> 
                let n = Octnode(Unchecked.defaultof<_>, false, System.Guid.Empty, 0, V3d.Zero, o)
                let e = entries |> Map.map (fun _ f -> f n)
                n.PointCountCell, e

        let copyTarget =
            if gl.IsGL2 then gl.COPY_WRITE_BUFFER
            else gl.ARRAY_BUFFER

        let alloc (o : Map<Durable.Def, obj>) =
            let gl = manager.Context.GL
            let cnt, buffers = read o
            let slot = mem.Alloc cnt
            slot.Memory.Value |> Map.iter (fun id b ->
                let e = attSizes.[id]
                let data = buffers.[id]
                gl.bindBuffer(copyTarget, b)
                gl.bufferSubData(copyTarget, float (e * slot.Offset), Fable.Core.U2.Case1 data.Data.View)
            )
            gl.bindBuffer(copyTarget, null)
            slot

        let free (slot : Block<_>) =
            mem.Free slot

        let slotCache = Dict<System.Guid, Block<_> * Box3d>(Unchecked.hash, Unchecked.equals)

        let calls = Dict<nref<Map<int, WebGLBuffer>>, Dict<DrawCall, Box3d>>(Unchecked.hash, Unchecked.equals)

        let addCall (call : Block<_>) (box :  Box3d) =
            let set = calls.GetOrCreate(call.Memory, fun _ -> Dict(Unchecked.hash, Unchecked.equals))
            set.[{ first = call.Offset; faceVertexCount = call.Size; instanceCount = 1 }] <- box

        let removeCall(call : Block<_>) =
            match calls.TryGetValue call.Memory with
            | Some set ->
                if set.Remove { first = call.Offset; faceVertexCount = call.Size; instanceCount = 1 } then
                    if set.Count = 0 then calls.Remove call.Memory |> ignore
                    true
                else
                    false
            | None ->
                false

        let mvp = TraversalState.modelViewProjTrafo t
        
        let inst = gl.getExtension("WEBGL_multi_draw_instanced") |> unbox<WEBGL_multi_draw_instanced>

        let run() =
            //Log.line "%d chunks" calls.Count
            let mvp = Mod.force mvp
            for mem, calls in calls do
                mem.Value |> Map.iter (fun id b ->
                    let atts = atts.[id]
                    gl.bindBuffer(gl.ARRAY_BUFFER, b)
                    let mutable mid = id
                    for att in atts do
                        gl.enableVertexAttribArray(float mid)
                        gl.vertexAttribPointer(float mid, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                        mid <- mid + 1
                    gl.bindBuffer(gl.ARRAY_BUFFER, null)
                )

                if unbox inst then
                    let mutable count = 0
                    let mutable offsets = Array.zeroCreate calls.Count
                    let mutable counts = Array.zeroCreate calls.Count
                    let mutable instanceCounts = Array.zeroCreate calls.Count

                    
                    for (call, bounds) in calls do
                        if bounds.IntersectsViewProj mvp then
                            offsets.[count] <- call.first
                            counts.[count] <- call.faceVertexCount
                            instanceCounts.[count] <- 1
                            count <- count + 1

                    inst.multiDrawArraysInstancedWEBGL(gl.POINTS, offsets, 0, counts, 0, instanceCounts, 0, count)
                    ()
                else
                    for (call, bounds) in calls do
                        if bounds.IntersectsViewProj mvp then
                            gl.drawArrays(gl.POINTS, float call.first, float call.faceVertexCount)

        let command =
            { new PreparedCommand(manager) with
                member x.Compile(t) =
                    [|
                        match t with
                        | Some prev -> yield! Compiler.updatePipelineState prev.ExitState pipeline
                        | None ->  yield! Compiler.setPipelineState pipeline
                        yield run
                    |]
                    
                member x.ExitState = pipeline
                member x.Acquire() = PreparedPipelineState.acquire pipeline
                member x.Release() = PreparedPipelineState.release pipeline
                member x.Update t = PreparedPipelineState.update t pipeline
                member x.Resources = PreparedPipelineState.resources pipeline
            }


        do sendCam()
        do w.onmessage <- fun e ->
            let msg = unbox<Reply> e.data
            match msg with
            | Reply.Perform (ops) ->
                let test = 
                    ops 
                    |> Seq.map (fun (k,vs) -> k, Operation.map (fun kvs -> DurableDataCodec.decodeMap (Stream(kvs)) |> snd) vs)
                    |> AtomicOperation.ofSeq

                queue <- AtomicQueue.enqueue test queue
                transact (fun () -> this.MarkOutdated())
            | _ ->
                ()

        do w.postMessage (Command.Add(0, url))

        override x.Kind = "SetReader"

        override x.Compute(token : AdaptiveToken) =
            let start = performance.now()
            let elapsed() = performance.now() - start

            while elapsed() < 16.0 && not (AtomicQueue.isEmpty queue) do
                let op, rest = AtomicQueue.dequeue queue
                queue <- rest
                
                op.ops |> Seq.iter (fun (el, op) ->
                    match op with
                    | Nop ->    
                        ()
                    | Deactivate ->
                        match slotCache.TryGetValue el with
                        | Some (slot, _) -> removeCall slot |> ignore
                        | None -> ()
                    | Free _ ->
                        match slotCache.TryRemove el with
                        | Some (slot, _)-> 
                            removeCall slot |> ignore
                            free slot
                        | None -> 
                            ()
                    | Activate ->
                        match slotCache.TryGetValue el with
                        | Some (slot, box) -> addCall slot box |> ignore
                        | None -> ()

                    | Alloc (v,a) ->
                        match slotCache.TryGetValue el with
                        | Some (slot, box) -> 
                            if a > 0 then addCall slot box |> ignore
                        | None ->
                            let bb = Octnode(Unchecked.defaultof<_>, false, System.Guid.Empty, 0, V3d.Zero, v).BoundingBox //v.[Durable.Octree.BoundingBoxExactGlobal] |> unbox<Box3d>
                            let bb = Box3d(bb.Min - rootCenter, bb.Max - rootCenter)
                            let slot = alloc v
                            slotCache.[el] <- (slot, bb)
                            if a > 0 then addCall slot bb |> ignore

                )

            if not (AtomicQueue.isEmpty queue) then
                let _ = time.GetValue token
                ()
        
            if initial then
                initial <- false
                command :> IRenderObject |> Add |> HDeltaSet.single
            else
                HDeltaSet.empty

        override x.Release() =
            alive <- false
            w.terminate()
            //commandCache |> Seq.iter (fun (cmd,_) -> cmd.Release())
            //commandCache.Clear()
            //cache.Clear()
            //delayed <- HDeltaSet.empty
            //state <- HRefSet.empty
            initial <- true
            queue <- AtomicQueue.empty
            PreparedPipelineState.release pipeline
            ()

    type TreeSg(ctrl : Aardvark.Application.RenderControl, center : V3d, url : string) =
        interface ISg with
            member x.RenderObjects(state) =
                ASet.create (fun () -> new TreeReader2(url, ctrl, state, center, ctrl.Time))


    let sg<'a> time center (url : string) : ISg =
        TreeSg(time, center, url) :> ISg


open Aardvark.Base.Management
open Fable.Core


[<EntryPoint>]
let main argv =

    //let man = new ChunkedMemoryManager<_>(Memory.arrayBuffer, 512)

    //let a = man.Alloc(16)
    //let b = man.Alloc(16)
    //let c = man.Alloc(16)
    //let d = man.Alloc(16)

    //Log.line "%A" a
    //Log.line "%A" b
    //Log.line "%A" c
    //Log.line "%A" d


    let mutable existing = 0.0

    let renderobj (control : Aardvark.Application.RenderControl) (rootCenter : V3d) (state : TraversalState) (pipe : PreparedPipelineState) (n : Map<Durable.Def, obj>) =
        let n = Octnode(Unchecked.defaultof<_>, false, System.Guid.Empty, 0, rootCenter, n)
        let manager = control.Manager
        let ctx = manager.Context

        let mvp = TraversalState.modelViewProjTrafo state

        let localBounds = 
            let bb = n.BoundingBox
            Box3d(bb.Min - rootCenter, bb.Max - rootCenter)

        let iface = pipe.program.Interface
        let resources = System.Collections.Generic.List<IResource>()
        let cleanup = System.Collections.Generic.List<unit -> unit>()
        let pos = n.PositionsLocal
        let code =
            let gl = manager.Context.GL

            if gl.IsGL2 then
                let bindings = 
                    iface.attributes
                    |> Map.toArray
                    |> Array.map (fun (slot, att) ->
                        let b =
                            if att.name = DefaultSemantic.Positions then HostBuffer pos
                            elif att.name = DefaultSemantic.Colors then HostBuffer n.Colors
                            else failwithf "unknown %A" att.name

                        let res = manager.CreateBuffer(Mod.constant (b :> IBuffer))
                        let atts = VertexAttrib.ofType gl b.Data.ElementType
                        cleanup.Add (fun () -> res.Release())
                        resources.Add res
                        res.Acquire()
                        slot, atts, res
                    )
                let vao = 
                    lazy (
                        let vao = gl.createVertexArray()
                        gl.bindVertexArray(vao)
                        for (slot, atts, res) in bindings do   
                            gl.bindBuffer(gl.ARRAY_BUFFER, res.Handle.Value.Handle)
                            let mutable mid = slot
                            for att in atts do
                                gl.enableVertexAttribArray(float mid)
                                gl.vertexAttribPointer(float mid, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                                mid <- mid + 1
                            gl.bindBuffer(gl.ARRAY_BUFFER, null)
                        gl.bindVertexArray(null)
                        cleanup.Add (fun () -> gl.deleteVertexArray vao)
                        vao
                    )

                [|
                    if pos.Length > 0 then
                        yield fun () -> 
                            let mvp = Mod.force mvp
                            if localBounds.IntersectsViewProj mvp then
                                gl.bindVertexArray(vao.Value)
                                gl.drawArrays(gl.POINTS, 0.0, float pos.Length)
                |]
            else
                [|  
                    if pos.Length > 0 then
                        for (slot, att) in Map.toSeq iface.attributes do
                            //let o = iface.attributes |> Map.toSeq |> Seq.map (fun (slot, att) -> string slot, att.name :> obj) |> Fable.Core.JsInterop.createObj
                            //console.error(o)
                            let b =
                                if att.name = DefaultSemantic.Positions then HostBuffer pos
                                elif att.name = DefaultSemantic.Colors then HostBuffer n.Colors
                                else failwithf "unknown %A" att.name

                            let res = manager.CreateBuffer(Mod.constant (b :> IBuffer))
                            let atts = VertexAttrib.ofType gl b.Data.ElementType
                            cleanup.Add (fun () -> res.Release())
                            resources.Add res
                            res.Acquire()
                            let hh = res.Handle
                            yield fun () -> 
                                gl.bindBuffer(gl.ARRAY_BUFFER, hh.Value.Handle)
                                let mutable mid = slot
                                for att in atts do
                                    gl.enableVertexAttribArray(float mid)
                                    gl.vertexAttribPointer(float mid, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                                    mid <- mid + 1
                                gl.bindBuffer(gl.ARRAY_BUFFER, null)
                    
                        yield fun () ->
                            let mvp = Mod.force mvp
                            if localBounds.IntersectsViewProj mvp then
                                gl.drawArrays(gl.POINTS, 0.0, float pos.Length)

                |]

        let cmd =
            { new PreparedCommand(manager) with
                member x.Compile(_) = code
                member x.ExitState = pipe
                member x.Acquire() =
                    existing <- existing + float pos.Length
                member x.Release() = 
                    existing <- existing - float pos.Length
                    cleanup |> Seq.iter (fun f -> f ())
                member x.Resources = Seq.empty
                member x.Update(t) = Prom.value ()
            }
        
        let update =
            resources |> Seq.map (fun r -> r.Update(AdaptiveToken.Top)) |> Prom.all |> Prom.map (fun _ -> ())

        cmd, update

        //let globalBounds = n.BoundingBox
        //let localBounds = Box3d(globalBounds.Min - rootCenter, globalBounds.Max - rootCenter)
        //let intersects (viewProj : Trafo3d) (b : Box3d) =
        //    true
        //    //b.IntersectsViewProj viewProj


        //let sg =
        //    Sg.draw PrimitiveTopology.PointList
        //    |> Sg.vertexAttribute "Positions" loc
        //    |> Sg.vertexAttribute "Colors" n.Colors
        //    //|> Sg.vertexAttribute "Normals" n.Normals
        
        //let obj = sg.RenderObjects state |> ASet.toList |> List.head |> unbox<IRenderObject>
        //let cmd = PreparedCommand.ofRenderObject control.Manager control.FramebufferSignature obj
        //let cmd = cmd.Value 
        //cmd.Acquire()
        //let mutable fst = false
        //let kinds = 
        //    Set.ofList [
        //        "Texture"
        //        "Sampler"
        //        "UniformBuffer"
        //        "UniformBufferSlot"
        //        "UniformLocation"
        //        "DepthTestMode"
        //        "DrawCall"
        //    ]
        //let res = 
        //    { new PreparedCommand(manager) with
        //        member x.Compile(prev) = 
        //            let mvp = (obj |> unbox<RenderObject>).pipeline.uniforms "ModelViewProjTrafo" |> unbox<IMod<Trafo3d>>
        //            let arr = cmd.Compile prev
        //            [|
        //                fun () ->
        //                    let vp = Mod.force mvp
        //                    if intersects vp localBounds then
        //                        for a in arr do a()
        //            |]
        //        member x.ExitState = cmd.ExitState
        //        member x.Acquire() = 
        //            if fst then fst <- false
        //            else cmd.Acquire()
        //        member x.Release() = cmd.Release()
        //        member x.Resources = 
        //            cmd.Resources |> Seq.filter (fun r -> Set.contains r.ResourceKind kinds)
        //        member x.Update(t) = 
        //            Prom.value ()
        //    }

        //res, cmd.Update(AdaptiveToken.Top)

    let rec logUsage() =
        Log.line "existing: %.0f" existing
        Aardvark.Import.JS.setTimeout  logUsage 1000 |> ignore
    logUsage()

    let query = 
        window.location.search.Split([| '&'; '?' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun str -> str.Split([| '=' |]))
        |> Array.choose (fun kvp -> if kvp.Length = 2 then Some (kvp.[0], kvp.[1]) else None)
        |> Map.ofArray

    let file =
        match Map.tryFind "blob" query with
        | Some id -> id
        | None -> "jbs-haus"

    let url = "./" + file + "/{0}"

    let db = Database(url, 1024.0)
    let tree = Octree db
   
    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then

            //let canvas = document.createElement_canvas()
            let canvas = document.getElementById "target" |> unbox<HTMLCanvasElement>
            canvas.tabIndex <- 1.0
            //document.body.appendChild(canvas) |> ignore
            //document.body.style.margin <- "0"
            //document.body.style.padding <- "0"
            //canvas.style.width <- "100%"
            //canvas.style.height <- "100%"
            
            let control = new Aardvark.Application.RenderControl(canvas, false, true)
            control.ClearColor <- V4d.OOOO
            let initial = CameraView.lookAt (V3d(6.0, 6.0, 4.0)) V3d.Zero V3d.OOI
            let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial
            let color = Mod.init true
            let view = 
                cam |> Mod.map (fun v -> 
                    let res = v |> CameraView.viewTrafo
                    //w.postMessage(Command.UpdateCamera(res, Trafo3d.Identity))
                    res
                )
            let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 70.0 0.1 1000.0 (float s.X / float s.Y) |> Frustum.projTrafo)


            control.Keyboard.DownWithRepeats.Add (fun k ->
                match k with
                | Aardvark.Application.Keys.V -> transact (fun () -> color.Value <- not color.Value)
                | _ -> ()
            )

            tree.Root.``then``(fun root ->

            
                let scale = 1.0 // 35.0 / root.BoundingBox.Size.Length
                let sg = Lod.sg control tree.Center url

                //let nodes = tree.GetNodes 1
                //nodes.``then``(fun nodes ->
                //    let v =  HSet.ofSeq nodes
                //    transact (fun () -> for v in v do set.Add v |> ignore)
                //) |> ignore


                let sg =
                    sg 
                    //|> ASet.map (render center) 
                    //|> Sg.set
                    |> Sg.trafo (Mod.constant (Trafo3d.Scale scale))
                    //|> Sg.trafo (Mod.constant (Trafo3d.Translation (V3d(-20.0, -20.0, 300.0))))
                    |> Sg.effect [
                        //FShade.Effect.ofFunction (FShadeTest.constantColor V4d.IIII)
                        FShade.Effect.ofFunction FShadeTest.depthVertex
                        FShade.Effect.ofFunction FShadeTest.circularPoint

                        //FShade.Effect.ofFunction FShadeTest.diffuseTexture
                        //FShade.Effect.ofFunction FShadeTest.simpleLight
                    ]
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj
                    |> Sg.uniform "ViewportSize" control.Size
                    |> Sg.uniform "ShowColor" color
                let objects = sg.RenderObjects()
                let task() = new RenderTask(control.FramebufferSignature, control.Manager, objects) :> IRenderTask

                control.RenderTask <- task()
            )  |> ignore
    )
    0
