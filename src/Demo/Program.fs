module Program

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Rendering.WebGL
open Aardvark.Import.Browser

type RenderCommand =
    | Render of aset<RenderObject>
    | Clear of colors : amap<string, V4d> * depth : IMod<Option<float>> * stencil : IMod<Option<int>>
    | Unordered of aset<RenderCommand>


module FShadeTest =
    open FShade
    type Vertex = 
        { 
            [<Position>] pos : V4d
            [<Color>] c : V4d
            [<Semantic("WorldPos")>] wp : V4d
            [<Normal>] n : V3d 
            [<TexCoord>] tc : V2d 
            [<PointSize>] s : float
            [<PointCoord>] pc : V2d
        }
        
    type MyRecord = { ambient : float; diffuse : float }
    type MyUnion =
        | A of MyRecord
        | B of float


    type UniformScope with
        member x.Ambient : MyUnion = uniform?Ambient

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
            return { v with pos = uniform.ViewProjTrafo * wp; wp = wp; n = Vec.normalize (uniform.NormalMatrix * v.n); s = 10.0 }
        }

    let circularPoint (v : Vertex) =
        fragment {
            let c = 2.0 * v.pc - V2d.II
            if c.LengthSquared > 1.0 then discard()
            return v.c
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

open Aardvark.Import.JS
open Microsoft.FSharp.Collections
open System



type Database(urlFormat : string) =
    
    member x.GetString(file : string) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchBuffer url |> Prom.map (fun data ->
            let arr = Uint8Array.Create(data, 0, data.byteLength)
            System.Text.Encoding.UTF8.GetString (unbox<byte[]> arr)
        )
    member x.Get(file : string) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchBuffer url |> Prom.map (fun data ->
            let s = Aardvark.Data.Stream(data)
            Aardvark.Data.DurableDataCodec.decode s
        )

    member inline x.TryGet<'a>(file : string) =
        x.Get(file) |> Prom.map (fun (def,o) ->
            match o with
            | :? 'a as o -> Some o
            | _ -> None
        )



[<AutoOpen>]
module Octree =
    open Aardvark.Data
    open Aardvark.Data.Durable

    let inline private tryGet<'a> (def : Def) (m : Map<Def, obj>) =
        match Map.tryFind def m with
        | Some (:? 'a as o) -> Some o
        | Some o -> Log.warn "bad type for %A %A" def o; None
        | _ -> None
        
    let inline private get<'a> (def : Def) (m : Map<Def, obj>) =
        match Map.tryFind def m with
        | Some (:? 'a as o) -> o
        | Some o -> Log.warn "bad type for %A %A" def o; failwithf "[Octree] could not get %s" def.Name
        | _ -> failwithf "[Octree] could not get %s" def.Name
            

    type Octnode(db : Database, dbid : Guid, m : Map<Def, obj>) =
        let mutable colorsRepaired = false
        let mutable subNodes : Option<Promise<Octnode>>[] = null

        let repairColors3 (b : C3bBuffer) =  
            if not colorsRepaired then
                colorsRepaired <- true
                let rgbs = Uint8Array.Create((b :> IArrayBuffer).Buffer, (b :> IArrayBuffer).ByteOffset, b.Length * 3)
                let mutable o = 0
                for i in 0 .. b.Length - 1 do
                    let b = rgbs.[o+0]
                    rgbs.[o+0] <- rgbs.[o+2]
                    rgbs.[o+2] <- b
                    o <- o + 3
            b

        override x.GetHashCode() = dbid.GetHashCode()
        override x.Equals o =
            match o with
            | :? Octnode as o -> dbid = o.Id
            | _ -> false

        member x.Id = dbid 

        member x.TryPositionsLocal = tryGet<V3fBuffer> Octree.PositionsLocal3f m

        member x.TryNormals = 
            match tryGet<V3fBuffer> Octree.Normals3f m with
            | Some v -> Some v
            | None ->
                match tryGet<Int8Buffer> Octree.Normals3sb m with
                | Some b ->
                    let v3 = V3fBuffer.init b.Length (fun i -> V3d(float b.[3*i+0] / 127.0, float b.[3*i+1] / 127.0, float b.[3*i+2] / 127.0))
                    Some v3
                | None ->
                    None

        member x.TryColors = tryGet<C3bBuffer> Octree.Colors3b m |> FSharp.Core.Option.map repairColors3

        member x.TryMinTreeDepth = tryGet<int> Octree.MinTreeDepth m
        member x.TryMaxTreeDepth = tryGet<int> Octree.MaxTreeDepth m
        member x.TryAvgPointDistance = tryGet<float32> Octree.AveragePointDistance m
        member x.TryAvgPointDistanceStdDev = tryGet<float32> Octree.AveragePointDistanceStdDev m
        member x.TryPointCountCell = tryGet<int> Octree.PointCountCell m
        member x.TryCell = tryGet<Cell> Octree.Cell m

        member x.PositionsLocal =
            match x.TryPositionsLocal with
            | Some v -> v
            | None -> failwith "[Octree] could not get positions"
            
        member x.Normals =
            match x.TryNormals with
            | Some v -> v
            | None -> failwith "[Octree] could not get normals"
            
        member x.Colors =
            match x.TryColors with
            | Some c -> c
            | None -> failwith "[Octree] could not get colors"


        member x.MinTreeDepth = get<int> Octree.MinTreeDepth m
        member x.MaxTreeDepth = get<int> Octree.MaxTreeDepth m
        member x.AvgPointDistance = get<float32> Octree.AveragePointDistance m
        member x.AvgPointDistanceStdDev = get<float32> Octree.AveragePointDistanceStdDev m
        member x.PointCountCell = get<int> Octree.PointCountCell m
        member x.Cell = get<Cell> Octree.Cell m
        member x.BoundingBox = get<Box3d> Octree.BoundingBoxExactGlobal m
        member x.SubNodeIds =
            match tryGet<Guid[]> Octree.SubnodesGuids m with
            | Some arr -> arr
            | None -> [||]

        member x.SubNodes =
            match subNodes with
            | null ->
                let promises = 
                    x.SubNodeIds |> Array.map (fun id ->
                        if id <> Guid.Empty then
                            let prom = 
                                db.Get(string id) |> Prom.map (fun (def, data) ->
                                    if def <> Octree.Node then 
                                        Log.warn "unexpected data: %A" def
                                        Unchecked.defaultof<_>
                                    else
                                        let d = unbox<Map<Def, obj>> data
                                        Octnode(db, id, d)
                                )
                            Some prom
                        else
                            None
                    )
                subNodes <- promises
                promises
            | ps ->
                ps

        member x.GetNodes(level : int) =
            if level <= 0 then
                Prom.value ([|x|])
            elif x.SubNodeIds.Length > 0 then
                x.SubNodes |> Array.choose id |> Prom.all |> Prom.bind (fun cs ->
                    cs |> Seq.map (fun n -> n.GetNodes(level-1)) |> Prom.all |> Prom.map (Seq.concat >> Seq.toArray)
                )
            else
                Prom.value ([|x|])
                


    type Octree(db : Database) =
        let root = 
            db.GetString("root.json") |> Prom.bind (fun str ->
                let obj = JSON.parse str
                console.warn obj
                let id : Guid = Fable.Core.JsInterop.(?) obj "RootId"
                db.Get(string id) |> Prom.map (fun (def, data) ->
                    if def <> Octree.Node then 
                        Log.warn "unexpected data: %A" def
                        Unchecked.defaultof<_>
                    else
                        let d = unbox<Map<Def, obj>> data
                        Octnode(db, id, d)
                )
            )
        member x.Root = root
        
        member x.GetNodes(level : int) =
            root |> Prom.bind (fun r -> r.GetNodes(level))

let render (rootCenter : V3d) (n : Octnode) =
    console.error("asdasdasdasd")
    let loc = n.PositionsLocal
    let off = n.Cell.Center - rootCenter
    Sg.draw PrimitiveTopology.PointList
    |> Sg.vertexAttribute "Positions" (V3fBuffer.init loc.Length (fun i -> loc.[i] + off))
    |> Sg.vertexAttribute "Colors" n.Colors



[<EntryPoint>]
let main argv =


    //Time.test()
    let db = Database "https://aardworxblobtest.blob.core.windows.net/jbs-haus/{0}?sv=2018-03-28&ss=b&srt=sco&sp=r&se=2020-05-03T17:31:38Z&st=2019-05-03T09:31:38Z&spr=https&sig=akIsUao0LL4SMyvYeC9nXTtBKesxRIZh8cz%2BskBqN2U%3D&sr=b"
    let tree = Octree db

    let set = cset()

   
    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then

            let canvas = document.createElement_canvas()
            canvas.tabIndex <- 1.0
            document.body.appendChild(canvas) |> ignore
            document.body.style.margin <- "0"
            document.body.style.padding <- "0"
            canvas.style.width <- "100%"
            canvas.style.height <- "100%"
            
            let control = new Aardvark.Application.RenderControl(canvas, true)

            let initial = CameraView.lookAt (V3d(6.0, 6.0, 4.0)) V3d.Zero V3d.OOI
            let cam = Aardvark.Application.DefaultCameraController.control control.Mouse control.Keyboard control.Time initial
            let anim = Mod.constant true //control.Keyboard.IsDown(Aardvark.Application.Keys.Space)
            let angle =
                Mod.integrate 0.0 control.Time [
                    anim |> Mod.map (fun a ->
                        if a then 
                            control.Time |> Mod.stepTime (fun _ dt o -> o + 0.1 * dt)
                        else
                            AFun.create id
                    )
                ]


            let view = cam |> Mod.map CameraView.viewTrafo
            let proj = control.Size |> Mod.map (fun s ->  Frustum.perspective 70.0 0.1 1000.0 (float s.X / float s.Y) |> Frustum.projTrafo)

            tree.Root.``then``(fun root ->

                let nodes = tree.GetNodes 100
                nodes.``then``(fun nodes ->
                    let v =  HSet.ofSeq nodes
                    Log.error "asdlkandklsamdklmsald: %A" v.Count
                    transact (fun () -> for v in v do set.Add v |> Log.warn "add: %A")
                ) |> ignore


                let center =  root.Cell.Center
                Log.warn "center: %A" center
                let sg =
                    set 
                    |> ASet.map (render center) 
                    |> Sg.set
                    //|> Sg.trafo (Mod.constant (Trafo3d.Scale 0.05))
                    |> Sg.trafo (Mod.constant (Trafo3d.Translation (V3d(-20.0, -20.0, 300.0))))
                    |> Sg.effect [
                        //FShade.Effect.ofFunction (FShadeTest.constantColor V4d.IIII)
                        FShade.Effect.ofFunction FShadeTest.trafo
                        FShade.Effect.ofFunction FShadeTest.circularPoint

                        //FShade.Effect.ofFunction FShadeTest.diffuseTexture
                        //FShade.Effect.ofFunction FShadeTest.simpleLight
                    ]
                    |> Sg.viewTrafo view
                    |> Sg.projTrafo proj


                let objects = sg.RenderObjects()
                let task() = new RenderTask(control.FramebufferSignature, control.Manager, objects) :> IRenderTask

                control.RenderTask <- task()
            )  |> ignore
    )
    0
