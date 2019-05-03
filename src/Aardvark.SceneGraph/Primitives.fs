namespace Aardvark.SceneGraph


open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

type Box3d(bmin : V3d, bMax : V3d) =
    member x.Min = bmin
    member x.Max = bMax

    member x.Center = 0.5 * (bmin + bMax)

    member x.Size = bMax - bmin

    static member FromMinAndSize(min : V3d, size : V3d) =
        Box3d(min, min + size)

    static member Unit =
        Box3d(V3d.Zero, V3d.III)

type Triangle3d(p0 : V3d, p1 : V3d, p2 : V3d) =
    member x.P0 = p0
    member x.P1 = p1
    member x.P2 = p2

[<AutoOpen>]
module SgPrimitives =
    
    let private boxVertices =
        let positions = V3fList(8)

        positions.Add(V3d(0.0, 0.0, 0.0))
        positions.Add(V3d(1.0, 0.0, 0.0))
        positions.Add(V3d(1.0, 1.0, 0.0))
        positions.Add(V3d(0.0, 1.0, 0.0))
        positions.Add(V3d(0.0, 0.0, 1.0))
        positions.Add(V3d(1.0, 0.0, 1.0))
        positions.Add(V3d(1.0, 1.0, 1.0))
        positions.Add(V3d(0.0, 1.0, 1.0))
        
        let normals = V3fList(6)
        normals.Add(V3d.IOO)
        normals.Add(V3d.OIO)
        normals.Add(V3d.OOI)
        normals.Add(-V3d.IOO)
        normals.Add(-V3d.OIO)
        normals.Add(-V3d.OOI)
        
        let texcoords = V2fList(6)
        texcoords.Add(V2d.OO)
        texcoords.Add(V2d.IO)
        texcoords.Add(V2d.II)
        texcoords.Add(V2d.OO)
        texcoords.Add(V2d.II)
        texcoords.Add(V2d.OI)

        let indices =
            [|
                1;2;6; 1;6;5
                2;3;7; 2;7;6
                4;5;6; 4;6;7
                3;0;4; 3;4;7
                0;1;5; 0;5;4
                0;3;2; 0;2;1
            |]

        let positions = V3fBuffer.init indices.Length (fun i -> positions.[indices.[i]])
        let normals = V3fBuffer.init indices.Length (fun i -> normals.[int (i / 6)])
        let texcoords = V2fBuffer.init indices.Length (fun i -> texcoords.[i % 6])


        //let texcoords = 
        //    let bla =
        //        promise { 
        //            do! Prom.delay 1000
        //            return HostBuffer texcoords :> IBuffer 
        //        }
        //    PromiseBuffer(bla) 

        let p = { buffer = Mod.constant (HostBuffer positions :> IBuffer); offset = 0; typ = Vec(Float 32, 3) }
        let n = { buffer = Mod.constant (HostBuffer normals :> IBuffer); offset = 0; typ = Vec(Float 32, 3) }
        let t = { buffer = Mod.constant (HostBuffer texcoords :> IBuffer); offset = 0; typ = Vec(Float 32, 2) }
        p, n, t

        
    let private cube = 
        let V3d(x : int,y : int,z : int) = V3d(float x, float y, float z).Normalized
        [|
            // +Z
            Triangle3d(V3d(1, 1, 1), V3d(-1, -1, 1), V3d(1, -1, 1))
            Triangle3d(V3d(-1, -1, 1), V3d(1, 1, 1), V3d(-1, 1, 1))

            // -Z
            Triangle3d(V3d(-1, -1, -1), V3d(1, 1, -1), V3d(1, -1, -1))
            Triangle3d(V3d(1, 1, -1), V3d(-1, -1, -1), V3d(-1, 1, -1))


            // +Y
            Triangle3d(V3d(-1, 1, -1), V3d(1, 1, 1), V3d(1, 1, -1))
            Triangle3d(V3d(1, 1, 1), V3d(-1, 1, -1), V3d(-1, 1, 1))

            // -Y
            Triangle3d(V3d(1, -1, 1), V3d(-1, -1, -1), V3d(1, -1, -1))
            Triangle3d(V3d(-1, -1, -1), V3d(1, -1, 1), V3d(-1, -1, 1))

            // +X
            Triangle3d(V3d(1, 1, 1), V3d(1, -1, -1), V3d(1, 1, -1))
            Triangle3d(V3d(1, -1, -1), V3d(1, 1, 1), V3d(1, -1, 1))

            // -X
            Triangle3d(V3d(-1, -1, -1), V3d(-1, 1, 1), V3d(-1, 1, -1))
            Triangle3d(V3d(-1, 1, 1), V3d(-1, -1, -1), V3d(-1, -1, 1))

        |]


    let private subdivide (tris : Triangle3d[]) =
        [|
            for t in tris do
                let mid = 0.5 * (t.P0 + t.P1) |> Vec.normalize

                yield Triangle3d(t.P1, t.P2, mid)
                yield Triangle3d(t.P2, t.P0, mid)
        |]

    let private sphereGeometry (tris : Triangle3d[]) =

        let ps = tris.Length * 3
        let p = V3fBuffer(ps)
        let n = V3fBuffer(ps)
        let tc = V2fBuffer(ps)

        let mutable i = 0
        for t in tris do
            let nv = Vec.normalize t.P0
            p.[i] <- t.P0
            n.[i] <- nv
            tc.[i] <- V2d((atan2 nv.Y nv.X + Constant.Pi) / Constant.PiTimesTwo, (asin nv.Z + Constant.PiHalf) / Constant.Pi)
            i <- i + 1
            let nv = Vec.normalize t.P1
            p.[i] <- t.P1
            n.[i] <- nv
            tc.[i] <- V2d((atan2 nv.Y nv.X + Constant.Pi) / Constant.PiTimesTwo, (asin nv.Z + Constant.PiHalf) / Constant.Pi)
            i <- i + 1
            let nv = Vec.normalize t.P2
            p.[i] <- t.P2
            n.[i] <- Vec.normalize t.P2
            tc.[i] <- V2d((atan2 nv.Y nv.X + Constant.Pi) / Constant.PiTimesTwo, (asin nv.Z + Constant.PiHalf) / Constant.Pi)
            i <- i + 1

        Sg.RenderNode(PrimitiveTopology.TriangleList, Mod.constant { faceVertexCount = p.Length; instanceCount = 1; first = 0 })
        |> Sg.vertexAttribute DefaultSemantic.Positions p
        |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates tc
        |> Sg.vertexAttribute DefaultSemantic.Normals n

    type private LazyList<'a> =
        | Cons of 'a * Lazy<LazyList<'a>>
        | Nil 

    module private LazyList =
        let initInfinite (create : int -> 'a) =
            let rec acc (i : int) =
                Cons(create i, lazy (acc (i+1)))

            lazy (acc 0)
            
        let rec map (acc : 'a -> 'b)  (l : Lazy<LazyList<'a>>) =
            lazy (
                match l.Value with
                | Cons(h, t) ->
                    Cons(acc h, map acc t)
                | Nil ->
                    Nil
            )
        let rec scan (acc : 's -> 'a -> 's) (s : 's) (l : Lazy<LazyList<'a>>) =
            lazy (
                match l.Value with
                | Cons(h, t) ->
                    Cons(s, scan acc (acc s h) t)
                | Nil ->
                    Nil
            )

        let rec item (i : int) (l : Lazy<LazyList<'a>>) =
            if i = 0 then 
                match l.Value with
                | Cons(h, _) -> h
                | Nil -> failwith "out of range"
            else
                match l.Value with
                | Cons(_,t) -> item (i - 1) t
                | Nil -> failwith "out of range"


    let private spheres =  
        LazyList.initInfinite id
            |> LazyList.scan (fun last _ -> subdivide last) cube
            |> LazyList.map sphereGeometry

    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]  
    module Sg =
        let box (b : Box3d) =
            let (p, n, t) = boxVertices

            let inline att (name : string) (view : BufferView) (sg : ISg) =
                Sg.VertexAttributeApplicator(name, view, sg) :> ISg

            let trafo =
                Trafo3d.Scale(b.Size) *
                Trafo3d.Translation(b.Min)

            Sg.RenderNode(PrimitiveTopology.TriangleList, Mod.constant { faceVertexCount = 36; instanceCount = 1; first = 0 })
            |> att DefaultSemantic.Positions p
            |> att DefaultSemantic.Normals n
            |> att DefaultSemantic.DiffuseColorCoordinates t
            |> Sg.trafo (Mod.constant trafo)

        let sphere (level : int) =
            LazyList.item level spheres
