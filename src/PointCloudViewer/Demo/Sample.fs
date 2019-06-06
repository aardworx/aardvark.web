module FShade.SimpleSample

open Aardvark.Import.Browser
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Utilities
open FShade


module Shader = 
    type Vertex =
        {
            [<Position>] pos : V4d
            [<Normal>] n : V3d
            [<Color>] c : V4d
            [<Semantic("WorldPosition")>] wp : V4d
        }

    let constantColor (color : V4d) (v : Vertex) =
        vertex {
            return { v with c = color }
        }

    let trafo (v : Vertex) =
        vertex {
            let wp = uniform.ModelTrafo * v.pos
            return { 
                v with 
                    pos = uniform.ViewProjTrafo * wp
                    wp = wp
                    n = Vec.normalize (uniform.NormalMatrix * v.n) 
            }
        }

    let normalColor (v : Vertex) =
        fragment {
            let a = 0.5 * (Vec.normalize v.n + V3d.III)
            return V4d(a, 1.0)
        }

    let simpleLight (v : Vertex) =
        fragment {
            let c = Vec.normalize (uniform.CameraLocation - v.wp.XYZ)
            let n = Vec.normalize v.n
            return (abs (Vec.dot c n)) * v.c
        }

let run() =
    show {
        antialias = true
        scene = fun (ctrl) ->
            Sg.ofList [
                Sg.box (Box3d(-V3d.III, V3d.III))
                |> Sg.trafo (Mod.constant (Trafo3d.Scale 2.0))

                Sg.box (Box3d(V3d(1.0, -1.0, -1.0), V3d(3.0, 1.0, 1.0)))
            ]
            |> Sg.shader {
                do! Shader.trafo
                do! Shader.constantColor V4d.IIII
                do! Shader.normalColor
                do! Shader.simpleLight
            }
    }

//run()