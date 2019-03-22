module Program

open System
open Aardvark.Base
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS

type Sepp<'a> =
    struct
        val mutable public A : 'a
        new(a) = { A = a }
    end


type V2fArray(len : int) =
    let store = Float32Array.Create(float (2 * len))

    member x.length = len

    member x.Item
        with get(i : int) = 
            let b = 2 * i
            V2d(store.[b], store.[b + 1])
        
        and set(i : int) (v : V2d) = 
            let b = 2 * i
            store.[b] <- v.X
            store.[b+1] <- v.Y

    member x.SetByIndex (value : int -> V2d) =
        for i in 0 .. len - 1 do
            x.[i] <- value i


let test (s : Sepp<int>) =
    let mutable s2 = s
    s2.A <- 10
    s2


[<EntryPoint>]
let main argv =

    let a = M44d.Rotation(V3d.III, 0.2) * M44d.Scale(V3d(0.1, 0.6, 10.0)) * M44d.Translation(V3d(1.0, 2.0, 3.0)) //(1.0, 2.0, 3.0, 0.0, 0.2, 0.0, 0.0, 0.0, 2.0)
    let b = a.Inverse

    let tt = Trafo3d.Translation(1.0, 2.0, 3.0) * Trafo3d.Scale(0.1, 0.6, 10.0) * Trafo3d.Rotation(V3d.III, 0.2)

    let t1 = a * b
    let t2 = b * a
    
    let printMat (name : string) (m : M44d) =
        console.log (sprintf "%s: " name)
        console.log ("  " + m.R0.ToString())
        console.log ("  " + m.R1.ToString())
        console.log ("  " + m.R2.ToString())
        console.log ("  " + m.R3.ToString())
        
    printMat "a" a
    printMat "aa" tt.Forward
    printMat "b" b
    printMat "t1" t1
    printMat "t2" t2

    console.log (string t1)



    0 // return an integer exit code
