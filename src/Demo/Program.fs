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

[<Emit("Object.getPrototypeOf($0)")>]
let tt (o : obj) : string = failwith ""

[<EntryPoint>]
let main argv =

    let s = Sepp(1)

    
    let mutable s2 = s
    s2.A <- 10
    let m = s2
    
    console.warn s.A


    let a = V2d(1.0, 2.0)
    let b = V2d(3.0, 2.0)

    let c = a + 2.0 * b

    let a = a :> obj

    console.warn ( tt s :> obj)

    0 // return an integer exit code
