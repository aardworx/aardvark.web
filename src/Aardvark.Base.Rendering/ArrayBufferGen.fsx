﻿
type PrimitiveType =
    | Float of bits : int
    | Int of signed : bool * bits : int

    | Vec of PrimitiveType * int
    | Mat of PrimitiveType * int * int

module PrimitiveType =
    let rec underlyingType (t : PrimitiveType) =
        match t with
        | Vec(t,_) -> underlyingType t
        | Mat(t,_,_) -> underlyingType t
        | _ -> t

    let rec count (t : PrimitiveType) =
        match t with
        | Vec(t,d) -> count t * d
        | Mat(t,r,c) -> count t * r * c
        | _ -> 1
        
    let rec byteSize (t : PrimitiveType) =
        match t with
        | Vec(t,d) -> byteSize t * d
        | Mat(t,r,c) -> byteSize t * r * c
        | Int(_, b) -> b / 8
        | Float b -> b / 8
            

    let item (i : int) (t : PrimitiveType) =
        match t with
        | Vec _ -> 
            if i = 0 then Some "X"
            elif i = 1 then Some "Y"
            elif i = 2 then Some "Z"
            elif i = 3 then Some "W"
            else None
        | Mat(_,rows,cols) ->
            let ri = i / cols
            let ci = i % cols
            sprintf "M%d%d" ri ci |> Some
        | _ ->
            None
    let toBufferPrefix (t : PrimitiveType) =
        match t with
        | Float 32 -> "Float32"
        | Float 64 -> "Float64"
        | Int(false, 8) -> "Uint8"
        | Int(false, 16) -> "Uint16"
        | Int(false, 32) -> "Uint32"
        | Int(true, 8) -> "Int8"
        | Int(true, 16) -> "Int16"
        | Int(true, 32) -> "Int32"
        | Vec(Int _, d) -> sprintf "V%di" d
        | Vec(Float 32, d) -> sprintf "V%df" d
        | Vec(Float 64, d) -> sprintf "V%dd" d
        | Mat(Float 32, r, c) -> sprintf "M%d%df" r c
        | Mat(Float 64, r, c) -> sprintf "M%d%dd" r c
        | _ -> failwith ""

    let toTypeName (t : PrimitiveType) =
        match t with
        | Float _ -> "float"
        | Int(false, 8) -> "uint8"
        | Int(false, 16) -> "uint16"
        | Int(false, 32) -> "uint32"
        | Int(true, 8) -> "int8"
        | Int(true, 16) -> "int16"
        | Int(true, 32) -> "int32"
        | Vec(Int _, d) -> sprintf "V%di" d
        | Vec(Float _, d) -> sprintf "V%dd" d
        | Mat(Float _, r, c) -> sprintf "M%d%dd" r c
        | _ -> failwith ""

    let rec toCtor (t : PrimitiveType) =
        match t with
        | Float b -> sprintf "Float %d" b
        | Int(s,b) -> sprintf "Int(%A, %d)" s b

        | Vec(t, d) -> sprintf "Vec(%s, %d)" (toCtor t) d
        | Mat(t, r, c) -> sprintf "Mat(%s, %d, %d)" (toCtor t) r c


let run() =
    let b = System.Text.StringBuilder()
    let printfn fmt = Printf.kprintf (fun str -> b.AppendLine str |> ignore) fmt

    let def (t : PrimitiveType) =
        let byteSize = PrimitiveType.byteSize t


        printfn "type %sBuffer(arr : ArrayBuffer, byteOffset : int, length : int) ="  (PrimitiveType.toBufferPrefix t)
        let arrName = PrimitiveType.underlyingType t |> PrimitiveType.toBufferPrefix
        let cnt = PrimitiveType.count t
        if cnt = 1 then
            printfn "    let store = %sArray.Create(arr, float byteOffset, float length)" arrName
        else
            printfn "    let store = %sArray.Create(arr, float byteOffset, float (%d * length))" arrName cnt
        
        let ut = PrimitiveType.underlyingType t
        let cast = 
            match ut with
            | Float _ -> id
            | t -> sprintf "%s(%s)" (PrimitiveType.toTypeName t)

            
        printfn "    static member ElementSize = %d" byteSize
        printfn "    static member PrimitiveType = %s" (PrimitiveType.toCtor t)
        printfn "    member x.Length = length"

        let tt = PrimitiveType.toTypeName t
        printfn "    member x.Item"
        printfn "        with get(i : int) ="
        if cnt = 1 then
            printfn "            %s" (cast "store.[i]")
        else
            printfn "            let i = %d * i" cnt
            printfn "            %s(%s)" tt (List.init cnt (fun i -> sprintf "store.[i + %d]" i |> cast) |> String.concat ", ")
        
        printfn "        and set(i : int) (v : %s) =" tt
        if cnt = 1 then
            printfn "            store.[i] <- float v"
        else
            printfn "            let i = %d * i" cnt
            for i in 0 .. cnt - 1 do
                match PrimitiveType.item i t with
                | Some f -> 
                    match ut with
                    | Float _ -> 
                        printfn "            store.[i+%d] <- v.%s" i f
                    | _ ->
                        printfn "            store.[i+%d] <- float v.%s" i f
                | _ ->
                    ()
        
        printfn "    new(cnt : int) = %sBuffer(ArrayBuffer.Create(float (%d * cnt)), 0, cnt)" (PrimitiveType.toBufferPrefix t) byteSize


        printfn "    static member init (cnt : int) (creator : int -> %s) = " tt
        printfn "        let res = %sBuffer(cnt)" (PrimitiveType.toBufferPrefix t)
        printfn "        for i in 0 .. cnt - 1 do res.[i] <- creator i"
        printfn "        res"
        printfn "    static member create (cnt : int) (value : %s) = " tt
        printfn "        let res = %sBuffer(cnt)" (PrimitiveType.toBufferPrefix t)
        printfn "        for i in 0 .. cnt - 1 do res.[i] <- value"
        printfn "        res"
        printfn "    static member zeroCreate (cnt : int) = "
        printfn "        %sBuffer(cnt)" (PrimitiveType.toBufferPrefix t)
        
        printfn "    static member ofArray (arr : %s[]) = " tt
        printfn "        let res = %sBuffer(arr.Length)" (PrimitiveType.toBufferPrefix t)
        printfn "        for i in 0 .. arr.Length - 1 do res.[i] <- arr.[i]"
        printfn "        res"
        
        printfn "    static member ofSeq (arr : seq<%s>) = %sBuffer.ofArray (Seq.toArray arr)" tt (PrimitiveType.toBufferPrefix t)
        printfn "    static member ofList (arr : list<%s>) = %sBuffer.ofArray (List.toArray arr)" tt (PrimitiveType.toBufferPrefix t)

        printfn "    member x.Sub(start : int, count : int) = %sBuffer(arr, byteOffset + %d*start, count)" (PrimitiveType.toBufferPrefix t) byteSize
        
        printfn "    interface IArrayBuffer with"
        printfn "        member x.ElementType = %s"  (PrimitiveType.toCtor t)
        printfn "        member x.Length = x.Length"
        printfn "        member x.Buffer = arr"
        printfn "        member x.ByteOffset = byteOffset"
        printfn "        member x.View = store |> unbox<ArrayBufferView>"
        printfn "        member x.Sub(s,c) = x.Sub(s,c) :> IArrayBuffer"
        
        printfn "    interface IArrayBuffer<%s> with" tt
        printfn "        member x.Item"
        printfn "            with get(i : int) = x.[i]"
        printfn "            and set(i : int) (v : %s) = x.[i] <- v" tt

    let types =
    
        [
            Int(true, 8)
            Int(true, 16)
            Int(true, 32)
        
            Int(false, 8)
            Int(false, 16)
            Int(false, 32)

            Float 32
            Float 64
        
            Vec(Int(true, 32), 2)
            Vec(Int(true, 32), 3)
            Vec(Int(true, 32), 4)
            Vec(Float 32, 2)
            Vec(Float 32, 3)
            Vec(Float 32, 4)
            Mat(Float 32, 2, 2)
            Mat(Float 32, 2, 3)
            Mat(Float 32, 3, 3)
            Mat(Float 32, 3, 4)
            Mat(Float 32, 4, 4)


        ]

    printfn "namespace Aardvark.Base.Rendering"
    printfn "open Fable.Import.JS"
    printfn "open Aardvark.Base"
    printfn ""
    for t in types do def t

    
    let str = b.ToString()
    System.IO.File.WriteAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "ArrayBuffer_auto.fs"), str)


run()