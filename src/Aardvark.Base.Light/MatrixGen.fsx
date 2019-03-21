


type Description =
    {
        baseType    : string
        suffix      : string
        rows        : int
        cols        : int
        zero        : string
        one         : string
        fract       : bool
    }


let run () = 
    let b = System.Text.StringBuilder()
    let printfn fmt = Printf.kprintf (fun str -> b.AppendLine str |> ignore) fmt
    let printf fmt = Printf.kprintf (fun str -> b.Append str |> ignore) fmt


    printfn "namespace Aardvark.Base"
    printfn ""
    printfn "open Aardvark.Base"
    printfn ""

    let getName (d : Description) =
        if d.rows = 1 then sprintf "V%d%s" d.cols d.suffix
        elif d.cols = 1 then sprintf "V%d%s" d.rows d.suffix
        else sprintf "M%d%d%s" d.rows d.cols d.suffix

    let print (fst : bool) (d : Description) =
        let vecNames = [| "X"; "Y"; "Z"; "W" |]
        let name = getName d
        let coords = Array.init (d.rows * d.cols) (fun i -> (i % d.rows),(i / d.rows))
        let names = Array.init (d.rows * d.cols) (fun i -> sprintf "M%d%d" (i % d.rows) (i / d.rows))
        let fieldNames = names |> Array.map (fun s -> s.ToLower())

        let t = if fst then "type" else "and"

        printfn "%s %s(%s) =" t name (fieldNames |> Seq.map (fun n -> sprintf "%s : %s" n d.baseType) |> String.concat ", ")

        // fields
        for n in names do
            printfn "    member __.%s : %s = %s" n d.baseType (n.ToLower())


        for r in 0 .. d.rows - 1 do
            let v = getName { d with rows = 1 }
            printfn "    member x.R%d : %s = %s(%s)" r v v (List.init d.cols (fun c -> sprintf "m%d%d" r c) |> String.concat ", ")
            
        for c in 0 .. d.cols - 1 do
            let v = getName { d with cols = 1 }
            printfn "    member x.C%d : %s = %s(%s)" c v v (List.init d.rows (fun r -> sprintf "m%d%d" r c) |> String.concat ", ")


        printfn "    static member Zero = %s(%s)" name (names |> Seq.map (fun _ -> d.zero) |> String.concat ", ")
        printfn "    static member Identity = %s(%s)" name (coords |> Seq.map (fun (r,c) -> if r = c then d.one else d.zero) |> String.concat ", ")


        let create (short : bool) (indent : string) (d : Description) (elem : int -> int -> string) =
            printfn "%s%s(" indent (getName d)

            for r in 0 .. d.rows - 1 do
                if short then printf "%s    " indent
                for c in 0 .. d.cols - 1 do
                    let last = r = d.rows - 1 && c = d.cols - 1
                    
                    if short then
                        if last then printf "%s" (elem r c)
                        else printf "%s, " (elem r c)
                    else
                        if last then printfn "%s    %s" indent (elem r c)
                        else printfn "%s    %s," indent (elem r c)
                printfn ""
            printfn "%s)" indent

        // multiply
        for i in  max 2 (d.cols - 1) .. min 4 (d.cols + 1) do
            let o = getName { d with rows = d.cols; cols = i }
            let r = { d with rows = d.rows; cols = i }
            let elem r c = List.init d.cols (fun i -> sprintf "l.M%d%d * r.M%d%d" r i i c) |> String.concat " + "


            printfn "    static member (*) (l : %s, r : %s) : %s =" name o (getName r)
            create false "        " r elem

        // add
        printfn "    static member (+) (l : %s, r : %s) : %s = " name name name
        create true "        " d (fun r c -> sprintf "l.M%d%d + r.M%d%d" r c r c)

        printfn "    static member (-) (l : %s, r : %s) : %s = " name name name
        create true "        " d (fun r c -> sprintf "l.M%d%d - r.M%d%d" r c r c)

        
        printfn "    static member (*) (l : %s, r : %s) : %s = " name d.baseType name
        create true "        " d (fun r c -> sprintf "l.M%d%d * r" r c)

        printfn "    static member (*) (l : %s, r : %s) : %s = " d.baseType name name
        create true "        " d (fun r c -> sprintf "l * r.M%d%d" r c)

        
        printfn "    static member (*) (l : %s, r : V%d%s) : V%d%s = " name d.cols d.suffix d.rows d.suffix        
        create true "        " { d with cols = 1 } (fun r _ -> List.init d.cols (fun c -> sprintf "l.M%d%d * r.%s" r c vecNames.[c]) |> String.concat " + ")


        // transpose 
        let t = { d with cols = d.rows; rows = d.cols }
        printfn "    member x.Transposed : %s =" (getName t)
        create true "        " t (fun r c -> sprintf "m%d%d" c r)
        

        // det
        if d.rows = d.cols then
            if d.rows = 2 then
                printfn "    member x.Det : %s = m00 * m11 - m01 * m10" d.baseType
            elif d.rows = 3 then
                printfn "    member x.Det : %s = " d.baseType
                printfn "        m00 * m11 * m22 + m01 * m12 * m20 + m02 * m10 * m21 - "
                printfn "        m20 * m11 * m02 - m21 * m12 * m00 - m22 * m10 * m01"
            elif d.rows = 4 then
                printfn "    member x.Det : %s = " d.baseType
                printfn "        m33 * ("
                printfn "            m00 * m11 * m22 + m01 * m12 * m20 + m02 * m10 * m21 - "
                printfn "            m20 * m11 * m02 - m21 * m12 * m00 - m22 * m10 * m01"
                printfn "        ) - "
                printfn "        m32 * ("
                printfn "           m00 * m11 * m23 + m01 * m13 * m20 + m03 * m10 * m21 -"
                printfn "           m20 * m11 * m03 - m21 * m13 * m00 - m23 * m10 * m01"
                printfn "        ) + "
                printfn "        m31 * ("
                printfn "           m00 * m12 * m23 + m02 * m13 * m20 + m03 * m10 * m22 - "
                printfn "           m20 * m12 * m03 - m22 * m13 * m00 - m23 * m10 * m02"
                printfn "        ) - "
                printfn "        m30 * ("
                printfn "           m01 * m12 * m23 + m02 * m13 * m21 + m03 * m11 * m22 -"
                printfn "           m21 * m12 * m03 - m22 * m13 * m01 - m23 * m11 * m02"
                printfn "        )"



    let descs =
        [
            for r in 2 .. 4 do
                for c in 2 .. 4 do
                    yield { baseType = "float"; suffix = "d"; rows = r; cols = c; zero = "0.0"; one = "1.0"; fract = true }
        ]


    let mutable fst = true
    for d in descs do print fst d; fst <- false
    
    let str = b.ToString()
    System.IO.File.WriteAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Matrices_auto.fs"), str)


run()


