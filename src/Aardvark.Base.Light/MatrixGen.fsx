


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
    printfn "open Fable.Core"
    printfn "open Fable.Import.JS"
    printfn ""

    let getName (d : Description) =
        if d.rows = 1 then sprintf "V%d%s" d.cols d.suffix
        elif d.cols = 1 then sprintf "V%d%s" d.rows d.suffix
        else sprintf "M%d%d%s" d.rows d.cols d.suffix

    let print (fst : bool) (d : Description) =
        let vecNames = [| "X"; "Y"; "Z"; "W" |]
        let name = getName d
        let coords = Array.init (d.rows * d.cols) (fun i -> (i / d.cols),(i % d.cols))
        let names = Array.init (d.rows * d.cols) (fun i -> sprintf "M%d%d" (i / d.cols) (i % d.cols))
        let fieldNames = names |> Array.map (fun s -> s.ToLower())

        let space =
            if d.cols >= d.rows && d.rows >= 3 then Some (d.cols - 1)
            else None


        let t = if fst then "type" else "and"

        printfn "%s %s(%s) =" t name (fieldNames |> Seq.map (fun n -> sprintf "%s : %s" n d.baseType) |> String.concat ", ")

        
        for n in names do
            printfn "    let mutable %s = %s" (n.ToLower()) (n.ToLower())

        // fields
        for n in names do
            printfn "    member __.%s" n
            printfn "        with get() : %s = %s" d.baseType (n.ToLower())
            printfn "        and set(v : %s) : unit = %s <- v" d.baseType (n.ToLower())

        printfn "    member x.Rows = %d" d.rows
        printfn "    member x.Cols = %d" d.cols

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

        let createInline (d : Description) (elem : int -> int -> string) =
            let coords = Array.init (d.rows * d.cols) (fun i -> (i % d.rows),(i / d.rows))
            sprintf "%s(%s)" (getName d) (coords |> Seq.map (fun (r,c) -> elem r c) |> String.concat ", ")
            
            
        let colType = { d with cols = 1 }
        let args = List.init d.cols (fun c -> sprintf "c%d : %s" c (getName colType)) |> String.concat ", "
        printfn "    static member FromCols(%s) : %s = " args name
        create true "        " d (fun r c -> sprintf "c%d.%s" c vecNames.[r])
                    
        let rowType = { d with rows = 1 }
        let args = List.init d.rows (fun r -> sprintf "r%d : %s" r (getName rowType)) |> String.concat ", "
        printfn "    static member FromRows(%s) : %s = " args name
        create true "        " d (fun r c -> sprintf "r%d.%s" r vecNames.[c])
            
        match space with
        | Some 3 -> 
            let elements = [| [| "c"; "s"; d.zero |]; [| "-s"; "c"; d.zero|]; [|d.zero; d.zero; d.one|] |]
            printfn "    static member RotationZ(angle : float) : %s = " name
            printfn "        let c = cos angle"
            printfn "        let s = sin angle"
            printfn "        %s" (createInline d (fun r c -> if c < elements.Length && r < elements.[c].Length then elements.[c].[r] elif r = c then d.one else d.zero))

            let elements = [| [| "c"; d.zero; "s" |]; [|d.zero; d.one; d.zero|]; [| "-s"; d.zero; "c";|] |]
            printfn "    static member RotationY(angle : float) : %s = " name
            printfn "        let c = cos angle"
            printfn "        let s = sin angle"
            printfn "        %s" (createInline d (fun r c -> if c < elements.Length && r < elements.[c].Length then elements.[c].[r] elif r = c then d.one else d.zero))
            
            let elements = [| [|d.one; d.zero; d.zero|]; [| d.zero; "c"; "s" |]; [| d.zero; "-s"; "c";|] |]
            printfn "    static member RotationX(angle : float) : %s = " name
            printfn "        let c = cos angle"
            printfn "        let s = sin angle"
            printfn "        %s" (createInline d (fun r c -> if c < elements.Length && r < elements.[c].Length then elements.[c].[r] elif r = c then d.one else d.zero))

            printfn "    static member Rotation(axis : V3d, angle : float) : %s =" name
            printfn "        let axis = axis.Normalized"
            printfn "        let n1 = axis.X"
            printfn "        let n2 = axis.Y"
            printfn "        let n3 = axis.Z"
            printfn "        let n12 = axis.X * axis.Y"
            printfn "        let n13 = axis.X * axis.Z"
            printfn "        let n23 = axis.Y * axis.Z"
            printfn "        let c = cos angle"
            printfn "        let s = sin angle"
            printfn "        let ci = 1.0 - c"

            let elements =
                [|
                    [|  "c+n1*n1*ci";       "n12*ci-n3*s";      "n13*ci+n2*s"   |]
                    [|  "n12*ci+n3*s";      "c+n2*n2*ci";       "n23*ci-n1*s"   |]
                    [|  "n13*ci-n2*s";      "n23*ci+n1*s";      "c+n3*n3*ci"    |]
                |]

            create true "        " d (fun r c -> if c < elements.Length && r < elements.[c].Length then elements.[c].[r] elif r = c then d.one else d.zero)

 

            //printfn "    static member Rotation(axis : V3d, angle : float) : %s = " name
            //printfn "        let c = cos angle"
            //printfn "        let s = sin angle"
            //()
        | Some 2 ->
            printfn "    static member Rotation(angle : float) : %s = " name
            printfn "        let c = cos angle"
            printfn "        let s = sin angle"
            let elements =
                [|
                    [| "c"; "s" |]
                    [| "-s"; "c"|]
                |]

            create true "        " d (fun r c -> if c < elements.Length && r < elements.[c].Length then elements.[c].[r] elif r = c then d.one else d.zero)
        | _ -> 
            ()

        match space with
        | Some s ->
            let scaleType = { d with rows = 1; cols = s }
            printfn "    static member Scale(scale : %s) : %s = " (getName scaleType) name
            create true "        " d (fun r c -> if r = c then (if c < scaleType.cols then sprintf "scale.%s" vecNames.[c] else d.one) else d.zero)
            
            printfn "    static member Scale(scale : %s) : %s = " d.baseType name
            create true "        " d (fun r c -> if r = c then (if c < scaleType.cols then "scale" else d.one) else d.zero)
        | None ->
            ()

        match space with
        | Some s ->
            let transType = { d with rows = 1; cols = s }
            printfn "    static member Translation(offset : %s) : %s = " (getName transType) name
            create true "        " d (fun r c -> if r = c then d.one elif c = d.cols - 1 then sprintf "offset.%s" vecNames.[r] else d.zero)
        | None ->
            ()
            
        match space with
        | Some s ->
            let inType = { d with rows = s; cols = 1 }
            let outType = { d with rows = s; cols = 1 }
            printfn "    member x.TransformPos(pos : %s) : %s = " (getName inType) (getName outType)
            create true "        " outType (fun r _ ->
                List.init d.cols (fun c -> 
                    if c < inType.rows then sprintf "m%d%d * pos.%s" r c vecNames.[c]
                    else sprintf "m%d%d" r c
                ) |> String.concat " + "
            )

            printfn "    member x.TransformDir(pos : %s) : %s = " (getName inType) (getName outType)
            create true "        " outType (fun r _ ->
                List.init (d.cols - 1) (fun c -> 
                    sprintf "m%d%d * pos.%s" r c vecNames.[c]
                ) |> String.concat " + "
            )

            
            if d.rows > s then
                printfn "    member x.TransformPosProj(pos : %s) : %s = " (getName inType) (getName outType)

                let w =
                    List.init d.cols (fun c -> 
                        if c < inType.rows then sprintf "m%d%d * pos.%s" (d.rows - 1) c vecNames.[c]
                        else sprintf "m%d%d" (d.rows - 1) c
                    ) |> String.concat " + "

                printfn "        let w = %s" w
                create true "        " outType (fun r _ ->
                    List.init d.cols (fun c -> 
                        if c < inType.rows then sprintf "m%d%d * pos.%s" r c vecNames.[c]
                        else sprintf "m%d%d" r c
                    ) |> String.concat " + "
                    |> sprintf "(%s) / w"
                )


        | None ->
            ()

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




        for rr in 2 .. 4  do
            for cc in 2 .. 4 do
                if rr <> d.rows || cc <> d.cols then
                    let s = { d with rows = rr; cols = cc }
                    printfn "    new(o : %s) = " (getName s)
                    create true "        " d (fun r c -> if r < rr && c < cc then sprintf "o.M%d%d" r c elif r = c then d.one else d.zero)

        printfn "    new(arr : Float32Array) = "
        create true "        " d (fun r c -> sprintf "arr.[%d]" (r * d.cols + c))
        
        printfn "    new(arr : Float64Array) = "
        create true "        " d (fun r c -> sprintf "arr.[%d]" (r * d.cols + c))


        
        if (d.rows > 2 && d.cols >= 2) || (d.rows >= 2 && d.cols >2) then
            printfn "    member x.UpperLeftM22() : M22%s = M22%s(x)" d.suffix d.suffix

        if (d.rows > 3 && d.cols >= 3) || (d.rows >= 3 && d.cols > 3) then
            printfn "    member x.UpperLeftM33() : M33%s = M33%s(x)" d.suffix d.suffix


        printfn "    member x.CopyTo(arr : Float32Array, index : int) = "
        let mutable o = 0
        for r in 0 .. d.rows - 1 do
            let setAll = List.init d.cols id |> Seq.mapi (fun i c -> sprintf "arr.[index + %d] <- m%d%d" (o + i) r c) |> String.concat "; "
            printfn "        %s" setAll
            o <- o + d.cols

        
        printfn "    member x.CopyTo(arr : Float64Array, index : int) = "
        let mutable o = 0
        for r in 0 .. d.rows - 1 do
            let setAll = List.init d.cols id |> Seq.mapi (fun i c -> sprintf "arr.[index + %d] <- m%d%d" (o + i) r c) |> String.concat "; "
            printfn "        %s" setAll
            o <- o + d.cols



        printfn "    member x.ToFloat32Array() : Float32Array ="
        printfn "        let arr = Float32Array.Create(%d.0)" (d.rows * d.cols)
        let mutable o = 0
        for r in 0 .. d.rows - 1 do
            let setAll = List.init d.cols id |> Seq.mapi (fun i c -> sprintf "arr.[%d] <- m%d%d" (o + i) r c) |> String.concat "; "
            printfn "        %s" setAll
            o <- o + d.cols
        printfn "        arr"

        
        printfn "    member x.ToFloat64Array() : Float64Array ="
        printfn "        let arr = Float64Array.Create(%d.0)" (d.rows * d.cols)
        let mutable o = 0
        for r in 0 .. d.rows - 1 do
            let setAll = List.init d.cols id |> Seq.mapi (fun i c -> sprintf "arr.[%d] <- m%d%d" (o + i) r c) |> String.concat "; "
            printfn "        %s" setAll
            o <- o + d.cols
        printfn "        arr"


        if d.rows = d.cols then
            let n = d.rows

            printfn "    member x.Inverse : %s = " name
            printfn "        let lu = x.ToFloat64Array()"
            printfn "        let inv = Float64Array.Create(%d.0)" (n * n)
            printfn "        let perm = Microsoft.FSharp.Collections.Array.zeroCreate %d" n
            printfn "        lu.LuFactorize(0, 1, %d, perm)" n
            printfn "        lu.LuInverse(0, 1, %d, perm, inv, 0, 1, %d)" n n
            printfn "        %s(inv)" name
            //let mutable xj = 0
            //for j in 0 .. n - 1 do
            //    let mutable xji = xj
            //    for i in 0 .. n - 1 do
            //        printfn "        %s" (set xji (sprintf "if perm.[%d] = %d then 1.0 else 0.0" j i))
            //        xji <- xji + 1
            //    xj <- xj + xy
                

        printfn "    override __.GetHashCode() = HashCode.Combine(%s)" (names |> Seq.map (fun n -> sprintf "%s.GetHashCode()" (n.ToLower())) |> String.concat ", ")
        printfn "    override __.Equals(o) = match o with | :? %s as o -> %s | _ -> false" name (names |> Seq.map (fun n -> sprintf "%s = o.%s" (n.ToLower()) n) |> String.concat " && ")
        let fmt = Seq.init d.rows (fun r -> sprintf "[%s]" (Seq.init d.cols (fun c -> "%f") |> String.concat ", ")) |> String.concat ", "
        let args = Seq.init d.rows (fun r -> Seq.init d.cols (fun c -> sprintf "m%d%d" r c) |> String.concat " ") |> String.concat " "
        printfn "    override __.ToString() = sprintf \"[%s]\" %s" fmt args
         
        printfn "    interface System.IComparable with"
        printfn "        member x.CompareTo o ="
        printfn "            match o with"
        printfn "            | :? %s as o ->" name
        printfn "                let mutable a = 0"
        
        let indent = "                "
        let mutable i = 0
        for n in Array.take (names.Length - 1) names do
            let iff = if i = 0 then "if" else "elif"
            printfn "%s%s (a <- compare %s o.%s; a <> 0) then a" indent iff (n.ToLower()) n
            i <- i + 1
        let n = names.[i]
        printfn "%selse compare %s o.%s" indent (n.ToLower()) n

        printfn "            | _ -> failwith \"uncomparable\""



    let descs =
        [
            for r in 2 .. 4 do
                for c in 2 .. 4 do                                                      //o.M00
                    yield { baseType = "float"; suffix = "d"; rows = r; cols = c; zero = " 0.0 "; one = " 1.0 "; fract = true }
        ]


    let mutable fst = true
    for d in descs do print fst d; fst <- false
    
    let str = b.ToString()
    System.IO.File.WriteAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Matrices_auto.fs"), str)


run()


