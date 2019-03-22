

type Description =
    {
        baseType    : string
        suffix      : string
        dimension   : int
        zero        : string
        one         : string
        fract       : bool
    }


let run () = 
    let b = System.Text.StringBuilder()
    let printfn fmt = Printf.kprintf (fun str -> b.AppendLine str |> ignore) fmt


    printfn "namespace Aardvark.Base"
    printfn ""

    let names = [| "X"; "Y"; "Z"; "W" |]

    let rec allCombinations (length : int) (alt : list<'a>) =
        if length <= 0 then
            [[]]
        else
            alt |> List.collect (fun a -> allCombinations (length - 1) alt |> List.map (fun l -> a :: l))

    let rec allChoices (maxLength : int) (alt : list<'a>) =
        List.init (maxLength + 1) id
        |> List.collect (fun l -> allCombinations l alt)

    let print (d : Description) =
        let names = Array.take d.dimension names
        let fieldNames = names |> Array.map (fun str -> str.ToLower()) 
        let name = sprintf "V%d%s" d.dimension d.suffix


        let decl = fieldNames |> Array.map (fun n -> sprintf "%s : %s" n d.baseType) |> String.concat ", "
        printfn "type %s(%s) = " name decl

        // props
        for n in names do
            printfn "    member __.%s = %s" n (n.ToLower())

        // swizzle
        let swizzles =
            let props = Array.toList names //@ ["I"; "O"]
            allChoices d.dimension props
            |> List.filter (function [] | [_] -> false | _ -> true)
            |> List.filter (List.exists (fun v -> v <> "I" && v <> "O"))
        for sw in swizzles do
            let len = List.length sw
            let resName = sprintf "V%d%s" len d.suffix
            let n = String.concat "" sw
            let values = sw |> List.map (fun n ->
                match n with
                | "I" -> d.one
                | "O" -> d.zero
                | _ -> n.ToLower()
            )
            printfn "    member __.%s = %s(%s)" n resName (String.concat ", " values)


        // creators
        printfn "    static member Zero = %s(%s)" name (Array.create d.dimension d.zero |> String.concat ", ")
        printfn "    static member One = %s(%s)" name (Array.create d.dimension d.one |> String.concat ", ")
        for l in allCombinations d.dimension ["O"; "I"] do
            let n = String.concat "" l
            let args = l |> List.map (function "O" -> d.zero | _ -> d.one) |> String.concat ", "
            printfn "    static member %s = %s(%s)" n name args 


        // operators
        let binary (op : string) = printfn "    static member (%s) (l : %s, r : %s) = %s(%s)" op name name name (names |> Array.map (fun n -> sprintf "l.%s %s r.%s" n op n) |> String.concat ", ")
        let vs (op : string) = printfn "    static member (%s) (l : %s, r : %s) = %s(%s)" op name d.baseType name (names |> Array.map (fun n -> sprintf "l.%s %s r" n op) |> String.concat ", ")
        let sv (op : string) = printfn "    static member (%s) (l : %s, r : %s) = %s(%s)" op d.baseType name name (names |> Array.map (fun n -> sprintf "l %s r.%s" op n) |> String.concat ", ")
        printfn "    static member (~-) (v : %s) = %s(%s)" name name (names |> Array.map (sprintf "-v.%s") |> String.concat ", ")
        binary "+"; binary "-";  binary "*"; binary "/"
        vs "*"; vs "/"; sv "*"; sv "/"

        // dot/cross
        printfn "    static member Dot(l : %s, r : %s) = %s" name name (names |> Seq.map (fun n -> sprintf "l.%s * r.%s" n n) |> String.concat " + ")
        printfn "    member __.Dot(r : %s) = %s" name (names |> Seq.map (fun n -> sprintf "%s * r.%s" (n.ToLower()) n) |> String.concat " + ")
        if d.dimension = 3 then
            printfn "    static member Cross(l : %s, r : %s) = %s(l.Y * r.Z - l.Z * r.Y, l.Z * r.X - l.X * r.Z, l.X * r.Y - l.Y * r.X)" name name name
            printfn "    member __.Cross(r : %s) = %s(y * r.Z - z * r.Y, z * r.X - x * r.Z, x * r.Y - y * r.X)" name name

        printfn "    member __.LengthSquared = %s" (names |> Seq.map (fun n -> sprintf "%s*%s" (n.ToLower()) (n.ToLower())) |> String.concat " + ")
        if d.fract then
            printfn "    member __.Length = sqrt (%s)" (names |> Seq.map (fun n -> sprintf "%s*%s" (n.ToLower()) (n.ToLower())) |> String.concat " + ")
            
            printfn "    member this.Normalized = let l = this.Length in %s(%s)" name (names |> Seq.map (fun n -> sprintf "%s/l" (n.ToLower())) |> String.concat ", ")
        
        else
            printfn "    member __.Length = sqrt (float (%s))" (names |> Seq.map (fun n -> sprintf "%s*%s" (n.ToLower()) (n.ToLower())) |> String.concat " + ")
            

        // hashcode/equals
        printfn "    override __.GetHashCode() = HashCode.Combine(%s)" (names |> Seq.map (fun n -> sprintf "%s.GetHashCode()" (n.ToLower())) |> String.concat ", ")
        printfn "    override __.Equals(o) = match o with | :? %s as o -> %s | _ -> false" name (names |> Seq.map (fun n -> sprintf "%s = o.%s" (n.ToLower()) n) |> String.concat " && ")
        printfn "    override __.ToString() = sprintf \"[%s]\" %s" (Seq.init d.dimension (fun _ -> if d.fract then "%f" else "%d") |> String.concat ", ") (names |> Seq.map (fun s -> s.ToLower()) |> String.concat " ")
        
        // IComparable
        printfn "    interface System.IComparable with"
        printfn "        member __.CompareTo(o) = "
        printfn "            match o with "
        printfn "            | :? %s as o -> " name
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


        // compares
        let cmp (all : bool) (op : string) =
            let nn = if all then "All" else "Any"

            let compose = if all then " && " else " || "

            let nnn =
                match op with
                | ">" -> "Greater"
                | ">=" -> "GreaterOrEqual"
                | "<" -> "Smaller"
                | "<=" -> "SmallerOrEqual"
                | "=" -> "Equal"
                | "<>" -> "Different"
                | _ -> failwith "bad"

            printfn "    member __.%s%s(o : %s) = %s" nn nnn name (names |> Seq.map (fun n -> sprintf "%s %s o.%s" (n.ToLower()) op n) |> String.concat compose )

        let ops = [ ">"; ">="; "<"; "<="; "="; "<>" ]
        for a in [false; true] do
            for o in ops do
                cmp a o


        if d.dimension > 2 then
            let sn = sprintf "V%d%s" (d.dimension - 1) d.suffix
            let els = List.init (d.dimension - 1) (fun i -> sprintf "v.%s" (names.[i]))
            printfn "    new(v : %s, l : %s) = %s(%s)" sn d.baseType name (els @ [ "l" ] |> String.concat ", ")


        if d.fract then
            let nInt = sprintf "V%di" d.dimension

            printfn "    new(v : %s) = %s(%s)" nInt name (names |> Seq.map (fun v -> sprintf "%s v.%s" d.baseType v) |> String.concat ", ")



    let descs =
        [
    
            for d in 2 .. 4 do
                yield { baseType = "int"; suffix = "i"; dimension = d; zero = "0"; one = "1"; fract = false }

            for d in 2 .. 4 do
                yield { baseType = "float"; suffix = "d"; dimension = d; zero = "0.0"; one = "1.0"; fract = true }
        ]
    
    for d in descs do
        print d

    let str = b.ToString()
    System.IO.File.WriteAllText(System.IO.Path.Combine(__SOURCE_DIRECTORY__, "Vectors_auto.fs"), str)


run()


