#if COMPILED
namespace Aardvark.Base
#endif


type largeuint(data : uint32[]) =
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

    static member Zero = largeuint([||])
    static member One = largeuint([| 1u |])
    
    new (v : uint8) = largeuint [| uint32 v |]
    new (v : int8) = largeuint [| uint32 v |]
    new (v : uint16) = largeuint [| uint32 v |]
    new (v : int16) = largeuint [| uint32 v |]
    new (v : uint32) = largeuint [| v |]
    new (v : int) = largeuint [| uint32 v |]
    new (v : uint64) = largeuint [| uint32 (v >>> 32); uint32 v |]
    new (v : int64) = largeuint [| uint32 (v >>> 32); uint32 v |]

    member x.IsZero = data.Length = 0

    member x.ToUInt32() =
        if data.Length > 0 then data.[data.Length - 1]
        else 0u

    member x.GetBit(i : int) : uint32 =
        let slot = data.Length - 1 - (i >>> 5)
        let inSlot = i &&& 31

        if slot >= 0 && slot <= data.Length then
            (data.[slot] >>> inSlot) &&& 1u
        else
            0u

    member x.WithBit(i : int, value : uint32) : largeuint =
        let value = value &&& 1u
        
        let slot = i >>> 5
        let inSlot = i &&& 31

        if slot < 0 then 
            x
        elif slot >= data.Length then
            let res = Array.zeroCreate (1 + slot)
            res.[res.Length - 1 - slot] <- (value <<< inSlot)
            let mutable o = res.Length - data.Length
            for i in 0 .. data.Length - 1 do
                res.[o] <- data.[i]
                o <- o + 1
            largeuint res
        else
            let slot = data.Length - 1 - slot
            let b = (data.[slot] >>> inSlot) &&& 1u
            if b = value then x
            else 
                let res = Array.copy data
                res.[slot] <- res.[slot] ^^^ (1u <<< inSlot)
                largeuint res

    override x.ToString() =
        data |> Seq.mapi (fun i b -> if i = 0 then sprintf "%X" b else sprintf "%08X" b) |> String.concat "" |> sprintf "0x%s"
        //let b = 10u
        //let mutable str = ""
        //let mutable (q,r) = largeuint.DivRem(x, b)
        //let qv = char (48u + r)
        //str <- System.String([|qv|]) + str
        //while q <> largeuint.Zero do
        //    let (a,b) = largeuint.DivRem(q, b)
        //    q <- a
        //    r <- b
        //    let qv = char (48u + r)
        //    str <- System.String([|qv|]) + str
        //str
        
    static member DivRem(n : largeuint, d : uint32) : largeuint * uint32 =
        let mutable q = largeuint.Zero
        let mutable r = 0u
        let bits = n.Bits
        for i in 1 .. bits do
            let i = bits - i
            r <- (r <<< 1) ||| n.GetBit i
            if r >= d then
                r <- r - d
                q <- q.WithBit(i, 1u)
               
        q, r

    static member DivRem(n : largeuint, d : largeuint) : largeuint * largeuint =
        let mutable q = largeuint.Zero
        let mutable r = largeuint.Zero
        let bits = n.Bits
        for i in 1 .. bits do
            let i = bits - i
            r <- (r <<< 1) ||| largeuint (n.GetBit i)
            if r >= d then
                r <- r - d
                q <- q.WithBit(i, 1u)
               
        q, r

    static member (|||) (l : largeuint, r : largeuint) =
        let l = l.Data
        let r = r.Data

        let l, r =
            if l.Length > r.Length then l, r
            else r, l

        let len = max l.Length r.Length
        let res = Array.zeroCreate len
        
        let mutable li = 0
        let mutable ri = 0 
        let m = l.Length - r.Length
        while li < m do
            res.[li] <- l.[li]
            li <- li + 1

        while ri < r.Length do
            res.[li] <- l.[li] ||| r.[ri]
            li <- li + 1
            ri <- ri + 1

        largeuint res
    
    static member (&&&) (l : largeuint, r : largeuint) =
        let l = l.Data
        let r = r.Data

        let l, r =
            if l.Length > r.Length then l, r
            else r, l

        let len = min l.Length r.Length
        let res = Array.zeroCreate len
            
        let m = l.Length - r.Length
        let mutable li = m
        let mutable ri = 0 

        while ri < r.Length do
            res.[ri] <- l.[li] &&& r.[ri]
            li <- li + 1
            ri <- ri + 1

        largeuint res

    static member (<<<) (l : largeuint, r : int) : largeuint =
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
                largeuint res

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

                largeuint res

    static member (>>>) (l : largeuint, r : int) =
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
                
                largeuint res
                
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
                

                largeuint res

    static member (+) (l : largeuint, r : largeuint) : largeuint =
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

        largeuint res

    static member (-) (l : largeuint, r : largeuint) : largeuint =
        let bits = max l.Bits r.Bits
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

        largeuint res
    
    static member (*) (l : largeuint, r : uint32) =
        if r = 0u then 
            largeuint.Zero
        elif r = 1u then
            l
        elif l.Data.Length = 0 then
            l
        elif l.Data.Length = 1 && l.ToUInt32() = 1u then
            largeuint r
        else
            let mutable res = largeuint.Zero
            let mutable l = l
            for i in 0 .. 31 do
                let b = (r >>> i) &&& 1u
                if b > 0u then
                    res <- res + l
                l <- l <<< 1
                
            res

    static member (*) (l : largeuint, r : uint64) =
        if r = 0UL then 
            largeuint.Zero
        elif r = 1UL then
            l
        elif l.Data.Length = 0 then
            l
        elif l.Data.Length = 1 && l.ToUInt32() = 1u then
            largeuint r
        else
            let mutable res = largeuint.Zero
            let mutable l = l
            for i in 0 .. 63 do
                let b = (r >>> i) &&& 1UL
                if b > 0UL then
                    res <- res + l
                l <- l <<< 1
            
            res

      

    static member DistanceIsOne(l : largeuint, r : largeuint) : bool =
        // TODO: faster implementation!!!
        let c = compare l r
        if c > 0 then 
            l = r + largeuint.One
        elif c < 0 then
            r = l + largeuint.One
        else
            false

    override x.GetHashCode() =
        let inline combine a b =
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
