namespace Aardvark.Base

open System
open System.Runtime.CompilerServices
open Fable.Core
open Aardvark.Import.JS
open FSharp.Collections



[<AllowNullLiteral>]
type private LinkedNode<'a> =
    class
        val mutable public Value : 'a
        val mutable public Next : LinkedNode<'a>
        val mutable public Prev : LinkedNode<'a>

        new(value, p, n) = { Value = value; Prev = p; Next = n }
    end

type Queue<'a>() =
    let mutable first : LinkedNode<'a> = null
    let mutable last : LinkedNode<'a> = null
    let mutable count = 0

    member x.Dequeue() =
        if first = null then
            failwith "bad"
        else
            count <- count - 1
            let f = first
            first <- f.Next
            if isNull first then last <- null
            else first.Prev <- null
            f.Value

    member x.Enqueue(value : 'a) =
        let n = LinkedNode(value, last, null)
        if last = null then first <- n
        else last.Next <- n
        last <- n
        count <- count + 1

    member x.Count = count

[<AutoOpen>]
module ListHeapExtensions = 
    type System.Collections.Generic.List<'a> with
        member x.HeapEnqueue(cmp : 'a -> 'a -> int, value : 'a) =
            let mutable i = x.Count
            let mutable run = true
            x.Add(value)
            while run && i > 0 do
                let i2 = int ((i - 1) / 2)
                if cmp value x.[i2] > 0 then 
                    run <- false
                else
                    x.[i] <- x.[i2]
                    i <- i2
                    
            x.[i] <- value

        member x.HeapDequeue(cmp : 'a -> 'a -> int) : 'a =
            let res = x.[0]
            let mutable count = x.Count
            if count = 1 then 
                x.Clear()
                res
            else
                let element = x.[count - 1]
                x.RemoveAt(count - 1)
                count <- count - 1
                let mutable i = 0
                let mutable i1 = 1
                while i1 < count do
                    let i2 = i1 + 1

                    let ni = if i2 < count && cmp x.[i1] x.[i2] > 0 then i2 else i1
                    if cmp x.[ni] element > 0 then
                        i1 <- count
                    else
                        x.[i] <- x.[ni]
                        i <- ni
                        i1 <- 2 * i + 1
                x.[i] <- element
                res


    type System.Collections.Generic.HashSet<'a> with
        member x.Consume(count : ref<int>) =
            let res = Seq.toArray x
            count := res.Length
            x.Clear()
            res

    let mutable private id = 1
    let newId() =
        let nid = 
            if id > 0 then -id - 1
            else -id + 1
        let i = id
        id <- nid
        i

    let pow (v : float) (e : float) = v ** e


type HashCode =
    static member Combine (a : uint32, b : uint32) =
        a ^^^ b + 0x9e3779b9u + (a <<< 6) + (a >>> 2) |> int

    static member Combine (a : int, b : int) =
        uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int

    static member Combine (a : int, b : int, [<ParamArray>] rest : int[]) =
        let mutable a = HashCode.Combine(a,b)
        for r in rest do    
            a <- HashCode.Combine(a, r)
        a

[<AutoOpen>]
module CoreExtensions =

    [<Emit("Object.getPrototypeOf($0).constructor.name")>]
    let private tt (o : obj) : string = failwith ""
    type System.Object with
        member x.GetTypeName() : string = tt x

[<AbstractClass; Sealed; Extension>]
type HashExtensions private() =
    [<Extension>]
    static member GetHash(value : 'a) = Unchecked.hash value


[<AbstractClass; Sealed; Extension>]
type ArrayExtensions private() =
    [<Extension>]
    static member LuFactorize(alu : Float64Array, a0 : int, ax : int, ay : int, p : int[]) =
        for i in 0 .. p.Length - 1 do p.[i] <- i
        let n = p.Length

        let mutable ak = a0
        let mutable a_k = 0

        for k in 0 .. n - 2 do
            let mutable pi = k
            let mutable pivot = alu.[ak + a_k]
            let mutable absPivot = abs pivot
            let mutable aik = ak + ay + a_k
            for i in k + 1 .. n - 1 do
                let value = alu.[aik]
                let absValue = abs value
                if absValue > absPivot then
                    pivot <- value
                    absPivot <- absValue
                    pi <- i
                aik <- aik + ay

            if absPivot < 1E-10 then
                failwith "cannot factorize"

            if pi <> k then
                let api = a0 + pi * ay
                let t = p.[pi]
                p.[pi] <- p.[k]
                p.[k] <- t

                let mutable apii = api
                let mutable aki = ak
                for i in 0 .. n - 1 do
                    let t = alu.[apii]
                    alu.[apii] <- alu.[aki]
                    alu.[aki] <- t

                    apii <- apii + ax
                    aki <- aki + ax

            let mutable aj = ak + ay
            let mutable ajk = aj + a_k
            for j in k + 1 .. n - 1 do
                let factor = alu.[ajk] / pivot
                alu.[ajk] <- factor

                let mutable aji = ajk + ax
                let mutable aki = ak + a_k + ax
                for i in k+1 .. n-1 do
                    alu.[aji] <- alu.[aji] - alu.[aki] * factor
                    aki <- aki + ax
                    aji <- aji + ax

                aj <- aj + ay
                ajk <- ajk + ay

            ak <- ak + ay
            a_k <- a_k + ax
    
    [<Extension>]
    static member LuInverse(lu : Float64Array, l0 : int, lx : int, ly : int, p : int[], x : Float64Array, x0 : int, xx : int, xy : int) =
        let n = p.Length
        let mutable xj = x0
        for j in 0 .. n - 1 do
            let mutable xji = xj
            for i in 0 .. n - 1 do
                x.[xji] <- (if p.[j] = i then 1.0 else 0.0)
                xji <- xji + xx
            xj <- xj + xy


        let mutable xk = x0 + xy
        let mutable lk = l0 + ly
        for k in 1 .. n - 1 do
            let mutable xj = x0
            let mutable lkj = lk
            for j in 0 .. k-1 do
                let mutable xki = xk
                let mutable xji = xj
                for i in 0 .. n - 1 do
                    x.[xki] <- x.[xki] - lu.[lkj] * x.[xji]
                    xki <- xki + xx
                    xji <- xji + xx

                xj <- xj + xy
                lkj <- lkj + lx
                ()
            xk <- xk + xy
            lk <- lk + ly


        let mutable k = n - 1
        let mutable xk = x0 + k * xy
        let mutable lk = l0 + k * ly
        let mutable lkk = lk + k * lx
        while k >= 0 do
            let mutable lkj = lkk + lx
            let mutable xj = xk + xy
            for j in k+1 .. n - 1 do
                let mutable xki = xk
                let mutable xji = xj
                for i in 0 .. n - 1 do
                    x.[xki] <- x.[xki] - lu.[lkj] * x.[xji]
                    xki <- xki + xx
                    xji <- xji + xx
                lkj <- lkj + lx
                xj <- xj + xy

            let factor = lu.[lkk] // storing the inverse would increase the mean error
            let mutable xki = xk
            for i in 0 .. n - 1 do
                x.[xki] <- x.[xki] / factor
                xki <- xki + xx


            k <- k - 1
            xk <- xk - xy
            lk <- lk - ly
            lkk <- lkk - ly - lx


[<AbstractClass; Sealed; Extension>]
type Fun private() =    
    [<Extension>]
    static member IsTiny (v : float) = v >= -1E-16 && v <= 1E-16
    [<Extension>]
    static member Sin(v : float) = sin v
    [<Extension>]
    static member Cos(v : float) = cos v
    [<Extension>]
    static member Tan(v : float) = tan v
    [<Extension>]
    static member Asin(v : float) = asin v
    [<Extension>]
    static member Acos(v : float) = acos v
    [<Extension>]
    static member Atan(v : float) = atan v
    [<Extension>]
    static member Atan2(y : float, x : float) = atan2 y x
    [<Extension>]
    static member Sinh(v : float) = Math.sinh v
    [<Extension>]
    static member Cosh(v : float) = Math.cosh v
    [<Extension>]
    static member Tanh(v : float) = Math.tanh v
    [<Extension>]
    static member Asinh(v : float) = Math.asinh v
    [<Extension>]
    static member Acosh(v : float) = Math.acosh v
    [<Extension>]
    static member Atanh(v : float) = Math.atanh v
    [<Extension>]
    static member Pow(v : float, e : float) = v ** e
    [<Extension>]
    static member Exp(v : float) = exp v
    [<Extension>]
    static member Log(v : float) = log v
    [<Extension>]
    static member Log2(v : float) = Math.log2 v
    [<Extension>]
    static member Sqrt(v : float) = sqrt v
    [<Extension>]
    static member Abs(v : int8) = abs v
    [<Extension>]
    static member Abs(v : int16) = abs v
    [<Extension>]
    static member Abs(v : int32) = abs v
    [<Extension>]
    static member Abs(v : int64) = abs v
    [<Extension>]
    static member Abs(v : float32) = abs v
    [<Extension>]
    static member Abs(v : float) = abs v
    
    [<Extension>]
    static member Sign(v : int8) = Math.sign (float v) |> int
    [<Extension>]
    static member Sign(v : int16) = Math.sign (float v) |> int
    [<Extension>]
    static member Sign(v : int32) = Math.sign (float v) |> int
    [<Extension>]
    static member Sign(v : int64) = Math.sign (float v) |> int
    [<Extension>]
    static member Sign(v : float) = Math.sign (float v) |> int
    [<Extension>]
    static member Floor(v : float) = floor v
    [<Extension>]
    static member Round(v : float) = round v
    [<Extension>]
    static member Ceiling(v : float) = ceil v
    [<Extension>]
    static member Frac(v : float) = v % 1.0


    
    [<Extension>]
    static member Min(a : int8, b : int8) = min a b
    [<Extension>]
    static member Min(a : int16, b : int16) = min a b
    [<Extension>]
    static member Min(a : int32, b : int32) = min a b
    [<Extension>]
    static member Min(a : int64, b : int64) = min a b
    [<Extension>]
    static member Min(a : uint8, b : uint8) = min a b
    [<Extension>]
    static member Min(a : uint16, b : uint16) = min a b
    [<Extension>]
    static member Min(a : uint32, b : uint32) = min a b
    [<Extension>]
    static member Min(a : uint64, b : uint64) = min a b
    [<Extension>]
    static member Min(a : float32, b : float32) = min a b
    [<Extension>]
    static member Min(a : float, b : float) = min a b
    
    [<Extension>]
    static member Max(a : int8, b : int8) = max a b
    [<Extension>]
    static member Max(a : int16, b : int16) = max a b
    [<Extension>]
    static member Max(a : int32, b : int32) = max a b
    [<Extension>]
    static member Max(a : int64, b : int64) = max a b
    [<Extension>]
    static member Max(a : uint8, b : uint8) = max a b
    [<Extension>]
    static member Max(a : uint16, b : uint16) = max a b
    [<Extension>]
    static member Max(a : uint32, b : uint32) = max a b
    [<Extension>]
    static member Max(a : uint64, b : uint64) = max a b
    [<Extension>]
    static member Max(a : float32, b : float32) = max a b
    [<Extension>]
    static member Max(a : float, b : float) = max a b

    static member Clamp(min : int8, max : int8, value : int8) = 
        if value < min then min
        elif value > max then max
        else value
    static member Clamp(min : int16, max : int16, value : int16) = 
        if value < min then min
        elif value > max then max
        else value
    static member Clamp(min : int32, max : int32, value : int32) = 
        if value < min then min
        elif value > max then max
        else value
    static member Clamp(min : int64, max : int64, value : int64) = 
        if value < min then min
        elif value > max then max
        else value
        
    static member Clamp(min : uint8, max : uint8, value : uint8) = 
        if value < min then min
        elif value > max then max
        else value
    static member Clamp(min : uint16, max : uint16, value : uint16) = 
        if value < min then min
        elif value > max then max
        else value
    static member Clamp(min : uint32, max : uint32, value : uint32) = 
        if value < min then min
        elif value > max then max
        else value
    static member Clamp(min : uint64, max : uint64, value : uint64) = 
        if value < min then min
        elif value > max then max
        else value
        
    static member Clamp(min : float32, max : float32, value : float32) = 
        if value < min then min
        elif value > max then max
        else value
    static member Clamp(min : float, max : float, value : float) = 
        if value < min then min
        elif value > max then max
        else value
        
    static member Lerp(min : float32, max : float32, t : float32) = 
        min + (max - min) * t
    static member Lerp(min : float, max : float, t : float) = 
        min + (max - min) * t
        
    [<Extension>]
    static member IsNaN (v : float) = Double.IsNaN v
    [<Extension>]
    static member IsInfinity (v : float) = Double.IsInfinity v


[<AbstractClass; Sealed>]
type Constant private() =
    
    static member Pi = 3.1415926535897932384626433832795
    static member PiInv = 0.318309886183790671537767526745028724068919291480912897495
    static member PiTimesTwo = 6.283185307179586476925286766559
    static member PiTimesThree = 9.424777960769379715387930149839
    static member PiTimesFour = 12.56637061435917295385057353312
    static member PiHalf = 1.570796326794896619231321691640
    static member PiQuarter = 0.78539816339744830961566084581988
    static member PiSquared = 9.869604401089358618834490999876
    static member SqrtPiTimesTwo = 2.5066282746310005024157652848110
    static member E = 2.71828182845905
    static member Sqrt2 = 1.414213562373095048801688724209
    static member Sqrt2Half = 0.70710678118654752440084436210485
    static member Sqrt3 = 1.732050807568877293527446341505
    static member Sqrt5 = 2.236067977499789696409173668731
    static member Ln2 = 0.69314718055994530941723212145818
    static member Ln2Inv = 1.4426950408889634073599246810023

    static member RadiansPerDegree = 0.0174532925199433
    static member DegreesPerRadian = 57.2957795130823
    static member SpeedOfLight = 299792458.0

    
[<AbstractClass; Sealed; Extension>]
type Conversion private() =
    [<Extension>]
    static member DegreesFromRadians (r : float) = Constant.DegreesPerRadian * r
    [<Extension>]
    static member RadiansFromDegrees (r : float) = Constant.RadiansPerDegree * r


module Prom = 
    open Aardvark.Import.Browser

    let fetchString (url : string) =
        Promise.Create(fun fin err ->
            let r = XMLHttpRequest.Create()
            r.addEventListener_load(fun e -> fin r.responseText)
            r.addEventListener_error(fun e -> err e.error)
            r.``open``("GET", url, true)
            try r.send("")
            with e -> err e
        ) |> unbox<Promise<string>>

    
    let fetchBuffer (url : string) =
        Promise.Create(fun fin err ->
            let r = XMLHttpRequest.Create()
            r.responseType <- "arraybuffer"
            r.addEventListener_error(fun e -> err e.error)
            r.addEventListener_load(fun e -> if r.status = 200.0 then fin r.response else err r.status)
            r.``open``("GET", url, true)
            try r.send("")
            with e -> err e
        ) |> unbox<Promise<ArrayBuffer>>


    let create (conts : ('a -> unit) -> (obj -> unit) -> unit) : Promise<'a> =
        Promise.Create(fun s e ->
            conts (unbox s) e
        ) |> unbox<Promise<'a>>
        
    let delay (time : int) =
        create (fun ok error ->
            setTimeout ok time |> ignore
        )

    let value (v : 'a) =
        Promise.resolve v

    let map (f : 'a -> 'b) (p : Promise<'a>) =
        create (fun s e ->
            p.``then``(
                (fun v -> s (f v)),
                (fun str -> e str)
            ) |> ignore
        )

    let all (ps : seq<Promise<'a>>) =
        Promise.all(Seq.toArray ps) |> unbox<Promise<seq<'a>>>

    let bind (f : 'a -> Promise<'b>) (m : Promise<'a>) =
        create (fun s e ->
            m.``then``(
                (fun v -> (f v).``then``(s, e) |> ignore),
                (fun err -> e err)
            ) |> ignore
        )

type Status =
    | Waiting   = 0
    | Finished  = 1
    | Canceled  = 2
    | Faulted   = 3

type Future<'a> =
    abstract member Status : Status
    abstract member Cancel : unit -> unit
    abstract member Continue : success : ('a -> unit) * error : (string -> unit) -> unit



module Future =
    open Aardvark.Import.Browser


    let create (action : ('a -> unit) -> (string -> unit) -> IDisposable) =
        let mutable callbacks = []
        let mutable status = Status.Waiting
        let mutable result = null
           
        let success v =
            if status = Status.Waiting then 
                status <- Status.Finished
                result <- v :> obj
                callbacks |> List.iter (fun (c,_) -> c v)
                callbacks <- []

        let error e =
            if status = Status.Waiting then
                status <- Status.Faulted
                result <- e :> obj
                callbacks |> List.iter (fun (_,c) -> c e)
                callbacks <- []

        let d = action success error

        { new Future<'a> with
            member x.Status = status
            member x.Cancel() = 
                if status = Status.Waiting then
                    status <- Status.Canceled
                    if unbox d then d.Dispose()
                    callbacks |> List.iter (fun (_,e) -> e "cancel")
                    callbacks <- []
            member x.Continue(success, error) =
                match status with
                | Status.Canceled -> error "cancel"
                | Status.Waiting -> callbacks <- (success,error) :: callbacks
                | Status.Finished -> success (unbox result)
                | Status.Faulted -> error (unbox result)
                | _ -> error (sprintf "bad status: %A" status)
        }


        
    let waitAll (fs : seq<Future<'a>>) =
        let arr = Seq.toArray fs
        if arr.Length = 0 then
            create (fun success error -> success (); null)
        elif arr.Length = 1 then
            create (fun s e -> arr.[0].Continue(ignore >> s, e); null)
        else
            create (fun success error ->
                let mutable cnt = arr.Length
                let s v =
                    cnt <- cnt  - 1 
                    if cnt = 0 then success ()

                let e err =
                    cnt <- -1
                    for a in arr do a.Cancel()
                    error err

                arr |> FSharp.Collections.Array.iteri (fun i a -> a.Continue(s, e))
                null
            )

    let whenAll (fs : seq<Future<'a>>) =
        let arr = Seq.toArray fs
        if arr.Length = 0 then
            create (fun success error -> success [||]; null)
        elif arr.Length = 1 then
            create (fun s e -> arr.[0].Continue(FSharp.Collections.Array.singleton >> s, e); null)
        else
            create (fun success error ->
                let res = FSharp.Collections.Array.zeroCreate arr.Length
                let mutable cnt = arr.Length
                let s i v =
                    cnt <- cnt  - 1 
                    res.[i] <- v
                    if cnt = 0 then success res

                let e err =
                    cnt <- -1
                    for a in arr do a.Cancel()
                    error err

                arr |> FSharp.Collections.Array.iteri (fun i a -> a.Continue(s i, e))
                null
            )

    let whenAny (fs : seq<Future<'a>>) =
        let arr = Seq.toArray fs
        if arr.Length = 0 then
            failwith "[Future] empty sequence in whenAny"
        elif arr.Length = 1 then
            arr.[0]
        else
            create (fun success error ->
                let mutable ecnt = arr.Length
                let mutable fin = false
                let s v =
                    ecnt <- -1
                    if not fin then 
                        fin <- true
                        success v
                let e err =
                    ecnt <- ecnt - 1
                    if ecnt = 0 then error err
             
                for a in arr do a.Continue(s, e)
                null
            )

    let map (mapping : 'a -> 'b) (f : Future<'a>) =
        create (fun success error ->
            f.Continue(
                (fun v ->
                    let res = try Choice1Of2 (mapping v) with e -> Choice2Of2 (string e)
                    match res with
                    | Choice1Of2 v -> success v
                    | Choice2Of2 e -> error e
                ),
                (fun e -> error e)
            )
            null
        )

    let bind (mappping : 'a -> Future<'b>) (f : Future<'a>) =
        create (fun success error ->
            f.Continue(
                (fun v -> 
                    let res = try Choice1Of2 (mappping(v)) with e -> Choice2Of2 (string e)
                    match res with
                    | Choice1Of2 v -> v.Continue(success, error)
                    | Choice2Of2 e -> error e
                ),
                error
            )
            null
        )

    let attempt (f : Future<'a>) =
        create (fun success _error ->
            f.Continue(Result.Ok >> success, Result.Error >> success)
            null
        )

    let tryWith (f : Future<'a>) (comp : string -> Future<'a>) =
        create (fun success error ->
            f.Continue(
                success,
                (fun e -> 
                    let res = try Choice1Of2 (comp e) with e -> Choice2Of2 (string e)
                    match res with
                    | Choice1Of2 v -> v.Continue(success, error)
                    | Choice2Of2 e -> error e
                )
            )
            null
        )

    let value (v : 'a) =
        create (fun success _ -> success v; null)


    
    let fetchBuffer (url : string) =
        create (fun fin err ->
            let r = XMLHttpRequest.Create()
            r.responseType <- "arraybuffer"
            r.addEventListener_error(fun e -> err (string e.error))
            r.addEventListener_load(fun e -> if r.status = 200.0 then fin (unbox<ArrayBuffer> r.response) elif r.status = 404.0 then err "not found" else err (string r.status))
            
            r.``open``("GET", url, true)
            try r.send("")
            with e -> err (string e)

            { new IDisposable with member x.Dispose() = r.abort() }
        )

    let tryFetchBuffer (url : string) =
        tryWith (fetchBuffer url |> map Some) (fun e -> value None) 

[<AutoOpen>]
module PromiseMonad =

    type FutureBuilder() =
        member x.Return (v : 'a) = Future.value v
        member x.Bind (m : Future<'a>, f : 'a -> Future<'b>) = Future.bind f m
        member x.Delay (f : unit -> Future<'a>) = f
        member x.Zero () = Future.value ()
        member x.Combine(l : Future<unit>, r : unit -> Future<'a>) = Future.bind r l
        member x.TryWith(b : unit -> Future<'a>, comp : string -> Future<'a>) = 
            let b = try b() |> Ok with e -> Result.Error (string e)
            match b with
            | Ok b -> Future.tryWith b comp
            | Error e -> comp e
        member x.For(s : seq<'a>, f : 'a -> Future<unit>) = s |> Seq.map f |> Future.waitAll
        member x.While(guard : unit -> bool, body : unit -> Future<unit>) =
            if guard() then x.Bind (body(), fun () -> x.While(guard, body))
            else Future.value ()

        member x.Run (f : unit -> Future<'a>) = f()

    let future = FutureBuilder()


    module Seq =
        let mapFuture (f : 'a -> Future<'b>) (l : seq<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map (fun f -> f :> seq<_>)
            
        let collectFuture (f : 'a -> Future<seq<'b>>) (l : seq<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map Seq.concat

        let chooseFuture (f : 'a -> Future<Option<'b>>) (l : seq<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map (Seq.choose id)
            
        let filterFuture (f : 'a -> Future<bool>) (l : seq<'a>) =
            let f v = v |> f |> Future.map (function true -> Some v | _ -> None)
            chooseFuture f l

    module List =
        let mapFuture (f : 'a -> Future<'b>) (l : list<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map FSharp.Collections.Array.toList
            
        let collectFuture (f : 'a -> Future<list<'b>>) (l : list<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map (Seq.concat >> Seq.toList)

        let chooseFuture (f : 'a -> Future<Option<'b>>) (l : list<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map (Seq.choose id >> Seq.toList)
            
        let filterFuture (f : 'a -> Future<bool>) (l : list<'a>) =
            let f v = v |> f |> Future.map (function true -> Some v | _ -> None)
            chooseFuture f l

    module Array =
        let mapFuture (f : 'a -> Future<'b>) (l : array<'a>) =
            l |> Seq.map f |> Future.whenAll
            
        let collectFuture (f : 'a -> Future<array<'b>>) (l : array<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map (FSharp.Collections.Array.concat)

        let chooseFuture (f : 'a -> Future<Option<'b>>) (l : array<'a>) =
            l |> Seq.map f |> Future.whenAll |> Future.map (FSharp.Collections.Array.choose id)
            
        let filterFuture (f : 'a -> Future<bool>) (l : array<'a>) =
            let f v = v |> f |> Future.map (function true -> Some v | _ -> None)
            chooseFuture f l



    type PromiseBuilder() =
        member x.Return v = Prom.value v
        member x.Bind(m : Promise<'a>, mapping : 'a -> Promise<'b>) = Prom.bind mapping m
        member x.Delay (f : unit -> Promise<'a>) = f

        member x.For(s : seq<'a>, f : 'a -> Promise<unit>) =
            s |> Seq.map f |> Seq.toArray |> Prom.all |> unbox<Promise<unit>>

        member x.Zero() = Prom.value ()
        member x.Combine(l : Promise<unit>, r : unit -> Promise<'a>) = Prom.bind r l

        member x.TryWith (f : unit -> Promise<'a>, err : obj -> Promise<'a>) =
            Prom.create (fun s realError ->
                try 
                    f().``then``(
                        (fun v -> try s v with e -> err(e).``then``(s, realError) |> ignore),
                        (fun e -> err(e).``then``(s, realError) |> ignore)
                    ) |> ignore
                with e ->
                    err(e).``then``(s, realError) |> ignore
            )

        member x.Run(f : unit -> Promise<'a>) = f()

    let promise = PromiseBuilder()


type Version(major : int, minor : int, build : int) =
    member x.Major = major
    member x.Minor = minor
    member x.Build = build

    new(major : int, minor : int) = Version(major, minor, 0)

    override x.GetHashCode() =
        HashCode.Combine(major, minor, build)

    override x.Equals o =
        match o with
        | :? Version as o -> o.Major = major && o.Minor = minor && o.Build = build
        | _ -> false

    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? Version as o -> 
                let c = compare major o.Major
                if c <> 0 then c
                else 
                    let c = compare minor o.Minor
                    if c <> 0 then c
                    else compare build o.Build
            | _ ->
                failwith "uncomparable"
