namespace Aardvark.Portable

open System
open Aardvark.Import.JS
open System.Runtime.CompilerServices



module Time =
    #if FABLE_COMPILER
    open Aardvark.Import.Browser
    let now() = performance.now()
    #else
    let private sw = System.Diagnostics.Stopwatch.StartNew()
    let now() = sw.Elapsed.TotalMilliseconds
    #endif

type CancelledException(msg : string) =
    inherit Exception(msg)
    override x.ToString() = sprintf "Cancel(%A)" msg

[<AutoOpen>]
module CancelPatterns =
    let inline (|Cancel|_|) (e : exn) =
        match e with
        | :? CancelledException as e -> Some e.Message
        | _ -> None

type CancellationSource() =
    let mutable canceled = false
    let mutable currentId = 0
    let mutable observers = System.Collections.Generic.Dictionary<int, exn -> unit>()

    member x.IsCancelled = canceled

    member x.Cancel(?msg : string) =
        let msg = defaultArg msg ""
        let e = CancelledException msg
        for c in observers.Values do c e
        canceled <- true
        observers.Clear()

    member x.Subscribe(cont : exn -> unit) =
        let id = currentId
        currentId <- id + 1
        observers.[id] <- cont
        { new IDisposable with member x.Dispose() = observers.Remove id |> ignore }

    member x.Clear() =
        observers.Clear()
        currentId <- 0

type Future<'a> = { run : CancellationSource * ('a -> unit) * (exn -> unit) -> unit }

[<AbstractClass; Sealed>]
type Interlocked private() =
    static member inline Decrement(r : ref<int>) =
        #if FABLE_COMPILER
        r := !r - 1
        !r
        #else
        System.Threading.Interlocked.Decrement(&r.contents)
        #endif

    static member inline Increment(r : ref<int>) =
        #if FABLE_COMPILER
        r := !r + 1
        !r
        #else
        System.Threading.Interlocked.Increment(&r.contents)
        #endif


    static member inline Exchange(r : ref<int>, value : int) =
        #if FABLE_COMPILER
        let o = !r
        r := value
        o
        #else
        System.Threading.Interlocked.Exchange(&r.contents, value)
        #endif

[<AbstractClass; Sealed; Extension>]
type Future private()=
    static let nop = { new IDisposable with member x.Dispose() = () }

    static member create<'a> (conts : CancellationSource -> ('a -> unit) -> (exn -> unit) -> unit) : Future<'a>  =
        { run = fun (c,s,e) ->
            let finished = ref 0
            let mutable d = nop
            let inline finish (isErr : bool) (cont : 'x -> unit) (v : 'x) =
                let f = Interlocked.Exchange(finished, 1)
                if f = 0 then
                    d.Dispose()
                    d <- nop
                    try cont v
                    with err ->
                        if isErr then 
                            Aardvark.Base.Log.error "unhandled exception in Future: %A" err
                        else 
                            try e err
                            with err -> Aardvark.Base.Log.error "unhandled exception in Future: %A" err

            d <- c.Subscribe (finish true e)
            conts c (finish false s) (finish true e)
        }

    static member sleep(ms : int) =
        #if FABLE_COMPILER 
        Future.create (fun c s e ->
            setTimeout (fun () -> s ()) ms |> ignore
        )
        #else
        Future.Await(System.Threading.Tasks.Task.Delay ms)
        #endif

    #if FABLE_COMPILER
    static member Await (p : Promise<'a>) : Future<'a> =
        Future.create (fun c s e ->
            p.``then``(fun v -> s v).catch(fun c -> e (unbox c)) |> ignore
        )
    #else
    static member Await (t : System.Threading.Tasks.Task) : Future<unit> =
        Future.create (fun c s e ->
            let a = t.GetAwaiter()
            a.OnCompleted(fun () ->
                try
                    a.GetResult()
                    s ()
                with 
                | :? System.OperationCanceledException as err -> e (CancelledException err.Message)
                | err -> e err
            )
        )
    static member Await (t : System.Threading.Tasks.Task<'a>) : Future<'a> =
        Future.create (fun c s e ->
            let a = t.GetAwaiter()
            a.OnCompleted(fun () ->
                try
                    let res = a.GetResult()
                    s res
                with 
                | :? System.OperationCanceledException as err -> e (CancelledException err.Message)
                | err -> e err
            )
        )
    #endif

    static member ofAsync (a : Async<'a>) =
        Future.create (fun c s e ->
            let cts = new System.Threading.CancellationTokenSource()
            let d = c.Subscribe(fun e -> cts.Cancel())

            let success v =
                d.Dispose()
                cts.Dispose()
                s v

            let error err =
                d.Dispose()
                cts.Dispose()
                e err

            Async.StartWithContinuations(a, success, error, (fun exn -> error (CancelledException exn.Message)), cts.Token)
        )

    static member inline bottom() = Future.create (fun _ _ _ -> ())

    static member inline error(e : exn) = Future.create (fun _ error _ -> error e)

    static member inline value (v : 'a) = { run = fun (c,s,e) -> if c.IsCancelled then e (CancelledException "cancel") else s v }
    
    static member inline later (v : unit -> 'a) = { run = fun (c,s,e) -> if c.IsCancelled then e (CancelledException "cancel") else s (v()) }

    static member inline map (mapping : 'a -> 'b) (v : Future<'a>) =
        Future.create (fun c s e ->
            v.run(c, mapping >> s, e)
        )

    static member inline bind (mapping : 'a -> Future<'b>) (v : Future<'a>) =
        Future.create (fun c s e ->
            v.run(c, (fun a -> mapping(a).run(c, s, e)), e)
        )
    
    static member inline delay (f : unit -> Future<'a>) =
        Future.create (fun c s e -> f().run(c,s,e))

    static member par (fs : array<Future<'a>>) : Future<array<'a>> =
        if fs.Length = 0 then 
            Future.value [||]
        elif fs.Length = 1 then
            fs.[0] |> Future.map (fun v -> [|v|])
        else
            Future.create<'a[]> (fun c s e ->
                let innerCancel = CancellationSource()
                let sub = c.Subscribe(fun e -> innerCancel.Cancel e.Message)

                let cnt = ref fs.Length
                let res = FSharp.Collections.Array.zeroCreate fs.Length
                for i in 0 .. fs.Length - 1 do
                    let success v =
                        res.[i] <- v
                        let rem = Interlocked.Decrement cnt
                        if rem = 0 then 
                            sub.Dispose()
                            innerCancel.Clear()
                            s res

                    let error (err : exn) =
                        sub.Dispose()
                        innerCancel.Cancel(err.Message)
                        e err

                    fs.[i].run(innerCancel, success, error)
            )

    static member par (fs : seq<Future<'a>>) : Future<seq<'a>> =
        Future.par (Seq.toArray fs) |> Future.map FSharp.Collections.Array.toSeq
        
    static member par (fs : list<Future<'a>>) : Future<list<'a>> =
        Future.par (List.toArray fs) |> Future.map FSharp.Collections.Array.toList

    static member seq (fs : array<Future<'a>>) : Future<'a[]> =
        if fs.Length = 0 then
            Future.value [||]
        elif fs.Length = 1 then
            fs.[0] |> Future.map (fun v -> [| v |])
        else
            let res = FSharp.Collections.Array.zeroCreate fs.Length
            let rec seq (i : int) (fs : array<Future<'a>>) =
                if i >= fs.Length then 
                    Future.value res
                else
                    fs.[i] |> Future.bind (fun r ->
                        res.[i] <- r
                        seq (i + 1) fs
                    )
            seq 0 fs

    static member seq (fs : seq<Future<'a>>) : Future<seq<'a>> =
        Future.seq (Seq.toArray fs) |> Future.map FSharp.Collections.Array.toSeq

    static member seq (fs : list<Future<'a>>) : Future<list<'a>> =
        Future.seq (List.toArray fs) |> Future.map FSharp.Collections.Array.toList

    static member foreach (mapping : 'a -> Future<unit>) (s : list<'a>) =
        match s with
        | [] -> Future.value ()
        | h :: t -> mapping h |> Future.bind (fun () -> Future.foreach mapping t)
        

    [<Extension>]
    static member start (f : Future<'a>, ?success : 'a -> unit, ?error : exn -> unit, ?cancel : CancellationSource) =
        let cancel = 
            match cancel with
            | Some c -> c
            | None -> CancellationSource() 

        let error =
            match error with
            | Some e -> e
            | None -> printfn "%A"

        let success =
            match success with
            | Some s -> s
            | None -> fun _ -> ()

        f.run(cancel, success, error)

[<AutoOpen>]
module FutureBuilderThings =
    
    type FutureBuilder() =
        member inline x.Using<'a, 'b when 'a :> IDisposable>(a : 'a, f: 'a -> Future<'b>) : Future<'b> =
            Future.create (fun c s e ->
                let s v =
                    try s v
                    finally a.Dispose()
                let e v =
                    try e v 
                    finally a.Dispose()
                f(a).run(c, s, e)
            )

        member inline x.Zero () = Future.value ()
        member inline x.Return (v : 'a) = Future.value v
        member inline x.ReturnFrom (f : Future<'a>) = f
        member inline x.Bind(m : Future<'a>, f : 'a -> Future<'b>) = Future.bind f m
        member inline x.Delay(f : unit -> Future<'a>) = Future.delay f
        member inline x.For(es : seq<'a>, f : 'a -> Future<unit>) = Future.foreach f (Seq.toList es)
        member x.While(g : unit -> bool, body : Future<unit>) = 
            if g() then body |> Future.bind (fun () -> x.While(g, body))
            else Future.value ()

        member x.TryWith(a : Future<'a>, comp : exn -> Future<'a>) =
            Future.create (fun c s e ->
                a.run(c, s, fun err -> comp(err).run(c,s,e))
            )

        member x.TryFinally(a : Future<'a>, comp : unit -> unit) =
            Future.create (fun c s e ->
                let s v =
                    try s v
                    finally comp()
                let e v =
                    try e v 
                    finally comp()
                a.run(c, s, e)
            )

        member x.Combine(l : Future<unit>, r : Future<'a>) =
            l |> Future.bind (fun () -> r)

    let future = FutureBuilder()

module Option =
    let rec mapf (mapping : 'a -> Future<'b>) (l : Option<'a>) =  
        match l with
        | None -> 
            Future.value None
        | Some h ->
            mapping h |> Future.map Some

    let rec bindf (mapping : 'a -> Future<Option<'b>>) (l : Option<'a>) =  
        match l with
        | None -> 
            Future.value None
        | Some h ->
            mapping h

module List =
    let rec mapf (mapping : 'a -> Future<'b>) (l : list<'a>) =  
        match l with
        | [] -> 
            Future.value []
        | h :: t ->
            mapping h |> Future.bind (fun h ->
                mapf mapping t |> Future.map (fun t -> h :: t)
            )

    let rec choosef (mapping : 'a -> Future<Option<'b>>) (l : list<'a>) =  
        match l with
        | [] ->
            Future.value []
        | h :: t ->
            mapping h |> Future.bind (fun h ->
                match h with
                | Some h -> 
                    choosef mapping t |> Future.map (fun t -> h :: t)
                | None ->
                    choosef mapping t
            )

    let rec filterf (mapping : 'a -> Future<bool>) (l : list<'a>) =  
        match l with
        | [] ->
            Future.value []
        | h :: t ->
            mapping h |> Future.bind (fun hv ->
                if hv then
                    filterf mapping t |> Future.map (fun t -> h :: t)
                else
                    filterf mapping t
            )

    let rec collectf (mapping : 'a -> Future<list<'b>>) (l : list<'a>) =
        match l with
        | [] ->
            Future.value []
        | h :: t ->
            mapping h |> Future.bind (fun h ->
                collectf mapping t |> Future.map (fun t -> h @ t)
            )

    let rec private mapifAux (i : int) (mapping : int -> 'a -> Future<'b>) (l : list<'a>) = 
        match l with
        | [] -> 
            Future.value []
        | h :: t ->
            mapping i h |> Future.bind (fun h ->
                mapifAux (i + 1) mapping t |> Future.map (fun t -> h :: t)
            )

    let mapif (mapping : int -> 'a -> Future<'b>) (l : list<'a>) = 
        mapifAux 0 mapping l

    let rec private chooseifAux (i : int) (mapping : int -> 'a -> Future<Option<'b>>) (l : list<'a>) = 
        match l with
        | [] -> 
            Future.value []
        | h :: t ->
            mapping i h |> Future.bind (fun h ->
                match h with
                | Some h -> chooseifAux (i + 1) mapping t |> Future.map (fun t -> h :: t)
                | None -> chooseifAux (i + 1) mapping t
            )

    let chooseif (mapping : int -> 'a -> Future<Option<'b>>) (l : list<'a>) = 
        chooseifAux 0 mapping l

module Seq =
    let inline mapf (mapping : 'a -> Future<'b>) (m : seq<'a>) =
        m |> Seq.toList |> List.mapf mapping |> Future.map List.toSeq

    let inline choosef (mapping : 'a -> Future<Option<'b>>) (m : seq<'a>) =
        m |> Seq.toList |> List.choosef mapping |> Future.map List.toSeq

    let inline filterf (predicate : 'a -> Future<bool>) (m : seq<'a>) =
        m |> Seq.toList |> List.filterf predicate |> Future.map List.toSeq
        
    let inline collectf (mapping : 'a -> Future<seq<'b>>) (m : seq<'a>) =
        m |> Seq.toList |> List.collectf (mapping >> Future.map Seq.toList) |> Future.map List.toSeq

    let inline mapif (mapping : int -> 'a -> Future<'b>) (m : seq<'a>) =
        m |> Seq.toList |> List.mapif mapping |> Future.map List.toSeq
        
    let inline chooseif (mapping : int -> 'a -> Future<Option<'b>>) (m : seq<'a>) =
        m |> Seq.toList |> List.chooseif mapping |> Future.map List.toSeq

module Array =
    let inline mapf (mapping : 'a -> Future<'b>) (m : array<'a>) =
        m |> List.ofArray |> List.mapf mapping |> Future.map List.toArray

    let inline choosef (mapping : 'a -> Future<Option<'b>>) (m : array<'a>) =
        m |> List.ofArray|> List.choosef mapping |> Future.map List.toArray

    let inline filterf (predicate : 'a -> Future<bool>) (m : array<'a>) =
        m |> List.ofArray |> List.filterf predicate |> Future.map List.toArray
        
    let inline collectf (mapping : 'a -> Future<array<'b>>) (m : array<'a>) =
        m |> List.ofArray |> List.collectf (mapping >> Future.map Seq.toList) |> Future.map List.toArray

    let inline mapif (mapping : int -> 'a -> Future<'b>) (m : array<'a>) =
        m |> List.ofArray |> List.mapif mapping |> Future.map List.toArray
        
    let inline chooseif (mapping : int -> 'a -> Future<Option<'b>>) (m : array<'a>) =
        m |> List.ofArray |> List.chooseif mapping |> Future.map List.toArray

module FutureTest =

    //type Binary(data : byte[], offset : int, length : int) =
        

    type IBlobStore =
        abstract member Get : file : string -> Future<byte[]>
        abstract member Set : file : string * data : byte[] * offset : int * length : int -> Future<unit>

    let run() =
        let cancel = CancellationSource()
        let a = Future.sleep 1000 |> Future.map (fun () -> cancel.Cancel("hinig"); "1000")
        let b = Future.sleep 1500 |> Future.map (fun () -> "1500")
        let f = Future.seq [| b; a |]

        let test = Future.later (fun () -> Aardvark.Base.Log.line "create"; { new IDisposable with member x.Dispose() = Aardvark.Base.Log.line "dispose"})


        let test =
            future {
                use! test = test
                Aardvark.Base.Log.line "used"
                let sa = Time.now()
                let! a = a
                Aardvark.Base.Log.line "got a %A (%.3fms)" a (Time.now() - sa)

                Aardvark.Base.Log.line "start b"
                let sb = Time.now()
                failwith "testexn"
                let! b = b
                Aardvark.Base.Log.line "got b %A (%.3fms)" b  (Time.now() - sb)
            }

        test.start(cancel = cancel)

        //f.start((fun v -> Aardvark.Base.Log.error "value: %A" v), (fun e -> Aardvark.Base.Log.error "error: %A" e), cancel)
        //cancel.Cancel()