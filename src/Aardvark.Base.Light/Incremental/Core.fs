﻿namespace Aardvark.Base.Incremental

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Aardvark.Base
open System.Collections.Concurrent
open System.Threading
open System.Linq



/// <summary>
/// IAdaptiveObject represents the core interface for all
/// adaptive objects and contains everything necessary for
/// tracking OutOfDate flags and managing in-/outputs in the
/// dependency tree.
///
/// Since eager evalutation might be desirable in some scenarios
/// the interface also contains a Level representing the execution
/// order when evaluating inside a transaction and a function called
/// Mark allowing implementations to actually perform the evaluation.
/// Mark returns a bool since eager evaluation might cause the change
/// propagation process to exit early (if the actual value was unchanged)
/// In order to make adaptive objects easily identifiable all adaptive
/// objects must also provide a globally unique id (Id)
/// </summary>
[<AllowNullLiteral>]
type IAdaptiveObject =

    abstract member Kind : string

    /// <summary>
    /// the globally unique id for the adaptive object
    /// </summary>
    abstract member Id : int

    /// <summary>
    /// the level for an adaptive object represents the
    /// maximal distance from an input cell in the depdency graph
    /// Note that this level is entirely managed by the system 
    /// and shall not be accessed directly by users of the system.
    /// </summary>
    abstract member Level : int with get, set

    /// <summary>
    /// Mark allows a specific implementation to
    /// evaluate the cell during the change propagation process.
    /// </summary>
    abstract member Mark : unit -> bool

    /// <summary>
    /// the outOfDate flag for the object is true
    /// whenever the object has been marked and shall
    /// be set to false by specific implementations.
    /// Note that this flag shall only be accessed when holding
    /// a lock on the adaptive object (allowing for concurrency)
    /// </summary>
    abstract member OutOfDate : bool with get, set

    abstract member Reevaluate : bool with get, set

    /// <summary>
    /// the adaptive inputs for the object
    /// </summary>
    abstract member Inputs : seq<IAdaptiveObject>

    /// <summary>
    /// the adaptive outputs for the object which are recommended
    /// to be represented by Weak references in order to allow for
    /// unused parts of the graph to be garbage collected.
    /// </summary>
    abstract member Outputs : HashSet<IAdaptiveObject>


    abstract member InputChanged : obj * IAdaptiveObject -> unit
    abstract member AllInputsProcessed : obj -> unit

    abstract member ReaderCount : int with get, set


type AdaptiveToken =
    struct
        val mutable public Caller : IAdaptiveObject
        val mutable public Tag : obj


        member inline x.WithCaller (c : IAdaptiveObject) =
            AdaptiveToken(c, x.Tag)

        member inline x.WithTag (t : obj) =
            AdaptiveToken(x.Caller, t)


        member inline x.Isolated =
            AdaptiveToken(x.Caller, x.Tag)

        static member inline Top = AdaptiveToken(null, null)
        static member inline Empty = Unchecked.defaultof<AdaptiveToken>

        new(caller : IAdaptiveObject, tag : obj) =
            {
                Caller = caller
                Tag = tag
            }
    end

/// <summary>
/// LevelChangedException is internally used by the system
/// to handle level changes during the change propagation.
/// </summary>
exception LevelChangedException of changedObject : IAdaptiveObject * newLevel : int * distanceFromRoot : int


/// <summary>
/// Transaction holds a set of adaptive objects which
/// have been changed and shall therefore be marked as outOfDate.
/// Commit "propagates" these changes into the dependency-graph, takes
/// care of the correct execution-order and acquires appropriate locks
/// for all objects affected.
/// </summary>
type Transaction() =
    static let emptyArray : IAdaptiveObject[] = Array.empty
    let mutable outputs = emptyArray

    // each thread may have its own running transaction
    [<ThreadStatic; DefaultValue>]
    static val mutable private RunningTransaction : Option<Transaction>

    [<ThreadStatic; DefaultValue>]
    static val mutable private CurrentTransaction : Option<Transaction>

    #if DEBUG
    let mutable isDisposed = false
    #endif


//    // each thread may have its own running transaction
//    static let running = new TrackAllThreadLocal<Option<Transaction>>(fun () -> None)
//    
    // we use a duplicate-queue here since we expect levels to be very similar 
    let q = DuplicatePriorityQueue<IAdaptiveObject, int>(fun o -> o.Level)
    //let causes = Dictionary<IAdaptiveObject, HashSet<IAdaptiveObject>>()

    // the contained set is useful for determinig if an element has
    // already been enqueued
    let contained = HashSet<IAdaptiveObject>()
    let mutable current : IAdaptiveObject = null
    let currentLevel = ref 0

    let getAndClear (set : ICollection<'a>) =
        let mutable content = []
        for e in set do content <- e::content
        set.Clear()
        content

    member x.IsContained e = contained.Contains e

    static member Running
        with get() = Transaction.RunningTransaction
        and set r = Transaction.RunningTransaction <- r

    static member Current
        with get() = Transaction.CurrentTransaction
        and set r = Transaction.CurrentTransaction <- r

    static member HasRunning =
        Transaction.RunningTransaction.IsSome
       
    static member RunningLevel =
        match Transaction.RunningTransaction with
            | Some t -> t.CurrentLevel
            | _ -> Int32.MaxValue - 1


    member x.CurrentLevel = !currentLevel

    /// <summary>
    /// enqueues an adaptive object for marking
    /// </summary>
    member x.Enqueue(e : IAdaptiveObject) =
        #if DEBUG
        if isDisposed then failwith "Invalid Enqueue! Transaction already disposed."
        #endif
        
        if contained.Add e then
            q.Enqueue e

    member x.Enqueue(e : IAdaptiveObject, cause : Option<IAdaptiveObject>) =
        #if DEBUG
        if isDisposed then failwith "Invalid Enqueue! Transaction already disposed."
        #endif
        if contained.Add e then
            q.Enqueue e
            match cause with
                | Some cause -> failwith "not implemented"
                    //match causes.TryGetValue e with
                    //    | (true, set) -> 
                    //        set.Add cause |> ignore
                    //    | _ ->
                    //        let set = HashSet [cause]
                    //        causes.[e] <- set
                | None -> ()

    member x.CurrentAdapiveObject = 
        if isNull current then None
        else Some current
        

    /// <summary>
    /// performs the entire marking process causing
    /// all affected objects to be made consistent with
    /// the enqueued changes.
    /// </summary>
    member x.Commit() =

        #if DEBUG
        if isDisposed then failwith "Invalid Commit Transaction already disposed."
        #endif


        // cache the currently running transaction (if any)
        // and make tourselves current.
        let old = Transaction.RunningTransaction
        Transaction.RunningTransaction <- Some x
        let mutable level = 0
        let myCauses = ref null
        
        let markCount = ref 0
        let traverseCount = ref 0
        let levelChangeCount = ref 0
        let outputCount = ref 0
        while q.Count > 0 do
            // dequeue the next element (having the minimal level)
            let e = q.Dequeue(currentLevel)
            current <- e

            traverseCount := !traverseCount + 1

            outputCount := 0


            // since we're about to access the outOfDate flag
            // for this object we must acquire a lock here.
            // Note that the transaction will at most hold one
            // lock at a time.
            //Monitor.Enter e

            if e.OutOfDate then
                e.AllInputsProcessed(x)

            else
                //e.EnterWrite()
                try
                    // if the element is already outOfDate we
                    // do not traverse the graph further.
                    if e.OutOfDate then
                        outputCount := 0
                        e.AllInputsProcessed(x)

                    else
                        

                        // if the object's level has changed since it
                        // was added to the queue we re-enqueue it with the new level
                        // Note that this may of course cause runtime overhead and
                        // might even change the asymptotic runtime behaviour of the entire
                        // system in the worst case but we opted for this approach since
                        // it is relatively simple to implement.
                        if !currentLevel <> e.Level then
                            q.Enqueue e
                            outputCount := 0
                        else
                            // however if the level is consistent we may proceed
                            // by marking the object as outOfDate
                            e.OutOfDate <- true
                            e.AllInputsProcessed(x)
                            markCount := !markCount + 1
                
                            try 
                                // here mark and the callbacks are allowed to evaluate
                                // the adaptive object but must expect any call to AddOutput to 
                                // raise a LevelChangedException whenever a level has been changed
                                if e.Mark() then
                                    // if everything succeeded we return all current outputs
                                    // which will cause them to be enqueued 
                                    outputs <- e.Outputs.Consume(outputCount)

                                else
                                    // if Mark told us not to continue we're done here
                                    outputCount := 0

                            with LevelChangedException(obj, objLevel, distance) ->
                                // if the level was changed either by a callback
                                // or Mark we re-enqueue the object with the new level and
                                // mark it upToDate again (since it would otherwise not be processed again)
                                e.Level <- max e.Level (objLevel + distance)
                                e.OutOfDate <- false

                                levelChangeCount := !levelChangeCount + 1

                                q.Enqueue e
                                outputCount := 0
                
                finally 
                    ()
                    //e.ExitWrite()

                // finally we enqueue all returned outputs
                for i in 0..!outputCount - 1 do
                    let o = outputs.[i]
                    o.InputChanged(x,e)
                    x.Enqueue o

            contained.Remove e |> ignore
            current <- null
            


        // when the commit is over we restore the old
        // running transaction (if any)
        Transaction.RunningTransaction <- old
        currentLevel := 0


//type private EmptyCollection<'a>() =
//    interface ICollection<'a> with
//        member x.Add _ = ()
//        member x.Clear() = ()
//        member x.Count = 0
//        member x.Contains _ = false
//        member x.Remove _ = false
//        member x.CopyTo(arr, idx) = ()
//        member x.IsReadOnly = false
//        member x.GetEnumerator() : IEnumerator<'a> = Seq.empty.GetEnumerator()
//        member x.GetEnumerator() : System.Collections.IEnumerator = Seq.empty.GetEnumerator() :> _


/// <summary>
/// defines a base class for all adaptive objects implementing
/// IAdaptiveObject.
/// </summary>
[<AllowNullLiteral; AbstractClass>]
type AdaptiveObject() =
    let mutable id = newId()
    let mutable outOfDateValue = true
    let mutable levelValue = 0
    let mutable outputs = HashSet()
    let mutable readerCountValue = 0
    let mutable reevaluate = false
    [<DefaultValue; ThreadStatic>]
    static val mutable private EvaluationDepthValue : int


    /// used for reseting EvaluationDepth in eager evaluation
    static member internal UnsaveEvaluationDepth
        with get() = AdaptiveObject.EvaluationDepthValue
        and set v = AdaptiveObject.EvaluationDepthValue <- v

    member x.Level
        with get() = if reevaluate then 0 else levelValue
        and set v = if not reevaluate then levelValue <- v

    member x.OutOfDate 
        with get() = 
            reevaluate || outOfDateValue
        and set v = 
            if not reevaluate then 
                outOfDateValue <- v

    member this.evaluate (token : AdaptiveToken) (f : AdaptiveToken -> 'a) =
        let caller = token.Caller
        let depth = AdaptiveObject.EvaluationDepthValue

        let mutable res = Unchecked.defaultof<_>

        reevaluate <- false

        try
            AdaptiveObject.EvaluationDepthValue <- depth + 1

            // this evaluation is performed optimistically
            // meaning that the "top-level" object needs to be allowed to
            // pull at least one value on every path.
            // This property must therefore be maintained for every
            // path in the entire system.
            let r = f(token.WithCaller this)
            this.OutOfDate <- false

            // if the object's level just got greater than or equal to
            // the level of the running transaction (if any)
            // we raise an exception since the evaluation
            // could be inconsistent atm.
            // the only exception to that is the top-level object itself
            let maxAllowedLevel =
                if depth > 1 then Transaction.RunningLevel - 1
                else Transaction.RunningLevel

            if this.Level > maxAllowedLevel then
                //printfn "%A tried to pull from level %A but has level %A" top.Id level top.Level
                // all greater pulls would be from the future
                raise <| LevelChangedException(this, this.Level, depth - 1)
                                                                     
            res <- r


            if not (isNull caller) then
                if reevaluate then
                    caller.Reevaluate <- true
                    caller.InputChanged(this, this)
                    caller.AllInputsProcessed(this)
                else
                    outputs.Add caller |> ignore
                    caller.Level <- max caller.Level (this.Level + 1)

        with _ ->
            AdaptiveObject.EvaluationDepthValue <- depth
            reraise()
                
        AdaptiveObject.EvaluationDepthValue <- depth


        res


    /// <summary>
    /// utility function for evaluating an object if
    /// it is marked as outOfDate. If the object is actually
    /// outOfDate the given function is executed and otherwise
    /// the given default value is returned.
    /// Note that this function takes care of appropriate locking
    /// </summary>
    member x.EvaluateIfNeeded (token : AdaptiveToken) (otherwise : 'a) (f : AdaptiveToken -> 'a) =
        x.evaluate token (fun token ->
            if x.OutOfDate then 
                f token
            else
                otherwise
        )

    /// <summary>
    /// utility function for evaluating an object even if it
    /// is not marked as outOfDate.
    /// Note that this function takes care of appropriate locking
    /// </summary>
    member x.EvaluateAlways (token : AdaptiveToken) (f : AdaptiveToken -> 'a) =
        x.evaluate token f

    abstract member MarkObj : unit -> bool
    default x.MarkObj () = true
    
    abstract member InputChangedObj : obj * IAdaptiveObject -> unit
    default x.InputChangedObj(t,ip) = ()

    abstract member AllInputsProcessedObj : obj -> unit
    default x.AllInputsProcessedObj(t) = ()

    abstract member Kind : string


    member x.Outputs = outputs

    member x.Id = id

    override x.GetHashCode() = 
        id

    override x.Equals o =
        match o with
            | :? AdaptiveObject as o -> id = o.Id
            | _ -> false

    interface IAdaptiveObject with
        member x.Id = x.Id

        member x.Kind = x.Kind

        member x.Level
            with get() = if reevaluate then 0 else levelValue
            and set v = if not reevaluate then levelValue <- v

        member x.OutOfDate 
            with get() = 
                reevaluate || outOfDateValue
            and set v = 
                if not reevaluate then 
                    outOfDateValue <- v

        member x.Reevaluate
            with get() = reevaluate
            and set v = reevaluate <- v

        member x.Outputs = outputs
        member x.Inputs = Seq.empty

        member x.Mark () = x.MarkObj ()
        member x.InputChanged(o,ip) = x.InputChangedObj(o, ip)
        member x.AllInputsProcessed(o) = x.AllInputsProcessedObj(o)
        member x.ReaderCount
            with get() = readerCountValue
            and set v = readerCountValue <- v


/// <summary>
/// defines a base class for all adaptive objects implementing
/// IAdaptiveObject and providing dirty-inputs for evaluation.
/// </summary>
[<AllowNullLiteral; AbstractClass>]
type DirtyTrackingAdaptiveObject<'a when 'a :> IAdaptiveObject>(kind : string) =
    inherit AdaptiveObject()

    let mutable scratch = HMap.empty
    let mutable dirty = HSet.empty

    override x.InputChangedObj(t,o) =
        if o.Kind = kind then 
            scratch <- scratch |> HMap.alter t (Option.defaultValue HSet.empty >> HSet.add (unbox<'a> o) >> Some)
        else
            ()

    override x.AllInputsProcessedObj(t) =
        match HMap.tryRemove t scratch with
        | Some (v, sc) ->
            scratch <- sc
            dirty <- HSet.union dirty v
        | None ->
            ()


//
//        member x.EvaluateIfNeeded' (token : AdaptiveToken) (otherwise : 'b) (compute : AdaptiveToken -> HashSet<'a> -> 'b) =
//            x.EvaluateIfNeeded token otherwise (fun token ->
//                let d = x.Dirty
//                let res = compute token d
//                d.Clear()
//                res
//            )

    member x.EvaluateAlways' (token : AdaptiveToken) (compute : AdaptiveToken -> hset<'a> -> 'b) =
        x.EvaluateAlways token (fun token ->
            let d = dirty
            dirty <- HSet.empty
            let res = compute token d
            res
        )




/// <summary>
/// defines a base class for all adaptive objects which are
/// actually constant.
/// Note that this class provides "dummy" implementations
/// for all memebers defined in IAdaptiveObject and does not 
/// keep track of in-/outputs.
/// </summary>
[<AbstractClass>]
type ConstantObject() =

    let mutable readerCount = 0

    abstract member Kind : string

    interface IAdaptiveObject with
        member x.Kind = x.Kind
        member x.Id = -1
        member x.Level
            with get() = 0
            and set l = failwith "cannot set level for constant"

        member x.Mark() = false
        member x.OutOfDate
            with get() = false
            and set o = failwith "cannot mark constant outOfDate"

        member x.Reevaluate
            with get() = false
            and set o = failwith "cannot mark constant outOfDate"

        member x.Inputs = Seq.empty
        member x.Outputs = HashSet()
        member x.InputChanged(o,ip) = ()
        member x.AllInputsProcessed(o) = ()
        member x.ReaderCount
            with get() = readerCount
            and set v = readerCount <- v




[<AutoOpen>]
module Marking =



    // since changeable inputs need a transaction
    // for enqueing their changes we use a thread local 
    // current transaction which basically allows for 
    // an implicit argument.
    //let internal current = new Threading.ThreadLocal<Option<Transaction>>(fun () -> None)

    /// <summary>
    /// returns the currently running transaction or (if none)
    /// the current transaction for the calling thread
    /// </summary>
    let getCurrentTransaction() =
        match Transaction.Running with
            | Some r -> Some r
            | None ->
                match Transaction.Current with
                    | Some c -> Some c
                    | None -> None

    let inline setCurrentTransaction t =
        Transaction.Current <- t

    /// <summary>
    /// executes a function "inside" a newly created
    /// transaction and commits the transaction
    /// </summary>
    let transact (f : unit -> 'a) =
        let t = new Transaction()
        let old = Transaction.Current
        Transaction.Current <- Some t
        let r = f()
        Transaction.Current <- old
        t.Commit()
        r


    // defines some extension utilites for
    // IAdaptiveObjects
    type IAdaptiveObject with
        /// <summary>
        /// utility for marking adaptive object as outOfDate.
        /// Note that this function will actually enqueue the
        /// object to the current transaction and will fail if
        /// no current transaction can be found.
        /// However objects which are already outOfDate might
        /// also be "marked" when not having a current transaction.
        /// </summary>
        member x.MarkOutdated (cause : Option<IAdaptiveObject>) =
            match getCurrentTransaction() with
                | Some t -> t.Enqueue(x, cause)
                | None -> 
                    lock x (fun () -> 
                        if x.OutOfDate then ()
                        elif x.Outputs.Count = 0 then x.OutOfDate <- true
                        else failwith "cannot mark object without transaction"
                    )


        member x.MarkOutdated (cause : Option<IAdaptiveObject>, fin : Option<unit -> unit>) =
            match getCurrentTransaction() with
                | Some t -> 
                    t.Enqueue(x, cause)
                    match fin with
                        | Some fin -> failwith "not supported"
                        | None -> ()
                | None -> 
                    lock x (fun () -> 
                        if x.OutOfDate then ()
                        elif x.Outputs.Count = 0 then x.OutOfDate <- true
                        else failwith "cannot mark object without transaction"
                    )
                    match fin with
                        | Some fin -> fin()
                        | None -> ()

        member x.MarkOutdated () =
            x.MarkOutdated None
               
        member x.MarkOutdated (fin : Option<unit -> unit>) =
            x.MarkOutdated(None, fin)
                            
        /// <summary>
        /// utility for adding an output to the object.
        /// Note that this will cause the output to be marked
        /// using MarkOutdated and may therefore only be used
        /// on objects being outOfDate or inside a transaction.
        /// </summary>
        member x.AddOutput(m : IAdaptiveObject) =
            m.MarkOutdated ( Some x )

        /// <summary>
        /// utility for removing an output from the object
        /// </summary>
        member x.RemoveOutput (m : IAdaptiveObject) =
            lock x (fun () -> x.Outputs.Remove m |> ignore)

//[<AutoOpen>]
//module CallbackExtensions =
    
//    let private undyingMarkingCallbacks = System.Runtime.CompilerServices.ConditionalWeakTable<IAdaptiveObject,HashSet<obj>>()

//    type private CallbackObject(inner : IAdaptiveObject, callback : CallbackObject -> unit) as this =

//        let modId = newId()
//        let mutable level = inner.Level + 1
//        let mutable live = 1
//        let mutable scope = Ag.getContext()
//        let mutable inner = inner
//        let mutable weakThis = null
//        let mutable readerCount = 0
//        do lock inner (fun () -> inner.Outputs.Add this |> ignore)

//        do lock undyingMarkingCallbacks (fun () -> undyingMarkingCallbacks.GetOrCreateValue(inner).Add this |> ignore )

//        member x.Mark() =
////            let old = AdaptiveSystemState.pushReadLocks()
////            try
//            Ag.useScope scope (fun () ->
//                callback x
//            )
////            finally
////                AdaptiveSystemState.popReadLocks old

//            false

//        interface IWeakable<IAdaptiveObject> with
//            member x.Weak =
//                let w = weakThis
//                if isNull w then 
//                    let w = WeakReference<IAdaptiveObject>(x)
//                    weakThis <- w
//                    w
//                else
//                    weakThis

//        interface IAdaptiveObject with
//            member x.Id = modId
//            member x.Level
//                with get() = System.Int32.MaxValue - 1
//                and set l = ()

//            member x.Mark() =
//                x.Mark()

//            member x.OutOfDate
//                with get() = false
//                and set o = ()

//            member x.Reevaluate
//                with get() = false
//                and set o = ()

//            member x.Inputs = Seq.singleton inner
//            member x.Outputs = VolatileCollection()
//            member x.InputChanged(o,ip) = ()
//            member x.AllInputsProcessed(o) = ()
//            member x.ReaderCount
//                with get() = readerCount
//                and set c = readerCount <- c

//        member x.Dispose() =
//            if Interlocked.Exchange(&live, 0) = 1 then
//                lock undyingMarkingCallbacks (fun () -> 
//                    match undyingMarkingCallbacks.TryGetValue(inner) with
//                        | (true,v) -> 
//                            v.Remove x |> ignore
//                            if v.Count = 0 then undyingMarkingCallbacks.Remove inner |> ignore
//                        | _ -> ()
//                )
//                inner.RemoveOutput x
//                match inner with
//                    | :? IDisposable as d -> d.Dispose()
//                    | _ -> ()
//                scope <- Unchecked.defaultof<_>
//                inner <- null

//        interface IDisposable with
//            member x.Dispose() = x.Dispose()

//    type IAdaptiveObject with

//        /// <summary>
//        /// utility for adding a "persistent" callback to
//        /// the object. returns a disposable "subscription" which
//        /// allows to destroy the callback.
//        /// </summary>
//        member x.AddMarkingCallback(f : unit -> unit) =
//            let res =
//                new CallbackObject(x, fun self ->
//                    lock x (fun _ -> 
//                        try
//                            f ()
//                        finally 
//                            x.Outputs.Add self |> ignore
//                    )
//                )

//            lock x (fun () -> x.Outputs.Add res |> ignore)

//            res :> IDisposable //{ new IDisposable with member __.Dispose() = live := false; x.MarkingCallbacks.Remove !self |> ignore}
 
//        /// <summary>
//        /// utility for adding a "persistent" callback to
//        /// the object. returns a disposable "subscription" which
//        /// allows to destroy the callback.
//        /// </summary>
//        member x.AddVolatileMarkingCallback(f : unit -> unit) =
//            let res =
//                new CallbackObject(x, fun self ->
//                    try
//                        f ()
//                        self.Dispose()
//                    with :? LevelChangedException as ex ->
//                        lock x (fun () -> x.Outputs.Add self |> ignore)
//                        raise ex

//                )

//            lock x (fun () -> x.Outputs.Add res |> ignore)

//            res :> IDisposable //{ new IDisposable with member __.Dispose() = live := false; x.MarkingCallbacks.Remove !self |> ignore}
 

//        member x.AddEvaluationCallback(f : AdaptiveToken -> unit) =
//            let res = 
//                new CallbackObject(x, fun self ->
//                    try
//                        f AdaptiveToken.Top
//                    finally 
//                        lock x (fun () -> x.Outputs.Add self |> ignore)
//                )

//            res.Mark() |> ignore

//            res :> IDisposable //{ new IDisposable with member __.Dispose() = live := false; x.MarkingCallbacks.Remove !self |> ignore}
 


//open System.Threading
 
///// <summary>
///// defines a base class for all decorated mods
///// </summary>
//type AdaptiveDecorator(o : IAdaptiveObject) =
//    let mutable o = o
//    let id = newId()
//    let mutable weakThis = null
//    let mutable readerCount = 0
//    member x.Id = id
//    member x.OutOfDate
//        with get() = o.OutOfDate
//        and set v = o.OutOfDate <- v

//    member x.Outputs = o.Outputs
//    member x.Inputs = o.Inputs
//    member x.Level 
//        with get() = o.Level
//        and set l = o.Level <- l

//    member x.Mark() = o.Mark()

//    override x.GetHashCode() = id
//    override x.Equals o =
//        match o with
//            | :? IAdaptiveObject as o -> x.Id = id
//            | _ -> false

//    interface IWeakable<IAdaptiveObject> with
//        member x.Weak =
//            let w = weakThis
//            if isNull w then 
//                let w = WeakReference<IAdaptiveObject>(x)
//                weakThis <- w
//                w
//            else
//                weakThis


//    interface IAdaptiveObject with
//        member x.Id = id
//        member x.OutOfDate
//            with get() = o.OutOfDate
//            and set v = o.OutOfDate <- v

//        member x.Reevaluate
//            with get() = o.Reevaluate
//            and set v = o.Reevaluate <- v


//        member x.Outputs = o.Outputs
//        member x.Inputs = o.Inputs
//        member x.Level 
//            with get() = o.Level
//            and set l = o.Level <- l

//        member x.Mark () = o.Mark()

//        member x.InputChanged(t,ip) = o.InputChanged (t,ip)
//        member x.AllInputsProcessed(t) = o.AllInputsProcessed(t)
//        member x.ReaderCount
//            with get() = readerCount
//            and set c = readerCount <- c
 
//type VolatileDirtySet<'a, 'b when 'a :> IAdaptiveObject and 'a : equality and 'a : not struct>(eval : 'a -> 'b) =
//    static let empty = ref HSet.empty
//    let mutable set : ref<hset<'a>> = ref HSet.empty

//    member x.Evaluate() =
//        let local = !Interlocked.Exchange(&set, empty) 
//        try
//            local 
//                |> HSet.toList
//                |> List.filter (fun o -> lock o (fun () -> o.OutOfDate))
//                |> List.map (fun o -> eval o)

//        with :? LevelChangedException as l ->
//            Interlocked.Change(&set, fun s -> ref (HSet.union local !s)) |> ignore
//            raise l

//    member x.Push(i : 'a) =
//        lock i (fun () ->
//            if i.OutOfDate then
//                Interlocked.Change(&set, fun s -> ref (HSet.add i !s)) |> ignore
//        )

//    member x.Add(i : 'a) =
//        x.Push(i)

//    member x.Remove(i : 'a) =
//        Interlocked.Change(&set, fun s -> ref (HSet.remove i !s)) |> ignore
 
//    member x.Clear() =
//        Interlocked.Exchange(&set, empty) |> ignore

//type MutableVolatileDirtySet<'a, 'b when 'a :> IAdaptiveObject and 'a : equality and 'a : not struct>(eval : 'a -> 'b) =
//    let lockObj = obj()
//    let set = HashSet<'a>()

//    member x.Evaluate() =
//        lock lockObj (fun () ->
//            let res = set |> Seq.toList
//            set.Clear()
//            res |> List.filter (fun o -> lock o (fun () -> o.OutOfDate))
//                |> List.map (fun o -> eval o)
//        )

//    member x.Push(i : 'a) =
//        lock lockObj (fun () ->
//            lock i (fun () ->
//                if i.OutOfDate then
//                    set.Add i |> ignore
//            )
//        )

//    member x.Add(i : 'a) =
//        x.Push(i)

//    member x.Remove(i : 'a) =
//        lock lockObj (fun () ->
//            set.Remove i |> ignore
//        )
 
//    member x.Clear() =
//        lock lockObj (fun () ->
//            set.Clear()
//        )


//type VolatileTaggedDirtySet<'a, 'b, 't when 'a :> IAdaptiveObject and 'a : equality and 'a : not struct>(eval : 'a -> 'b) =
//    static let empty = ref HSet.empty
//    let mutable set : ref<hset<'a>> = empty
//    let tagDict = Dictionary<'a, HashSet<'t>>()

//    member x.Evaluate() =
//        lock tagDict (fun () ->
//            let local = !Interlocked.Exchange(&set, empty) 
//            try
//                local |> HSet.toList
//                      |> List.filter (fun o -> lock o (fun () -> o.OutOfDate))
//                      |> List.map (fun o ->
//                            match tagDict.TryGetValue o with
//                                | (true, tags) -> o, Seq.toList tags
//                                | _ -> o, []
//                         )
//                      |> List.map (fun (o, tags) -> eval o, tags)

//            with :? LevelChangedException as l ->
//                Interlocked.Change(&set, fun s -> ref (HSet.union local !s)) |> ignore
//                raise l
//        )

//    member x.Push(i : 'a) =
//        lock tagDict (fun () ->
//            lock i (fun () ->
//                if i.OutOfDate && tagDict.ContainsKey i then
//                    Interlocked.Change(&set, fun s -> ref (HSet.add i !s)) |> ignore
//            )
//        )

//    member x.Add(tag : 't, i : 'a) =
//        lock tagDict (fun () ->
//            match tagDict.TryGetValue i with
//                | (true, set) -> 
//                    set.Add tag |> ignore
//                    false
//                | _ ->
//                    tagDict.[i] <- HashSet [tag]
//                    x.Push i
//                    true
//        )

//    member x.Remove(tag : 't, i : 'a) =
//        lock tagDict (fun () ->
//            match tagDict.TryGetValue i with
//                | (true, tags) -> 
//                    if tags.Remove tag then
//                        if tags.Count = 0 then
//                            Interlocked.Change(&set, fun s -> ref (HSet.remove i !s)) |> ignore
//                            true
//                        else
//                            false
//                    else
//                        failwithf "[VolatileTaggedDirtySet] could not remove tag %A for element %A" tag i
                                      
//                | _ ->
//                    failwithf "[VolatileTaggedDirtySet] could not remove element: %A" i
//        )

//    member x.Clear() =
//        lock tagDict (fun () ->
//            tagDict.Clear()
//            Interlocked.Exchange(&set, empty) |> ignore
//        )

//type MutableVolatileTaggedDirtySet<'a, 'b, 't when 'a :> IAdaptiveObject and 'a : equality and 'a : not struct>(eval : 'a -> 'b) =
//    let set = HashSet<'a>()
//    let tagDict = Dictionary<'a, HashSet<'t>>()

//    member x.Evaluate() =
//        lock tagDict (fun () ->
//            try
//                let result = 
//                    set |> Seq.toList
//                        |> List.filter (fun o -> lock o (fun () -> o.OutOfDate))
//                        |> List.choose (fun o ->
//                              match tagDict.TryGetValue o with
//                                  | (true, tags) -> Some(o, Seq.toList tags)
//                                  | _ -> None
//                           )
//                        |> List.map (fun (o, tags) -> eval o, tags)

//                set.Clear()
//                result

//            with :? LevelChangedException as l ->
//                raise l
//        )

//    member x.Push(i : 'a) =
//        lock tagDict (fun () ->
//            lock i (fun () ->
//                if i.OutOfDate && tagDict.ContainsKey i then
//                    set.Add i |> ignore
//            )
//        )

//    member x.Add(tag : 't, i : 'a) =
//        lock tagDict (fun () ->
//            match tagDict.TryGetValue i with
//                | (true, set) -> 
//                    set.Add tag |> ignore
//                    false
//                | _ ->
//                    tagDict.[i] <- HashSet [tag]
//                    x.Push i
//                    true
//        )

//    member x.Remove(tag : 't, i : 'a) =
//        lock tagDict (fun () ->
//            match tagDict.TryGetValue i with
//                | (true, tags) -> 
//                    if tags.Remove tag then
//                        if tags.Count = 0 then
//                            set.Remove i |> ignore
//                            true
//                        else
//                            false
//                    else
//                        failwithf "[VolatileTaggedDirtySet] could not remove tag %A for element %A" tag i
                                      
//                | _ ->
//                    failwithf "[VolatileTaggedDirtySet] could not remove element: %A" i
//        )

//    member x.Clear() =
//        lock tagDict (fun () ->
//            tagDict.Clear()
//            set.Clear()
//        )
