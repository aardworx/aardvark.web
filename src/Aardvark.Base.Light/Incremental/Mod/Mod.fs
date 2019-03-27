namespace Aardvark.Base.Incremental

open System
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Collections.Concurrent
open Aardvark.Base

/// <summary>
/// IMod is the non-generic base interface for 
/// modifiable cells. This is needed due to the
/// lack of existential types in the .NET.
/// </summary>
[<AllowNullLiteral>]
type IMod =
    inherit IAdaptiveObject

    /// <summary>
    /// returns whether or not the cell's content 
    /// will remain constant. 
    /// </summary>
    abstract member IsConstant : bool

    /// <summary>
    /// returns the cell's content and evaluates
    /// the respective computation if needed.
    /// </summary>
    abstract member GetValueObj : AdaptiveToken -> obj




/// <summary>
/// IMod<'a> represents the base interface for
/// modifiable cells and provides a method for
/// getting the cell's current content.
/// </summary>
[<AllowNullLiteral>]
type IMod<'a> =
    inherit IMod

    /// <summary>
    /// returns the cell's content and evaluates
    /// the respective computation if needed.
    /// </summary>
    abstract member GetValue : AdaptiveToken -> 'a
    

/// <summary>
/// ModRef<'a> represents a changeable input
/// cell which can be changed by the user and
/// implements IMod<'a>
/// </summary>
type IModRef<'a> =
    inherit IMod<'a>

    /// Gets or sets the refs value.
    /// Note: can only be set inside an active transaction.
    abstract member Value : 'a with get,set

    abstract member UnsafeCache : 'a with get,set


/// <summary>
/// ModRef<'a> represents a changeable input
/// cell which can be changed by the user and
/// implements IMod<'a>
/// </summary>
type ModRef<'a>(value : 'a) =
    inherit AdaptiveObject()

    let mutable value = value
    let mutable cache = value
    
    override x.Kind = "Mod"
    member x.UnsafeCache
        with get() = value
        and set v = value <- v

    member x.Value
        with get() = value
        and set v =
            if not <| Object.Equals(v, value) then
                value <- v
                x.MarkOutdated()


    interface IMod<'a> with
        member x.GetValueObj(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    cache <- value
                cache :> obj
            )
        member x.GetValue(token : AdaptiveToken) =
            x.EvaluateAlways token (fun token ->
                if x.OutOfDate then
                    cache <- value
                cache
            )

        member x.IsConstant = false

    interface IModRef<'a> with
        member x.UnsafeCache
            with get() = value
            and set v = value <- v

        member x.Value
            with get() = value
            and set v =
                if not <| Object.Equals(v, value) then
                    value <- v
                    x.MarkOutdated()



    override x.ToString() =
        sprintf "{ value = %A }" value




// ConstantMod<'a> represents a constant mod-cell
// and implements IMod<'a> (making use of the core
// class ConstantObject). Note that ConstantMod<'a> allows
// computations to be delayed (which is useful if the
// creation of the value is computationally expensive)
// Note that constant cells are considered equal whenever
// their content is equal. Therefore equality checks will 
// force the evaluation of a constant cell.
type ConstantMod<'a>(value : Lazy<'a>) =
    inherit ConstantObject()

    override x.Kind = "Mod"

    member x.Value =
        value.Value

    interface IMod with
        member x.GetValueObj(token : AdaptiveToken) = 
            value.Value :> obj
            
        member x.IsConstant = true
        
    interface IMod<'a> with
        member x.GetValue(token : AdaptiveToken) = 
            value.Value
                
            
    override x.GetHashCode() =
        let v = value.Value :> obj
        if isNull v then 0
        else Unchecked.hash v

    override x.Equals o =
        match o with
            | :? ConstantMod<'a> as o ->
                System.Object.Equals(value.Value, o.Value)
            | _ -> 
                false

    override x.ToString() =
        value.Value.ToString()

    new(value : 'a) = ConstantMod<'a>(System.Lazy<'a>.CreateFromValue value)
    new(compute : unit -> 'a) = ConstantMod<'a>( lazy (compute()) )


/// <summary>
/// defines functions for composing mods and
/// managing evaluation order, etc.
/// </summary>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Mod =
    let mutable private depth = 0

    [<AbstractClass>]
    type AbstractMod<'a>() =
        inherit AdaptiveObject()



        let mutable cache = Unchecked.defaultof<'a>
        
        override x.Kind = "Mod"
        abstract member Compute : AdaptiveToken -> 'a

        interface IMod with
            member x.IsConstant = false
            member x.GetValueObj(token) =
                x.EvaluateAlways token (fun token ->
                    if x.OutOfDate then
                        cache <- x.Compute(token)
                    cache :> obj
                )

        interface IMod<'a> with
            member x.GetValue(token) =
                x.EvaluateAlways token (fun token ->
                    if x.OutOfDate then
                        cache <- x.Compute(token)
                    cache
                )

        override x.MarkObj () =
            cache <- Unchecked.defaultof<_>
            true

        override x.ToString() =
            if x.OutOfDate then sprintf "{ cache = %A (outOfDate) }" cache
            else sprintf "{ value = %A }" cache

    //[<AbstractClass>]
    //type AbstractDirtyTrackingMod<'i, 'a when 'i :> IAdaptiveObject> =
    //    class
    //        inherit DirtyTrackingAdaptiveObject<'i>
    //        val mutable public cache : 'a
    //        val mutable public scope : Ag.Scope

    //        abstract member Compute : AdaptiveToken * HashSet<'i> -> 'a

    //        member x.GetValue(token) =
    //            x.EvaluateAlways' token (fun (token : AdaptiveToken) (dirty : HashSet<'i>) ->
    //                if x.OutOfDate then
    //                    Ag.useScope x.scope (fun () ->
    //                        x.cache <- x.Compute(token,dirty)
    //                    )
    //                x.cache
    //            )

    //        override x.Mark () =
    //            x.cache <- Unchecked.defaultof<_>
    //            true

    //        override x.ToString() =
    //            if x.OutOfDate then sprintf "{ cache = %A (outOfDate) }" x.cache
    //            else sprintf "{ value = %A }" x.cache

    //        interface IMod with
    //            member x.IsConstant = false
    //            member x.GetValue(caller) = x.GetValue(caller) :> obj

    //        interface IMod<'a> with
    //            member x.GetValue(caller) = x.GetValue(caller)

    //        new() =
    //            { cache = Unchecked.defaultof<'a>; scope = Ag.getContext() }
    //    end


    //[<AbstractClass>]
    //type AbstractModWithFinalizer<'a>() =
    //    inherit AbstractMod<'a>()
    //    abstract member Release : unit -> unit
    //    default x.Release() = ()

    //    override x.Finalize() =
    //        try
    //            x.Release()
    //            x.cache <- Unchecked.defaultof<_> // TODO: not sure whether this makes things worse or better
    //            x.scope <- Ag.emptyScope
    //        with e ->
    //            ()

    // LazyMod<'a> (as the name suggests) implements IMod<'a>
    // and will be evaluated lazily (if not forced to be eager
    // by a callback or subsequent eager computations)
    type LazyMod<'a>(compute : AdaptiveToken -> 'a) =
        inherit AbstractMod<'a>()
        override x.Compute(token) = compute(token)



    type internal MapMod<'a, 'b>(inner : IMod<'a>, f : 'a -> 'b) =
        inherit AbstractMod<'b>()

        member x.Inner = inner
        member x.F = f

        override x.Compute(token) =
            inner.GetValue token |> f

    type internal Map2Mod<'a, 'b, 'c>(a : IMod<'a>, b : IMod<'b>, f : 'a -> 'b -> 'c) =
        inherit AbstractMod<'c>()

        member x.Left = a
        member x.Right = a
        member x.F = f

        override x.Compute(token) =
            f (a.GetValue token) (b.GetValue token)


    type internal BindMod<'a, 'b>(m : IMod<'a>, f : 'a -> IMod<'b>) =
        inherit AbstractMod<'b>()

        let mutable inner : Option<'a * IMod<'b>> = None
        let mutable mChanged = 1

        override x.InputChangedObj(t, i) =
            if i.Id = m.Id then
                mChanged <- 1

        override x.Compute(token) =
            // whenever the result is outOfDate we
            // need to pull the input's value
            // Note that the input is not necessarily outOfDate at this point
            let v = m.GetValue token

            let changed = 
                let m = mChanged
                mChanged <- 0
                m

            //let cv = hasChanged v

            let mChanged = changed <> 0

            match inner with
                // if the function argument has not changed
                // since the last execution we expect f to return
                // the identical cell
                | Some (v', inner) when not mChanged ->
                    // since the inner cell might be outOfDate we
                    // simply pull its value and don't touch any in-/outputs.
                    inner.GetValue token
                        
                | _ ->
                    // whenever the argument's value changed we need to 
                    // re-execute the function and store the new inner cell.
                    let i = f v :> IMod<_>
                    let old = inner
                    inner <- Some (v, i)


                    match old with
                        // if there was an old inner cell which
                        // is different from the new one we
                        // remove the resulting cell from the old
                        // outputs and add it to the new ones. 
                        | Some (_,old) when old <> i -> 
                            old.RemoveOutput x |> ignore

                        // in any other case the graph remained
                        // constant and we don't change a thing.
                        | _ -> ()

                    // finally we pull the value from the
                    // new inner cell.
                    i.GetValue token

    type internal Bind2Mod<'a, 'b, 'c>(ma : IMod<'a>, mb : IMod<'b>, f : 'a -> 'b -> IMod<'c>) =
        inherit AbstractMod<'c>()
        static let empty = ref HSet.empty

        let mutable inner : Option<'a * 'b * IMod<'c>> = None
        let mutable changedInputs = empty

        override x.InputChangedObj(t, i) =
            changedInputs := HSet.add i !changedInputs

        override x.Compute(token) =
            let changed = 
                let c = !changedInputs
                changedInputs <- empty
                c

            let a = ma.GetValue token
            let b = mb.GetValue token

            let ca = HSet.contains (ma :> IAdaptiveObject) changed
            let cb = HSet.contains (mb :> IAdaptiveObject) changed

            match inner with
                | Some (va, vb, inner) when not ca && not cb ->
                    inner.GetValue token
                | _ ->

                    let i = f a b :> IMod<_>
                    let old = inner
                    inner <- Some (a, b, i)

                    match old with
                        | Some (_,_,old) when old <> i -> 
                            old.RemoveOutput x |> ignore

                        | _ -> ()

                        
                    i.GetValue token 
 
    type internal DynamicMod<'a>(f : unit -> IMod<'a>) =
        inherit AbstractMod<'a>()

        let inner = lazy (f())

        override x.Compute(token) =
            inner.Value.GetValue token

    /// <summary>
    /// creates a custom modifiable cell using the given
    /// compute function. If no inputs are added to the
    /// cell it will actually be constant.
    /// However the system will not statically assume the
    /// cell to be constant in any case.
    /// </summary>
    let custom (compute : AdaptiveToken -> 'a) : IMod<'a> =
        LazyMod(compute) :> IMod<_>



    /// <summary>
    /// changes the value of the given cell. Note that this
    /// function may only be used inside a current transaction.
    /// </summary>
    let change (m : IModRef<'a>) (value : 'a) =
        m.Value <- value


    /// <summary>
    /// initializes a new constant cell using the given value.
    /// </summary>
    let constant (v : 'a)  =
        ConstantMod<'a>(v) :> IMod<_>

    /// <summary>
    /// initializes a new modifiable input cell using the given value.
    /// </summary>
    let init (v : 'a) =
        ModRef v


    /// <summary>
    /// initializes a new constant cell using the given lazy value.
    /// </summary>
    let delay (f : unit -> 'a) =
        ConstantMod<'a> (f) :> IMod<_>

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// resulting in a new dependent cell.
    /// </summary>
    let map (f : 'a -> 'b) (m : IMod<'a>) =
        if m.IsConstant then
            delay (fun () -> m.GetValue(AdaptiveToken.Empty) |> f)
        else
            MapMod(m, f) :> IMod<_>

    /// <summary>
    /// adaptively applies a function to two cell's values
    /// resulting in a new dependent cell.
    /// </summary>
    let map2 (f : 'a -> 'b -> 'c) (m1 : IMod<'a>) (m2 : IMod<'b>)=
        match m1.IsConstant, m2.IsConstant with
            | (true, true) -> 
                delay (fun () -> f (m1.GetValue(AdaptiveToken.Empty)) (m2.GetValue(AdaptiveToken.Empty))) 
            | (true, false) -> 
                map (fun b -> f (m1.GetValue(AdaptiveToken.Empty)) b) m2
            | (false, true) -> 
                map (fun a -> f a (m2.GetValue(AdaptiveToken.Empty))) m1
            | (false, false) ->
                Map2Mod(m1, m2, f) :> IMod<_>



    /// <summary>
    /// creates a modifiable cell using the given inputs
    /// and compute function (being evaluated whenever any of
    /// the inputs changes.
    /// </summary>
    let mapN (f : seq<'a> -> 'b) (inputs : seq<#IMod<'a>>) =
        let inputs : list<IMod<'a>> = Seq.toList (Seq.cast inputs)
        custom (fun token ->
            let values = inputs |> List.map (fun i -> i.GetValue token)
            f values
        )
        //MapNMod(Seq.toList (Seq.cast inputs), List.toSeq >> f) :> IMod<_>
//        let objs = inputs |> Seq.cast |> Seq.toList
//        objs |> mapCustom (fun s ->
//            let values = inputs |> Seq.map (fun m -> m.GetValue s) |> Seq.toList
//            f values
//        )

    /// <summary>
    /// adaptively applies a function to a cell's value
    /// and returns a new dependent cell holding the inner
    /// cell's content.
    /// </summary>
    let bind (f : 'a -> #IMod<'b>) (m : IMod<'a>) =
        if m.IsConstant then
            m.GetValue(AdaptiveToken.Empty) |> f :> IMod<_>
        else
            BindMod(m, fun v -> f v :> _) :> IMod<_>
      

    /// <summary>
    /// adaptively applies a function to two cell's values
    /// and returns a new dependent cell holding the inner
    /// cell's content.
    /// </summary>
    let bind2 (f : 'a -> 'b -> #IMod<'c>) (ma : IMod<'a>) (mb : IMod<'b>) =
        match ma.IsConstant, mb.IsConstant with
            | (true, true) ->
                f (ma.GetValue(AdaptiveToken.Empty)) (mb.GetValue(AdaptiveToken.Empty)) :> IMod<_>
            | (false, true) ->
                bind (fun a -> (f a (mb.GetValue(AdaptiveToken.Empty))) :> IMod<_>) ma
            | (true, false) ->
                bind (fun b -> (f (ma.GetValue(AdaptiveToken.Empty)) b) :> IMod<_>) mb
            | (false, false) ->
                Bind2Mod(ma, mb, fun a b -> (f a b) :> _) :> IMod<_>

    /// <summary>
    /// creates a dynamic cell using the given function
    /// while maintaining lazy evaluation.
    /// </summary>
    let dynamic (f : unit -> IMod<'a>) =
        DynamicMod(f) :> IMod<_>


    /// <summary>
    /// forces the evaluation of a cell and returns its current value
    /// </summary>
    let force (m : IMod<'a>) =
        m.GetValue(AdaptiveToken.Top)


    let useCurrent<'a when 'a :> IDisposable> (f : AdaptiveToken -> 'a) : IMod<'a>  =
        let current = ref None

        custom (fun self ->
            match !current with
             | None -> 
                let v = f self
                current := Some v
                v
             | Some v ->
                v.Dispose()
                let v = f self
                current :=  Some v
                v
        )


//[<AutoOpen>]
//module ModExtensions =
    
//    // reflect the type argument used by a given
//    // mod-type or return None if no mod type.
//    let rec private extractModTypeArg (t : Type) (typedef : Type) =
//        if t.IsGenericType && t.GetGenericTypeDefinition() = typedef then
//            Some (t.GetGenericArguments().[0])
//        else
//            let iface = t.GetInterface(typedef.FullName)
//            if isNull iface then None
//            else extractModTypeArg iface typedef

//    /// <summary>
//    /// matches all types implementing IMod<'a> and
//    /// extracts typeof<'a> using reflection.
//    /// </summary>
//    let (|ModRefOf|_|) (t : Type) =
//        match extractModTypeArg t typedefof<ModRef<_>> with
//            | Some t -> ModRefOf t |> Some
//            | None -> None

//    let (|ModOf|_|) (t : Type) =    
//        match extractModTypeArg t typedefof<IMod<_>> with
//            | Some t -> ModOf t |> Some
//            | None -> None


[<AutoOpen>]
module EvaluationUtilities =

    let evaluateTopLevel (f : unit -> 'a) : 'a =
        let currentTransaction = Transaction.Running
        Transaction.Running <- None

        try
            f ()
        finally
            Transaction.Running <- currentTransaction

