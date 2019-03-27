namespace Aardvark.Base.Incremental

open System
open System.Collections
open System.Collections.Generic
open Aardvark.Base.Incremental
open Aardvark.Base

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module ASet =

    [<AutoOpen>]
    module Implementation = 
        type EmptyReader<'a> private() =
            inherit ConstantObject()

            static let instance = new EmptyReader<'a>() :> IOpReader<_,_>
            static member Instance = instance


            override x.Kind = "SetReader"
            interface IOpReader<hdeltaset<'a>> with
                member x.Dispose() =
                    ()

                member x.GetOperations caller =
                    HDeltaSet.empty

            interface IOpReader<hrefset<'a>, hdeltaset<'a>> with
                member x.State = HRefSet.empty

        type EmptySet<'a> private() =
            let content = Mod.constant HRefSet.empty

            static let instance = EmptySet<'a>() :> aset<_>

            static member Instance = instance


            interface aset<'a> with
                member x.IsConstant = true
                member x.Content = content
                member x.GetReader() = EmptyReader.Instance

        type ConstantSet<'a>(content : Lazy<hrefset<'a>>) =
            let deltas = lazy ( content.Value |> Seq.map Add |> HDeltaSet.ofSeq)
            let mcontent = ConstantMod<hrefset<'a>>(content) :> IMod<_>

            interface aset<'a> with
                member x.IsConstant = true
                member x.GetReader() = new History.Readers.ConstantReader<_,_>(HRefSet.trace, deltas, content, "SetReader") :> ISetReader<_>
                member x.Content = mcontent
        
            new(content : hrefset<'a>) = ConstantSet<'a>(System.Lazy<hrefset<_>>.CreateFromValue content)

        type AdaptiveSet<'a>(newReader : unit -> IOpReader<hdeltaset<'a>>) =
            let h = History.ofReader HRefSet.trace newReader

            interface aset<'a> with
                member x.IsConstant = false
                member x.Content = h :> IMod<_>
                member x.GetReader() = h.NewReader("SetReader")

        let inline unexpected() =
            failwith "[ASet] deltas are expected to be unique"

        let inline aset (f : unit -> #IOpReader<hdeltaset<'a>>) =
            AdaptiveSet<'a>(fun () -> f() :> IOpReader<_>) :> aset<_>

        let inline constant (l : Lazy<hrefset<'a>>) =
            ConstantSet<'a>(l) :> aset<_>

    [<AutoOpen>]
    module Readers =
        type MapReader<'a, 'b>(input : aset<'a>, f : 'a -> 'b) =
            inherit AbstractReader<hdeltaset<'b>>(HDeltaSet.monoid)
            


            let cache = Cache f
            let r = input.GetReader()

            override x.Kind = "SetReader"

            override x.Release() =
                r.Dispose()
                cache.Clear ignore

            override x.Compute(token) =
                r.GetOperations token |> HDeltaSet.map (fun d ->
                    match d with
                        | Add(1, v) -> Add(cache.Invoke v)
                        | Rem(1, v) -> Rem(cache.Revoke v)
                        | _ -> unexpected()
                )
                
        type MapUseReader<'a, 'b when 'b :> IDisposable>(input : aset<'a>, f : 'a -> 'b) =
            inherit AbstractReader<hdeltaset<'b>>(HDeltaSet.monoid)
            
            let cache = Cache f
            let r = input.GetReader()
            
            override x.Kind = "SetReader"
            override x.Release() =
                r.Dispose()

            override x.Compute(token) =
                r.GetOperations token |> HDeltaSet.map (fun d ->
                    match d with
                        | Add(1, v) -> 
                            Add(cache.Invoke v)

                        | Rem(1, v) -> 
                            let del, value = cache.RevokeAndGetDeleted v
                            if del then value.Dispose()
                            Rem(value)

                        | _ -> 
                            unexpected()
                )

        type ChooseReader<'a, 'b>(input : aset<'a>, f : 'a -> Option<'b>) =
            inherit AbstractReader<hdeltaset<'b>>(HDeltaSet.monoid)
            
            let cache = Cache f
            let r = input.GetReader()
            
            override x.Kind = "SetReader"
            override x.Release() =
                r.Dispose()
                cache.Clear ignore

            override x.Compute(token) =
                r.GetOperations token |> HDeltaSet.choose (fun d ->
                    match d with
                        | Add(1, v) -> 
                            match cache.Invoke v with
                                | Some v -> Some (Add v)
                                | None -> None
                        | Rem(1, v) ->
                            match cache.Revoke v with
                                | Some v -> Some (Rem v)
                                | None -> None
                        | _ -> 
                            unexpected()
                )

        type FilterReader<'a>(input : aset<'a>, f : 'a -> bool) =
            inherit AbstractReader<hdeltaset<'a>>(HDeltaSet.monoid)
            
            let cache = Cache f
            let r = input.GetReader()
            
            override x.Kind = "SetReader"
            override x.Release() =
                r.Dispose()
                cache.Clear ignore

            override x.Compute(token) =
                r.GetOperations token |> HDeltaSet.choose (fun d ->
                    match d with
                        | Add(1, v) -> 
                            if cache.Invoke v then Some (Add v)
                            else None
                        | Rem(1, v) ->
                            if cache.Revoke v then Some (Rem v)
                            else None
                        | _ -> 
                            unexpected()
                )

        type UnionReader<'a>(input : aset<aset<'a>>) =
            inherit AbstractDirtyReader<ISetReader<'a>, hdeltaset<'a>>(HDeltaSet.monoid, "SetReader")

            let r = input.GetReader()
            let cache = Cache(fun (a : aset<'a>) -> a.GetReader())
            
            override x.Kind = "SetReader"
            override x.Release() =
                r.Dispose()
                cache.Clear (fun r -> r.Dispose())

            override x.Compute(token,dirty) =
                let mutable deltas = 
                    r.GetOperations token |> HDeltaSet.collect (fun d ->
                        match d with
                            | Add(1, v) ->
                                let r = cache.Invoke v
                                dirty.Remove r |> ignore
                                r.GetOperations token

                            | Rem(1, v) -> 
                                let deleted, r = cache.RevokeAndGetDeleted v
                                dirty.Remove r |> ignore
                                if deleted then 
                                    let ops = HRefSet.computeDelta r.State HRefSet.empty
                                    r.Dispose()
                                    ops
                                else
                                    r.GetOperations token
                                
                            | _ -> unexpected()
                    )

                for d in dirty do
                    deltas <- HDeltaSet.combine deltas (d.GetOperations token)

                deltas

        type UnionFixedReader<'a>(input : hrefset<aset<'a>>) =
            inherit AbstractDirtyReader<ISetReader<'a>, hdeltaset<'a>>(HDeltaSet.monoid, "SetReader")

            let mutable initial = true
            let input = input |> HRefSet.map (fun s -> s.GetReader())
            
            override x.Kind = "SetReader"
            override x.Release() =
                for i in input do
                    i.Dispose()

            override x.Compute(token,dirty) =
                if initial then
                    initial <- false
                    input |> HRefSet.fold (fun deltas r -> HDeltaSet.combine deltas (r.GetOperations token)) HDeltaSet.empty
                else
                    dirty |> Seq.fold (fun deltas r -> HDeltaSet.combine deltas (r.GetOperations token)) HDeltaSet.empty

        type DifferenceReader<'a>(l : aset<'a>, r : aset<'a>) =
            inherit AbstractReader<hdeltaset<'a>>(HDeltaSet.monoid)
            
            let l = l.GetReader()
            let r = r.GetReader()
            
            override x.Kind = "SetReader"
            override x.Release() =
                l.Dispose()
                r.Dispose()

            override x.Compute(token) =
                let lops = l.GetOperations token
                let rops = r.GetOperations token

                let rops = HDeltaSet.map SetOperation.inverse rops

                HDeltaSet.combine lops rops




        type CollectReader<'a, 'b>(input : aset<'a>, f : 'a -> aset<'b>) =
            inherit AbstractDirtyReader<ISetReader<'b>, hdeltaset<'b>>(HDeltaSet.monoid, "SetReader")

            let r = input.GetReader()
            let cache = Cache(fun a -> (f a).GetReader())
            
            override x.Kind = "SetReader"
            override x.Release() =
                r.Dispose()
                cache.Clear (fun r -> r.Dispose())

            override x.Compute(token,dirty) =
                let mutable deltas = 
                    r.GetOperations token |> HDeltaSet.collect (fun d ->
                        match d with
                            | Add(1, v) ->
                                let r = cache.Invoke v
                                dirty.Remove r |> ignore
                                r.GetOperations token

                            | Rem(1, v) -> 
                                match cache.RevokeAndGetDeletedTotal v with
                                    | Some (deleted,r) -> 
                                        dirty.Remove r |> ignore
                                        if deleted then 
                                            let ops = HRefSet.computeDelta r.State HRefSet.empty
                                            r.Dispose()
                                            ops
                                        else
                                            r.GetOperations token
                                    | None -> 
                                        Fable.Import.JS.console.warn "serious hate occured"
                                        HDeltaSet.empty
                                
                            | _ -> unexpected()
                    )

                for d in dirty do
                    deltas <- HDeltaSet.combine deltas (d.GetOperations token)

                deltas

        type CollectSetReader<'a, 'b>(input : aset<'a>, f : 'a -> hrefset<'b>) =
            inherit AbstractReader<hdeltaset<'b>>(HDeltaSet.monoid)
            
            let r = input.GetReader()
            let cache = Cache f
            
            override x.Kind = "SetReader"
            override x.Release() =
                r.Dispose()
                cache.Clear ignore

            override x.Compute(token) =
                r.GetOperations token |> HDeltaSet.collect (fun d ->
                    match d with
                        | Add(1,v) -> 
                            HRefSet.computeDelta HRefSet.empty (cache.Invoke v)
                        | Rem(1,v) ->
                            HRefSet.computeDelta (cache.Revoke v) HRefSet.empty
                        | _ ->
                            unexpected()
                )


        type ModSetReader<'a>(input : IMod<hrefset<'a>>) =
            inherit AbstractReader<hdeltaset<'a>>(HDeltaSet.monoid)

            let mutable old = HRefSet.empty
            
            override x.Kind = "SetReader"
            override x.Release() =
                lock input (fun () -> input.Outputs.Remove x |> ignore)

            override x.Compute(token) =
                let n = input.GetValue token
                let deltas = HRefSet.computeDelta old n
                old <- n
                deltas

        type ModValueReader<'a>(input : IMod<'a>) =
            inherit AbstractReader<hdeltaset<'a>>(HDeltaSet.monoid)

            let mutable old = None
            
            override x.Kind = "SetReader"
            override x.Release() =
                lock input (fun () -> input.Outputs.Remove x |> ignore)
                old <- None

            override x.Compute(token) =
                let n = input.GetValue token
                let delta = 
                    match old with
                        | None -> HDeltaSet.ofList [Add n]
                        | Some o when Object.Equals(o, n) -> HDeltaSet.empty
                        | Some o -> HDeltaSet.ofList [Rem o; Add n]
                old <- Some n
                delta

        type BindReader<'a, 'b>(input : IMod<'a>, f : 'a -> aset<'b>) =
            inherit AbstractReader<hdeltaset<'b>>(HDeltaSet.monoid)
            
            let mutable inputChanged = true
            let mutable old : Option<'a * ISetReader<'b>> = None
            
            override x.Kind = "SetReader"
            override x.InputChangedObj(t : obj, i : IAdaptiveObject) =
                inputChanged <- inputChanged || Object.ReferenceEquals(i, input)

            override x.Release() =
                lock input (fun () -> input.Outputs.Remove x |> ignore)
                match old with
                    | Some (_,r) -> 
                        r.Dispose()
                        old <- None
                    | _ ->
                        ()

            override x.Compute(token) =
                let v = input.GetValue token
                match old with
                    | Some(_,ro) when inputChanged ->
                        inputChanged <- false
                        let rem = HRefSet.computeDelta ro.State HRefSet.empty
                        ro.Dispose()
                        let r = (f v).GetReader()
                        old <- Some(v, r)
                        let add = r.GetOperations token
                        HDeltaSet.combine rem add


                    | Some(vo, ro) ->
                        ro.GetOperations token

                    | None ->
                        let r = (f v).GetReader()
                        old <- Some(v, r)
                        r.GetOperations token

        type CustomReader<'a>(compute : AdaptiveToken -> hrefset<'a> -> hdeltaset<'a>) =
            inherit AbstractReader<hrefset<'a>, hdeltaset<'a>>(HRefSet.trace)
            
            override x.Kind = "SetReader"
            override x.Release() =
                ()

            override x.Compute(token) =
                compute token x.State
            
        type FlattenReader<'a>(input : aset<IMod<'a>>) =
            inherit AbstractDirtyReader<IMod<'a>, hdeltaset<'a>>(HDeltaSet.monoid, "Mod")
            
            let r = input.GetReader()

            let mutable initial = true
            let mutable cache = HMap.empty<IMod<'a>, 'a>

            member x.Invoke(token : AdaptiveToken, m : IMod<'a>) =
                let v = m.GetValue token
                cache <- HMap.add m v cache
                v

            member x.Invoke2(token : AdaptiveToken, m : IMod<'a>) =
                let o = cache.[m]
                let v = m.GetValue token
                cache <- HMap.add m v cache
                o, v

            member x.Revoke(m : IMod<'a>, dirty : ref<hset<_>>) =
                match HMap.tryRemove m cache with
                | Some (v, cc) ->
                    cache <- cc
                    lock m (fun () -> m.Outputs.Remove x |> ignore )
                    dirty := HSet.remove m !dirty
                    v
                | None -> 
                    failwith "[ASet] cannot remove unknown object"


            override x.Kind = "SetReader"
            override x.Release() =
                for m in r.State do 
                    lock m (fun () -> m.Outputs.Remove x |> ignore)
                r.Dispose()

            override x.Compute(token, dirty) =
                let dirty = ref dirty
                let mutable deltas = 
                    r.GetOperations token |> HDeltaSet.map (fun d ->
                        match d with
                            | Add(1,m) -> Add(x.Invoke(token, m))
                            | Rem(1,m) -> Rem(x.Revoke(m, dirty))
                            | _ -> unexpected()
                    )

                for d in !dirty do
                    let o, n = x.Invoke2(token, d)
                    if not <| Object.Equals(o,n) then
                        deltas <- HDeltaSet.combine deltas (HDeltaSet.ofList [Add n; Rem o])

                deltas
            
        type MapMReader<'a, 'b>(input : aset<'a>, f : 'a -> IMod<'b>) =
            inherit AbstractDirtyReader<IMod<'b>, hdeltaset<'b>>(HDeltaSet.monoid, "Mod")
            
            let r = input.GetReader()

            let f = Cache f
            let mutable initial = true
            let mutable cache : hmap<IMod<'b>, int * 'b> = HMap.empty

            let addOrUpdate (m : IMod<'b>) (value : 'b) =
                cache <- HMap.alter m (fun o ->
                    match o with
                    | Some (r,v) ->
                        Some (r+1,value)
                    | None ->
                        Some (1, value)
                ) cache

            let update (m : IMod<'b>) (value : 'b) =
                let old = ref value
                cache <- HMap.alter m (fun o ->
                    match o with
                    | Some (r,v) ->
                        old := v
                        Some (r,value)
                    | None ->
                        None
                ) cache
                !old
                

            member x.Invoke(token : AdaptiveToken, v : 'a) =
                let m = f.Invoke v
                let v = m.GetValue token
                addOrUpdate m v
                v

            member x.Invoke2(token : AdaptiveToken, m : IMod<'b>) =
                let v = m.GetValue token
                let o = update m v
                //let r = cache.[m]
                //let (rc, o) = !r
                //r := (rc, v)
                o, v

            member x.Revoke(v : 'a, dirty : ref<hset<_>>) =
                let m = f.Revoke v
                
                let old = ref None
                cache <- HMap.alter m (fun o ->
                    match o with
                    | Some (1, o) ->
                        dirty := HSet.remove m !dirty
                        m.Outputs.Remove x |> ignore
                        old := Some o
                        None
                    | Some (r,o) ->
                        old := Some o
                        Some (r-1,o)
                    | None ->
                        None
                ) cache


                match !old with
                | Some v -> v
                | _ -> failwith "[ASet] cannot remove unknown object"

                //match cache.TryGetValue m with
                //    | (true, r) -> 
                //        let (cnt, v) = !r
                //        if cnt = 1 then
                //            cache.Remove m |> ignore
                //            dirty.Remove m |> ignore
                //            lock m (fun () -> m.Outputs.Remove x |> ignore )
                //            v
                //        else
                //            r := (cnt - 1, v)
                //            v
                //    | _ -> 
                //        failwith "[ASet] cannot remove unknown object"

                
            override x.Kind = "SetReader"
            override x.Release() =
                f.Clear ignore
                for (m,_) in cache do 
                    lock m (fun () -> m.Outputs.Remove x |> ignore)
                cache <- HMap.empty

                r.Dispose()

            override x.Compute(token, dirty) =
                let dirty = ref dirty
                let mutable deltas = 
                    r.GetOperations token |> HDeltaSet.map (fun d ->
                        match d with
                            | Add(1,m) -> Add(x.Invoke(token,m))
                            | Rem(1,m) -> Rem(x.Revoke(m, dirty))
                            | _ -> unexpected()
                    )

                for d in !dirty do
                    let o, n = x.Invoke2(token, d)
                    if not <| Object.Equals(o,n) then
                        deltas <- HDeltaSet.combine deltas (HDeltaSet.ofList [Add n; Rem o])

                deltas

        type ChooseMReader<'a, 'b>(input : aset<'a>, f : 'a -> IMod<Option<'b>>) =
            inherit AbstractDirtyReader<IMod<Option<'b>>, hdeltaset<'b>>(HDeltaSet.monoid, "Mod")
            
            let r = input.GetReader()

            let f = Cache f
            let mutable initial = true
            let mutable cache : hmap<IMod<Option<'b>>, int * Option<'b>> = HMap.empty
            
            let addOrUpdate (m : IMod<Option<'b>>) (value : Option<'b>) =
                cache <- HMap.alter m (fun o ->
                    match o with
                    | Some (r,v) ->
                        Some (r+1,value)
                    | None ->
                        Some (1, value)
                ) cache
            
            let update (m : IMod<Option<'b>>) (value : Option<'b>) =
                let old = ref None
                cache <- HMap.alter m (fun o ->
                    match o with
                    | Some (r,v) ->
                        old := v
                        Some (r,value)
                    | None ->
                        None
                ) cache
                !old

            member x.Invoke(token : AdaptiveToken, v : 'a) =
                let m = f.Invoke v
                let v = m.GetValue token
                addOrUpdate m v
                //let r = cache.GetOrCreate(m, fun _ -> ref (0, None))
                //r := (fst !r + 1, v)
                v

            member x.Invoke2(token : AdaptiveToken, m : IMod<Option<'b>>) =
                let v = m.GetValue token
                let o = update m v
                o, v
                //match cache.TryGetValue m with
                //    | (true, r) ->
                //        let (rc, o) = !r
                //        let v = m.GetValue token
                //        r := (rc, v)
                //        o, v
                //    | _ ->
                //      None, None  

            member x.Revoke(v : 'a, dirty : ref<hset<IMod<Option<'b>>>>) =
                let m = f.Revoke v

                
                let old = ref None
                cache <- HMap.alter m (fun o ->
                    match o with
                    | Some (1, o) ->
                        dirty := HSet.remove m !dirty
                        m.Outputs.Remove x |> ignore
                        old := o
                        None
                    | Some (r,o) ->
                        old := o
                        Some (r-1,o)
                    | None ->
                        None
                ) cache
                !old

                //match cache.TryGetValue m with
                //    | (true, r) -> 
                //        let (rc, v) = !r
                //        if rc = 1 then
                //            cache.Remove m |> ignore
                //            lock m (fun () -> m.Outputs.Remove x |> ignore )
                //        else
                //            r := (rc - 1, v)
                //        v
                //    | _ -> 
                //        failwith "[ASet] cannot remove unknown object"

            override x.Kind = "SetReader"

            override x.Release() =
                f.Clear ignore

                for (m,_) in cache do 
                    lock m (fun () -> m.Outputs.Remove x |> ignore)
                cache <- HMap.empty

                r.Dispose()

            override x.Compute(token, dirty) =
                let dirty = ref dirty
                let mutable deltas = 
                    r.GetOperations token |> HDeltaSet.choose (fun d ->
                        match d with
                            | Add(1,m) -> 
                                match x.Invoke(token,m) with
                                    | Some v -> Some (Add v)
                                    | None -> None

                            | Rem(1,m) ->
                                match x.Revoke(m, dirty) with
                                    | Some v -> Some (Rem v)
                                    | None -> None

                            | _ -> 
                                unexpected()
                    )

                for d in !dirty do
                    let change = 
                        match x.Invoke2(token, d) with
                            | None, None -> 
                                HDeltaSet.empty

                            | None, Some v ->
                                HDeltaSet.single (Add v)

                            | Some o, None ->
                                HDeltaSet.single (Rem o)

                            | Some o, Some n ->
                                if Object.Equals(o, n) then
                                    HDeltaSet.empty
                                else
                                    HDeltaSet.ofList [Rem o; Add n]

                    deltas <- HDeltaSet.combine deltas change

                deltas

    // =====================================================================================
    // CREATORS (of*)
    // =====================================================================================

    /// the empty aset
    let empty<'a> = EmptySet<'a>.Instance

    /// creates a new aset containing only the given element
    let single (v : 'a) =
        ConstantSet(HRefSet.single v) :> aset<_>

    /// creates a new aset using the given set content
    let ofSet (set : hrefset<'a>) =
        ConstantSet(set) :> aset<_>

    /// create a new aset using all distinct entries from the sequence
    let ofSeq (seq : seq<'a>) =
        seq |> HRefSet.ofSeq |> ofSet
        
    /// create a new aset using all distinct entries from the list
    let ofList (list : list<'a>) =
        list |> HRefSet.ofList |> ofSet
        
    /// create a new aset using all distinct entries from the array
    let ofArray (arr : 'a[]) =
        arr |> HRefSet.ofArray |> ofSet

    /// creates set which will always contain the elements given by the mod-cell
    let ofMod (m : IMod<hrefset<'a>>) =
        if m.IsConstant then
            constant <| lazy ( Mod.force m )
        else
            aset <| fun () -> new ModSetReader<'a>(m)
            
    /// creates a singleton set which will always contain the latest value of the given mod-cell
    let ofModSingle (m : IMod<'a>) =
        if m.IsConstant then
            constant <| lazy ( m |> Mod.force |> HRefSet.single )
        else
            aset <| fun () -> new ModValueReader<'a>(m)


    // =====================================================================================
    // VIEWS (to*)
    // =====================================================================================

    /// creates a set from the current state of the aset
    let toSet (set : aset<'a>) =
        set.Content |> Mod.force
        
    /// creates a seq from the current state of the aset
    let toSeq (set : aset<'a>) =
        set.Content |> Mod.force :> seq<_>
        
    /// creates a list from the current state of the aset
    let toList (set : aset<'a>) =
        set.Content |> Mod.force |> HRefSet.toList
        
    /// creates an array from the current state of the aset
    let toArray (set : aset<'a>) =
        set.Content |> Mod.force |> HRefSet.toArray

    /// creates a mod-cell containing the set's content as set
    let toMod (s : aset<'a>) =
        s.Content


    // =====================================================================================
    // OPERATIONS
    // =====================================================================================

    let union (l : aset<'a>) (r : aset<'a>) =
        if l.IsConstant && r.IsConstant then
            constant <| lazy ( HRefSet.union (Mod.force l.Content) (Mod.force r.Content) )
        else
            aset <| fun () -> new UnionFixedReader<'a>(HRefSet.ofList [l; r])

    let difference (l : aset<'a>) (r : aset<'a>) =
        if l.IsConstant && r.IsConstant then
            constant <| lazy ( HRefSet.difference (Mod.force l.Content) (Mod.force r.Content) )
        else
            aset <| fun () -> new DifferenceReader<'a>(l, r)

    let unionMany' (sets : seq<aset<'a>>) =
        let sets = HRefSet.ofSeq sets
        if sets |> Seq.forall (fun s -> s.IsConstant) then
            constant <| lazy ( sets |> HRefSet.collect (fun s -> s.Content |> Mod.force) )
        else
            aset <| fun () -> new UnionFixedReader<'a>(sets)

    let unionMany (sets : aset<aset<'a>>) =
        if sets.IsConstant then
            sets.Content |> Mod.force |> unionMany'
        else
            aset <| fun () -> new UnionReader<'a>(sets)

    // =====================================================================================
    // PROJECTIONS
    // =====================================================================================

    /// creates a new aset whose elements are the result of applying the given function to each of the elements of the given set
    let map (mapping : 'a -> 'b) (set : aset<'a>) =
        if set.IsConstant then
            constant <| lazy ( set.Content |> Mod.force |> HRefSet.map mapping )
        else
            aset <| fun () -> new MapReader<'a, 'b>(set, mapping)
 

    let mapUse<'a, 'b when 'b :> IDisposable> (mapping : 'a -> 'b) (set : aset<'a>) : aset<'b> =
        aset <| fun () -> new MapUseReader<'a, 'b>(set, mapping)

        
    /// applies the given function to each element of the given aset. returns an aset comprised of the results x for each element
    /// where the function returns Some(x)
    let choose (chooser : 'a -> Option<'b>) (set : aset<'a>) =
        if set.IsConstant then
            constant <| lazy ( set.Content |> Mod.force |> HRefSet.choose chooser )
        else
            aset <| fun () -> new ChooseReader<'a, 'b>(set, chooser)

    /// creates a new aset containing only the elements of the given one for which the given predicate returns true
    let filter (predicate : 'a -> bool) (set : aset<'a>) =
        if set.IsConstant then
            constant <| lazy ( set.Content |> Mod.force |> HRefSet.filter predicate )
        else
            aset <| fun () -> new FilterReader<'a>(set, predicate)

    /// applies the given function to each element of the given aset. unions all the results and returns the combined aset
    let collect (mapping : 'a -> aset<'b>) (set : aset<'a>) =
        if set.IsConstant then
            set.Content |> Mod.force |> HRefSet.map mapping |> unionMany'
        else
            aset <| fun () -> new CollectReader<'a, 'b>(set, mapping)
        
    /// applies the given function to each element of the given aset. unions all the results and returns the combined aset
    let collect' (mapping : 'a -> #seq<'b>) (set : aset<'a>) =
        let mapping = mapping >> HRefSet.ofSeq
        if set.IsConstant then
            constant <| lazy ( set.Content |> Mod.force |> HRefSet.collect mapping )
        else
            aset <| fun () -> new CollectSetReader<'a, 'b>(set, mapping)
    
    
    // =====================================================================================
    // MOD INTEROP
    // =====================================================================================

    let flattenM (set : aset<IMod<'a>>) =
        if set.IsConstant && set.Content |> Mod.force |> Seq.forall (fun m -> m.IsConstant) then
            constant <| lazy (set.Content |> Mod.force |> HRefSet.map Mod.force)
        else
            aset <| fun () -> new FlattenReader<'a>(set)

    let mapM (mapping : 'a -> IMod<'b>) (set : aset<'a>) =
        if set.IsConstant then
            set.Content |> Mod.force |> HRefSet.map mapping |> ofSet |> flattenM
        else
            aset <| fun () -> new MapMReader<'a, 'b>(set, mapping)

    let chooseM (mapping : 'a -> IMod<Option<'b>>) (set : aset<'a>) =
        aset <| fun () -> new ChooseMReader<'a, 'b>(set, mapping)

    let filterM (predicate : 'a -> IMod<bool>) (set : aset<'a>) =
        set |> chooseM (fun a ->
            a |> predicate |> Mod.map (fun v -> if v then Some a else None)
        )

    let bind (mapping : 'a -> aset<'b>) (m : IMod<'a>) =
        if m.IsConstant then
            mapping (Mod.force m)
        else
            aset <| fun () -> new BindReader<'a, 'b>(m, mapping)

    let bind2 (mapping : 'a -> 'b -> aset<'c>) (a : IMod<'a>) (b : IMod<'b>) =
        match a.IsConstant, b.IsConstant with
            | true,  true  -> 
                mapping (Mod.force a) (Mod.force b)
            | true,  false ->
                let mapping = mapping (Mod.force a)
                b |> bind mapping

            | false, true  ->
                let mapping = 
                    let b = Mod.force b
                    fun a -> mapping a b

                a |> bind mapping

            | false, false ->
                let tup = Mod.map2 (fun a b -> (a,b)) a b
                tup |> bind (fun (a,b) -> mapping a b)

    
    // =====================================================================================
    // FOLDS
    // =====================================================================================

    let foldHalfGroup (add : 's -> 'a -> 's) (trySub : 's -> 'a -> Option<'s>) (zero : 's) (s : aset<'a>) =
        let r = s.GetReader()
        let mutable res = zero

        let rec traverse (d : list<SetOperation<'a>>) =
            match d with
                | [] -> true
                | d :: rest ->
                    match d with
                        | Add(1,v) -> 
                            res <- add res v
                            traverse rest

                        | Rem(1,v) ->
                            match trySub res v with
                                | Some s ->
                                    res <- s
                                    traverse rest
                                | None ->
                                    false
                        | _ -> 
                            failwithf "[ASet] unexpected delta: %A" d
                                    

        Mod.custom (fun self ->
            let ops = r.GetOperations self

            let worked = traverse (HDeltaSet.toList ops)

            if not worked then
                res <- r.State |> HRefSet.fold add zero
                
            res
        )

    let fold (f : 's -> 'a -> 's) (seed : 's) (s : aset<'a>) =
        foldHalfGroup f (fun _ _ -> None) seed s

    let foldGroup (add : 's -> 'a -> 's) (sub : 's -> 'a -> 's) (zero : 's) (s : aset<'a>) =
        foldHalfGroup add (fun a b -> Some (sub a b)) zero s
       
    let contains (value : 'a) (set : aset<'a>) =
        let add (missing : hset<'a>) (v : 'a) =
            HSet.remove v missing

        let rem (missing : hset<'a>) (v : 'a) =
            if Unchecked.equals v value then HSet.add v missing
            else missing

        set 
            |> foldGroup add rem (HSet.ofList [value]) 
            |> Mod.map HSet.isEmpty
       
    let containsAll (seq : seq<'a>) (set : aset<'a>) =
        let all = HSet.ofSeq seq

        let add (missing : hset<'a>) (value : 'a) =
            HSet.remove value missing

        let rem (missing : hset<'a>) (value : 'a) =
            if HSet.contains value all then HSet.add value missing
            else missing

        foldGroup add rem all set |> Mod.map HSet.isEmpty
      
    let containsAny (seq : seq<'a>) (set : aset<'a>) =
        let all = HSet.ofSeq seq

        let add (contained : hset<'a>) (value : 'a) =
            if HSet.contains value all then HSet.add value contained
            else contained

        let rem (contained : hset<'a>) (value : 'a) =
            HSet.remove value contained

        foldGroup add rem HSet.empty set |> Mod.map (not << HSet.isEmpty)

    let count (set : aset<'a>) =
        set.Content |> Mod.map HRefSet.count

    /// Adaptively calculates the sum of all elements in the set
    let inline sum (s : aset<'a>) = foldGroup (+) (-) LanguagePrimitives.GenericZero s

    /// Adaptively calculates the product of all elements in the set
    let inline product (s : aset<'a>) = foldGroup (*) (/) LanguagePrimitives.GenericOne s



    /// creates a new aset using the given reader-creator
    let create (f : unit -> #IOpReader<hdeltaset<'a>>) =
        aset f

    let custom (f : AdaptiveToken -> hrefset<'a> -> hdeltaset<'a>) =
        aset <| fun () -> new CustomReader<'a>(f)
