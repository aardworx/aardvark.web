namespace Aardvark.Base.Incremental

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Collections.Generic
open Aardvark.Base
open Fable.Core

type IUpdatable<'a> =
    abstract member Update : 'a -> unit

type ResetMod<'a>(value : 'a, [<Inject>] ?r : ITypeResolver<'a>) =
    inherit AdaptiveObject()

    let mutable value = value
    let mutable cache = value
    
    override x.Kind = "Mod"

    member x.UpdateNoEquality(v : 'a) =
        if not <| Object.ReferenceEquals(v, value) then
            value <- v
            x.MarkOutdated()

    member x.Update(v : 'a) =
        if not <| Object.ReferenceEquals(v, value) then
            if Unchecked.equals v value then
                value <- v
            else
                value <- v
                x.MarkOutdated()
        

    member x.GetValue(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                cache <- value
            
            cache
        )

    override x.ToString() =
       sprintf "{ value = %A }" value

    interface IUpdatable<'a> with
        member x.Update v = x.Update v

    interface IMod with
        member x.ValueType = r |> Option.map (fun r -> r.ResolveType())
        member x.IsConstant = false
        member x.GetValueObj(caller) = x.GetValue(caller) :> obj

    interface IMod<'a> with
        member x.GetValue(caller) = x.GetValue(caller)

type ResetSet<'a>(initial : hset<'a>) =
    let history = History HRefSet.traceNoRefCount
    do initial |> Seq.map Add |> HDeltaSet.ofSeq |> history.Perform |> ignore

    let mutable current = initial

    member x.Update(values : hset<'a>) =
        if not (Object.ReferenceEquals(values, current)) then
            let ops = HSet.computeDelta current values
            current <- values
            history.Perform ops |> ignore

    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'a>> with
        member x.Update v = x.Update v

    interface aset<'a> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader("SetReader")
        member x.Content = history :> IMod<_>

type ResetMapSet<'k, 'v>(getId : 'k -> obj, initial : hset<'k>, create : 'k -> 'v, update : 'v * 'k -> unit) =
    let history = History HRefSet.traceNoRefCount
    let cache = Dict<obj, ref<'k> * 'v>(Unchecked.hash, Unchecked.equals)

    let mutable current = HSet.empty

    let update (keys : hset<'k>) =
        let keyDeltas = HSet.computeDelta current keys

        
        let valueDeltas =
            keyDeltas |> HDeltaSet.choose (fun d ->
                match d with
                    | Add(_,k) ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(getId k, fun _ ->
                                isNew <- true
                                ref k, create k
                            )

                        if isNew then
                            Some (Add v)
                        else
                            r := k
                            None

                    | Rem(_,k) ->
                        match cache.TryRemove k with
                            | Some (_,v) ->
                                Some (Rem v)
                            | _ ->
                                None
            )



        current <- keys
        history.Perform valueDeltas |> ignore
        for (_,(r, v)) in cache do
            update(v, !r)
        
    do update initial

    member x.Update(keys : hset<'k>) =
        if not (Object.ReferenceEquals(keys, current)) then
            update keys
    
    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'k>> with
        member x.Update v = x.Update v

    interface aset<'v> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader("SetReader")
        member x.Content = history :> IMod<_>

    new(initial : hset<'k>, create : 'k -> 'v, update : 'v * 'k -> unit) = ResetMapSet(unbox, initial, create, update)

type ResetList<'a>(initial : plist<'a>) =
    let history = History PList.trace
    do 
        let delta = plist.ComputeDeltas(PList.empty, initial)
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : plist<'a>) =
        if not (Object.ReferenceEquals(values, current)) then
            let delta = plist.ComputeDeltas(current, values)
            history.Perform delta |> ignore
            current <- values

    interface alist<'a> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("ListReader")

type ResetMapList<'k, 'v>(initial : plist<'k>, create : Index -> 'k -> 'v, update : 'v * 'k -> unit) =
    
    let history = History PList.trace
    let cache = Dict<Index, ref<'k> * 'v>(Unchecked.hash, Unchecked.equals)

    let mutable current = PList.empty

    let update (keys : plist<'k>) =
        let keyDeltas = plist.ComputeDeltas(current, keys)

        let valueDeltas =
            keyDeltas |> PDeltaList.choose (fun i op ->
                match op with
                    | Set k ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(i, fun _ ->
                                isNew <- true
                                ref k, create i k
                            )

                        if isNew then
                            Some (Set v)
                        else
                            r := k
                            None

                    | Remove ->
                        match cache.TryRemove i with
                            | Some (_,v) ->
                                Some Remove
                            | _ ->
                                None
            )

        current <- keys
        history.Perform valueDeltas |> ignore
        for (_, (r, v)) in cache do
            update(v, !r)       

    do update initial

    member x.Update(keys : plist<'k>) =
        if not (Object.ReferenceEquals(keys, current)) then
            update keys

    override x.ToString() =
        current.ToString()

    interface alist<'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("ListReader")

type ResetMapOption<'a, 'b>(initial : Option<'a>, create : 'a -> 'b, update : 'b * 'a -> unit) =
    inherit Mod.AbstractMod<Option<'b>>(None)
    let b = ResetMod<Option<'b>>(initial |> Option.map create)

    member x.Update(v : Option<'a>) =
        match b.GetValue(AdaptiveToken.Top), v with
            | Some _, None -> b.Update(None)
            | None , None -> ()
            | None, Some v -> b.Update(create v |> Some)
            | Some o, Some n ->
                if not (System.Object.ReferenceEquals(o,n)) then
                    update (o,n)
            
    interface IUpdatable<Option<'a>> with
        member x.Update v = x.Update v

    override x.Compute(token) =
        b.GetValue(token)

type ResetMap<'k, 'v>(initial : hmap<'k, 'v>) =
    let history = History HMap.trace
    do 
        let delta = HMap.computeDelta HMap.empty initial
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : hmap<'k, 'v>) =
        if not (Object.ReferenceEquals(values, current)) then
            let delta = HMap.computeDelta current values
            history.Perform delta |> ignore
            current <- values

    interface amap<'k, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("MapReader")

type ResetMapMap<'a, 'b, 'v>(initial : hmap<'a, 'b>, create : 'a -> 'b -> 'v, update : 'v * 'b -> unit) =

    let history = History HMap.trace
    let cache = Dict<'a, ref<'b> * 'v>(Unchecked.hash, Unchecked.equals)

    let mutable current = HMap.empty

    let update (keys : hmap<'a, 'b>) =
        let keyDeltas = HMap.computeDelta current keys

        let valueDeltas =
            keyDeltas |> HMap.choose (fun i op ->
                match op with
                    | Set k ->
                        let mutable isNew = false
                        let r, v = 
                            cache.GetOrCreate(i, fun _ ->
                                isNew <- true
                                ref k, create i k
                            )

                        if isNew then
                            Some (Set v)
                        else
                            r := k
                            None

                    | Remove ->
                        match cache.TryRemove i with
                            | Some (_,v) ->
                                Some Remove
                            | _ ->
                                None
            )

        current <- keys
        history.Perform valueDeltas |> ignore
        for (_,(r, v)) in cache do
            update(v, !r)       

    do update initial

    member x.Update(keys : hmap<'a, 'b>) =
        if not (Object.ReferenceEquals(Object, current)) then
            update keys

    override x.ToString() =
        current.ToString()

    interface amap<'a, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("MapReader")



[<AbstractClass; Sealed>]
type ResetMod private() =
    static member Create(v : 'a) = ResetMod<'a>(v)
    static member Update(m : ResetMod<'a>, v : 'a) = m.Update v


type MSet<'a>(initial : hset<'a>) =
    let history = History HRefSet.traceNoRefCount
    do initial |> Seq.map Add |> HDeltaSet.ofSeq |> history.Perform |> ignore

    let mutable current = initial

    member x.Update(values : hset<'a>) =
        if not (Object.ReferenceEquals(values, current)) then
            let ops = HSet.computeDelta current values
            current <- values
            if not (HDeltaSet.isEmpty ops) then
                history.Perform ops |> ignore

    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'a>> with
        member x.Update v = x.Update v

    interface aset<'a> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader("SetReader")
        member x.Content = history :> IMod<_>

type MSet<'k, 'm, 'v>(getId : 'k -> obj, initial : hset<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
    let history = History HRefSet.traceNoRefCount
    let cache = Dict<obj, ref<'k> * 'm>(Unchecked.hash, Unchecked.equals)

    let mutable current = HSet.empty

    let update (keys : hset<'k>) =
        if not (Object.ReferenceEquals(keys, current)) then
            let keyDeltas = HSet.computeDelta current keys
            current <- keys

            if not (HDeltaSet.isEmpty keyDeltas) then
                let valueDeltas =
                    keyDeltas |> HDeltaSet.choose (fun d ->
                        match d with
                            | Add(_,k) ->
                                let mutable isNew = false
                                let r, v = 
                                    cache.GetOrCreate(getId k, fun _ ->
                                        isNew <- true
                                        ref k, create k
                                    )

                                if isNew then
                                    Some (Add (view v))
                                else
                                    r := k
                                    None

                            | Rem(_,k) ->
                                match cache.TryRemove k with
                                    | Some (_,v) ->
                                        Some (Rem (view v))
                                    | _ ->
                                        None
                    )


                    
                history.Perform valueDeltas |> ignore
                for (_, (r, v)) in cache do
                    update(v, !r)
        
    do update initial

    member x.Update(keys : hset<'k>) =
        update keys

    override x.ToString() =
        current.ToString()
        
    interface IUpdatable<hset<'k>> with
        member x.Update v = x.Update v

    interface aset<'v> with
        member x.IsConstant = false
        member x.GetReader() = history.NewReader("SetReader")
        member x.Content = history :> IMod<_>

[<AbstractClass; Sealed>]
type MSet private() =
    static member Create(getId : 'k -> obj, initial : hset<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
        MSet<'k, 'm, 'v>(getId, initial, create, update, view)

    static member Update(m : MSet<'k, 'm, 'v>, v : hset<'k>) =
        m.Update(v)

    static member Create(initial : hset<'k>) =
        MSet<'k>(initial)

    static member Update(m : MSet<'k>, v : hset<'k>) =
        m.Update(v)



type MList<'a>(initial : plist<'a>) =
    let history = History PList.trace
    do 
        let delta = plist.ComputeDeltas(PList.empty, initial)
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : plist<'a>) =
        if not (Object.ReferenceEquals(values, current)) then
            let delta = plist.ComputeDeltas(current, values)
            current <- values
            if not (PDeltaList.isEmpty delta) then
                history.Perform delta |> ignore

    interface alist<'a> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("ListReader")

type MList<'k, 'm, 'v>(initial : plist<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
    
    let history = History PList.trace
    let cache = Dict<Index, ref<'k> * 'm>(Unchecked.hash, Unchecked.equals)

    let mutable current = PList.empty

    let update (keys : plist<'k>) =
        if not (Object.ReferenceEquals(keys, current)) then
            let keyDeltas = plist.ComputeDeltas(current, keys)
            current <- keys
        
            if not (PDeltaList.isEmpty keyDeltas) then
                let valueDeltas =
                    keyDeltas |> PDeltaList.choose (fun i op ->
                        match op with
                            | Set k ->
                                let mutable isNew = false
                                let r, v = 
                                    cache.GetOrCreate(i, fun _ ->
                                        isNew <- true
                                        ref k, create k
                                    )

                                if isNew then
                                    Some (Set (view v))
                                else
                                    r := k
                                    None

                            | Remove ->
                                match cache.TryRemove i with
                                    | Some (_,v) ->
                                        Some Remove
                                    | _ ->
                                        None
                    )

                history.Perform valueDeltas |> ignore
                for (_,(r, v)) in cache do
                    update(v, !r)       

    do update initial

    member x.Update(keys : plist<'k>) =
        update keys

    override x.ToString() =
        current.ToString()

    interface alist<'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("ListReader")

[<AbstractClass; Sealed>]
type MList private() =
    static member Create(initial : plist<'k>, create : 'k -> 'm, update : 'm * 'k -> unit, view : 'm -> 'v) =
        MList<'k, 'm, 'v>(initial, create, update, view)

    static member Update(m : MList<'k, 'm, 'v>, v : plist<'k>) =
        m.Update(v)

    static member Create(initial : plist<'k>) =
        MList<'k>(initial)

    static member Update(m : MList<'k>, v : plist<'k>) =
        m.Update(v)


type MMap<'k, 'v>(initial : hmap<'k, 'v>) =
    let history = History HMap.trace
    do 
        let delta = HMap.computeDelta HMap.empty initial
        history.Perform delta |> ignore

    let mutable current = initial

    member x.Update(values : hmap<'k, 'v>) =
        if not (Object.ReferenceEquals(values, current)) then
            let delta = HMap.computeDelta current values
            current <- values

            if not (HMap.isEmpty delta) then
                history.Perform delta |> ignore

    interface amap<'k, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("MapReader")

type MMap<'a, 'b, 'm, 'v>(initial : hmap<'a, 'b>, create : 'b -> 'm, update : 'm * 'b -> unit, view : 'm -> 'v) =

    let history = History HMap.trace
    let cache = Dict<'a, ref<'b> * 'm>(Unchecked.hash, Unchecked.equals)

    let mutable current = HMap.empty

    let update (keys : hmap<'a, 'b>) =
        if not (System.Object.ReferenceEquals(current, keys)) then
            let keyDeltas = HMap.computeDelta current keys
            current <- keys

            if not (HMap.isEmpty keyDeltas) then

                let valueDeltas =
                    keyDeltas |> HMap.choose (fun i op ->
                        match op with
                            | Set k ->
                                let mutable isNew = false
                                let r, v = 
                                    cache.GetOrCreate(i, fun _ ->
                                        isNew <- true
                                        ref k, create k
                                    )

                                if isNew then
                                    Some (Set (view v))
                                else
                                    r := k
                                    None

                            | Remove ->
                                match cache.TryRemove i with
                                    | Some (_,v) ->
                                        Some Remove
                                    | _ ->
                                        None
                    )

                history.Perform valueDeltas |> ignore
                for (_,(r, v)) in cache do
                    update(v, !r)       

    do update initial

    member x.Update(keys : hmap<'a, 'b>) =
        update keys

    override x.ToString() =
        current.ToString()

    interface amap<'a, 'v> with
        member x.IsConstant = false
        member x.Content = history :> IMod<_>
        member x.GetReader() = history.NewReader("MapReader")

[<AbstractClass; Sealed>]
type MMap private() =
    static member Create(initial : hmap<'a, 'b>, create : 'b -> 'm, update : 'm * 'b -> unit, view : 'm -> 'v) =
        MMap<'a, 'b, 'm, 'v>(initial, create, update, view)

    static member Update(m : MMap<'a, 'b, 'm, 'v>, v : hmap<'a, 'b>) =
        m.Update(v)

    static member Create(initial : hmap<'a, 'b>) =
        MMap<'a, 'b>(initial)

    static member Update(m : MMap<'a, 'b>, v : hmap<'a, 'b>) =
        m.Update(v)



type MOption<'a>(initial : Option<'a>) =
    inherit ResetMod<Option<'a>>(initial)

type MOption<'a, 'b, 'v>(initial : Option<'a>, create : 'a -> 'b, update : 'b * 'a -> unit, view : 'b -> 'v) =
    inherit Mod.AbstractMod<Option<'v>>(None)
    let b = ResetMod<Option<'b>>(initial |> Option.map create)

    member x.Update(v : Option<'a>) =
        match b.GetValue(AdaptiveToken.Top), v with
            | Some _, None -> b.Update(None)
            | None , None -> ()
            | None, Some v -> b.Update(create v |> Some)
            | Some o, Some n ->
                if not (System.Object.ReferenceEquals(o,n)) then
                    update (o,n)
            
    interface IUpdatable<Option<'a>> with
        member x.Update v = x.Update v

    override x.Compute(token) =
        b.GetValue(token) |> Option.map view

[<AbstractClass; Sealed>]
type MOption private() =
    static member Create(initial : Option<'a>, create : 'a -> 'm, update : 'm * 'a -> unit, view : 'm -> 'v) =
        MOption<'a, 'm, 'v>(initial, create, update, view)

    static member Update(m : MOption<'a, 'm, 'v>, v : Option<'a>) =
        m.Update(v)

    static member Create(initial : Option<'a>) =
        MOption<'a>(initial)

    static member Update(m : MOption<'a>, v : Option<'a>) =
        m.Update(v)


type EqModRef<'a>(value : 'a, [<Inject>] ?r : ITypeResolver<'a>) =
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
            if not <| Object.ReferenceEquals(v, value) then
                value <- v
                x.MarkOutdated()


    member x.GetValue(token : AdaptiveToken) =
        x.EvaluateAlways token (fun token ->
            if x.OutOfDate then
                cache <- value
            cache
        )

    override x.ToString() =
       sprintf "{ value = %A }" value

    interface IMod with
        member x.ValueType = r |> Option.map (fun r -> r.ResolveType())
        member x.IsConstant = false
        member x.GetValueObj(caller) = x.GetValue(caller) :> obj

    interface IMod<'a> with
        member x.GetValue(caller) = x.GetValue(caller)

    interface IModRef<'a> with
        member x.Value 
            with get () = x.Value
            and set v = x.Value <- v
        member x.UnsafeCache
            with get() = x.UnsafeCache
            and set v = x.UnsafeCache <- v
