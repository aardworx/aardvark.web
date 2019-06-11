namespace Aardvark.UI

open Aardvark.Base
open Aardvark.Base.Incremental

type EventCallback<'msg> =
    {
        useCapture  : bool
        callback    : Aardvark.Import.Browser.Event -> seq<'msg>
    }

type EventCallbacks<'msg>(store : MapExt<bool, list<Aardvark.Import.Browser.Event -> seq<'msg>>>) =
    static let empty = EventCallbacks<'msg>(MapExt.empty)

    static member Empty = empty
    static member Zero = empty

    member private x.Store = store

    member x.IsEmpty = store.IsEmpty

    member x.HasCapture = MapExt.containsKey true store
    member x.HasNonCapture = MapExt.containsKey false store

    member x.Add(callback : EventCallback<'msg>) =
        store |> MapExt.alter callback.useCapture (fun o ->
            match o with
            | Some o -> Some (callback.callback :: o)
            | None -> Some [callback.callback]
        ) |> EventCallbacks

    member x.AsSeq = store |> MapExt.toSeq |> Seq.collect (fun (c, cbs) -> cbs |> Seq.map (fun cb -> { useCapture = c; callback = cb }))
    member x.AsList = store |> MapExt.toList |> List.collect (fun (c, cbs) -> cbs |> List.map (fun cb -> { useCapture = c; callback = cb }))
    
    member x.Map (mapping : 'msg -> 'a) =
        store |> MapExt.map (fun k v ->
            v |> List.map (fun cb -> cb >> Seq.map mapping)
        ) |> EventCallbacks<'a>

    member x.Choose (mapping : 'msg -> Option<'a>) =
        store |> MapExt.map (fun k v ->
            v |> List.map (fun cb -> cb >> Seq.choose mapping)
        ) |> EventCallbacks<'a>

    member x.Collect (mapping : 'msg -> seq<'a>) =
        store |> MapExt.map (fun k v ->
            v |> List.map (fun cb -> cb >> Seq.collect mapping)
        ) |> EventCallbacks<'a>

    static member (+) (l : EventCallbacks<'msg>, r : EventCallbacks<'msg>) =
        MapExt.unionWith List.append l.Store r.Store |> EventCallbacks

    static member Create (callback : Aardvark.Import.Browser.Event -> 'msg) = EventCallbacks(MapExt.ofList [false, [callback >> Seq.singleton]])
    static member Create (callback : Aardvark.Import.Browser.Event -> seq<'msg>) = EventCallbacks(MapExt.ofList [false, [callback]])
    static member Create (callback : Aardvark.Import.Browser.Event -> Option<'msg>) = EventCallbacks(MapExt.ofList [false, [fun v -> match callback v with | Some m -> Seq.singleton m | _ -> Seq.empty ]])

module EventCallbacks =
    let inline isEmpty (callbacks : EventCallbacks<'msg>) = callbacks.IsEmpty
    let inline toSeq (callbacks : EventCallbacks<'msg>) = callbacks.AsSeq
    let inline toList (callbacks : EventCallbacks<'msg>) = callbacks.AsList
    let inline toArray (callbacks : EventCallbacks<'msg>) = callbacks.AsSeq |> Seq.toArray
    
    let inline empty<'msg> = EventCallbacks<'msg>.Empty
    let inline singleton (callback : EventCallback<'msg>) = EventCallbacks<'msg>(MapExt.singleton callback.useCapture [callback.callback])
    let inline add (callback : EventCallback<'msg>) (m : EventCallbacks<'msg>) = m.Add callback
    let ofSeq (callbacks : seq<EventCallback<'msg>>) = callbacks |> Seq.fold (fun s e -> add e s) empty
    let ofList (callbacks : list<EventCallback<'msg>>) = callbacks |> List.fold (fun s e -> add e s) empty
    let ofArray (callbacks : array<EventCallback<'msg>>) = callbacks |> Array.fold (fun s e -> add e s) empty

type AttributeValue<'msg> =
    | String of string
    | Event of callbacks : EventCallbacks<'msg>

type AttributeMap<'msg>(values : amap<string, AttributeValue<'msg>>) =  

    static let union (key : string) (l : AttributeValue<'msg>) (r : AttributeValue<'msg>) =
        match key with
        | "class" ->
            match l, r with
            | String l, String r -> String (l + " " + r)
            | _, r -> r
        | "style" ->
            match l, r with
            | String l, String r -> String (l + "; " + r)
            | _, r -> r
        | _ ->
            match l, r with
            | Event l, Event r -> Event (l + r)
            | _, r -> r

    member x.Values = values

    member x.Add(key : string, value : AttributeValue<'msg>) = 
        let v = AMap.ofList [key, value]
        AttributeMap(AMap.unionWith union values v)

    member x.Add(key : string, value : string) = 
        x.Add(key, String value)
        
    member x.Add(key : string, value : EventCallback<'msg>) = 
        x.Add(key, Event (EventCallbacks.singleton value))

    member x.Add(key : string, value : Aardvark.Import.Browser.Event -> seq<'msg>) = 
        x.Add(key, Event (EventCallbacks.singleton { useCapture = false; callback = value }))

    static member Empty = AttributeMap<'msg>(AMap.empty)
    static member Single(key : string, value : AttributeValue<'msg>) = AttributeMap<'msg>(AMap.ofList [key, value])

    static member Union(l : AttributeMap<'msg>, r : AttributeMap<'msg>) =
        AttributeMap(AMap.unionWith union l.Values r.Values)

    static member OfASet (set : aset<string * AttributeValue<'msg>>) =
        let merge (key : string) (o : Option<AttributeValue<'msg>>) (n : AttributeValue<'msg>) =
            match o with
            | Some o -> union key o n |> Some
            | None -> n |> Some
        set 
        |> AMap.ofASet 
        |> AMap.choose (fun k vs -> vs |> Seq.fold (merge k) None)
        |> AttributeMap

    static member OfAMap (map : amap<string, AttributeValue<'msg>>) =
        AttributeMap map

    static member OfSeq (seq : seq<string * AttributeValue<'msg>>) =
        let merge (o : Option<AttributeValue<'msg>>) (key : string, n : AttributeValue<'msg>) =
            match o with
            | Some o -> union key o n |> Some
            | None -> n |> Some

        seq
        |> Seq.groupBy fst
        |> Seq.map (fun (key, vs) -> key, vs |> Seq.fold merge None |> Option.get)
        |> AMap.ofSeq
        |> AttributeMap

    static member OfList (list : list<string * AttributeValue<'msg>>) = AttributeMap<'msg>.OfSeq list
    static member OfArray (arr : array<string * AttributeValue<'msg>>) = AttributeMap<'msg>.OfSeq arr
       
module AttributeMap =   
    let inline empty<'msg> = AttributeMap<'msg>.Empty

    let inline ofSeq (seq : seq<string * AttributeValue<'msg>>) = AttributeMap<'msg>.OfSeq seq
    let inline ofList (list : list<string * AttributeValue<'msg>>) = AttributeMap<'msg>.OfList list
    let inline ofArray (arr : array<string * AttributeValue<'msg>>) = AttributeMap<'msg>.OfArray arr

    let inline single (key : string) (value : AttributeValue<'msg>) = AttributeMap<'msg>.Single(key, value)
    let inline add (key : string) (value : AttributeValue<'msg>) (m : AttributeMap<'msg>) = m.Add(key, value)
    let inline union (l : #seq<AttributeMap<'msg>>) = l |> Seq.fold (fun l r -> AttributeMap.Union(l,r)) AttributeMap.Empty

    let inline toAMap (m : AttributeMap<'msg>) = m.Values

    let map (mapping : string -> 'a -> 'b) (m : AttributeMap<'a>) =
        m.Values |> AMap.map (fun k v ->
            match v with
            | String v -> String v
            | Event f -> f.Map (mapping k) |> Event
        ) |> AttributeMap
        
    let choose (mapping : string -> 'a -> Option<'b>) (m : AttributeMap<'a>) =
        m.Values |> AMap.map (fun k v ->
            match v with
            | String v -> String v
            | Event f -> f.Choose (mapping k) |> Event
        ) |> AttributeMap
        
    let collect (mapping : string -> 'a -> seq<'b>) (m : AttributeMap<'a>) =
        m.Values |> AMap.map (fun k v ->
            match v with
            | String v -> String v
            | Event f -> f.Collect (mapping k) |> Event
        ) |> AttributeMap

[<AutoOpen>]
module AttributeMapBuilder =
    type AttributeMapBuilder() =
        member inline x.Yield((key : string, value : string)) = AttributeMap.single key (String value)
        member inline x.Yield((key : string, value : Aardvark.Import.Browser.Event -> seq<'msg>)) = AttributeMap.single key (Event (EventCallbacks.singleton { useCapture = false; callback = value }))
        member inline x.Yield((key : string, value : AttributeValue<'msg>)) = AttributeMap.single key value

        
        member inline x.Yield((key : string, value : IMod<string>)) = value |> AMap.bind (fun v -> AMap.ofList [key, String v]) |> AttributeMap
        member inline x.Yield((key : string, value : IMod<Aardvark.Import.Browser.Event -> seq<'msg>>)) = value |> AMap.bind (fun v -> AMap.ofList [key, Event (EventCallbacks.singleton { useCapture = false; callback = v })]) |> AttributeMap
        member inline x.Yield((key : string, value : IMod<AttributeValue<'msg>>)) = value |> AMap.bind (fun v -> AMap.ofList [key, v]) |> AttributeMap
        
        member inline x.Yield((key : string, value : IMod<Option<string>>)) = value |> AMap.bind (function Some v -> AMap.ofList [key, String v] | _ -> AMap.empty) |> AttributeMap
        member inline x.Yield((key : string, value : IMod<Option<Aardvark.Import.Browser.Event -> seq<'msg>>>)) = value |> AMap.bind (function Some v -> AMap.ofList [key, Event (EventCallbacks.singleton { useCapture = false; callback = v })] | _ -> AMap.empty) |> AttributeMap
        member inline x.Yield((key : string, value : IMod<Option<AttributeValue<'msg>>>)) = value |> AMap.bind (function Some v -> AMap.ofList [key, v] | _ -> AMap.empty) |> AttributeMap

        member inline x.YieldFrom(m : AttributeMap<'msg>) = m
        member inline x.YieldFrom(m : seq<string * AttributeValue<'msg>>) = AttributeMap.ofSeq m
        member inline x.YieldFrom(m : aset<string * AttributeValue<'msg>>) = AttributeMap.OfASet m
        member inline x.YieldFrom(m : amap<string, AttributeValue<'msg>>) = AttributeMap m
        
        member inline x.YieldFrom(m : seq<string * string>) = m |> Seq.map (fun (k,v) -> k, String v) |> AttributeMap.ofSeq
        member inline x.YieldFrom(m : aset<string * string>) = m |> ASet.map (fun (k,v) -> k, String v) |> AttributeMap.OfASet
        member inline x.YieldFrom(m : amap<string, string>) = m |> AMap.map (fun k v -> String v) |> AttributeMap
        

        member inline x.Combine (l : AttributeMap<'msg>, r : AttributeMap<'msg>) = AttributeMap.union [l;r]
        member inline x.Delay(f : unit -> AttributeMap<'msg>) = f()
        member inline x.Zero() = AttributeMap.empty
        member inline x.Bind(m : IMod<'a>, f : 'a -> AttributeMap<'msg>) = m |> AMap.bind (fun v -> f(v).Values) |> AttributeMap

        member inline x.For(s : seq<'a>, f : 'a -> AttributeMap<'msg>) = 
            s |> Seq.map f |> AttributeMap.union

        member inline x.For(s : aset<'a>, f : 'a -> AttributeMap<'msg>) = 
            s |> ASet.map f |> ASet.toMod |> AMap.bind (AttributeMap.union >> AttributeMap.toAMap) |> AttributeMap

        member inline x.For(s : alist<'a>, f : 'a -> AttributeMap<'msg>) = 
            s |> AList.map f |> AList.toMod |> AMap.bind (AttributeMap.union >> AttributeMap.toAMap) |> AttributeMap
            
        member inline x.For(s : amap<'a, 'b>, f : ('a * 'b) -> AttributeMap<'msg>) = 
            s |> AMap.map (fun k v -> f(k,v)) |> AMap.toMod |> AMap.bind (HMap.values >> AttributeMap.union >> AttributeMap.toAMap) |> AttributeMap

    
    let attributes = AttributeMapBuilder()
    let inline att k v = (k,v)

    let private test (a : IMod<bool>) (width : IMod<float>) (keys : amap<string, string>) =
        attributes {
            yield att "class" "asdas"
            yield att "onclick" (fun e -> Seq.singleton e)
            yield att "style" (width |> Mod.map (fun w -> if w > 0.1 then Some (sprintf "%.3f%%" w) else None))

            match! a with
            | true -> 
                yield att "class" "sepp"
            | _ ->
                ()

            yield! keys
            for (k, v) in keys do
                yield att k ("yeah" + v)
                
        }


