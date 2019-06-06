namespace Aardvark.UI

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Base.Rendering
open Aardvark.Application

type AttributeValue<'msg> =
    | String of string
    | Event of (Aardvark.Import.Browser.Event -> seq<'msg>)


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
            | Event l, Event r -> Event (fun e -> Seq.append (l e) (r e))
            | _, r -> r

    member x.Values = values

    member x.Add(key : string, value : AttributeValue<'msg>) = 
        let v = AMap.ofList [key, value]
        AttributeMap(AMap.unionWith union values v)

    member x.Add(key : string, value : string) = 
        x.Add(key, String value)

    member x.Add(key : string, value : Aardvark.Import.Browser.Event -> seq<'msg>) = 
        x.Add(key, Event value)

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
            | Event f -> f >> Seq.map (mapping k) |> Event
        ) |> AttributeMap
        
    let choose (mapping : string -> 'a -> Option<'b>) (m : AttributeMap<'a>) =
        m.Values |> AMap.map (fun k v ->
            match v with
            | String v -> String v
            | Event f -> f >> Seq.choose (mapping k) |> Event
        ) |> AttributeMap
        
    let collect (mapping : string -> 'a -> seq<'b>) (m : AttributeMap<'a>) =
        m.Values |> AMap.map (fun k v ->
            match v with
            | String v -> String v
            | Event f -> f >> Seq.collect (mapping k) |> Event
        ) |> AttributeMap
module AttributeMapBuilder =
    type AttributeMapBuilder() =
        member inline x.Yield((key : string, value : string)) = AttributeMap.single key (String value)
        member inline x.Yield((key : string, value : Aardvark.Import.Browser.Event -> seq<'msg>)) = AttributeMap.single key (Event value)
        member inline x.Yield((key : string, value : AttributeValue<'msg>)) = AttributeMap.single key value

        
        member inline x.Yield((key : string, value : IMod<string>)) = value |> AMap.bind (fun v -> AMap.ofList [key, String v]) |> AttributeMap
        member inline x.Yield((key : string, value : IMod<Aardvark.Import.Browser.Event -> seq<'msg>>)) = value |> AMap.bind (fun v -> AMap.ofList [key, Event v]) |> AttributeMap
        member inline x.Yield((key : string, value : IMod<AttributeValue<'msg>>)) = value |> AMap.bind (fun v -> AMap.ofList [key, v]) |> AttributeMap
        
        member inline x.Yield((key : string, value : IMod<Option<string>>)) = value |> AMap.bind (function Some v -> AMap.ofList [key, String v] | _ -> AMap.empty) |> AttributeMap
        member inline x.Yield((key : string, value : IMod<Option<Aardvark.Import.Browser.Event -> seq<'msg>>>)) = value |> AMap.bind (function Some v -> AMap.ofList [key, Event v] | _ -> AMap.empty) |> AttributeMap
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

    let test (a : IMod<bool>) (width : IMod<float>) (keys : amap<string, string>) =
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




[<RequireQualifiedAccess>]
type Node<'msg> =
    internal
    | NEmpty
    | NText of tag : string * attributes : AttributeMap<'msg> * value : IMod<string>
    | NNode of tag : string * attributes : AttributeMap<'msg> * children : alist<Node<'msg>>
    | NRender of attributes : AttributeMap<'msg> * scene : (RenderControl -> ISg)
    | NMap of mapping : (obj -> seq<'msg>) * child : Node<obj>

    member x.Attributes : AttributeMap<'msg> =
        match x with
        | NEmpty -> AttributeMap.empty
        | NText(_,a,_) -> a
        | NNode(_,a,_) -> a
        | NRender(a,_) -> a
        | NMap(mapping,inner) ->
            inner.Attributes |> AttributeMap.collect (fun _ -> mapping)

    member x.TagName : Option<string> =
        match x with
        | NEmpty -> None
        | NText(t,_,_) -> Some t
        | NNode(t,_,_) -> Some t
        | NRender _ -> Some "canvas"
        | NMap(_,i) -> i.TagName

    static member Empty : Node<'msg> = NEmpty
    static member Text(tag : string, attributes : AttributeMap<'msg>, value : IMod<string>) = NText(tag, attributes, value)
    static member Node(tag : string, attributes : AttributeMap<'msg>, children : alist<Node<'msg>>) = NNode(tag, attributes, children)
    static member Render(attributes : AttributeMap<'msg>, scene : ISg) = NRender(attributes, fun _ -> scene)
    static member Render(attributes : AttributeMap<'msg>, scene : RenderControl -> ISg) = NRender(attributes, scene)
    static member Map(mapping : 'a -> 'b, m : Node<'a>) = NMap((fun o -> Seq.singleton (mapping (unbox o))), unbox m)
    static member Choose (mapping : 'a -> Option<'b>, m : Node<'a>) : Node<'b> = NMap((fun o -> match mapping (unbox o) with | Some v -> Seq.singleton v | None -> Seq.empty), unbox m)
    static member Collect (mapping : 'a -> seq<'b>, m : Node<'a>) : Node<'b> = NMap((fun o -> mapping (unbox o)), unbox m)
    static member Ignore (m : Node<'a>) : Node<'msg> = NMap((fun _ -> Seq.empty), unbox m)

module Updater = 
    open Fable.Core
    open Fable.Core.JsInterop
    open Aardvark.Import.JS
    open Aardvark.Import.Browser

    type Scope<'msg> =
        {
            emit : seq<'msg> -> unit
        }

    [<AbstractClass>] 
    type NodeUpdater<'msg>(s : Scope<'msg>) =
        inherit AdaptiveObject()


        static let create (scope : Scope<'msg>) (createNode : string -> HTMLElement) (n : Node<'msg>) =
            match n with
            | Node.NEmpty -> EmptyUpdater<'msg>(scope) :> NodeUpdater<_>
            | Node.NText(a,b,c) -> TextUpdater<'msg>(scope, createNode(a), a, b, c) :> NodeUpdater<_>
            | Node.NNode(a,b,c) -> InnerNodeUpdater<'msg>(scope, createNode(a), a, b, c) :> NodeUpdater<_>
            | Node.NMap(mapping,b) -> MapUpdater<'msg>(scope, b, createNode, mapping) :> NodeUpdater<_>
            | Node.NRender(att, scene) -> RenderUpdater<'msg>(scope, createNode "canvas", att, scene) :> NodeUpdater<_>

        static member Create(parent : HTMLElement, scope : Scope<'msg>, n : Node<'msg>) =
            let createNode (tag : string) =
                let n = document.createElement(tag)
                let arr = FSharp.Collections.Array.init (int parent.children.length) (fun i -> parent.children.[i])
                for c in arr do c.remove()
                parent.appendChild n |> ignore
                n
            create scope createNode n
            
        static member Create(createNode : string -> HTMLElement, scope : Scope<'msg>, n : Node<'msg>) =
            create scope createNode n

        override x.Kind = "NodeUpdater"

        member x.Scope = s

        abstract member Node : HTMLElement
        abstract member Compute : AdaptiveToken -> unit
        abstract member Kill : unit -> unit
        abstract member TryReplace : AdaptiveToken * Node<'msg>  -> bool


        member x.Update(t : AdaptiveToken) =
            x.EvaluateIfNeeded t () (fun t ->
                x.Compute(t)
            )

        member x.Destroy() =
            let foo = ref 0
            x.Outputs.Consume(foo) |> ignore
            x.Kill()

    and AttributeUpdater<'msg>(node : HTMLElement, attributes : AttributeMap<'msg>) =
        inherit AdaptiveObject()
        let mutable attributes = attributes
        let mutable reader = attributes.Values.GetReader()
        let listeners = Dict<string, EventListenerObject>(Unchecked.hash, Unchecked.equals)

        let update (ops : hdeltamap<string,AttributeValue<'msg>>) (s : Scope<'msg>) =
            for (k, o) in ops do
                match o with
                | Remove ->
                    match listeners.TryRemove k with
                    | Some l ->
                        node.removeEventListener(k, U2.Case2 l)
                    | None -> 
                        node.removeAttribute k
                | Set value ->
                    match value with
                    | String v -> 
                        node.setAttribute(k, v)
                    | Event callback ->
                        let listener =
                            listeners.GetOrCreate(k, fun k ->
                                { new EventListenerObject with
                                    member x.handleEvent e = Seq.delay (fun () -> callback e) |> s.emit
                                }
                            )
                        //Log.warn "addEventListener(%s)" k
                        node.addEventListener(k, U2.Case2 listener)

        override x.Kind = "AttributeUpdater"

        member x.Replace(newAttributes : AttributeMap<'msg>, t : AdaptiveToken, s : Scope<'msg>) =
            x.EvaluateAlways t (fun t ->
                let newReader = newAttributes.Values.GetReader()
                let _ = newReader.GetOperations(t)
                let ops = HMap.computeDelta reader.State newReader.State

                for (k,o) in ops do
                    match o with
                    | Set v -> 
                        match v with
                        | AttributeValue.String v -> Log.warn "set %s = %s" k v
                        | _ -> Log.warn "set %s" k
                    | Remove -> 
                        Log.warn "remove %s" k

                reader.Dispose()
                attributes <- newAttributes
                reader <- newReader
                update ops s
            )

        member x.Update(t : AdaptiveToken, s : Scope<'msg>) =
            x.EvaluateIfNeeded t () (fun t -> 
                let ops = reader.GetOperations t
                update ops s
            )

        member x.Destroy() =
            for (k, _) in reader.State do
                match listeners.TryGetValue k with
                | Some l ->
                    node.removeEventListener(k, U2.Case2 l)
                | None -> 
                    node.removeAttribute k
            listeners.Clear()
            reader.Dispose()
     
    and EmptyUpdater<'msg>(s : Scope<'msg>) =
        inherit NodeUpdater<'msg>(s)

        override x.Node = null
        override x.Compute(_) = ()
        override x.Kill() = ()
        override x.TryReplace (_t : AdaptiveToken, n : Node<'msg>) = 
            match n with
            | Node.NEmpty -> true
            | _ -> false

    and TextUpdater<'msg>(scope : Scope<'msg>, node : HTMLElement, tag : string, attributes : AttributeMap<'msg>, content : IMod<string>) =
        inherit NodeUpdater<'msg>(scope)

        let mutable attributes = attributes
        let mutable content = content

        let mutable lastValue = None
        let att = AttributeUpdater(node, attributes)
        
        override x.Node = node
        override x.Compute(t) =
            att.Update(t, scope)
            let v = content.GetValue t
            match lastValue with
            | Some o when o = v -> ()
            | _ ->
                //Log.warn "%s.innerText = \"%s\"" node.tagName v
                node.innerText <- v
                lastValue <- Some v

        override x.Kill() =
            att.Destroy()
            node.innerText <- ""
            lastValue <- None
            
        override x.TryReplace (t : AdaptiveToken, n : Node<'msg>) =
            match n with
            | Node.NText(nt, a, c) when nt = tag ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(a, t, scope)
                    attributes <- a

                    content.RemoveOutput x
                    content <- c
                    let v = c.GetValue t
                    match lastValue with
                    | Some o when o = v -> ()
                    | _ ->
                        //Log.warn "repl %s.innerText = \"%s\"" node.tagName v
                        node.innerText <- v
                        lastValue <- Some v

                    true
                )
            | _ ->
                false

    and InnerNodeUpdater<'msg>(scope : Scope<'msg>, node : HTMLElement, tag : string, attributes : AttributeMap<'msg>, children : alist<Node<'msg>>) =
        inherit NodeUpdater<'msg>(scope)

        let mutable attributes = attributes
        let mutable children = children
        let mutable reader = children.GetReader()

        let mutable nodes : MapExt<Index, NodeUpdater<'msg>> = MapExt.empty
        let att = AttributeUpdater(node, attributes)
        

        let update (t : AdaptiveToken) (ops : pdeltalist<Node<'msg>>) =
            for (i, op) in PDeltaList.toSeq ops do
                match op with
                | Remove ->
                    match MapExt.tryRemove i nodes with
                    | Some (u, rest) ->
                        nodes <- rest
                        u.Destroy()
                        if unbox u.Node then
                            //Log.warn "remove %s" u.Node.tagName
                            u.Node.remove()
                    | None ->
                        Log.warn "strange"

                | Set value ->
                    let (_l, s, r) = MapExt.neighbours i nodes

                    let insert (tag : string) =
                        //Log.warn "insert %s" tag
                        match r with
                        | Some (_ri, r) when unbox r.Node ->
                            let n = document.createElement(tag)
                            node.insertBefore(n, r.Node) |> ignore
                            n
                                
                        | _ ->
                            let n = document.createElement(tag)
                            node.appendChild n |> ignore
                            n

                    match s with
                    | Some (_,s) ->
                        if not (s.TryReplace(t, value)) then
                            s.Destroy()
                            if unbox s.Node then s.Node.remove()
                            let n = NodeUpdater<'msg>.Create(insert, scope, value)
                            n.Update(t)
                            nodes <- MapExt.add i n nodes

                    | None ->
                        let n = NodeUpdater<'msg>.Create(insert, scope, value)
                        n.Update(t)
                        nodes <- MapExt.add i n nodes

            for (_,s) in MapExt.toSeq nodes do
                s.Update(t)

        override x.Node = node
        override x.Compute(t) =
            att.Update(t, scope)

            let ops = reader.GetOperations t
            update t ops
                
            
        override x.Kill() =
            att.Destroy()

            for (k, v) in MapExt.toSeq nodes do
                v.Destroy()
                if unbox v.Node then v.Node.remove() 

            nodes <- MapExt.empty


        override x.TryReplace (t : AdaptiveToken, n : Node<'msg>) =
            match n with
            | Node.NNode(ntag, natt, nchildren) when tag = ntag ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(natt, t, scope)
                    attributes <- natt

                    let r = nchildren.GetReader()
                    let _ = r.GetOperations t
                    let nState = PList.toMap r.State
                    let oState = nodes

                    let tryUpdate (key : Index) (o : Option<NodeUpdater<'msg>>) (n : Option<Node<'msg>>) =
                        match o, n with
                        | Some o, Some n -> 
                            if o.TryReplace(t, n) then
                                None
                            else
                                Some (ElementOperation.Set n)
                        | None, Some n ->
                            Some (ElementOperation.Set n)
                        | Some o, None ->
                            nodes <- MapExt.remove key nodes
                            o.Destroy()
                            Some (ElementOperation.Remove)
                        | None, None ->
                            None

                    let deltas = MapExt.choose2 tryUpdate oState nState |> PDeltaList.ofMap

                    children <- nchildren
                    reader <- r

                    update t deltas
                    true
                )
            | _ ->
                false

    and MapUpdater<'msg>(scope : Scope<'msg>, inner : Node<obj>, createNode : string -> HTMLElement, mapping : obj -> seq<'msg>) =
        inherit NodeUpdater<'msg>(scope)

        let mutable mapping = mapping
        let innerScope =
            { 
                emit = fun v -> v |> Seq.collect mapping |> scope.emit
            }

        let inner = NodeUpdater<obj>.Create(createNode, innerScope, inner)
        
        override x.Kill() = inner.Destroy()
        override x.Node = inner.Node
        override x.Compute(t) = x.EvaluateIfNeeded t () (fun t -> inner.Update(t))
        
        override x.TryReplace (t : AdaptiveToken, n : Node<'msg>) =
            match n with
            | Node.NMap(nmapping,ninner) ->
                x.EvaluateAlways t (fun t ->
                    if inner.TryReplace(t, ninner) then
                        mapping <- nmapping
                        true
                    else
                        false
                )
            | _ -> 
                false

    and RenderUpdater<'msg>(scope : Scope<'msg>, node : HTMLElement, attributes : AttributeMap<'msg>, scene : RenderControl -> ISg) =
        inherit NodeUpdater<'msg>(scope)

        let canvas = unbox<HTMLCanvasElement> node
        let control = new Aardvark.Application.RenderControl(canvas, true, true, ClearColor = V4d.OOOO)
        
        let att = AttributeUpdater<'msg>(node, attributes)

        let wrap(scene : RenderControl -> ISg) =
            scene control
            |> Sg.uniform "ViewportSize" control.Size

        let mutable scene = scene
        let mutable objects = (wrap scene).RenderObjects()
        let mutable task = new Aardvark.Rendering.WebGL.RenderTask(control.FramebufferSignature, control.Manager, objects)
        do control.RenderTask <- task
        override x.Kill() =
            att.Destroy()
            control.RenderTask <- RenderTask.empty
            task.Dispose()
            objects <- ASet.empty
            // control.Dispose()

        override x.TryReplace (t : AdaptiveToken, n : Node<'msg>) = 
            match n with
            | Node.NRender(a,b) ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(a, t, scope)

                    control.RenderTask <- RenderTask.empty
                    task.Dispose()

                    scene <- b
                    objects <- (wrap scene).RenderObjects()
                    task <- new Aardvark.Rendering.WebGL.RenderTask(control.FramebufferSignature, control.Manager, objects)
                    control.RenderTask <- task

                    true
                )
            | _ -> 
                false

        override x.Compute(t) =
            x.EvaluateIfNeeded t () (fun t ->
                att.Update(t, scope)
            )

        override x.Node = node



module Node =

    let inline empty<'msg> : Node<'msg> = Node<'msg>.Empty
    let inline text (tag : string) (att : AttributeMap<'msg>) (value : IMod<string>) = Node<'msg>.Text(tag, att, value)
    let inline node (tag : string) (att : AttributeMap<'msg>) (children : alist<Node<'msg>>) = Node<'msg>.Node(tag, att, children)
    let inline render (att : AttributeMap<'msg>) (sg : ISg) = Node<'msg>.Render(att, sg)
    let inline map (mapping : 'a -> 'b) (m : Node<'a>) : Node<'b> = Node<'a>.Map(mapping, m)
    let inline choose (mapping : 'a -> Option<'b>) (m : Node<'a>) = Node<'a>.Choose(mapping, m)
    let inline collect (mapping : 'a -> seq<'b>) (m : Node<'a>) = Node<'a>.Collect(mapping, m)
    let inline ignore (m : Node<'a>) : Node<'b> = Node<'b>.Ignore(m)
    let inline attributes<'msg>(n : Node<'msg>) : AttributeMap<'msg> = n.Attributes
    let inline tag (n : Node<'msg>) = n.TagName

    let inline newUpdater (parent : Aardvark.Import.Browser.HTMLElement) (scope : Updater.Scope<'msg>) (n : Node<'msg>) = Updater.NodeUpdater<'msg>.Create(parent, scope, n)
