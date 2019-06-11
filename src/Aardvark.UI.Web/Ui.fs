namespace Aardvark.UI

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Base.Rendering
open Aardvark.Application

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


        static let create (scope : Scope<'msg>) (createNode : string -> HTMLElement) (n : DomNode<'msg>) =
            match n with
            | DomNode.NEmpty -> EmptyUpdater<'msg>(scope) :> NodeUpdater<_>
            | DomNode.NText(a,b,c) -> TextUpdater<'msg>(scope, createNode(a), a, b, c) :> NodeUpdater<_>
            | DomNode.NNode(a,b,c) -> InnerNodeUpdater<'msg>(scope, createNode(a), a, b, c) :> NodeUpdater<_>
            | DomNode.NMap(mapping,b) -> MapUpdater<'msg>(scope, b, createNode, mapping) :> NodeUpdater<_>
            | DomNode.NRender(att, scene) -> RenderUpdater<'msg>(scope, createNode "canvas", att, scene) :> NodeUpdater<_>
            | DomNode.NBoot(b, s, i) -> BootUpdater<'msg>(scope, defaultArg b ignore, defaultArg s ignore, i, createNode) :> NodeUpdater<_>
        static member Create(parent : HTMLElement, scope : Scope<'msg>, n : DomNode<'msg>) =
            let createNode (tag : string) =
                let n = document.createElement(tag)
                let arr = FSharp.Collections.Array.init (int parent.children.length) (fun i -> parent.children.[i])
                for c in arr do c.remove()
                parent.appendChild n |> ignore
                n
            create scope createNode n
            
        static member Create(createNode : string -> HTMLElement, scope : Scope<'msg>, n : DomNode<'msg>) =
            create scope createNode n

        override x.Kind = "NodeUpdater"

        member x.Scope = s

        abstract member Node : HTMLElement
        abstract member Compute : AdaptiveToken -> unit
        abstract member Kill : unit -> unit
        abstract member TryReplace : AdaptiveToken * DomNode<'msg>  -> bool


        member x.Update(t : AdaptiveToken) =
            x.EvaluateIfNeeded t () (fun t ->
                x.Compute(t)
            )

        member x.Destroy() =
            let foo = ref 0
            x.Outputs.Consume(foo) |> ignore
            x.Kill()

    and AttributeUpdater<'msg>(node : HTMLElement, attributes : AttributeMap<'msg>, trySubscribe : string -> Scope<'msg> -> EventCallbacks<'msg> -> System.IDisposable) =
        inherit AdaptiveObject()
        let mutable attributes = attributes
        let mutable reader = attributes.Values.GetReader()
        let parentListenerIds = Dict<string, System.IDisposable>(Unchecked.hash, Unchecked.equals)
        let listeners = Dict<string, EventListenerObject * EventListenerObject>(Unchecked.hash, Unchecked.equals)

        let update (ops : hdeltamap<string,AttributeValue<'msg>>) (s : Scope<'msg>) =
            for (k, o) in ops do
                match o with
                | Remove ->
                    match parentListenerIds.TryRemove k with
                    | Some (id) ->
                        id.Dispose()
                    | None -> 
                        match listeners.TryRemove k with
                        | Some(capture, nonCaputre) ->
                            if unbox capture then node.removeEventListener(k, U2.Case2 capture, true)
                            if unbox nonCaputre then node.removeEventListener(k, U2.Case2 nonCaputre, false)
                        | None -> 
                            node.removeAttribute k
                | Set value ->
                    match value with
                    | String v -> 
                        node.setAttribute(k, v)
                    | Event callbacks ->
                        if k = "boot" then
                            callbacks.AsSeq |> Seq.iter (fun cb ->
                                cb.callback (unbox (node, s)) |> ignore
                            )
                        else
                            let id = trySubscribe k s callbacks
                            if unbox id then
                                parentListenerIds.[k] <- id
                            else 
                                let capture, nonCaputre = callbacks |> EventCallbacks.toList |> List.partition (fun cb -> cb.useCapture)
                                let capture, nonCaputre =
                                    listeners.GetOrCreate(k, fun k -> 
                                        let capture =  
                                            match capture with 
                                            | [] -> null
                                            | _ -> 
                                                { new EventListenerObject with
                                                    member x.handleEvent e = Seq.delay (fun () -> capture |> Seq.collect (fun cb -> cb.callback e)) |> s.emit
                                                }
                                        let nonCaputre =  
                                            match nonCaputre with 
                                            | [] -> null
                                            | _ -> 
                                                { new EventListenerObject with
                                                    member x.handleEvent e = Seq.delay (fun () -> nonCaputre |> Seq.collect (fun cb -> cb.callback e)) |> s.emit
                                                }
                                        capture, nonCaputre
                                    )
                                //Log.warn "addEventListener(%s)" k
                                if unbox capture then node.addEventListener(k, U2.Case2 capture, true)
                                if unbox nonCaputre then node.addEventListener(k, U2.Case2 nonCaputre, false)

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
                | Some(capture, nonCapture) ->
                    if unbox capture then node.removeEventListener(k, U2.Case2 capture, true)
                    if unbox nonCapture then node.removeEventListener(k, U2.Case2 nonCapture, true)
                | None -> 
                    node.removeAttribute k
            listeners.Clear()
            reader.Dispose()
     
        new(node : HTMLElement, attributes : AttributeMap<'msg>) = AttributeUpdater<'msg>(node, attributes, (fun _ _ _ -> null))

    and EmptyUpdater<'msg>(s : Scope<'msg>) =
        inherit NodeUpdater<'msg>(s)

        override x.Node = null
        override x.Compute(_) = ()
        override x.Kill() = ()
        override x.TryReplace (_t : AdaptiveToken, n : DomNode<'msg>) = 
            match n with
            | DomNode.NEmpty -> true
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
            
        override x.TryReplace (t : AdaptiveToken, n : DomNode<'msg>) =
            match n with
            | DomNode.NText(nt, a, c) when nt = tag ->
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

    and InnerNodeUpdater<'msg>(scope : Scope<'msg>, node : HTMLElement, tag : string, attributes : AttributeMap<'msg>, children : alist<DomNode<'msg>>) =
        inherit NodeUpdater<'msg>(scope)

        let mutable attributes = attributes
        let mutable children = children
        let mutable reader = children.GetReader()

        let mutable nodes : MapExt<Index, NodeUpdater<'msg>> = MapExt.empty
        let att = AttributeUpdater(node, attributes)
        

        let update (t : AdaptiveToken) (ops : pdeltalist<DomNode<'msg>>) =
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


        override x.TryReplace (t : AdaptiveToken, n : DomNode<'msg>) =
            match n with
            | DomNode.NNode(ntag, natt, nchildren) when tag = ntag ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(natt, t, scope)
                    attributes <- natt

                    let r = nchildren.GetReader()
                    let _ = r.GetOperations t
                    let nState = PList.toMap r.State
                    let oState = nodes

                    let tryUpdate (key : Index) (o : Option<NodeUpdater<'msg>>) (n : Option<DomNode<'msg>>) =
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

    and MapUpdater<'msg>(scope : Scope<'msg>, inner : DomNode<obj>, createNode : string -> HTMLElement, mapping : obj -> seq<'msg>) =
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
        
        override x.TryReplace (t : AdaptiveToken, n : DomNode<'msg>) =
            match n with
            | DomNode.NMap(nmapping,ninner) ->
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
        
        let trySubscribe (name : string) (s : Scope<'msg>) (cb : EventCallbacks<'msg>) =
            if name = "rendered" then
                control.Rendered.Subscribe(fun () -> 
                    for cb in cb.AsSeq do
                        s.emit (cb.callback null)
                )
            else 
                null
                


        let att = AttributeUpdater<'msg>(node, attributes, trySubscribe)

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

        override x.TryReplace (t : AdaptiveToken, n : DomNode<'msg>) = 
            match n with
            | DomNode.NRender(a,b) ->
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

    and BootUpdater<'msg>(scope : Scope<'msg>, boot : HTMLElement -> unit, shutdown : HTMLElement -> unit, inner : DomNode<'msg>, createNode : string -> HTMLElement) =
        inherit NodeUpdater<'msg>(scope)
        
        let mutable boot = boot
        let mutable shutdown = shutdown
        
        let inner = NodeUpdater<'msg>.Create(createNode, scope, inner)
        let mutable node = null

        override x.Kill() = 
            if unbox node then shutdown node
            inner.Destroy()
            node <- null
            boot <- ignore
            shutdown <- ignore

        override x.Node = inner.Node
        override x.Compute(t) = 
            x.EvaluateIfNeeded t () (fun t -> 
                inner.Update(t)
                if node <> inner.Node then
                    if unbox node then shutdown node
                    if unbox inner.Node then boot inner.Node
                    node <- inner.Node
            )

        override x.TryReplace (t : AdaptiveToken, n : DomNode<'msg>) =
            match n with
            | DomNode.NBoot(b, s, i) ->
                x.EvaluateAlways t (fun t ->
                    let b = defaultArg b ignore
                    let s = defaultArg s ignore

                    if inner.TryReplace(t, i) then
                        if unbox node then shutdown node
                        if unbox inner.Node then b inner.Node
                        node <- inner.Node
                        boot <- b
                        shutdown <- s
                        true
                    else
                        false
                )
            | _ ->
                false
        



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DomNode =
    open Aardvark.Import.Browser

    let inline empty<'msg> : DomNode<'msg> = DomNode.Empty
    let inline text (tag : string) (att : AttributeMap<'msg>) (value : IMod<string>) = DomNode.Text(tag, att, value)
    let inline node (tag : string) (att : AttributeMap<'msg>) (children : alist<DomNode<'msg>>) = DomNode.Node(tag, att, children)
    let inline render (att : AttributeMap<'msg>) (sg : ISg) = DomNode.Render(att, sg)
    let inline map (mapping : 'a -> 'b) (m : DomNode<'a>) : DomNode<'b> = DomNode.Map(mapping, m)
    let inline choose (mapping : 'a -> Option<'b>) (m : DomNode<'a>) = DomNode.Choose(mapping, m)
    let inline collect (mapping : 'a -> seq<'b>) (m : DomNode<'a>) = DomNode.Collect(mapping, m)
    let inline ignore (m : DomNode<'a>) : DomNode<'b> = DomNode.Ignore(m)
    let inline tag (n : DomNode<'msg>) = n.TagName

    let onBoot (boot : HTMLElement -> unit) (n : DomNode<'msg>) =
        match n with
        | DomNode.NBoot(b, s, i) ->
            match b with
            | Some b -> 
                DomNode.NBoot(Some (fun e -> b e; boot e), s, i)
            | None ->
                DomNode.NBoot(Some boot, s, i)
        | _ ->
            DomNode.NBoot(Some boot, None, n)

    let onShutdown (shutdown : HTMLElement -> unit) (n : DomNode<'msg>) =
        match n with
        | DomNode.NBoot(b, s, i) ->
            match s with
            | Some s -> 
                DomNode.NBoot(b, Some (fun e -> s e; shutdown e), i)
            | None ->
                DomNode.NBoot(b, Some shutdown, i)
        | _ ->
            DomNode.NBoot(None, Some shutdown, n)
                
                 


    let inline newUpdater (parent : Aardvark.Import.Browser.HTMLElement) (scope : Updater.Scope<'msg>) (n : DomNode<'msg>) = 
        Updater.NodeUpdater<'msg>.Create(parent, scope, n)
