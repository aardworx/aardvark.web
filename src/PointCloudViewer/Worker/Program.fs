module Worker

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Data
open Aardvark.Data.Durable
open Fable.Core


//type Config<'a> =
//    {
//        quality     : Trafo3d -> 'a -> float
//        children    : 'a -> array<Aardvark.Import.JS.Promise<'a>>
//    }


[<AutoOpen>]
module Lod =
    open System.Collections.Generic

    type ImmutableTree<'a> =
        {
            original : 'a
            children : list<ImmutableTree<'a>>
        }

    type MutableTree<'a> =
        {
            original    : 'a
            kill        : ref<unit -> unit>
            children    : ref<Option<ref<list<MutableTree<'a>>>>>
        }

    type State<'a> =
        {
            center      : V3d
            quality     : Trafo3d -> 'a -> float
            children    : 'a -> array<Aardvark.Import.JS.Promise<'a>>
            size        : 'a -> int
            roots       : hmap<string, MutableTree<'a>>
            running     : ref<int>
            lasts       : ref<hmap<string, ref<Option<ImmutableTree<'a>>>>>
        }


    let updateMutableTree (state : State<'a>) (view : Trafo3d) =
        let cmp (l, _) (r, _) = compare l r
        let queue = List<float * MutableTree<'a>>()

        let inline enqueue (node : MutableTree<'a>) =
            let q = state.quality view node.original
            queue.HeapEnqueue(cmp, (q, node))

        let mutable pointCount = 0.0
        let running = state.running
        let inline inc v = running := !running + v
        let inline dec v = running := !running - v


        let cap = 50000000.0


        let inline collapse (e : MutableTree<'a>) =
            match !e.children with
            | Some r ->
                match !r with
                | [] -> 
                    e.kill.Value()
                    e.kill := (fun () -> ())
                    e.children := None
                | _ -> 
                    Log.debug "collapse %A" e.original
                    let rec kill (node : MutableTree<'a>) =
                        let cs = !node.children
                        node.kill.Value()
                        node.kill := (fun () -> ())
                        node.children := None
                        match cs with
                        | Some cs -> !cs |> List.iter kill
                        | None -> ()

                    kill e
            | None ->
                ()


        for (_,t) in state.roots do
            enqueue t
        while pointCount < cap && !state.running < 12 && queue.Count > 0 do
            //Log.line "running: %d" !state.running
            let (q, e) = queue.HeapDequeue(cmp)

            let size = float (state.size e.original)
            
            pointCount <- pointCount + size

            //Log.line "q: %.3f" q
            if q <= 1.0 then   
                match !e.children with
                | None ->
                    Log.debug "split %A; %.3f" e.original q
                    let r = ref []
                    let prom = state.children e.original
                    let kill () = ()
                    let prom = prom |> FSharp.Collections.Array.map (fun p -> inc 1; p |> Prom.map (fun r -> dec 1; r))
                    prom |> Prom.all |> Prom.map (fun v ->
                        let children = Seq.toList v
                        r := children |> List.map (fun c -> { original = c; kill = ref (fun () -> ()); children = ref None })
                    ) |> ignore
                    e.kill := fun () -> kill()
                    e.children := Some r
                | Some cs -> 
                    for c in !cs do enqueue c
            else 
                collapse e

        if pointCount >= cap then
            while queue.Count > 0 do
                let (q,e) = queue.HeapDequeue(cmp)
                let s = state.size e.original
                pointCount <- pointCount + float s
                collapse e
        //console.warn pointCount

        ()

    let rec alli (m : ImmutableTree<'a>) =
        match m.children with
        | [] -> Seq.singleton m.original
        | cs -> cs |> Seq.collect (alli)

    
    let rec all (m : MutableTree<'a>) =
        match !m.children with
        | Some cs ->
            match !cs with
            | [] -> Seq.singleton m.original
            | cs -> cs |> Seq.collect (all)
        | None ->
            Seq.singleton m.original

    let rec kill (l : ImmutableTree<'a>) =
        l.children |> List.fold (fun o c -> o + kill c) (AtomicOperation.ofList [l.original, Operation.Free])

    let computeDelta (l : Option<ImmutableTree<'a>>) (r : MutableTree<'a>) =
        let rec computeDelta (deltas : ref<AtomicQueue<'a, 'a>>) (l : Option<ImmutableTree<'a>>) (r : Option<MutableTree<'a>>) =
            let inline children (m : MutableTree<'a>) =
                match !m.children with
                | None -> []
                | Some r -> !r
        
            match l, r with
            | None, None -> 
                None
            | Some l, None ->
                deltas := AtomicQueue.enqueue (kill l) !deltas
                None

            | None, Some r ->
                match children r with
                | [] ->
                    deltas := AtomicQueue.enqueue (AtomicOperation.ofList [r.original, Operation.Alloc(r.original, true)]) !deltas
                    Some { ImmutableTree.original = r.original; ImmutableTree.children = [] }
                | cs ->
                    let childQueue = ref AtomicQueue.empty
                    let cs = cs |> List.choose (fun r -> computeDelta childQueue None (Some r))
                    let op = 
                        AtomicOperation.ofList [r.original, Operation.Alloc(r.original, false)] +
                        AtomicQueue.toOperation !childQueue
                    deltas := AtomicQueue.enqueue op !deltas
                    Some { ImmutableTree.original = r.original; ImmutableTree.children = cs }
            
            | Some l, Some r ->
                match l.children, children r with
                | [], [] -> 
                    Some l
                | lc, [] -> 
                    //deltas := AtomicQueue.enqueue (AtomicOperation.ofList [l.original, Operation.Activate]) !deltas
                    let op =
                        AtomicOperation.ofList [l.original, Operation.Activate] +
                        (lc |> List.sumBy kill)
                    deltas := AtomicQueue.enqueue op !deltas
                    Some { l with children = [] }
                | [], rc ->

                    let childQueue = ref AtomicQueue.empty
                    let cs = rc |> List.choose (fun r -> computeDelta childQueue None (Some r))

                    let op =
                        AtomicOperation.ofList [l.original, Operation.Deactivate] +
                        (AtomicQueue.toOperation !childQueue)
                        
                    deltas := AtomicQueue.enqueue op !deltas
                    Some { l with children = cs }
                | lc, rc ->
                    let cs = 
                        List.zip lc rc |> List.choose (fun (lc,rc) ->
                            computeDelta deltas (Some lc) (Some rc)
                        )
                    Some { l with children = cs }
    
        let delta = ref AtomicQueue.empty
        let n = computeDelta delta l (Some r)
        n, !delta

[<Emit("postMessage($0)")>]
let postMessage (msg : obj) : unit = jsNative

type pako =
    abstract member deflate : Uint8Array -> Uint8Array 
    abstract member inflate : Uint8Array -> Uint8Array

[<AutoOpen>]
module GlobalThings =

    let [<Import("*", "pako")>] pako: pako = jsNative

[<EntryPoint>]
let main _ =
    let cache = Dict<string, Promise<Octree>>(Unchecked.hash, Unchecked.equals)

    let create (url : Database) =
        let avgSize = 300.0 * 1024.0
        let totalMem = 512.0 * 1024.0 * 1024.0
        let store = 
            match url with
            | Local name -> 
                LocalBlobStore.connect name |> unbox<Promise<IBlobStore>>
            | Url url ->
                let db = HttpBlobStore(url)
                Prom.value (db :> IBlobStore)
            | Azure(token, id) ->
                Azure.openRead token id
        store |> Prom.map (fun store -> Octree store)

    let mutable center = V3d.Zero

    let mutable state : State<Octnode> =
        { 
            size = fun (n : Octnode) -> n.PointCountCell
            center = V3d.Zero
            quality = fun (view : Trafo3d) (n : Octnode) -> n.SplitQuality(center, view)
            children = fun n -> (FSharp.Collections.Array.choose (fun a -> a) n.SubNodes)
            roots = HMap.empty
            running = ref 0
            lasts = ref HMap.empty
        }
    let mutable currentView = Trafo3d.Identity


    let rec update (s : State<Octnode>) =
        //Log.line "update %d" states.Count
        updateMutableTree s currentView
        let mutable queue = AtomicQueue.empty
        let lasts = !s.lasts


        let merge (k : string) (root : Option<MutableTree<Octnode>>) (last : Option<ref<Option<ImmutableTree<Octnode>>>>) =
            match root, last with
            | Some root, Some last -> 
                let (a,b) = computeDelta !last root
                last := a
                queue <- AtomicQueue.combine queue b
                Some last

            | Some root, None ->
                let (a,b) = computeDelta None root
                let r = ref a
                queue <- AtomicQueue.combine queue b
                Some r

            | None, Some last ->
                match !last with
                | Some l ->
                    let b = kill l
                    queue <- AtomicQueue.enqueue b queue
                | None ->
                    ()
                
                None
            | None, None ->
                None

        s.lasts := HMap.choose2 merge s.roots !s.lasts


        for (key, r) in s.roots do
            match HMap.tryFind key lasts with
            | Some last ->
                let (a,b) = computeDelta !last r
                last := a
                queue <- AtomicQueue.combine queue b
            | None ->
                let (a,b) = computeDelta None r
                let r = ref a
                s.lasts := HMap.add key r !s.lasts
                queue <- AtomicQueue.combine queue b
                    




        for op in queue do
            let res = 
                op.ops |> Seq.map (fun (n,o) -> 
                    match o.value with
                    | Some v -> 
                        n.Id, { alloc = o.alloc; active = o.active; value = Some (v.Data.[Octree.Buffer] |> unbox<_>) }
                    | None -> n.Id, { alloc = o.alloc; active = o.active; value = None }
                ) |> Seq.toArray

            postMessage(Reply.Perform res)
        setTimeout (fun () -> update state) 50 |> ignore

    update state

    self.addEventListener_message (fun e ->
        match unbox<Command> e.data with
        | Command.Add (url) ->
            let tree = cache.GetOrCreate(url, fun _ -> 
                match Database.parse url with
                | Some url -> create url
                | None -> Log.error "bad url: %A" url; failwith ""
            )
            
            tree.``then``(fun tree ->
                tree.Root.``then`` (fun root -> 
                    let center = tree.Center
                    postMessage(Reply.SetRootCenter(url, center))

                    state <-
                        { state with 
                            center = tree.Center
                            quality = fun (view : Trafo3d) (n : Octnode) -> n.SplitQuality(center, view)
                            roots = HMap.add url  { MutableTree.original = root; MutableTree.kill = ref id; MutableTree.children = ref None } state.roots
                        }

                ) |> ignore
            ) |> ignore
        | Command.Remove url ->
            match cache.TryRemove url with
            | Some t -> 
                t.``then``(fun t -> 
                    t.Root.``then`` (fun r -> 
                        state <-
                            { state with 
                                roots = HMap.remove url state.roots
                            }
                        ) |> ignore
                ) |> ignore
            | None -> ()

        | Command.UpdateCamera(view, proj) ->
            currentView <- view
            //if unbox timeout then clearTimeout(timeout)
            //timeout <- setTimeout update 10
    )
    0
