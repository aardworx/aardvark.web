module Worker

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Data
open Aardvark.Data.Durable
open Fable.Core
open Aardvark.SceneGraph

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
            root        : MutableTree<'a>
            running     : ref<int>
            last        : ref<Option<ImmutableTree<'a>>>
        }


    let updateMutableTree (state : State<'a>) (view : Trafo3d) =
        let cmp (l, _) (r, _) = compare l r
        let queue = List<float * MutableTree<'a>>()

        let inline enqueue (node : MutableTree<'a>) =
            let q = state.quality view node.original
            queue.HeapEnqueue(cmp, (q, node))

        let running = state.running
        let inline inc v = running := !running + v
        let inline dec v = running := !running - v

        enqueue state.root
        while !state.running < 12 && queue.Count > 0 do
            //Log.line "running: %d" !state.running
            let (q, e) = queue.HeapDequeue(cmp)
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

     
        ()

    let computeDelta (l : Option<ImmutableTree<'a>>) (r : MutableTree<'a>) =
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



let minDist (b : Box3d) (v : V3d) =

    let bMax = V3d(max b.Min.X b.Max.X, max b.Min.Y b.Max.Y, max b.Min.Z b.Max.Z)
    let bMin = V3d(min b.Min.X b.Max.X, min b.Min.Y b.Max.Y, min b.Min.Z b.Max.Z)
    let b = Box3d(bMin, bMax)

    let x = 
        if v.X > b.Max.X then b.Max.X
        elif v.X < b.Min.X then b.Min.X
        else v.X
        
    let y = 
        if v.Y > b.Max.Y then b.Max.Y
        elif v.Y < b.Min.Y then b.Min.Y
        else v.Y
        
    let z = 
        if v.Z > b.Max.Z then b.Max.Z
        elif v.Z < b.Min.Z then b.Min.Z
        else v.Z

    let c = V3d(x,y,z)
    //Log.line "%A %A -> %A" b v c
    v - c |> Vec.length

let angle (localBounds : Box3d) (view : Trafo3d) (avgPointDistance : float) =
    let cam = view.Backward.C3.XYZ
    let minDist = minDist localBounds cam
    let minDist = max 0.01 minDist
    Constant.DegreesPerRadian * atan2 avgPointDistance minDist



let quality (rootCenter : V3d) (view : Trafo3d) (n : Octnode) =
    if n.SubNodeIds.Length > 0 then 
        let worldBounds = n.BoundingBox
        let localBounds = Box3d(worldBounds.Min - rootCenter, worldBounds.Max - rootCenter)

        let dist  =
            let normMax = max (max (abs localBounds.Size.X) (abs localBounds.Size.Y)) (abs localBounds.Size.Z)
            normMax / 40.0

        let q = 1.0 / angle localBounds view dist
        q
    else 
        System.Double.PositiveInfinity

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
    let cache = Dict<int, Octree>(Unchecked.hash, Unchecked.equals)

    let create (url : string) =
        let avgSize = 300.0 * 1024.0
        let totalMem = 512.0 * 1024.0 * 1024.0
        let db = Database(url, totalMem / avgSize)
        Octree(db)

    let mutable states : hmap<int, State<Octnode>> = HMap.empty
    let mutable currentView = Trafo3d.Identity


    let rec update() =
        //Log.line "update %d" states.Count
        for (_,s) in states do
            let center = s.center

            updateMutableTree s currentView
            let (a,b) = computeDelta !s.last s.root
            s.last := a

            for op in b do
                let res = 
                    op.ops |> Seq.map (fun (n,o) -> 
                        match o.value with
                        | Some v -> 
                            n.Id, { alloc = o.alloc; active = o.active; value = Some (v.Data.[Octree.Buffer] |> unbox<_>) }
                        | None -> n.Id, { alloc = o.alloc; active = o.active; value = None }
                    ) |> Seq.toArray

                postMessage(Reply.Perform res)
        setTimeout update 50 |> ignore

    update()

    self.addEventListener_message (fun e ->
        match unbox<Command> e.data with
        | Command.Add (tid, url) ->
            let tree = cache.GetOrCreate(tid, fun _ -> create url)
            tree.Root.``then`` (fun root -> 
                let center = tree.Center
                let state = 
                    { 
                        center = tree.Center
                        quality = quality center
                        children = fun n -> (FSharp.Collections.Array.choose (fun a -> a) n.SubNodes)
                        root = { MutableTree.original = root; MutableTree.kill = ref id; MutableTree.children = ref None } 
                        running = ref 0
                        last = ref None
                    }
            
                states <- HMap.add tid state states
            ) |> ignore

        | Command.Remove id ->
            match cache.TryRemove id with
            | Some t -> t.Root.``then`` (fun r -> states <- HMap.remove id states) |> ignore
            | None -> ()

        | Command.UpdateCamera(view, proj) ->
            currentView <- view
            //if unbox timeout then clearTimeout(timeout)
            //timeout <- setTimeout update 10
    )
    0
