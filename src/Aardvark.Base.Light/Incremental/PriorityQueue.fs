namespace Aardvark.Base.Incremental




open System.Collections.Generic
open Aardvark.Base

/// <summary>
/// represents a simple priority queue using user-given compare function
/// </summary>
type PriorityQueue<'a>(cmp : 'a -> 'a -> int) =

    // we simply use the Aardvark.Base implementation here.
    // to ensure that all compare-functions used are identical
    // we wrap the base implementation in PriorityQueue.
    let store = List<'a>()

    /// <summary>
    /// enqueues a new element
    /// </summary>
    member x.Enqueue (v : 'a) =
        store.HeapEnqueue(cmp, v)

    /// <summary>
    /// dequeues the min element from the queue and
    /// fails if the queue is empty
    /// </summary>
    member x.Dequeue() =
        store.HeapDequeue(cmp)

    /// <summary>
    /// returns the number of elements currently contained in the queue
    /// </summary>
    member x.Count =
        store.Count

    /// <summary>
    /// returns the current minimal value (according to cmp) contained
    /// and fails if the queue is empty.
    /// </summary>
    member x.Min = store.[0]

/// <summary>
/// implements a queue with "uncomparable" duplicates. 
/// This is helpful since regular heap implementation cannot
/// deal with a large number of duplicated keys efficiently.
/// Note: the duplicated values will be returned in the order they were enqueued
/// </summary>
type DuplicatePriorityQueue<'a, 'k when 'k : comparison>(extract : 'a -> 'k) =
    let q = PriorityQueue<'k> compare
    let values = Dictionary<'k, Queue<'a>>()
    let mutable count = 0
    let mutable currentQueue = Unchecked.defaultof<_>

    /// <summary>
    /// enqueues a new element
    /// </summary>
    member x.Enqueue(v : 'a) =
        let k = extract v
        count <- count + 1

        if values.ContainsKey k then
            currentQueue <- values.[k]
            currentQueue.Enqueue v
        else
            let inner = Queue<'a>()
            inner.Enqueue v
            values.[k] <- inner
            q.Enqueue k

             
    /// <summary>
    /// dequeues the current minimal value (and its key)
    /// </summary>   
    member x.Dequeue(key : ref<'k>) =
        let k = q.Min

        if values.ContainsKey k then
            currentQueue <- values.[k]
            let res = currentQueue.Dequeue()
            count <- count - 1
            if currentQueue.Count = 0 then
                q.Dequeue() |> ignore
                values.Remove k |> ignore

            key := k
            res
        else
            failwith "inconsistent state in DuplicatePriorityQueue"

    /// <summary>
    /// returns the number of elements currently contained in the queue
    /// </summary>
    member x.Count =
        count



type Cache<'k, 'v>(creator : 'k -> 'v) =
    let mutable store = HMap.empty

    member x.Invoke(k : 'k) =
        let res = ref Unchecked.defaultof<'v>
        store <- 
            store |> HMap.alter k (fun o ->
                match o with
                | Some (v,r) -> 
                    res := v
                    Some (v, r + 1)
                | None ->
                    let v = creator k
                    res := v
                    Some (v, 1)
            )
        !res

    member x.Clear(dispose : 'v -> unit) =
        store |> HMap.iter (fun _ (v,_) -> dispose v)
        store <- HMap.empty
        
    member x.RevokeAndGetDeletedTotal(k : 'k) =
        let res = ref None
        store <- 
            store |> HMap.alter k (fun o ->
                match o with
                | Some (v,1) ->
                    res := Some(true, v)
                    None
                | Some (v,r) -> 
                    res := Some(false, v)
                    Some (v, r - 1)
                | None ->
                    failwith "cannot revoke unknown object"
                    None
            )
        !res
    member x.RevokeAndGetDeleted(k : 'k) =
        let res = ref Unchecked.defaultof<'v>
        let del = ref false
        store <- 
            store |> HMap.alter k (fun o ->
                match o with
                | Some (v,1) ->
                    del := true
                    res := v
                    None
                | Some (v,r) -> 
                    res := v
                    Some (v, r - 1)
                | None ->
                    failwith "cannot revoke unknown object"
                    None
            )
        !del, !res

    member x.Revoke(k : 'k) =
        let res = ref Unchecked.defaultof<'v>
        store <- 
            store |> HMap.alter k (fun o ->
                match o with
                | Some (v,1) ->
                    res := v
                    None
                | Some (v,r) -> 
                    res := v
                    Some (v, r - 1)
                | None ->
                    failwith "cannot revoke unknown object"
                    None
            )
        !res


module ChangeTracker =
    let track<'a>() : 'a -> bool =
        let old = ref None
        fun v -> 
            match !old with
            | Some o when Unchecked.equals v o  -> false
            | _ -> old := Some v;  true