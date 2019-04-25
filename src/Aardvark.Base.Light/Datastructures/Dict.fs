namespace Aardvark.Base

open Aardvark.Import.JS


module private DictConstants =


    let primeSizes =
        [|
            (*    prime no.           prime *)
            (*           2                3       +  1 = 2^2 *)
            (*           4 *) 7                // +  1 = 2^3, minimal size
            (*           6 *) 13               // +  3 = 2^4
            (*          11 *) 31               // +  1 = 2^5
            (*          18 *) 61               // +  3 = 2^6
            (*          31 *) 127              // +  1 = 2^7
            (*          54 *) 251              // +  5 = 2^8
            (*          97 *) 509              // +  3 = 2^9
            (*         172 *) 1021             // +  3 = 2^10
            (*         309 *) 2039             // +  9 = 2^11
            (*         564 *) 4093             // +  3 = 2^12
            (*        1028 *) 8191             // +  1 = 2^13
            (*        1900 *) 16381            // +  3 = 2^14
            (*        3512 *) 32749            // + 19 = 2^15
            (*        6542 *) 65521            // + 15 = 2^16
            (*       12251 *) 131071           // +  1 = 2^17
            (*       23000 *) 262139           // +  5 = 2^18
            (*       43390 *) 524287           // +  1 = 2^19
            (*       82025 *) 1048573          // +  3 = 2^20
            (*      155611 *) 2097143          // +  9 = 2^21
            (*      295947 *) 4194301          // +  3 = 2^22
            (*      564163 *) 8388593          // + 15 = 2^23
            (*     1077871 *) 16777213         // +  3 = 2^24
            (*     2063689 *) 33554393         // + 39 = 2^25
            (*     3957809 *) 67108859         // +  5 = 2^26
            (*     7603553 *) 134217689        // + 39 = 2^27
            (*    14630843 *) 268435399        // + 57 = 2^28
            (*    28192750 *) 536870909        // +  3 = 2^29
            (*    54400028 *) 1073741789       // + 35 = 2^30
            (*   105097565 *) 2147483647       // +  1 = 2^31
        |]



[<AllowNullLiteral>]
type private DictEntry<'k, 'v> =
    class
        val mutable public hash : int
        val mutable public key : 'k
        val mutable public value : 'v
        val mutable public next : DictEntry<'k, 'v>

        new(h,k,v) = { hash = h; key = k; value = v; next = null }
    end

type Dict<'k, 'v>(hash : 'k -> int, equals : 'k -> 'k -> bool) =
    
    let mutable count = 0
    let mutable capacityIndex = 0
    let mutable capacity = DictConstants.primeSizes.[capacityIndex]

    let mutable store : DictEntry<'k, 'v>[] = FSharp.Collections.Array.zeroCreate capacity


    let rec tryFindEntry (key : 'k) (e : DictEntry<'k, 'v>) =
        if unbox e then
            if equals e.key key then
                Some e.value
            else
                tryFindEntry key e.next
        else
            None

    let rec setEntry (h : int) (key : 'k) (value : 'v) (e : DictEntry<'k, 'v>) =
        if unbox e then
            if equals e.key key then
                e.value <- value
                e
            else
                let n = setEntry h key value e.next
                e.next <- n
                e
        else
            let n = DictEntry(h, key, value)
            count <- count + 1
            n

    let rec tryRemoveEntry (v : ref<Option<'v>>) (key : 'k) (e : DictEntry<'k, 'v>) =
        if unbox e then
            if equals e.key key then
                v := Some e.value
                count <- count - 1
                e.next
            else
                let n = tryRemoveEntry v key e.next
                e.next <- n
                e
        else
            e

    let rec alterEntry (h : int) (key : 'k) (update : Option<'v> -> Option<'v>) (e : DictEntry<'k, 'v>) =
        if unbox e then
            if equals e.key key then
                match update (Some e.value) with
                | Some n -> 
                    e.value <- n
                    e
                | None ->
                    count <- count - 1
                    e.next
            else
                let n = alterEntry h key update e.next
                e.next <- n
                e
        else
            match update None with
            | None ->
                null
            | Some v ->
                let n = DictEntry(h, key, v)
                count <- count + 1
                n
                
    let rec getOrCreateEntry (v : ref<'v>) (h : int) (key : 'k) (update : 'k -> 'v) (e : DictEntry<'k, 'v>) =
        if unbox e then
            if equals e.key key then
                v := e.value
                e
            else
                let n = getOrCreateEntry v h key update e.next
                e.next <- n
                e
        else
            let nv = update key
            v:= nv
            let n = DictEntry(h, key, nv)
            count <- count + 1
            n

    let resize(newCapIndex : int) =
        let newCapIndex = if newCapIndex < 0 then 0 elif newCapIndex >= 29 then 28 else newCapIndex
        if newCapIndex <> capacityIndex then
            let newCap = DictConstants.primeSizes.[newCapIndex]
            let newStore : DictEntry<'k, 'v>[] = FSharp.Collections.Array.zeroCreate newCap

            for i in 0 .. store.Length - 1 do
                let mutable e = store.[i]
                while unbox e do
                    let n = e.next
                    let slot = uint32 e.hash % uint32 newCap |> int
                    e.next <- newStore.[slot]
                    newStore.[slot] <- e
                    e <- n

            store <- newStore
            capacityIndex <- newCapIndex
            capacity <- newCap


    member x.Count = count

    member x.GetOrCreate (key : 'k, creator : 'k -> 'v) =
        let res = ref Unchecked.defaultof<_>
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        store.[slot] <- getOrCreateEntry res h key creator store.[slot]
        if count > capacity then resize (capacityIndex + 1)
        !res

    member x.TryGetValue(key : 'k) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        tryFindEntry key store.[slot]

    member x.TryRemove(key : 'k) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        let r = ref None
        store.[slot] <- tryRemoveEntry r key store.[slot]
        if count * 2 < capacity then resize (capacityIndex - 1)
        !r
        
    member x.Remove(key : 'k) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        let r = ref None
        store.[slot] <- tryRemoveEntry r key store.[slot]
        if count * 2 < capacity then resize (capacityIndex - 1)
        Option.isSome !r

    member x.Alter(key : 'k, update : Option<'v> -> Option<'v>) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        store.[slot] <- alterEntry h key update store.[slot]
        if count * 2 < capacity then resize (capacityIndex - 1)
        elif count > capacity then resize (capacityIndex + 1)

    member x.Clear() =
        let cap = DictConstants.primeSizes.[0]
        store <- FSharp.Collections.Array.zeroCreate cap
        capacity <- cap
        capacityIndex <- 0
        count <- 0

    member x.Item
        with get (key : 'k) =
            match x.TryGetValue key with
            | Some v -> v
            | None -> failwith "key not found"

        and set (key : 'k) (value : 'v) =
            let h = hash key
            let slot = uint32 h % uint32 capacity |> int
            store.[slot] <- setEntry h key value store.[slot]
            if count > capacity then resize (capacityIndex + 1)

    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new DictEnumerator<_,_>(store) :> _
        
    interface System.Collections.Generic.IEnumerable<'k * 'v> with
        member x.GetEnumerator() = new DictEnumerator<_,_>(store) :> _

and private DictEnumerator<'k, 'v>(store : DictEntry<'k, 'v>[]) =
    let mutable id = -1
    let mutable current : DictEntry<'k, 'v> = null

    member x.MoveNext() =
        if unbox current then
            current <- current.next
            if unbox current then
                true
            else
                x.MoveNext()
        else
            id <- id + 1
            if id < store.Length then
                current <- store.[id]
                if unbox current then true
                else x.MoveNext()
            else
                false

    member x.Current = (current.key, current.value)
    member x.Reset() =
        id <- -1
        current <- null
    interface System.Collections.IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = x.Reset()

        
    interface System.Collections.Generic.IEnumerator<'k * 'v> with
        member x.Dispose() = ()
        member x.Current = x.Current



[<AllowNullLiteral>]
type private DictSetEntry<'k> =
    
    class
        val mutable public hash : int
        val mutable public key : 'k
        val mutable public next : DictSetEntry<'k>

        new(h,k) = { hash = h; key = k; next = null }
    end

type DictSet<'k>(hash : 'k -> int, equals : 'k -> 'k -> bool) =
    
    let mutable count = 0
    let mutable capacityIndex = 0
    let mutable capacity = DictConstants.primeSizes.[capacityIndex]
    let mutable store : DictSetEntry<'k>[] = FSharp.Collections.Array.zeroCreate capacity



    let rec containsEntry (key : 'k) (e : DictSetEntry<'k>) =
        if unbox e then
            if equals e.key key then
                true
            else
                containsEntry key e.next
        else
            false

    let rec addEntry (h : int) (key : 'k) (e : DictSetEntry<'k>) =
        if unbox e then
            if equals e.key key then
                e
            else
                let n = addEntry h key e.next
                e.next <- n
                e
        else
            let n = DictSetEntry(h, key)
            count <- count + 1
            n

    let rec removeEntry (key : 'k) (e : DictSetEntry<'k>) =
        if unbox e then
            if equals e.key key then
                count <- count - 1
                e.next
            else
                let n = removeEntry key e.next
                e.next <- n
                e
        else
            null

    let rec alterEntry (h : int) (key : 'k) (update : bool -> bool) (e : DictSetEntry<'k>) =
        if unbox e then
            if equals e.key key then
                match update true with
                | true -> 
                    e
                | false ->
                    count <- count - 1
                    e.next
            else
                let n = alterEntry h key update e.next
                e.next <- n
                e
        else
            match update false with
            | false ->
                null
            | true ->
                let n = DictSetEntry(h, key)
                count <- count + 1
                n
            

    let resize(newCapIndex : int) =
        let newCapIndex = if newCapIndex < 0 then 0 elif newCapIndex >= 29 then 28 else newCapIndex
        if newCapIndex <> capacityIndex then
            let newCap = DictConstants.primeSizes.[newCapIndex]
            let newStore : DictSetEntry<'k>[] = FSharp.Collections.Array.zeroCreate newCap

            for i in 0 .. store.Length - 1 do
                let mutable e = store.[i]
                while unbox e do
                    let n = e.next
                    let slot = uint32 e.hash % uint32 newCap |> int
                    e.next <- newStore.[slot]
                    newStore.[slot] <- e
                    e <- n

            store <- newStore
            capacityIndex <- newCapIndex
            capacity <- newCap


    member x.Count = count

    member x.Add(key : 'k) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        let c = count
        store.[slot] <- addEntry h key store.[slot]
        if count > capacity then resize (capacityIndex + 1)
        c <> count

    member x.Contains(key : 'k) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        containsEntry key store.[slot]

    member x.Remove(key : 'k) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        let c = count
        store.[slot] <- removeEntry key store.[slot]
        if count * 2 < capacity then resize (capacityIndex - 1)
        c <> count
    
    member x.Alter(key : 'k, update : bool -> bool) =
        let h = hash key
        let slot = uint32 h % uint32 capacity |> int
        store.[slot] <- alterEntry h key update store.[slot]
        if count * 2 < capacity then resize (capacityIndex - 1)
        elif count > capacity then resize (capacityIndex + 1)

    member x.Clear() =
        let cap = DictConstants.primeSizes.[0]
        store <- FSharp.Collections.Array.zeroCreate cap
        capacity <- cap
        capacityIndex <- 0
        count <- 0
        
        
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = new DictSetEnumerator<_>(store) :> _
                
    interface System.Collections.Generic.IEnumerable<'k> with
        member x.GetEnumerator() = new DictSetEnumerator<_>(store) :> _
        
and private DictSetEnumerator<'k>(store : DictSetEntry<'k>[]) =
    let mutable id = -1
    let mutable current : DictSetEntry<'k> = null

    member x.MoveNext() =
        if unbox current then
            current <- current.next
            if unbox current then
                true
            else
                x.MoveNext()
        else
            id <- id + 1
            if id < store.Length then
                current <- store.[id]
                if unbox current then true
                else x.MoveNext()
            else
                false

    member x.Current = current.key
    member x.Reset() =
        id <- -1
        current <- null
    interface System.Collections.IEnumerator with
        member x.MoveNext() = x.MoveNext()
        member x.Current = x.Current :> obj
        member x.Reset() = x.Reset()

        
    interface System.Collections.Generic.IEnumerator<'k> with
        member x.Dispose() = ()
        member x.Current = x.Current

