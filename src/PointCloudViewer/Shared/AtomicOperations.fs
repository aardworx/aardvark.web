namespace Aardvark.Data

open System.Collections.Generic
open Aardvark.Base
open Aardvark.Base.Incremental

[<AutoOpen>]
module AtomicOps =

    module HMap =
        let keys (m : hmap<'a, 'b>) =
            HSet.ofSeq (Seq.map fst (HMap.toSeq m))

        let applySetDelta (set : hdeltaset<'a>) (value : 'b) (m : hmap<'a, 'b>) =
            let delta = 
                set |> HDeltaSet.toHMap |> HMap.map (fun e r ->
                    if r > 0 then ElementOperation.Set value
                    else ElementOperation.Remove
                )
            HMap.applyDelta m delta |> fst

    [<StructuredFormatDisplay("{AsString}")>]
    type Operation<'a> =
        {
            alloc   : int
            active  : int
            value   : Option<'a>
        }


        member x.Inverse =
            {
                alloc = -x.alloc
                active = -x.active
                value = x.value
            }
        
        member x.ToString(name : string) =
            if x.alloc > 0 then 
                if x.active > 0 then sprintf "alloc(%s, +1)" name
                elif x.active < 0 then sprintf "alloc(%s, -1)" name
                else sprintf "alloc(%s)" name
            elif x.alloc < 0 then sprintf "free(%s)" name
            elif x.active > 0 then sprintf "activate(%s)" name
            elif x.active < 0 then sprintf "deactivate(%s)" name
            else sprintf "nop(%s)" name

        override x.ToString() =
            if x.alloc > 0 then 
                if x.active > 0 then sprintf "alloc(%A, +1)" x.value.Value
                elif x.active < 0 then sprintf "alloc(%A, -1)" x.value.Value
                else sprintf "alloc(%A)" x.value.Value
            elif x.alloc < 0 then "free"
            elif x.active > 0 then "activate"
            elif x.active < 0 then "deactivate"
            else "nop"

        member private x.AsString = x.ToString()

        static member Zero : Operation<'a> = { alloc = 0; active = 0; value = None }

        static member Nop : Operation<'a> = { alloc = 0; active = 0; value = None }
        static member Alloc(value, active) : Operation<'a> = { alloc = 1; active = (if active then 1 else 0); value = Some value }
        static member Free : Operation<'a> = { alloc = -1; active = -1; value = None }
        static member Activate : Operation<'a> = { alloc = 0; active = 1; value = None }
        static member Deactivate : Operation<'a> = { alloc = 0; active = -1; value = None }

        static member (+) (l : Operation<'a>, r : Operation<'a>) =
            {
                alloc = l.alloc + r.alloc
                active = l.active + r.active
                value = match r.value with | Some v -> Some v | None -> l.value
            }

    module Operation =
        let map (f : 'a -> 'b) (o : Operation<'a>) =
            {
                alloc   = o.alloc
                active  = o.active
                value   = (match o.value with | Some v -> Some (f v) | None -> None)
            }

    let Nop<'a> = Operation<'a>.Nop
    let Alloc(v,a) = Operation.Alloc(v,a)
    let Free<'a> = Operation<'a>.Free
    let Activate<'a> = Operation<'a>.Activate
    let Deactivate<'a> = Operation<'a>.Deactivate

    let (|Nop|Alloc|Free|Activate|Deactivate|) (o : Operation<'a>) =
        if o.alloc > 0 then Alloc(o.value.Value, o.active)
        elif o.alloc < 0 then Free(o.active)
        elif o.active > 0 then Activate
        elif o.active < 0 then Deactivate
        else Nop
        
    [<StructuredFormatDisplay("{AsString}")>]
    type AtomicOperation<'a, 'b> =
        {
            keys : hset<'a>
            ops : hmap<'a, Operation<'b>>
        }
            
        override x.ToString() =
            x.ops 
            |> Seq.map (fun (a, op) -> op.ToString(sprintf "%A" a)) 
            |> String.concat "; " |> sprintf "atomic [%s]"

        member private x.AsString = x.ToString()

        member x.Inverse =
            {
                keys = x.keys
                ops = x.ops |> HMap.map (fun _ o -> o.Inverse)
            }

        static member Empty : AtomicOperation<'a, 'b> = { keys = HSet.empty; ops = HMap.empty }
        static member Zero : AtomicOperation<'a, 'b> = { keys = HSet.empty; ops = HMap.empty }

        static member (+) (l : AtomicOperation<'a, 'b>, r : AtomicOperation<'a, 'b>) =
            let merge (key : 'a) (l : Option<Operation<'b>>) (r : Option<Operation<'b>>) =
                match l with
                | None -> r
                | Some l ->
                    match r with
                    | None -> Some l
                    | Some r -> 
                        match l + r with
                        | Nop -> None
                        | op -> Some op

            let ops = HMap.choose2 merge l.ops r.ops 
            let keys = HMap.keys ops
            { ops = ops; keys = keys }
            
        member x.IsEmpty = HMap.isEmpty x.ops
            
    module AtomicOperation =

        let empty<'a, 'b> = AtomicOperation<'a, 'b>.Empty
        
        let ofHMap (ops : hmap<'a, Operation<'b>>) =
            let keys = HMap.keys ops
            { ops = ops; keys = keys }

        let ofSeq (s : seq<'a * Operation<'b>>) =
            let ops = HMap.ofSeq s
            let keys = HMap.keys ops
            { ops = ops; keys = keys }
                
        let ofList (l : list<'a * Operation<'b>>) = ofSeq l
        let ofArray (a : array<'a * Operation<'b>>) = ofSeq a

    type AtomicQueue<'a, 'b> private(classId : uint32, classes : hmap<'a, uint32>, values : MapExt<uint32, AtomicOperation<'a, 'b>>) =
        let classId = if HMap.isEmpty classes then 0u else classId

        static let empty = AtomicQueue<'a, 'b>(0u, HMap.empty, MapExt.empty)

        static member Empty = empty

        member x.Enqueue(op : AtomicOperation<'a, 'b>) =
            if not op.IsEmpty then
                let clazzes = op.keys |> HSet.choose (fun k -> HMap.tryFind k classes)

                if clazzes.Count = 0 then
                    let id = classId
                    let classId = id + 1u
                    let classes = op.keys |> Seq.fold (fun c k -> HMap.add k id c) classes
                    let values = MapExt.add id op values
                    AtomicQueue(classId, classes, values)
                        
                else
                    let mutable values = values
                    let mutable classes = classes
                    let mutable result = AtomicOperation.empty
                    for c in clazzes do
                        match MapExt.tryRemove c values with
                        | Some (o, rest) ->
                            values <- rest
                            classes <- op.keys |> HSet.fold (fun cs c -> HMap.remove c cs) classes
                            // may not overlap here
                            result <- { ops = HMap.union result.ops o.ops; keys = HSet.union result.keys o.keys } //result + o

                        | None ->
                            ()

                    let result = result + op
                    if result.IsEmpty then
                        AtomicQueue(classId, classes, values)
                    else
                        let id = classId
                        let classId = id + 1u

                        let classes = result.keys |> HSet.fold (fun cs c -> HMap.add c id cs) classes
                        let values = MapExt.add id result values
                        AtomicQueue(classId, classes, values)
                            
            else
                x
            
        member x.TryDequeue() =
            match MapExt.tryMin values with
            | None ->
                None
            | Some clazz ->
                let v = values.[clazz]
                let values = MapExt.remove clazz values
                let classes = v.keys |> HSet.fold (fun cs c -> HMap.remove c cs) classes
                let newQueue = AtomicQueue(classId, classes, values)
                Some (v, newQueue)

        member x.Dequeue() =
            match x.TryDequeue() with
            | None -> failwith "empty AtomicQueue"
            | Some t -> t

        member x.IsEmpty = MapExt.isEmpty values

        member x.Count = values.Count

        member x.UnionWith(other : AtomicQueue<'a, 'b>) =
            if x.Count < other.Count then
                other.UnionWith x
            else
                other |> Seq.fold (fun (s : AtomicQueue<_,_>) e -> s.Enqueue e) x

        static member (+) (s : AtomicQueue<'a, 'b>, a : AtomicOperation<'a, 'b>) = s.Enqueue a

        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = new AtomicQueueEnumerator<_,_>((values :> seq<_>).GetEnumerator()) :> _
                
        interface IEnumerable<AtomicOperation<'a, 'b>> with
            member x.GetEnumerator() = new AtomicQueueEnumerator<_,_>((values :> seq<_>).GetEnumerator()) :> _

    and private AtomicQueueEnumerator<'a, 'b>(e : IEnumerator<KeyValuePair<uint32, AtomicOperation<'a, 'b>>>) =
        interface System.Collections.IEnumerator with
            member x.MoveNext() = e.MoveNext()
            member x.Current = e.Current.Value :> obj
            member x.Reset() = e.Reset()

        interface IEnumerator<AtomicOperation<'a, 'b>> with
            member x.Dispose() = e.Dispose()
            member x.Current = e.Current.Value

    module AtomicQueue =

        [<GeneralizableValue>]
        let empty<'a, 'b> = AtomicQueue<'a, 'b>.Empty

        let inline isEmpty (queue : AtomicQueue<'a, 'b>) = queue.IsEmpty
        let inline count (queue : AtomicQueue<'a, 'b>) = queue.Count
        let inline enqueue (v : AtomicOperation<'a, 'b>) (queue : AtomicQueue<'a, 'b>) = queue.Enqueue v
        let inline tryDequeue (queue : AtomicQueue<'a, 'b>) = queue.TryDequeue()
        let inline dequeue (queue : AtomicQueue<'a, 'b>) = queue.Dequeue()
        let inline combine (l : AtomicQueue<'a, 'b>) (r : AtomicQueue<'a, 'b>) = l.UnionWith r
            
        let enqueueMany (v : #seq<AtomicOperation<'a, 'b>>) (queue : AtomicQueue<'a, 'b>) = v |> Seq.fold (fun s e -> enqueue e s) queue
        let ofSeq (s : seq<AtomicOperation<'a, 'b>>) = s |> Seq.fold (fun q e -> enqueue e q) empty
        let ofList (l : list<AtomicOperation<'a, 'b>>) = l |> List.fold (fun q e -> enqueue e q) empty
        let ofArray (a : array<AtomicOperation<'a, 'b>>) = a |> Array.fold (fun q e -> enqueue e q) empty
                
        let toSeq (queue : AtomicQueue<'a, 'b>) = queue :> seq<_>
        let toList (queue : AtomicQueue<'a, 'b>) = queue |> Seq.toList
        let toArray (queue : AtomicQueue<'a, 'b>) = queue |> Seq.toArray
        
        let toOperation (queue : AtomicQueue<'a, 'b>) =
            queue |> Seq.sum
