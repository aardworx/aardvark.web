namespace Aardvark.Base

open System
open Aardvark.Base

module Management =
    
    type Memory<'a> =
        {
            malloc : int -> 'a
            mfree : 'a -> int -> unit
            mcopy : 'a -> int -> 'a -> int -> int -> unit
            mrealloc : 'a -> int -> int -> 'a
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Memory =
        open Fable.Core
        open Aardvark.Import.JS
        open Aardvark.Import.Browser


        let arrayBuffer =
            {
                malloc = fun s -> ArrayBuffer.Create(s)

                mfree = fun a s -> ()

                mcopy = fun src srcOff dst dstOff size -> 
                    let s = Uint8Array.Create(src, srcOff, size)
                    let d = Uint8Array.Create(dst, dstOff, size)
                    d.set(unbox s)

                mrealloc = fun ptr o n -> 
                    let s = min o n
                    let r = ArrayBuffer.Create(n)
                    let src = Uint8Array.Create(ptr, 0, s)
                    let dst = Uint8Array.Create(r, 0, s)
                    dst.set(unbox src)
                    r
            }
     
        let dataView =
            {
                malloc = fun s -> DataView.Create(ArrayBuffer.Create(s))

                mfree = fun a s -> ()

                mcopy = fun src srcOff dst dstOff size -> 
                    let s = Uint8Array.Create(src.buffer, src.byteOffset + srcOff, size)
                    let d = Uint8Array.Create(dst.buffer, dst.byteOffset + dstOff, size)
                    d.set(unbox s)

                mrealloc = fun ptr o n -> 
                    let s = min o n
                    let r = ArrayBuffer.Create(n)
                    let src = Uint8Array.Create(ptr.buffer, ptr.byteOffset, s)
                    let dst = Uint8Array.Create(r, 0, s)
                    dst.set(unbox src)
                    DataView.Create(r)
            }

        let webgl (usage : float) (gl : WebGLRenderingContext) =

            let inline bind (f : float -> 'a) (b : WebGLBuffer) =
                let old = gl.getParameter(gl.ARRAY_BUFFER_BINDING) |> unbox<WebGLBuffer>
                gl.bindBuffer(gl.ARRAY_BUFFER, b)
                let res = f gl.ARRAY_BUFFER
                gl.bindBuffer(gl.ARRAY_BUFFER, old)
                res

            {
                malloc = fun s ->
                    let b = gl.createBuffer()
                    b |> bind (fun target ->
                        gl.bufferData(target, U3.Case1 (float s), usage)
                        b
                    )

                mfree = fun b s ->
                    gl.deleteBuffer b

                mcopy = fun src srcOff dst dstOff size -> 
                    failwith "not possible"

                mrealloc = fun ptr o n -> 
                    failwith "not possible"
            }

        let webgl2 (usage : float) (gl : WebGL2RenderingContext) =
            {
                malloc = fun s ->
                    let b = gl.createBuffer()
                    gl.bindBuffer(gl.COPY_WRITE_BUFFER, b)
                    gl.bufferData(gl.COPY_WRITE_BUFFER, U3.Case1 (float s), usage)
                    gl.bindBuffer(gl.COPY_WRITE_BUFFER, null)
                    b

                mfree = fun b s ->
                    gl.deleteBuffer b

                mcopy = fun src srcOff dst dstOff size -> 
                    gl.bindBuffer(gl.COPY_READ_BUFFER, src)
                    gl.bindBuffer(gl.COPY_WRITE_BUFFER, dst)
                    gl.copyBufferSubData(gl.COPY_READ_BUFFER, gl.COPY_WRITE_BUFFER, float srcOff, float dstOff, float size)
                    gl.bindBuffer(gl.COPY_READ_BUFFER, null)
                    gl.bindBuffer(gl.COPY_WRITE_BUFFER, null)

                mrealloc = fun ptr o n -> 
                    let s = min o n
                    let b = gl.createBuffer()
                    gl.bindBuffer(gl.COPY_WRITE_BUFFER, b)
                    gl.bufferData(gl.COPY_WRITE_BUFFER, U3.Case1 (float n), usage)
                    gl.bindBuffer(gl.COPY_READ_BUFFER, ptr)
                    gl.copyBufferSubData(gl.COPY_READ_BUFFER, gl.COPY_WRITE_BUFFER, 0.0, 0.0, float s)
                    gl.bindBuffer(gl.COPY_READ_BUFFER, null)
                    gl.bindBuffer(gl.COPY_WRITE_BUFFER, null)
                    gl.deleteBuffer ptr
                    b
            }

        let nop =
            {
                malloc = fun _ -> ()
                mfree = fun _ _ -> ()
                mrealloc = fun _ _ _ -> ()
                mcopy = fun _ _ _ _ _ -> ()
            }

    type nref<'a>(value : 'a) =
        static let mutable currentId = 1

        let mutable value = value
        let id =
            let v = currentId
            currentId <- v + 1
            v

        member private x.Id = id
        member x.Value
            with get() = value
            and set v = value <- v

        override x.GetHashCode() = id
        override x.Equals o =
            match o with
                | :? nref<'a> as o -> id = o.Id
                | _ -> false

        interface IComparable with
            member x.CompareTo o =
                match o with
                    | :? nref<'a> as o -> compare id o.Id
                    | _ -> failwith "uncomparable"

                    
        interface IComparable<nref<'a>> with
            member x.CompareTo o = compare id o.Id

    let inline private (!) (r : nref<'a>) =
        r.Value
        
    let inline private (:=) (r : nref<'a>) (value : 'a) =
        r.Value <- value

    [<AllowNullLiteral>]
    type Block<'a> =
        class
            val mutable public Parent : IMemoryManager<'a>
            val mutable public Memory : nref<'a>
            val mutable public Next : Block<'a>
            val mutable public Prev : Block<'a>
            val mutable public Offset : int
            val mutable public Size : int
            val mutable public IsFree : bool

            override x.ToString() =
                sprintf "[%d,%d)" x.Offset (x.Offset + x.Size)

            new(parent, m, o, s, f, p, n) = { Parent = parent; Memory = m; Offset = o; Size = s; IsFree = f; Prev = p; Next = n }
            new(parent, m, o, s, f) = { Parent = parent; Memory = m; Offset = o; Size = s; IsFree = f; Prev = null; Next = null }

            override x.GetHashCode() =
                HashCode.Combine(Unchecked.hash x.Memory, Unchecked.hash x.Offset, Unchecked.hash x.Size)

            override x.Equals o =
                match o with
                | :? Block<'a> as o -> o.Memory = x.Memory && o.Offset = x.Offset && o.Size = x.Size
                | _ -> false

            interface IComparable with
                member l.CompareTo(r) =
                    match r with
                    | :? Block<'a> as r -> 
                        if isNull l then
                            if isNull r then 0
                            else 1
                        elif isNull r then
                            -1
                        else
                            let c = compare l.Size r.Size
                            if c <> 0 then c
                            else 
                                let c = compare l.Offset r.Offset    
                                if c <> 0 then c
                                else compare l.Memory r.Memory   
                    | _ ->
                        failwith "bad comparison"


        end

    and FreeList<'a>() =
        static let comparer =
            { new System.Collections.Generic.IComparer<Block<'a>> with
                member x.Compare(l : Block<'a>, r : Block<'a>) =
                    if isNull l then
                        if isNull r then 0
                        else 1
                    elif isNull r then
                        -1
                    else
                        let c = compare l.Size r.Size
                        if c <> 0 then c
                        else 
                            let c = compare l.Offset r.Offset    
                            if c <> 0 then c
                            else compare l.Memory r.Memory   
            }

        let mutable store = MapExt<Block<'a>, int>(comparer, MapExtImplementation.MapEmpty)
        
        static let next (align : int) (v : int) =
            if v % align = 0 then v
            else v + (align - v % align)

        member x.TryGetGreaterOrEqual(size : int) =
            let query = Block(Unchecked.defaultof<_>, Unchecked.defaultof<_>, -1, size, true)
            let (_, _, r) = MapExt.neighbours query store
            match r with
            | Some (r,_) ->
                store <- MapExt.remove r store
                Some r
            | None ->
                None

        member x.TryGetAligned(align : int, size : int) =
            let min = Block(Unchecked.defaultof<_>, Unchecked.defaultof<_>, -1, size, true)
            let _,_,r = MapExt.split min store
            let res = 
                r |> MapExt.toSeq |> Seq.tryFind (fun (b,_) ->
                    let o = next align b.Offset
                    let s = b.Size - (o - b.Offset)
                    s >= size
                )

            match res with
                | Some (b,_) -> 
                    store <- MapExt.remove b store
                    Some b
                | None ->
                    None

        member x.Insert(b : Block<'a>) =
            store <- MapExt.add b 0 store

        member x.Remove(b : Block<'a>) =
            store <- MapExt.remove b store

        member x.Clear() =
            store <- MapExt.empty

    and IMemoryManager<'a> =
        interface end

    and MemoryManager<'a>(mem : Memory<'a>, initialCapacity : int) as this =
        static let nextPowerOfTwo (v : int) =
            let mutable x = v - 1
            x <- x ||| (x >>> 1)
            x <- x ||| (x >>> 2)
            x <- x ||| (x >>> 4)
            x <- x ||| (x >>> 8)
            x <- x ||| (x >>> 16)
            x + 1

        let free = FreeList<'a>()
        
        let store = nref <| mem.malloc initialCapacity
        let mutable capacity = initialCapacity
        let mutable first = Block<'a>(this, store, 0, initialCapacity, true)
        let mutable last = first
        do free.Insert(first)

        static let next (align : int) (v : int) =
            if v % align = 0 then v
            else v + (align - v % align)

        let changeCapacity (newCapacity : int) =
            let newCapacity = max newCapacity initialCapacity
            let oldCapacity = capacity
            if newCapacity <> oldCapacity then
                let o = !store
                let n = mem.mrealloc o oldCapacity newCapacity
                store := n
                capacity <- newCapacity
                let o = ()

                let additional = newCapacity - oldCapacity
                if additional > 0 then
                    if last.IsFree then
                        free.Remove(last) |> ignore
                        last.Size <- last.Size + additional
                        free.Insert(last)
                    else
                        let newFree = Block<'a>(this, store, oldCapacity, additional, true, last, null)
                        last.Next <- newFree
                        last <- newFree
                        free.Insert(newFree)
                else (* additional < 0 *)
                    let freed = -additional
                    if not last.IsFree  || last.Size < freed then
                        failwith "invalid memory manager state"
        
                    if last.Size > freed then
                        free.Remove(last) |> ignore
                        last.Size <- last.Size - freed
                        free.Insert(last)
                    else (* last.Size = freed *)
                        free.Remove(last) |> ignore
                        let l = last
                        if isNull l.Prev then first <- null
                        else l.Prev.Next <- null
                        last <- l.Prev

        let grow (additional : int) =
            let newCapacity = nextPowerOfTwo (capacity + additional)
            changeCapacity newCapacity
            
        member x.Alloc(align : int, size : int) =
            if size = 0 then
                Block<'a>(x, store, 0, 0, true, null, null)
            else
                lock free (fun () ->
                    match free.TryGetAligned(align, size) with
                        | Some b ->
                            let alignedOffset = next align b.Offset
                            let alignedSize = b.Size - (alignedOffset - b.Offset)
                            if alignedOffset > b.Offset then
                                let l = Block<'a>(x, store, b.Offset, alignedOffset - b.Offset, true, b.Prev, b)
                                if isNull l.Prev then first <- l
                                else l.Prev.Next <- l
                                b.Prev <- l
                                free.Insert(l)
                                b.Offset <- alignedOffset
                                b.Size <- alignedSize        
                            
                            if alignedSize > size then
                                let r = Block<'a>(x, store, alignedOffset + size, alignedSize - size, true, b, b.Next)
                                if isNull r.Next then last <- r
                                else r.Next.Prev <- r
                                b.Next <- r
                                free.Insert(r)
                                b.Size <- size

                            b.IsFree <- false
                            b
                        | None ->
                            grow size
                            x.Alloc(align, size)

                )

        member x.Alloc(size : int) =
            if size = 0 then
                Block<'a>(x, store, 0, 0, true, null, null)
            else
                lock free (fun () ->
                    match free.TryGetGreaterOrEqual size with
                        | Some b ->
                            if b.Size > size then
                                let rest = Block<'a>(x, store, b.Offset + size, b.Size - size, true, b, b.Next)
                        
                                if isNull rest.Next then last <- rest
                                else rest.Next.Prev <- rest
                                b.Next <- rest

                                free.Insert(rest)
                                b.Size <- size

                            b.IsFree <- false
                            b
                        | None ->
                            grow size
                            x.Alloc size
                )

        member x.Free(b : Block<'a>) =
            if not b.IsFree then
                lock free (fun () ->
                    if not b.IsFree then
                        let old = b
                    
                        let b = Block(x, store, b.Offset, b.Size, b.IsFree, b.Prev, b.Next)
                        if isNull b.Prev then first <- b
                        else b.Prev.Next <- b

                        if isNull b.Next then last <- b
                        else b.Next.Prev <- b

                        old.Next <- null
                        old.Prev <- null
                        old.IsFree <- true
                        old.Offset <- -1
                        old.Size <- 0


                        let prev = b.Prev
                        let next = b.Next
                        if not (isNull prev) && prev.IsFree then
                            free.Remove(prev) |> ignore
                        
                            b.Prev <- prev.Prev
                            if isNull prev.Prev then first <- b
                            else prev.Prev.Next <- b

                            b.Offset <- prev.Offset
                            b.Size <- b.Size + prev.Size

                        if not (isNull next) && next.IsFree then
                            free.Remove(next) |> ignore
                            b.Next <- next.Next
                            if isNull next.Next then last <- b
                            else next.Next.Prev <- b
                            b.Next <- next.Next

                            b.Size <- b.Size + next.Size


                        b.IsFree <- true
                        free.Insert(b)

                        if last.IsFree then
                            let c = nextPowerOfTwo last.Offset
                            changeCapacity c

                )

        member x.Realloc(b : Block<'a>, align : int, size : int) =
            if b.Size <> size then
                lock free (fun () ->
                    if b.IsFree then
                        let n = x.Alloc(align, size)

                        b.Prev <- n.Prev
                        b.Next <- n.Next
                        b.Size <- n.Size
                        b.Offset <- n.Offset
                        b.IsFree <- false

                        if isNull b.Prev then first <- b
                        else b.Prev.Next <- b
                        if isNull b.Next then last <- b
                        else b.Next.Prev <- b

                    elif b.Size > size then
                        if size = 0 then
                            x.Free(b)
                        else
                            let r = Block(x, store, b.Offset + size, b.Size - size, false, b, b.Next)
                            b.Next <- r
                            if isNull r.Next then last <- r
                            else r.Next.Prev <- r
                            x.Free(r)

                    elif b.Size < size then
                        let next = b.Next
                        let missing = size - b.Size
                        if not (isNull next) && next.IsFree && next.Size >= missing then
                            free.Remove next |> ignore

                            if missing < next.Size then
                                next.Offset <- next.Offset + missing
                                next.Size <- next.Size - missing
                                b.Size <- size
                                free.Insert(next)

                            else
                                b.Next <- next.Next
                                if isNull b.Next then last <- b
                                else b.Next.Prev <- b
                                b.Size <- size


                        else
                            let n = x.Alloc(align, size)
                            mem.mcopy !store b.Offset !store n.Offset b.Size
                            x.Free b

                            b.Prev <- n.Prev
                            b.Next <- n.Next
                            b.Size <- n.Size
                            b.Offset <- n.Offset
                            b.IsFree <- false

                            if isNull b.Prev then first <- b
                            else b.Prev.Next <- b
                            if isNull b.Next then last <- b
                            else b.Next.Prev <- b
            
                )

        member x.Realloc(b : Block<'a>, size : int) =
            x.Realloc(b, 1, size)

        member x.Capactiy = lock free (fun () -> capacity)

        member x.Use(b : Block<'a>, action : 'a -> int -> int -> 'r) =
            if b.IsFree then failwith "cannot use free block"
            action !store b.Offset b.Size

        member x.Use(action : 'a -> 'r) =
            action !store

        member x.Dispose() =
            mem.mfree !store capacity
            first <- null
            last <- null
            free.Clear()
            capacity <- -1

        member x.UnsafePointer = store.Value

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface IMemoryManager<'a>

    and ChunkedMemoryManager<'a>(mem : Memory<'a>, chunkSize : int) as this =
        
        let free = FreeList<'a>()
        let allocated = Dict<'a, int>(Unchecked.hash, Unchecked.equals)
        let mutable usedMemory = 0

//        do
//            let store = mem.malloc chunkSize
//            free.Insert(Block<'a>(this, nref store, 0n, chunkSize, true))
//            allocated.Add (store, chunkSize) |> ignore
//            usedMemory <- chunkSize

        static let next (align : int) (v : int) =
            if v % align = 0 then v
            else v + (align - v % align)

        let grow (additional : int) =
            let blockCap = max chunkSize additional
            usedMemory <- usedMemory + blockCap
            let newMem = mem.malloc blockCap
            allocated.[newMem] <- blockCap
            free.Insert(Block<'a>(this, nref newMem, 0, blockCap, true))

        let freed(block : Block<'a>) =
            if isNull block.Prev && isNull block.Next then
                match allocated.TryRemove !block.Memory with
                    | Some size -> 
                        usedMemory <- usedMemory - size
                        mem.mfree !block.Memory size
                    | _ ->
                        failwith "bad inconsistent hate"
            else
                free.Insert(block)

        member x.Alloc(align : int, size : int) =
            lock free (fun () ->
                match free.TryGetAligned(align, size) with
                    | Some b ->
                        let alignedOffset = next align b.Offset
                        let alignedSize = b.Size - (alignedOffset - b.Offset)
                        if alignedOffset > b.Offset then
                            let l = Block<'a>(x, b.Memory, b.Offset, alignedOffset - b.Offset, true, b.Prev, b)
                            if not (isNull l.Prev) then l.Prev.Next <- l
                            b.Prev <- l
                            free.Insert(l)
                            b.Offset <- alignedOffset
                            b.Size <- alignedSize        
                            
                        if alignedSize > size then
                            let r = Block<'a>(x, b.Memory, alignedOffset + size, alignedSize - size, true, b, b.Next)
                            if not (isNull r.Next) then r.Next.Prev <- r
                            b.Next <- r
                            free.Insert(r)
                            b.Size <- size

                        b.IsFree <- false
                        b
                    | None ->
                        grow size
                        x.Alloc(align, size)

            )

        member x.Alloc(size : int) =
            lock free (fun () ->
                match free.TryGetGreaterOrEqual size with
                    | Some b ->
                        if b.Size > size then
                            let rest = Block<'a>(x, b.Memory, b.Offset + size, b.Size - size, true, b, b.Next)
                        
                            if not (isNull rest.Next) then rest.Next.Prev <- rest
                            b.Next <- rest

                            free.Insert(rest)
                            b.Size <- size

                        b.IsFree <- false
                        b
                    | None ->
                        grow size
                        x.Alloc size
            )

        member x.Free(b : Block<'a>) =
            if not b.IsFree then
                lock free (fun () ->
                    let old = b
                    
                    let b = Block(x, b.Memory, b.Offset, b.Size, b.IsFree, b.Prev, b.Next)
                    if not (isNull b.Prev) then b.Prev.Next <- b

                    if not (isNull b.Next) then b.Next.Prev <- b

                    old.Next <- null
                    old.Prev <- null
                    old.IsFree <- true
                    old.Offset <- -1
                    old.Size <- 0


                    let prev = b.Prev
                    let next = b.Next
                    if not (isNull prev) && prev.IsFree then
                        free.Remove(prev) |> ignore
                        
                        b.Prev <- prev.Prev
                        if not (isNull prev.Prev) then prev.Prev.Next <- b

                        b.Offset <- prev.Offset
                        b.Size <- b.Size + prev.Size

                    if not (isNull next) && next.IsFree then
                        free.Remove(next) |> ignore
                        b.Next <- next.Next
                        if not (isNull next.Next) then next.Next.Prev <- b
                        b.Next <- next.Next

                        b.Size <- b.Size + next.Size


                    b.IsFree <- true
                    freed(b)

                )

        member x.Realloc(b : Block<'a>, align : int, size : int) =
            if b.Size <> size then
                lock free (fun () ->
                    if b.IsFree then
                        let n = x.Alloc(align, size)

                        b.Prev <- n.Prev
                        b.Next <- n.Next
                        b.Size <- n.Size
                        b.Offset <- n.Offset
                        b.IsFree <- false

                        if not (isNull b.Prev) then b.Prev.Next <- b
                        if not (isNull b.Next) then b.Next.Prev <- b

                    elif b.Size > size then
                        if size = 0 then
                            x.Free(b)
                        else
                            let r = Block(x, b.Memory, b.Offset + size, b.Size - size, false, b, b.Next)
                            b.Next <- r
                            if not (isNull r.Next) then r.Next.Prev <- r
                            x.Free(r)

                    elif b.Size < size then
                        let next = b.Next
                        let missing = size - b.Size
                        if not (isNull next) && next.IsFree && next.Size >= missing then
                            free.Remove next |> ignore

                            if missing < next.Size then
                                next.Offset <- next.Offset + missing
                                next.Size <- next.Size - missing
                                b.Size <- size
                                free.Insert(next)

                            else
                                b.Next <- next.Next
                                if not (isNull b.Next) then b.Next.Prev <- b
                                b.Size <- size


                        else
                            failwithf "[MemoryManager] cannot realloc when no mcopy given"
                )

        member x.Realloc(b : Block<'a>, size : int) =
            x.Realloc(b, 1, size)

        member x.Capactiy = lock free (fun () -> usedMemory)

        member x.Use(b : Block<'a>, action : 'a -> int -> int -> 'r) =
            if b.IsFree then failwith "cannot use free block"
            action !b.Memory b.Offset b.Size

        member x.Dispose() =
            for (a, s) in allocated do mem.mfree a s
            allocated.Clear()
            free.Clear()

        interface IDisposable with
            member x.Dispose() = x.Dispose()
            
        interface IMemoryManager<'a>
   

   
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MemoryManager =
        let createNop() = new MemoryManager<_>(Memory.nop, 16) 

   
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ChunkedMemoryManager =
        let createNop() = new ChunkedMemoryManager<_>(Memory.nop, 16) 

