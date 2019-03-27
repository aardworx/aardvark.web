module Program

open System
open Aardvark.Base
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Incremental

type Sepp<'a> =
    struct
        val mutable public A : 'a
        new(a) = { A = a }
    end


type V2fArray(len : int) =
    let store = Float32Array.Create(float (2 * len))

    member x.length = len

    member x.Item
        with get(i : int) = 
            let b = 2 * i
            V2d(store.[b], store.[b + 1])
        
        and set(i : int) (v : V2d) = 
            let b = 2 * i
            store.[b] <- v.X
            store.[b+1] <- v.Y

    member x.SetByIndex (value : int -> V2d) =
        for i in 0 .. len - 1 do
            x.[i] <- value i


let test (s : Sepp<int>) =
    let mutable s2 = s
    s2.A <- 10
    s2



type WebGLVertexArrayObject =
    interface end

type WebGL2RenderingContext =
    inherit WebGLRenderingContext
    abstract member UNIFORM_BUFFER : float
    abstract member bindBufferRange : target : float * index : float * buffer : WebGLBuffer * offset : float * size : float -> unit
    abstract member bindBufferBase : target : float * index : float * buffer : WebGLBuffer -> unit

    abstract member createVertexArray : unit -> WebGLVertexArrayObject
    abstract member deleteVertexArray : WebGLVertexArrayObject -> unit
    abstract member bindVertexArray : WebGLVertexArrayObject -> unit

type Context(gl : WebGL2RenderingContext) =
    let mutable id = 1

    static let versionRx = System.Text.RegularExpressions.Regex @"\#version.*(\r\n|\r|\n)"

    member x.GL = gl

    member x.NewId() = 
        let r = id
        id <- r + 1
        r

    member x.CompileShader (stage : float, code : string) =
        let def =
            if stage = gl.VERTEX_SHADER then "VERTEX"
            elif stage = gl.FRAGMENT_SHADER then "FRAGMENT"
            else "UNKNOWN"

        let code = versionRx.Replace(code, fun m -> m.Value + "#define " + def + "\r\n")

        let shader = gl.createShader(stage)
        gl.shaderSource(shader, code)
        gl.compileShader(shader)
        
        let s = gl.getShaderParameter(shader, gl.COMPILE_STATUS) |> unbox<int>
        if s = 0 then
            let log = gl.getShaderInfoLog(shader)
            console.warn s
            console.warn log
            None
        else
            Some shader

    member x.CreateProgram(code : string) =

        match x.CompileShader(gl.VERTEX_SHADER, code), x.CompileShader(gl.FRAGMENT_SHADER, code) with
        | Some vs, Some fs ->
            let p = gl.createProgram()
            gl.attachShader(p, vs)
            gl.attachShader(p, fs)
            gl.linkProgram(p)

            let status = gl.getProgramParameter(p, gl.LINK_STATUS) |> unbox<int>
            if status <> 0 then
            
                Some p
            else
                let log = gl.getProgramInfoLog(p)
                console.warn log
                None
        | _ ->
            None

[<AutoOpen>]  
module BufferImpl = 

    type IArray =
        abstract member buffer : ArrayBuffer

    type V2fArray(len : int) =
        let store = Float32Array.Create(float (2 * len))

        member x.Length = len
        member x.Item
            with get(i : int) =
                let i = 2 * i
                V2d(store.[i], store.[i+1])
            and set (i : int) (value : V2d) =
                let i = 2 * i
                store.[i] <- value.X
                store.[i+1] <- value.Y
        
        interface IArray with
            member x.buffer = store.buffer

        static member init (count : int) (f : int -> V2d) =
            let res = V2fArray(count)
            for i in 0 .. count - 1 do
                res.[i] <- f i
            res
        
        static member ofArray (arr : V2d[]) =
            let res = V2fArray(arr.Length)
            for i in 0 .. arr.Length - 1 do
                res.[i] <- arr.[i]
            res

    type V3fArray(len : int) =
        let store = Float32Array.Create(float (3 * len))

        member x.Length = len
        member x.Item
            with get(i : int) =
                let i = 3 * i
                V3d(store.[i], store.[i+1], store.[i+2])
            and set (i : int) (value : V3d) =
                let i = 3 * i
                store.[i] <- value.X
                store.[i+1] <- value.Y
                store.[i+2] <- value.Z
                
        interface IArray with
            member x.buffer = store.buffer
            
        static member init (count : int) (f : int -> V3d) =
            let res = V3fArray(count)
            for i in 0 .. count - 1 do
                res.[i] <- f i
            res
            
        static member ofArray (arr : V3d[]) =
            let res = V3fArray(arr.Length)
            for i in 0 .. arr.Length - 1 do
                res.[i] <- arr.[i]
            res

    type V4fArray(len : int) =
        let store = Float32Array.Create(float (4 * len))

        member x.Length = len
        member x.Item
            with get(i : int) =
                let i = 4 * i
                V4d(store.[i], store.[i+1], store.[i+2], store.[i+3])
            and set (i : int) (value : V4d) =
                let i = 4 * i
                store.[i] <- value.X
                store.[i+1] <- value.Y
                store.[i+2] <- value.Z
                store.[i+3] <- value.W
                        
        interface IArray with
            member x.buffer = store.buffer


    type IBuffer = interface end

    [<AbstractClass>]
    type Resource(ctx : Context) =
        let id = ctx.NewId()
        let mutable refCount = 1
        
        member x.Acquire() = 
            refCount <- refCount + 1

        member x.Release() = 
            refCount <- refCount - 1
            if refCount = 0 then x.Destroy()


        abstract member Destroy : unit -> unit

        member x.Id = id
        override x.GetHashCode() = id
        override x.Equals o =
            match o with
            | :? Resource as o -> id = o.Id
            | _ -> false

        
        interface IComparable with
            member x.CompareTo o =
                match o with
                | :? Resource as o -> compare id o.Id
                | _ -> failwith "uncomparable"

    type HostBuffer(data : ArrayBuffer) =
        interface IBuffer
        member x.Data = data

        new(arr : IArray) = HostBuffer(arr.buffer)

    type Buffer(ctx : Context, handle : WebGLBuffer) =
        inherit Resource(ctx)
        member x.Handle = handle

        override x.Destroy() = ctx.GL.deleteBuffer(handle)
        override x.ToString() = string handle

        interface IBuffer

    type Context with

        member x.DeleteBuffer(b : Buffer) =
            b.Release()

        member x.CreateBuffer(data : IBuffer) =
            match data with
            | :? HostBuffer as data ->
                let b = x.GL.createBuffer()
                x.GL.bindBuffer(x.GL.ARRAY_BUFFER, b)
                x.GL.bufferData(x.GL.ARRAY_BUFFER, U3.Case3 data.Data, x.GL.STATIC_DRAW)
                Buffer(x, b)

            | :? Buffer as b ->
                b.Acquire()
                b

            | _ ->
                failwith "bad buffer"

        member x.CreateBuffer (arr : IArray) =
            x.CreateBuffer(HostBuffer arr.buffer)


//type WebGLRenderingContext with
//    member x.UNIFORM_BUFFER = 35345.0

//    [<Emit("$0.bindBufferRange($1, $2, $3, $4, $5)")>]
//    member x.bindBufferRange(target : float, index : float, buffer : WebGLBuffer, offset : float, size : float) = jsNative

type IRenderTask =  
    inherit System.IDisposable
    abstract member Run : V2i -> unit

module RenderTask =
    let empty = 
        { new IRenderTask with
            member x.Run _ = ()
            member x.Dispose() = ()
        }

type MouseButtons =
    | Left = 1
    | Middle = 2
    | Right = 3
    | Button4 = 4
    | Button5 = 5

type Subject<'a>() =
    let mutable id = 0
    let mutable observers = System.Collections.Generic.Dictionary<int, IObserver<'a>>()

    member x.OnNext(value : 'a) =
        if not (isNull observers) then
            for o in observers.Values do
                o.OnNext(value)

    member x.OnCompleted() =
        if not (isNull observers) then
            for o in observers.Values do
                o.OnCompleted()

    member x.OnError(err : exn) =
        if not (isNull observers) then
            for o in observers.Values do
                o.OnError(err)

    member x.Dispose() =
        if not (isNull observers) then
            observers.Clear()
            observers <- null
            
    member x.Subscribe(obs : IObserver<'a>) =
        if not (isNull observers) then
            let i = id
            id <- i + 1
            observers.[i] <- obs
            { new IDisposable with member x.Dispose() = observers.Remove i |> ignore }
        else
            { new IDisposable with member x.Dispose() = () }

    interface IObservable<'a> with
        member x.Subscribe(obs : IObserver<'a>) = x.Subscribe(obs)

    interface IDisposable with
        member x.Dispose() = x.Dispose()

type Mouse(c : HTMLElement) =

    let down = new Subject<MouseButtons * V2d>()
    let up = new Subject<MouseButtons * V2d>()
    let move = new Subject<V2i>()
    let wheel = new Subject<float>()

    static let createHandler (handler : 'e -> unit) = U2.Case2 { new EventListenerObject with member x.handleEvent e = let e = unbox<'e> e in handler e }

    let downHandler     = createHandler (fun (e : MouseEvent) ->        down.OnNext(unbox (int e.which), V2d(e.clientX, e.clientY)); e.preventDefault(); e.stopPropagation() )
    let upHandler       = createHandler (fun (e : MouseEvent) ->        up.OnNext(unbox (int e.which), V2d(e.clientX, e.clientY)); e.preventDefault(); e.stopPropagation())
    let moveHandler     = createHandler (fun (e : MouseEvent) ->        move.OnNext(V2i(int e.clientX, int e.clientY)); e.preventDefault(); e.stopPropagation())
    let wheelHandler    = createHandler (fun (e : MouseWheelEvent) ->   wheel.OnNext(e.wheelDelta / 120.0); e.preventDefault(); e.stopPropagation())
    let contextHandler  = createHandler (fun (e : Event) ->             e.preventDefault())

    do
        c.addEventListener("mousedown", downHandler, true)
        c.addEventListener("mouseup", upHandler, true)
        c.addEventListener("mousemove", moveHandler, true)
        c.addEventListener("mousewheel", wheelHandler, true)
        c.addEventListener("contextmenu", contextHandler)

    member x.Down = down :> IObservable<_>
    member x.Up = up :> IObservable<_>
    member x.Move = move :> IObservable<_>
    member x.Wheel = wheel :> IObservable<_>

    member x.Dispose() =
        c.removeEventListener("mousedown", downHandler, true)
        c.removeEventListener("mouseup", upHandler, true)
        c.removeEventListener("mousemove", moveHandler, true)
        c.removeEventListener("mousewheel", wheelHandler, true)
        c.removeEventListener("contextmenu", contextHandler)
        down.Dispose()
        up.Dispose()
        move.Dispose()
        wheel.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

type RenderView(c : HTMLCanvasElement) =
    
    let mutable task = RenderTask.empty
    let mouse = new Mouse(c)

    member x.Mouse = mouse
    member x.RenderTask
        with get() = 
            task
        and set t = 
            task.Dispose()
            task <- t


    member x.Dispose() =
        task.Dispose()
        task <- RenderTask.empty
        mouse.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()


let seop() =
    Fable.Import.JS.Promise.Create(fun res err ->
        let req = XMLHttpRequest.Create()
        req.responseType <- "arraybuffer"

        req.``open``("GET", "https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Sending_and_Receiving_Binary_Data")
        req.onload <- fun e -> res req.response
        req.send("")
    ) |> unbox<Promise<ArrayBuffer>>

let testy() =
    let a = Mod.init 10
    let b = Mod.init 10
    let c = a |> Mod.bind (fun a -> if a < 11 then b |> Mod.map ((+) 2) else Mod.constant a)


    console.warn(Mod.force c)
    transact (fun () -> a.Value <- 100)
    console.warn(Mod.force c)
    transact (fun () -> b.Value <- 100)
    console.warn(c.OutOfDate)
    console.warn(Mod.force  c)
    
    transact (fun () -> a.Value <- 25)
    console.warn(Mod.force  c)

    let test = [1;2;3;4]
    let x = Mod.constant test :> obj
    let y = Mod.constant test :> obj
    console.warn("a: " + (string (x.GetHash())))
    console.warn("b: " + (string (y.GetHash())))


[<EntryPoint>]
let main argv =
    testy()

    document.addEventListener_readystatechange(fun e ->
        if document.readyState = "complete" then
            let canvas = document.createElement_canvas()
            document.body.appendChild(canvas) |> ignore
            document.body.style.margin <- "0"
            document.body.style.padding <- "0"

            canvas.style.width <- "100%"
            canvas.style.height <- "100%"
            
            let gl = canvas.getContext("webgl2") |> unbox<WebGL2RenderingContext>
            let ctx = Context(gl)


            let pos =   
                V3fArray.ofArray [|
                    V3d(-1.0, -1.0, 0.0)
                    V3d(1.0, -1.0, 0.0)
                    V3d(1.0, 1.0, 0.0)

                    
                    V3d(-1.0, -1.0, 0.0)
                    V3d(1.0, 1.0, 0.0)
                    V3d(-1.0, 1.0, 0.0)
                |]
                
                
            let tc =   
                V2fArray.ofArray [|
                    V2d(0.0, 0.0)
                    V2d(1.0, 0.0)
                    V2d(1.0, 1.0)
                             
                    V2d(0.0, 0.0)
                    V2d(1.0, 1.0)
                    V2d(0.0, 1.0)
                |]
                

            let b = ctx.CreateBuffer pos
            let tcb = ctx.CreateBuffer tc

            //let b = gl.createBuffer()
            //gl.bindBuffer(gl.ARRAY_BUFFER, b)
            //gl.bufferData(gl.ARRAY_BUFFER, U3.Case3 pos.buffer, gl.STATIC_DRAW)
            //gl.bindBuffer(gl.ARRAY_BUFFER, null)

            let shader =
                """#version 300 es

                    #ifdef VERTEX

                    uniform View {
                        mat4 MVP;
                    };

                    layout(location = 0) in vec3 pos;
                    layout(location = 1) in vec2 tc;
                    out vec2 cc;
                    void main() {
                        gl_Position = vec4(pos, 1.0) * MVP;
                        cc = tc;
                    }
                    #endif

                    #ifdef FRAGMENT
                    precision highp float;
                    precision highp int;
                    in vec2 cc;
                    layout(location = 0) out vec4 color;
                    void main() {
                        color = vec4(cc.x,cc.y,1,1);
                    }
                    #endif
                """

            let p = ctx.CreateProgram(shader)


            //let set = System.Collections.Generic.HashSet<obj>()

            //do 
            //    let a = obj()
            //    let b = obj()

            //    console.log (set.Add a)
            //    console.log (set.Add b)
            //    console.log (set.Remove a)
            //    console.log (set.Remove b)
            //    console.log (set.Remove a)
            //    console.log (set.Remove b)


            //let q = System.Collections.Generic.List<int>()
            //let cmp = compare
            //q.HeapEnqueue(cmp, 10)
            //q.HeapEnqueue(cmp, 4)
            //q.HeapEnqueue(cmp, 2)
            //q.HeapEnqueue(cmp, 123)
            //q.HeapEnqueue(cmp, 311)
            //q.HeapEnqueue(cmp, 12)
            //q.HeapEnqueue(cmp, 3)
            //q.HeapEnqueue(cmp, 1999)


            //while q.Count > 0 do
            //    console.warn(q.HeapDequeue(cmp))

            let m = M44d.RotationZ(0.3)

            let ub = gl.createBuffer()
            gl.bindBuffer(gl.UNIFORM_BUFFER, ub)
            gl.bufferData(gl.UNIFORM_BUFFER, U3.Case3 (m.ToFloat32Array().buffer), gl.DYNAMIC_DRAW)
            gl.bindBuffer(gl.UNIFORM_BUFFER, null)

            //let vao = gl.createVertexArray()
            //do
            //    let i = Index.after Index.zero
            //    let j = Index.after i
            //    let b = Index.between i j
            //    console.warn (i < b)
            //    console.warn (b < j)
            //    console.warn (i < j)
            //    console.warn ((i = b))
            //    console.warn ((b = j))
            //    console.warn ((i = j))
            //    console.warn ((i > b))
            //    console.warn ((b > j))
            //    console.warn ((i > j))

            let view = new RenderView(canvas)
            //view.Dispose()

            let mutable down = Set.empty

            view.Mouse.Down.Add (fun (b, p) ->
                down <- Set.add b down
                console.log (sprintf "down %A %A" b p)
            )
            view.Mouse.Up.Add (fun (b, p) ->
                down <- Set.remove b down
                console.log (sprintf "up %A %A" b p)
            )
            view.Mouse.Move.Add (fun (p) ->
                if not (Set.isEmpty down) then
                    console.log (sprintf "drag %A" p)
            )
            view.Mouse.Wheel.Add (fun (p) ->
                console.log (sprintf "wheel %A" p)
            )
            let mutable t0 = None
            let mutable angle = 0.0
            let rec render() =
                let t = performance.now()
                match t0 with
                | Some tb ->
                    let dt = t - tb
                    //console.log(dt)
                    t0 <- Some t
                    angle <- angle + 0.001 * dt
                | None ->
                    t0 <- Some t
                
                let rect = canvas.getBoundingClientRect()

                

                let mvp =
                    let m = Trafo3d.RotationZ(angle)
                    let v = CameraView.lookAt (V3d.III * 6.0) V3d.Zero V3d.OOI |> CameraView.viewTrafo
                    let p = Frustum.perspective 60.0 0.1 100.0 (rect.width / rect.height) |> Frustum.projTrafo
                    m * v * p

                gl.bindBuffer(gl.UNIFORM_BUFFER, ub)
                gl.bufferSubData(gl.UNIFORM_BUFFER, 0.0, U2.Case2 (mvp.Forward.ToFloat32Array().buffer))
                gl.bindBuffer(gl.UNIFORM_BUFFER, null)



                if canvas.width <> rect.width then canvas.width <- rect.width
                if canvas.height <> rect.height then canvas.height <- rect.height
                //console.log(sprintf "%.0fx%.0f" rect.width rect.height)
                gl.viewport(0.0, 0.0, rect.width, rect.height)
                gl.clearColor(0.0, 0.0, 0.0, 1.0)
                gl.clearDepth(1.0)
                gl.clear(float (int gl.COLOR_BUFFER_BIT ||| int gl.DEPTH_BUFFER_BIT))

                gl.useProgram(p.Value)
                gl.enableVertexAttribArray(0.0)
                gl.enableVertexAttribArray(1.0)

                gl.bindBufferBase(gl.UNIFORM_BUFFER, 0.0, ub)

                gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
                gl.vertexAttribPointer(0.0, 3.0, gl.FLOAT, false, 0.0, 0.0)
                gl.bindBuffer(gl.ARRAY_BUFFER, tcb.Handle)
                gl.vertexAttribPointer(1.0, 2.0, gl.FLOAT, false, 0.0, 0.0)

                gl.drawArrays(gl.TRIANGLES, 0.0, 6.0)

                gl.bindBuffer(gl.ARRAY_BUFFER, null)
                gl.bindBufferBase(gl.UNIFORM_BUFFER, 0.0, null)

                gl.disableVertexAttribArray(1.0)
                gl.disableVertexAttribArray(0.0)
                gl.useProgram(null)

                setTimeout render 16 |> ignore
                //window.requestAnimationFrame(render) |> ignore

            render()

    )



    0 // return an integer exit code
