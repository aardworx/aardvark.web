namespace Aardvark.Rendering.WebGL


open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Rendering
open Fable.Core
open Aardvark.Base


type Buffer(ctx : Context, handle : WebGLBuffer) =
    inherit Resource(ctx)
    member x.Handle = handle

    override x.Destroy() = 
        Log.debug "destroy buffer"
        ctx.GL.deleteBuffer(handle)
    override x.ToString() = string handle

    interface IBuffer


[<AutoOpen>]  
module BufferImpl = 
    
    let rand = System.Random()

    type WebGL2RenderingContext with
        member gl.Promise = 
            let fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0.0)
            gl.flush()
            let status = gl.getSyncParameter(fence, gl.SYNC_STATUS)
            if status = gl.SIGNALED then 
                gl.deleteSync fence
                Promise.resolve ()
            elif status <> gl.UNSIGNALED then 
                gl.deleteSync fence
                Promise.reject (sprintf "bad status: %A" status)
            else
                Promise.Create(fun ok error ->
                    let rec check() =
                        let status = gl.getSyncParameter(fence, gl.SYNC_STATUS)
                        if status = gl.SIGNALED then gl.deleteSync fence; ok()
                        elif status = gl.UNSIGNALED then setTimeout check (rand.NextDouble() * 5000.0 |> int) |> ignore
                        else gl.deleteSync fence; error (sprintf "bad status: %A" status)
                    check()
                ) |> unbox<Promise<unit>>

    type Context with

        member x.DeleteBuffer(b : Buffer) =
            b.Release()

        member x.CreateBuffer(target : float, data : IBuffer) =
            match data with
            | :? PromiseBuffer as p ->
                p.Promise |> Prom.bind (fun b -> x.CreateBuffer(target, b))
            | :? HostBuffer as data ->
                Log.debug "create buffer"
                let b = x.GL.createBuffer()
                x.GL.bindBuffer(target, b)
                x.GL.bufferData(target, U3.Case2 data.Data.View, x.GL.STATIC_DRAW)
                //x.GL.Promise |> Prom.map (fun () ->

                    //let maxWait = x.GL.getParameter(x.GL.MAX_CLIENT_WAIT_TIMEOUT_WEBGL) |> unbox<float>
                    //Log.error "maxWait: %A" maxWait
                    //let mutable s = x.GL.clientWaitSync(fence, x.GL.SYNC_FLUSH_COMMANDS_BIT, 0.0)
                    //while s <> x.GL.WAIT_FAILED && s <> x.GL.ALREADY_SIGNALED && s <> x.GL.CONDITION_SATISFIED do
                    //    s <- x.GL.clientWaitSync(fence, 0.0, 0.0)

                    //let status = x.GL.getSyncParameter(fence, x.GL.SYNC_STATUS)
                    //if status = x.GL.SIGNALED then Log.warn "signaled"
                    //elif status = x.GL.UNSIGNALED then Log.warn "unsignaled"
                    //else Log.warn "status: %A" status

                Prom.value (Buffer(x, b))
                //)

            | :? Buffer as b ->
                b.Acquire()
                Prom.value b

            | _ ->
                failwith "bad buffer"

        member x.CreateBuffer (target : float, arr : IArrayBuffer) =
            x.CreateBuffer(target, HostBuffer arr)

