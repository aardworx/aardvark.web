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
    type Context with

        member x.DeleteBuffer(b : Buffer) =
            b.Release()

        member x.CreateBuffer(target : float, data : IBuffer) =
            match data with
            | :? HostBuffer as data ->
                Log.debug "create buffer"
                let b = x.GL.createBuffer()
                x.GL.bindBuffer(target, b)
                x.GL.bufferData(target, U3.Case2 data.Data.View, x.GL.STATIC_DRAW)
                Buffer(x, b)

            | :? Buffer as b ->
                b.Acquire()
                b

            | _ ->
                failwith "bad buffer"

        member x.CreateBuffer (target : float, arr : IArrayBuffer) =
            x.CreateBuffer(target, HostBuffer arr)

