namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open FSharp.Collections
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL
   

type VertexAttrib =
    {
        size    : int
        typ     : float
        norm    : bool
        stride  : int
        offset  : int
    }

type IndexInfo =
    {
        typ : float
        offset : int
        size : int
    }

type PreparedRenderObject =
    {
        program             : Program
        uniformBuffers      : Map<int, IResource<UniformBuffer>>
        vertexBuffers       : Map<int, IResource<Buffer> * list<VertexAttrib>>
        indexBuffer         : Option<IResource<Buffer> * IndexInfo>
        mode                : float
        call                : IMod<DrawCall>
    }

module PreparedRenderObject =

    let update (t : AdaptiveToken) (o : PreparedRenderObject) =
        o.uniformBuffers |> Map.iter (fun _ b -> b.GetHandle(t) |> ignore)
        o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.GetHandle(t) |> ignore)
        o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.GetHandle t |> ignore)
        o.call.GetValue(t) |> ignore

    let acquire (o : PreparedRenderObject) =
        o.program.Acquire()
        o.uniformBuffers |> Map.iter (fun _ b -> b.Acquire())
        o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.Acquire())
        o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.Acquire())
        
    let release (o : PreparedRenderObject) =
        o.program.Release()
        o.uniformBuffers |> Map.iter (fun _ b -> b.Release())
        o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.Release())
        o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.Release())


    let render (o : PreparedRenderObject) =
        let gl = o.program.Context.GL


        gl.useProgram(o.program.Handle)
        
        // bind uniforms
        for (id, b) in Map.toSeq o.uniformBuffers do
            let b = b.GetHandle(AdaptiveToken.Top)
            gl.bindBufferBase(gl.UNIFORM_BUFFER, float id, b.Handle)

        // bind buffers
        for (id, (b, atts)) in Map.toSeq o.vertexBuffers do
            let b = b.GetHandle(AdaptiveToken.Top)
            gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
            let mutable id = id
            for att in atts do
                gl.enableVertexAttribArray(float id)
                gl.vertexAttribPointer(float id, float att.size, att.typ, att.norm, float att.stride, float att.offset)
                id <- id + 1
            gl.bindBuffer(gl.ARRAY_BUFFER, null)

        
        let call = o.call.GetValue AdaptiveToken.Top
        match o.indexBuffer with
        | Some (ib, info) ->
            let ib = ib.GetHandle(AdaptiveToken.Top)
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
            gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))
        | None ->
            gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)


[<AutoOpen>]
module Resources =

    module VertexAttrib =
        let rec ofType (gl : WebGL2RenderingContext) (t : PrimitiveType) =
            match t with
            | PrimitiveType.Bool           -> [ { size = 1; typ = gl.BOOL; norm = false; stride = 0; offset = 0 } ]
            | PrimitiveType.Int(true, 8)   -> [ { size = 1; typ = gl.BYTE; norm = false; stride = 0; offset = 0 }  ]
            | PrimitiveType.Int(false, 8)  -> [ { size = 1; typ = gl.UNSIGNED_BYTE; norm = false; stride = 0; offset = 0 } ]
            | PrimitiveType.Int(true, 16)  -> [ { size = 1; typ = gl.SHORT; norm = false; stride = 0; offset = 0 } ]
            | PrimitiveType.Int(false, 16) -> [ { size = 1; typ = gl.UNSIGNED_SHORT; norm = false; stride = 0; offset = 0 } ]
            | PrimitiveType.Int(true, 32)  -> [ { size = 1; typ = gl.INT; norm = false; stride = 0; offset = 0 } ]
            | PrimitiveType.Int(false, 32) -> [ { size = 1; typ = gl.UNSIGNED_INT; norm = false; stride = 0; offset = 0 } ]
            | PrimitiveType.Float(32)      -> [ { size = 1; typ = gl.FLOAT; norm = false; stride = 0; offset = 0 } ]

            | PrimitiveType.Vec(inner, d) ->
                match ofType gl inner with
                | [t] -> [ { t with size = d } ]
                | _ -> failwithf "[GL] bad vector type: %A" t
            | PrimitiveType.Mat(inner, r, c) ->
                match ofType gl inner with
                | [t] -> failwith "[GL] matrix attributes not implemented"
                | _ -> failwithf "[GL] bad matrix type: %A" t
            | _ ->
                failwithf "[GL] bad attribute type: %A" t


    type ResourceManager with
        member x.Prepare(signature : FramebufferSignature, o : RenderObject) =
            let gl = x.Context.GL

            let program = x.CreateProgram(signature, o.pipeline.shader)

            let uniformBuffers = 
                program.Interface.uniformBlocks |> Map.map (fun index block ->
                    x.CreateUniformBuffer(block, fun n -> Map.tryFind n o.pipeline.uniforms)
                )

            let vertexBuffers =
                program.Interface.attributes |> Map.map (fun index p ->
                    match Map.tryFind p.name o.vertexBuffers with
                    | Some b ->
                        let buffer = x.CreateBuffer(b.buffer)
                        let atts = VertexAttrib.ofType gl b.typ |> List.map (fun a -> { a with offset = a.offset + b.offset })

                        buffer, atts

                    | None ->
                        failwithf "[GL] could not get vertex attribute %s" p.name
                )

            let vertexAttributes =
                program.Interface.attributes |> Map.map (fun index p ->
                    match Map.tryFind p.name o.vertexBuffers with
                    | Some b ->
                        b.typ, b.offset
                    | None ->
                        failwithf "[GL] could not get vertex attribute %s" p.name
                            
                )

            let indexBuffer =
                match o.indexBuffer with
                | Some view ->
                    let b = x.CreateIndexBuffer(view.buffer)
                    let info =
                        match view.typ with
                        | Int(_, 32) -> { offset = view.offset; size = 4; typ = gl.UNSIGNED_INT }
                        | Int(_, 16) -> { offset = view.offset; size = 2; typ = gl.UNSIGNED_SHORT }
                        | Int(_, 8) -> { offset = view.offset; size = 1; typ = gl.UNSIGNED_BYTE }
                        | t -> failwithf "[GL] bad index type: %A" t
                    Some (b,info)
                | None ->
                    None

            let mode =
                match o.mode with
                | PrimitiveTopology.PointList -> gl.POINTS
                | PrimitiveTopology.LineList -> gl.LINES
                | PrimitiveTopology.LineStrip -> gl.LINE_STRIP
                | PrimitiveTopology.TriangleList -> gl.TRIANGLES
                | PrimitiveTopology.TriangleStrip -> gl.TRIANGLE_STRIP


            {
                program             = program
                uniformBuffers      = uniformBuffers
                indexBuffer         = indexBuffer
                vertexBuffers       = vertexBuffers
                mode                = mode
                call                = o.call
            }

