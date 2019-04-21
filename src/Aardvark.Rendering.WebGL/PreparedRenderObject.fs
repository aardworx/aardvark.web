namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open FSharp.Collections
open Aardvark.Base.Rendering
open Aardvark.Rendering.WebGL
open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections


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

[<CustomEquality;NoComparison>]
type PreparedRenderObject =
    {
        id                  : int
        program             : Program
        uniformBuffers      : Map<int, IResource<UniformBuffer>>
        vertexBuffers       : Map<int, IResource<Buffer> * list<VertexAttrib>>
        samplers            : Map<int, WebGLUniformLocation * IResource<Texture>>
        uniforms            : hmap<WebGLUniformLocation, PrimitiveType * IResource<UniformLocation>>
        indexBuffer         : Option<IResource<Buffer> * IndexInfo>
        mode                : float
        depthMode           : IResource<Option<float>>
        call                : IResource<DrawCall>
    }

    override x.GetHashCode() = x.id
    override x.Equals o =
        match o with
        | :? PreparedRenderObject as o -> x.id = o.id
        | _ -> false
    //interface IComparable with
    //    member x.CompareTo o =
    //        match o with
    //        | :? PreparedRenderObject as o -> compare x.id o.id
    //        | _ -> failwith "uncomparable"


module PreparedRenderObject =

    let resources (o : PreparedRenderObject) =
        seq {
            yield! o.uniformBuffers |> Map.toSeq |> Seq.map (fun (_,b) -> b :> IResource)
            yield! o.uniforms |> Seq.map (fun (_,(_,l)) -> l :> IResource)
            yield! o.vertexBuffers |> Map.toSeq |> Seq.map (fun (_,(b,_)) -> b :> IResource)
            yield! o.samplers |> Map.toSeq |> Seq.map (fun (_,(_,b)) -> b :> IResource)
            match o.indexBuffer with
            | Some(ib,_) -> yield ib :> IResource
            | None -> ()

            yield o.depthMode :> IResource
            yield o.call :> IResource

        }

    let update (t : AdaptiveToken) (o : PreparedRenderObject) =
        let all = 
            seq {
                yield! o.uniformBuffers |> Map.toSeq |> Seq.map (fun (_,b) -> b.Update t)
                yield! o.uniforms |> HMap.toSeq |> Seq.map (fun (_,(_,b)) -> b.Update t)
                yield! o.vertexBuffers  |> Map.toSeq |> Seq.map (fun (_,(b,_)) -> b.Update t)
                yield! o.samplers  |> Map.toSeq |> Seq.map (fun (_,(_,b)) -> b.Update t)
                match o.indexBuffer with
                | Some(ib,_) -> yield ib.Update(t)
                | None -> ()
                yield o.call.Update(t)
                yield o.depthMode.Update t
            }

        Prom.all all |> unbox<Promise<unit>>

    let acquire (o : PreparedRenderObject) =
        o.program.Acquire()
        o.uniformBuffers |> Map.iter (fun _ b -> b.Acquire())
        o.uniforms |> HMap.iter (fun _ (_,b) -> b.Acquire())
        o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.Acquire())
        o.samplers |> Map.iter (fun _ (_,b) -> b.Acquire())
        o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.Acquire())
        o.call.Acquire()
        o.depthMode.Acquire()

    let release (o : PreparedRenderObject) =
        o.program.Release()
        o.uniformBuffers |> Map.iter (fun _ b -> b.Release())
        o.uniforms |> HMap.iter (fun _ (_,b) -> b.Release())
        o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.Release())
        o.samplers |> Map.iter (fun _ (_,b) -> b.Release())
        o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.Release())
        o.call.Release()
        o.depthMode.Release()


    //let render (o : PreparedRenderObject) =
    //    let gl = o.program.Context.GL

    //    gl.useProgram(o.program.Handle)
        
    //    match o.depthMode.Handle.Value with
    //    | Some m ->
    //        gl.enable(gl.DEPTH_TEST)
    //        gl.depthFunc(m)
    //    | None ->
    //        gl.disable(gl.DEPTH_TEST)


    //    // bind uniforms
    //    for (id, b) in Map.toSeq o.uniformBuffers do
    //        let b = b.Handle.Value
    //        gl.bindBufferBase(gl.UNIFORM_BUFFER, float id, b.Handle)

    //    // bind buffers
    //    for (id, (b, atts)) in Map.toSeq o.vertexBuffers do
    //        let b = b.Handle.Value
    //        gl.bindBuffer(gl.ARRAY_BUFFER, b.Handle)
    //        let mutable id = id
    //        for att in atts do
    //            gl.enableVertexAttribArray(float id)
    //            gl.vertexAttribPointer(float id, float att.size, att.typ, att.norm, float att.stride, float att.offset)
    //            id <- id + 1
    //        gl.bindBuffer(gl.ARRAY_BUFFER, null)



        
    //    let call = o.call.Handle.Value
    //    match o.indexBuffer with
    //    | Some (ib, info) ->
    //        let ib = ib.Handle.Value
    //        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ib.Handle)
    //        gl.drawElements(o.mode, float call.faceVertexCount, info.typ, float (info.offset + call.first * info.size))
    //    | None ->
    //        gl.drawArrays(o.mode, float call.first, float call.faceVertexCount)


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


    module ShaderCompiler =
        open FShade
        open FShade.GLSL

        let private cache = Dict<string * bool * FramebufferSignature, GLSLShader>(Unchecked.hash, Unchecked.equals)

        let compile (glsl300 : bool) (signature : FramebufferSignature) (effect : Effect) =
            let key = effect.Id, glsl300, signature
            cache.GetOrCreate(key, fun _ ->
                let attachments =
                    signature.Colors |> Map.toSeq |> Seq.map (fun (slot, name) ->
                        name, (typeof<V4d>, slot)
                    )
                    |> Map.ofSeq

                //let attachments = 
                //    if signature.Depth then
                //        attachments |> Map.add DefaultSemantic.Depth (typeof<float>, -1)
                //    else
                //        attachments

                let cfg = 
                    {
                        depthRange = V2d(-1.0, 1.0)
                        flipHandedness = false
                        outputs = attachments
                        lastStage = ShaderStage.Fragment
                    }
                let module_ =
                    effect
                    |> Effect.toModule cfg

                let glsl = 
                    if glsl300 then ModuleCompiler.compileGLES300 module_
                    else ModuleCompiler.compileGLES100 module_
                    
                Log.line "%s" glsl.code

                glsl
            )

        


    type ResourceManager with
        member x.Prepare(signature : FramebufferSignature, o : RenderObject) =
            let gl = x.Context.GL

            let shader = ShaderCompiler.compile gl.IsGL2 signature o.pipeline.shader


            let program = x.CreateProgram(signature, shader.code)


            let uniformBuffers = 
                program.Interface.uniformBlocks |> Map.map (fun index block ->
                    x.CreateUniformBuffer(block, o.pipeline.uniforms)
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

            let samplers =
                program.Interface.samplers |> Map.toSeq |> Seq.choose (fun (name, location) ->
                    let semantic, samplerState = 
                        match MapExt.tryFind name shader.iface.samplers with
                        | Some sam -> 
                            match sam.samplerTextures with
                            | [(name, state)] -> name, state
                            | _ -> failwith "[GL] texture arrays not implemented"
                        | None ->
                            name, FShade.SamplerState.empty
                    match o.pipeline.uniforms semantic with
                    | Some m ->
                        let tex = x.CreateTexture(unbox m, samplerState)
                        Some (location, tex)
                    | None ->
                        None
                )
                |> Seq.mapi (fun i v -> i, v)
                |> Map.ofSeq

            let uniforms =
                program.Interface.uniforms |> Map.toSeq |> Seq.choose (fun (name, (location, typ)) ->
                    match o.pipeline.uniforms name with
                    | Some m ->
                        let l = x.CreateUniformLocation(typ, m)
                        Some (location, (typ, l))
                    | None ->
                        None
                )
                |> HMap.ofSeq


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
                | _ -> gl.POINTS

            let depthMode = x.CreateDepthMode(o.pipeline.depthMode)

            {
                id                  = newId()
                program             = program
                uniformBuffers      = uniformBuffers
                uniforms            = uniforms
                indexBuffer         = indexBuffer
                samplers            = samplers
                vertexBuffers       = vertexBuffers
                mode                = mode
                depthMode           = depthMode
                call                = x.CreateDrawCall(o.call)
            }

