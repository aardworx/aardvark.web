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
        samplers            : Map<int, WebGLUniformLocation * IResource<Texture> * Option<IResource<Sampler>>>
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
            yield! o.samplers |> Map.toSeq |> Seq.collect (fun (_,(_,b,s)) -> (b :> IResource) :: [match s with | Some s -> yield s | _ -> ()])
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
                yield! o.samplers  |> Map.toSeq |> Seq.collect (fun (_,(_,b, s)) -> b.Update(t) :: [match s with | Some s -> yield s.Update t | _ -> ()])
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
        o.samplers |> Map.iter (fun _ (_,b, s) -> b.Acquire(); match s with | Some s -> s.Acquire() | _ -> ())
        o.indexBuffer |> FSharp.Core.Option.iter (fun (b,_) -> b.Acquire())
        o.call.Acquire()
        o.depthMode.Acquire()

    let release (o : PreparedRenderObject) =
        o.program.Release()
        o.uniformBuffers |> Map.iter (fun _ b -> b.Release())
        o.uniforms |> HMap.iter (fun _ (_,b) -> b.Release())
        o.vertexBuffers |> Map.iter (fun _ (b,_) -> b.Release())
        o.samplers |> Map.iter (fun _ (_,b, s) -> b.Release(); match s with | Some s -> s.Release() | _ -> ())
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

        module GLSLType =
            let rec toString (t : GLSLType) =
                match t with
                | GLSLType.Array(len, t, _) -> sprintf "%s[%d]" (toString t) len
                | GLSLType.Bool -> "bool"
                | GLSLType.DynamicArray(t,_) -> sprintf "%s[]" (toString t)
                | GLSLType.Float b -> sprintf "float%d" b
                | GLSLType.Int(true, b) -> sprintf "int%d" b
                | GLSLType.Int(false, b) -> sprintf "uint%d" b
                | GLSLType.Image i -> "image"
                | GLSLType.Mat(c, r, Float 32) -> if c = r then sprintf "mat%d" r else sprintf "mat%dx%d" c r
                | GLSLType.Mat(c, r, Float 64) -> if c = r then sprintf "dmat%d" r else sprintf "mat%dx%d" c r
                | GLSLType.Mat(c, r, t) -> sprintf "%smat%dx%d" (toString t) c r
                | GLSLType.Sampler s -> 
                    let dimStr =
                        match s.dimension with
                            | SamplerDimension.Sampler1d -> "1D"
                            | SamplerDimension.Sampler2d -> "2D"
                            | SamplerDimension.Sampler3d -> "3D"
                            | SamplerDimension.SamplerCube -> "Cube"
                            | _ -> failwith "unsupported sampler dimension"

                    let shadowSuffix = if s.isShadow then "Shadow" else ""
                    let msSuffix = if s.isMS then "MS" else ""
                    let typePrefix = 
                        match s.valueType with
                            | Vec(_, Int _) -> "i"
                            | _ -> ""

                    if s.isArray then sprintf "%ssampler%s%sArray%s" typePrefix dimStr msSuffix shadowSuffix
                    else sprintf "%ssampler%s%s%s" typePrefix dimStr msSuffix shadowSuffix 
                        
                | GLSLType.Struct(name, fields, _) -> sprintf "struct %s { %s }" name (fields |> Seq.map (fun (n,t,_) -> sprintf "%s : %s" n (toString t)) |> FSharp.Core.String.concat "; ")
                | GLSLType.Vec(d, Float 32) -> sprintf "vec%d" d
                | GLSLType.Vec(d, Float 64) -> sprintf "dvec%d" d
                | GLSLType.Vec(d, Int(true, 32)) -> sprintf "ivec%d" d
                | GLSLType.Vec(d, Int(false, 32)) -> sprintf "uvec%d" d
                | GLSLType.Vec(d, Bool) -> sprintf "bvec%d" d
                | GLSLType.Vec(d, t) -> sprintf "%sx%d" (toString t) d
                | GLSLType.Void -> "void"
                
        let printShader (s : GLSLShader) =

            let lines = s.code.Split([|"\r\n"|], StringSplitOptions.None)
            let len = log10 (float lines.Length + 0.5) |> ceil |> int
            let code = 
                lines |> FSharp.Collections.Array.mapi (fun i l -> 
                    let id = string (1 + i)
                    let id = 
                        if id.Length < len then System.String(' ', len - id.Length) + id
                        else id
                    id + "  " + l
                ) |> FSharp.Core.String.concat "\n"



            console.groupCollapsed "interface"

            console.group "inputs"
            for i in s.iface.inputs do
                let typ = GLSLType.toString i.paramType
                if i.paramSemantic <> i.paramName then
                    console.log (sprintf "layout(location = %d) %s %s [%s]" i.paramLocation typ i.paramName i.paramSemantic)
                else
                    console.log (sprintf "layout(location = %d) %s %s" i.paramLocation typ i.paramName)
            console.groupEnd()

            
            console.group "uniforms"
            for (name, buf) in MapExt.toArray s.iface.uniformBuffers do
                console.group (sprintf "uniform %s // size: %d" name buf.ubSize)
                for f in buf.ubFields do    
                    let typ = GLSLType.toString f.ufType
                    console.log (sprintf "%s %s // offset: %d" typ f.ufName f.ufOffset)
                console.groupEnd()

            for (name, sam) in MapExt.toArray s.iface.samplers do
                let tex = 
                    match sam.samplerTextures with
                    | [name,_] -> name
                    | _ -> List.map fst sam.samplerTextures |> FSharp.Core.String.concat ", " |> sprintf "[%s]"

                let typ = GLSLType.toString (GLSLType.Sampler sam.samplerType)
                console.groupCollapsed (sprintf "%s %s (%s)" typ name tex)
                console.log("type", typ)
                console.log("name", "\"" + name + "\"")
                console.log("binding", sam.samplerBinding)
                console.log("count", sam.samplerCount)
                console.groupEnd()



            console.groupEnd()

            console.group "outputs"
            for i in s.iface.outputs do
                let typ = GLSLType.toString i.paramType
                if i.paramSemantic <> i.paramName then
                    console.log (sprintf "layout(location = %d) %s %s [%s]" i.paramLocation typ i.paramName i.paramSemantic)
                else
                    console.log (sprintf "layout(location = %d) %s %s" i.paramLocation typ i.paramName)
            console.groupEnd()

            
            console.groupCollapsed "shaders"
            for (stage, shader) in MapExt.toSeq s.iface.shaders do
                let name =
                   match stage with
                   | ShaderStage.Vertex -> "Vertex"
                   | ShaderStage.TessControl -> "TessControl"
                   | ShaderStage.TessEval -> "TessEval"
                   | ShaderStage.Geometry -> "Geometry"
                   | ShaderStage.Fragment -> "Fragment"
                   | _ -> "Compute"
                console.group name
                
                console.group "inputs"
                match MapExt.tryFind Imperative.ParameterKind.Input shader.shaderBuiltIns with
                | Some inputs ->
                    for (name, typ) in MapExt.toSeq inputs do
                        let typ = GLSLType.toString typ
                        console.log (sprintf "builtin %s %s" typ name)
                | None -> ()
                for i in shader.shaderInputs do
                    let typ = GLSLType.toString i.paramType

                    let loc =
                        if stage = ShaderStage.Vertex then sprintf "layout(location = %d) " i.paramLocation 
                        else ""
                    if i.paramSemantic <> i.paramName then
                        console.log (sprintf "%s%s %s [%s]" loc typ i.paramName i.paramSemantic)
                    else
                        console.log (sprintf "%s%s %s" loc typ i.paramName)
                    
                console.groupEnd()

                console.group "outputs"
                match MapExt.tryFind Imperative.ParameterKind.Output shader.shaderBuiltIns with
                | Some inputs ->
                    for (name, typ) in MapExt.toSeq inputs do
                        let typ = GLSLType.toString typ
                        console.log (sprintf "builtin %s %s" typ name)
                | None -> ()
                for i in shader.shaderOutputs do
                    let typ = GLSLType.toString i.paramType
                    
                    let loc =
                        if stage = ShaderStage.Fragment then sprintf "layout(location = %d) " i.paramLocation 
                        else ""

                    if i.paramSemantic <> i.paramName then
                        console.log (sprintf "%s%s %s [%s]" loc typ i.paramName i.paramSemantic)
                    else
                        console.log (sprintf "%s%s %s" loc typ i.paramName)
                    
                console.groupEnd()

                console.groupEnd()
            console.groupEnd()
            
            
            console.groupEnd()
            
            console.group "code"
            console.log code
            console.groupEnd()
            

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
                    


                let str = if glsl300 then "GLES300" else "GLES100"
                Log.start "compiled %s (%d)" str glsl.code.Length
                printShader glsl
                Log.stop()

                glsl
            )

        


    type ResourceManager with
        member x.Prepare(signature : FramebufferSignature, o : RenderObject) =
            let gl = x.Context.GL
            let mutable failed = false

            let shader = ShaderCompiler.compile gl.IsGL2 signature o.pipeline.shader


            let program = x.CreateProgram(signature, shader.code)


            let uniformBuffers = 
                program.Interface.uniformBlocks |> Map.map (fun index block ->
                    x.CreateUniformBuffer(block, o.pipeline.uniforms)
                )

            let vertexBuffers =
                program.Interface.attributes |> Map.choose (fun index p ->
                    match Map.tryFind p.name o.vertexBuffers with
                    | Some b ->
                        let buffer = x.CreateBuffer(b.buffer)
                        let atts = VertexAttrib.ofType gl b.typ |> List.map (fun a -> { a with offset = a.offset + b.offset })

                        Some (buffer, atts)

                    | None ->
                        Log.error "[GL] could not get vertex attribute %s" p.name
                        None
                )

            let samplers =
                program.Interface.samplers |> Map.toSeq |> Seq.choose (fun (name, location) ->
                    let semantic, samplerState = 
                        match MapExt.tryFind name shader.iface.samplers with
                        | Some sam -> 
                            match sam.samplerTextures with
                            | [(name, state)] -> name, state
                            | _ -> 
                                Log.error "[GL] texture arrays not implemented"
                                name, FShade.SamplerState.empty
                        | None ->
                            name, FShade.SamplerState.empty
                    match o.pipeline.uniforms semantic with
                    | Some m ->
                        if x.IsGL2 then
                            let anisotropic = match samplerState.MaxAnisotropy with | Some a -> a > 1 | None -> false
                            if anisotropic then
                                Log.warn "duplicated texture due to anisotropic filtering: https://github.com/KhronosGroup/WebGL/issues/2006"
                                let tex = x.CreateSampledTexture(unbox m, samplerState)
                                Some (location, tex, None)
                            else
                                let tex = x.CreateTexture(unbox m)
                                let sam = x.CreateSampler(Mod.constant samplerState)
                                Some (location, tex, Some sam)
                        else
                            let tex = x.CreateSampledTexture(unbox m, samplerState)
                            Some (location, tex, None)
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
                        | t ->
                            Log.error "[GL] bad index type: %A" t
                            failed <- true
                            { offset = view.offset; size = 0; typ = gl.UNSIGNED_INT }
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

            if not failed then
                Some {
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
            else
                None

