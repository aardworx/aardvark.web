namespace Aardvark.Rendering.WebGL

open Aardvark.Import.Browser
open Aardvark.Import.JS
open FSharp.Collections
open Aardvark.Base.Rendering
open Aardvark.Base

type ProgramParameter =
    {
        name    : string
        typ     : PrimitiveType
        size    : int
    }

type UniformField =
    {
        name        : string
        offset      : int
        stride      : int
        size        : int
        rowMajor    : bool
        typ         : PrimitiveType
    }

type UniformBlockInfo =
    {
        index           : int
        name            : string
        size            : int
        fields          : list<UniformField>
        fieldsByName    : Map<string, UniformField>
    }

type ProgramInterface =
    {
        attributes      : Map<int, ProgramParameter>
        uniforms        : Map<string, WebGLUniformLocation * PrimitiveType>
        samplers        : Map<string, WebGLUniformLocation>
        uniformBlocks   : Map<int, UniformBlockInfo>
    }

type Program(ctx : Context, handle : WebGLProgram, iface : ProgramInterface) =
    inherit Resource(ctx)
    member x.Handle = handle
    member x.Interface = iface

    override x.Destroy() = 
        Log.debug "destroy program"
        ctx.GL.deleteProgram handle

[<AutoOpen>]
module ProgramImpl = 
    module PrimitiveType =
        let ofGLType (gl : WebGL2RenderingContext) (t : float) =
            if t = gl.BOOL then Bool
            elif t = gl.BYTE then Int(true, 8)
            elif t = gl.UNSIGNED_BYTE then Int(false, 8)
            elif t = gl.SHORT then Int(true, 16)
            elif t = gl.UNSIGNED_SHORT then Int(false, 16)
            elif t = gl.INT then Int(true, 32)
            elif t = gl.UNSIGNED_INT then Int(false, 32)
            elif t = gl.FLOAT then Float(32)

            elif t = gl.INT_VEC2 then Vec(Int(true, 32), 2)
            elif t = gl.INT_VEC3 then Vec(Int(true, 32), 3)
            elif t = gl.INT_VEC4 then Vec(Int(true, 32), 4)

            elif t = gl.BOOL_VEC2 then Vec(Bool, 2)
            elif t = gl.BOOL_VEC3 then Vec(Bool, 3)
            elif t = gl.BOOL_VEC4 then Vec(Bool, 4)

            elif t = gl.FLOAT_VEC2 then Vec(Float 32, 2)
            elif t = gl.FLOAT_VEC3 then Vec(Float 32, 3)
            elif t = gl.FLOAT_VEC4 then Vec(Float 32, 4)
            
            elif t = gl.FLOAT_MAT2 then Mat(Float 32, 2, 2)
            elif t = gl.FLOAT_MAT3 then Mat(Float 32, 3, 3)
            elif t = gl.FLOAT_MAT4 then Mat(Float 32, 4, 4)

            else failwithf "invalid type: %A" t

    type Tree (fields : list<UniformField>, children : MapExt<string, Tree>) =
        member x.Fields = fields
        member x.Children = children

    module Tree =
        let empty = Tree([], MapExt.empty)

        let private getKey (f : UniformField) =
            let arr = f.name.Split([| '.' |]) 
            if arr.Length > 0 then
                List.ofArray arr.[0 .. arr.Length - 2]
            else
                []
                
        let private getName (f : UniformField) =
            let arr = f.name.Split([| '.' |]) 
            if arr.Length > 0 then
                arr.[arr.Length-1]
            else
                f.name

        let add (f : UniformField) (t : Tree) =
            let key = getKey f
            let rec add (t : Tree) (key : list<string>) (f : UniformField) =
                match key with
                | [] -> Tree(f :: t.Fields, t.Children)
                | h :: hs ->
                    let children = 
                        t.Children |> MapExt.alter h (fun o ->
                            let o = match o with | Some o -> o | None -> empty
                            add o hs f |> Some
                        )
                    Tree(t.Fields, children)
            add t key f

        let rec allFields (t : Tree) : list<UniformField> =
            let structs = 
                t.Children |> MapExt.toList |> List.choose (fun (name, tree) -> 
                    let fields = allFields tree |> List.sortBy (fun f -> f.offset)
                    match fields with
                    | [] -> None
                    | h :: _ ->
                        let firstOffset = h.offset
                        let fields = fields |> List.map (fun f -> { FieldInfo.name = f.name; FieldInfo.offset = f.offset - firstOffset; FieldInfo.typ = f.typ; FieldInfo.rowMajor = f.rowMajor; FieldInfo.size = f.size; FieldInfo.stride = f.stride })
                        let size = fields |> List.map (fun f -> f.offset + PrimitiveType.size f.typ) |> List.max
                        Some {
                            UniformField.name = name
                            UniformField.typ = Struct(size, fields)
                            UniformField.offset = firstOffset
                            UniformField.stride = 0
                            UniformField.size = size
                            UniformField.rowMajor = false
                        }
                )
            let direct =
                t.Fields |> List.map ( fun f -> { f with name = getName f })

            List.append direct structs

    let private groupStructs (fields : list<UniformField>) =
        fields 
        |> List.fold (fun t f -> Tree.add f t) Tree.empty
        |> Tree.allFields
        |> List.sortBy (fun f -> f.offset)

    type WebGL2RenderingContext with
        member x.IsGL2 = 
            
            let v = unbox<string> (x.getParameter(x.VERSION)) 
            //Log.error "VERSION: %A" v
            if v.Contains "WebGL 2.0" then  
                true
            else
                x.getExtension("EXT_frag_depth") |> ignore
                false
        member x.GetAttributes(p : WebGLProgram) =
            Map.ofList [
                let cnt = x.getProgramParameter(p, x.ACTIVE_ATTRIBUTES) |> unbox<int>
                for ai in 0 .. cnt - 1 do
                    let att = x.getActiveAttrib(p, float ai)
                    if unbox att then
                        let loc = x.getAttribLocation(p, att.name) |> int
                        let t = PrimitiveType.ofGLType x att.``type``
                        yield loc, { name = att.name; typ = t; size = int att.size }
            ]

        member x.GetUniformBlocks(p : WebGLProgram, info : MapExt<string, FShade.GLSL.GLSLUniformBuffer>) =
            if x.IsGL2 then
                let cnt = x.getProgramParameter(p, x.ACTIVE_UNIFORM_BLOCKS) |> unbox<int>
                Map.ofList [
                    for bi in 0 .. cnt - 1 do
                        let name = x.getActiveUniformBlockName(p, float bi)
                        let indices = x.getActiveUniformBlockParameter(p, float bi, x.UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES) |> unbox<Uint32Array>
                        let size = x.getActiveUniformBlockParameter(p, float bi, x.UNIFORM_BLOCK_DATA_SIZE) |> unbox<int>

                        let offsets     = x.getActiveUniforms(p, indices, x.UNIFORM_OFFSET) |> unbox<Uint32Array>
                        let sizes       = x.getActiveUniforms(p, indices, x.UNIFORM_SIZE) |> unbox<Uint32Array>
                        let strides     = x.getActiveUniforms(p, indices, x.UNIFORM_ARRAY_STRIDE) |> unbox<Uint32Array>
                        let rowMajors   = x.getActiveUniforms(p, indices, x.UNIFORM_IS_ROW_MAJOR) |> unbox<bool[]>
                        let types       = x.getActiveUniforms(p, indices, x.UNIFORM_TYPE) |> unbox<Uint32Array>
        
                        x.uniformBlockBinding(p, float bi, float bi)
                        let fields =
                            let ubInfo = 
                                match MapExt.tryFind name info with
                                | Some i -> i.ubFields
                                | None -> []

                            List.init (int indices.length) (fun i ->
                                let off = offsets.[i] |> unbox<int>
                                let size = sizes.[i] |> unbox<int>
                                let stride = strides.[i] |> unbox<int>
                                let rowMajor = rowMajors.[i] |> unbox<bool>
                                let t = types.[i] |> float |> PrimitiveType.ofGLType x

                                let r = x.getActiveUniform(p, float indices.[i])
                                if unbox r.name then
                                    Some { offset = off; stride = stride; size = size; rowMajor = rowMajor; name = r.name; typ = t }
                                else
                                    let name = ubInfo.[i].ufName
                                    Some { offset = off; stride = stride; size = size; rowMajor = rowMajor; name = name; typ = t }
                                    //document.write(sprintf "bad: %A" (JSON.stringify { offset = off; stride = stride; size = size; rowMajor = rowMajor; name = r.name; typ = t }))
                                    //None
                                
                            )
                            |> List.choose id
                            |> groupStructs
                            |> List.sortBy (fun f -> f.offset)


                        yield bi, { index = bi; size = size; name = name; fields = fields; fieldsByName = fields |> Seq.map (fun f -> f.name, f) |> Map.ofSeq }
                ]
            else
                Map.empty

        member x.GetActiveSamplers(p : WebGLProgram, known : Set<string>) =
            let cnt = x.getProgramParameter(p, x.ACTIVE_UNIFORMS) |> unbox<int>

            let samplers = 
                List.init cnt (fun i ->
                    let u = x.getActiveUniform(p, float i)
                    if u.``type`` = x.SAMPLER_2D then
                        let loc = x.getUniformLocation(p, u.name)
                        if unbox loc then Some (u.name, loc)
                        else None
                    else    
                        None
                )
                |> List.choose id
            Map.ofList samplers
            
        member x.GetActiveUniforms(p : WebGLProgram, known : Set<string>) =
            let cnt = x.getProgramParameter(p, x.ACTIVE_UNIFORMS) |> unbox<int>

            let uniforms = 
                List.init cnt (fun i ->
                    let u = x.getActiveUniform(p, float i)
                    if not (Set.contains u.name known) then
                        let loc = x.getUniformLocation(p, u.name)
                        if unbox loc then Some (u.name, (loc, PrimitiveType.ofGLType x u.``type``))
                        else None
                    else    
                        None
                )
                |> List.choose id
            Map.ofList uniforms

        member x.FindOutputLocation(p : WebGLProgram, name : string) =
            let names = [name; name + "Out"; "fs_" + name]
            names |> List.tryPick (fun name ->
                let v = 0 //x.getFragDataLocation(p, name) |> unbox<int>
                if v >= 0 then Some v
                else None
            )

        member x.GetProgramInterface(signature : FramebufferSignature, p : WebGLProgram, iface : FShade.GLSL.GLSLProgramInterface) =  
            let valid = 
                if x.IsGL2 then
                    signature.Colors |> Map.forall (fun slot name ->
                        match x.FindOutputLocation(p, name) with
                        | Some loc when loc = slot -> true
                        | None -> console.warn (sprintf "[GL] program does not contain output: %s" name); false
                        | Some l -> console.warn (sprintf "[GL] program location for %s is %d (expected %d)" name l slot);false
                    )
                else
                    true
            if valid then
                let blocks = x.GetUniformBlocks(p, iface.uniformBuffers)

                let known =
                    blocks 
                    |> Map.toSeq
                    |> Seq.collect (fun (_,b) -> b.fields |> Seq.map (fun f -> f.name))
                    |> Set.ofSeq

                let samplers = x.GetActiveSamplers(p, known)

                let known = samplers |> Map.fold (fun s n _ -> Set.add n s) known
                let uniforms = x.GetActiveUniforms(p, known)

                Some {
                    attributes = x.GetAttributes(p)
                    uniformBlocks = blocks
                    uniforms = uniforms
                    samplers = samplers
                }
            else
                None
            
    let private versionRx = System.Text.RegularExpressions.Regex @"\#version.*(\r\n|\r|\n)"

    type Context with

        member x.CompileShader (stage : float, code : string) =
            let def =
                if stage = x.GL.VERTEX_SHADER then "Vertex"
                elif stage = x.GL.FRAGMENT_SHADER then "Fragment"
                else "UNKNOWN"

            let code = versionRx.Replace(code, fun m -> m.Value + "#define " + def + "\r\n")

            let shader = x.GL.createShader(stage)
            x.GL.shaderSource(shader, code)
            x.GL.compileShader(shader)
    
            let s = x.GL.getShaderParameter(shader, x.GL.COMPILE_STATUS) |> unbox<int>
            if s = 0 then
                let log = x.GL.getShaderInfoLog(shader)
                console.warn s
                console.warn log
                None
            else
                Some shader

        member x.CreateProgram(signature : FramebufferSignature, code : string, iface : FShade.GLSL.GLSLProgramInterface) =
            Log.debug "create program"

            match x.CompileShader(x.GL.VERTEX_SHADER, code), x.CompileShader(x.GL.FRAGMENT_SHADER, code) with
            | Some vs, Some fs ->
                let p = x.GL.createProgram()
                x.GL.attachShader(p, vs)
                x.GL.attachShader(p, fs)
                x.GL.linkProgram(p)

                let status = x.GL.getProgramParameter(p, x.GL.LINK_STATUS) |> unbox<int>
                if status <> 0 then
                    match x.GL.GetProgramInterface(signature, p, iface) with
                    | Some iface -> 
                        Some(Program(x, p, iface))
                    | None ->
                        x.GL.deleteProgram p
                        None
                else
                    let log = x.GL.getProgramInfoLog(p)
                    console.warn log
                    x.GL.deleteProgram p
                    None
            | _ ->
                None

