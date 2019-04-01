namespace Aardvark.Rendering.WebGL

open Fable.Import.Browser
open Fable.Import.JS
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

    type WebGL2RenderingContext with
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

        member x.GetUniformBlocks(p : WebGLProgram) =
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
                        List.init (int indices.length) (fun i ->
                            let off = offsets.[i] |> unbox<int>
                            let size = sizes.[i] |> unbox<int>
                            let stride = strides.[i] |> unbox<int>
                            let rowMajor = rowMajors.[i] |> unbox<bool>
                            let t = types.[i] |> PrimitiveType.ofGLType x

                            let r = x.getActiveUniform(p, indices.[i])
                            if unbox r.name then
                                Some { offset = off; stride = stride; size = size; rowMajor = rowMajor; name = r.name; typ = t }
                            else
                                document.write(sprintf "bad: %A" (JSON.stringify { offset = off; stride = stride; size = size; rowMajor = rowMajor; name = r.name; typ = t }))
                                None
                                
                        )
                        |> List.choose id
                        |> List.sortBy (fun f -> f.offset)

                    yield bi, { index = bi; size = size; name = name; fields = fields; fieldsByName = fields |> Seq.map (fun f -> f.name, f) |> Map.ofSeq }
            ]

        member x.FindOutputLocation(p : WebGLProgram, name : string) =
            let names = [name; name + "Out"; "fs_" + name]
            names |> List.tryPick (fun name ->
                let v = x.getFragDataLocation(p, name) |> unbox<int>
                if v >= 0 then Some v
                else None
            )

        member x.GetProgramInterface(signature : FramebufferSignature, p : WebGLProgram) =  
            let valid = 
                signature.Colors |> Map.forall (fun slot name ->
                    match x.FindOutputLocation(p, name) with
                    | Some loc when loc = slot -> true
                    | None -> console.warn (sprintf "[GL] program does not contain output: %s" name); false
                    | Some l -> console.warn (sprintf "[GL] program location for %s is %d (expected %d)" name l slot);false
                )
            if valid then
                Some {
                    attributes = x.GetAttributes(p)
                    uniformBlocks = x.GetUniformBlocks(p)
                }
            else
                None
            
    let private versionRx = System.Text.RegularExpressions.Regex @"\#version.*(\r\n|\r|\n)"

    type Context with

        member x.CompileShader (stage : float, code : string) =
            let def =
                if stage = x.GL.VERTEX_SHADER then "VERTEX"
                elif stage = x.GL.FRAGMENT_SHADER then "FRAGMENT"
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

        member x.CreateProgram(signature : FramebufferSignature, code : string) =
            Log.debug "create program"
            match x.CompileShader(x.GL.VERTEX_SHADER, code), x.CompileShader(x.GL.FRAGMENT_SHADER, code) with
            | Some vs, Some fs ->
                let p = x.GL.createProgram()
                x.GL.attachShader(p, vs)
                x.GL.attachShader(p, fs)
                x.GL.linkProgram(p)

                let status = x.GL.getProgramParameter(p, x.GL.LINK_STATUS) |> unbox<int>
                if status <> 0 then
                    match x.GL.GetProgramInterface(signature, p) with
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

