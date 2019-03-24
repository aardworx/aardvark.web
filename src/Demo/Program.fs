module Program

open System
open Aardvark.Base
open Fable.Core
open Fable.Import.Browser
open Fable.Import.JS

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

type Context(gl : WebGLRenderingContext) =

    static let versionRx = System.Text.RegularExpressions.Regex @"\#version.*(\r\n|\r|\n)"

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





//type WebGLRenderingContext with
//    member x.UNIFORM_BUFFER = 35345.0

//    [<Emit("$0.bindBufferRange($1, $2, $3, $4, $5)")>]
//    member x.bindBufferRange(target : float, index : float, buffer : WebGLBuffer, offset : float, size : float) = jsNative

  
type WebGL2RenderingContext =
    inherit WebGLRenderingContext
    abstract member UNIFORM_BUFFER : float
    abstract member bindBufferRange : target : float * index : float * buffer : WebGLBuffer * offset : float * size : float -> unit


[<EntryPoint>]
let main argv =

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
                Float32Array.``of`` [|
                    -1.0; -1.0; 0.0
                    1.0; -1.0; 0.0
                    1.0; 1.0; 0.0
                |]



            let b = gl.createBuffer()
            gl.bindBuffer(gl.ARRAY_BUFFER, b)
            gl.bufferData(gl.ARRAY_BUFFER, U3.Case3 pos.buffer, gl.STATIC_DRAW)
            gl.bindBuffer(gl.ARRAY_BUFFER, null)

            let shader =
                """#version 300 es

                    #ifdef VERTEX

                    uniform View {
                        mat4 MVP;
                    };

                    layout(location = 0) in vec3 pos;
                    void main() {
                        gl_Position = vec4(pos, 1.0) * MVP;
                    }
                    #endif

                    #ifdef FRAGMENT
                    precision highp float;
                    precision highp int;
                    layout(location = 0) out vec4 color;
                    void main() {
                        color = vec4(1,0,0,1);
                    }
                    #endif
                """

            let p = ctx.CreateProgram(shader)



            let m = M44d.RotationZ(0.3)

            let ub = gl.createBuffer()
            gl.bindBuffer(gl.UNIFORM_BUFFER, ub)
            gl.bufferData(gl.UNIFORM_BUFFER, U3.Case3 (m.ToFloat32Array().buffer), gl.DYNAMIC_DRAW)
            gl.bindBuffer(gl.UNIFORM_BUFFER, null)


            let mutable t0 = None //performance.now()
            let mutable angle = 0.0
            let rec render _ =
                let t = performance.now()
                match t0 with
                | Some tb ->
                    let dt = t - tb
                    t0 <- Some t
                    angle <- angle + 0.001 * dt
                | None ->
                    t0 <- Some t
                
                let m = M44d.RotationZ(angle)
                gl.bindBuffer(gl.UNIFORM_BUFFER, ub)
                gl.bufferData(gl.UNIFORM_BUFFER, U3.Case3 (m.ToFloat32Array().buffer), gl.DYNAMIC_DRAW)
                gl.bindBuffer(gl.UNIFORM_BUFFER, null)

                let rect = canvas.getBoundingClientRect()

                if canvas.width <> rect.width then canvas.width <- rect.width
                if canvas.height <> rect.height then canvas.height <- rect.height
                //console.log(sprintf "%.0fx%.0f" rect.width rect.height)
                gl.viewport(0.0, 0.0, rect.width, rect.height)
                gl.clearColor(0.0, 0.0, 0.0, 1.0)
                gl.clearDepth(1.0)
                gl.clear(float (int gl.COLOR_BUFFER_BIT ||| int gl.DEPTH_BUFFER_BIT))

                gl.useProgram(p.Value)
                gl.bindBufferRange(gl.UNIFORM_BUFFER, 0.0, ub, 0.0, 64.0)
                gl.bindBuffer(gl.ARRAY_BUFFER, b)
                gl.enableVertexAttribArray(0.0)
                gl.vertexAttribPointer(0.0, 3.0, gl.FLOAT, false, 0.0, 0.0)
                gl.bindBuffer(gl.ARRAY_BUFFER, null)
                gl.drawArrays(gl.TRIANGLES, 0.0, 3.0)
                gl.useProgram(null)
                window.requestAnimationFrame(render) |> ignore

            window.requestAnimationFrame(render) |> ignore

    )







    let a = M44d.Rotation(V3d.III, 0.2) * M44d.Scale(V3d(0.1, 0.6, 10.0)) * M44d.Translation(V3d(1.0, 2.0, 3.0)) //(1.0, 2.0, 3.0, 0.0, 0.2, 0.0, 0.0, 0.0, 2.0)
    let b = a.Inverse

    let tt = Trafo3d.Translation(1.0, 2.0, 3.0) * Trafo3d.Scale(0.1, 0.6, 10.0) * Trafo3d.Rotation(V3d.III, 0.2)

    let t1 = a * b
    let t2 = b * a
    
    let printMat (name : string) (m : M44d) =
        console.log (sprintf "%s: " name)
        console.log ("  " + m.R0.ToString())
        console.log ("  " + m.R1.ToString())
        console.log ("  " + m.R2.ToString())
        console.log ("  " + m.R3.ToString())
        
    printMat "a" a
    printMat "aa" tt.Forward
    printMat "b" b
    printMat "t1" t1
    printMat "t2" t2

    console.log (string t1)



    0 // return an integer exit code
