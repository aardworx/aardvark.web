namespace Aardvark.Rendering.WebGL


open Fable.Import.Browser
open Fable.Import.JS
open FSharp.Collections
open Aardvark.Base.Rendering
open Fable.Core
open Aardvark.Base

type Texture(ctx : Context, handle : WebGLTexture) =
    inherit Resource(ctx)

    member x.Handle = handle

    override x.Destroy() =
        ctx.GL.deleteTexture handle

    override x.ToString() = string handle

type Sampler(ctx : Context, handle : WebGLSampler) =
    inherit Resource(ctx)
    
    member x.Handle = handle

    override x.Destroy() =
        ctx.GL.deleteSampler handle

    override x.ToString() = string handle


[<AutoOpen>]  
module TextureImpl = 
    
    type ImageBitmap =
        abstract member width : int
        abstract member height : int

    [<Emit("createImageBitmap($0)")>]
    let createImageBitmap(b : Blob) : Promise<ImageBitmap> = jsNative

    let loadTexture(url : string) =
        Prom.create (fun ok error ->
            let img = Image.Create()
            img.onload <- fun e ->
                let c = document.createElement_canvas()
                c.width <- img.width
                c.height <- img.height
                let ctx = c.getContext_2d()
                ctx.scale(1.0, -1.0)
                ctx.translate(0.0, -img.height)
                ctx.drawImage(U3.Case1 img, 0.0, 0.0, float img.width, float img.height)
                ctx.getImageData(0.0, 0.0, img.width, img.height) |> ok
            img.src <- url


            //let r = XMLHttpRequest.Create()
            //r.responseType <- "blob"

            //r.onload <- fun e ->
            //    let blob = r.response |> unbox<Blob>
            //    createImageBitmap(blob).``then``(fun v ->
            //        ok v
            //    ) |> ignore

            //r.``open``("GET", url)
            //r.send ""
        )

    let resize(img : ImageData, size : V2i) =
        if int img.width = size.X && int img.height = size.Y then
            img
        else
            Log.warn  "implicit texture resize: %A -> %A" (V2i(int img.width, int img.height)) size
            let src = document.createElement_canvas()
            let dst = document.createElement_canvas()
            let sctx = src.getContext_2d()
            let dctx = dst.getContext_2d()

            src.width <- img.width
            src.height <- img.height
            sctx.putImageData(img, 0.0, 0.0)

            dst.width <- float size.X
            dst.height <- float size.Y
            dctx.drawImage(U3.Case2 src, 0.0, 0.0, img.width, img.height, 0.0, 0.0, float size.X, float size.Y)
            dctx.getImageData(0.0, 0.0, float size.X, float size.Y)

    let nextPowerOfTwo (v : int) =
        let mutable x = v - 1
        x <- x ||| (x >>> 1)
        x <- x ||| (x >>> 2)
        x <- x ||| (x >>> 4)
        x <- x ||| (x >>> 8)
        x <- x ||| (x >>> 16)
        x + 1

    [<AutoOpen>]
    module FShadeConversions =
        open FShade

        module Filter = 
            let toMinMag (gl : WebGL2RenderingContext) (f : Option<FShade.Filter>) =
                match f with
                | Some Filter.Anisotropic -> (gl.LINEAR_MIPMAP_LINEAR, gl.LINEAR, true, true)
                | Some Filter.MinLinearMagMipPoint -> (gl.LINEAR_MIPMAP_NEAREST, gl.NEAREST, false, true)
                | Some Filter.MinLinearMagPointMipLinear -> (gl.LINEAR_MIPMAP_LINEAR, gl.NEAREST, false, true)
                | Some Filter.MinMagLinearMipPoint -> (gl.LINEAR_MIPMAP_NEAREST, gl.LINEAR, false, true)
                | Some Filter.MinMagMipLinear -> (gl.LINEAR_MIPMAP_LINEAR, gl.LINEAR, false, true)
                | Some Filter.MinMagMipPoint -> (gl.NEAREST_MIPMAP_NEAREST, gl.NEAREST, false, true)
                | Some Filter.MinMagPointMipLinear -> (gl.NEAREST_MIPMAP_LINEAR, gl.NEAREST, false, true)
                | Some Filter.MinPointMagLinearMipPoint -> (gl.NEAREST_MIPMAP_NEAREST, gl.LINEAR, false, true)
                | Some Filter.MinPointMagMipLinear -> (gl.NEAREST_MIPMAP_LINEAR, gl.LINEAR, false, true)
                | Some Filter.MinMagPoint -> (gl.NEAREST, gl.NEAREST, false, false)
                | Some Filter.MinMagLinear -> (gl.LINEAR, gl.LINEAR, false, false)
                | Some Filter.MinPointMagLinear -> (gl.NEAREST, gl.LINEAR, false, false)
                | Some Filter.MinLinearMagPoint -> (gl.LINEAR, gl.NEAREST, false, false)
                | _ -> (gl.LINEAR_MIPMAP_LINEAR, gl.LINEAR, false, true)

        module WrapMode =
            let toRepeatMode (gl : WebGL2RenderingContext) (f : Option<WrapMode>) =
                match f with
                | Some WrapMode.Clamp -> gl.CLAMP_TO_EDGE
                | Some WrapMode.Wrap -> gl.REPEAT
                | Some WrapMode.Mirror -> gl.MIRRORED_REPEAT
                | Some WrapMode.MirrorOnce -> gl.MIRRORED_REPEAT
                | Some WrapMode.Border -> gl.CLAMP_TO_EDGE
                | _ -> gl.REPEAT

        module ComparisonFunction =
            let toGL (gl : WebGL2RenderingContext) (f : Option<ComparisonFunction>) =
                match f with
                | Some f ->
                    match f with
                    | ComparisonFunction.Always -> Some gl.ALWAYS
                    | ComparisonFunction.Equal -> Some gl.EQUAL
                    | ComparisonFunction.Greater -> Some gl.GREATER
                    | ComparisonFunction.GreaterOrEqual -> Some gl.GEQUAL
                    | ComparisonFunction.Less -> Some gl.LESS
                    | ComparisonFunction.LessOrEqual -> Some gl.LEQUAL
                    | ComparisonFunction.Never -> Some gl.NEVER
                    | ComparisonFunction.NotEqual -> Some gl.NOTEQUAL
                    | _ -> None
                | None ->
                    None

    type Context with

        member x.CreateSampler(sam : FShade.SamplerState) =
            let gl = x.GL
            let handle = gl.createSampler()
            
            let fMin, fMag, anisotropy, mipMap = Filter.toMinMag gl sam.Filter
            gl.samplerParameteri(handle, gl.TEXTURE_MIN_FILTER, fMin)
            gl.samplerParameteri(handle, gl.TEXTURE_MAG_FILTER, fMag)
            
            let ext = gl.getExtension("EXT_texture_filter_anisotropic") |> unbox<EXT_texture_filter_anisotropic>
            if anisotropy then
                if unbox ext then
                    let v = gl.getParameter(ext.MAX_TEXTURE_MAX_ANISOTROPY_EXT) |> unbox<float>
                    let a = min v (match sam.MaxAnisotropy with | Some v -> float v | None -> 1024.0)
                    gl.samplerParameterf(handle, ext.TEXTURE_MAX_ANISOTROPY_EXT, a)
                    //Log.warn "[GL] max anisotropy: %.0f" a
                else
                    Log.warn "[GL] anisotropic filtering not supported"

            
            let addressU = WrapMode.toRepeatMode gl sam.AddressU
            let addressV = WrapMode.toRepeatMode gl sam.AddressV
            gl.samplerParameteri(handle, gl.TEXTURE_WRAP_S, addressU)
            gl.samplerParameteri(handle, gl.TEXTURE_WRAP_T, addressV)


            match ComparisonFunction.toGL gl sam.Comparison with
            | Some f ->
                gl.samplerParameteri(handle, gl.TEXTURE_COMPARE_FUNC, f)
                gl.samplerParameteri(handle, gl.TEXTURE_COMPARE_MODE, gl.COMPARE_REF_TO_TEXTURE)
            | None ->
                ()

            match sam.MinLod with
            | Some min -> gl.samplerParameteri(handle, gl.TEXTURE_MIN_LOD, min) 
            | None -> ()

            match sam.MaxLod with
            | Some max -> gl.samplerParameteri(handle, gl.TEXTURE_MAX_LOD, max)
            | None -> ()

            if not mipMap then
                gl.samplerParameteri(handle, gl.TEXTURE_MAX_LEVEL, 0.0)

            Sampler(x, handle)

        member x.CreateTexture(t : ITexture, sam : Option<FShade.SamplerState>) =
            match t with
            | :? FileTexture as t ->
                loadTexture(t.Url).``then``(fun bmp ->
                    let gl = x.GL
                    let tex = gl.createTexture()
                    gl.bindTexture(gl.TEXTURE_2D, tex)

                    let bmp =
                        if gl.IsGL2 then 
                            bmp
                        else 
                            let w = nextPowerOfTwo (int bmp.width)
                            let h = nextPowerOfTwo (int bmp.height)
                            resize(bmp, V2i(w,h))
                    
                    let ext = gl.getExtension("EXT_texture_filter_anisotropic") |> unbox<EXT_texture_filter_anisotropic>
                    
                    if gl.IsGL2 then
                        gl.texImage2D(gl.TEXTURE_2D, 0.0, gl.RGBA, float bmp.width, float bmp.height, 0.0, gl.RGBA, gl.UNSIGNED_BYTE, bmp :> obj)
                    else
                        gl.texImage2D(gl.TEXTURE_2D, 0.0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, bmp)

                    match sam with
                    | Some sam -> 
                        let fMin, fMag, anisotropy, mipMap = Filter.toMinMag gl sam.Filter

                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, fMag)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, fMin)
                        
                        if anisotropy && unbox ext then
                            let v = gl.getParameter(ext.MAX_TEXTURE_MAX_ANISOTROPY_EXT) |> unbox<float>
                            let a = min v (match sam.MaxAnisotropy with | Some v -> float v | None -> 1024.0)
                            gl.texParameteri(gl.TEXTURE_2D, ext.TEXTURE_MAX_ANISOTROPY_EXT, a)

                        let addressU = WrapMode.toRepeatMode gl sam.AddressU
                        let addressV = WrapMode.toRepeatMode gl sam.AddressU
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, addressU)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, addressV)

                        if gl.IsGL2 then
                            match ComparisonFunction.toGL gl sam.Comparison with
                            | Some f ->
                                gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_COMPARE_FUNC, f)
                                gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_COMPARE_MODE, gl.COMPARE_REF_TO_TEXTURE)
                            | None ->
                                ()

                            match sam.MinLod with
                            | Some min -> gl.texParameterf(gl.TEXTURE_2D, gl.TEXTURE_MIN_LOD, min) 
                            | None -> ()

                            match sam.MaxLod with
                            | Some max -> gl.texParameterf(gl.TEXTURE_2D, gl.TEXTURE_MAX_LOD, max)
                            | None -> ()

                            if not mipMap then
                                gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAX_LEVEL, 0.0)
                        
                        if mipMap then
                            gl.generateMipmap(gl.TEXTURE_2D)

                    | None ->
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)

                        if  unbox ext then
                            let v = gl.getParameter(ext.MAX_TEXTURE_MAX_ANISOTROPY_EXT) |> unbox<float>
                            gl.texParameteri(gl.TEXTURE_2D, ext.TEXTURE_MAX_ANISOTROPY_EXT, v)

                        gl.generateMipmap(gl.TEXTURE_2D)

                    
                    
                    


                    gl.bindTexture(gl.TEXTURE_2D, null)
                    Texture(x, tex)
                )
            | _ ->
                failwith "bad texture"

        //member x.CreateSampler(sam : FShade.SamplerState) =
        //    let gl = x.GL
        //    gl.createSa


