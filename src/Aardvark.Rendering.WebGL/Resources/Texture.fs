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

    type Context with
        member x.CreateTexture(t : ITexture) =
            match t with
            | :? FileTexture as t ->
                loadTexture(t.Url).``then``(fun bmp ->
                    let gl = x.GL
                    let tex = gl.createTexture()
                    gl.bindTexture(gl.TEXTURE_2D, tex)

                    
                    
                    let ext = gl.getExtension("EXT_texture_filter_anisotropic") |> unbox<EXT_texture_filter_anisotropic>
                    
                    if gl.IsGL2 then
                        gl.texImage2D(gl.TEXTURE_2D, 0.0, gl.RGBA, float bmp.width, float bmp.height, 0.0, gl.RGBA, gl.UNSIGNED_BYTE, bmp :> obj)
                    
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
                        if unbox ext then
                            let v = gl.getParameter(ext.MAX_TEXTURE_MAX_ANISOTROPY_EXT) |> unbox<float>
                            gl.texParameteri(gl.TEXTURE_2D, ext.TEXTURE_MAX_ANISOTROPY_EXT, v)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT)
                        
                        gl.generateMipmap(gl.TEXTURE_2D)
                    
                    
                    
                    else
                        let w = nextPowerOfTwo (int bmp.width)
                        let h = nextPowerOfTwo (int bmp.height)
                        let bmp = resize(bmp, V2i(w,h))


                        gl.texImage2D(gl.TEXTURE_2D, 0.0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, bmp)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
                        if unbox ext then
                            let v = gl.getParameter(ext.MAX_TEXTURE_MAX_ANISOTROPY_EXT) |> unbox<float>
                            gl.texParameteri(gl.TEXTURE_2D, ext.TEXTURE_MAX_ANISOTROPY_EXT, v)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT)
                        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT)
                        gl.generateMipmap(gl.TEXTURE_2D)
                    


                    gl.bindTexture(gl.TEXTURE_2D, null)
                    Texture(x, tex)
                )
            | _ ->
                failwith "bad texture"


