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

    [<Emit("createImageBitmap($0, { imageOrientation: \"flipY\" })")>]
    let createImageBitmap(b : Blob) : Promise<ImageBitmap> = jsNative

    let loadTexture(url : string) =
        Prom.create (fun ok error ->
            let r = XMLHttpRequest.Create()
            r.responseType <- "blob"

            r.onload <- fun e ->
                let blob = r.response |> unbox<Blob>
                createImageBitmap(blob).``then``(fun v ->
                    ok v
                ) |> ignore

            r.``open``("GET", url)
            r.send ""
        )

    type Context with
        member x.CreateTexture(t : ITexture) =
            match t with
            | :? FileTexture as t ->
                loadTexture(t.Url).``then``(fun bmp ->
                    let gl = x.GL
                    let tex = gl.createTexture()
                    gl.bindTexture(gl.TEXTURE_2D, tex)
                    gl.texImage2D(gl.TEXTURE_2D, 0.0, gl.RGBA, float bmp.width, float bmp.height, 0.0, gl.RGBA, gl.UNSIGNED_BYTE, bmp :> obj)
                    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
                    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR)
                    gl.generateMipmap(gl.TEXTURE_2D)
                    gl.bindTexture(gl.TEXTURE_2D, null)
                    Texture(x, tex)
                )
            | _ ->
                failwith "bad texture"


