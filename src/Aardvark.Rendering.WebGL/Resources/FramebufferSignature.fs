namespace Aardvark.Rendering.WebGL

open Aardvark.Base.Rendering

type FramebufferSignature(ctx : Context, colors : Map<int, string>, depth : bool) =
    inherit Resource(ctx)

    member x.Colors = colors
    member x.Depth = depth
    override x.Destroy() = ()

[<AutoOpen>]
module FramebufferSignatureImpl =
    type Context with
        member x.CreateFramebufferSignature (attachments : Set<string>) =
            let depth =
                Set.contains DefaultSemantic.Depth attachments
            let colors =
                attachments
                |> Set.remove DefaultSemantic.Depth
                |> Seq.sortWith(fun l r -> 
                    if l = DefaultSemantic.Colors then
                        if r = DefaultSemantic.Colors then 0
                        else -1
                    elif r = DefaultSemantic.Colors then
                        1
                    else
                        compare l r 
                )
                |> Seq.mapi (fun i n -> i,n)
                |> Map.ofSeq

            FramebufferSignature(x, colors, depth)

        member x.DefaultFramebufferSignature =
            x.CreateFramebufferSignature(Set.ofList [DefaultSemantic.Colors; DefaultSemantic.Depth])
