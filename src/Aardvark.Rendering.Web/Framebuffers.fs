namespace Aardvark.Base.Rendering

open FSharp.Collections


type IFramebufferSignature =
    abstract member Colors : Map<int, string>
    abstract member Depth : bool
    