namespace Aardvark.Base.Rendering

open Fable.Import.JS
open Aardvark.Base
open Aardvark.Base.Incremental
open FSharp.Collections


type IFramebufferSignature =
    abstract member Colors : Map<int, string>
    abstract member Depth : bool
    
type DepthTestMode =
    | None = 0
    | Less = 1
    | LessOrEqual = 2
    | Greater = 3
    | GreaterOrEqual = 4
    | Equal = 5
    | NotEqual = 6
    | Never = 7
    | Always = 8
    
type PrimitiveTopology =
    | PointList = 0
    | LineList = 1
    | LineStrip = 2
    | TriangleList = 3
    | TriangleStrip = 4
    
type DrawCall =
    {
        first           : int
        faceVertexCount : int
        instanceCount   : int
    }
