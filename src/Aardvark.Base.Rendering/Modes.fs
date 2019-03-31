namespace Aardvark.Base.Rendering

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
