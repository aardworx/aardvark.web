namespace Aardvark.Base.Rendering

open Aardvark.Base.Incremental
open FSharp.Collections


type PipelineState =
    {
        shader          : string
        depthMode       : IMod<DepthTestMode>
        uniforms        : string -> Option<IMod>
    }



type RenderObject =
    {
        pipeline        : PipelineState
        vertexBuffers   : Map<string, BufferView>
        indexBuffer     : Option<BufferView>
        mode            : PrimitiveTopology
        call            : IMod<DrawCall>
    }
    
