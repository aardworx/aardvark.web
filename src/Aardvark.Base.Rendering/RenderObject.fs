namespace Aardvark.Base.Rendering

open Aardvark.Base.Incremental
open FSharp.Collections


type PipelineState =
    {
        shader          : string
        depthMode       : IMod<DepthTestMode>
        uniforms        : string -> Option<IMod>
    }
[<CustomEquality; CustomComparison>]
type RenderObject =
    {
        id              : int
        pipeline        : PipelineState
        vertexBuffers   : Map<string, BufferView>
        indexBuffer     : Option<BufferView>
        mode            : PrimitiveTopology
        call            : IMod<DrawCall>
    }
    
    
    override x.GetHashCode() = x.id
    override x.Equals o =
        match o with
        | :? RenderObject as o -> x.id = o.id
        | _ -> false
    interface System.IComparable with
        member x.CompareTo o =
            match o with
            | :? RenderObject as o -> compare x.id o.id
            | _ -> failwith "uncomparable"