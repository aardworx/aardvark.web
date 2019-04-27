namespace FShade
open Aardvark.Base


[<SamplerType>]
type Sampler1dArrayShadowMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1dArrayShadowMS>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, sample : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler1dArrayMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1dArrayMS>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, sample : int) : V4d = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler1dArrayShadow(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1dArrayShadow>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, cmp : float, offset : int) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, cmp : float, offset : int, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, slice : int, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, cmp : float, dTdx : float, dTdy : float) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, lod : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler1dArray(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1dArray>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, offset : int) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, offset : int, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, slice : int, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, dTdx : float, dTdy : float) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, lod : int) : V4d = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler1dShadowMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1dShadowMS>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : int, sample : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler1dMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1dMS>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : int, sample : int) : V4d = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler1dShadow(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1dShadow>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<float>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, cmp : float, offset : int) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, cmp : float, offset : int, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d, cmp : float) : float = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, cmp : float, lodBias : float) : float = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, cmp : float, dTdx : float, dTdy : float) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler1d(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler1d>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, offset : int) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, offset : int, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d) : V4d = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, lodBias : float) : V4d = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, dTdx : float, dTdy : float) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : V4d = onlyInShaderCode "Read"
    
    member x.Item
        with get (coord : int) : V4d = onlyInShaderCode "Fetch"
    
    member x.Item
        with get(coord : int, level : int) : V4d = onlyInShaderCode "Fetch"

[<SamplerType>]
type Sampler2dArrayShadowMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2dArrayShadowMS>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, sample : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler2dArrayMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2dArrayMS>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, sample : int) : V4d = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler2dArrayShadow(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2dArrayShadow>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, cmp : float, offset : V2i) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, cmp : float, offset : V2i, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, cmp : float, dTdx : V2d, dTdy : V2d) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, comp : int) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, comp : int) : V4d = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, lod : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler2dArray(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2dArray>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, dTdx : V2d, dTdy : V2d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, comp : int) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, comp : int) : V4d = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, lod : int) : V4d = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler2dShadowMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2dShadowMS>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, sample : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler2dMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2dMS>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, sample : int) : V4d = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler2dShadow(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2dShadow>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, cmp : float, offset : V2i) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, cmp : float, offset : V2i, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d, cmp : float) : float = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, cmp : float, lodBias : float) : float = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, cmp : float, dTdx : V2d, dTdy : V2d) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, comp : int) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, offset : V2i, comp : int) : V4d = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler2d(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler2d>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, offset : V2i) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, offset : V2i, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d) : V4d = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, lodBias : float) : V4d = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, dTdx : V2d, dTdy : V2d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, comp : int) : V4d = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, offset : V2i, comp : int) : V4d = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : V4d = onlyInShaderCode "Read"
    
    member x.Item
        with get (coord : V2i) : V4d = onlyInShaderCode "Fetch"
    
    member x.Item
        with get(coord : V2i, level : int) : V4d = onlyInShaderCode "Fetch"

[<SamplerType>]
type Sampler3dShadowMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler3dShadowMS>

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, sample : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler3dMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler3dMS>

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, sample : int) : V4d = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler3dShadow(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler3dShadow>

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, cmp : float, offset : V3i) : float = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, cmp : float, offset : V3i, lodBias : float) : float = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d, cmp : float) : float = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, cmp : float, lodBias : float) : float = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, cmp : float, dTdx : V3d, dTdy : V3d) : float = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : float = onlyInShaderCode "Read"
    

[<SamplerType>]
type Sampler3d(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<Sampler3d>

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, offset : V3i) : V4d = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, offset : V3i, lodBias : float) : V4d = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d) : V4d = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, lodBias : float) : V4d = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4d = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4d = onlyInShaderCode "Read"
    
    member x.Item
        with get (coord : V3i) : V4d = onlyInShaderCode "Fetch"
    
    member x.Item
        with get(coord : V3i, level : int) : V4d = onlyInShaderCode "Fetch"

[<SamplerType>]
type SamplerCubeArrayShadowMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCubeArrayShadowMS>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    

[<SamplerType>]
type SamplerCubeArrayMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCubeArrayMS>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    

[<SamplerType>]
type SamplerCubeArrayShadow(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCubeArrayShadow>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    

[<SamplerType>]
type SamplerCubeArray(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCubeArray>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    

[<SamplerType>]
type SamplerCubeShadowMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCubeShadowMS>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    

[<SamplerType>]
type SamplerCubeMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCubeMS>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    

[<SamplerType>]
type SamplerCubeShadow(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCubeShadow>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<float>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = true
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, cmp : float) : float = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, cmp : float, lodBias : float) : float = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, cmp : float, level : float) : float = onlyInShaderCode "SampleLevel"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    

[<SamplerType>]
type SamplerCube(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<SamplerCube>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4d = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4d = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4d = onlyInShaderCode "SampleLevel"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    

[<SamplerType>]
type IntSampler1dArrayMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler1dArrayMS>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, sample : int) : V4i = onlyInShaderCode "Read"
    

[<SamplerType>]
type IntSampler1dArray(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler1dArray>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float, slice : int) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, slice : int, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, slice : int, offset : int) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, slice : int, offset : int, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, slice : int, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, slice : int, dTdx : float, dTdy : float) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, slice : int, lod : int) : V4i = onlyInShaderCode "Read"
    

[<SamplerType>]
type IntSampler1dMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler1dMS>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : int, sample : int) : V4i = onlyInShaderCode "Read"
    

[<SamplerType>]
type IntSampler1d(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler1d>

    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<float>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : int = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : int = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : float, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : float, offset : int) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : float, offset : int, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V2d) : V4i = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V2d, lodBias : float) : V4i = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : float, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : float, dTdx : float, dTdy : float) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : float) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : int, lod : int) : V4i = onlyInShaderCode "Read"
    
    member x.Item
        with get (coord : int) : V4i = onlyInShaderCode "Fetch"
    
    member x.Item
        with get(coord : int, level : int) : V4i = onlyInShaderCode "Fetch"

[<SamplerType>]
type IntSampler2dArrayMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler2dArrayMS>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, sample : int) : V4i = onlyInShaderCode "Read"
    

[<SamplerType>]
type IntSampler2dArray(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler2dArray>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d, slice : int) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, slice : int, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, slice : int, offset : V2i, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, slice : int, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, slice : int, dTdx : V2d, dTdy : V2d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, slice : int, comp : int) : V4i = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, slice : int, offset : V2i, comp : int) : V4i = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, slice : int, lod : int) : V4i = onlyInShaderCode "Read"
    

[<SamplerType>]
type IntSampler2dMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler2dMS>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, sample : int) : V4i = onlyInShaderCode "Read"
    

[<SamplerType>]
type IntSampler2d(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler2d>

    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V2d) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V2d, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V2d, offset : V2i) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V2d, offset : V2i, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V3d) : V4i = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V3d, lodBias : float) : V4i = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V2d, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V2d, dTdx : V2d, dTdy : V2d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V2d) : V2d = onlyInShaderCode "QueryLod"
    
    /// gathers one component for the neighbouring 4 texels
    member x.Gather(coord : V2d, comp : int) : V4i = onlyInShaderCode "Gather"
    
    /// gathers one component for the neighbouring 4 texels with an offset
    member x.GatherOffset(coord : V2d, offset : V2i, comp : int) : V4i = onlyInShaderCode "GatherOffset"
    
    /// non-sampled texture read
    member x.Read(coord : V2i, lod : int) : V4i = onlyInShaderCode "Read"
    
    member x.Item
        with get (coord : V2i) : V4i = onlyInShaderCode "Fetch"
    
    member x.Item
        with get(coord : V2i, level : int) : V4i = onlyInShaderCode "Fetch"

[<SamplerType>]
type IntSampler3dMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler3dMS>

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, sample : int) : V4i = onlyInShaderCode "Read"
    

[<SamplerType>]
type IntSampler3d(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSampler3d>

    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V3i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V3i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with offset
    member x.SampleOffset(coord : V3d, offset : V3i) : V4i = onlyInShaderCode "SampleOffset"
    
    /// regular sampled texture-lookup with offset with lod-bias
    member x.SampleOffset(coord : V3d, offset : V3i, lodBias : float) : V4i = onlyInShaderCode "SampleOffset"
    
    /// projective sampled texture-lookup
    member x.SampleProj(coord : V4d) : V4i = onlyInShaderCode "SampleProj"
    
    /// projective sampled texture-lookup with lod-bias
    member x.SampleProj(coord : V4d, lodBias : float) : V4i = onlyInShaderCode "SampleProj"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// sampled texture-lookup with explicit gradients
    member x.SampleGrad(coord : V3d, dTdx : V3d, dTdy : V3d) : V4i = onlyInShaderCode "SampleGrad"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    
    /// non-sampled texture read
    member x.Read(coord : V3i, lod : int) : V4i = onlyInShaderCode "Read"
    
    member x.Item
        with get (coord : V3i) : V4i = onlyInShaderCode "Fetch"
    
    member x.Item
        with get(coord : V3i, level : int) : V4i = onlyInShaderCode "Fetch"

[<SamplerType>]
type IntSamplerCubeArrayMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSamplerCubeArrayMS>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    

[<SamplerType>]
type IntSamplerCubeArray(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSamplerCubeArray>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = true
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d, slice : int) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, slice : int, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, slice : int, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    

[<SamplerType>]
type IntSamplerCubeMS(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSamplerCubeMS>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = true
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    

[<SamplerType>]
type IntSamplerCube(tex : string, state : SamplerState) =
    interface ISampler with
        member x.Texture = tex
        member x.State = state
        member x.SelfType = typeof<IntSamplerCube>

    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3d>
    static member IsArray = false
    static member IsShadow = false
    static member IsMultisampled = false
    
    /// the mipmap-levels for the sampler
    member x.MipMapLevels : int = onlyInShaderCode "MipMapLevels"
    
    /// the size for the sampler
    member x.GetSize (level : int) : V2i = onlyInShaderCode "GetSize"
    
    /// the size for the sampler
    member x.Size : V2i = onlyInShaderCode "Size"
    
    /// regular sampled texture-lookup
    member x.Sample(coord : V3d) : V4i = onlyInShaderCode "Sample"
    
    /// regular sampled texture-lookup with lod-bias
    member x.Sample(coord : V3d, lodBias : float) : V4i = onlyInShaderCode "Sample"
    
    /// sampled texture-lookup with given level
    member x.SampleLevel(coord : V3d, level : float) : V4i = onlyInShaderCode "SampleLevel"
    
    /// query lod levels
    member x.QueryLod(coord : V3d) : V2d = onlyInShaderCode "QueryLod"
    

[<AutoOpen>]
module SamplerBuilders = 
    type Sampler1dArrayShadowMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1dArrayShadowMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dArrayShadowMS(t + string i, s))

    let sampler1dArrayShadowMS = Sampler1dArrayShadowMSBuilder()
    
    type Sampler1dArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1dArrayMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dArrayMS(t + string i, s))

    let sampler1dArrayMS = Sampler1dArrayMSBuilder()
    
    type Sampler1dArrayShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1dArrayShadow(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dArrayShadow(t + string i, s))

    let sampler1dArrayShadow = Sampler1dArrayShadowBuilder()
    
    type Sampler1dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1dArray(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dArray(t + string i, s))

    let sampler1dArray = Sampler1dArrayBuilder()
    
    type Sampler1dShadowMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1dShadowMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dShadowMS(t + string i, s))

    let sampler1dShadowMS = Sampler1dShadowMSBuilder()
    
    type Sampler1dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1dMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dMS(t + string i, s))

    let sampler1dMS = Sampler1dMSBuilder()
    
    type Sampler1dShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1dShadow(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1dShadow(t + string i, s))

    let sampler1dShadow = Sampler1dShadowBuilder()
    
    type Sampler1dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler1d(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler1d(t + string i, s))

    let sampler1d = Sampler1dBuilder()
    
    type Sampler2dArrayShadowMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2dArrayShadowMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dArrayShadowMS(t + string i, s))

    let sampler2dArrayShadowMS = Sampler2dArrayShadowMSBuilder()
    
    type Sampler2dArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2dArrayMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dArrayMS(t + string i, s))

    let sampler2dArrayMS = Sampler2dArrayMSBuilder()
    
    type Sampler2dArrayShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2dArrayShadow(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dArrayShadow(t + string i, s))

    let sampler2dArrayShadow = Sampler2dArrayShadowBuilder()
    
    type Sampler2dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2dArray(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dArray(t + string i, s))

    let sampler2dArray = Sampler2dArrayBuilder()
    
    type Sampler2dShadowMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2dShadowMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dShadowMS(t + string i, s))

    let sampler2dShadowMS = Sampler2dShadowMSBuilder()
    
    type Sampler2dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2dMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dMS(t + string i, s))

    let sampler2dMS = Sampler2dMSBuilder()
    
    type Sampler2dShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2dShadow(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2dShadow(t + string i, s))

    let sampler2dShadow = Sampler2dShadowBuilder()
    
    type Sampler2dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler2d(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler2d(t + string i, s))

    let sampler2d = Sampler2dBuilder()
    
    type Sampler3dShadowMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler3dShadowMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler3dShadowMS(t + string i, s))

    let sampler3dShadowMS = Sampler3dShadowMSBuilder()
    
    type Sampler3dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler3dMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler3dMS(t + string i, s))

    let sampler3dMS = Sampler3dMSBuilder()
    
    type Sampler3dShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler3dShadow(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler3dShadow(t + string i, s))

    let sampler3dShadow = Sampler3dShadowBuilder()
    
    type Sampler3dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            Sampler3d(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> Sampler3d(t + string i, s))

    let sampler3d = Sampler3dBuilder()
    
    type SamplerCubeArrayShadowMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCubeArrayShadowMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeArrayShadowMS(t + string i, s))

    let samplerCubeArrayShadowMS = SamplerCubeArrayShadowMSBuilder()
    
    type SamplerCubeArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCubeArrayMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeArrayMS(t + string i, s))

    let samplerCubeArrayMS = SamplerCubeArrayMSBuilder()
    
    type SamplerCubeArrayShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCubeArrayShadow(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeArrayShadow(t + string i, s))

    let samplerCubeArrayShadow = SamplerCubeArrayShadowBuilder()
    
    type SamplerCubeArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCubeArray(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeArray(t + string i, s))

    let samplerCubeArray = SamplerCubeArrayBuilder()
    
    type SamplerCubeShadowMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCubeShadowMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeShadowMS(t + string i, s))

    let samplerCubeShadowMS = SamplerCubeShadowMSBuilder()
    
    type SamplerCubeMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCubeMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeMS(t + string i, s))

    let samplerCubeMS = SamplerCubeMSBuilder()
    
    type SamplerCubeShadowBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCubeShadow(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCubeShadow(t + string i, s))

    let samplerCubeShadow = SamplerCubeShadowBuilder()
    
    type SamplerCubeBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            SamplerCube(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> SamplerCube(t + string i, s))

    let samplerCube = SamplerCubeBuilder()
    
    type IntSampler1dArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler1dArrayMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler1dArrayMS(t + string i, s))

    let intSampler1dArrayMS = IntSampler1dArrayMSBuilder()
    
    type IntSampler1dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler1dArray(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler1dArray(t + string i, s))

    let intSampler1dArray = IntSampler1dArrayBuilder()
    
    type IntSampler1dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler1dMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler1dMS(t + string i, s))

    let intSampler1dMS = IntSampler1dMSBuilder()
    
    type IntSampler1dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler1d(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler1d(t + string i, s))

    let intSampler1d = IntSampler1dBuilder()
    
    type IntSampler2dArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler2dArrayMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2dArrayMS(t + string i, s))

    let intSampler2dArrayMS = IntSampler2dArrayMSBuilder()
    
    type IntSampler2dArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler2dArray(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2dArray(t + string i, s))

    let intSampler2dArray = IntSampler2dArrayBuilder()
    
    type IntSampler2dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler2dMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2dMS(t + string i, s))

    let intSampler2dMS = IntSampler2dMSBuilder()
    
    type IntSampler2dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler2d(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler2d(t + string i, s))

    let intSampler2d = IntSampler2dBuilder()
    
    type IntSampler3dMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler3dMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler3dMS(t + string i, s))

    let intSampler3dMS = IntSampler3dMSBuilder()
    
    type IntSampler3dBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSampler3d(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSampler3d(t + string i, s))

    let intSampler3d = IntSampler3dBuilder()
    
    type IntSamplerCubeArrayMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSamplerCubeArrayMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSamplerCubeArrayMS(t + string i, s))

    let intSamplerCubeArrayMS = IntSamplerCubeArrayMSBuilder()
    
    type IntSamplerCubeArrayBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSamplerCubeArray(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSamplerCubeArray(t + string i, s))

    let intSamplerCubeArray = IntSamplerCubeArrayBuilder()
    
    type IntSamplerCubeMSBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSamplerCubeMS(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSamplerCubeMS(t + string i, s))

    let intSamplerCubeMS = IntSamplerCubeMSBuilder()
    
    type IntSamplerCubeBuilder() = 
        inherit SamplerBaseBuilder()
        member x.Run((t : string, s : SamplerState)) =
            IntSamplerCube(t, s)
        member x.Run(((t : string, count : int), s : SamplerState)) =
            Array.init count (fun i -> IntSamplerCube(t + string i, s))

    let intSamplerCube = IntSamplerCubeBuilder()
    

[<ImageType>]
type Image1dArrayMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<int>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int, slice : int, sample : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : int, slice : int, sample : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image1dArray<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<int>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int, slice : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : int, slice : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image1dMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<int>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int, sample : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : int, sample : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image1d<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<int>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image2dArrayMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i, slice : int, sample : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : V2i, slice : int, sample : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image2dArray<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i, slice : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : V2i, slice : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image2dMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i, sample : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : V2i, sample : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image2d<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i) : V4d = onlyInShaderCode "fetch"
        and set(coord : V2i) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image3dMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V3i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, sample : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : V3i, sample : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type Image3d<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i) : V4d = onlyInShaderCode "fetch"
        and set(coord : V3i) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type ImageCubeArrayMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3i>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, slice : int, sample : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : V3i, slice : int, sample : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type ImageCubeArray<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, slice : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : V3i, slice : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type ImageCubeMS<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, sample : int) : V4d = onlyInShaderCode "fetch"
        and set(coord : V3i, sample : int) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type ImageCube<'f when 'f :> Formats.IFloatingFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4d>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i) : V4d = onlyInShaderCode "fetch"
        and set(coord : V3i) (v : V4d) : unit = onlyInShaderCode "write"


[<ImageType>]
type IntImage1dArrayMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<int>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int, slice : int, sample : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : int, slice : int, sample : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : int, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : int, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : int, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : int, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : int, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : int, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : int, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : int, slice : int, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage1dArray<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<int>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int, slice : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : int, slice : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : int, slice : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : int, slice : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage1dMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<int>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int, sample : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : int, sample : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : int, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : int, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : int, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : int, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage1d<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler1d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<int>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : int = onlyInShaderCode "Size"
    member x.Item
        with get(coord : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage2dArrayMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i, slice : int, sample : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : V2i, slice : int, sample : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V2i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V2i, slice : int, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage2dArray<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i, slice : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : V2i, slice : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V2i, slice : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V2i, slice : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage2dMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i, sample : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : V2i, sample : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V2i, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V2i, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage2d<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler2d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V2i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V2i) : V4i = onlyInShaderCode "fetch"
        and set(coord : V2i) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V2i, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V2i, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V2i, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V2i, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V2i, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V2i, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V2i, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V2i, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage3dMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V3i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, sample : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : V3i, sample : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V3i, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImage3d<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.Sampler3d
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V3i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i) : V4i = onlyInShaderCode "fetch"
        and set(coord : V3i) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V3i, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V3i, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V3i, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V3i, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V3i, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V3i, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V3i, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V3i, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImageCubeArrayMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3i>
    static member IsArray = true
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, slice : int, sample : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : V3i, slice : int, sample : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V3i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V3i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V3i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V3i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V3i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V3i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V3i, slice : int, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V3i, slice : int, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImageCubeArray<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3i>
    static member IsArray = true
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, slice : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : V3i, slice : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V3i, slice : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V3i, slice : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V3i, slice : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V3i, slice : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V3i, slice : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V3i, slice : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V3i, slice : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V3i, slice : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImageCubeMS<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = true
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i, sample : int) : V4i = onlyInShaderCode "fetch"
        and set(coord : V3i, sample : int) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V3i, sample : int, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V3i, sample : int, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

[<ImageType>]
type IntImageCube<'f when 'f :> Formats.ISignedFormat>() =
    interface IImage
    static member FormatType = typeof<'f>
    static member Dimension = SamplerDimension.SamplerCube
    static member ValueType = typeof<V4i>
    static member CoordType = typeof<V3i>
    static member IsArray = false
    static member IsMultisampled = false
    
    member x.Size : V2i = onlyInShaderCode "Size"
    member x.Item
        with get(coord : V3i) : V4i = onlyInShaderCode "fetch"
        and set(coord : V3i) (v : V4i) : unit = onlyInShaderCode "write"

    member x.AtomicAdd(coord : V3i, data : int) : int = onlyInShaderCode "AtomicAdd"
    member x.AtomicMin(coord : V3i, data : int) : int = onlyInShaderCode "AtomicMin"
    member x.AtomicMax(coord : V3i, data : int) : int = onlyInShaderCode "AtomicMax"
    member x.AtomicAnd(coord : V3i, data : int) : int = onlyInShaderCode "AtomicAnd"
    member x.AtomicOr(coord : V3i, data : int) : int = onlyInShaderCode "AtomicOr"
    member x.AtomicXor(coord : V3i, data : int) : int = onlyInShaderCode "AtomicXor"
    member x.AtomicExchange(coord : V3i, data : int) : int = onlyInShaderCode "AtomicExchange"
    member x.AtomicCompareExchange(coord : V3i, cmp : int, data : int) : int = onlyInShaderCode "AtomicCompareExchange"

