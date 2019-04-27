namespace FShade

open Aardvark.Import.JS
open System
open Aardvark.Base



type UniformScope private(parent : Option<UniformScope>, name : string) = 
    static let mutable currentId = 0

    static let glob = UniformScope(None, "Global")

    let fullName =
        match parent with
        | Some parent -> 
            if parent.IsGlobal then name 
            else parent.FullName + "_" + name
        | None -> 
            name

    let childScopes = System.Collections.Generic.Dictionary<string, UniformScope>()
      
    static member Global = glob

    member x.IsGlobal =
        match parent with
        | None -> true
        | _ -> false

    member x.FullName = fullName

    interface IComparable with
        member x.CompareTo o =
            match o with
                | :? UniformScope as o -> compare fullName o.FullName
                | _ -> failwith "uncomparable"

    override x.GetHashCode() =
        fullName.GetHashCode()

    override x.Equals(o) =
        match o with
            | :? UniformScope as o -> o.FullName = fullName
            | _ -> false

    member x.Parent = parent
    member x.Name = name

    member x.GetChildScope(n : string) =
        match childScopes.TryGetValue n with
            | (true,s) -> 
                s
            | _ -> 
                let s = UniformScope(Some x, n)
                childScopes.[n] <- s
                s
   
    static member TryCreate (o : obj) =
        match o with
        | :? UniformScope as o ->
            Some o
        | _ ->
            let hasScope = hasProperty "UniformScope" o
            let hasSemantic = hasProperty "Semantic" o
            if hasScope && hasSemantic then
                let scope : UniformScope = Fable.Core.JsInterop.(?) o "UniformScope"
                let sem : string = Fable.Core.JsInterop.(?) o "Semantic"
                scope.GetChildScope sem |> Some
            else
                None

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type ShaderBuilderAttribute() =
    inherit Attribute()

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type SemanticAttribute(s : string) =
    inherit Attribute()
    member x.Semantic = s

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type InterpolationAttribute(qualifier : InterpolationMode) =
    inherit Attribute()
    member x.Qualifier = qualifier
    
[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type DepthAttribute(mode : DepthWriteMode) =
    inherit SemanticAttribute("Depth")
    member x.Mode = mode
    new() = DepthAttribute(DepthWriteMode.Any)

[<AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field)>]
type PrimitiveIndexAttribute(index : int) =
    inherit Attribute()
    member x.Index = index

[<AbstractClass>]
type AbstractShaderBuilder() =
    abstract member ShaderStage : ShaderStage
    abstract member OutputTopology : Option<OutputTopology>


[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type SamplerTypeAttribute() =
    inherit Attribute()
    
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type ImageTypeAttribute() =
    inherit Attribute()
    

type ParameterDescription =
    {
        paramType           : Type
        paramInterpolation  : InterpolationMode
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ParameterDescription =

    let inline paramInterpolation (p : ParameterDescription) = p.paramInterpolation
    let inline paramType (p : ParameterDescription) = p.paramType

    let ofType (t : Type) =
        {
            paramType = t
            paramInterpolation = InterpolationMode.Default
        }


module Formats =
    type IFormat = interface end 
    type IFloatingFormat = inherit IFormat
    type ISignedFormat = inherit IFormat
    type IUnsignedFormat = inherit IFormat

    type rgba32f() = interface IFloatingFormat
    type rgba16f() = interface IFloatingFormat
    type rg32f() = interface IFloatingFormat
    type rg16f() = interface IFloatingFormat
    type r11g11b10f() = interface IFloatingFormat
    type r32f() = interface IFloatingFormat
    type r16f() = interface IFloatingFormat

    type rgba16() = interface IFloatingFormat
    type rgb10a2() = interface IFloatingFormat
    type rgba8() = interface IFloatingFormat
    type rg16() = interface IFloatingFormat
    type rg8() = interface IFloatingFormat
    type r16() = interface IFloatingFormat
    type r8() = interface IFloatingFormat

    type rgba16_snorm() = interface IFloatingFormat
    type rgba8_snorm() = interface IFloatingFormat
    type rg16_snorm() = interface IFloatingFormat
    type rg8_snorm() = interface IFloatingFormat
    type r16_snorm() = interface IFloatingFormat
    type r8_snorm() = interface IFloatingFormat

    type rgba32ui() = interface IUnsignedFormat
    type rgba16ui() = interface IUnsignedFormat
    type rgb10a2ui() = interface IUnsignedFormat
    type rgba8ui() = interface IUnsignedFormat
    type rg32ui() = interface IUnsignedFormat
    type rg16ui() = interface IUnsignedFormat
    type rg8ui() = interface IUnsignedFormat
    type r32ui() = interface IUnsignedFormat
    type r16ui() = interface IUnsignedFormat
    type r8ui() = interface IUnsignedFormat

    type rgba32i() = interface ISignedFormat
    type rgba16i() = interface ISignedFormat
    type rgba8i() = interface ISignedFormat
    type rg32i() = interface ISignedFormat
    type rg16i() = interface ISignedFormat
    type rg8i() = interface ISignedFormat
    type r32i() = interface ISignedFormat
    type r16i() = interface ISignedFormat
    type r8i() = interface ISignedFormat

[<AutoOpen>]
module ImageFormatExtensions =
    
    module ImageFormat =
        open Formats
        open Aardvark.Base

        let ofFormatType =
            ConversionHelpers.lookupTableOption [
                typeof<rgba32f>, ImageFormat.Rgba32f
                typeof<rgba16f>, ImageFormat.Rgba16f
                typeof<rg32f>, ImageFormat.Rg32f
                typeof<rg16f>, ImageFormat.Rg16f
                typeof<r11g11b10f>, ImageFormat.R11fG11fB10f
                typeof<r32f>, ImageFormat.R32f
                typeof<r16f>, ImageFormat.R16f

                typeof<rgba16>, ImageFormat.Rgba16
                typeof<rgb10a2>, ImageFormat.Rgb10A2
                typeof<rgba8>, ImageFormat.Rgba8
                typeof<rg16>, ImageFormat.Rg16
                typeof<rg8>, ImageFormat.Rg8
                typeof<r16>, ImageFormat.R16
                typeof<r8>, ImageFormat.R8

                typeof<rgba16_snorm>, ImageFormat.Rgba16
                typeof<rgba8_snorm>, ImageFormat.Rgba8
                typeof<rg16_snorm>, ImageFormat.Rg16
                typeof<rg8_snorm>, ImageFormat.Rg8
                typeof<r16_snorm>, ImageFormat.R16
                typeof<r8_snorm>, ImageFormat.R8

                typeof<rgba32ui>, ImageFormat.Rgba32ui
                typeof<rgba16ui>, ImageFormat.Rgba16ui
                typeof<rgb10a2ui>, ImageFormat.Rgb10A2ui
                typeof<rgba8ui>, ImageFormat.Rgba8ui
                typeof<rg32ui>, ImageFormat.Rg32ui
                typeof<rg16ui>, ImageFormat.Rg16ui
                typeof<rg8ui>, ImageFormat.Rg8ui
                typeof<r32ui>, ImageFormat.R32ui
                typeof<r16ui>, ImageFormat.R16ui
                typeof<r8ui>, ImageFormat.R8ui

                typeof<rgba32i>, ImageFormat.Rgba32i
                typeof<rgba16i>, ImageFormat.Rgba16i
                typeof<rgba8i>, ImageFormat.Rgba8i
                typeof<rg32i>, ImageFormat.Rg32i
                typeof<rg16i>, ImageFormat.Rg16i
                typeof<rg8i>, ImageFormat.Rg8i
                typeof<r32i>, ImageFormat.R32i
                typeof<r16i>, ImageFormat.R16i
                typeof<r8i>, ImageFormat.R8i
            ]




type UniformValue =
    | Attribute of scope : UniformScope * name : string
    | Sampler of textureName : string * SamplerState
    | SamplerArray of array<string * SamplerState>

type UniformParameter =
    {
        uniformName         : string
        uniformType         : Type
        uniformValue        : UniformValue
    }




type ISampler =
    abstract member SelfType : System.Type
    abstract member Texture : string
    abstract member State : SamplerState

type IImage =
    interface end

//type ShaderTextureHandle(semantic : string, scope : UniformScope) =
//    static member CreateUniform(semantic : string, scope : UniformScope) = ShaderTextureHandle(semantic, scope)

//    override x.Semantic = semantic
//    override x.Scope = scope

//    member x.WithIndex (i : int) =
//        ShaderTextureHandle(semantic + string i, scope)

//    new() = ShaderTextureHandle(null, Unchecked.defaultof<UniformScope>)       
    

type TextureMustBeSpecified = TextureMustBeSpecified

type SamplerBaseBuilder() =
    member x.Yield(_) = TextureMustBeSpecified

    [<CustomOperation("texture")>]
    member x.Texture(b : TextureMustBeSpecified, t : string) =
        let t = 
            if hasProperty "Semantic" t then Fable.Core.JsInterop.(?) t "Semantic"
            else t
        
        (t, SamplerState.empty)

    [<CustomOperation("textureArray")>]
    member x.TextureArray(b : TextureMustBeSpecified, t : string, count : int) =
        let t = 
            if hasProperty "Semantic" t then Fable.Core.JsInterop.(?) t "Semantic"
            else t

        ((t, count), SamplerState.empty)


    [<CustomOperation("addressU")>]
    member x.AddressU((t, h : SamplerState), w : WrapMode) = t,{ h with AddressU = Some w }
             
    [<CustomOperation("addressV")>]
    member x.AddressV((t, h : SamplerState), w : WrapMode) = t,{ h with AddressV = Some w }
             
    [<CustomOperation("addressW")>]
    member x.AddressW((t, h : SamplerState), w : WrapMode) = t,{ h with AddressW = Some w }
             
    [<CustomOperation("maxAnisotropy")>]
    member x.MaxAnisotropy((t, h : SamplerState), a : int) = t,{ h with MaxAnisotropy = Some a }
             
    [<CustomOperation("borderColor")>]
    member x.BorderColor((t, h : SamplerState), c : V4d) = t,{ h with BorderColor = Some c }
             
    [<CustomOperation("maxLod")>]
    member x.MaxLod((t, h : SamplerState), c : float) = t,{ h with MaxLod = Some c }
             
    [<CustomOperation("minLod")>]
    member x.MinLod((t, h : SamplerState), c : float) = t,{ h with MinLod = Some c }
             
    [<CustomOperation("mipLodBias")>]
    member x.MipLodBias((t, h : SamplerState), c : float) = t,{ h with MipLodBias = Some c }
             
    [<CustomOperation("filter")>]
    member x.Filter((t, h : SamplerState), f : Filter) = t,{ h with Filter = Some f }

    [<CustomOperation("comparison")>]
    member x.Comparison((t, h : SamplerState), f : ComparisonFunction) = t,{ h with Comparison = Some f }

[<AutoOpen>]
module UniformExtensions =

    let uniform = UniformScope.Global //(None, "Global")


    let (?) (s : UniformScope) (name : string) : 'a = 
        match UniformScope.TryCreate s with
        | Some s ->
            let o = obj()
            Fable.Core.JsInterop.(?<-) o "UniformScope" s
            Fable.Core.JsInterop.(?<-) o "Semantic" name
            unbox o
        | None ->
            failwithf "not a UniforScope: %A" s
     

[<AutoOpen>]
module SplicingExtensions =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.ExprShape


    let rec (|ExprValue|_|) (e : Expr) =
        match e with
            | Coerce(ExprValue v, _) -> Some v
            | Value((:? Expr as v),_) -> Some v
            | _ -> None

    let rec private removeValueNames (e : Expr) =
        match e with
            | ValueWithName(v, t, _) -> Expr.Value(v, t)
            | ShapeVar _ -> e
            | ShapeLambda(v,b) -> Expr.Lambda(v, removeValueNames b)
            | ShapeCombination(o, args) ->
                RebuildShapeCombination(o, args |> List.map removeValueNames)

    let rec private inlineSplices (e : Expr) =
        match e with
            | Call(None, mi, [ExprValue v]) when mi.Name = "op_Splice" || mi.Name = "op_SpliceUntyped" ->
                if v.Type = e.Type then
                    removeValueNames v
                else
                    Expr.Coerce(removeValueNames v, e.Type)
        
            | ShapeVar v -> Expr.Var v
            | ShapeLambda(v,b) -> Expr.Lambda(v, inlineSplices b)
            | ShapeCombination(o, args) -> RebuildShapeCombination(o, args |> List.map inlineSplices)
            

    type Expr with
        static member InlineSplices (e : Expr) =
            inlineSplices e

        member x.InlineSplices() =
            inlineSplices x