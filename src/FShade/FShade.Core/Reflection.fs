namespace FShade

open System
open System.Reflection

open Aardvark.Base

open FShade

[<AutoOpen>]
module CoreReflectionPatterns = 
    let private getProp<'a> (name : string) (t : Type) =
        t.GetProperty(name).GetValue(null) |> unbox<'a>

    let private toSemAttribute (a : obj) =
        match a with
        | :? SemanticAttribute as att -> Some att
        | _ -> None
        
    type System.Type with
        member x.IsSamplerType =
            let res = x.FullName.StartsWith "FShade.Sampler" || x.FullName.StartsWith "FShade.IntSampler"
            res
            //x.GetCustomAttributes(typeof<SamplerTypeAttribute>, true) |> Seq.isEmpty |> not
        member x.IsImageType =
            x.FullName.StartsWith "FShade.Image" || x.FullName.StartsWith "FShade.IntImage"
            //x.GetCustomAttributes(typeof<ImageTypeAttribute>, true) |> Seq.isEmpty |> not
        member x.IsShaderBuilder =
            match x.FullName with
            | "FShade.ShaderBuilders.BaseBuilder"
            | "FShade.ShaderBuilders.VertexBuilder"
            | "FShade.ShaderBuilders.FragmentBuilder"
            | "FShade.ShaderBuilders.GeometryBuilder"
            | "FShade.ShaderBuilders.TessBuilder"
            | "FShade.ShaderBuilders.ComputeBuilder" -> 
                true
            | _ ->
                false
            //x.GetCustomAttributes(typeof<ImageTypeAttribute>, true) |> Seq.isEmpty |> not

    type System.Reflection.MemberInfo with
        member x.Type =
            match x with
                | :? PropertyInfo as p -> p.PropertyType
                | :? FieldInfo as f -> f.FieldType
                | _ -> failwith "no member type could be determined"

        member x.Semantic =
            let att = x.GetCustomAttributes(true) |> Seq.choose toSemAttribute |> Seq.toList
            match att with
                | x::_ -> x.Semantic
                | _ -> x.Name

        member x.Interpolation =
            let att = x.GetCustomAttributes(typeof<InterpolationAttribute>, true) |> Seq.toList
            match att with
                | x::_ -> (unbox<InterpolationAttribute> x).Qualifier
                | _ -> InterpolationMode.Default
                
        member x.DepthWriteMode =
            let att = x.GetCustomAttributes(typeof<DepthAttribute>, true) |> Seq.toList
            match att with
                | x::_ -> (unbox<DepthAttribute> x).Mode
                | _ -> DepthWriteMode.Any

        member x.PrimitiveIndex =
            let att = x.GetCustomAttributes(typeof<PrimitiveIndexAttribute>, true) |> Seq.toList
            match att with
                | x::_ -> Some (unbox<PrimitiveIndexAttribute> x).Index
                | _ -> None

    /// <summary>
    /// determines whether a given type is a Sampler and returns its properties if successful.
    /// The properties are given by SamplerType(dim, isArray, isShadow, isMS, valueType)
    /// </summary>
    let (|SamplerType|_|) (t : Type) =
        if t.IsSamplerType then
            let dim : SamplerDimension = getProp "Dimension" t
            let isArray : bool = getProp "IsArray" t
            let isShadow : bool = getProp "IsShadow" t
            let isMS : bool = getProp "IsMultisampled" t
            let valueType : Type = getProp "ValueType" t

            SamplerType(dim, isArray, isShadow, isMS, valueType) |> Some

        else
            None

    /// <summary>
    /// determines whether a given type is an Image and returns its properties if successful.
    /// The properties are given by ImageType(dim, isArray, isMS, valueType)
    /// </summary>
    let (|ImageType|_|) (t : Type) =
        if t.IsImageType then
            let dim : SamplerDimension = getProp "Dimension" t
            let isArray : bool = getProp "IsArray" t
            let isMS : bool = getProp "IsMultisampled" t
            let valueType : Type = getProp "ValueType" t
            let format : Type = getProp "FormatType" t
            ImageType(format, dim, isArray, isMS, valueType) |> Some
        else
            None

    type UniformParameter with
        member x.decorations =
            match x.uniformType with
                | ImageType(fmt,_,_,_,_) -> [Imperative.UniformDecoration.Format fmt]
                | _ -> []

type DebugRange =
    {
        file : string
        startLine : int
        startCol : int
        endLine : int
        endCol : int
    }


[<AutoOpen>]
module BasicQuotationPatterns =
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns

    let (|BuilderCall|_|) (e : Expr) =
        match e with
            | Call(Some t, mi, args) when t.Type.IsShaderBuilder ->
                BuilderCall(t, mi, args) |> Some
            | _ -> None

    let (|UniformScopeType|_|) (t : Type) =
        if t = typeof<UniformScope> then UniformScopeType |> Some
        else None

    let (|Uniform|_|) (e : Expr) =
        match e with

            | PropertyGet(None, pi, []) ->
                match pi.Type with
                    | SamplerType(_) ->
                        match Expr.TryEval e with
                            | Some sam ->
                                let sam = sam |> unbox<ISampler>
                                let tex = sam.Texture
                                Some { 
                                    uniformName = pi.Name
                                    uniformType = sam.SelfType
                                    uniformValue = Sampler(tex, sam.State) 
                                } 

                            | None ->
                                None

                    | ArrayOf((SamplerType _ as t)) ->
                        match Expr.TryEval e with
                            | Some sam ->
                                failwith "Arr missing"
                                //let arr = sam |> unbox<Array>
                                //let samplers = 
                                //    List.init arr.Length (fun i -> 
                                //        let sam1 = arr.GetValue i |> unbox<ISampler>
                                //        let tex = sam1.Texture
                                //        tex.Semantic, sam1.State
                                //    )

                                //let t = failwith "Arr missing" //Peano.getArrayType arr.Length t
                                //Some {
                                //    uniformName = pi.Name
                                //    uniformType = t
                                //    uniformValue = SamplerArray(List.toArray samplers)
                                //} 

                            | None -> 
                                None

                    | _ -> None

            | Call(None, Method("op_Dynamic", [UniformScopeType; _]), [scope; Value(s,_)]) ->
                match Expr.TryEval scope with
                    | Some scope ->
                        let scope = scope |> unbox
                        Some {
                            uniformName = unbox s
                            uniformType = e.Type
                            uniformValue = Attribute(scope, unbox s)
                        }

                    | None ->
                        None

            | PropertyGet(Some scope, p, []) when scope.Type = typeof<UniformScope> ->
                match Expr.TryEval scope with
                    | Some scope ->
                        let result = p.GetValue(scope, [||])

                        let hasSem = hasProperty "Semantic" result
                        let hasScope = hasProperty "UniformScope" result
                        if hasSem && hasScope then
                            let sem : string = Fable.Core.JsInterop.(?) result "UniformScope"
                            let scope : UniformScope = Fable.Core.JsInterop.(?) result "Scope"
                            Some {
                                uniformName = sem
                                uniformType = e.Type
                                uniformValue = Attribute(scope, sem)
                            }
                        else
                            None
                    | None ->
                        None
//                with :? TargetInvocationException as ex ->
//                    match ex.InnerException with
//                        | :? SemanticException as s -> 
//                            Some {
//                                uniformName = s.Semantic
//                                uniformType = p.PropertyType
//                                uniformValue = Attribute(s.Scope, s.Semantic)
//                            }
//
//                        | _ -> 
//                            None

            | Call(Some scope, m, [])
            | Call(None, m, [scope]) when scope.Type = typeof<UniformScope> ->
                match Expr.TryEval scope with
                    | Some scope ->
                        let result = m.Invoke(scope, [| |])
                        let hasSem = hasProperty "Semantic" result
                        let hasScope = hasProperty "UniformScope" result
                        if hasSem && hasScope then
                            let sem : string = Fable.Core.JsInterop.(?) result "Semantic"
                            let scope : UniformScope = Fable.Core.JsInterop.(?) result "UniformScope"
                            Some {
                                uniformName = sem
                                uniformType = e.Type
                                uniformValue = Attribute(scope, sem)
                            }
                        else
                            None
                    | None ->
                        None


            | _ -> None



    let (|DebugRange|_|) (e : Expr) =
        match e with
            | NewTuple([String "DebugRange"; NewTuple [String file; Int32 startLine; Int32 startCol; Int32 endLine; Int32 endCol]]) -> 
                Some { file = file; startLine = startLine; startCol = startCol; endLine = endLine; endCol = endCol }
            | _ -> None


    module Map =
        let choose (f : 'k -> 'v -> Option<'r>) (m : Map<'k, 'v>) =
            let mutable res = Map.empty
            for (k,v) in Map.toSeq m do
                match f k v with
                    | Some v -> res <- Map.add k v res
                    | _ -> ()
            res

        let keys (m : Map<'a, 'b>) = m |> Map.toSeq |> Seq.map fst
        let values (m : Map<'a, 'b>) = m |> Map.toSeq |> Seq.map snd

        let intersect (l : Map<'a, 'b>) (r : Map<'a, 'c>) =
            l |> choose (fun k lv ->
                match Map.tryFind k r with
                    | Some rv -> Some (lv, rv)
                    | None -> None
            )

        let difference (l : Map<'a, 'b>) (r : Map<'a, 'c>) =
            l |> choose (fun k lv ->
                match Map.tryFind k r with
                    | Some rv -> None
                    | None -> Some lv
            )