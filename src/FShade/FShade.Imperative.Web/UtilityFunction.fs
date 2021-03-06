﻿namespace FShade.Imperative

open System
open System.Reflection
open System.Runtime.CompilerServices
open FShade.Imperative
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Aardvark.Base
open FShade

[<AutoOpen>]
module private UtilityFunctionHelpers =
    let cleanHash (str : string) =
        let str : string = str.Replace("+", "_")
        let str : string = str.Replace("/", "00")
        let str : string = str.Replace("=", "")
        str

[<CustomEquality; NoComparison>]
type UtilityFunction =
    {
        functionId          : string
        functionName        : string
        functionArguments   : list<Var>
        functionBody        : Expr
        functionMethod      : Option<MethodBase>
        functionTag         : obj
        functionIsInline    : bool
    }
    member x.uniqueName = x.functionName + "_" + cleanHash x.functionId
    member x.returnType = x.functionBody.Type
    
    override x.GetHashCode() = x.uniqueName.GetHashCode()
    override x.Equals(o) =
        match o with
            | :? UtilityFunction as o ->
                x.functionId = o.functionId &&
                x.functionName = o.functionName &&
                List.forall2 (fun (l : Var) (r : Var) -> l.Name = r.Name && l.Type = r.Type && l.IsMutable = r.IsMutable) x.functionArguments o.functionArguments
            | _ ->
                false

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UtilityFunction =
    let tryCreate (m : MethodBase) =
        let intrinsic = m.GetCustomAttributes(typeof<IntrinsicAttribute>, true) |> Seq.isEmpty |> not
        if intrinsic then
            None
        else
            match ExprWorkardound.TryGetReflectedDefinition m with
                | Some e ->
                    let isInline = m.GetCustomAttributes(typeof<InlineAttribute>, true) |> Seq.isEmpty |> not

                    match e with
                        | Lambdas(args, body) ->
                            let args = List.concat args

                            let args, body = 
                                if m.IsStatic then 
                                    args, body
                                else
                                    match args with
                                        | this :: args ->
                                            let mThis = Var(this.Name, this.Type, true)
                                            let body = body.Substitute(fun vi -> if vi = this then Some (Expr.Var mThis) else None)
                                            mThis :: args, body
                                        | _ ->
                                            args, body

                            let args =
                                args |> List.filter (fun a -> a.Type <> typeof<unit>)

                            Some {
                                functionId = Guid.NewGuid() |> string //Expr.ComputeHash body
                                functionName = methodName m
                                functionArguments = args
                                functionBody = body
                                functionMethod = Some m
                                functionTag = null
                                functionIsInline = isInline
                            }
                        | _ ->
                            None
                | None ->
                    None

    let ofMethodBase (m : MethodBase) =
        match tryCreate m with
            | Some f -> f
            | None -> failwithf "[FShade] utility function %A is not reflectable" m

    let map (mapping : Expr -> Expr) (f : UtilityFunction) =
        let b = mapping f.functionBody
        { f with functionBody = b; functionId = Guid.NewGuid() |> string } //Expr.ComputeHash b }

   
[<AutoOpen>]
module UtilityFunctionExpressionExtensions =
    
    type Expr with

        static member CallFunction(f : UtilityFunction, args : list<Expr>) =
            assert ( List.forall2 (fun (v : Var) (e : Expr) -> v.Type = e.Type) f.functionArguments args )

            let args =
                match args with
                    | [] -> Expr.Unit
                    | args -> Expr.NewTuple args

            Expr.Coerce(
                Expr.NewTuple [Expr.Value "__FUNCTIONCALL__"; Expr.Value f; args ], 
                f.returnType
            )


    let (|CallFunction|_|) (e : Expr) =
        match e with
            | Coerce(NewTuple [ String "__FUNCTIONCALL__"; Value((:? UtilityFunction as f), _); args], t) when t = f.returnType ->
                match args with
                    | Unit -> Some(f,[])
                    | NewTuple args -> Some(f, args)
                    | _ -> None
            | _ ->
                None



[<AbstractClass; Sealed; Extension>]
type ExpressionSubstitutionExtensions private() =
    static let zero = V2i(0,0)

    static let (<+>) (l : V2i) (r : V2i) =
        V2i(l.X + r.X, l.Y + r.Y)

    static let (<|>) (l : V2i) (r : V2i) =
        V2i(min l.X r.X, max l.Y r.Y)

    static let (<*>) (l : V2i) (r : int) =
        V2i(l.X * r, l.Y * r)

    static let rec numberOfCalls (mi : MethodInfo) (e : Expr) =
        match e with
            | Call(t, m, args) ->
                if m = mi || (m.IsGenericMethod && m.GetGenericMethodDefinition() = mi) then
                    V2i(1,1)
                else
                    let args = 
                        match t with
                            | Some t -> (t :: args)
                            | None -> args
                    args |> List.fold (fun r e -> r <+> numberOfCalls mi e) zero

            | Sequential(l, r) ->
                numberOfCalls mi l <+> numberOfCalls mi r

            | IfThenElse(cond, i, e) ->
                numberOfCalls mi cond <+> (numberOfCalls mi i <|> numberOfCalls mi e)

            | ForInteger(v, first, step, last, body) ->
                let minimal = 
                    numberOfCalls mi first <+>
                    numberOfCalls mi step <+>
                    numberOfCalls mi last

                let inner = numberOfCalls mi body

                match first, step, last with
                    | Int32 first, Int32 step, Int32 last -> 
                        let cnt = [first .. step .. last] |> List.length
                        minimal <+> inner <*> cnt
                    | _ ->
                        if inner.Y = 0 then
                            minimal
                        else
                            V2i(minimal.X, Int32.MaxValue)

            | WhileLoop(guard, body) ->
                let minimal = numberOfCalls mi guard
                let inner = numberOfCalls mi body
                if inner.Y = 0 && minimal.Y = 0 then
                    V2i(0,0)
                else
                    V2i(0, Int32.MaxValue)

            | Let(v, e, b) ->
                numberOfCalls mi e <+> numberOfCalls mi b

            | ShapeVar v -> zero
            | ShapeLambda(v, b) -> numberOfCalls mi b
            | ShapeCombination(o, args) -> args |> List.fold (fun r e -> r <+> numberOfCalls mi e) zero

    static let rec substituteReads (substitute : ParameterKind -> Type -> string -> Option<Expr> -> Option<Expr>) (e : Expr) =
        match e with
            | ReadInput(kind, name, index) ->
                let index = index |> Option.map (substituteReads substitute)
                match substitute kind e.Type name index with
                    | Some e -> e
                    | None -> 
                        match index with
                            | Some index -> Expr.ReadInput(kind, e.Type, name, index)
                            | None -> e

            | CallFunction(utility, args) ->
                let args = args |> List.map (substituteReads substitute)
                let utility = utility |> UtilityFunction.map (substituteReads substitute)
                Expr.CallFunction(utility, args)

            | ShapeLambda(v,b) -> Expr.Lambda(v, substituteReads substitute b)
            | ShapeVar _ -> e
            | ShapeCombination(o, args) ->
                RebuildShapeCombination(o, args |> List.map (substituteReads substitute))

    static let rec substituteWrites (substitute : Map<string, Option<Expr> * Expr> -> Option<Expr>) (e : Expr) =
        match e with
            | WriteOutputs values ->
                match substitute values with
                    | Some e -> e
                    | None ->
                        let newValues = 
                            values |> Map.map (fun _ (index, value) -> 
                                let value = substituteWrites substitute value
                                let index = index |> Option.map (substituteWrites substitute)
                                index, value
                            )
                        Expr.WriteOutputs newValues

            | ShapeLambda(v,b) -> Expr.Lambda(v, substituteWrites substitute b)
            | ShapeVar _ -> e
            | ShapeCombination(o, args) ->
                RebuildShapeCombination(o, args |> List.map (substituteWrites substitute))
 
        
    [<Extension>]
    static member SubstituteReads (e : Expr, substitute : ParameterKind -> Type -> string -> Option<Expr> -> Option<Expr>) =
        substituteReads substitute e
        
    [<Extension>]
    static member SubstituteWrites (e : Expr, substitute : Map<string, Option<Expr> * Expr> -> Option<Expr>) =
        substituteWrites substitute e

    [<Extension>]
    static member ComputeCallCount (e : Expr, mi : MethodInfo) =
        numberOfCalls mi e


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Expr =
    let inline substitute (f : Var -> Option<Expr>) (e : Expr) = e.Substitute f
    let inline substituteReads (f : ParameterKind -> Type -> string -> Option<Expr> -> Option<Expr>) (e : Expr) = e.SubstituteReads f
    let inline substituteWrites (f : Map<string, Option<Expr> * Expr> -> Option<Expr>) (e : Expr) = e.SubstituteWrites f
    let getAffectedOutputsMap (e : Expr) = Affected.getAffectedOutputsMap e
    let computeCallCount (mi : MethodInfo) (e : Expr) = e.ComputeCallCount(mi)
