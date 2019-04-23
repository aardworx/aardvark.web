namespace FShade

open System
open System.Reflection

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Reflection

open Aardvark.Base
open FShade.Imperative


/// the constant folding pass tries to evaluate constant expressions throughout the tree.
/// e.g. `let a = 10 * 2 + 3 in ...` translates to `let a = 23 in ...` and so on.
/// NOTE: since some built-in F# functions cannot be invoked dynamically (using reflection) the
///       module contains a list of many functions but may print a warning in certain scenarios.
module ConstantFolding =
    
    type State = 
        {
            variableValues : Map<Var, obj>
            isGlobalSideEffect : MemberInfo -> bool
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let empty = 
            { 
                variableValues = Map.empty
                isGlobalSideEffect = fun mi -> 
                    match mi with
                        | :? MethodInfo as mi -> mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<System.Void>
                        | _ -> false
            }

        let needsCall (mi : MethodInfo) : State<State, bool> =
            State.get |> State.map (fun s -> 
                (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.ReadInput) ||
                (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.ReadInputIndexed) ||
                (mi = MethodInfo.WriteOutputs) || 
                (mi = MethodInfo.Unroll) ||
                (mi.IsGenericMethod && mi.GetGenericMethodDefinition() = MethodInfo.NewRef) ||
                (s.isGlobalSideEffect (mi :> MemberInfo))
            )

        let needsField (f : FieldInfo) : State<State, bool> =
            State.get |> State.map (fun s ->
                s.isGlobalSideEffect (f :> MemberInfo)
            )

        let needsProperty (p : PropertyInfo) : State<State, bool> =
            State.get |> State.map (fun s ->
                s.isGlobalSideEffect (p :> MemberInfo)
            )

        let setVar (v : Var) (value : obj) =
            State.modify (fun s ->
                { s with variableValues = Map.add v value s.variableValues }
            )

        let getVar (v : Var) =
            State.get |> State.map (fun s ->
                Map.tryFind v s.variableValues
            )

    let rec private (|AllConstant|_|) (e : list<Expr>) =
        match e with
            | [] -> Some []
            | h :: t ->
                match h, t with
                    | Value(h,_), AllConstant t -> Some (h :: t)
                    | _ -> None

    open Fable.Core.JsInterop

    type FunctionTrampoline () =
        static member FTramp (a : 'a, [<Fable.Core.Inject>] ?ra : Fable.Core.ITypeResolver<'a>) =
            ra.Value.ResolveType(), a :> obj

    let inline tramp (v : 'a) = FunctionTrampoline.FTramp v


    let functionTable (l : list<string * (Type * obj)>) =
        let table = Dict(Unchecked.hash, Unchecked.equals)

        let rec getFunctionElements (t : Type) =
            if t.Name.StartsWith "FSharpFunc" then
                let a, r = FSharpType.GetFunctionElements t
                let args, ret = getFunctionElements r
                a :: args, ret
            else
                [], t

        for (name, (fType, f)) in l do
            let args, ret = getFunctionElements fType

            let rec run (f : obj) (args : list<obj>) : obj = 
                match args with
                | h :: t -> 
                    let aasd = f?apply(null, [|h|])
                    run aasd t
                | [] ->
                    f

            

            table.[(name, args)] <- run f

        table

    let operators = 
        functionTable [
            "op_UnaryNegation", ((~-) : int8 -> int8) |> tramp
            "op_UnaryNegation", ((~-) : int16 -> int16) |> tramp
            "op_UnaryNegation", ((~-) : int32 -> int32) |> tramp
            "op_UnaryNegation", ((~-) : int64 -> int64) |> tramp
            "op_UnaryNegation", ((~-) : float32 -> float32) |> tramp
            "op_UnaryNegation", ((~-) : float -> float) |> tramp
            "op_UnaryNegation", ((~-) : decimal -> decimal) |> tramp

            "op_Addition", ((+) : int8 -> int8 -> int8) |> tramp
            "op_Addition", ((+) : int16 -> int16 -> int16) |> tramp
            "op_Addition", ((+) : int32 -> int32 -> int32) |> tramp
            "op_Addition", ((+) : int64 -> int64 -> int64) |> tramp
            "op_Addition", ((+) : uint8 -> uint8 -> uint8) |> tramp
            "op_Addition", ((+) : uint16 -> uint16 -> uint16) |> tramp
            "op_Addition", ((+) : uint32 -> uint32 -> uint32) |> tramp
            "op_Addition", ((+) : uint64 -> uint64 -> uint64) |> tramp
            "op_Addition", ((+) : float32 -> float32 -> float32) |> tramp
            "op_Addition", ((+) : float -> float -> float) |> tramp
            "op_Addition", ((+) : decimal -> decimal -> decimal) |> tramp

            
            "op_Subtraction", ((-) : int8 -> int8 -> int8) |> tramp
            "op_Subtraction", ((-) : int16 -> int16 -> int16) |> tramp
            "op_Subtraction", ((-) : int32 -> int32 -> int32) |> tramp
            "op_Subtraction", ((-) : int64 -> int64 -> int64) |> tramp
            "op_Subtraction", ((-) : uint8 -> uint8 -> uint8) |> tramp
            "op_Subtraction", ((-) : uint16 -> uint16 -> uint16) |> tramp
            "op_Subtraction", ((-) : uint32 -> uint32 -> uint32) |> tramp
            "op_Subtraction", ((-) : uint64 -> uint64 -> uint64) |> tramp
            "op_Subtraction", ((-) : float32 -> float32 -> float32) |> tramp
            "op_Subtraction", ((-) : float -> float -> float) |> tramp
            "op_Subtraction", ((-) : decimal -> decimal -> decimal) |> tramp

            
            "op_Multiply", ((*) : int8 -> int8 -> int8) |> tramp
            "op_Multiply", ((*) : int16 -> int16 -> int16) |> tramp
            "op_Multiply", ((*) : int32 -> int32 -> int32) |> tramp
            "op_Multiply", ((*) : int64 -> int64 -> int64) |> tramp
            "op_Multiply", ((*) : uint8 -> uint8 -> uint8) |> tramp
            "op_Multiply", ((*) : uint16 -> uint16 -> uint16) |> tramp
            "op_Multiply", ((*) : uint32 -> uint32 -> uint32) |> tramp
            "op_Multiply", ((*) : uint64 -> uint64 -> uint64) |> tramp
            "op_Multiply", ((*) : float32 -> float32 -> float32) |> tramp
            "op_Multiply", ((*) : float -> float -> float) |> tramp
            "op_Multiply", ((*) : decimal -> decimal -> decimal) |> tramp

            
            "op_Division", ((/) : int8 -> int8 -> int8) |> tramp
            "op_Division", ((/) : int16 -> int16 -> int16) |> tramp
            "op_Division", ((/) : int32 -> int32 -> int32) |> tramp
            "op_Division", ((/) : int64 -> int64 -> int64) |> tramp
            "op_Division", ((/) : uint8 -> uint8 -> uint8) |> tramp
            "op_Division", ((/) : uint16 -> uint16 -> uint16) |> tramp
            "op_Division", ((/) : uint32 -> uint32 -> uint32) |> tramp
            "op_Division", ((/) : uint64 -> uint64 -> uint64) |> tramp
            "op_Division", ((/) : float32 -> float32 -> float32) |> tramp
            "op_Division", ((/) : float -> float -> float) |> tramp
            "op_Division", ((/) : decimal -> decimal -> decimal) |> tramp

            
            "op_Modulus", ((%) : int8 -> int8 -> int8) |> tramp
            "op_Modulus", ((%) : int16 -> int16 -> int16) |> tramp
            "op_Modulus", ((%) : int32 -> int32 -> int32) |> tramp
            "op_Modulus", ((%) : int64 -> int64 -> int64) |> tramp
            "op_Modulus", ((%) : uint8 -> uint8 -> uint8) |> tramp
            "op_Modulus", ((%) : uint16 -> uint16 -> uint16) |> tramp
            "op_Modulus", ((%) : uint32 -> uint32 -> uint32) |> tramp
            "op_Modulus", ((%) : uint64 -> uint64 -> uint64) |> tramp
            "op_Modulus", ((%) : float32 -> float32 -> float32) |> tramp
            "op_Modulus", ((%) : float -> float -> float) |> tramp
            "op_Modulus", ((%) : decimal -> decimal -> decimal) |> tramp

            
            "op_BitwiseOr", ((|||) : int8 -> int8 -> int8) |> tramp
            "op_BitwiseOr", ((|||) : int16 -> int16 -> int16) |> tramp
            "op_BitwiseOr", ((|||) : int32 -> int32 -> int32) |> tramp
            "op_BitwiseOr", ((|||) : int64 -> int64 -> int64) |> tramp
            "op_BitwiseOr", ((|||) : uint8 -> uint8 -> uint8) |> tramp
            "op_BitwiseOr", ((|||) : uint16 -> uint16 -> uint16) |> tramp
            "op_BitwiseOr", ((|||) : uint32 -> uint32 -> uint32) |> tramp
            "op_BitwiseOr", ((|||) : uint64 -> uint64 -> uint64) |> tramp

            
            "op_BitwiseAnd", ((&&&) : int8 -> int8 -> int8) |> tramp
            "op_BitwiseAnd", ((&&&) : int16 -> int16 -> int16) |> tramp
            "op_BitwiseAnd", ((&&&) : int32 -> int32 -> int32) |> tramp
            "op_BitwiseAnd", ((&&&) : int64 -> int64 -> int64) |> tramp
            "op_BitwiseAnd", ((&&&) : uint8 -> uint8 -> uint8) |> tramp
            "op_BitwiseAnd", ((&&&) : uint16 -> uint16 -> uint16) |> tramp
            "op_BitwiseAnd", ((&&&) : uint32 -> uint32 -> uint32) |> tramp
            "op_BitwiseAnd", ((&&&) : uint64 -> uint64 -> uint64) |> tramp

            
            "op_ExclusiveOr", ((^^^) : int8 -> int8 -> int8) |> tramp
            "op_ExclusiveOr", ((^^^) : int16 -> int16 -> int16) |> tramp
            "op_ExclusiveOr", ((^^^) : int32 -> int32 -> int32) |> tramp
            "op_ExclusiveOr", ((^^^) : int64 -> int64 -> int64) |> tramp
            "op_ExclusiveOr", ((^^^) : uint8 -> uint8 -> uint8) |> tramp
            "op_ExclusiveOr", ((^^^) : uint16 -> uint16 -> uint16) |> tramp
            "op_ExclusiveOr", ((^^^) : uint32 -> uint32 -> uint32) |> tramp
            "op_ExclusiveOr", ((^^^) : uint64 -> uint64 -> uint64) |> tramp

            
            "op_LessThan", ((<) : int8 -> int8 -> bool) |> tramp
            "op_LessThan", ((<) : int16 -> int16 -> bool) |> tramp
            "op_LessThan", ((<) : int32 -> int32 -> bool) |> tramp
            "op_LessThan", ((<) : int64 -> int64 -> bool) |> tramp
            "op_LessThan", ((<) : uint8 -> uint8 -> bool) |> tramp
            "op_LessThan", ((<) : uint16 -> uint16 -> bool) |> tramp
            "op_LessThan", ((<) : uint32 -> uint32 -> bool) |> tramp
            "op_LessThan", ((<) : uint64 -> uint64 -> bool) |> tramp
            "op_LessThan", ((<) : float32 -> float32 -> bool) |> tramp
            "op_LessThan", ((<) : float -> float -> bool) |> tramp
            "op_LessThan", ((<) : decimal -> decimal -> bool) |> tramp

            
            "op_LessThanOrEqual", ((<=) : int8 -> int8 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : int16 -> int16 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : int32 -> int32 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : int64 -> int64 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : uint8 -> uint8 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : uint16 -> uint16 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : uint32 -> uint32 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : uint64 -> uint64 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : float32 -> float32 -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : float -> float -> bool) |> tramp
            "op_LessThanOrEqual", ((<=) : decimal -> decimal -> bool) |> tramp

            
            "op_GreaterThan", ((>) : int8 -> int8 -> bool) |> tramp
            "op_GreaterThan", ((>) : int16 -> int16 -> bool) |> tramp
            "op_GreaterThan", ((>) : int32 -> int32 -> bool) |> tramp
            "op_GreaterThan", ((>) : int64 -> int64 -> bool) |> tramp
            "op_GreaterThan", ((>) : uint8 -> uint8 -> bool) |> tramp
            "op_GreaterThan", ((>) : uint16 -> uint16 -> bool) |> tramp
            "op_GreaterThan", ((>) : uint32 -> uint32 -> bool) |> tramp
            "op_GreaterThan", ((>) : uint64 -> uint64 -> bool) |> tramp
            "op_GreaterThan", ((>) : float32 -> float32 -> bool) |> tramp
            "op_GreaterThan", ((>) : float -> float -> bool) |> tramp
            "op_GreaterThan", ((>) : decimal -> decimal -> bool) |> tramp

            
            "op_GreaterThanOrEqual", ((>=) : int8 -> int8 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : int16 -> int16 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : int32 -> int32 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : int64 -> int64 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : uint8 -> uint8 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : uint16 -> uint16 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : uint32 -> uint32 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : uint64 -> uint64 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : float32 -> float32 -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : float -> float -> bool) |> tramp
            "op_GreaterThanOrEqual", ((>=) : decimal -> decimal -> bool) |> tramp

            
            "op_Equality", ((=) : int8 -> int8 -> bool) |> tramp
            "op_Equality", ((=) : int16 -> int16 -> bool) |> tramp
            "op_Equality", ((=) : int32 -> int32 -> bool) |> tramp
            "op_Equality", ((=) : int64 -> int64 -> bool) |> tramp
            "op_Equality", ((=) : uint8 -> uint8 -> bool) |> tramp
            "op_Equality", ((=) : uint16 -> uint16 -> bool) |> tramp
            "op_Equality", ((=) : uint32 -> uint32 -> bool) |> tramp
            "op_Equality", ((=) : uint64 -> uint64 -> bool) |> tramp
            "op_Equality", ((=) : float32 -> float32 -> bool) |> tramp
            "op_Equality", ((=) : float -> float -> bool) |> tramp
            "op_Equality", ((=) : decimal -> decimal -> bool) |> tramp

            
            "op_Inequality", ((<>) : int8 -> int8 -> bool) |> tramp
            "op_Inequality", ((<>) : int16 -> int16 -> bool) |> tramp
            "op_Inequality", ((<>) : int32 -> int32 -> bool) |> tramp
            "op_Inequality", ((<>) : int64 -> int64 -> bool) |> tramp
            "op_Inequality", ((<>) : uint8 -> uint8 -> bool) |> tramp
            "op_Inequality", ((<>) : uint16 -> uint16 -> bool) |> tramp
            "op_Inequality", ((<>) : uint32 -> uint32 -> bool) |> tramp
            "op_Inequality", ((<>) : uint64 -> uint64 -> bool) |> tramp
            "op_Inequality", ((<>) : float32 -> float32 -> bool) |> tramp
            "op_Inequality", ((<>) : float -> float -> bool) |> tramp
            "op_Inequality", ((<>) : decimal -> decimal -> bool) |> tramp

            
            "ToSByte", (int8 : int8 -> _) |> tramp
            "ToSByte", (int8 : int16 -> _) |> tramp
            "ToSByte", (int8 : int32 -> _) |> tramp
            "ToSByte", (int8 : int64 -> _) |> tramp
            "ToSByte", (int8 : uint8 -> _) |> tramp
            "ToSByte", (int8 : uint16 -> _) |> tramp
            "ToSByte", (int8 : uint32 -> _) |> tramp
            "ToSByte", (int8 : uint64 -> _) |> tramp
            "ToSByte", (int8 : float32 -> _) |> tramp
            "ToSByte", (int8 : float -> _) |> tramp
            "ToSByte", (int8 : decimal -> _) |> tramp
            
            "ToByte", (uint8 : int8 -> _) |> tramp
            "ToByte", (uint8 : int16 -> _) |> tramp
            "ToByte", (uint8 : int32 -> _) |> tramp
            "ToByte", (uint8 : int64 -> _) |> tramp
            "ToByte", (uint8 : uint8 -> _) |> tramp
            "ToByte", (uint8 : uint16 -> _) |> tramp
            "ToByte", (uint8 : uint32 -> _) |> tramp
            "ToByte", (uint8 : uint64 -> _) |> tramp
            "ToByte", (uint8 : float32 -> _) |> tramp
            "ToByte", (uint8 : float -> _) |> tramp
            "ToByte", (uint8 : decimal -> _) |> tramp
            
            "ToInt16", (int16 : int8 -> _) |> tramp
            "ToInt16", (int16 : int16 -> _) |> tramp
            "ToInt16", (int16 : int32 -> _) |> tramp
            "ToInt16", (int16 : int64 -> _) |> tramp
            "ToInt16", (int16 : uint8 -> _) |> tramp
            "ToInt16", (int16 : uint16 -> _) |> tramp
            "ToInt16", (int16 : uint32 -> _) |> tramp
            "ToInt16", (int16 : uint64 -> _) |> tramp
            "ToInt16", (int16 : float32 -> _) |> tramp
            "ToInt16", (int16 : float -> _) |> tramp
            "ToInt16", (int16 : decimal -> _) |> tramp
            
            "ToUInt16", (uint16 : int8 -> _) |> tramp
            "ToUInt16", (uint16 : int16 -> _) |> tramp
            "ToUInt16", (uint16 : int32 -> _) |> tramp
            "ToUInt16", (uint16 : int64 -> _) |> tramp
            "ToUInt16", (uint16 : uint8 -> _) |> tramp
            "ToUInt16", (uint16 : uint16 -> _) |> tramp
            "ToUInt16", (uint16 : uint32 -> _) |> tramp
            "ToUInt16", (uint16 : uint64 -> _) |> tramp
            "ToUInt16", (uint16 : float32 -> _) |> tramp
            "ToUInt16", (uint16 : float -> _) |> tramp
            "ToUInt16", (uint16 : decimal -> _) |> tramp
            
            
            "ToInt", (int : int8 -> _) |> tramp
            "ToInt", (int : int16 -> _) |> tramp
            "ToInt", (int : int32 -> _) |> tramp
            "ToInt", (int : int64 -> _) |> tramp
            "ToInt", (int : uint8 -> _) |> tramp
            "ToInt", (int : uint16 -> _) |> tramp
            "ToInt", (int : uint32 -> _) |> tramp
            "ToInt", (int : uint64 -> _) |> tramp
            "ToInt", (int : float32 -> _) |> tramp
            "ToInt", (int : float -> _) |> tramp
            "ToInt", (int : decimal -> _) |> tramp
            
            "ToUInt32", (uint32 : int8 -> _) |> tramp
            "ToUInt32", (uint32 : int16 -> _) |> tramp
            "ToUInt32", (uint32 : int32 -> _) |> tramp
            "ToUInt32", (uint32 : int64 -> _) |> tramp
            "ToUInt32", (uint32 : uint8 -> _) |> tramp
            "ToUInt32", (uint32 : uint16 -> _) |> tramp
            "ToUInt32", (uint32 : uint32 -> _) |> tramp
            "ToUInt32", (uint32 : uint64 -> _) |> tramp
            "ToUInt32", (uint32 : float32 -> _) |> tramp
            "ToUInt32", (uint32 : float -> _) |> tramp
            "ToUInt32", (uint32 : decimal -> _) |> tramp
            
            "ToInt64", (int64 : int8 -> _) |> tramp
            "ToInt64", (int64 : int16 -> _) |> tramp
            "ToInt64", (int64 : int32 -> _) |> tramp
            "ToInt64", (int64 : int64 -> _) |> tramp
            "ToInt64", (int64 : uint8 -> _) |> tramp
            "ToInt64", (int64 : uint16 -> _) |> tramp
            "ToInt64", (int64 : uint32 -> _) |> tramp
            "ToInt64", (int64 : uint64 -> _) |> tramp
            "ToInt64", (int64 : float32 -> _) |> tramp
            "ToInt64", (int64 : float -> _) |> tramp
            "ToInt64", (int64 : decimal -> _) |> tramp
            
            "ToUInt64", (uint64 : int8 -> _) |> tramp
            "ToUInt64", (uint64 : int16 -> _) |> tramp
            "ToUInt64", (uint64 : int32 -> _) |> tramp
            "ToUInt64", (uint64 : int64 -> _) |> tramp
            "ToUInt64", (uint64 : uint8 -> _) |> tramp
            "ToUInt64", (uint64 : uint16 -> _) |> tramp
            "ToUInt64", (uint64 : uint32 -> _) |> tramp
            "ToUInt64", (uint64 : uint64 -> _) |> tramp
            "ToUInt64", (uint64 : float32 -> _) |> tramp
            "ToUInt64", (uint64 : float -> _) |> tramp
            "ToUInt64", (uint64 : decimal -> _) |> tramp
            
            "ToSingle", (float32 : int8 -> _) |> tramp
            "ToSingle", (float32 : int16 -> _) |> tramp
            "ToSingle", (float32 : int32 -> _) |> tramp
            "ToSingle", (float32 : int64 -> _) |> tramp
            "ToSingle", (float32 : uint8 -> _) |> tramp
            "ToSingle", (float32 : uint16 -> _) |> tramp
            "ToSingle", (float32 : uint32 -> _) |> tramp
            "ToSingle", (float32 : uint64 -> _) |> tramp
            "ToSingle", (float32 : float32 -> _) |> tramp
            "ToSingle", (float32 : float -> _) |> tramp
            "ToSingle", (float32 : decimal -> _) |> tramp
            
            "ToDouble", (float : int8 -> _) |> tramp
            "ToDouble", (float : int16 -> _) |> tramp
            "ToDouble", (float : int32 -> _) |> tramp
            "ToDouble", (float : int64 -> _) |> tramp
            "ToDouble", (float : uint8 -> _) |> tramp
            "ToDouble", (float : uint16 -> _) |> tramp
            "ToDouble", (float : uint32 -> _) |> tramp
            "ToDouble", (float : uint64 -> _) |> tramp
            "ToDouble", (float : float32 -> _) |> tramp
            "ToDouble", (float : float -> _) |> tramp
            "ToDouble", (float : decimal -> _) |> tramp


        ]


    let rec (|SeqCons|_|) (e : Expr) =
        match e with
            | Unit ->
                None

            | Sequential(SeqCons(head, tail), r) ->
                let tail = 
                    match tail with
                        | Some t -> Expr.Sequential(t, r) |> Some
                        | None -> Some r
                Some (head, tail)
                

            | e -> 
                Some (e, None)
  
    let rec (|SeqCons2|_|) (e : Expr) =
        match e with
            | SeqCons(a, Some (SeqCons(b, c))) ->
                Some (a,b,c)
            | _ ->
                None
   
    let rec evaluateConstantsS (e : Expr) =
        state {
            match e with
                | WriteOutputs values ->
                    let! values = 
                        values |> Map.mapS (fun _ (i, v) -> 
                            state {
                                let! v = 
                                    match v with
                                        | Coerce(v, t) -> evaluateConstantsS v |> State.map (fun v -> Expr.Coerce(v, t))
                                        | v -> evaluateConstantsS v

                                let! i = i |> Option.mapS evaluateConstantsS
                                return i, v
                            }
                                    
                        )
                    return Expr.WriteOutputs(values)



                | AddressOf e ->
                    let! e = evaluateConstantsS e
                    return Expr.AddressOf(e)

                | AddressSet(v, e) ->
                    let! v = evaluateConstantsS v
                    let! e = evaluateConstantsS e
                    return Expr.AddressSet(v, e)
                    
             

                | Application(lambda, arg) ->
                    let! lambda = evaluateConstantsS lambda
                    let! arg = evaluateConstantsS arg
                    match lambda, arg with
                        | Value(lambda, lt), Value(arg, at) ->
                            let mi = lt.GetMethod("Invoke", [|at|])
                            return Expr.Value(mi.Invoke(lambda, [|arg|]), e.Type)
                        | _ ->
                            return Expr.Application(lambda, arg)

                | SeqCons2((Unroll as u), ForInteger(v, first, step, last, body), rest) ->
                    let! first = evaluateConstantsS first
                    let! step = evaluateConstantsS step
                    let! last = evaluateConstantsS last
                    
                    match first, step, last with
                        | Int32 f, Int32 s, Int32 l ->
                            let! unrolled = 
                                [f .. s .. l] |> List.mapS (fun i -> 
                                    let value = Expr.Value(i)
                                    let body = body.Substitute(fun vi -> if vi = v then Some value else None)
                                    evaluateConstantsS body
                                )
                            match rest with
                                | Some rest -> 
                                    let! rest = evaluateConstantsS rest
                                    return Expr.Seq [Expr.Seq unrolled; rest]
                                | None ->
                                    return Expr.Seq unrolled
                        | _ ->
                            let! body = evaluateConstantsS body
                            match rest with
                                | Some rest -> 
                                    let! rest = evaluateConstantsS rest
                                    match body with
                                        | Value _ -> return rest
                                        | _ -> 
                                            return Expr.Seq [u; Expr.ForInteger(v, first, step, last, body); rest]
                                | None ->
                                    return Expr.Seq [u; Expr.ForInteger(v, first, step, last, body)]


                    

                | ForInteger(v, first, step, last, body) ->
                    let! first = evaluateConstantsS first
                    let! step = evaluateConstantsS step
                    let! last = evaluateConstantsS last
                    let! body = evaluateConstantsS body

                    match first, step, last with
                        | Int32 f, Int32 s, Int32 l ->
                            if s > 0 && l < f then return Expr.Unit
                            elif s < 0 && l > f then return Expr.Unit
                            else return Expr.ForInteger(v, first, step, last, body)
                        | _ ->

                            match body with
                                | Value _ -> return Expr.Unit
                                | _ -> return Expr.ForInteger(v, first, step, last, body)
                
                | ForEach(v, s, b) ->
                    let! s = evaluateConstantsS s
                    let! b = evaluateConstantsS b
                    match b with
                        | Unit -> return Expr.Unit
                        | _ -> return Expr.ForEach(v, s, b)

                | CallFunction(utility, args) ->
                    let! args = args |> List.mapS evaluateConstantsS
                    let! s = State.get
                    let utility =
                        utility |> UtilityFunction.map (fun e ->
                            let innerState = ref { State.empty with isGlobalSideEffect = s.isGlobalSideEffect }
                            evaluateConstantsS(e).Run(innerState)
                        )

                    return Expr.CallFunction(utility, args)
                    

                | Call(None, mi, [v]) when mi.Name = "op_Splice" || mi.Name = "op_SpliceUntyped" ->
                    let! v = evaluateConstantsS v
                    match v with
                        | ExprValue ve -> 
                            let! ve = evaluateConstantsS ve
                            return ve
                        | _ -> 
                            return Expr.Call(mi, [v])
   
                | Call(None, mi, args) ->
                    let! needed = State.needsCall mi
                    let! args = args |> List.mapS evaluateConstantsS
                    
                    if needed then
                        return Expr.Call(mi, args)
                    else
                        match args with
                            | AllConstant values ->
                                let key = mi.Name, args |> List.map (fun v -> v.Type)
                                match operators.TryGetValue key with
                                    | Some f -> 
                                        return Expr.Value(f values, e.Type)
                                    | _ -> 
                                        let value = 
                                            try mi.Invoke(null, values |> List.toArray) |> Some
                                            with 
                                                | ShaderOnlyExn _ -> None
                                                | _ -> 
                                                    Log.warn "[FShade] could not evaluate: %A" mi
                                                    None

                                        match value with
                                            | Some v -> return Expr.Value(v, e.Type)
                                            | None -> return Expr.Call(mi, args)
                            | _ ->
                                return Expr.Call(mi, args) 

                | Call(Some t, mi, args) ->
                    let! needed = State.needsCall mi
                    let! t = evaluateConstantsS t
                    let! args = args |> List.mapS evaluateConstantsS
                    
                    if needed then
                        return Expr.Call(t, mi, args)
                    else
                        match t, args with
                            | Value(tv,_), AllConstant values -> 
                                
                                let value = 
                                    try mi.Invoke(tv, values |> List.toArray) |> Some
                                    with 
                                        | ShaderOnlyExn _ -> None
                                        | _ -> 
                                            Log.warn "[FShade] could not evaluate: %A" mi
                                            None
                                match value with    
                                    | Some v -> return Expr.Value(v, e.Type)
                                    | None -> return Expr.Call(t, mi, args)
                            | _ ->
                                return Expr.Call(t, mi, args)

                | Coerce(e, t) ->
                    let! e = evaluateConstantsS e
                    match e with
                        | Value(e,_) -> return Expr.Value(e, t)
                        | _ -> return Expr.Coerce(e, t)

                | DefaultValue t ->
                    return Expr.DefaultValue t

                | FieldGet(None, f) ->
                    let! n = State.needsField f
                    if n then return e
                    else return Expr.Value(f.GetValue null, e.Type)

                | FieldGet(Some t, f) ->
                    let! t = evaluateConstantsS t
                    let! n = State.needsField f
                    match t with
                        | Value(t,_) when not n && unbox t ->  return Expr.Value(f.GetValue t, e.Type)
                        | _ -> return Expr.FieldGet(t, f)

                | FieldSet(None, f, value) ->
                    let! value = evaluateConstantsS value
                    return Expr.FieldSet(f, value)

                | FieldSet(Some t, f, value) ->
                    let! t = evaluateConstantsS t
                    let! value = evaluateConstantsS value
                    return Expr.FieldSet(t, f, value)

                | IfThenElse(cond, i, e) ->
                    let! cond = evaluateConstantsS cond
                    match cond with
                        | Bool true -> 
                            return! evaluateConstantsS i

                        | Bool false -> 
                            return! evaluateConstantsS e

                        | _ ->
                            let! i = evaluateConstantsS i
                            let! e = evaluateConstantsS e
                            return Expr.IfThenElse(cond, i, e)

                | Lambda(v, b) ->
                    let! b = evaluateConstantsS b
                    return Expr.Lambda(v, b)

                | Let(v, e, b) ->
                    let! e = evaluateConstantsS e
                    match e with
                        | Value(value,_) when not v.IsMutable ->
                            do! State.setVar v value
                            return! evaluateConstantsS b
                        | _ ->
                            let! b = evaluateConstantsS b
                            return Expr.Let(v, e, b)

                | NewArray(t, args) ->
                    let! args = args |> List.mapS evaluateConstantsS
                    return Expr.NewArray(t, args)

                | NewDelegate(t, vars, body) ->
                    let! body = evaluateConstantsS body
                    return Expr.NewDelegate(t, vars, body)

                | NewObject(ctor, args) ->
                    let! args = args |> List.mapS evaluateConstantsS
                    match args with
                        | AllConstant args ->
                            return Expr.Value(ctor.Invoke(List.toArray args), e.Type)
                        | _ ->
                            return Expr.NewObject(ctor, args)
                    
                | NewRecord(t, args) ->
                    let! args = args |> List.mapS evaluateConstantsS
                    match args with
                        | AllConstant args ->
                            return Expr.Value(FSharpValue.MakeRecord(t, List.toArray args, true), e.Type)
                        | _ ->
                            return Expr.NewRecord(t, args)

                | NewTuple(args) ->
                    let! args = args |> List.mapS evaluateConstantsS
                    match args with
                        | AllConstant args ->
                            return Expr.Value(FSharpValue.MakeTuple(List.toArray args, e.Type), e.Type)
                        | _ ->
                            return Expr.NewTuple(args)
                    
                | NewUnionCase(ci, args) ->
                    let! args = args |> List.mapS evaluateConstantsS
                    match args with
                        | AllConstant args ->
                            return Expr.Value(FSharpValue.MakeUnion(ci, List.toArray args, true), e.Type)
                        | _ ->
                            return Expr.NewUnionCase(ci, args)
                    
                | PropertyGet(None, pi, indices) ->
                    let! indices = indices |> List.mapS evaluateConstantsS
                    let! n = State.needsProperty pi
                    match indices with
                        | AllConstant indexValues when not n ->
                            let value = 
                                try pi.GetValue(null, List.toArray indexValues) |> Some
                                with 
                                    | ShaderOnlyExn _ -> None
                                    | _ -> 
                                        Log.warn "[FShade] could not evaluate: %A" pi
                                        None
                            match value with
                                | Some value -> return Expr.Value(value, e.Type)
                                | None -> return Expr.PropertyGet(pi, indices)

                        | _ ->
                            return Expr.PropertyGet(pi, indices)

                | PropertyGet(Some t, pi, indices) ->
                    let! t = evaluateConstantsS t
                    let! indices = indices |> List.mapS evaluateConstantsS
                    let! n = State.needsProperty pi
                    match t, indices with
                        | Value(tv,_), AllConstant indexValues when not n && not (isNull tv)->
                            let value = 
                                try pi.GetValue(tv, List.toArray indexValues) |> Some
                                with 
                                    | ShaderOnlyExn _ -> None
                                    | _ -> 
                                        Log.warn "[FShade] could not evaluate: %A" pi
                                        None

                            match value with
                                | Some value -> return Expr.Value(value, e.Type)
                                | None -> return Expr.PropertyGet(t, pi, indices)
                        | _ ->
                            return Expr.PropertyGet(t, pi, indices)

                | PropertySet(None, pi, indices, value) ->
                    let! indices = indices |> List.mapS evaluateConstantsS
                    let! value = evaluateConstantsS value
                    return Expr.PropertySet(pi, value, indices)

                | PropertySet(Some t, pi, indices, value) ->
                    let! t = evaluateConstantsS t
                    let! indices = indices |> List.mapS evaluateConstantsS
                    let! value = evaluateConstantsS value
                    return Expr.PropertySet(t, pi, value, indices)
                    
                | QuoteRaw e ->
                    //let! e = evaluateConstantsS e
                    return Expr.Value(e)
                    
                | QuoteTyped te ->
                    let! te = evaluateConstantsS te
                    let mi = typeof<Expr>.GetMethod("Cast")
                    let mi = mi.MakeGenericMethod [| te.Type |]
                    let v = mi.Invoke(null, [|te :> obj|])
                    return Expr.Value(v, mi.ReturnType)

                | Sequential(l, r) ->
                    let! l = evaluateConstantsS l
                    let! r = evaluateConstantsS r
                    match l with
                        | Unit -> return r
                        | _ -> return Expr.Sequential(l, r)

                | TryFinally(t,f) ->
                    let! t = evaluateConstantsS t
                    let! f = evaluateConstantsS f
                    return Expr.TryFinally(t, f)

                | TryWith(a, b, c, d, e) ->
                    let! a = evaluateConstantsS a
                    let! c = evaluateConstantsS c
                    let! e = evaluateConstantsS e
                    return Expr.TryWith(a, b, c, d, e)

                | TupleGet(t, i) ->
                    let! t = evaluateConstantsS t
                    match t with
                        | Value(t,_) -> return Expr.Value(FSharpValue.GetTupleField(t, i), e.Type)
                        | _ -> return Expr.TupleGet(t, i)

                | TypeTest(v, t) ->
                    let! v = evaluateConstantsS v
                    return Expr.TypeTest(v, t)

                | UnionCaseTest(e, ci) ->
                    let! e = evaluateConstantsS e
                    // TODO: could be avoided
                    return Expr.UnionCaseTest(e, ci)

                | Value _ ->
                    return e

                | VarSet (v, e) ->
                    let! e = evaluateConstantsS e
                    return Expr.VarSet (v, e)

                | Var v ->
                    let! value = State.getVar v
                    match value with
                        | Some v -> return Expr.Value(v, e.Type)
                        | _ -> return e

                | WhileLoop(guard, body) ->
                    let! guard = evaluateConstantsS guard
                    let! body = evaluateConstantsS body
                    match guard, body with
                        | Bool false, _ -> return Expr.Unit
                        | _, Unit -> return Expr.Unit
                        | g, b -> return Expr.WhileLoop(g, b)

                | _ ->
                    return failwithf "[FShade] unexpected expression %A" e
        }

    let evaluateConstants (e : Expr) =
        let run = evaluateConstantsS e
        let state = ref State.empty
        run.Run(state)

    let evaluateConstants' (isSideEffect : MethodInfo -> bool) (e : Expr) =
        let run = evaluateConstantsS e

        let isSideEffect (m : MemberInfo) =
            match m with    
                | :? MethodInfo as mi -> isSideEffect mi
                | _ -> false

        let state = ref { State.empty with isGlobalSideEffect = isSideEffect }
        run.Run(state)

    let evaluateConstants'' (isSideEffect : MemberInfo -> bool) (e : Expr) =
        let run = evaluateConstantsS e
        let state = ref { State.empty with isGlobalSideEffect = isSideEffect }
        run.Run(state)
  