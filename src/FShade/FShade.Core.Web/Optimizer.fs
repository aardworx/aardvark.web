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

[<AutoOpen>]
module Optimizer =
 


    /// creates a new expression only containing e's visible side-effects.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let withoutValue (e : Expr) =
        DeadCodeElimination.withoutValue e
        
    /// creates a new expression by removing all unused variables/calls.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let eliminateDeadCode (e : Expr) =
        DeadCodeElimination.eliminateDeadCode e
        
    /// creates a new expression by evaluating all constant-subexpressions where possible.
    /// NOTE: all methods are assumed to be pure (except for the ones returning void/unit)
    let evaluateConstants (e : Expr) =
        ConstantFolding.evaluateConstants e
        
    /// creates a new expression only containing e's visible side-effects.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let withoutValue' (isSideEffect : MethodInfo -> bool) (e : Expr) =
        DeadCodeElimination.withoutValue' isSideEffect e
        
    /// creates a new expression by removing all unused variables/calls.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let eliminateDeadCode' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
        DeadCodeElimination.eliminateDeadCode' isSideEffect e
        
    /// creates a new expression by evaluating all constant-subexpressions where possible.
    /// NOTE: isSideEffect needs to determine whether a Method has non-local side-effects.
    let evaluateConstants' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
        ConstantFolding.evaluateConstants' isSideEffect e

    /// creates a new expression by lifting all shader inputs to function-arguments until they can be read. (in the shader's main entry)
    let liftInputs (e : Expr) =
        InputLifting.liftInputs e

    /// hoists imperative constructs. For example `let a = let b = 10 in b * 199` translates to `let b = 10 in let a = b * 199`.
    /// this ensures that most imperative constructs occur on statement-level and can easily be compiled to C like languages.
    let hoistImperativeConstructs (e : Expr) =
        StatementHoisting.processStatement e