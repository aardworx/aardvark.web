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



module UtilityFunction =

    let rec singleReturn (e : Expr) =
        match e with
        | Lambda(_, b) ->
            singleReturn b
        | Sequential(_, r) -> 
            singleReturn r
        | IfThenElse _ ->
            false
        | LetRecursive(_, b)
        | Let(_, _, b) ->
            singleReturn b


        | e ->
            true
        

    let rec subsituteValue (s : Expr -> Expr) (e : Expr) =
        match e with
        | Lambda(v, b) ->
            Expr.Lambda(v, subsituteValue s b)
        | Sequential(l, r) -> 
            Expr.Sequential(l, subsituteValue s r)
        | IfThenElse(c, i, e) ->
            Expr.IfThenElse(c, subsituteValue s i, subsituteValue s e)
        | Let(v, e, b) ->
            Expr.Let(v, e, subsituteValue s b)
        | e ->
            s e

    
    let rec subsituteValueS (s : Expr -> State<'s, Expr>) (e : Expr) : State<'s, Expr> =
        state {
            match e with
            | Lambda(v, b) ->
                let! b = subsituteValueS s b
                return Expr.Lambda(v, b)
            | Sequential(l, r) -> 
                let! r = subsituteValueS s r
                return Expr.Sequential(l, r)
            | IfThenElse(c, i, e) ->
                let! i = subsituteValueS s i
                let! e = subsituteValueS s e
                return Expr.IfThenElse(c, i, e)
            | Let(v, e, b) ->
                let! b = subsituteValueS s b
                return Expr.Let(v, e, b)
            | e ->
                return! s e
        }

    let rec (|TrivialOrInput|_|) (e : Expr) =
        match e with
            | Var _ 
            | Value _
            | FieldGet(None, _)
            | PropertyGet(None, _, [])
            | TupleGet(Trivial, _)
            | PropertyGet(Some TrivialOrInput, (FSharpTypeProperty | ArrayLengthProperty), [])
            | FieldGet(Some TrivialOrInput, _) 
            | ReadInput(_, _, None) 
            | ReadInput(_, _, Some TrivialOrInput) -> 
                Some()
            | _ ->
                None

    let inlineCode (f : UtilityFunction) (args : list<Expr>) =
        let rec wrap (v : list<Var>) (e : list<Expr>) (b : Expr) =
            match v, e with
                | [], [] -> 
                    b

                | v :: vs, (TrivialOrInput as ev) :: es ->
                    let b = b.Substitute(fun vi -> if vi = v then Some ev else None)
                    wrap vs es b

                | v :: vs, e :: es ->
                    let useCnt = ref 0
                    let bt = b.Substitute(fun vi -> if vi = v then useCnt := !useCnt + 1; Some e else None)
                    if !useCnt <= 1 then
                        wrap vs es bt
                    else
                        Expr.Let(v,e, wrap vs es b)

                | _ ->
                    failwithf "[FShade] bad arity for utility function call"

        wrap f.functionArguments args f.functionBody

[<AutoOpen>]
module Helpers = 
    module State =
        let withLocalState (m : State<'s, 'a>) =
            State.get |> State.map (fun s ->
                m |> State.run s
            )

    let rec (|LExpr|_|) (e : Expr) =
        // TODO: complete?
        match e with
            | Var v -> Some v
            | FieldGet(Some (LExpr v), fi) -> Some v
            | PropertyGet(Some (LExpr v), _, _) -> Some v
            | GetArray(LExpr a, i) -> Some a
            | _ -> None

    let rec (|MutableArgument|_|) (e : Expr) =
        match e with
            | Var v -> 
                if v.IsMutable then
                    Some v
                else
                    match v.Type with
                        //| ArrOf _ -> Some v
                        | ArrayOf _ -> Some v
                        | Ref _ -> Some v
                        | _ -> None
            | GetArray(Var v, _) -> Some v

            | RefOf (MutableArgument v) -> Some v
            | AddressOf (MutableArgument v) -> Some v
            | _ -> None

    let rec (|StorageArgument|_|) (e : Expr) =
        match e with
            | RefOf (GetArray(ReadInput(ParameterKind.Uniform, _, _), _)) ->
                Some ()
            | _ ->
                None