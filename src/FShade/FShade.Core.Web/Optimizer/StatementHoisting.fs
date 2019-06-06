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


module StatementHoisting =
    
    type Hoisting =
        {
            finalize : Option<Expr -> Expr>
        }

        static member Zero = { finalize = None }

        static member (+) (l : Hoisting, r : Hoisting) =
            match l.finalize, r.finalize with
                | Some lf, Some rf -> { finalize = Some (lf >> rf) }
                | Some _, None -> l
                | None, Some _ -> r
                | None, None -> l

        static member Sequential (e : Expr) =
            { finalize = Some (fun f -> Expr.Seq [e; f]) }

    let rec processExpression (expr : Expr) : Expr * Hoisting =
        match expr with
            | WriteOutputs _
            | AddressSet _ 
            | ForInteger _ 
            | ForEach _ 
            | FieldSet _
            | PropertySet _ 
            | VarSet _
            | WhileLoop _ ->
                let expr = processStatement expr
                Expr.Unit, Hoisting.Sequential expr

            | IfThenElse _ when expr.Type = typeof<unit> ->
                let expr = processStatement expr
                Expr.Unit, Hoisting.Sequential expr
                
                
            | Sequential(a, Sequential(b,c)) ->
                Expr.Sequential(Expr.Sequential(a,b), c) |> processExpression

            | Sequential(l,r) ->
                let l = processStatement l
                let r, rh = processExpression r
                r, rh + Hoisting.Sequential l


            | Let(v,e,b) ->
                let b, bh = processExpression b
                let e, eh = processExpression e
                b, bh + { finalize = Some (fun f -> Expr.Let(v, e, f)) } + eh


            | TryFinally _ -> failwith ""
            | TryWith _ -> failwith ""
            
            | AddressOf e ->
                let e, eh = processExpression e
                Expr.AddressOf(e), eh

            | Application(l,a) ->
                let l, lh = processExpression l
                let a, ah = processExpression a
                Expr.Application(l,a), lh + ah

            | CallFunction(utility, args) ->
                let args, ah = processManyExpresions args
                let utility = utility |> UtilityFunction.map processStatement
                Expr.CallFunction(utility, args), ah
                
            | Call(None, mi, args) ->
                let args, ah = processManyExpresions args
                Expr.Call(mi, args), ah
                
            | Call(Some t, mi, args) ->
                let t, th = processExpression t
                let args, ah = processManyExpresions args
                Expr.Call(t, mi, args), th + ah

                
            | Coerce(e, t) ->
                let e, eh = processExpression e
                Expr.Coerce(e, t), eh

            | DefaultValue t ->
                Expr.DefaultValue t, Hoisting.Zero

            | FieldGet(None, f) ->
                Expr.FieldGet(f), Hoisting.Zero

            | FieldGet(Some t, f) ->
                let t, th = processExpression t
                Expr.FieldGet(t, f), th

            | IfThenElse(c, i, e) ->
                let c, ch = processExpression c
                Expr.IfThenElse(c, i, e), ch

            | Lambda(v,b) ->
                Expr.Lambda(v, processStatement b), Hoisting.Zero

            | NewArray(t, args) ->
                let args, ah = processManyExpresions args
                Expr.NewArray(t, args), ah

            | NewDelegate(t, vars, body) ->
                Expr.NewDelegate(t, vars, processStatement body), Hoisting.Zero

            | NewObject(ctor, args) ->
                let args, ah = processManyExpresions args
                Expr.NewObject(ctor, args), ah

            | NewRecord(t, args) ->
                let args, ah = processManyExpresions args
                Expr.NewRecord(t, args), ah

            | NewTuple(args) ->
                let args, ah = processManyExpresions args
                Expr.NewTuple(args), ah

            | NewUnionCase(ci, args) ->
                let args, ah = processManyExpresions args
                Expr.NewUnionCase(ci, args), ah

            | PropertyGet(None, pi, ii) ->
                let ii, ih = processManyExpresions ii
                Expr.PropertyGet(pi, ii), ih

            | PropertyGet(Some t, pi, ii) ->
                let t, th = processExpression t
                let ii, ih = processManyExpresions ii
                Expr.PropertyGet(t, pi, ii), th + ih

            | QuoteRaw e ->
                Expr.QuoteRaw e, Hoisting.Zero
                
            | QuoteTyped e -> 
                Expr.QuoteTyped e, Hoisting.Zero


            | TupleGet(v, i) ->
                let v, vh = processExpression v
                Expr.TupleGet(v, i), vh

            | TypeTest(v,t) ->
                let v, vh = processExpression v
                Expr.TypeTest(v, t), vh

            | UnionCaseTest(v, ci) ->
                let v, vh = processExpression v
                Expr.UnionCaseTest(v,ci), vh

            | Value _ ->
                expr, Hoisting.Zero

            | Var _ ->
                expr, Hoisting.Zero


            | _ -> failwithf "[FShade] unexpected expression %A" expr

    and processManyExpresions (l : list<Expr>) =
        List.foldBack (fun a (r, rh) ->
            let a, ah = processExpression a
            (a :: r, ah + rh)
        ) l ([], Hoisting.Zero)

    and processStatement (expr : Expr) : Expr =
        let inline apply (h : Hoisting) (e : Expr) =
            match h.finalize with
                | Some f -> e |> f |> processStatement
                | None -> e

        match expr with 
            | WriteOutputs outputs -> 
                let mutable res = Map.empty
                let mutable hoist = Hoisting.Zero
                for (name, (idx,value)) in Map.toSeq outputs do
                    match idx with
                        | Some idx ->
                            let idx, ih = processExpression idx
                            let v, vh = processExpression value
                            res <- Map.add name (Some idx, v) res
                            hoist <- hoist + ih + vh
                        | None ->
                            let v, vh = processExpression value
                            res <- Map.add name (None, v) res
                            hoist <- hoist + vh
                      
                      
                Expr.WriteOutputs(res) |> apply hoist 

            | AddressSet(v,e) ->
                let v, vh = processExpression v
                let e, eh = processExpression e 
                Expr.AddressSet(v,e) |> apply (vh + eh)

            | ForInteger(v, start, step, stop, body) ->
                let body = processStatement body
                
                let start, h0 = processExpression start
                let step, h1 = processExpression step
                let stop, h2 = processExpression stop

                Expr.ForInteger(v, start, step, stop, body) |> apply (h0 + h1 + h2)

                 
            | ForEach(v,s,body) ->
                let body = processStatement body
                let s, sh = processExpression s

                Expr.ForEach(v, s, body) |> apply sh


            | FieldSet(None, f, value) ->
                let value, vh = processExpression value
                Expr.FieldSet(f, value) |> apply vh

            | FieldSet(Some t, f, value) ->
                let t, th = processExpression t
                let value, vh = processExpression value
                Expr.FieldSet(t, f, value) |> apply (th + vh)

            | PropertySet(None, pi, index, value) ->
                let index, ih = processManyExpresions index
                let value, vh = processExpression value
                Expr.PropertySet(pi, value, index) |> apply (ih + vh)

            | PropertySet(Some t, pi, index, value) ->
                let t, th = processExpression t 
                let index, ih = processManyExpresions index
                let value, vh = processExpression value
                Expr.PropertySet(t, pi, value, index) |> apply (th + ih + vh)
                 
            | WhileLoop (guard, body) ->
                let guard, gh = processExpression guard
                let body = processStatement body
                Expr.WhileLoop(guard, body) |> apply gh

            | Sequential(l,r) ->
                let l = processStatement l
                let r = processStatement r
                Expr.Sequential(l,r)


            | Let(v,e,b) ->
                let b = processStatement b
                let e, eh = processExpression e
                Expr.Let(v,e,b) |> apply eh

            | VarSet(v,e) ->
                let e, eh = processExpression e
                Expr.VarSet(v,e) |> apply eh

            | IfThenElse(c,i,e) when e.Type = typeof<unit> ->
                let c, ch = processExpression c
                let i = processStatement i
                let e = processStatement e
                Expr.IfThenElse(c,i,e) |> apply ch

            | e ->
                let e, eh = processExpression e
                match eh.finalize with
                    | Some f -> f e
                    | None -> e

    let hoistImperative (e : Expr) =
        processStatement e
