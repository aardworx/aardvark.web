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

module InputLifting =
    type State =
        {
            usedInputs : Map<string, ParameterDescription>
        }
             
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module State =
        let empty = 
            { 
                usedInputs = Map.empty
            }

    
    let rec liftInputsS (e : Expr) : State<State, Expr> =
        state {
            match e with

                | ReadInput(ParameterKind.Input, name, idx) ->
                    do! State.modify (fun s ->
                        if Map.containsKey name s.usedInputs then
                            s
                        else
                            let typ =
                                match idx with
                                    | Some _ -> e.Type.MakeArrayType()
                                    | _ -> e.Type
                            { s with usedInputs = Map.add name (ParameterDescription.ofType typ) s.usedInputs }
                    )
                    return e

                | ReadInput(kind, name, idx) ->
                    match idx with
                        | Some idx ->
                            let! idx = liftInputsS idx
                            return Expr.ReadInput(kind, e.Type, name, idx)
                        | None ->
                            return Expr.ReadInput(kind, e.Type, name)
                            
                | WriteOutputs outputs -> 
                    let! outputs = 
                        outputs |> Map.mapS (fun name (idx, value) ->
                            state {
                                let! idx = Option.mapS liftInputsS idx
                                let! value = liftInputsS value
                                return (idx, value)
                            }
                        )
                    return Expr.WriteOutputs outputs

                | AddressOf e ->
                    let! e = liftInputsS e
                    return Expr.AddressOf e

                | AddressSet(v, e) ->
                    let! v = liftInputsS v
                    let! e = liftInputsS e
                    return Expr.AddressSet(v,e)

                | Application(lambda, arg) ->
                    let! lambda = liftInputsS lambda
                    let! arg = liftInputsS arg
                    return Expr.Application(lambda, arg)

                | ForInteger(v, first, step, last, body) ->
                    let! first = liftInputsS first
                    let! step = liftInputsS step
                    let! last = liftInputsS last
                    let! body = liftInputsS body
                    return Expr.ForInteger(v, first, step, last, body)

                | ForEach(v, s, b) ->
                    let! s = liftInputsS s
                    let! b = liftInputsS b
                    return Expr.ForEach(v, s, b)

                | CallFunction(utility, args) ->
                    
                    let usedInputs = ref Map.empty
                    let newBody = liftFunctionInputsS(utility.functionBody).Run(usedInputs)

                    let (vars, values) = !usedInputs |> Map.toList |> List.map snd |> List.unzip

                    let utility =
                        { utility with
                            functionArguments = utility.functionArguments @ vars
                            functionBody = newBody
                            functionId = Guid.NewGuid() |> string
                            functionTag = null
                        }


                    let! values = values |> List.mapS liftInputsS
                    return Expr.CallFunction(utility, args @ values)
                    
   
                | Call(None, mi, args) ->
                    let! args = args |> List.mapS liftInputsS
                    return Expr.Call(mi, args)

                | Call(Some t, mi, args) ->
                    let! t = liftInputsS t
                    let! args = args |> List.mapS liftInputsS
                    return Expr.Call(t, mi, args)
                    
                | Coerce(e, t) ->
                    let! e = liftInputsS e
                    return Expr.Coerce(e, t)

                | DefaultValue t ->
                    return e

                | FieldGet(None, f) ->
                    return e

                | FieldGet(Some t, f) ->
                    let! t = liftInputsS t
                    return Expr.FieldGet(t, f)

                | FieldSet(None, f, value) ->
                    let! value = liftInputsS value
                    return Expr.FieldSet(f, value)

                | FieldSet(Some t, f, value) ->
                    let! t = liftInputsS t
                    let! value = liftInputsS value
                    return Expr.FieldSet(t, f, value)

                | IfThenElse(cond, i, e) ->
                    let! cond = liftInputsS cond
                    let! i = liftInputsS i
                    let! e = liftInputsS e
                    return Expr.IfThenElse(cond, i, e)

                | Lambda(v,b) ->
                    let! b = liftInputsS b
                    return Expr.Lambda(v,b)

                | Let(v, e, b) ->
                    let! e = liftInputsS e
                    let! b = liftInputsS b
                    return Expr.Let(v,e,b)

                | NewTuple(args) ->
                    let! args = args |> List.mapS liftInputsS
                    return Expr.NewTuple args

                | NewArray(t, args) ->
                    let! args = args |> List.mapS liftInputsS
                    return Expr.NewArray(t, args)

                | NewDelegate(t, vars, body) ->
                    let! body = liftInputsS body
                    return Expr.NewDelegate(t, vars, body)

                | NewObject(ctor, args) ->
                    let! args = args |> List.mapS liftInputsS
                    return Expr.NewObject(ctor, args)

                | NewRecord(t, args) ->
                    let! args = args |> List.mapS liftInputsS
                    return Expr.NewRecord(t, args)

                | NewUnionCase(ci, args) ->
                    let! args = args |> List.mapS liftInputsS
                    return Expr.NewUnionCase(ci, args)

                | PropertyGet(None, pi, indices) ->
                    let! indices = indices |> List.mapS liftInputsS
                    return Expr.PropertyGet(pi, indices)

                | PropertyGet(Some t, pi, indices) ->
                    let! t = liftInputsS t
                    let! indices = indices |> List.mapS liftInputsS
                    return Expr.PropertyGet(t, pi, indices)
                    
                | PropertySet(None, pi, indices, value) ->
                    let! value = liftInputsS value
                    let! indices = indices |> List.mapS liftInputsS
                    return Expr.PropertySet(pi, value, indices)

                | PropertySet(Some t, pi, indices, value) ->
                    let! t = liftInputsS t
                    let! value = liftInputsS value
                    let! indices = indices |> List.mapS liftInputsS
                    return Expr.PropertySet(t, pi, value, indices)
                    
                | QuoteRaw e ->
                    let! e = liftInputsS e
                    return Expr.QuoteRaw e

                | QuoteTyped e ->
                    let! e = liftInputsS e
                    return Expr.QuoteTyped e

                | Sequential(l,r) ->
                    let! l = liftInputsS l
                    let! r = liftInputsS r
                    return Expr.Sequential(l,r)

                | TryFinally(t, f) ->
                    let! t = liftInputsS t
                    let! f = liftInputsS f
                    return Expr.TryFinally(t, f)

                | TryWith(a, b, c, d, e) ->
                    let! a = liftInputsS a
                    let! c = liftInputsS c
                    let! e = liftInputsS e
                    return Expr.TryWith(a, b, c, d, e)

                | TupleGet(t, i) ->
                    let! t = liftInputsS t
                    return Expr.TupleGet(t, i)

                | TypeTest(t, i) ->
                    let! t = liftInputsS t
                    return Expr.TypeTest(t,i)

                | UnionCaseTest(e, ci) ->
                    let! e = liftInputsS e
                    return Expr.UnionCaseTest(e, ci)

                | Value _ ->
                    return e

                | VarSet(v, e) ->
                    let! e = liftInputsS e
                    return Expr.VarSet(v,e)

                | Var v ->
                    return e

                | WhileLoop(guard, body) ->
                    let! guard = liftInputsS guard
                    let! body = liftInputsS body
                    return Expr.WhileLoop(guard, body)


                | _ ->
                    return failwithf "[FShade] unexpected expression %A" e
        }

    and liftFunctionInputsS (e : Expr) : State<Map<_,_>, Expr> =
        state {
            match e with
                | ReadInput(ParameterKind.Input, name, idx) ->
                    let! (s : Map<string, Var * Expr>) = State.get

                    let idxHash = idx |> Option.map string |> Option.defaultValue ""
                    let key = name + idxHash

                    match Map.tryFind key s with
                        | Some (v,_) -> 
                            return Expr.Var v
                        | None ->
                            let v = Var(name, e.Type)
                            do! State.put (Map.add key (v,e) s)
                            return Expr.Var v

                | ReadInput(kind, name, idx) ->
                    match idx with
                        | Some idx ->
                            let! idx = liftFunctionInputsS idx
                            return Expr.ReadInput(kind, e.Type, name, idx)
                        | None ->
                            return Expr.ReadInput(kind, e.Type, name)

                | CallFunction(utility, args) ->
                    let! args = args |> List.mapS liftFunctionInputsS
                    let usedInputs = ref Map.empty
                    let newBody = liftFunctionInputsS(utility.functionBody).Run(usedInputs)

                    let (vars, values) = !usedInputs |> Map.toList |> List.map snd |> List.unzip

                    let utility =
                        { utility with
                            functionArguments = utility.functionArguments @ vars
                            functionBody = newBody
                            functionId = Guid.NewGuid() |> string
                            functionTag = null
                        }


                    let! values = values |> List.mapS liftFunctionInputsS
                    return Expr.CallFunction(utility, args @ values)

                    
                | ShapeVar v -> 
                    return Expr.Var v

                | ShapeLambda(v, b) ->
                    let! b = liftFunctionInputsS b
                    return Expr.Lambda(v, b)

                | ShapeCombination(o, args) -> 
                    let! args = args |> List.mapS liftFunctionInputsS
                    return RebuildShapeCombination(o, args)
        }

    let liftInputs (e : Expr) =
        let run = liftInputsS(e)
        let state = ref State.empty
        let res = run.Run(state)
        res
