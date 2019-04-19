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


/// The dead code elimination removes unused bindings from the code
/// e.g. `let a = someFunction(x,y) in 10` gets reduced to `10` assuming
/// that all functions are pure except the ones identified by the given function.
/// this handling seems proper since most GLSL functions are pure (except for discard, EmitVertex, etc.)
module DeadCodeElimination = 

    type EliminationState =
        {
            isGlobalSideEffect : MethodInfo -> bool
            usedVariables : Set<Var>
        }


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module EliminationState =
        let empty = 
            { 
                isGlobalSideEffect = fun mi -> mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<System.Void>
                usedVariables = Set.empty
            }
        let useVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.add v s.usedVariables })
        let remVar (v : Var) = State.modify (fun s -> { s with usedVariables = Set.remove v s.usedVariables })
        let isUsed (v : Var) = State.get |> State.map (fun s -> Set.contains v s.usedVariables)
        let useVars (vars : seq<Var>) = State.modify (fun s -> { s with usedVariables = Set.union (Set.ofSeq vars) s.usedVariables })
   

        let merge (l : EliminationState) (r : EliminationState) =
            {
                isGlobalSideEffect = l.isGlobalSideEffect
                usedVariables = Set.union l.usedVariables r.usedVariables
            }

        let rec fix (m : State<EliminationState, 'a>) =
            state {
                let! initial = State.get
                let! res = m
                let! final = State.get
                if initial.usedVariables = final.usedVariables then return res
                else return! fix m
            }
            
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module List =
        let rec existsS (f : 'a -> State<'s, bool>) (l : list<'a>) =
            state {
                match l with
                    | [] -> return false
                    | h :: t ->
                        let! r = f h
                        if r then 
                            return true
                        else
                            return! existsS f t
            }

    let private staticallyNeededCalls =
        HSet.ofList [
            MethodInfo.WriteOutputs
            MethodInfo.Unroll
        ]

    let private callNeededS (t : Option<Expr>) (mi : MethodInfo) (args : list<Expr>) =
        state {
            let! state = State.get

            if state.isGlobalSideEffect mi || staticallyNeededCalls.Contains mi then
                return true
            else
                let needsMutableParameter (p : ParameterInfo) (v : Expr) =
                    let pt = p.ParameterType
                    if pt.IsArray (* || pt.IsArr *) || pt.IsRef then
                        match v with
                            | MutableArgument v -> Set.contains v state.usedVariables
                            | StorageArgument -> true
                            | _ -> false
                    else
                        false

                let parameters = mi.GetParameters() |> Array.toList
                match t with
                    | Some t ->
                        match t with
                            | MutableArgument v when Set.contains v state.usedVariables -> 
                                return true
                            | StorageArgument -> 
                                return true
                            | _ -> 
                                return List.exists2 needsMutableParameter parameters args
                    | None ->
                        return List.exists2 needsMutableParameter parameters args
        }

    let rec private hasSideEffectsS (e : Expr) =
        state {
            match e with
                | CallFunction(utility, args) ->
                    let! aa = args |> List.existsS hasSideEffectsS
                    if aa then
                        return true
                    else
                        return! hasSideEffectsS utility.functionBody
                | Call(t, mi, args) ->
                    let! ts = t |> Option.mapS hasSideEffectsS
                    match ts with
                        | Some true -> 
                            return true
                        | _ ->
                            let! aa = args |> List.existsS hasSideEffectsS
                            if aa then 
                                return true
                            else
                                let! state = State.get

                                if state.isGlobalSideEffect mi || staticallyNeededCalls.Contains mi then
                                    return true
                                else
                                    return false

                | ShapeLambda(_,b) ->
                    return! hasSideEffectsS b

                | ShapeVar _ ->
                    return false

                | ShapeCombination(o, args) ->
                    return! List.existsS hasSideEffectsS args
        }

    let rec withoutValueS (e : Expr) : State<EliminationState, Expr> =
        state {
            match e with
                | ExprOf t when t = typeof<unit> ->
                    return! eliminateDeadCodeS e

                
                | IfThenElse(cond, i, e) ->
                    let! elseState, e = State.withLocalState (withoutValueS e)
                    let! ifState, i = State.withLocalState (withoutValueS i)
                    do! State.put (EliminationState.merge ifState elseState)
                
                    match i, e with
                        | Unit, Unit -> 
                            return! withoutValueS cond
                        | _ ->
                            let! cond = eliminateDeadCodeS cond
                            return Expr.IfThenElse(cond, i, e)

                | AddressOf e ->
                    return! withoutValueS e

                | AddressSet(e,v) ->
                    let! v = withoutValueS v
                    let! e = withoutValueS e
                    return Expr.Seq [v; e]

                | Application(l, a) ->
                    let! a = eliminateDeadCodeS a
                    let! l = eliminateDeadCodeS l
                    return Expr.Application(l, a)

                | CallFunction(utility, args) ->
                    let! sideEffects = hasSideEffectsS utility.functionBody
                    if sideEffects then
                        let! self = eliminateDeadCodeS e
                        if self.Type = typeof<unit> then
                            return e
                        else
                            return Expr.Ignore e
                    else
                        return Expr.Unit

                | Call(t, mi, args) ->
                    let! needed = callNeededS t mi args

                    if needed then
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! t = t |> Option.mapS eliminateDeadCodeS

                        match t with
                            | Some t -> return Expr.Call(t, mi, args) |> Expr.Ignore
                            | None -> return Expr.Call(mi, args) |> Expr.Ignore
                    else
                        let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        let! t = t |> Option.mapS withoutValueS

                        match t with
                            | Some t -> return Expr.Seq (t :: args)
                            | None -> return Expr.Seq args            


                | Coerce(e,t) ->
                    return! withoutValueS e

                | DefaultValue t ->
                    return Expr.Unit

                | FieldGet(None, _) -> 
                    return Expr.Unit

                | FieldGet(Some t, _) ->
                    return! withoutValueS t

                | Lambda(v, b) ->
                    return Expr.Unit

                | LetRecursive _ ->
                    return failwith "recursive bindings not implemented"
                
                | Let(v,e,b) ->
                    let! b = withoutValueS b
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! e = eliminateDeadCodeS e
                        return Expr.Let(v, e, b)

                    else
                        let! e = withoutValueS e
                        match e with
                            | Unit -> return b
                            | _ -> return Expr.Sequential(e, b)

                | NewArray(t, args) ->
                    let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                    return Expr.Seq args

                | NewDelegate(t, vars, e) ->
                    return! withoutValueS e
  
                | NewObject(ctor, args) ->
                    let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                    return Expr.Seq args

                | NewRecord(t, args) ->
                    let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                    return Expr.Seq args

                | NewTuple(args) ->
                    let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                    return Expr.Seq args

                | NewUnionCase(ci, args) ->
                    let! args = args |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                    return Expr.Seq args


                | PropertyGet(None, pi, idx) ->
                    let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                    return Expr.Seq idx

                | PropertyGet(Some t, pi, idx) ->
                    let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                    let! t = withoutValueS t
                    return Expr.Seq (t :: idx)
                
                | QuoteRaw _ | QuoteTyped _ -> 
                    return failwith "not implemented"

                | Sequential(l, r) ->
                    let! r = withoutValueS r
                    let! l = withoutValueS l
                    match l, r with
                        | Unit, r   -> return r
                        | l, Unit   -> return l
                        | l, r      -> return Expr.Sequential(l, r)

                | TryWith _ | TryFinally _ -> 
                    return failwith "not implemented"

                | TupleGet(t, i) ->
                    return! withoutValueS t

                | TypeTest(e, t) ->
                    return! withoutValueS e

                | UnionCaseTest(e, ci) ->
                    return! withoutValueS e

                | Value _ ->
                    return Expr.Unit

                | Var v ->
                    return Expr.Unit

                | _ ->
                    return failwith ""

    
        }

    and eliminateDeadCodeS (e : Expr) : State<EliminationState, Expr> =
        state {
            match e with

                // side effects affecting variables
                | VarSet(v, e) ->
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! e = eliminateDeadCodeS e
                        return Expr.VarSet(v, e)
                    else
                        return! withoutValueS e

                | FieldSet(Some (LExpr v as target), fi, value) ->
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! value = eliminateDeadCodeS value
                        let! target = eliminateDeadCodeS target
                        return Expr.FieldSet(target, fi, value)
                    else
                        let! v = withoutValueS value
                        let! t = withoutValueS target
                        return Expr.Seq [v;t]

                | PropertySet(Some (LExpr v as target), pi, idx, value) ->
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! value = eliminateDeadCodeS value
                        let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! target = eliminateDeadCodeS target
                        return Expr.PropertySet(target, pi, value, idx)
                    else 
                        let! value = withoutValueS value
                        let! idx = idx |> List.rev |> List.mapS withoutValueS |> State.map List.rev
                        let! target = withoutValueS target

                        return Expr.Seq [
                            yield target
                            yield! idx
                            yield value
                        ]

                // global side effects
                | FieldSet(None, f, value) ->
                    let! value = eliminateDeadCodeS value
                    return Expr.FieldSet(f, value)

                | PropertySet(None, pi, idx, value) ->
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    let! value = eliminateDeadCodeS value
                    return Expr.PropertySet(pi, value, idx)

                // unknown side effects
                | FieldSet(Some t, f, value) ->
                    Log.warn "[FShade] found FieldSet on unknown expression: %A" t
                    let! value = eliminateDeadCodeS value
                    let! t = eliminateDeadCodeS t
                    return Expr.FieldSet(t, f, value)

                | PropertySet(Some t, pi, idx, value) ->
                    Log.warn "[FShade] found PropertySet on unknown expression: %A" t
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    let! value = eliminateDeadCodeS value
                    let! t = eliminateDeadCodeS t
                    return Expr.PropertySet(t, pi, value, idx)


                // control-flow
                | IfThenElse(cond, i, e) ->
                    let! elseState, e = State.withLocalState (eliminateDeadCodeS e)
                    let! ifState, i = State.withLocalState (eliminateDeadCodeS i)
                    do! State.put (EliminationState.merge ifState elseState)

                    match i, e with
                        | Unit, Unit ->
                            return! withoutValueS cond
                        | _ ->
                            let! cond = eliminateDeadCodeS cond
                            return Expr.IfThenElse(cond, i, e)

                | ForEach(v, seq, body) ->
                    let iterate =
                        state {
                            let! body = eliminateDeadCodeS body
                            match body with
                                | Unit ->
                                    return! withoutValueS seq
                                | _ ->
                                    let! seq = eliminateDeadCodeS seq
                                    return Expr.ForEach(v, seq, body)
                                
                        }
                    return! EliminationState.fix iterate

                | ForInteger(v, first, step, last, body) ->
                    let iterate =
                        state {
                            let! body = eliminateDeadCodeS body
                            match body with
                                | Unit ->
                                    let! last = withoutValueS last
                                    let! step = withoutValueS step
                                    let! first = withoutValueS first
                                    return Expr.Seq [first; step; last]
                                | _ ->
                                    let! last = eliminateDeadCodeS last
                                    let! step = eliminateDeadCodeS step
                                    let! first = eliminateDeadCodeS first
                                    return Expr.ForInteger(v, first, step, last, body)
                                
                        }

                    return! EliminationState.fix iterate

                | WhileLoop(guard, body) ->
                    let iterate =
                        state {
                            let! body = eliminateDeadCodeS body
                            match body with
                                | Unit -> 
                                    let! guardState, guard = State.withLocalState (withoutValueS guard)
                                    match guard with
                                        | Unit ->
                                            do! State.put guardState
                                            return Expr.Unit
                                        | _ -> 
                                            let! guard = eliminateDeadCodeS guard
                                            return Expr.WhileLoop(guard, body)
                                | _ ->
                                    let! guard = eliminateDeadCodeS guard
                                    return Expr.WhileLoop(guard, body)
                        }

                    return! EliminationState.fix iterate


                | Ignore e ->
                    return! withoutValueS e

                | AddressOf e ->
                    let! e = eliminateDeadCodeS e
                    return Expr.AddressOf(e)

                | AddressSet(e,v) ->
                    let! v = eliminateDeadCodeS v
                    let! e = eliminateDeadCodeS e
                    return Expr.AddressSet(e, v)

                | Application(l, a) ->
                    let! a = eliminateDeadCodeS a
                    let! l = eliminateDeadCodeS l
                    return Expr.Application(l, a)
       
                | CallFunction(utility, args) ->
                    // TODO: is the call needed???
                    let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    let! s = State.get

                    let utility =
                        utility |> UtilityFunction.map (fun b ->
                            let innerState = ref { EliminationState.empty with isGlobalSideEffect = s.isGlobalSideEffect }

                            for (v, a) in List.zip args utility.functionArguments do
                                let isMutable = a.IsMutable (* || a.Type.IsArr *) || a.Type.IsArray || a.Type.IsRef
                                let isUsed =
                                    match v with
                                        | LExpr v 
                                        | RefOf (LExpr v) -> Set.contains v s.usedVariables
                                        | _ -> false

                                if isMutable && isUsed then
                                    innerState := { !innerState with usedVariables = Set.add a innerState.Value.usedVariables }
                                    

                            let res = eliminateDeadCodeS(b).Run(innerState)
                            res
                        )

                    let usedVariables = utility.functionBody.GetFreeVars() |> Set.ofSeq
                    let allArgsUsed = List.forall (fun v -> Set.contains v usedVariables) utility.functionArguments

                    if allArgsUsed then
                        return Expr.CallFunction(utility, args)
                    else
                        let args, values = 
                            List.zip utility.functionArguments args
                                |> List.filter (fun (v,_) -> Set.contains v usedVariables)
                                |> List.unzip

                        let utility =
                            { utility with
                                functionArguments = args
                                functionTag = null   
                            }

                        return Expr.CallFunction(utility, values)

                | SetArray(arr, idx, value) ->
                    let! s = State.get
                    let needed = 
                        match arr with
                            | Var v -> Set.contains v s.usedVariables
                            | ReadInput _ -> true
                            | _ -> false

                    if needed then
                        let! value = eliminateDeadCodeS value
                        let! idx = eliminateDeadCodeS idx
                        let! arr = eliminateDeadCodeS arr
                        return Expr.ArraySet(arr, idx, value)
                    else
                        return Expr.Unit

                | Call(t, mi, args) ->
                    let! needed = 
                        if e.Type <> typeof<unit> then State.value true
                        else callNeededS t mi args

                    if needed then
                        let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                        let! t = t |> Option.mapS eliminateDeadCodeS

                        match t with
                            | Some t -> return Expr.Call(t, mi, args)
                            | None -> return Expr.Call(mi, args)
                    else
                        return Expr.Unit

                | Coerce(e,t) ->
                    let! e = eliminateDeadCodeS e
                    return Expr.Coerce(e, t)

                | DefaultValue t ->
                    return e

                | FieldGet(None, f) -> 
                    return e

                | FieldGet(Some t, f) ->
                    let! t = eliminateDeadCodeS t
                    return Expr.FieldGet(t, f)

                | Lambda(v, b) ->
                    let! b = eliminateDeadCodeS b
                    return Expr.Lambda(v, b)
  
                | LetRecursive _ ->
                    return failwith "recursive bindings not implemented"
                
                | Let(v,e,b) ->
                    let! b = eliminateDeadCodeS b
                    let! vUsed = EliminationState.isUsed v
                    if vUsed then
                        let! e = eliminateDeadCodeS e
                        return Expr.Let(v, e, b)

                    else
                        let! e = withoutValueS e
                        match e with
                            | Unit -> return b
                            | _ -> return Expr.Sequential(e, b)

                | NewArray(t, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    return Expr.NewArray(t, args)

                | NewDelegate(t, vars, e) ->
                    let! e = eliminateDeadCodeS e
                    return Expr.NewDelegate(t, vars, e)
  
                | NewObject(ctor, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    return Expr.NewObject(ctor, args)

                | NewRecord(t, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    return Expr.NewRecord(t, args)

                | NewTuple(args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    return Expr.NewTuple(args)

                | NewUnionCase(ci, args) ->
                    let! args = args |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    return Expr.NewUnionCase(ci, args)

                | PropertyGet(None, pi, idx) ->
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    return Expr.PropertyGet(pi, idx)

                | PropertyGet(Some t, pi, idx) ->
                    let! idx = idx |> List.rev |> List.mapS eliminateDeadCodeS |> State.map List.rev
                    let! t = eliminateDeadCodeS t
                    return Expr.PropertyGet(t, pi, idx)
                
                | QuoteRaw _ | QuoteTyped _ -> 
                    return failwith "not implemented"

                | Sequential(l, r) ->
                    let! r = eliminateDeadCodeS r
                    let! l = eliminateDeadCodeS l
                    match l, r with
                        | Unit, r   -> return r
                        | l, Unit   -> return l
                        | l, r      -> return Expr.Sequential(l, r)

                | TryWith _ | TryFinally _ -> 
                    return failwith "not implemented"

                | TupleGet(t, i) ->
                    let! t = eliminateDeadCodeS t
                    return Expr.TupleGet(t, i)

                | TypeTest(e, t) ->
                    let! e = eliminateDeadCodeS e
                    return Expr.TypeTest(e, t)

                | UnionCaseTest(e, ci) ->
                    let! e = eliminateDeadCodeS e
                    return Expr.UnionCaseTest(e, ci)

                | Value _ ->
                    return e


                | Var v ->
                    do! EliminationState.useVar v
                    return e

                | _ ->
                    return failwithf "[FShade] unexpected expression %A" e
        }

    let withoutValue (e : Expr) =
        let run = withoutValueS e
        let state = ref EliminationState.empty
        run.Run(state)

    let eliminateDeadCode (e : Expr) =
        let run = eliminateDeadCodeS e
        let state = ref EliminationState.empty
        run.Run(state)

    let withoutValue' (isSideEffect : MethodInfo -> bool) (e : Expr) =
        let run = withoutValueS e
        let state = ref { EliminationState.empty with isGlobalSideEffect = isSideEffect }
        run.Run(state)

    let eliminateDeadCode' (isSideEffect : MethodInfo -> bool)  (e : Expr) =
        let run = eliminateDeadCodeS e
        let state = ref { EliminationState.empty with isGlobalSideEffect = isSideEffect }
        run.Run(state)
