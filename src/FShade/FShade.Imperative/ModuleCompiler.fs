namespace FShade.Imperative

open System
open System.Reflection
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open Aardvark.Base

open FShade
open FShade.Imperative

[<AllowNullLiteral>] 
type IDeletableComparable =
    inherit IComparable
    abstract member IsDeleted : bool

[<AllowNullLiteral>] 
type ISortKey =
    inherit IDeletableComparable
    abstract member Clock : IOrder

and IOrder =
    abstract member Count : int
    abstract member Root : ISortKey

module SimpleOrder =
    open System.Collections.Generic

    [<AllowNullLiteral>]
    type SortKey =
        class
            val mutable public Clock : Order
            val mutable public Tag : uint64
            val mutable public Next : SortKey
            val mutable public Prev : SortKey

            member x.Time =
                x.Tag - x.Clock.Root.Tag

            member x.CompareTo (o : SortKey) =
                if isNull o.Next || isNull x.Next then
                    failwith "cannot compare deleted times"

                if o.Clock <> x.Clock then
                    failwith "cannot compare times from different clocks"

                compare x.Time o.Time

            interface IComparable with
                member x.CompareTo o =
                    match o with
                        | :? SortKey as o -> x.CompareTo(o)
                        | _ -> failwithf "cannot compare time with %A" o

            interface IComparable<ISortKey> with
                member x.CompareTo o =
                    match o with
                        | :? SortKey as o -> x.CompareTo o
                        | _ -> failwithf "cannot compare time with %A" o

            override x.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
            override x.Equals o = System.Object.ReferenceEquals(x,o)

            interface ISortKey with
                member x.Clock = x.Clock :> IOrder
                member x.IsDeleted = isNull x.Next
                //member x.Next = x.Next :> ISortKey

            new(c) = { Clock = c; Tag = 0UL; Next = null; Prev = null }
        end

    and Order =
        class
            val mutable public Root : SortKey
            val mutable public Count : int

            member x.After (t : SortKey) =
                if t.Clock <> x then
                    failwith "cannot insert after a different clock's time"

                let distance (a : SortKey) (b : SortKey) =
                    if a = b then System.UInt64.MaxValue
                    else b.Tag - a.Tag

                let mutable dn = distance t t.Next

                // if the distance to the next time is 1 (no room)
                // relabel all times s.t. the new one can be inserted
                if dn = 1UL then
                    // find a range s.t. distance(range) >= 1 + |range|^2 
                    let mutable current = t.Next
                    let mutable j = 1UL
                    while distance t current < 1UL + j * j do
                        current <- current.Next
                        j <- j + 1UL

                    // distribute all times in the range equally spaced
                    let step = (distance t current) / j
                    current <- t.Next
                    let mutable currentTime = t.Tag + step
                    for k in 1UL..(j-1UL) do
                        current.Tag <- currentTime
                        current <- current.Next
                        currentTime <- currentTime + step

                    // store the distance to the next time
                    dn <- step

                // insert the new time with distance (dn / 2) after
                // the given one (there has to be enough room now)
                let res = SortKey(x)
                res.Tag <- t.Tag + dn / 2UL

                res.Next <- t.Next
                res.Prev <- t
                t.Next.Prev <- res
                t.Next <- res

                res

            member x.Before (t : SortKey) =
                if t = x.Root then
                    failwith "cannot insert before root-time"
                x.After t.Prev

            member x.Delete (t : SortKey) =
                if not (isNull t.Next) then
                    if t.Clock <> x then
                        failwith "cannot delete time from different clock"

                    t.Prev.Next <- t.Next
                    t.Next.Prev <- t.Prev
                    t.Next <- null
                    t.Prev <- null
                    t.Tag <- 0UL
                    t.Clock <- Unchecked.defaultof<_>      

            member x.Clear() =
                let r = SortKey(x)
                x.Root <- r
                r.Next <- r
                r.Prev <- r
                x.Count <- 1

            interface IOrder with
                member x.Root = x.Root :> ISortKey
                member x.Count = x.Count

            static member New() =
                let c = Order()
                let r = SortKey(c)
                c.Root <- r
                r.Next <- r
                r.Prev <- r
                c

            private new() = { Root = null; Count = 1 }
        end

    let create() =
        Order.New()


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ModuleCompiler =
    open Compiler

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ValueCompiler =
        open SimpleOrder

        type GraphNode(definition : CValueDef, dependencies : hset<GraphNode>) =
            let mutable sortKey : Option<SortKey> = None
            let mutable dependencies = dependencies

            member x.Definition = definition
            member x.Dependencies 
                with get() = dependencies
                and set d = dependencies <- d

            member x.SortKey 
                with get() = sortKey
                and set v = sortKey <- v

            interface IComparable with
                member x.CompareTo o =
                    match o with
                        | :? GraphNode as o -> compare sortKey o.SortKey
                        | _ -> failwithf "[FShade] cannot compare GraphNode to %A" o


            override x.Equals o =
                System.Object.ReferenceEquals(x,o)

            override x.GetHashCode() =
                System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
      

            member x.AssignOrder(afterTime : SortKey) =
                match sortKey with
                    | Some s when s < afterTime -> s
                    | _ -> 
                        let t = afterTime.Clock.Before afterTime
                        sortKey <- Some t
                        for d in dependencies do
                            d.AssignOrder t |> ignore

                        t

        type GraphState =
            {
                moduleState     : ModuleState
                cache           : Dictionary<obj, GraphNode>
            }

        [<AutoOpen>]
        module private Helpers = 
            let rec build (globals : GraphNode) (key : 'a) (compile : 'a -> State<CompilerState, CValueDef>) : State<GraphState, GraphNode> =
                state {
                    let! (state : GraphState) = State.get
                    let cache : Dictionary<obj, GraphNode> = state.cache

                    match cache.TryGetValue (key :> obj) with
                        | (true, v) -> 
                            return v
                        | _ ->
                            let s = ref <| emptyState state.moduleState
                            let def = compile(key).Run(s)
                            let s = !s
                            do! State.modify (fun gs -> { gs with moduleState = s.moduleState })

                            let difference (l : hmap<'k, 'v>) (r : hmap<'k, 'x>) =
                                HMap.choose2 (fun _ l r ->
                                    match r with
                                        | Some _ -> None
                                        | _ -> l
                                ) l r

                            let localFunctions      = s.usedFunctions |> HMap.toSeq |> Seq.map snd |> Seq.toList
                            let globalFunctions     = s.usedGlobalFunctions |> HSet.toList //difference s.moduleState.globalFunctions state.moduleState.globalFunctions |> HMap.toSeq |> Seq.map snd |> Seq.toList
                            let globalConstants     = s.usedConstants |> HSet.toList //difference s.moduleState.globalConstants state.moduleState.globalConstants |> HMap.toSeq |> Seq.map snd |> Seq.toList
                            
                            
                            let node = GraphNode(def, HSet.empty)
                            cache.[key] <- node

                            let! localFunctions = localFunctions |> List.mapS (ofFunction globals)
                            let! globalFunctions = globalFunctions |> List.mapS (ofFunction globals)
                            let! constants = globalConstants |> List.mapS (ofConstant globals)

                            let dependencies = 
                                List.concat [
                                    (if HSet.isEmpty s.usedGlobals then [] else [globals])
                                    localFunctions
                                    globalFunctions
                                    constants
                                ]

                            node.Dependencies <- HSet.ofList dependencies
                            

                            do! State.modify (fun gs -> 
                                { gs with moduleState = { gs.moduleState with globalFunctions = s.moduleState.globalFunctions; globalConstants = s.moduleState.globalConstants } }
                            )

                            return node

                }

            and ofFunction (globals : GraphNode) (f : FunctionDefinition) =
                build globals f compileFunctionS

            and ofConstant (globals : GraphNode) (f : ConstantDefinition) =
                build globals f compileConstantS


            let ofEntry (e : EntryPoint) =
                state {
                    let! (s : GraphState) = State.get
                    let! globals = build Unchecked.defaultof<_> (e,e.uniforms) (snd >> compileUniformsS)

                    let globalNames = e.uniforms |> List.map (fun u -> u.uniformName) |> Set.ofList

                    do! State.modify (fun s -> { s with moduleState = { s.moduleState with ModuleState.globalParameters = globalNames } })
                    let! root = build globals e compileEntryS

                    let root = GraphNode(root.Definition, HSet.add globals root.Dependencies)

                    do! State.modify (fun s -> { s with moduleState = { s.moduleState with ModuleState.globalParameters = Set.empty } })

                    return e, globals, root
                }

            type Meta =
                | Ifdef of string
                | Endif of string

            let flatten (graphs : list<EntryPoint * GraphNode * GraphNode>) =
                let order = SimpleOrder.create()
                let defines = Dict<SortKey, Meta>(Unchecked.hash, Unchecked.equals)
                let mutable lastRootDef = order.Root

                let rec flattenDependencies (stack : hset<GraphNode>) (g : GraphNode) =
                    if HSet.contains g stack then
                        failwithf "[FShade] found cyclic definition for %A" g.Definition

                    let dependencies = HSet.toList g.Dependencies
                    match dependencies with
                        | [] -> 
                            match g.SortKey with
                                | Some _ -> ()
                                | None ->
                                    let t = order.After lastRootDef
                                    lastRootDef <- t
                                    g.SortKey <- Some t

                        | deps ->
                            for d in dependencies do flattenDependencies (HSet.add g stack) d
                            let max = dependencies |> List.map (fun d -> d.SortKey.Value) |> List.max
                            match g.SortKey with
                                | Some o when o < max -> g.SortKey <- Some (order.After max)
                                | None -> g.SortKey <- Some (order.After max)
                                | _ -> ()

                let mutable afterLastEntry = order.Root
                for (entry, globals, definition) in graphs do
                    
                    let t0 = order.After afterLastEntry
      
                    globals.SortKey <- Some t0
                    flattenDependencies HSet.empty definition
                    if definition.SortKey.Value < t0 then
                        definition.SortKey <- Some (order.After t0)

                    afterLastEntry <- order.After definition.SortKey.Value

                    match entry.conditional with
                        | Some d -> 
                            defines.[globals.SortKey.Value] <- Ifdef d
                            defines.[definition.SortKey.Value] <- Endif d
                        | _ ->
                            ()


                let allDefinitions = 
                    let list = List()
                    let rec visit (defs : HashSet<GraphNode>) (d : GraphNode) =
                        if defs.Add d then
                            list.Add d
                            for dep in d.Dependencies do visit defs dep

                    let set = HashSet()
                    for (_, g, d) in graphs do visit set g; visit set d

                    list |> Seq.toArray |> Array.sort
                

                let tryGetEvent (t : SortKey) =
                    match defines.TryGetValue t with
                        | Some v -> Some v
                        | _ -> None

                let res = List()
                let mutable currentIfDef = None
                let current = List()
                for n in allDefinitions do
                    match tryGetEvent n.SortKey.Value with
                        | Some (Ifdef d) -> 
                            if current.Count > 0 then res.AddRange current
                            currentIfDef <- Some d
                            current.Clear()
                            match n.Definition with
                                | CUniformDef [] -> ()
                                | def -> current.Add def

                        | Some (Endif d) ->
                            match n.Definition with
                                | CUniformDef [] -> ()
                                | def -> current.Add def
                            currentIfDef <- None
                            res.Add (CConditionalDef(d, Seq.toList current))
                            current.Clear()

                        | None ->
                            match n.Definition with
                                | CUniformDef [] -> ()
                                | def -> current.Add def
                        
                if current.Count > 0 then
                    match currentIfDef with   
                        | Some d -> res.Add(CConditionalDef(d, Seq.toList current))
                        | None -> res.AddRange(current)



                Seq.toList res

        let ofModule (backend : Backend) (m : Module) =
            let compile =
                state {
                    let! nodes = m.entries |> List.mapS ofEntry
                    return nodes
                }

            let mutable state = 
                ref { 
                    moduleState =
                        {
                            backend             = backend
                            constantIndex       = 0
                            usedTypes           = HMap.empty
                            globalFunctions     = HMap.empty
                            globalConstants     = HMap.empty
                            globalParameters    = Set.empty
                            tryGetOverrideCode  = m.tryGetOverrideCode
                            globalNameIndices   = Map.empty
                        }
                    cache = Dictionary() 
                }

            let nodes = compile.Run(state)
            let state = !state
            let usedTypes = state.moduleState.usedTypes |> HMap.toSeq |> Seq.map snd |> Seq.toList
            flatten nodes, usedTypes

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TypeCompiler =
        open SimpleOrder

        type TypeGraphNode(definition : Option<CTypeDef>, dependencies : hset<TypeGraphNode>) =
            let mutable sortKey : SortKey = Unchecked.defaultof<SortKey>

            member x.Definition = definition
            member x.Dependencies = dependencies

            member x.SortKey = sortKey

            interface IComparable with
                member x.CompareTo o =
                    match o with
                        | :? TypeGraphNode as o -> compare sortKey o.SortKey
                        | _ -> failwithf "[FShade] cannot compare GraphNode to %A" o


            override x.Equals o =
                System.Object.ReferenceEquals(x,o)

            override x.GetHashCode() =
                System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
      

            member x.AssignOrder(o : Order) =
                match HSet.toList dependencies with
                    | [] -> 
                        sortKey <- o.After(o.Root)

                    | deps ->
                        let max = deps |> List.map (fun d -> d.AssignOrder o) |> List.max
                        sortKey <- o.After(max)

                sortKey

        type TypeGraphState =
            {
                cache : Dict<CType, Option<TypeGraphNode>>
            }

        [<AutoOpen>]
        module private Helpers =
            let allDirectChildTypes (ctype : CType) =
                match ctype with
                    | CStruct(_, fields, _) -> 
                        fields |> List.map fst |> HSet.ofList

                    | CVector(t,_) 
                    | CMatrix(t, _, _) 
                    | CPointer(_, t) 
                    | CArray(t,_) ->
                        HSet.ofList [t]

                    | _ ->
                        HSet.empty

            let rec ofType (t : CType) =
                state {
                    let! (s : TypeGraphState) = State.get
                    match s.cache.TryGetValue t with
                        | Some n ->
                            return n
                        | _ ->
                            let! used = t |> allDirectChildTypes |> HSet.toList |> List.chooseS ofType |>> HSet.ofList

                            match t with
                                | CStruct(name, fields, _) ->
                                    let def = CStructDef(name, fields)
                                    let node = TypeGraphNode(Some def, used)
                                    s.cache.[t] <- Some node
                                    return Some node

                                | _ ->
                                    if HSet.isEmpty used then
                                        return None
                                    else
                                        return Some <| TypeGraphNode(None, used)


                }
            
            let flatten (graphs : list<TypeGraphNode>) =
                let order = SimpleOrder.create()
                for t in graphs do t.AssignOrder order |> ignore

                let rec visit (defs : HashSet<TypeGraphNode>) (d : TypeGraphNode) =
                    if defs.Add d then
                        for dep in d.Dependencies do visit defs dep

                let set = HashSet()
                for t in graphs do visit set t

                set |> Seq.toArray |> Array.sort |> Array.toList |> List.choose (fun t -> t.Definition)

        let ofTypes (types : list<CType>) =
            let compile =
                state {
                    let! nodes = types |> List.chooseS ofType
                    return nodes
                }

            let state = 
                ref { 
                    cache = Dict(Unchecked.hash, Unchecked.equals) 
                }
            
            compile.Run(state) |> flatten

    let compile (backend : Backend) (m : Module) =
        let values, types = ValueCompiler.ofModule backend m
        let types = TypeCompiler.ofTypes types

        {
            CModule.cuserData = m.userData
            CModule.types = types
            CModule.values = values
        }


[<AbstractClass; Sealed; Extension>]
type Compiler private() =

    [<Extension>]
    static member Compile(backend : Compiler.Backend, m : Module) =
        ModuleCompiler.compile backend m
