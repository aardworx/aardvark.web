﻿namespace Aardvark.Base.Incremental

type afun<'a, 'b> =
    inherit IAdaptiveObject
    abstract member Evaluate : AdaptiveToken * 'a -> 'b



module AFun =

    type AdaptiveFun<'a, 'b>(f : IMod<AdaptiveToken -> 'a -> 'b>) =
        inherit AdaptiveObject()

        override x.Kind = "AFun"

        member x.Evaluate (caller, v) = 
            x.EvaluateAlways caller (fun token ->
                x.OutOfDate <- true
                f.GetValue token token v
            )

        interface afun<'a, 'b> with
            member x.Evaluate (c, v) = x.Evaluate(c, v)

        new(f) = AdaptiveFun (Mod.constant f)

    type ConstantFun<'a, 'b>(value : IMod<'b>) =
        inherit AdaptiveObject()
        
        override x.Kind = "AFun"

        member x.Evaluate (token : AdaptiveToken, v : 'a) = 
            x.EvaluateAlways token (fun token ->
                value.GetValue token
            )

        interface afun<'a, 'b> with
            member x.Evaluate (token, v) = x.Evaluate (token, v)

    let run (v : 'a) (f : afun<'a, 'b>) =
        Mod.custom (fun s -> 
            f.Evaluate (s, v)
        )

    let apply (v : IMod<'a>) (f : afun<'a, 'b>) =
        Mod.custom (fun s -> 
            f.Evaluate (s, v.GetValue s)
        )

    let create (f : 'a -> 'b) =
        AdaptiveFun(fun _ -> f) :> afun<_,_>

    let constant (v : 'b) : afun<'a, 'b> =
        ConstantFun(Mod.constant v) :> afun<_,_>

    let ofMod (f : IMod<'a -> 'b>) =
        AdaptiveFun(f |> Mod.map (fun f _ -> f)) :> afun<_,_>

    let bind (f : 'a -> afun<'x, 'y>) (m : IMod<'a>) =
        let mf = m |> Mod.map f

        let inner = ref None
        let self = ref Unchecked.defaultof<_>
        self :=
            AdaptiveFun(fun token x ->
                let f = mf.GetValue(token)

                match !inner with
                    | Some f' when f' <> f ->
                        f'.RemoveOutput !self
                    | _ ->
                        ()
                mf.GetValue(token).Evaluate(token, x)
            )

        !self :> afun<_,_>

    let compose (g : afun<'b, 'c>) (f : afun<'a, 'b>) =
        AdaptiveFun(fun token v -> g.Evaluate(token, f.Evaluate(token, v))) :> afun<_,_>

    let zipWith (combine : 'b -> 'c -> 'd) (f : afun<'a,'b>) (g : afun<'a, 'c>) =
        AdaptiveFun(fun token v -> combine (f.Evaluate(token, v)) (g.Evaluate(token, v))) :> afun<_,_>

    let zip (f : afun<'a,'b>) (g : afun<'a, 'c>) =
        zipWith (fun a b -> (a,b)) f g

    let rec chain (l : list<afun<'a, 'a>>) =
        match l with
            | [] -> create id
            | [f] -> f
            | f::fs -> compose (chain fs) f

    let chainM (l : IMod<list<afun<'a, 'a>>>) =
        l |> Mod.map chain |> bind id

    let runChain l initial =
        l |> chain |> run initial

    let integrate (ti : IAdaptiveObject) (f : afun<'a, 'a>) (initial : 'a) =
        let input = Mod.init initial
        let inputChanged = ChangeTracker.track<'a>()
        initial |> inputChanged |> ignore

        Mod.custom (fun s -> 
            lock ti (fun () -> ti.Outputs.Remove input |> ignore)
            let v = (input :> IMod<_>).GetValue s
            let res = f.Evaluate(s, v)
            if inputChanged res then
                input.UnsafeCache <- res
                ti.Outputs.Add input |> ignore

            res
        )

[<AutoOpen>]
module ``AFun Builder`` =

    type AFunBuilder() =
        member x.Bind(m : IMod<'a>, f : 'a -> afun<'x, 'y>) =
            AFun.bind f m

        member x.Return(f : 'a -> 'b) =
            AFun.create f

        member x.ReturnFrom(f : afun<'a, 'b>) = 
            f

        member x.ReturnFrom(m : IMod<'a -> 'b>) =
            AFun.ofMod m

    let inline (>>.) (f : afun<'a, 'b>) (g : afun<'b, 'c>) =
        AFun.compose g f

    let inline (<<.) (g : afun<'b, 'c>) (f : afun<'a, 'b>) =
        AFun.compose g f

    let afun = AFunBuilder()



type astate<'s, 'a> = { runState : afun<'s, 's * 'a> }

module AState =
    let create (v : 'a) =
        { runState = AFun.create (fun s -> s,v) }

    let map (f : 'a -> 'b) (m : astate<'s, 'a>) =
        let changed = ChangeTracker.track<'a>()
        let cache = ref None
        { runState = 
            m.runState 
                |> AFun.compose (
                    AFun.create (fun (s,v) -> 
                        let c = changed v
                        match !cache with
                            | Some o when not c -> (s,o)
                            | _ ->
                                let n = f v
                                cache := Some n
                                (s,n)
                    )
                   ) 
        }

    let bind (f : 'a -> astate<'s, 'b>) (m : astate<'s, 'a>) =
        let cache : ref<Option<afun<_,_>>> = ref None
        let tracker = ChangeTracker.track<'a>()

        let self = 
            AFun.AdaptiveFun(fun token s ->
                let (s, v) = m.runState.Evaluate (token, s)

                let c = tracker v
                match !cache with
                    | Some old when not c ->
                        old.Evaluate (token, s)
                    | _ ->
                        let inner = (f v).runState
                        match !cache with
                            | Some old -> old.RemoveOutput token.Caller
                            | None -> ()
                        cache := Some inner

                        inner.Evaluate (token, s)
            )

        { runState = self }

    let bindMod (f : 'a -> astate<'s, 'b>) (m : IMod<'a>) =
        let mf = m |> Mod.map f

        let inner : ref<Option<afun<_,_>>> = ref None
        let self =
            AFun.AdaptiveFun(fun token s -> 
                let run = mf.GetValue(token).runState
                
                match !inner with
                    | Some old when old <> run ->
                        old.RemoveOutput token.Caller

                    | _ -> ()

                inner := Some run 
                run.Evaluate (token, s)
            )

        { runState = self :> afun<_,_> }

    let ofMod (m : IMod<'a>) : astate<'s, 'a> =
        let run = AFun.ofMod(m |> Mod.map (fun v s -> (s,v)))
        { runState = run }

    let ofAFun (m : afun<'a, 'b>) : astate<'s, 'a -> 'b> =
        let run = AFun.AdaptiveFun(fun token s -> (s,fun v -> m.Evaluate(token,v)))
        { runState = run }

    let getState<'s> = { runState = AFun.create (fun s -> (s,s)) }
    let putState (s : 's) = { runState = AFun.create (fun _ -> (s,())) }
    let modifyState (f : 's -> 's) = { runState = AFun.create (fun s -> (f s,())) }
    let modifyState' (f : 's -> 's) = { runState = AFun.create (fun s -> (f s,s)) }


[<AutoOpen>]
module ``AState Builder`` =
    type AStateBuilder() =
        member x.Bind(m : astate<'s, 'a>, f : 'a -> astate<'s, 'b>) =
            AState.bind f m

        member x.Bind(m : IMod<'a>, f : 'a -> astate<'s, 'b>) =
            AState.bindMod f m

        member x.Return(v : 'a) =
            AState.create v

    let astate = AStateBuilder()



type ControllerState = { prev : Map<int, obj>; pulled : Map<int, obj> }
type Controller<'a> = astate<ControllerState, 'a>

[<AutoOpen>]
module ``Controller Builder`` =
    open AState

    let preWith (f : 'a -> 'a -> 'b) (m : IMod<'a>) =
        if m.IsConstant then
            let v = m.GetValue(AdaptiveToken.Top)
            AState.create (f v v)
        else
            m |> AState.bindMod (fun v ->
                modifyState' (fun s -> { s with pulled = Map.add m.Id (v :> obj) s.pulled })
                    |> AState.map (fun s ->
                        let last =
                            match Map.tryFind m.Id s.prev with
                                | Some v -> unbox<'a> v
                                | _ -> v
                        f last v
                       )
            )

    let inline withLast (m : IMod<'a>) = preWith (fun a b -> (a,b)) m

    let inline pre (m : IMod<'a>) = preWith (fun a _ -> a) m

    let inline differentiate (m : IMod<'a>) = preWith (fun o n -> n - o) m



    type ControllerBuilder() =

        member x.Bind(m : Controller<'a>, f : 'a -> Controller<'b>) : Controller<'b> =
            AState.bind f m

        member x.Bind(m : IMod<'a>, f : 'a -> Controller<'b>) : Controller<'b> =
            AState.bindMod f m

        member x.Return(v : 'a) : Controller<'a> =
            AState.create v

        member x.ReturnFrom(v : IMod<'a -> 'b>) : Controller<'a -> 'b> =
            AState.ofMod v

        member x.ReturnFrom(v : afun<'a, 'b>) : Controller<'a -> 'b> =
            AState.ofAFun v

        member x.Zero() : Controller<'a -> 'a> =
            AState.create id

        member x.Run(c : Controller<'a -> 'b>) : afun<'a, 'b> =
            let initial = { prev = Map.empty; pulled = Map.empty }
            let state = Mod.init initial
            let res = c.runState |> AFun.apply state
            let stateChanged = ChangeTracker.track<ControllerState>()
            initial |> stateChanged |> ignore

            //let ti = AdaptiveObject.Time
            let mf =
                res |> Mod.map (fun (newState, v) ->
                    //lock ti (fun () -> ti.Outputs.Remove state |> ignore)

                    if newState.pulled <> newState.prev then
                        state.UnsafeCache <-  { prev = newState.pulled; pulled = Map.empty }
                        //lock ti (fun () -> ti.Outputs.Add state |> ignore)

                    v
                )
            AFun.ofMod mf

    let controller = ControllerBuilder()



[<AutoOpen>]
module ModExtensions = 
    //type AdaptiveFunc<'a>(func : AdaptiveToken -> 'a -> 'a) =
    //    inherit AdaptiveObject()
        
    //    static let identity = AdaptiveFunc<'a>(fun _ v -> v)
    //    static member Identity = identity

    //    override x.Kind = "AFun"

    //    member x.Run(caller : AdaptiveToken, v : 'a) =
    //        x.EvaluateAlways caller (fun caller ->
    //            if x.OutOfDate then
    //                func caller v
    //            else 
    //                v
    //        )

    module Mod =
        let withLast (f : 's -> 's -> 'a -> 'a) (state : IMod<'s>)  : afun<'a, 'a> =

            let oldState = ref Unchecked.defaultof<_> 
            let res =
                AFun.AdaptiveFun<'a, 'a>(fun res value ->
                    let s = state.GetValue res
                    let newA = f !oldState s value
                    oldState := s 
                    newA
                )
            oldState := state.GetValue(AdaptiveToken.Top)
            lock state (fun () ->
                state.Outputs.Add res |> ignore
                state.AddOutput res
            )

            res :> afun<_,_>

        let inline step (f : 's -> 'sd -> 'a -> 'a) (state : IMod<'s>) : afun<'a, 'a> =
            withLast (fun o n a -> f o (n - o) a) state

        let withTime (f : float -> float -> 'a -> 'a) (state : IMod<float>)  : afun<'a, 'a> =

            let oldState = ref <| -10.0

            let res =
                AFun.AdaptiveFun<'a, 'a>(fun t value ->
                    let s = state.GetValue t
                    let o = 
                        if !oldState < 0.0 then s
                        else !oldState
                    let newA = f o s value
                    oldState := s 
                    newA
                )
            //state.Outputs.Add res |> ignore
            //state.AddOutput res

            res :> afun<_,_>       

        let inline stepTime (f : float -> float -> 'a -> 'a) (state : IMod<float>) : afun<'a, 'a> =
            withTime (fun o n a -> f o (n - o) a) state


        let wrap (m : IMod<'a>) (c : IMod<afun<'a, 'a>>) =
            Mod.custom (fun t ->
                c.GetValue(t).Evaluate(t,(m.GetValue t))
            )
        let rec private int (initial : IMod<'a>) (controllers : list<IMod<afun<'a, 'a>>>): IMod<'a> =
            
            let mutable a = initial
            for e in controllers do
                a <- wrap a e

            a


            //match controllers with
            //    | c::cs ->
            //        let result = 
            //            Mod.custom (fun t ->
            //                c.GetValue(t).Evaluate(t,(initial.GetValue t))
            //            )

            //        int result cs

            //    | [] -> 
            //        initial

        let integrate (initial : 'a) (time : IMod<float>) (controllers : list<IMod<afun<'a, 'a>>>) =
            let currentValue = ref initial
            let current = Mod.custom (fun _ -> !currentValue)
            //let isSubscribed = ref false
            //time.AddOutput current
            //time |> Mod.registerCallback (fun _ -> current.MarkOutdated()) |> ignore
        
            let result = int current controllers
            //time.AddOutput current
            

            Mod.custom (fun token ->
                let v = result.GetValue token
                if !currentValue <> v then
                    currentValue := v
                    time.GetValue token |> ignore
                    time.Outputs.Add current |> ignore
                    //time.GetValue(AdaptiveToken(current, null)) |> ignore

                v
            )
