﻿namespace Aardvark.SceneGraph


open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

type TraversalState =
    {
        shader              : FShade.Effect
        trafos              : list<IMod<Trafo3d>>
        viewTrafo           : IMod<Trafo3d>
        projTrafo           : IMod<Trafo3d>
        uniforms            : Map<string, IMod>
        vertexAttriubtes    : Map<string, BufferView>
        indexBuffer         : Option<BufferView>
        depthMode           : IMod<DepthTestMode>
    }



type ISg =
    abstract member RenderObjects : TraversalState -> aset<IRenderObject>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]    
module Sg =
    let private mulCache : Aardvark.Import.JS.WeakMap<IMod<Trafo3d>, Aardvark.Import.JS.WeakMap<IMod<Trafo3d>, IMod<Trafo3d>>> = Aardvark.Import.JS.WeakMap.Create() |> unbox
    let private cameraLocationCache : Aardvark.Import.JS.WeakMap<IMod, IMod<V3d>> = Aardvark.Import.JS.WeakMap.Create() |> unbox


    let (<*>) (l : IMod<Trafo3d>) (r : IMod<Trafo3d>) =
        if l.IsConstant && r.IsConstant then
            Mod.constant (l.GetValue(AdaptiveToken.Top) * r.GetValue(AdaptiveToken.Top))
        else
            let c = mulCache.get(l)
            if unbox c then
                let d = c.get(r)
                if unbox d then 
                    d
                else
                    let d = Mod.map2 (*) l r
                    c.set(r, d) |> ignore
                    d
            else
                let c = Aardvark.Import.JS.WeakMap.Create() |> unbox
                mulCache.set(l, c) |> ignore
                let d = Mod.map2 (*) l r
                c.set(r, d) |> ignore
                d
    module Option =
        let map2 (f : 'a -> 'b -> 'c) (a : Option<'a>) (b : Option<'b>) =
            match a with
            | Some a ->
                match b with
                | Some b -> Some (f a b)
                | None -> None
            | None ->
                None
        let map3 (f : 'a -> 'b -> 'c -> 'd) (a : Option<'a>) (b : Option<'b>) (c : Option<'c>) =
            match a with
            | Some a ->
                match b with
                | Some b -> 
                    match c with
                    | Some c -> Some (f a b c)
                    | None -> None
                | None -> None
            | None ->
                None

    let private mul (l : IMod) (r : IMod) = unbox l <*> unbox r :> IMod


    let private camLocation (m : IMod) =
        let r = cameraLocationCache.get m
        if unbox r then
            r
        else
            let m = unbox<IMod<Trafo3d>> m
            let r = m |> Mod.map (fun (t : Trafo3d) -> t.Backward.C3.XYZ)
            cameraLocationCache.set(m, r) |> ignore
            r
    let rec private getUniform (m : Map<string, IMod>) (name : string) =
        match Map.tryFind name m with
        | Some v -> Some v
        | None ->
            if name.EndsWith "Inv" then
                match getUniform m (name.Substring(0, name.Length - 3)) with
                | Some m -> unbox<IMod<Trafo3d>> m |> Mod.map (fun t -> t.Inverse) :> IMod |> Some
                | None -> None
            else
                match name with
                | "ModelViewTrafo" -> Option.map2 mul (getUniform m "ModelTrafo") (getUniform m "ViewTrafo")
                | "ViewProjTrafo" -> Option.map2 mul (getUniform m "ViewTrafo") (getUniform m "ProjTrafo")
                | "ModelViewProjTrafo" -> Option.map3 (fun a b c -> mul a (mul b c)) (getUniform m "ModelTrafo") (getUniform m "ViewTrafo") (getUniform m "ProjTrafo")
                
                | "NormalMatrix" -> Option.map (fun v -> v |> unbox<IMod<Trafo3d>> |> Mod.map (fun (t : Trafo3d) -> M33d t.Backward.Transposed) :> IMod) (getUniform m "ModelTrafo")

                | "CameraLocation" -> Option.map (fun v -> v |> camLocation :> IMod) (getUniform m "ViewTrafo")

                | _ -> None

    [<AbstractClass>]
    type ASg() =
        abstract member RenderObjects : TraversalState -> aset<IRenderObject>
        interface ISg with
            member x.RenderObjects s = x.RenderObjects s

    type EmptyNode() =
        inherit ASg()
        override x.RenderObjects _ = ASet.empty

    type RenderNode(mode : PrimitiveTopology, call : IMod<DrawCall>) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            
            let call =
                if unbox call then 
                    call
                else
                    let noCall = { faceVertexCount = 0; first = 0; instanceCount = 0 }
                    match state.indexBuffer with
                    | Some ib -> 
                        ib.buffer |> Mod.map (function :? HostBuffer as b ->  { faceVertexCount = b.Data.Length; first = 0; instanceCount = 1 } | _ -> noCall)
                    | None ->
                        let anyAtt = 
                            state.vertexAttriubtes
                            |> Map.toSeq
                            |> Seq.tryHead
                        match anyAtt with
                        | Some (_,att) ->
                            att.buffer |> Mod.map (fun b ->
                                match b with
                                | :? HostBuffer as b ->
                                     { faceVertexCount = b.Data.Length; first = 0; instanceCount = 1 }
                                | _ ->
                                    noCall

                            )
                        | None ->
                            Mod.constant noCall

            let uniforms = 
                state.uniforms
                |> Map.add "ModelTrafo" (List.foldBack (<*>) state.trafos (Mod.constant Trafo3d.Identity)  :> IMod)
                |> Map.add "ViewTrafo" (state.viewTrafo :> IMod)
                |> Map.add "ProjTrafo" (state.projTrafo :> IMod)

            let o =
                {
                    id = newId()
                    pipeline = { shader = state.shader; uniforms = getUniform uniforms; depthMode = state.depthMode }
                    vertexBuffers = state.vertexAttriubtes
                    indexBuffer = state.indexBuffer
                    mode = mode
                    call = call
                } :> IRenderObject
            ASet.single o

    type VertexAttributeApplicator(name : string, att : BufferView, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with vertexAttriubtes = Map.add name att state.vertexAttriubtes }
        
    type UniformApplicator(name : string, value : IMod, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with uniforms = Map.add name value state.uniforms }
                    
    type IndexApplicator(att : BufferView, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with indexBuffer = Some att }

    type TrafoApplicator(trafo : IMod<Trafo3d>, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with trafos = trafo :: state.trafos }
            
    type ViewTrafoApplicator(trafo : IMod<Trafo3d>, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with viewTrafo = trafo }
            
    type ProjTrafoApplicator(trafo : IMod<Trafo3d>, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with projTrafo = trafo }
            
    type EffectApplicator(shader : FShade.Effect, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with shader = shader }

    type Set(nodes : aset<ISg>) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            nodes |> ASet.collect (fun s -> s.RenderObjects(state))
        
    type DepthTestModeApplicator(mode : IMod<DepthTestMode>, sg : ISg) =
        inherit ASg()
        override x.RenderObjects (state : TraversalState) =
            sg.RenderObjects { state with depthMode = mode }

    type DynamicSg(inner : IMod<ISg>) =
        inherit ASg()
        
        override x.RenderObjects (state : TraversalState) =
            inner |> ASet.bind (fun i -> i.RenderObjects(state))



    type SgShaderBuilder() =
        member x.Yield(()) = []

        member x.Bind (e : FShade.Effect, f : unit -> list<FShade.Effect>) =
            e :: f()
            
        member x.Bind (e : ('a -> Microsoft.FSharp.Quotations.Expr<'b>), f : unit -> list<FShade.Effect>, [<Fable.Core.Inject>] ?ra : Fable.Core.ITypeResolver<'a>) =
            let effect = FShade.Effect.ofFunction(e, ?ra = ra)
            effect :: f()

        member x.Zero() =
            []

        member x.Return(()) =
            []

        member x.Combine(l : list<FShade.Effect>, r : list<FShade.Effect>) = l @ r

        member x.Delay(f : unit -> list<FShade.Effect>) = f()

        member x.Run(l : list<FShade.Effect>) =
            fun (s : ISg) -> EffectApplicator(FShade.Effect.compose l, s) :> ISg




    let empty = EmptyNode() :> ISg
    let draw (mode : PrimitiveTopology) = RenderNode(mode, null) :> ISg
    let inline indexBuffer (att : IMod<'a>) (sg : ISg) = IndexApplicator(BufferView.ofArray att, sg) :> ISg
    let inline vertexBuffer (name : string) (att : IMod<'a>) (sg : ISg) = VertexAttributeApplicator(name, BufferView.ofArray att, sg) :> ISg
    let inline index (att : 'a) (sg : ISg) = IndexApplicator(BufferView.ofArray (Mod.constant att), sg) :> ISg
    let inline vertexAttribute (name : string) (att : 'a) (sg : ISg) = VertexAttributeApplicator(name, BufferView.ofArray (Mod.constant att), sg) :> ISg
    let trafo (t : IMod<Trafo3d>) (sg : ISg) = TrafoApplicator(t, sg) :> ISg
    let viewTrafo (t : IMod<Trafo3d>) (sg : ISg) = ViewTrafoApplicator(t, sg) :> ISg
    let projTrafo (t : IMod<Trafo3d>) (sg : ISg) = ProjTrafoApplicator(t, sg) :> ISg
    let effect (effect : list<FShade.Effect>) (sg : ISg) = EffectApplicator(FShade.Effect.compose effect, sg) :> ISg
    let set (sg : aset<ISg>) = Set(sg) :> ISg
    let ofSeq (sg : seq<ISg>) = sg |> ASet.ofSeq |> set
    let ofList (sg : list<ISg>) = sg |> ASet.ofList |> set
    let uniform (name : string) (value : IMod<'a>) (sg : ISg) = UniformApplicator(name, value :> IMod, sg) :> ISg
    let depthTest (mode : IMod<DepthTestMode>) (sg : ISg) = DepthTestModeApplicator(mode, sg) :> ISg
    let shader = SgShaderBuilder()
    let dynamic (m : IMod<ISg>) = DynamicSg(m) :> ISg
[<AutoOpen>]
module SgExtensions =

    module TraversalState =
        let empty =
            {
                shader = FShade.Effect.empty
                trafos = []
                depthMode = Mod.constant DepthTestMode.LessOrEqual
                viewTrafo = Mod.constant Trafo3d.Identity
                projTrafo = Mod.constant Trafo3d.Identity
                uniforms = Map.empty
                vertexAttriubtes = Map.empty
                indexBuffer = None
            }
    type ISg with
        member x.RenderObjects() = x.RenderObjects TraversalState.empty


module TraversalState =
    let modelTrafo (s : TraversalState) = List.foldBack (Sg.(<*>)) s.trafos (Mod.constant Trafo3d.Identity)
    let modelViewTrafo (s : TraversalState) = (Sg.(<*>)) (modelTrafo s) s.viewTrafo
    let viewProjTrafo (s : TraversalState) = (Sg.(<*>)) s.viewTrafo s.projTrafo
    let modelViewProjTrafo (s : TraversalState) = (Sg.(<*>)) (modelViewTrafo s) s.projTrafo