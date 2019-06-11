namespace Aardvark.UI

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.Import.Browser

[<RequireQualifiedAccess>]
type DomNode<'msg> =
    internal
    | NEmpty
    | NText of tag : string * attributes : AttributeMap<'msg> * value : IMod<string>
    | NNode of tag : string * attributes : AttributeMap<'msg> * children : alist<DomNode<'msg>>
    | NRender of attributes : AttributeMap<'msg> * scene : (RenderControl -> ISg)
    | NMap of mapping : (obj -> seq<'msg>) * child : DomNode<obj>
    | NBoot of boot : Option<HTMLElement -> unit> * shutdown : Option<HTMLElement -> unit> * inner : DomNode<'msg>

    member x.TagName : Option<string> =
        match x with
        | NEmpty -> None
        | NText(t,_,_) -> Some t
        | NNode(t,_,_) -> Some t
        | NRender _ -> Some "canvas"
        | NMap(_,i) -> i.TagName
        | NBoot(_,_,i) -> i.TagName

    static member Empty : DomNode<'msg> = NEmpty

[<AbstractClass; Sealed>]
type DomNode private() =
    static member Text(tag : string, attributes : AttributeMap<'msg>, value : IMod<string>) = DomNode.NText(tag, attributes, value)
    static member Node(tag : string, attributes : AttributeMap<'msg>, children : alist<DomNode<'msg>>) = DomNode.NNode(tag, attributes, children)
    static member Render(attributes : AttributeMap<'msg>, scene : ISg) = DomNode.NRender(attributes, fun _ -> scene)
    static member Render(attributes : AttributeMap<'msg>, scene : RenderControl -> ISg) = DomNode.NRender(attributes, scene)
    static member Map(mapping : 'a -> 'b, m : DomNode<'a>) = DomNode.NMap((fun o -> Seq.singleton (mapping (unbox o))), unbox m)
    static member Choose (mapping : 'a -> Option<'b>, m : DomNode<'a>) : DomNode<'b> = DomNode.NMap((fun o -> match mapping (unbox o) with | Some v -> Seq.singleton v | None -> Seq.empty), unbox m)
    static member Collect (mapping : 'a -> seq<'b>, m : DomNode<'a>) : DomNode<'b> = DomNode.NMap((fun o -> mapping (unbox o)), unbox m)
    static member Ignore (m : DomNode<'a>) : DomNode<'msg> = DomNode.NMap((fun _ -> Seq.empty), unbox m)
