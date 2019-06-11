namespace Aardvark.UI

open Aardvark.Import.Browser
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.SceneGraph

[<AutoOpen>]
module Attributes =
    let click (callback : MouseEvent -> 'msg) =
        "click", AttributeValue.Event (EventCallbacks.singleton { useCapture = false; callback = fun e -> unbox e |> callback |> Seq.singleton })
        
    let style (value : string) =
        "style", AttributeValue.String value

    let clazz (value : string) =
        "class", AttributeValue.String value

    let pointerdown (callback : PointerEvent -> 'msg) =
        "pointerdown", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> unbox e |> callback |> Seq.singleton })
        
    let pointerup (callback : PointerEvent -> 'msg) =
        "pointerup", AttributeValue.Event(EventCallbacks.singleton { useCapture = true; callback = fun e -> unbox e |> callback |> Seq.singleton })
        
    let pointermove (callback : PointerEvent -> 'msg) =
        "pointermove", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> unbox e |> callback |> Seq.singleton })
        
    let wheel (callback : MouseWheelEvent -> 'msg) =
        "wheel", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> unbox e |> callback |> Seq.singleton })

    let keydown (callback : KeyboardEvent -> 'msg) =
        "keydown", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> unbox e |> callback |> Seq.singleton })
        
    let keyup (callback : KeyboardEvent -> 'msg) =
        "keyup", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> unbox e |> callback |> Seq.singleton })



    let contextmenu (callback : PointerEvent -> 'msg) =
        "contextmenu", AttributeValue.Event(EventCallbacks.singleton { useCapture = true; callback = fun e -> unbox e |> callback |> Seq.singleton })

    let blur (callback : unit -> 'msg) =
        "blur", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> callback() |> Seq.singleton })
        
    let rendered (callback : unit -> 'msg) =
        "rendered", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> callback() |> Seq.singleton })
            
    let boot (callback : HTMLElement -> Updater.Scope<'msg> -> unit) =
        "boot", AttributeValue.Event(EventCallbacks.singleton { useCapture = false; callback = fun e -> let (e,s) = unbox e in callback e s; Seq.empty })

    let always (key : string, v : AttributeValue<'msg>) =
        key, Mod.constant (Some v)
        
    let onlyWhen (c : IMod<bool>) (key : string, v : AttributeValue<'msg>) =
        key, (c |> Mod.map (function true -> Some v | false -> None))
