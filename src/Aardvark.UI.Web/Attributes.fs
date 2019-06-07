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

