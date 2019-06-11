// ts2fable 0.0.0
module rec hammer
open System
open Fable.Core
open Aardvark.Import.JS
open Aardvark.Import.Browser

let [<Import("*","hammerjs")>] Hammer: HammerStatic = jsNative

type [<AllowNullLiteral>] IExports =
    abstract Hammer: HammerStatic
    abstract HammerManagerConstructor: HammerManagerConstructorStatic
    abstract HammerInput: HammerInputStatic
    abstract MouseInput: MouseInputStatic
    abstract PointerEventInput: PointerEventInputStatic
    abstract SingleTouchInput: SingleTouchInputStatic
    abstract TouchInput: TouchInputStatic
    abstract TouchMouseInput: TouchMouseInputStatic
    abstract Recognizer: RecognizerStaticStatic
    abstract AttrRecognizer: AttrRecognizerStatic
    abstract PanRecognizer: PanRecognizerStaticStatic
    abstract PinchRecognizer: PinchRecognizerStaticStatic
    abstract PressRecognizer: PressRecognizerStaticStatic
    abstract RotateRecognizer: RotateRecognizerStaticStatic
    abstract SwipeRecognizer: SwipeRecognizerStaticStatic
    abstract TapRecognizer: TapRecognizerStaticStatic
    abstract TouchAction: TouchActionStatic

type [<AllowNullLiteral>] HammerStatic =
    abstract defaults: HammerDefaults with get, set
    abstract VERSION: float with get, set
    abstract INPUT_START: float with get, set
    abstract INPUT_MOVE: float with get, set
    abstract INPUT_END: float with get, set
    abstract INPUT_CANCEL: float with get, set
    abstract STATE_POSSIBLE: float with get, set
    abstract STATE_BEGAN: float with get, set
    abstract STATE_CHANGED: float with get, set
    abstract STATE_ENDED: float with get, set
    abstract STATE_RECOGNIZED: float with get, set
    abstract STATE_CANCELLED: float with get, set
    abstract STATE_FAILED: float with get, set
    abstract DIRECTION_NONE: float with get, set
    abstract DIRECTION_LEFT: float with get, set
    abstract DIRECTION_RIGHT: float with get, set
    abstract DIRECTION_UP: float with get, set
    abstract DIRECTION_DOWN: float with get, set
    abstract DIRECTION_HORIZONTAL: float with get, set
    abstract DIRECTION_VERTICAL: float with get, set
    abstract DIRECTION_ALL: float with get, set
    abstract Manager: HammerManagerConstructor with get, set
    abstract Input: HammerInput with get, set
    abstract TouchAction: TouchAction with get, set
    abstract TouchInput: TouchInput with get, set
    abstract MouseInput: MouseInput with get, set
    abstract PointerEventInput: PointerEventInput with get, set
    abstract TouchMouseInput: TouchMouseInput with get, set
    abstract SingleTouchInput: SingleTouchInput with get, set
    abstract Recognizer: RecognizerStatic with get, set
    abstract AttrRecognizer: AttrRecognizerStatic with get, set
    abstract Tap: TapRecognizerStatic with get, set
    abstract Pan: PanRecognizerStatic with get, set
    abstract Swipe: SwipeRecognizerStatic with get, set
    abstract Pinch: PinchRecognizerStatic with get, set
    abstract Rotate: RotateRecognizerStatic with get, set
    abstract Press: PressRecognizerStatic with get, set
    abstract on: target: EventTarget * types: string * handler: Function -> unit
    abstract off: target: EventTarget * types: string * handler: Function -> unit
    abstract each: obj: obj option * iterator: Function * context: obj option -> unit
    abstract merge: dest: obj option * src: obj option -> obj option
    abstract extend: dest: obj option * src: obj option * merge: bool -> obj option
    abstract ``inherit``: child: Function * ``base``: Function * properties: obj option -> obj option
    abstract bindFn: fn: Function * context: obj option -> Function
    abstract prefixed: obj: obj option * property: string -> string
    [<Emit "new Hammer($1...)">] abstract Create: element: U2<HTMLElement, SVGElement> * ?options: HammerOptions -> HammerManager


type RecognizerTuple =
    U4<RecognizerStatic, RecognizerStatic * RecognizerOptions, RecognizerStatic * RecognizerOptions * U2<string, ResizeArray<string>>, RecognizerStatic * RecognizerOptions * U2<string, ResizeArray<string>> * U2<U2<string, Recognizer>, ResizeArray<U2<string, Recognizer>>>>

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RecognizerTuple =
    let ofCase1 v: RecognizerTuple = v |> U4.Case1
    let isCase1 (v: RecognizerTuple) = match v with U4.Case1 _ -> true | _ -> false
    let asCase1 (v: RecognizerTuple) = match v with U4.Case1 o -> Some o | _ -> None
    let ofCase2 v: RecognizerTuple = v |> U4.Case2
    let isCase2 (v: RecognizerTuple) = match v with U4.Case2 _ -> true | _ -> false
    let asCase2 (v: RecognizerTuple) = match v with U4.Case2 o -> Some o | _ -> None
    let ofCase3 v: RecognizerTuple = v |> U4.Case3
    let isCase3 (v: RecognizerTuple) = match v with U4.Case3 _ -> true | _ -> false
    let asCase3 (v: RecognizerTuple) = match v with U4.Case3 o -> Some o | _ -> None
    let ofCase4 v: RecognizerTuple = v |> U4.Case4
    let isCase4 (v: RecognizerTuple) = match v with U4.Case4 _ -> true | _ -> false
    let asCase4 (v: RecognizerTuple) = match v with U4.Case4 o -> Some o | _ -> None

type [<AllowNullLiteral>] HammerDefaults =
    inherit HammerOptions
    abstract domEvents: bool with get, set
    abstract enable: bool with get, set
    abstract preset: ResizeArray<RecognizerTuple> with get, set
    abstract touchAction: string with get, set
    abstract cssProps: CssProps with get, set
    abstract inputClass: HammerInput with get, set
    abstract inputTarget: EventTarget with get, set

type [<AllowNullLiteral>] CssProps =
    abstract contentZooming: string with get, set
    abstract tapHighlightColor: string with get, set
    abstract touchCallout: string with get, set
    abstract touchSelect: string with get, set
    abstract userDrag: string with get, set
    abstract userSelect: string with get, set

type [<AllowNullLiteral>] HammerOptions =
    abstract cssProps: CssProps option with get, set
    abstract domEvents: bool option with get, set
    abstract enable: U2<bool, (HammerManager -> bool)> option with get, set
    abstract preset: ResizeArray<RecognizerTuple> option with get, set
    abstract touchAction: string option with get, set
    abstract recognizers: ResizeArray<RecognizerTuple> option with get, set
    abstract inputClass: HammerInput option with get, set
    abstract inputTarget: EventTarget option with get, set

type [<AllowNullLiteral>] HammerManagerConstructor =
    interface end

type [<AllowNullLiteral>] HammerManagerConstructorStatic =
    [<Emit "new $0($1...)">] abstract Create: element: EventTarget * ?options: HammerOptions -> HammerManagerConstructor

type  HammerListener = HammerInput -> unit

type [<AllowNullLiteral>] HammerManager =
    abstract add: recogniser: Recognizer -> Recognizer
    abstract add: recogniser: ResizeArray<Recognizer> -> Recognizer
    abstract destroy: unit -> unit
    abstract emit: ``event``: string * data: obj option -> unit
    abstract get: recogniser: Recognizer -> Recognizer
    abstract get: recogniser: string -> Recognizer
    abstract off: events: string * ?handler: HammerListener -> unit
    abstract on: events: string * handler: HammerListener -> unit
    abstract recognize: inputData: obj option -> unit
    abstract remove: recogniser: Recognizer -> HammerManager
    abstract remove: recogniser: string -> HammerManager
    abstract set: options: HammerOptions -> HammerManager
    abstract stop: force: bool -> unit

type [<AllowNullLiteral>] HammerInput =
    abstract destroy: unit -> unit
    abstract handler: unit -> unit
    abstract init: unit -> unit
    /// Name of the event. Like panstart. 
    abstract ``type``: string with get, set
    /// Movement of the X axis. 
    abstract deltaX: float with get, set
    /// Movement of the Y axis. 
    abstract deltaY: float with get, set
    /// Total time in ms since the first input. 
    abstract deltaTime: float with get, set
    /// Distance moved. 
    abstract distance: float with get, set
    /// Angle moved. 
    abstract angle: float with get, set
    /// Velocity on the X axis, in px/ms. 
    abstract velocityX: float with get, set
    /// Velocity on the Y axis, in px/ms 
    abstract velocityY: float with get, set
    /// Highest velocityX/Y value. 
    abstract velocity: float with get, set
    /// Direction moved. Matches the DIRECTION constants. 
    abstract direction: float with get, set
    /// Direction moved from it's starting point. Matches the DIRECTION constants. 
    abstract offsetDirection: float with get, set
    /// Scaling that has been done when multi-touch. 1 on a single touch. 
    abstract scale: float with get, set
    /// Rotation that has been done when multi-touch. 0 on a single touch. 
    abstract rotation: float with get, set
    /// Center position for multi-touch, or just the single pointer. 
    abstract center: HammerPoint with get, set
    /// Source event object, type TouchEvent, MouseEvent or PointerEvent. 
    abstract srcEvent: U3<TouchEvent, MouseEvent, PointerEvent> with get, set
    /// Target that received the event. 
    abstract target: HTMLElement with get, set
    /// Primary pointer type, could be touch, mouse, pen or kinect. 
    abstract pointerType: string with get, set
    /// Event type, matches the INPUT constants. 
    abstract eventType: float with get, set
    /// true when the first input. 
    abstract isFirst: bool with get, set
    /// true when the final (last) input. 
    abstract isFinal: bool with get, set
    /// Array with all pointers, including the ended pointers (touchend, mouseup). 
    abstract pointers: ResizeArray<obj option> with get, set
    /// Array with all new/moved/lost pointers. 
    abstract changedPointers: ResizeArray<obj option> with get, set
    /// Reference to the srcEvent.preventDefault() method. Only for experts! 
    abstract preventDefault: Function with get, set

type [<AllowNullLiteral>] HammerInputStatic =
    [<Emit "new $0($1...)">] abstract Create: manager: HammerManager * callback: Function -> HammerInput

type [<AllowNullLiteral>] MouseInput =
    inherit HammerInput

type [<AllowNullLiteral>] MouseInputStatic =
    [<Emit "new $0($1...)">] abstract Create: manager: HammerManager * callback: Function -> MouseInput

type [<AllowNullLiteral>] PointerEventInput =
    inherit HammerInput

type [<AllowNullLiteral>] PointerEventInputStatic =
    [<Emit "new $0($1...)">] abstract Create: manager: HammerManager * callback: Function -> PointerEventInput

type [<AllowNullLiteral>] SingleTouchInput =
    inherit HammerInput

type [<AllowNullLiteral>] SingleTouchInputStatic =
    [<Emit "new $0($1...)">] abstract Create: manager: HammerManager * callback: Function -> SingleTouchInput

type [<AllowNullLiteral>] TouchInput =
    inherit HammerInput

type [<AllowNullLiteral>] TouchInputStatic =
    [<Emit "new $0($1...)">] abstract Create: manager: HammerManager * callback: Function -> TouchInput

type [<AllowNullLiteral>] TouchMouseInput =
    inherit HammerInput

type [<AllowNullLiteral>] TouchMouseInputStatic =
    [<Emit "new $0($1...)">] abstract Create: manager: HammerManager * callback: Function -> TouchMouseInput

type [<AllowNullLiteral>] RecognizerOptions =
    abstract direction: float option with get, set
    abstract enable: U2<bool, (Recognizer -> HammerInput -> bool)> option with get, set
    abstract ``event``: string option with get, set
    abstract interval: float option with get, set
    abstract pointers: float option with get, set
    abstract posThreshold: float option with get, set
    abstract taps: float option with get, set
    abstract threshold: float option with get, set
    abstract time: float option with get, set
    abstract velocity: float option with get, set

type [<AllowNullLiteral>] RecognizerStatic =
    interface end

type [<AllowNullLiteral>] RecognizerStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> RecognizerStatic

type [<AllowNullLiteral>] Recognizer =
    abstract defaults: obj option with get, set
    abstract canEmit: unit -> bool
    abstract canRecognizeWith: otherRecognizer: Recognizer -> bool
    abstract dropRecognizeWith: otherRecognizer: U3<Recognizer, ResizeArray<Recognizer>, string> -> Recognizer
    abstract dropRequireFailure: otherRecognizer: U3<Recognizer, ResizeArray<Recognizer>, string> -> Recognizer
    abstract emit: input: HammerInput -> unit
    abstract getTouchAction: unit -> ResizeArray<obj option>
    abstract hasRequireFailures: unit -> bool
    abstract ``process``: inputData: HammerInput -> string
    abstract recognize: inputData: HammerInput -> unit
    abstract recognizeWith: otherRecognizer: U3<Recognizer, ResizeArray<Recognizer>, string> -> Recognizer
    abstract requireFailure: otherRecognizer: U3<Recognizer, ResizeArray<Recognizer>, string> -> Recognizer
    abstract reset: unit -> unit
    abstract set: ?options: RecognizerOptions -> Recognizer
    abstract tryEmit: input: HammerInput -> unit

type [<AllowNullLiteral>] AttrRecognizerStatic =
    abstract attrTest: input: HammerInput -> bool
    abstract ``process``: input: HammerInput -> obj option
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> AttrRecognizer

type [<AllowNullLiteral>] AttrRecognizer =
    inherit Recognizer

type [<AllowNullLiteral>] PanRecognizerStatic =
    interface end

type [<AllowNullLiteral>] PanRecognizerStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> PanRecognizerStatic

type [<AllowNullLiteral>] PanRecognizer =
    inherit AttrRecognizer

type [<AllowNullLiteral>] PinchRecognizerStatic =
    interface end

type [<AllowNullLiteral>] PinchRecognizerStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> PinchRecognizerStatic

type [<AllowNullLiteral>] PinchRecognizer =
    inherit AttrRecognizer

type [<AllowNullLiteral>] PressRecognizerStatic =
    interface end

type [<AllowNullLiteral>] PressRecognizerStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> PressRecognizerStatic

type [<AllowNullLiteral>] PressRecognizer =
    inherit AttrRecognizer

type [<AllowNullLiteral>] RotateRecognizerStatic =
    interface end

type [<AllowNullLiteral>] RotateRecognizerStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> RotateRecognizerStatic

type [<AllowNullLiteral>] RotateRecognizer =
    inherit AttrRecognizer

type [<AllowNullLiteral>] SwipeRecognizerStatic =
    interface end

type [<AllowNullLiteral>] SwipeRecognizerStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> SwipeRecognizerStatic

type [<AllowNullLiteral>] SwipeRecognizer =
    inherit AttrRecognizer

type [<AllowNullLiteral>] TapRecognizerStatic =
    interface end

type [<AllowNullLiteral>] TapRecognizerStaticStatic =
    [<Emit "new $0($1...)">] abstract Create: ?options: RecognizerOptions -> TapRecognizerStatic

type [<AllowNullLiteral>] TapRecognizer =
    inherit AttrRecognizer

type [<AllowNullLiteral>] TouchAction =
    abstract compute: unit -> string
    abstract preventDefaults: input: HammerInput -> unit
    abstract preventSrc: srcEvent: obj option -> unit
    abstract set: value: string -> unit
    abstract update: unit -> unit

type [<AllowNullLiteral>] TouchActionStatic =
    [<Emit "new $0($1...)">] abstract Create: manager: HammerManager * value: string -> TouchAction

type [<AllowNullLiteral>] HammerPoint =
    abstract x: float with get, set
    abstract y: float with get, set
