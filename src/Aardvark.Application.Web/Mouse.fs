namespace Aardvark.Application

open Fable.Core
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Base.Incremental
open System
open Aardvark.Base

type PixelPosition(pos : V2d, size : V2d) =
    member x.Position = pos
    member x.ViewportSize = size

    member x.Ndc =
        V2d(
            2.0 * pos.X / size.X - 1.0,
            2.0 * pos.Y / size.Y - 1.0
        )

    member x.NormalizedPosition =
        pos / size

    override x.GetHashCode() =
        HashCode.Combine(Unchecked.hash pos, Unchecked.hash size)

    override x.Equals o =
        match o with
        | :? PixelPosition as o -> pos = o.Position && size = o.ViewportSize
        | _ -> false

    interface IComparable with
        member x.CompareTo o =
            match o with
            | :? PixelPosition as o ->
                let c = compare pos o.Position 
                if c = 0 then compare size o.ViewportSize
                else c
            | _ ->
                failwith "uncomparable"

type MouseButtons =
    | None   = 0
    | Left   = 1
    | Middle = 2
    | Right  = 3
    | Button4 = 4
    | Button5 = 5

type MouseAction =
    | Down of MouseButtons * PixelPosition
    | Up of MouseButtons * PixelPosition
    | Click of MouseButtons * PixelPosition
    | DoubleClick of MouseButtons * PixelPosition
    | Move of PixelPosition
    | Scroll of float
    | Enter of PixelPosition
    | Exit of PixelPosition


type IMouse =
    inherit IDisposable

    abstract member Actions : IObservable<MouseAction>

    abstract member Position : IMod<PixelPosition>
    abstract member IsDown : MouseButtons -> IMod<bool>
    abstract member TotalScroll : IMod<float>
    abstract member Inside : IMod<bool>

    abstract member Down : IObservable<MouseButtons>
    abstract member Up : IObservable<MouseButtons>
    abstract member Move : IObservable<PixelPosition * PixelPosition>
    abstract member Click : IObservable<MouseButtons>
    abstract member DoubleClick : IObservable<MouseButtons>
    abstract member Scroll : IObservable<float>
    abstract member Enter : IObservable<PixelPosition>
    abstract member Leave : IObservable<PixelPosition> 

type Subject<'a>() =
    let mutable id = 0
    let mutable observers = System.Collections.Generic.Dictionary<int, IObserver<'a>>()

    member x.OnNext(value : 'a) =
        if not (isNull observers) then
            for o in observers.Values do
                o.OnNext(value)

    member x.OnCompleted() =
        if not (isNull observers) then
            for o in observers.Values do
                o.OnCompleted()

    member x.OnError(err : exn) =
        if not (isNull observers) then
            for o in observers.Values do
                o.OnError(err)

    member x.Dispose() =
        if not (isNull observers) then
            observers.Clear()
            observers <- null
            
    member x.Subscribe(obs : IObserver<'a>) =
        if not (isNull observers) then
            let i = id
            id <- i + 1
            observers.[i] <- obs
            { new IDisposable with member x.Dispose() = observers.Remove i |> ignore }
        else
            { new IDisposable with member x.Dispose() = () }

    interface IObservable<'a> with
        member x.Subscribe(obs : IObserver<'a>) = x.Subscribe(obs)

    interface IDisposable with
        member x.Dispose() = x.Dispose()


open Fable.Core.JsInterop
type Mouse(c : HTMLElement) =

    let mutable states =
        [|
            Mod.init false
            Mod.init false
            Mod.init false
            Mod.init false
            Mod.init false
        |]


    let handleButton (down : bool) (w : float) =
        let i = int w - 1
        if i >= 0 && i < 5 then transact (fun () -> states.[i].Value <- down)

    let pos = Mod.init (PixelPosition(V2d.Zero, V2d.II))
    let totalScroll = Mod.init 0.0
    let inside = Mod.init true

    let pixelPos (e : MouseEvent) =
        let r = c.getBoundingClientRect()
        let pp = PixelPosition(V2d(e.clientX, r.height - e.clientY), V2d(r.width, r.height))
        pp

    let setPos (i : bool) (e : MouseEvent) =
        let pp = pixelPos e
        transact (fun () ->
            inside.Value <- i
            pos.Value <- pp
        )
        
    let setPosAndScroll (e : MouseWheelEvent) =
        let pp = pixelPos e
        transact (fun () -> 
            inside.Value <- true
            pos.Value <- pp
            totalScroll.Value <- totalScroll.Value + (e.wheelDelta / 120.0)
        )


    let dblclick = new Subject<MouseButtons>()
    let click = new Subject<MouseButtons>()
    let down = new Subject<MouseButtons>()
    let up = new Subject<MouseButtons>()
    let move = new Subject<PixelPosition * PixelPosition>()
    let wheel = new Subject<float>()
    let enter = new Subject<PixelPosition>()
    let exit = new Subject<PixelPosition>()

    let events =
        lazy (
            let s = new Subject<MouseAction>()


            dblclick.Subscribe(fun b -> s.OnNext(MouseAction.DoubleClick(b, pos.Value))) |> ignore
            click.Subscribe(fun b -> s.OnNext(MouseAction.Click(b, pos.Value))) |> ignore
            down.Subscribe(fun b -> s.OnNext(MouseAction.Down(b, pos.Value))) |> ignore
            up.Subscribe(fun b -> s.OnNext(MouseAction.Up(b, pos.Value))) |> ignore
            move.Subscribe(fun (_,t) -> s.OnNext(MouseAction.Move t)) |> ignore
            wheel.Subscribe(fun d -> s.OnNext(MouseAction.Scroll d)) |> ignore
            enter.Subscribe(fun d -> s.OnNext(MouseAction.Enter d)) |> ignore
            exit.Subscribe(fun d -> s.OnNext(MouseAction.Exit d)) |> ignore
            s
        )


    static let createHandler (handler : 'e -> unit) = U2.Case2 { new EventListenerObject with member x.handleEvent e = let e = unbox<'e> e in handler e }
    
    let enterHandler    = createHandler (fun (e : MouseEvent) ->        setPos true e; enter.OnNext(pos.Value))
    let exitHandler     = createHandler (fun (e : MouseEvent) ->        setPos false e; exit.OnNext(pos.Value))
    let dblclickHandler = createHandler (fun (e : MouseEvent) ->        setPos true e; dblclick.OnNext(unbox (int e.which)); e.preventDefault(); e.stopPropagation())    
    let clickHandler    = createHandler (fun (e : MouseEvent) ->        setPos true e; click.OnNext(unbox (int e.which)); e.preventDefault(); e.stopPropagation() )      
    let downHandler     = createHandler (fun (e : MouseEvent) ->        setPos true e; down.OnNext(unbox (int e.which)); e.preventDefault(); e.stopPropagation(); handleButton true e.which )
    let upHandler       = createHandler (fun (e : MouseEvent) ->        setPos true e; up.OnNext(unbox (int e.which)); e.preventDefault(); e.stopPropagation();  handleButton false e.which)
    let moveHandler     = createHandler (fun (e : MouseEvent) ->        let o = pos.Value in setPos true e; move.OnNext(o, pos.Value); e.preventDefault(); e.stopPropagation())
    let wheelHandler    = createHandler (fun (e : MouseWheelEvent) ->   setPosAndScroll e; (if unbox e.wheelDelta then wheel.OnNext(e.wheelDelta / 120.0) else wheel.OnNext(e?deltaY / -3.0)); e.preventDefault(); e.stopPropagation() )
    let contextHandler  = createHandler (fun (e : Event) ->             e.preventDefault())

    do
        c.addEventListener("mouseover", enterHandler, true)
        c.addEventListener("mouseout", exitHandler, true)
        c.addEventListener("mousedown", downHandler, true)
        c.addEventListener("mousedown", downHandler, true)
        c.addEventListener("mouseup", upHandler, true)
        c.addEventListener("mousemove", moveHandler, true)
        c.addEventListener("wheel", wheelHandler, true)
        c.addEventListener("click", clickHandler, true)
        c.addEventListener("auxclick", clickHandler, true)
        c.addEventListener("dblclick", dblclickHandler, true)
        c.addEventListener("contextmenu", contextHandler)
        

    member x.Inside = inside :> IMod<_>
    member x.TotalScroll = totalScroll :> IMod<_>
    member x.IsDown(b : MouseButtons) = 
        if b >= MouseButtons.Left && b <= MouseButtons.Button5 then
            states.[int b - 1] :> IMod<_>
        else
            Mod.constant false

    member x.Actions = events.Value :> IObservable<_>
    member x.Position = pos :> IMod<_>
    member x.Enter = enter :> IObservable<_>
    member x.Leave = exit :> IObservable<_>
    member x.DoubleClick = dblclick :> IObservable<_>
    member x.Click = click :> IObservable<_>
    member x.Down = down :> IObservable<_>
    member x.Up = up :> IObservable<_>
    member x.Move = move :> IObservable<_>
    member x.Scroll = wheel :> IObservable<_>

    member x.Dispose() =
        c.removeEventListener("mouseover", enterHandler, true)
        c.removeEventListener("mouseout", exitHandler, true)
        c.removeEventListener("mousedown", downHandler, true)
        c.removeEventListener("mouseup", upHandler, true)
        c.removeEventListener("mousemove", moveHandler, true)
        c.removeEventListener("mousewheel", wheelHandler, true)
        c.removeEventListener("click", clickHandler, true)
        c.removeEventListener("auxclick", clickHandler, true)
        c.removeEventListener("dblclick", dblclickHandler, true)
        c.removeEventListener("contextmenu", contextHandler)
        events.Value.Dispose()
        down.Dispose()
        up.Dispose()
        move.Dispose()
        wheel.Dispose()
        click.Dispose()
        dblclick.Dispose()
        enter.Dispose()
        exit.Dispose()

    interface IDisposable with
        member x.Dispose() = x.Dispose()

    interface IMouse with
        member this.Click  = this.Click
        member this.DoubleClick = this.DoubleClick
        member this.Down = this.Down
        member this.Enter = this.Enter
        member this.Actions = this.Actions
        member this.Inside = this.Inside
        member this.IsDown b = this.IsDown b
        member this.Leave = this.Leave
        member this.Move = this.Move
        member this.Position = this.Position
        member this.Scroll = this.Scroll
        member this.TotalScroll = this.TotalScroll
        member this.Up = this.Up