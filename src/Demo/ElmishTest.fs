﻿module ElmTest


open Elmish
open Elmish.React
open Fable.React

//open Fable.Helpers.React
//open Fable.Helpers.React.Props

// MODEL

type Model = int

type Msg =
| Increment
| Decrement

let init() : Model = 0

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

// VIEW (rendered with React)

let view (model:Model) dispatch =
    failwith ""
  //div []
  //    [ button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
  //      div [] [ str (string model) ]
  //      button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] 
  //]

let run () =
    // App
    Program.mkSimple init update view
    |> Program.withReact "elmish-app"
    |> Program.withConsoleTrace
    |> Program.run