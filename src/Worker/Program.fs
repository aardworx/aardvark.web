module Worker

open System
open Aardvark.Base
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Fable.Core

type Command =
    | Load of url : string

type Reply =
    | Text of string
    | Loaded of data : ArrayBuffer
    

type Message =
    {
        id      : int
        data    : obj
    }


let processMessage (msg : Command) =
   Some (Text <| sprintf "Worker got %A" msg)


[<EntryPoint>]
let main argv =
    self.addEventListener_message (fun e ->
        
        let msg = unbox<Message> e.data
        match processMessage (unbox msg.data) with
        | Some reply -> self.postMessage({ id = msg.id; data = reply }, null)
        | None -> ()
    )
    0
