module Bla 
open Fable.Core
open Aardvark.Import.JS
open Aardvark.Import.Browser
open Aardvark.Base
open Aardvark.Data

[<Emit("postMessage($0)")>]
let private postMessage (msg : obj) : unit = jsNative

//[<EntryPoint>]
//let main args =
let self = self |> unbox<Worker>

self.addEventListener_message (fun e ->
    PointCloudImporter.execute postMessage (unbox e.data)
)

//    0