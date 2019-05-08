namespace Aardvark.Data


open Aardvark.Import.JS
open Microsoft.FSharp.Collections
open System
open Aardvark.Base

type pako =
    abstract member deflate : Uint8Array -> Uint8Array 
    abstract member inflate : Uint8Array -> Uint8Array

[<AutoOpen>]
module GlobalThings =
    open Fable.Core

    let [<Import("*", "pako")>] pako : pako = jsNative



type Database(urlFormat : string) =
    
    member x.GetString(file : string) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchBuffer url |> Prom.map (fun data ->
            let arr = Uint8Array.Create(data, 0, data.byteLength)
            System.Text.Encoding.UTF8.GetString (unbox<byte[]> arr)
        )

    member x.Get(file : string, gzip : bool) =
        let url = System.String.Format(urlFormat, file)
        Prom.fetchBuffer url |> Prom.map (fun data ->
            let data =
                if gzip then pako.inflate(Uint8Array.Create data).buffer
                else data
            let s = Aardvark.Data.Stream(data)
            let (def, o) = Aardvark.Data.DurableDataCodec.decode s
            if def = Durable.Octree.Node then
                let o = o |> unbox |> Map.add Durable.Octree.Buffer (data :> obj)
                (def, o :> obj)
            else
                (def, o)
        )

    member inline x.TryGet<'a>(file : string, gzip : bool) =
        x.Get(file, gzip) |> Prom.map (fun (def,o) ->
            match o with
            | :? 'a as o -> Some o
            | _ -> None
        )

