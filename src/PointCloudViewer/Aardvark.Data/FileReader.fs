namespace Aardvark.Data

open Fable.Core
open Aardvark.Base
open Aardvark.Import.JS
open Aardvark.Import.Browser

module Blob =
    [<AutoOpen>]
    module private Helpers = 
        [<Emit("$0.charCodeAt($1)")>]
        let charCodeAt (str : string) (i : float) : float = jsNative

        type System.String with
            member inline x.charCodeAt(i : float) = charCodeAt x i

    type ReadLineMessage<'a> = 
        | Progress of totalSize : float * read : float * time : float
        | Chunk of 'a
        | Done

    let private lineBreak = [| '\n'; '\r'; char 0 |]

    let readLines (chunkSize : int) (init : unit -> 's) (parse : 's -> string -> unit) (emit : ReadLineMessage<'s> -> Promise<'a>) (b : Blob) : Promise<'a> =
        let nextNewLine (str : string) (o : int) =
            let fst = str.IndexOfAny(lineBreak, o)
            let mutable n = fst
            if n >= 0 then
                let mutable next = n + 1
                while next < str.Length && (let c = str.charCodeAt(float next) in c = 10.0  || c = 13.0 || c = 0.0) do
                    n <- n + 1
                fst, n
            else
                -1, -1

        let parseLines (str : string) =
            let state = init()
            let mutable cnt = 0
            let mutable o = 0
            let mutable ns, ne = nextNewLine str o
            while ns >= 0 do
                let line = str.Substring(o, ns - o)
                parse state line
                o <- ne + 1
                let (a,b) = nextNewLine str o
                ns <- a
                ne <- b
                cnt <- cnt + 1

            if cnt > 0 then 
                emit (Chunk state) |> Prom.map (fun _ -> o)
            else
                Prom.value o

        let startTime = performance.now()
        let rec readChunk (b : Blob) (start : float) (size : float) : Promise<'a> =
            let s = min (b.size - start) size
            if s <= 0.0 then
                emit Done
            else
                //Log.line "chunk %.0f %.0f" start (start + s)
                let chunk = b.slice(start, start + s)
                let r = FileReader.Create()

                let readCurrent =
                    Promise.Create(fun success error ->
                        r.addEventListener_load(fun e ->
                            let str = Fable.Core.JsInterop.(?) e.target "result" |> unbox<string>
                            success str
                        )
                        r.readAsText chunk
                    ) |> unbox<Promise<string>>

                promise {
                    let! str = readCurrent
                    let! off = parseLines(str)
                    if off = 0 then
                        return! readChunk b start (2.0 * size)
                    else
                        let oo = start + float off
                        //Log.warn "offset: %.0f" oo
                        let! _ = emit(Progress(b.size, oo, performance.now() - startTime))
                        return! readChunk b oo size
                    
                }

                //r.addEventListener_load(fun e ->
                //    let str = Fable.Core.JsInterop.(?) e.target "result" |> unbox<string>
                //    parseLines(str).``then``(fun off ->
                //        if off = 0 then
                //            readChunk b start (2.0 * size)
                //        else
                //            let oo = start + float off
                //            emit(Progress(b.size, oo, performance.now() - startTime)).``then`` (fun _ ->
                //                readChunk b oo size
                //            ) 
                //    )
                //)
                //r.readAsText chunk


        readChunk b 0.0 (float chunkSize)

