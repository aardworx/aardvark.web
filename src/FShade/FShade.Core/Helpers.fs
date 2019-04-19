namespace FShade

open Aardvark.Base

module ConversionHelpers =
    let lookupTableOption (l : seq<'k * 'v>) =
        let d = Dict(Unchecked.hash, Unchecked.equals)
        for (k,v) in l do d.[k] <- v
        fun (k : 'k) ->
            match d.TryGetValue k with
            | Some r -> Some r
            | None -> None