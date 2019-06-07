namespace Aardvark.Data

open System
open Aardvark.Base
open Aardvark.Data.Durable

type Database =
    | Url of string
    | Local of name : string
    | Azure of token : string * id : Guid

module Database =
    let toString (db : Database) =
        match db with
        | Url u -> u
        | Local n -> sprintf "local://%s" n
        | Azure(token, id) -> sprintf "azure://%s/%s" token (string id)

    let parse (str : string) =
        if str.StartsWith "local://" then 
            str.Substring 8 |> Local |> Some
        elif str.StartsWith "azure://" then
            let rest = str.Substring(8).Split('/') 
            if rest.Length = 2 then
                Azure(rest.[0], System.Guid rest.[1]) |> Some
            else
                None
        else
            str |> Url |> Some

[<RequireQualifiedAccess>]
type Command =
    | Add of dburl : string
    | Remove of dburl : string
    | UpdateCamera of view : Trafo3d * proj : Trafo3d
    
[<RequireQualifiedAccess>]
type Reply =
    | SetRootCenter of url : string * center : V3d
    | Perform of array<Guid * Operation<Aardvark.Import.JS.ArrayBuffer>> //AtomicOperation<Guid, Map<Def, obj>>
    | Info of string
