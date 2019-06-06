namespace Aardvark.Data

open System
open Aardvark.Base
open Aardvark.Data.Durable

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
