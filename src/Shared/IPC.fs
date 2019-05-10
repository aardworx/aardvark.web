namespace Aardvark.Data

open System
open Aardvark.Base
open Aardvark.Data.Durable

[<RequireQualifiedAccess>]
type Command =
    | Add of id : int * dburl : string
    | Remove of id : int
    | UpdateCamera of view : Trafo3d * proj : Trafo3d
    
[<RequireQualifiedAccess>]
type Reply =
    | Perform of array<Guid * Operation<Aardvark.Import.JS.ArrayBuffer>> //AtomicOperation<Guid, Map<Def, obj>>
    | Info of string
