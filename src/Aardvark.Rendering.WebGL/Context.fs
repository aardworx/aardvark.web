namespace Aardvark.Rendering.WebGL

open System
open Aardvark.Base
open Aardvark.Import.JS
open Aardvark.Import.Browser

type Context(gl : WebGL2RenderingContext) =
    let mutable id = 1

    member x.GL = gl

    member x.NewId() = 
        let r = id
        id <- r + 1
        r

[<AbstractClass>]
type Resource(ctx : Context) =
    let id = ctx.NewId()
    let mutable refCount = 1
    
    member x.Context = ctx

    member x.Acquire() = 
        refCount <- refCount + 1

    member x.Release() = 
        refCount <- refCount - 1
        if refCount = 0 then 
            x.Destroy()


    abstract member Destroy : unit -> unit

    member x.Id = id
    override x.GetHashCode() = id
    override x.Equals o =
        match o with
        | :? Resource as o -> id = o.Id
        | _ -> false

    
    interface IComparable with
        member x.CompareTo o =
            match o with
            | :? Resource as o -> compare id o.Id
            | _ -> failwith "uncomparable"


