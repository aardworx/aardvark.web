namespace Aardvark.Base


module Log =
    open Fable.Import.JS


    let start fmt = Printf.kprintf (fun str -> console.group str) fmt
    let stop () = console.groupEnd()
    let line fmt = Printf.kprintf (fun str -> console.log(str)) fmt
    let warn fmt = Printf.kprintf (fun str -> console.warn(str)) fmt
    let error fmt = Printf.kprintf (fun str -> console.error(str)) fmt
