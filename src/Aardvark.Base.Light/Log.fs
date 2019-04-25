namespace Aardvark.Base


module Log =
    open Aardvark.Import.JS

    
    let mutable private currentTimers = []

    let startTime fmt = Printf.kprintf (fun str -> currentTimers <- str :: currentTimers; console.time str) fmt
    let stopTime() =
        match currentTimers with
        | h :: t ->
            console.timeEnd(h)
            currentTimers <- t
        | [] ->
            console.timeEnd()
            
    let startCollapsed fmt = Printf.kprintf (fun str -> console.groupCollapsed str) fmt
    let start fmt = Printf.kprintf (fun str -> console.group str) fmt
    let stop () = console.groupEnd()
    let debug fmt = Printf.kprintf (fun str -> ()) fmt
    let line fmt = Printf.kprintf (fun str -> console.log(str)) fmt
    let warn fmt = Printf.kprintf (fun str -> console.warn(str)) fmt
    let error fmt = Printf.kprintf (fun str -> console.error(str)) fmt
