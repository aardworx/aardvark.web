namespace Aardvark.Base.Incremental

open System
open System.Threading
open System.Runtime.CompilerServices
open System.Collections.Generic
open Aardvark.Base

module private AttributeTargets =
    [<Literal>]
    let Type =
        AttributeTargets.Class |||
        AttributeTargets.Delegate |||
        AttributeTargets.Enum |||
        AttributeTargets.GenericParameter |||
        AttributeTargets.Interface |||
        AttributeTargets.Struct
   
[<Sealed; AttributeUsage(AttributeTargets.Type, AllowMultiple = false)>]
type DomainTypeAttribute() = inherit System.Attribute()

[<Sealed; AttributeUsage(AttributeTargets.Property ||| AttributeTargets.Field, AllowMultiple = false)>]
type PrimaryKeyAttribute() = inherit System.Attribute()

[<Sealed; AttributeUsage(AttributeTargets.Property ||| System.AttributeTargets.Field, AllowMultiple = false)>]
type TreatAsValueAttribute() = inherit System.Attribute()

[<Sealed; AttributeUsage(AttributeTargets.Property ||| System.AttributeTargets.Field, AllowMultiple = false)>]
type NonIncrementalAttribute() = inherit System.Attribute()

[<Sealed; AttributeUsage(AttributeTargets.Property ||| System.AttributeTargets.Field, AllowMultiple = false)>]
type NoDeepEqualityAttribute() = inherit System.Attribute()

[<Sealed; AttributeUsage(AttributeTargets.Property ||| System.AttributeTargets.Field, AllowMultiple = false)>]
type LongRunningAttribute() = inherit System.Attribute()

[<AllowNullLiteral>]
type Id() =
    static let mutable current = 0
    #if FABLE_COMPILER
    let id =
        let v = current + 1
        current <- v
        v
    #else
    let id = Interlocked.Increment(&current)
    #endif

    static member New = Id()
    override x.ToString() = sprintf "Id %d" id






