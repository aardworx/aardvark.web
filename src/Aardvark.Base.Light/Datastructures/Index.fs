namespace Aardvark.Base

open System
open System.Threading
open System.Collections.Generic

[<AbstractClass>]
type Index() =
    abstract member CompareTo : obj -> int
    abstract member After : unit -> Index
    abstract member Before : unit -> Index
    abstract member Between : Index -> Index

    [<CompilerMessage("Next is considered harmful", 4321, IsError=false, IsHidden=true)>]
    abstract member Next : Index

    default x.GetHashCode() = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(x)
    default x.Equals o = System.Object.ReferenceEquals(x,o)

    interface IComparable with
        member x.CompareTo(o : obj) = x.CompareTo o
        
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Index =

    type Time private(number : largeuint, dexp : int) =
    
        let number, dexp =
            if number.IsZero then
                number, 0
            else
                let mutable number = number
                let mutable dexp = dexp
                while number.GetBit 0 = 0u do
                    number <- number >>> 1
                    dexp <- dexp - 1
                number, dexp

        member private x.Number = number
        member private x.DenomiatorExp = dexp

        static member Zero = Time(largeuint.Zero, 0)
        static member One = Time(largeuint.One, 0)
        
        override x.ToString() =

            //let denomiatior = largeuint.One <<< dexp

            //let a = number
            //let b = denomiatior - number
            //if b < a then
            //    sprintf "1-%s*2^-%d" (string b) dexp

            //else
            sprintf "%s*2^-%d" (string number) dexp

        override x.GetHashCode() =
            let a = number.GetHashCode() 
            let b = dexp.GetHashCode()
            uint32 a ^^^ uint32 b + 0x9e3779b9u + (uint32 a <<< 6) + (uint32 a >>> 2) |> int

        override x.Equals o =
            match o with
            | :? Time as o -> number = o.Number && dexp = o.DenomiatorExp
            | _ -> false

        static member Between(l : Time, r : Time) =
            let le = l.DenomiatorExp
            let re = r.DenomiatorExp
            let c = compare le re
            let mutable a = Unchecked.defaultof<_>
            let mutable b = Unchecked.defaultof<_>
            let mutable e = 0
            if c < 0 then
                a <- l.Number <<< (re - le)
                b <- r.Number
                e <- re

            elif c > 0 then
                a <- l.Number
                b <- r.Number <<< (le - re)
                e <- le
            
            else
                a <- l.Number
                b <- r.Number
                e <- le

            //if a = b then failwith "equal Times"
            if largeuint.DistanceIsOne(a,b) then
                Time(a + b, e + 1)
            else
                Time((a + b) >>> 1, e)
        
        interface System.IComparable with
            member x.CompareTo o =
                match o with
                | :? Time as o -> 
                    if dexp < o.DenomiatorExp then
                        let a = number <<< (o.DenomiatorExp - dexp)
                        let b = o.Number
                        compare a b
                    elif o.DenomiatorExp < dexp then
                        let a = number
                        let b = o.Number <<< (dexp - o.DenomiatorExp)
                        compare a b
                    else
                        compare number o.Number
                | _ ->
                    failwith "uncomparable"

    [<StructuredFormatDisplay("{AsString}")>]
    type private TimeIndex(time : Time) =
        inherit Index()
        member x.Time = time
        
        member private x.AsString = string time

        override x.ToString() = string time
        override x.After() = TimeIndex(Time.Between(time, Time.One)) :> Index
        override x.Before() = TimeIndex(Time.Between(Time.Zero, time)) :> Index
        override x.Between(other) = 
            let other = unbox<TimeIndex> other
            TimeIndex(Time.Between(time, other.Time)) :> Index
        override x.CompareTo o =
            let other = unbox<TimeIndex> o
            compare time other.Time

        override x.Next = failwith "bad"
        
        override x.GetHashCode() = time.GetHashCode()
        override x.Equals o =
            match o with
            | :? TimeIndex as o -> time = o.Time
            | _ -> false

    let zero = TimeIndex(Time.Zero) :> Index
    let after (r : Index) = r.After()
    let before (r : Index) = r.Before()
    let between (l : Index) (r : Index) = l.Between r
