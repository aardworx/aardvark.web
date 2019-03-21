namespace Aardvark.Base

open System
open Aardvark.Base


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec =
    let inline x (a : ^a) : ^b = (^a : (member X : ^b) (a))
    let inline y (a : ^a) : ^b = (^a : (member Y : ^b) (a))
    let inline z (a : ^a) : ^b = (^a : (member Z : ^b) (a))
    let inline w (a : ^a) : ^b = (^a : (member W : ^b) (a))
    
    let inline xy (a : ^a) : ^b = (^a : (member XY : ^b) (a))
    let inline yz (a : ^a) : ^b = (^a : (member YZ : ^b) (a))
    let inline zw (a : ^a) : ^b = (^a : (member ZW : ^b) (a))
    let inline xyz (a : ^a) : ^b = (^a : (member XYZ : ^b) (a))
    let inline yzw (a : ^a) : ^b = (^a : (member YZW : ^b) (a))


    
    let inline dot (a : ^a) (b : ^a) : ^b = (^a : (static member Dot : ^a * ^a -> ^b) (a, b))
    let inline cross (a : ^a) (b : ^a) : ^b = (^a : (static member Cross : ^a * ^a -> ^b) (a, b))
    
    let inline normalize (a : ^a) : ^a = (^a : (member Normalized : ^a) (a))
    let inline length (a : ^a) : ^b = (^a : (member Length : ^b) (a))
    let inline lengthSquared (a : ^a) : ^b = (^a : (member LengthSquared : ^b) (a))


    let inline reflect (v : ^a) (n : ^a) : ^a =
        let t = (^a : (static member Dot : ^a -> ^a -> ^b) (v,n))
        v - (t + t) * n


    let inline refract (v : ^a) (n : ^a) (eta : ^b) =
        let t = (^a : (static member Dot : ^a -> ^a -> ^b) (v,n))

        let one = LanguagePrimitives.GenericOne
        let k = one - eta * eta * (one - t*t)
        if k < LanguagePrimitives.GenericZero then
            LanguagePrimitives.GenericZero
        else
            eta * v - (eta * t + sqrt k) * n


    let inline anySmaller< ^a, ^b when ^a : (member AnySmaller : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AnySmaller : ^b -> bool) (v,value))

    let inline anyGreater< ^a, ^b when ^a : (member AnyGreater : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AnyGreater : ^b -> bool) (v,value))

    let inline allSmaller< ^a, ^b when ^a : (member AllSmaller : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AllSmaller : ^b -> bool) (v,value))

    let inline allGreater< ^a, ^b when ^a : (member AllGreater : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AllGreater : ^b -> bool) (v,value))

    let inline anySmallerOrEqual< ^a, ^b when ^a : (member AnySmallerOrEqual : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AnySmallerOrEqual : ^b -> bool) (v,value))

    let inline anyGreaterOrEqual< ^a, ^b when ^a : (member AnyGreaterOrEqual : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AnyGreaterOrEqual : ^b -> bool) (v,value))

    let inline allSmallerOrEqual< ^a, ^b when ^a : (member AllSmallerOrEqual : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AllSmallerOrEqual : ^b -> bool) (v,value))

    let inline allGreaterOrEqual< ^a, ^b when ^a : (member AllGreaterOrEqual : ^b -> bool)> (v : ^a) (value : ^b) =
        (^a : (member AllGreaterOrEqual : ^b -> bool) (v,value))

