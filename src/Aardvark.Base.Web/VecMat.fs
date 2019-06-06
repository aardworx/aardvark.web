namespace Aardvark.Base

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Trafo =
    let forward (t : Trafo3d) = t.Forward
    let backward (t : Trafo3d) = t.Backward


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Mat =
    let upperLeftM33 (m : M44d) = M33d m
    let inline transformDir (m : M44d) (v : V3d) = m.TransformDir v  
    let inline transformPos (m : M44d) (v : V3d) = m.TransformPos v  
    let inline transpose (m : 'm) = (m :> NoSRTP.IHasTransposed<_>).Transposed
    let inline inverse (m : 'm) = (m :> NoSRTP.IHasInverse<_>).Inverse
    let inline det (m : 'm) = (m :> NoSRTP.IHasDet<_>).Det



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vec =
    let inline x<'a, 'b when 'a :> NoSRTP.IHasX<'b>> (a : 'a) : 'b = a.X
    let inline y<'a, 'b when 'a :> NoSRTP.IHasY<'b>> (a : 'a) : 'b = a.Y
    let inline z<'a, 'b when 'a :> NoSRTP.IHasZ<'b>> (a : 'a) : 'b = a.Z
    let inline w<'a, 'b when 'a :> NoSRTP.IHasW<'b>> (a : 'a) : 'b = a.W

    
    let inline xy<'a, 'b when 'a :> NoSRTP.IHasXY<'b>> (a : 'a) : 'b = a.XY
    let inline yz<'a, 'b when 'a :> NoSRTP.IHasYZ<'b>> (a : 'a) : 'b = a.YZ
    let inline zw<'a, 'b when 'a :> NoSRTP.IHasZW<'b>> (a : 'a) : 'b = a.ZW

    let inline xyz<'a, 'b when 'a :> NoSRTP.IHasXYZ<'b>> (a : 'a) : 'b = a.XYZ
    let inline yzw<'a, 'b when 'a :> NoSRTP.IHasYZW<'b>> (a : 'a) : 'b = a.YZW
    


    
    let inline dot<'a, 'b, 'c when 'a :> NoSRTP.IHasDot<'b, 'c>> (a : 'a) (b : 'b) : 'c = a.Dot(b)
    let inline cross<'a, 'b, 'c when 'a :> NoSRTP.IHasCross<'b, 'c>> (a : 'a) (b : 'b) : 'c = a.Cross(b)
    
    
    let inline normalize<'a, 'b when 'a :> NoSRTP.IHasNormalized<'b>> (a : 'a) : 'b = a.Normalized
    let inline lengthSquared<'a, 'b when 'a :> NoSRTP.IHasLengthSquared<'b>> (a : 'a) : 'b = a.LengthSquared
    let inline length<'a, 'b when 'a :> NoSRTP.IHasLength<'b>> (a : 'a) : 'b = a.Length

    let inline reflect (v : ^a) (n : ^a) : ^a =
        let t = (v :> NoSRTP.IHasDot<_,_>).Dot(n)
        v - (t + t) * n


    let inline refract (v : ^a) (n : ^a) (eta : ^b) =
        let t = (v :> NoSRTP.IHasDot<_,_>).Dot(n)

        let one = LanguagePrimitives.GenericOne
        let k = one - eta * eta * (one - t*t)
        if k < LanguagePrimitives.GenericZero then
            LanguagePrimitives.GenericZero
        else
            eta * v - (eta * t + sqrt k) * n
