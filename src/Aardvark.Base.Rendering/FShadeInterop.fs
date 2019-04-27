namespace FShade

open Aardvark.Base

[<AutoOpen>]
module FShadeRenderingExtensions =

    type UniformScope with
        member x.ModelTrafo : M44d = uniform?PerModel?ModelTrafo
        member x.ViewTrafo : M44d = uniform?PerView?ViewTrafo
        member x.ProjTrafo : M44d = uniform?PerView?ProjTrafo
        member x.ModelViewTrafo : M44d = uniform?PerModel?ModelViewTrafo
        member x.ViewProjTrafo : M44d = uniform?PerView?ViewProjTrafo
        member x.ModelViewProjTrafo : M44d = uniform?PerModel?ModelViewProjTrafo

        
        member x.ModelTrafoInv : M44d = uniform?PerModel?ModelTrafoInv
        member x.ViewTrafoInv : M44d = uniform?PerView?ViewTrafoInv
        member x.ProjTrafoInv : M44d = uniform?PerView?ProjTrafoInv
        member x.ModelViewTrafoInv : M44d = uniform?PerModel?ModelViewTrafoInv
        member x.ViewProjTrafoInv : M44d = uniform?PerView?ViewProjTrafoInv
        member x.ModelViewProjTrafoInv : M44d = uniform?PerModel?ModelViewProjTrafoInv

        
        member x.NormalMatrix : M33d = uniform?PerModel?NormalMatrix
        member x.NormalMatrixInv : M33d = uniform?PerModel?NormalMatrixInv

        member x.CameraLocation : V3d = uniform?PerView?CameraLocation
        member x.ViewportSize : V2i = uniform?PerView?ViewportSize



    type TexCoordAttribute() = inherit SemanticAttribute("DiffuseColorCoordinates")
    type NormalAttribute() = inherit SemanticAttribute("Normals")
    type BiNormalAttribute() = inherit SemanticAttribute("DiffuseColorUTangents")
    type TangentAttribute() = inherit SemanticAttribute("DiffuseColorVTangents")
    type ColorAttribute() = inherit SemanticAttribute("Colors")
