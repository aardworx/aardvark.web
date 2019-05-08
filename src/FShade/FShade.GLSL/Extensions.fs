namespace FShade

open System
open Aardvark.Base
open FShade.Imperative
open FShade.GLSL

[<AutoOpen>]
module Backends =

    let gles300 =
        Backend.Create {
            version                 = GLSLVersion(3,0,0, "es")
            precision               = ["precision highp float;"; "precision highp int;"]
            inOut                   = InOutMode.InOut
            createInternalLocations = false
            stdLayout               = false
            enabledExtensions       = Set.ofList [ ]
            createUniformBuffers    = true
            bindingMode             = BindingMode.None
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = true
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
        }
    let gles100 =
        Backend.Create {
            version                 = GLSLVersion(1,0)
            precision               = ["precision highp float;"; "precision highp int;"]
            inOut                   = InOutMode.AttributeVarying
            createInternalLocations = false
            stdLayout               = false
            enabledExtensions       = Set.ofList [ "GL_EXT_frag_depth" ]
            createUniformBuffers    = false
            bindingMode             = BindingMode.None
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = false
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
        }

    let glsl410 =
        Backend.Create {
            version                 = GLSLVersion(4,1)
            precision               = []
            inOut                   = InOutMode.InOut
            createInternalLocations = true
            stdLayout               = true
            enabledExtensions       = Set.ofList [ ]
            createUniformBuffers    = true
            bindingMode             = BindingMode.PerKind
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = true
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
        }

    let glsl430 =
        Backend.Create {
            version                 = GLSLVersion(4,3)
            precision               = []
            inOut                   = InOutMode.InOut
            createInternalLocations = true
            stdLayout               = true
            enabledExtensions       = Set.ofList [ ]
            createUniformBuffers    = true
            bindingMode             = BindingMode.PerKind
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = true
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
        }

    let glsl120 =
        Backend.Create {
            version                 = GLSLVersion(1,2)
            precision               = []
            inOut                   = InOutMode.AttributeVarying
            createInternalLocations = false
            stdLayout               = true
            enabledExtensions       = Set.empty
            createUniformBuffers    = false
            bindingMode             = BindingMode.None
            createDescriptorSets    = false
            stepDescriptorSets      = false
            createInputLocations    = false
            createPerStageUniforms  = false
            reverseMatrixLogic      = true
        }

    let glslVulkan =
        Backend.Create {
            version                 = GLSLVersion(4,5)
            precision               = []
            inOut                   = InOutMode.InOut
            createInternalLocations = true
            stdLayout               = true
            enabledExtensions       = Set.ofList [ "GL_ARB_tessellation_shader"; "GL_ARB_separate_shader_objects"; "GL_ARB_shading_language_420pack" ]
            createUniformBuffers    = true
            bindingMode             = BindingMode.Global
            createDescriptorSets    = true
            stepDescriptorSets      = false
            createInputLocations    = true
            createPerStageUniforms  = true
            reverseMatrixLogic      = true
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ModuleCompiler =

        let private containsCompute (m : Module) =
            m.entries |> List.exists (fun e -> e.decorations |> List.exists (function EntryDecoration.Stages { self = ShaderStage.Compute } -> true | _ -> false))

        let compileGLSL (cfg : Backend) (module_ : Module) =
            let cfg =
                if containsCompute module_ then
                    Backend.Create {
                        cfg.Config with
                            version                 = max (GLSLVersion(4,4)) cfg.Config.version
                    }
                else
                    cfg

            module_ 
                |> ModuleCompiler.compile cfg 
                |> Assembler.assemble cfg
                
        let compileGLES300 (module_ : Module) =
            compileGLSL gles300 module_
            
        let compileGLES100 (module_ : Module) =
            compileGLSL gles100 module_
        let compileGLSL120 (module_ : Module) =
            compileGLSL glsl120 module_

        let compileGLSL410 (module_ : Module) =
            compileGLSL glsl410 module_
            
        let compileGLSL430 (module_ : Module) =
            compileGLSL glsl430 module_

        let compileGLSLVulkan (module_ : Module) =
            compileGLSL glslVulkan module_