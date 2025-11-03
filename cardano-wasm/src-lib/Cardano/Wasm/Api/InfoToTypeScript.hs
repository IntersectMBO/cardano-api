module Cardano.Wasm.Api.InfoToTypeScript where

import Cardano.Wasm.Api.Info (tsTypeAsString)
import Cardano.Wasm.Api.Info qualified as Info
import Cardano.Wasm.Api.TypeScriptDefs qualified as TypeScript

import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map

-- | Converts the Cardano API information to a TypeScript declaration file AST.
apiInfoToTypeScriptFile :: Info.ApiInfo -> [TypeScript.TypeScriptFile]
apiInfoToTypeScriptFile apiInfo =
  ( TypeScript.TypeScriptFile
      { TypeScript.typeScriptFileName = Info.dashCaseName (Info.mainObject apiInfo) <> ".d.ts"
      , TypeScript.typeScriptFileContent =
          virtualObjectInfoToInterfaceDecs
            False
            voMap
            (Info.mainObject apiInfo)
            ++ [ TypeScript.Declaration
                   [ Info.initialiseFunctionDoc apiInfo
                   , "@returns " <> Info.initialiseFunctionReturnDoc apiInfo
                   ]
                   ( TypeScript.FunctionDec $
                       TypeScript.FunctionHeader
                         { TypeScript.functionName = "initialise"
                         , TypeScript.functionParams = []
                         , TypeScript.functionReturnType =
                             "Promise<" <> Info.virtualObjectName (Info.mainObject apiInfo) <> ">"
                         }
                   )
               , TypeScript.Declaration [] (TypeScript.ExportDec True "initialise")
               ]
      }
  )
    : virtualObjectInterfaces
 where
  virtualObjectInterfaces =
    map (virtualObjectInfoToTypeScriptFile voMap) (Info.virtualObjects apiInfo)

  voMap = Map.fromList [(Info.virtualObjectName vo, vo) | vo <- Info.virtualObjects apiInfo]

importDeclaration :: Info.VirtualObjectInfo -> TypeScript.Declaration
importDeclaration vo =
  TypeScript.Declaration
    { TypeScript.declarationComment = []
    , TypeScript.declarationContent =
        TypeScript.ImportDec (Info.virtualObjectName vo) $ Info.dashCaseName vo
    }

importDeclarations
  :: Map String Info.VirtualObjectInfo -> Info.VirtualObjectInfo -> [TypeScript.Declaration]
importDeclarations voMap (Info.VirtualObjectInfo{Info.virtualObjectMethods = methods}) =
  map
    importDeclaration
    $ nub
      [ vo
      | Info.MethodInfo{Info.methodReturnType = Info.NewObject returnType} <- methods
      , Just vo <- [Map.lookup returnType voMap]
      ]

virtualObjectInfoToTypeScriptFile
  :: Map String Info.VirtualObjectInfo -> Info.VirtualObjectInfo -> TypeScript.TypeScriptFile
virtualObjectInfoToTypeScriptFile voMap vo =
  TypeScript.TypeScriptFile
    { TypeScript.typeScriptFileName = Info.dashCaseName vo <> ".d.ts"
    , TypeScript.typeScriptFileContent = virtualObjectInfoToInterfaceDecs True voMap vo
    }

virtualObjectInfoToInterfaceDecs
  :: Bool -> Map String Info.VirtualObjectInfo -> Info.VirtualObjectInfo -> [TypeScript.Declaration]
virtualObjectInfoToInterfaceDecs isDefaultExport voMap vo =
  importDeclarations voMap vo
    ++ [ TypeScript.Declaration
           [Info.virtualObjectDoc vo]
           ( TypeScript.InterfaceDec
               (Info.virtualObjectName vo)
               ( [ TypeScript.InterfaceContent
                     [ "The type of the object, used for identification (the \""
                         <> Info.virtualObjectName vo
                         <> "\" string)."
                     ]
                     (TypeScript.InterfaceProperty "objectType" "string")
                 ]
                   <> map (methodInfoToInterfaceContent (Info.virtualObjectName vo)) (Info.virtualObjectMethods vo)
               )
           )
       ]
    ++ [ TypeScript.Declaration [] (TypeScript.ExportDec True $ Info.virtualObjectName vo) | isDefaultExport
       ]

methodInfoToInterfaceContent :: String -> Info.MethodInfo -> TypeScript.InterfaceContent
methodInfoToInterfaceContent selfTypeName method =
  TypeScript.InterfaceContent
    ( [Info.methodDoc method]
        <> map (\p -> "@param " <> Info.paramName p <> " " <> Info.paramDoc p) (Info.methodParams method)
        <> ["@returns " <> Info.methodReturnDoc method]
    )
    ( TypeScript.InterfaceMethod
        (Info.methodName method)
        (map paramInfoToFunctionParam $ Info.methodParams method)
        (methodReturnTypeToString selfTypeName $ Info.methodReturnType method)
    )

paramInfoToFunctionParam :: Info.ParamInfo -> TypeScript.FunctionParam
paramInfoToFunctionParam p =
  TypeScript.FunctionParam
    { TypeScript.paramName = Info.paramName p
    , TypeScript.paramType = tsTypeAsString $ Info.paramType p
    }

methodReturnTypeToString :: String -> Info.MethodReturnTypeInfo -> String
methodReturnTypeToString selfTypeName Info.Fluent = selfTypeName
methodReturnTypeToString _ (Info.NewObject objTypeName) = "Promise<" <> objTypeName <> ">"
methodReturnTypeToString _ (Info.OtherType typeName) = "Promise<" <> tsTypeAsString typeName <> ">"
