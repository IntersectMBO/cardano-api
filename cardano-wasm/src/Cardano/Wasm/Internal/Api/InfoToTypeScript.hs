module Cardano.Wasm.Internal.Api.InfoToTypeScript where

import Cardano.Wasm.Internal.Api.Info (tsTypeAsString)
import Cardano.Wasm.Internal.Api.Info qualified as Info
import Cardano.Wasm.Internal.Api.TypeScriptDefs qualified as TypeScript

-- | Converts the Cardano API information to a TypeScript declaration file AST.
apiInfoToTypeScriptFile :: Info.ApiInfo -> TypeScript.TypeScriptFile
apiInfoToTypeScriptFile apiInfo =
  TypeScript.TypeScriptFile
    { TypeScript.typeScriptFileName = "cardano-api.d.ts"
    , TypeScript.typeScriptFileContent =
        [ TypeScript.Declaration [] (TypeScript.ExportDec True "initialise")
        , TypeScript.Declaration
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
        ]
          <> virtualObjectInterfaces
    }
 where
  virtualObjectInterfaces =
    map virtualObjectInfoToInterfaceDec (Info.virtualObjects apiInfo <> [Info.mainObject apiInfo])

virtualObjectInfoToInterfaceDec :: Info.VirtualObjectInfo -> TypeScript.Declaration
virtualObjectInfoToInterfaceDec vo =
  TypeScript.Declaration
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
