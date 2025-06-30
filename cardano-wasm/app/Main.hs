module Main where

import Cardano.Wasm.Internal.Api.Info (apiInfo)
import Cardano.Wasm.Internal.Api.InfoToTypeScript (apiInfoToTypeScriptFile)
import Cardano.Wasm.Internal.Api.TypeScriptDefs (printTypeScriptFile)

main :: IO ()
main = printTypeScriptFile (apiInfoToTypeScriptFile apiInfo)
