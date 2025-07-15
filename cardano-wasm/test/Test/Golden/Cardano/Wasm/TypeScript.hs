module Test.Golden.Cardano.Wasm.TypeScript where

import Hedgehog (Property)
import Hedgehog.Extras (defaultExecConfig, exec)
import Hedgehog.Extras qualified as H

hprop_cardano_wasm_typescript_declarations_match_generated :: Property
hprop_cardano_wasm_typescript_declarations_match_generated =
  H.propertyOnce $ do
    result <- exec defaultExecConfig "cardano-wasm" []
    H.diffVsGoldenFile result "example/cardano-api.d.ts"
