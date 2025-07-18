module Test.Golden.Cardano.Wasm.TypeScript where

import Hedgehog as H
import Hedgehog.Extras qualified as H

hprop_cardano_wasm_typescript_declarations_match_generated :: Property
hprop_cardano_wasm_typescript_declarations_match_generated =
  H.propertyOnce $ do
    result <- H.execFlex "cardano-wasm" "CARDANO_WASM" []
    H.diffVsGoldenFile result "example/cardano-api.d.ts"
