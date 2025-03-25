module Cardano.Api.Wasm where

import Basement.Base16

test :: IO ()
test =
  print $ hexWord16 12
