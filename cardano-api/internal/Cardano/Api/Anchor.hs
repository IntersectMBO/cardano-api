module Cardano.Api.Anchor (
    AnchorUrl(..),
    AnchorDataHash(..)
  ) where

import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.SafeHash as L


-- | The URL to build the anchor to pass to DRep registration certificate
newtype AnchorUrl = AnchorUrl
  { unAnchorUrl :: L.Url
  } deriving (Eq, Show)

-- | The hash to build the anchor to pass to DRep registration certificate
newtype AnchorDataHash = AnchorDataHash
  { unAnchorDataHash :: L.SafeHash Crypto.StandardCrypto L.AnchorData
  } deriving (Eq, Show)
