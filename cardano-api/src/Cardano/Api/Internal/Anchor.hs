module Cardano.Api.Internal.Anchor
  ( AnchorUrl (..)
  , AnchorDataHash (..)
  )
where

import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Crypto qualified as Crypto
import Cardano.Ledger.SafeHash qualified as L

-- | The URL to build the anchor to pass to DRep registration certificate
newtype AnchorUrl = AnchorUrl
  { unAnchorUrl :: L.Url
  }
  deriving (Eq, Show)

-- | The hash to build the anchor to pass to DRep registration certificate
newtype AnchorDataHash = AnchorDataHash
  { unAnchorDataHash :: L.SafeHash Crypto.StandardCrypto L.AnchorData
  }
  deriving (Eq, Show)
