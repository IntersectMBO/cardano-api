module Cardano.Api.Governance.Internal.Metadata.Anchor
  ( AnchorUrl (..)
  , AnchorDataHash (..)
  )
where

import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Hashes qualified as L

-- | The URL to build the anchor to pass to DRep registration certificate
newtype AnchorUrl = AnchorUrl
  { unAnchorUrl :: L.Url
  }
  deriving (Eq, Show)

-- | The hash to build the anchor to pass to DRep registration certificate
newtype AnchorDataHash = AnchorDataHash
  { unAnchorDataHash :: L.SafeHash L.AnchorData
  }
  deriving (Eq, Show)
