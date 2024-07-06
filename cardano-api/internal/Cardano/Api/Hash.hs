{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Hash
  ( Hash
  , CastHash (..)
  , AsType (AsHash)
  , renderSafeHashAsHex
  )
where

import Cardano.Api.HasTypeProxy
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.SafeHash as Ledger
import Data.Kind (Type)
import qualified Data.Text as Text

data family Hash keyrole :: Type

class CastHash roleA roleB where
  castHash :: Hash roleA -> Hash roleB

instance HasTypeProxy a => HasTypeProxy (Hash a) where
  data AsType (Hash a) = AsHash (AsType a)
  proxyToAsType _ = AsHash (proxyToAsType (Proxy :: Proxy a))

renderSafeHashAsHex :: Ledger.SafeHash c tag -> Text.Text
renderSafeHashAsHex = Hash.hashToTextAsHex . Ledger.extractHash
