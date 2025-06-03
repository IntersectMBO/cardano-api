{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Internal.Hash
  ( -- * Hash
    Hash
  , CastHash (..)
  , AsType (AsHash)
  , renderSafeHashAsHex

    -- * HasTypeProxy
  , HasTypeProxy (proxyToAsType)
  , asType
  , Proxy (..)
  , FromSomeType (..)
  )
where

import Cardano.Api.Internal.HasTypeProxy

import Cardano.Crypto.Hash qualified as Hash
import Cardano.Ledger.Hashes qualified as Ledger

import Data.Kind (Type)
import Data.Text qualified as Text

data family Hash keyrole :: Type

class CastHash roleA roleB where
  castHash :: Hash roleA -> Hash roleB

instance HasTypeProxy a => HasTypeProxy (Hash a) where
  data AsType (Hash a) = AsHash (AsType a)
  proxyToAsType _ = AsHash (proxyToAsType (Proxy :: Proxy a))

renderSafeHashAsHex :: Ledger.SafeHash tag -> Text.Text
renderSafeHashAsHex = Hash.hashToTextAsHex . Ledger.extractHash
