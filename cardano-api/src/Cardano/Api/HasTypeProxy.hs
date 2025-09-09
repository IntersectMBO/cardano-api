{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Cardano.Api.HasTypeProxy
  ( HasTypeProxy (AsType, proxyToAsType)
  , asType
  , AsType (..)
  , Proxy (..)
  , FromSomeType (..)
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word8)
import Numeric.Natural (Natural)

class Typeable t => HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to deserialisation functions for example.
  data AsType t

  proxyToAsType :: Proxy t -> AsType t

instance HasTypeProxy Word8 where
  data AsType Word8 = AsWord8
  proxyToAsType _ = AsWord8

instance HasTypeProxy Word16 where
  data AsType Word16 = AsWord16
  proxyToAsType _ = AsWord16

instance HasTypeProxy Natural where
  data AsType Natural = AsNatural
  proxyToAsType _ = AsNatural

instance HasTypeProxy BS.ByteString where
  data AsType BS.ByteString = AsByteString
  proxyToAsType _ = AsByteString

instance HasTypeProxy BSL.ByteString where
  data AsType BSL.ByteString = AsByteStringLazy
  proxyToAsType _ = AsByteStringLazy

data FromSomeType (c :: Type -> Constraint) b where
  FromSomeType :: c a => AsType a -> (a -> b) -> FromSomeType c b

-- | Provide type proxy from the already existing 'HasTypeProxy' instance
asType :: HasTypeProxy t => AsType t
asType = proxyToAsType Proxy
