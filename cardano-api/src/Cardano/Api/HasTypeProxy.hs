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
import Data.Typeable
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)

class Typeable t => HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to deserialisation functions for example.
  data AsType t

  proxyToAsType :: Proxy t -> AsType t

-- | Generalised show instance for all singletons of 'AsType' displaying the type.
instance Typeable t => Show (AsType t) where
  show = show . typeOf

instance HasTypeProxy Word8 where
  data AsType Word8 = AsWord8
  proxyToAsType _ = AsWord8

instance HasTypeProxy Word16 where
  data AsType Word16 = AsWord16
  proxyToAsType _ = AsWord16

instance HasTypeProxy Word32 where
  data AsType Word32 = AsWord32
  proxyToAsType _ = AsWord32

instance HasTypeProxy Word64 where
  data AsType Word64 = AsWord64
  proxyToAsType _ = AsWord64

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
