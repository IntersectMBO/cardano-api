{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.HasTypeProxy
  ( HasTypeProxy (AsType, proxyToAsType)
  , asType
  , Proxy (..)
  , FromSomeType (..)
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)

class Typeable t => HasTypeProxy t where
  -- | A family of singleton types used in this API to indicate which type to
  -- use where it would otherwise be ambiguous or merely unclear.
  --
  -- Values of this type are passed to deserialisation functions for example.
  data AsType t

  proxyToAsType :: Proxy t -> AsType t

data FromSomeType (c :: Type -> Constraint) b where
  FromSomeType :: c a => AsType a -> (a -> b) -> FromSomeType c b

-- | Provide type proxy from the already existing 'HasTypeProxy' instance
asType :: HasTypeProxy t => AsType t
asType = proxyToAsType Proxy
