{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastError(..)
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)

import           Data.Kind (Type)

data EraCastError = forall fromEra toEra value.
  ( IsCardanoEra toEra
  , Show value
  ) =>
    EraCastError
    { originalValue :: value
    , fromEra :: CardanoEra fromEra
    , toEra :: CardanoEra toEra
    }

class EraCast (f :: Type -> Type) where
  eraCast :: IsCardanoEra toEra
          => CardanoEra toEra
          -> CardanoEra fromEra
          -> f fromEra
          -> Either EraCastError (f toEra)
