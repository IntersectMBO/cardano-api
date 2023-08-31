{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Api.EraCast
  ( EraCast(..)
  , EraCastError(..)
  ) where

import           Cardano.Api.Eras (CardanoEra (..), IsCardanoEra)

import           Data.Kind (Type)

data EraCastError = forall fromEra toEra.
  ( IsCardanoEra fromEra
  , IsCardanoEra toEra
  ) =>
    EraCastError
    { fromEra :: CardanoEra fromEra
    , toEra :: CardanoEra toEra
    }

class EraCast (f :: Type -> Type) where
  eraCast :: (IsCardanoEra fromEra, IsCardanoEra toEra)
          => CardanoEra toEra
          -> f fromEra
          -> Either EraCastError (f toEra)
