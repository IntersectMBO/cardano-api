{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Api.Eras.Mandatory
  ( Mandatory(..)
  , mandatoryInEra
  ) where

import           Cardano.Api.Eras.Complement

data Mandatory whole feature era a where
  Mandatory :: feature era -> a -> Mandatory whole feature era a
  Absent :: (Complement whole feature) era -> Mandatory whole feature era a

mandatoryInEra :: (Complement whole feature era -> b) -> (feature era -> a -> b) -> Mandatory whole feature era a -> b
mandatoryInEra no yes = \case
  Mandatory feature a -> yes feature a
  Absent c -> no c
