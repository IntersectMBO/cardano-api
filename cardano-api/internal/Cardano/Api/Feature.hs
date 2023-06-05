{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Feature
  ( FeatureValue (..)
  , Feature(..)
  , supportedInShelleyBasedEra
  , valueOrDefault
  , asFeatureValue
  , asValueInShelleyBasedEra
  ) where

import           Cardano.Api.Eras

import           Data.Kind

class Feature (feature :: Type -> Type) where
  supportedInEra :: ()
    => a
    -> (feature era -> a)
    -> CardanoEra era
    -> a

supportedInShelleyBasedEra :: ()
  => Feature feature
  => a
  -> (feature era -> a)
  -> ShelleyBasedEra era
  -> a
supportedInShelleyBasedEra no yes = supportedInEra no yes . shelleyBasedToCardanoEra

data FeatureValue feature a where
  FeatureValue :: feature -> a -> FeatureValue feature a
  NoFeatureValue :: FeatureValue feature a

deriving instance (Eq a, Eq feature) => Eq (FeatureValue feature a)
deriving instance (Show a, Show feature) => Show (FeatureValue feature a)

valueOrDefault :: a -> FeatureValue feature a -> a
valueOrDefault defaultValue = \case
  NoFeatureValue -> defaultValue
  FeatureValue _ a -> a

asFeatureValue :: ()
  => Feature feature
  => a
  -> CardanoEra era
  -> FeatureValue (feature era) a
asFeatureValue value = supportedInEra NoFeatureValue (flip FeatureValue value)

asValueInShelleyBasedEra :: ()
  => Feature feature
  => a
  -> ShelleyBasedEra era
  -> FeatureValue (feature era) a
asValueInShelleyBasedEra value = asFeatureValue value . shelleyBasedToCardanoEra
