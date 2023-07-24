{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Feature
  ( FeatureValue (..)
  , valueOrDefault
  , asFeatureValue
  , asFeatureValueInShelleyBasedEra
  , isFeatureValue
  ) where

import           Cardano.Api.Eras

-- | A value of type @'FeatureValue' feature era a@ is either:
data FeatureValue feature era a where
  -- | A value is available for this feature in this era
  FeatureValue
    :: feature era
    -- ^ The witness that the feature is supported in this era
    -> a
    -- ^ The value to use
    -> FeatureValue feature era a

  -- | No value is available for this feature in this era
  NoFeatureValue
    :: FeatureValue feature era a

deriving instance (Eq a, Eq (feature era)) => Eq (FeatureValue feature era a)
deriving instance (Show a, Show (feature era)) => Show (FeatureValue feature era a)

-- | Determine if a value is defined.
--
-- If the value is not defined, it could be because the feature is not supported or
-- because the feature is supported but the value is not available.
isFeatureValue :: FeatureValue feature era a -> Bool
isFeatureValue = \case
  NoFeatureValue -> False
  FeatureValue _ _ -> True

-- | Get the value if it is defined, otherwise return the default value.
valueOrDefault :: a -> FeatureValue feature era a -> a
valueOrDefault defaultValue = \case
  NoFeatureValue -> defaultValue
  FeatureValue _ a -> a

-- | Attempt to construct a 'FeatureValue' from a value and era.
-- If the feature is not supported in the era, then 'NoFeatureValue' is returned.
asFeatureValue :: ()
  => FeatureInEra feature
  => a
  -> CardanoEra era
  -> FeatureValue feature era a
asFeatureValue value = featureInEra NoFeatureValue (`FeatureValue` value)

-- | Attempt to construct a 'FeatureValue' from a value and a shelley-based-era.
asFeatureValueInShelleyBasedEra :: ()
  => FeatureInEra feature
  => a
  -> ShelleyBasedEra era
  -> FeatureValue feature era a
asFeatureValueInShelleyBasedEra value = asFeatureValue value . shelleyBasedToCardanoEra
