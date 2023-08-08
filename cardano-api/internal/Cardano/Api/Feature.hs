{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Api.Feature
  ( MaybeAvailable (..)
  , asFeaturedInEra
  , asFeaturedInShelleyBasedEra
  , isAvailable
  , maybeToMaybeAvailable
  ) where

import           Cardano.Api.Eras

import           Data.Aeson
import           Data.Kind
import           Data.Type.Equality

-- | A value only if the feature is supported in this era
data MaybeAvailable (feature :: Type -> Type) a where
  Featured
    :: Show (feature era)
    => feature era
    -- ^ The witness that the feature is supported in this era
    -> a
    -- ^ The value to use
    -> MaybeAvailable feature a

  FeatureNotAvailable :: MaybeAvailable feature a


instance (Eq era, TestEquality feature) => Eq (MaybeAvailable feature era) where
  Featured feature1 a1 == Featured feature2 a2 =
    case testEquality feature1 feature2 of
      Just Refl -> a1 == a2
      Nothing   -> False
  FeatureNotAvailable == FeatureNotAvailable = True
  _ == _ = False

deriving instance (Show a) => Show (MaybeAvailable feature a)

instance ToJSON a => ToJSON (MaybeAvailable feature a) where
  toJSON FeatureNotAvailable = Null
  toJSON (Featured _feature a) = toJSON a

isAvailable :: MaybeAvailable feature a -> Bool
isAvailable FeatureNotAvailable = False
isAvailable (Featured _ _) = True

maybeToMaybeAvailable
  :: FeatureInEra feature
  => Show (feature era)
  => CardanoEra era -> Maybe a -> MaybeAvailable feature a
maybeToMaybeAvailable _ Nothing = FeatureNotAvailable
maybeToMaybeAvailable era (Just a) = featureInEra FeatureNotAvailable (`Featured` a) era

-- | Attempt to construct a 'FeatureValue' from a value and era.
-- If the feature is not supported in the era, then 'NoFeatureValue' is returned.
asFeaturedInEra :: ()
  => FeatureInEra feature
  => Show (feature era)
  => a
  -> CardanoEra era
  -> MaybeAvailable feature a
asFeaturedInEra value = featureInEra FeatureNotAvailable (`Featured` value)

-- | Attempt to construct a 'FeatureValue' from a value and a shelley-based-era.
asFeaturedInShelleyBasedEra :: ()
  => FeatureInEra feature
  => Show (feature era)
  => a
  -> ShelleyBasedEra era
  -> MaybeAvailable feature a
asFeaturedInShelleyBasedEra value = asFeaturedInEra value . shelleyBasedToCardanoEra
