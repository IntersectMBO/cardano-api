{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.Api.Feature
  ( Featured (..)
  , unFeatured
  , asFeaturedInEra
  , asFeaturedInShelleyBasedEra
  ) where

import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Core

-- | A value only if the eon includes era
data Featured eon era a where
  Featured
    :: eon era
    -- ^ The witness that the eon includes era
    -> a
    -- ^ The value to use
    -> Featured eon era a

deriving instance (Eq a, Eq (eon era)) => Eq (Featured eon era a)
deriving instance (Show a, Show (eon era)) => Show (Featured eon era a)

instance Functor (Featured eon era) where
  fmap f (Featured eon a) = Featured eon (f a)

unFeatured :: Featured eon era a -> a
unFeatured (Featured _ a) = a

-- | Attempt to construct a 'FeatureValue' from a value and era.
-- If the eon is not supported in the era, then 'NoFeatureValue' is returned.
asFeaturedInEra :: ()
  => Eon eon
  => a
  -> CardanoEra era
  -> Maybe (Featured eon era a)
asFeaturedInEra value = inEonForEra Nothing (Just . flip Featured value)

-- | Attempt to construct a 'FeatureValue' from a value and a shelley-based-era.
asFeaturedInShelleyBasedEra :: ()
  => Eon eon
  => a
  -> ShelleyBasedEra era
  -> Maybe (Featured eon era a)
asFeaturedInShelleyBasedEra value = asFeaturedInEra value . toCardanoEra
