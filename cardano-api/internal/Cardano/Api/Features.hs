{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Features
  ( Feature(..)
  , supportedInEra
  , maybeSupportedInEra
  , whenMaybeSupportedInEra
  ) where

import           Cardano.Api.Eras
import           Data.Aeson (ToJSON (..))

data ProtocolParameterUTxOCostPerWord

data ProtocolParameterUTxOCostPerByte

-- | A representation of a feature that is supported in a given era.
data Feature feature where
  ProtocolParameterUTxOCostPerWord :: Feature ProtocolParameterUTxOCostPerWord
  ProtocolParameterUTxOCostPerByte :: Feature ProtocolParameterUTxOCostPerByte

deriving instance Eq (Feature f)
deriving instance Show (Feature f)

instance ToJSON (Feature f) where
  toJSON = toJSON . show

-- | Determine whether a feature is supported in a given era.
--
-- If the feature is not supported in the given era, 'no' is returned, otherwise 'yes'.
supportedInEra
  :: a
  -- ^ The value to return if the feature is not supported in the given era.
  -> a
  -- ^ The value to return if the feature is supported in the given era.
  -> Feature feature
  -- ^ The feature to check.
  -> CardanoEra era
  -- ^ The era to check.
  -> a
supportedInEra no yes = \case
  ProtocolParameterUTxOCostPerWord -> \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> yes
    BabbageEra  -> no
    ConwayEra   -> no
  ProtocolParameterUTxOCostPerByte -> \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> yes
    ConwayEra   -> yes

-- | Determine whether a feature is supported in a given era.
--
-- If the feature is not supported in the given era, 'pure Nothing' is returned, otherwise 'Just <$> fa'.
whenMaybeSupportedInEra :: ()
  => Applicative f
  => f a
  -- ^ The value to return if the feature is not supported in the given era.
  -> Feature feature
  -- ^ The feature to check.
  -> CardanoEra era
  -- ^ The era to check.
  -> f (Maybe a)
whenMaybeSupportedInEra fa = supportedInEra (pure Nothing) (Just <$> fa)

-- | Determine whether a feature is supported in a given era.
--
-- If the feature is not supported in the given era, 'Nothing' is returned, otherwise 'Just a'.
maybeSupportedInEra :: ()
  => a
  -- ^ The value to return if the feature is not supported in the given era.
  -> Feature feature
  -- ^ The feature to check.
  -> CardanoEra era
  -- ^ The era to check.
  -> Maybe a
maybeSupportedInEra a = supportedInEra Nothing (Just a)
