{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Features
  ( Feature(..)
  , supportedInEra
  , untypedSupportedInEra
  , maybeSupportedInEra
  , whenMaybeSupportedInEra
  ) where

import           Cardano.Api.Eras
import           Cardano.Api.Value
import           Data.Aeson (ToJSON (..))

data ProtocolParameterUTxOCostPerWord

data ProtocolParameterUTxOCostPerByte

-- | A representation of a feature that is supported in a given era.
data Feature feature a where
  ProtocolParameterUTxOCostPerWord :: Feature ProtocolParameterUTxOCostPerWord Lovelace
  ProtocolParameterUTxOCostPerByte :: Feature ProtocolParameterUTxOCostPerByte Lovelace

deriving instance Eq (Feature f a)
deriving instance Show (Feature f a)

instance ToJSON (Feature f a) where
  toJSON = toJSON . show

-- | Determine whether a feature is supported in a given era.
--
-- If the feature is not supported in the given era, 'no' is returned, otherwise 'yes'.
untypedSupportedInEra
  :: a
  -- ^ The value to return if the feature is not supported in the given era.
  -> a
  -- ^ The value to return if the feature is supported in the given era.
  -> Feature feature b
  -- ^ The feature to check.
  -> CardanoEra era
  -- ^ The era to check.
  -> a
untypedSupportedInEra no yes = \case
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
-- If the feature is not supported in the given era, 'no' is returned, otherwise 'yes'.
supportedInEra
  :: a
  -- ^ The value to return if the feature is not supported in the given era.
  -> a
  -- ^ The value to return if the feature is supported in the given era.
  -> Feature feature a
  -- ^ The feature to check.
  -> CardanoEra era
  -- ^ The era to check.
  -> a
supportedInEra = untypedSupportedInEra

-- | Determine whether a feature is supported in a given era.
--
-- If the feature is not supported in the given era, 'pure Nothing' is returned, otherwise 'Just <$> fa'.
whenMaybeSupportedInEra :: ()
  => Applicative f
  => f a
  -- ^ The value to return if the feature is not supported in the given era.
  -> Feature feature a
  -- ^ The feature to check.
  -> CardanoEra era
  -- ^ The era to check.
  -> f (Maybe a)
whenMaybeSupportedInEra fa = untypedSupportedInEra (pure Nothing) (Just <$> fa)

-- | Determine whether a feature is supported in a given era.
--
-- If the feature is not supported in the given era, 'Nothing' is returned, otherwise 'Just a'.
maybeSupportedInEra :: ()
  => a
  -- ^ The value to return if the feature is not supported in the given era.
  -> Feature feature a
  -- ^ The feature to check.
  -> CardanoEra era
  -- ^ The era to check.
  -> Maybe a
maybeSupportedInEra a = untypedSupportedInEra Nothing (Just a)
