{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Feature
  ( Featured (..)
  , asFeaturedInEra
  , asFeaturedInShelleyBasedEra
  , (.:?^)
  ) where

import           Cardano.Api.Eras

import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Types

-- | A value only if the feature is supported in this era
data Featured feature era a where
  Featured
    :: feature era
    -- ^ The witness that the feature is supported in this era
    -> a
    -- ^ The value to use
    -> Featured feature era a

deriving instance (Eq a, Eq (feature era)) => Eq (Featured feature era a)
deriving instance (Show a, Show (feature era)) => Show (Featured feature era a)

instance Functor (Featured feature era) where
  fmap f (Featured feature a) = Featured feature (f a)

instance ToJSON a => ToJSON (Featured feature era a) where
  toJSON = \case
    Featured _ a -> toJSON a

instance
  {-# OVERLAPPING #-}
  ( IsCardanoEra era
  , FromJSON a
  , FeatureInEra feature
  ) => FromJSON (Maybe (Featured feature era a)) where
  parseJSON v =
    featureInEra
      (pure Nothing)
      (\fe -> Just . Featured fe <$> parseJSON v)
      cardanoEra

-- | Attempt to construct a 'Featured' from a value and era.
-- If the feature is not supported in the era, then 'NoFeatureValue' is returned.
asFeaturedInEra :: ()
  => FeatureInEra feature
  => a
  -> CardanoEra era
  -> Maybe (Featured feature era a)
asFeaturedInEra value = featureInEra Nothing (Just . flip Featured value)

-- | Attempt to construct a 'Featured' from a value and a shelley-based-era.
asFeaturedInShelleyBasedEra :: ()
  => FeatureInEra feature
  => a
  -> ShelleyBasedEra era
  -> Maybe (Featured feature era a)
asFeaturedInShelleyBasedEra value = asFeaturedInEra value . shelleyBasedToCardanoEra

-- | Variant of '.:!' with explicit parser function.
explicitParseFieldFeatureValue :: ()
  => IsCardanoEra era
  => FeatureInEra feature
  => (Value -> Parser a)
  -> Object
  -> Key
  -> Parser (Maybe (Featured feature era a))
explicitParseFieldFeatureValue p obj key =
  case KM.lookup key obj of
    Nothing -> pure Nothing
    Just Aeson.Null -> pure Nothing
    Just v -> featureInEra (failUnsupported v) (parseSupported v) era
  where
    era = cardanoEra
    failUnsupported v =
      fail $ mconcat
        [ "The field " <> show key <> " with value " <> show v
        , " is not valid for the era " <> show era <> "."
        ]
    parseSupported v fe = Just . Featured fe <$> p v

(.:?^) :: ()
  => IsCardanoEra era
  => FromJSON a
  => FeatureInEra feature
  => Object
  -> Key
  -> Parser (Maybe (Featured feature era a))
(.:?^) = explicitParseFieldFeatureValue parseJSON
