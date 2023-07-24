{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Feature.ConwayEraOnwards
  ( ConwayEraOnwards(..)
  ) where

import           Cardano.Api.Eras

data ConwayEraOnwards era where
  ConwayEraOnwardsConway :: ConwayEraOnwards ConwayEra

deriving instance Show (ConwayEraOnwards era)
deriving instance Eq (ConwayEraOnwards era)

instance FeatureInEra ConwayEraOnwards where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> yes ConwayEraOnwardsConway
