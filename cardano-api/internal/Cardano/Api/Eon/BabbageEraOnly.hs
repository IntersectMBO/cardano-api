{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eon.BabbageEraOnly
  ( BabbageEraOnly(..)
  , babbageEraOnlyConstraints
  , babbageEraOnlyToCardanoEra

  , BabbageEraOnlyConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

data BabbageEraOnly era where
  BabbageEraOnlyBabbage  :: BabbageEraOnly BabbageEra

deriving instance Show (BabbageEraOnly era)
deriving instance Eq (BabbageEraOnly era)

instance Eon BabbageEraOnly where
  inEonForEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> yes BabbageEraOnlyBabbage
    ConwayEra   -> no

instance ToCardanoEra BabbageEraOnly where
  toCardanoEra = \case
    BabbageEraOnlyBabbage  -> BabbageEra

type BabbageEraOnlyConstraints era =
  ( IsCardanoEra era
  , Typeable era
  )

babbageEraOnlyConstraints :: ()
  => BabbageEraOnly era
  -> (BabbageEraOnlyConstraints era => a)
  -> a
babbageEraOnlyConstraints = \case
  BabbageEraOnlyBabbage  -> id

babbageEraOnlyToCardanoEra :: BabbageEraOnly era -> CardanoEra era
babbageEraOnlyToCardanoEra = \case
  BabbageEraOnlyBabbage  -> BabbageEra
