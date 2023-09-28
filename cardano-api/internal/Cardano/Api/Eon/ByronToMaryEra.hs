{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eon.ByronToMaryEra
  ( ByronToMaryEra(..)
  , IsByronToMaryEra(..)
  , byronToMaryEraConstraints
  , byronToMaryEraToCardanoEra

  , ByronToMaryEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

class IsByronToMaryEra era where
  byronToMaryEra :: ByronToMaryEra era

data ByronToMaryEra era where
  ByronToMaryEraByron   :: ByronToMaryEra ByronEra
  ByronToMaryEraShelley :: ByronToMaryEra ShelleyEra
  ByronToMaryEraAllegra :: ByronToMaryEra AllegraEra
  ByronToMaryEraMary    :: ByronToMaryEra MaryEra

deriving instance Show (ByronToMaryEra era)
deriving instance Eq (ByronToMaryEra era)

instance IsByronToMaryEra ByronEra where
  byronToMaryEra = ByronToMaryEraByron

instance IsByronToMaryEra ShelleyEra where
  byronToMaryEra = ByronToMaryEraShelley

instance IsByronToMaryEra AllegraEra where
  byronToMaryEra = ByronToMaryEraAllegra

instance IsByronToMaryEra MaryEra where
  byronToMaryEra = ByronToMaryEraMary

instance Eon ByronToMaryEra where
  inEonForEra no yes = \case
    ByronEra    -> yes ByronToMaryEraByron
    ShelleyEra  -> yes ByronToMaryEraShelley
    AllegraEra  -> yes ByronToMaryEraAllegra
    MaryEra     -> yes ByronToMaryEraMary
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ByronToMaryEra where
  toCardanoEra = \case
    ByronToMaryEraByron   -> ByronEra
    ByronToMaryEraShelley -> ShelleyEra
    ByronToMaryEraAllegra -> AllegraEra
    ByronToMaryEraMary    -> MaryEra

type ByronToMaryEraConstraints era =
  ( IsCardanoEra era
  , IsByronToMaryEra era
  , Typeable era
  )

byronToMaryEraConstraints :: ()
  => ByronToMaryEra era
  -> (ByronToMaryEraConstraints era => a)
  -> a
byronToMaryEraConstraints = \case
  ByronToMaryEraByron    -> id
  ByronToMaryEraShelley  -> id
  ByronToMaryEraAllegra  -> id
  ByronToMaryEraMary     -> id

byronToMaryEraToCardanoEra :: ByronToMaryEra era -> CardanoEra era
byronToMaryEraToCardanoEra = \case
  ByronToMaryEraByron    -> ByronEra
  ByronToMaryEraShelley  -> ShelleyEra
  ByronToMaryEraAllegra  -> AllegraEra
  ByronToMaryEraMary     -> MaryEra
