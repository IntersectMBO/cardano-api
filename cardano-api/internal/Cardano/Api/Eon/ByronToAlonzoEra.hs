{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Eon.ByronToAlonzoEra
  ( ByronToAlonzoEra(..)
  , byronToAlonzoEraConstraints
  , byronToAlonzoEraToCardanoEra

  , ByronToAlonzoEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import           Data.Typeable (Typeable)

data ByronToAlonzoEra era where
  ByronToAlonzoEraByron   :: ByronToAlonzoEra ByronEra
  ByronToAlonzoEraShelley :: ByronToAlonzoEra ShelleyEra
  ByronToAlonzoEraAllegra :: ByronToAlonzoEra AllegraEra
  ByronToAlonzoEraMary    :: ByronToAlonzoEra MaryEra
  ByronToAlonzoEraAlonzo  :: ByronToAlonzoEra AlonzoEra

deriving instance Show (ByronToAlonzoEra era)
deriving instance Eq (ByronToAlonzoEra era)

instance Eon ByronToAlonzoEra where
  -- This is a trivial existential type - replace with a 'AnyBabbageEraOnwards' when it will be used anywhere
  type AnyEon ByronToAlonzoEra = Some ByronToAlonzoEra
  inEonForEra no yes = \case
    ByronEra    -> yes ByronToAlonzoEraByron
    ShelleyEra  -> yes ByronToAlonzoEraShelley
    AllegraEra  -> yes ByronToAlonzoEraAllegra
    MaryEra     -> yes ByronToAlonzoEraMary
    AlonzoEra   -> yes ByronToAlonzoEraAlonzo
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ByronToAlonzoEra where
  toCardanoEra = \case
    ByronToAlonzoEraByron   -> ByronEra
    ByronToAlonzoEraShelley -> ShelleyEra
    ByronToAlonzoEraAllegra -> AllegraEra
    ByronToAlonzoEraMary    -> MaryEra
    ByronToAlonzoEraAlonzo  -> AlonzoEra

type ByronToAlonzoEraConstraints era =
  ( IsCardanoEra era
  , Typeable era
  )

byronToAlonzoEraConstraints :: ()
  => ByronToAlonzoEra era
  -> (ByronToAlonzoEraConstraints era => a)
  -> a
byronToAlonzoEraConstraints = \case
  ByronToAlonzoEraByron    -> id
  ByronToAlonzoEraShelley  -> id
  ByronToAlonzoEraAllegra  -> id
  ByronToAlonzoEraMary     -> id
  ByronToAlonzoEraAlonzo   -> id

byronToAlonzoEraToCardanoEra :: ByronToAlonzoEra era -> CardanoEra era
byronToAlonzoEraToCardanoEra = \case
  ByronToAlonzoEraByron    -> ByronEra
  ByronToAlonzoEraShelley  -> ShelleyEra
  ByronToAlonzoEraAllegra  -> AllegraEra
  ByronToAlonzoEraMary     -> MaryEra
  ByronToAlonzoEraAlonzo   -> AlonzoEra
