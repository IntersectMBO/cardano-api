{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.ByronToAllegraEra
  ( ByronToAllegraEra(..)
  , byronToAllegraEraConstraints
  , byronToAllegraEraToCardanoEra

  , ByronToAllegraEraConstraints
  ) where

import           Cardano.Api.Eras.Core

import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Core as L

import           Data.Typeable (Typeable)

data ByronToAllegraEra era where
  ByronToAllegraEraByron   :: ByronToAllegraEra ByronEra
  ByronToAllegraEraShelley :: ByronToAllegraEra ShelleyEra
  ByronToAllegraEraAllegra :: ByronToAllegraEra AllegraEra

deriving instance Show (ByronToAllegraEra era)
deriving instance Eq (ByronToAllegraEra era)

instance Eon ByronToAllegraEra where
  inEonForEra no yes = \case
    ByronEra    -> yes ByronToAllegraEraByron
    ShelleyEra  -> yes ByronToAllegraEraShelley
    AllegraEra  -> yes ByronToAllegraEraAllegra
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ByronToAllegraEra where
  toCardanoEra = \case
    ByronToAllegraEraByron    -> ByronEra
    ByronToAllegraEraShelley  -> ShelleyEra
    ByronToAllegraEraAllegra  -> AllegraEra

type ByronToAllegraEraConstraints era =
  ( IsCardanoEra era
  , Typeable era
  , L.Value (LedgerEra era) ~ L.Coin
  )

byronToAllegraEraConstraints :: ()
  => ByronToAllegraEra era
  -> (ByronToAllegraEraConstraints era => a)
  -> a
byronToAllegraEraConstraints = \case
  ByronToAllegraEraByron    -> id
  ByronToAllegraEraShelley  -> id
  ByronToAllegraEraAllegra  -> id

byronToAllegraEraToCardanoEra :: ByronToAllegraEra era -> CardanoEra era
byronToAllegraEraToCardanoEra = \case
  ByronToAllegraEraByron    -> ByronEra
  ByronToAllegraEraShelley  -> ShelleyEra
  ByronToAllegraEraAllegra  -> AllegraEra
