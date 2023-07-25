{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.ShelleyToBabbageEra
  ( ShelleyToBabbageEra(..)
  , shelleyToBabbageEraConstraints
  , shelleyToBabbageEraToCardanoEra
  , shelleyToBabbageEraToShelleyBasedEra
  ) where

import           Cardano.Api.Eras

import           Cardano.Crypto.Hash.Class (HashAlgorithm)
import qualified Cardano.Ledger.Api as L

data ShelleyToBabbageEra era where
  ShelleyToBabbageEraShelley :: ShelleyToBabbageEra ShelleyEra
  ShelleyToBabbageEraAllegra :: ShelleyToBabbageEra AllegraEra
  ShelleyToBabbageEraMary :: ShelleyToBabbageEra MaryEra
  ShelleyToBabbageEraAlonzo :: ShelleyToBabbageEra AlonzoEra
  ShelleyToBabbageEraBabbage :: ShelleyToBabbageEra BabbageEra

deriving instance Show (ShelleyToBabbageEra era)
deriving instance Eq (ShelleyToBabbageEra era)

instance FeatureInEra ShelleyToBabbageEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToBabbageEraShelley
    AllegraEra  -> yes ShelleyToBabbageEraAllegra
    MaryEra     -> yes ShelleyToBabbageEraMary
    AlonzoEra   -> yes ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes ShelleyToBabbageEraBabbage
    ConwayEra   -> no

type ShelleyToBabbageEraConstraints era =
  ( L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  )

shelleyToBabbageEraConstraints
  :: ShelleyToBabbageEra era
  -> (ShelleyToBabbageEraConstraints era => a)
  -> a
shelleyToBabbageEraConstraints = \case
  ShelleyToBabbageEraShelley -> id
  ShelleyToBabbageEraAllegra -> id
  ShelleyToBabbageEraMary    -> id
  ShelleyToBabbageEraAlonzo  -> id
  ShelleyToBabbageEraBabbage -> id

shelleyToBabbageEraToCardanoEra :: ShelleyToBabbageEra era -> CardanoEra era
shelleyToBabbageEraToCardanoEra = shelleyBasedToCardanoEra . shelleyToBabbageEraToShelleyBasedEra

shelleyToBabbageEraToShelleyBasedEra :: ShelleyToBabbageEra era -> ShelleyBasedEra era
shelleyToBabbageEraToShelleyBasedEra = \case
  ShelleyToBabbageEraShelley -> ShelleyBasedEraShelley
  ShelleyToBabbageEraAllegra -> ShelleyBasedEraAllegra
  ShelleyToBabbageEraMary    -> ShelleyBasedEraMary
  ShelleyToBabbageEraAlonzo  -> ShelleyBasedEraAlonzo
  ShelleyToBabbageEraBabbage -> ShelleyBasedEraBabbage
