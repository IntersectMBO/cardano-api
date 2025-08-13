{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Gen.Cardano.Api.Era
  ( shelleyBasedEraTestConstraints
  , shelleyToBabbageEraTestConstraints
  , conwayEraOnwardsTestConstraints
  )
where

import Cardano.Api hiding (txIns)

import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger

import Data.Functor.Identity qualified as Ledger

import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()

import Test.QuickCheck (Arbitrary (..))

shelleyBasedEraTestConstraints
  :: ()
  => ShelleyBasedEra era
  -> ( ( Ledger.Era (ShelleyLedgerEra era)
       , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (ShelleyLedgerEra era))
       , Arbitrary (Ledger.PParamsHKD Ledger.Identity (ShelleyLedgerEra era))
       )
       => a
     )
  -> a
shelleyBasedEraTestConstraints = \case
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary -> id
  ShelleyBasedEraAlonzo -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway -> id
  ShelleyBasedEraDijkstra -> id

shelleyToBabbageEraTestConstraints
  :: ()
  => ShelleyToBabbageEra era
  -> ( ( Ledger.Era (ShelleyLedgerEra era)
       , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (ShelleyLedgerEra era))
       , Arbitrary (Ledger.PParamsHKD Ledger.Identity (ShelleyLedgerEra era))
       )
       => a
     )
  -> a
shelleyToBabbageEraTestConstraints = \case
  ShelleyToBabbageEraShelley -> id
  ShelleyToBabbageEraAllegra -> id
  ShelleyToBabbageEraMary -> id
  ShelleyToBabbageEraAlonzo -> id
  ShelleyToBabbageEraBabbage -> id

conwayEraOnwardsTestConstraints
  :: ()
  => ConwayEraOnwards era
  -> ( ( Ledger.Era (ShelleyLedgerEra era)
       , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (ShelleyLedgerEra era))
       , Arbitrary (Ledger.PParamsHKD Ledger.Identity (ShelleyLedgerEra era))
       )
       => a
     )
  -> a
conwayEraOnwardsTestConstraints = \case
  ConwayEraOnwardsConway -> id
  ConwayEraOnwardsDijkstra -> id
