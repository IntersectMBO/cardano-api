{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Gen.Cardano.Api.Era
  ( shelleyBasedEraTestConstraints
  , conwayEraOnwardsTestConstraints
  )
where

import Cardano.Api hiding (txIns)

import Cardano.Ledger.Core qualified as Ledger

import Data.Maybe.Strict

import Test.Gen.Cardano.Api.Orphans ()

import Test.QuickCheck

shelleyBasedEraTestConstraints
  :: ()
  => ShelleyBasedEra era
  -> ( ( Ledger.Era (ShelleyLedgerEra era)
       , Arbitrary (Ledger.PParams (ShelleyLedgerEra era))
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

conwayEraOnwardsTestConstraints
  :: ()
  => ConwayEraOnwards era
  -> ( ( Ledger.Era (ShelleyLedgerEra era)
       , Arbitrary (Ledger.PParamsHKD StrictMaybe (ShelleyLedgerEra era))
       )
       => a
     )
  -> a
conwayEraOnwardsTestConstraints = \case
  ConwayEraOnwardsConway -> id
