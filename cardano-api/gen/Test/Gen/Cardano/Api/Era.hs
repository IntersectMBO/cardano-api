{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Gen.Cardano.Api.Era
  ( shelleyBasedEraTestConstraints
  , conwayEraOnwardsTestConstraints
  , genAnyShelleyBasedEra
  , genAnyCardanoEra
  )
where

import Cardano.Api hiding (txIns)

import Cardano.Ledger.Core qualified as Ledger

import Data.Maybe.Strict

import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Test.QuickCheck hiding (Gen)

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
  ShelleyBasedEraDijkstra -> id

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
  ConwayEraOnwardsDijkstra -> id

-- | Generator for any Shelley-based era
genAnyShelleyBasedEra :: Gen AnyShelleyBasedEra
genAnyShelleyBasedEra = Gen.element [minBound .. maxBound]

-- | Generator for any Cardano era
genAnyCardanoEra :: Gen AnyCardanoEra
genAnyCardanoEra = Gen.element [minBound .. maxBound]
