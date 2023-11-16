{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Test.Gen.Cardano.Api.Era
  ( shelleyBasedEraTestConstraints
  , shelleyToBabbageEraTestConstraints
  , conwayEraOnwardsTestConstraints
  ) where

import           Cardano.Api hiding (txIns)
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Data.Functor.Identity as Ledger

import           Test.Cardano.Ledger.Conway.Arbitrary ()
import           Test.Cardano.Ledger.Core.Arbitrary ()

import           Test.QuickCheck (Arbitrary (..))

shelleyBasedEraTestConstraints :: ()
  => ShelleyBasedEra era
  ->  ( ( Ledger.Era (LedgerEra era)
        , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (LedgerEra era))
        , Arbitrary (Ledger.PParamsHKD Ledger.Identity (LedgerEra era))
        )
      => a
      )
  -> a
shelleyBasedEraTestConstraints = \case
  ShelleyBasedEraShelley  -> id
  ShelleyBasedEraAllegra  -> id
  ShelleyBasedEraMary     -> id
  ShelleyBasedEraAlonzo   -> id
  ShelleyBasedEraBabbage  -> id
  ShelleyBasedEraConway   -> id

shelleyToBabbageEraTestConstraints :: ()
  => ShelleyToBabbageEra era
  ->  ( ( Ledger.Era (LedgerEra era)
        , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (LedgerEra era))
        , Arbitrary (Ledger.PParamsHKD Ledger.Identity (LedgerEra era))
        )
      => a
      )
  -> a
shelleyToBabbageEraTestConstraints = \case
  ShelleyToBabbageEraShelley  -> id
  ShelleyToBabbageEraAllegra  -> id
  ShelleyToBabbageEraMary     -> id
  ShelleyToBabbageEraAlonzo   -> id
  ShelleyToBabbageEraBabbage  -> id

conwayEraOnwardsTestConstraints :: ()
  => ConwayEraOnwards era
  ->  ( ( Ledger.Era (LedgerEra era)
        , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (LedgerEra era))
        , Arbitrary (Ledger.PParamsHKD Ledger.Identity (LedgerEra era))
        )
      => a
      )
  -> a
conwayEraOnwardsTestConstraints = \case
  ConwayEraOnwardsConway -> id
