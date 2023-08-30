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

import           Test.Cardano.Ledger.Conway.Arbitrary ()
import           Test.Cardano.Ledger.Core.Arbitrary ()

import           Test.QuickCheck (Arbitrary (..))

shelleyBasedEraTestConstraints :: ()
  => ConwayEraOnwards era
  ->  ( ( Ledger.Era (ShelleyLedgerEra era)
        , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (ShelleyLedgerEra era))
        )
      => a
      )
  -> a
shelleyBasedEraTestConstraints = \case
  ConwayEraOnwardsConway -> id

shelleyToBabbageEraTestConstraints :: ()
  => ConwayEraOnwards era
  ->  ( ( Ledger.Era (ShelleyLedgerEra era)
        , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (ShelleyLedgerEra era))
        )
      => a
      )
  -> a
shelleyToBabbageEraTestConstraints = \case
  ConwayEraOnwardsConway -> id

conwayEraOnwardsTestConstraints :: ()
  => ConwayEraOnwards era
  ->  ( ( Ledger.Era (ShelleyLedgerEra era)
        , Arbitrary (Ledger.PParamsHKD Ledger.StrictMaybe (ShelleyLedgerEra era))
        )
      => a
      )
  -> a
conwayEraOnwardsTestConstraints = \case
  ConwayEraOnwardsConway -> id
