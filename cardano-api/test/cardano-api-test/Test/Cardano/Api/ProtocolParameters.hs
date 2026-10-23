{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Api.ProtocolParameters
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as Ledger (MaxPledgeLeverage (..))

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger (emptyPParamsUpdate)
import Cardano.Ledger.Dijkstra.PParams qualified as Ledger

import Control.Monad (forM_)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro ((&), (.~))

import Test.Gen.Cardano.Api.ProtocolParameters
import Test.Gen.Cardano.Api.Typed (genCostModels)

import Hedgehog (Property, forAll, property, tripping, (===))
import Hedgehog qualified as H
import Hedgehog.Gen.QuickCheck qualified as Q
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Generated updates round-trip through ledger conversion and CBOR in every era.
-- The shared generator omits cost models, which are tested separately below.
prop_roundtrip_protocol_parameters :: ShelleyBasedEra era -> Property
prop_roundtrip_protocol_parameters era =
  shelleyBasedEraConstraints era $ property $ do
    update <- forAll $ genEraBasedProtocolParametersUpdate (toCardanoEra era)
    let ledgerUpdate = createEraBasedProtocolParamUpdate era update
    fromLedgerPParamsUpdate era ledgerUpdate === update
    tripping update CBOR.serialize' CBOR.decodeFull'

-- | Start from the ledger's generator so missing API fields cannot hide data loss.
prop_roundtrip_dijkstra_ledger_update :: Property
prop_roundtrip_dijkstra_ledger_update = property $ do
  ledgerUpdate <- forAll Q.arbitrary
  let update = fromLedgerPParamsUpdate ShelleyBasedEraDijkstra ledgerUpdate
  createEraBasedProtocolParamUpdate ShelleyBasedEraDijkstra update === ledgerUpdate

-- | Cost models need separate coverage because arbitrary cost models do not
-- round-trip through CBOR. Ledger conversion must still preserve them exactly.
prop_roundtrip_cost_model_updates
  :: Ledger.AlonzoEraPParams (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> Property
prop_roundtrip_cost_model_updates era =
  shelleyBasedEraConstraints era $ property $ do
    costModels <- forAll $ genStrictMaybe genCostModels
    let ledgerUpdate = Ledger.emptyPParamsUpdate & Ledger.ppuCostModelsL .~ costModels
        update = fromLedgerPParamsUpdate era ledgerUpdate
    createEraBasedProtocolParamUpdate era update === ledgerUpdate

-- | Leaving the leverage limit unchanged, removing it, and setting it remain distinct.
prop_max_pledge_leverage_update :: Property
prop_max_pledge_leverage_update = H.withTests 1 . property $ do
  leverage <- H.evalMaybe $ Ledger.boundRational 100
  forM_
    [ SNothing
    , SJust (Ledger.MaxPledgeLeverage SNothing)
    , SJust (Ledger.MaxPledgeLeverage (SJust leverage))
    ]
    $ \maxLeverage -> do
      let ledgerUpdate = Ledger.emptyPParamsUpdate & Ledger.ppuMaxPledgeLeverageL .~ maxLeverage
          update = fromLedgerPParamsUpdate ShelleyBasedEraDijkstra ledgerUpdate
      case update of
        DijkstraEraBasedProtocolParametersUpdate _ _ _ _ params ->
          idMaxPledgeLeverage params === maxLeverage
      createEraBasedProtocolParamUpdate ShelleyBasedEraDijkstra update === ledgerUpdate
      tripping update CBOR.serialize' CBOR.decodeFull'

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.ProtocolParameters"
    [ testGroup
        "roundtrip protocol parameters"
        [ testProperty (show era) (prop_roundtrip_protocol_parameters era)
        | AnyShelleyBasedEra era <- [minBound .. maxBound]
        ]
    , testGroup
        "roundtrip cost model updates"
        [ testProperty "Alonzo" (prop_roundtrip_cost_model_updates ShelleyBasedEraAlonzo)
        , testProperty "Babbage" (prop_roundtrip_cost_model_updates ShelleyBasedEraBabbage)
        , testProperty "Conway" (prop_roundtrip_cost_model_updates ShelleyBasedEraConway)
        , testProperty "Dijkstra" (prop_roundtrip_cost_model_updates ShelleyBasedEraDijkstra)
        ]
    , testProperty "roundtrip Dijkstra ledger update" prop_roundtrip_dijkstra_ledger_update
    , testProperty "maximum pledge leverage update" prop_max_pledge_leverage_update
    ]
