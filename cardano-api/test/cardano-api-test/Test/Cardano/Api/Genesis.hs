{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Genesis
  ( tests
  )
where

import Cardano.Api
import Cardano.Api.Experimental.Era (Some (..))
import Cardano.Api.Ledger qualified as L

import Cardano.Binary qualified as CB
import Cardano.Ledger.Alonzo.Genesis qualified as L
import Cardano.Ledger.Binary qualified as L
import Cardano.Ledger.Plutus qualified as L

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Map.Strict qualified as M
import Data.Maybe
import GHC.Stack

import Hedgehog as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Test reading and decoding of AlonzoGenesis with cost models - era dependent test
prop_reading_plutus_v2_costmodel_cbor_roundtrip_era_sensitive
  :: forall era
   . CardanoEra era
  -- ^ An era in which we read the cost model
  -> PlutusV2CostModelFormat
  -- ^ cost model in genesis variant
  -> Property
prop_reading_plutus_v2_costmodel_cbor_roundtrip_era_sensitive era cmf = H.propertyOnce $ do
  H.noteShow_ $ "Era: " <> pshow era
  H.noteShow_ $ "Cost model type: " <> show cmf
  (allCostModels, v2costModelValues) <-
    H.leftFailM $ loadPlutusV2CostModelFromGenesis (getGenesisFile cmf)

  H.noteShow_ v2costModelValues

  -- the V2 params count is expected to be 175
  -- vide https://github.com/IntersectMBO/cardano-ledger/pull/5241/files
  -- will allways succeed
  length v2costModelValues === 175

  -- Make sure that our just read genesis is CBOR encoding roundtripping
  -- because after protocol version >= 9 the CBOR decoder is failing on errors
  aeo <- H.nothingFail $ forEraMaybeEon @AlonzoEraOnwards era
  let allCostModelsBs = encodeCborInEraCostModels aeo allCostModels
  allCostModels' <- H.leftFail $ decodeCborInEraCostModels aeo allCostModelsBs
  H.note_ "Check that read genesis is CBOR encoding roundtripping"
  allCostModels' === allCostModels

-- | Test reading and decoding of AlonzoGenesis with cost models - an era independent test
prop_reading_plutus_v2_costmodel_json
  :: PlutusV2CostModelFormat
  -> Property
prop_reading_plutus_v2_costmodel_json cmf = H.propertyOnce $ do
  H.noteShow_ $ "Cost model type: " <> show cmf
  mCostModelValues <- fmap snd <$> loadPlutusV2CostModelFromGenesis (getGenesisFile cmf)

  H.noteShow_ mCostModelValues

  costModelValues <- H.leftFail mCostModelValues
  length costModelValues === getCostModelFileParamCount cmf

prop_check_default_alonzo_genesis_roundtrips :: Property
prop_check_default_alonzo_genesis_roundtrips = H.propertyOnce $ do
  let eras =
        catMaybes
          [ Some <$> forEraMaybeEon @AlonzoEraOnwards era
          | AnyCardanoEra era <- [(AnyCardanoEra AlonzoEra) .. maxBound]
          ]

  forM_ eras $ \(Some aeo) -> do
    let defaultCostModels = L.agCostModels alonzoGenesisDefaults
        defaultCostModelsBs = encodeCborInEraCostModels aeo defaultCostModels
    H.note_ $ "Decode alonzo genesis for era " <> show aeo
    defaultCostModels' <- H.leftFail $ decodeCborInEraCostModels aeo defaultCostModelsBs
    H.note_ $ "Check that the default genesis is CBOR encoding roundtripping for era " <> show aeo
    defaultCostModels' === defaultCostModels

-- * Utilities

data PlutusV2CostModelFormat
  = Map175
  | Array175
  deriving (Eq, Show)

getGenesisFile :: PlutusV2CostModelFormat -> FilePath
getGenesisFile =
  ("./test/cardano-api-test/files/input/genesis/spec.alonzo-v2-cost-model-" <>) . \case
    Map175 -> "map-175.json"
    Array175 -> "array-175.json"

getCostModelFileParamCount :: PlutusV2CostModelFormat -> Int
getCostModelFileParamCount = \case
  Map175 -> 175
  Array175 -> 175

loadPlutusV2CostModelFromGenesis
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> m (Either String (L.CostModels, [Int64]))
loadPlutusV2CostModelFromGenesis filePath = withFrozenCallStack . runExceptT $ do
  genesis <- H.readJsonFileOk filePath
  let costModels = L.agCostModels genesis
  liftEither
    . fmap ((costModels,) . L.getCostModelParams)
    . maybe (Left "No PlutusV2 model found") Right
    . M.lookup L.PlutusV2
    $ L.costModelsValid costModels

decodeCborInEraCostModels
  :: forall era
   . AlonzoEraOnwards era
  -> LBS.ByteString
  -> Either L.DecoderError L.CostModels
decodeCborInEraCostModels aeo = CB.decodeFullDecoder "AlonzoGenesis" fromEraCbor'
 where
  fromEraCbor' :: CBOR.Decoder s L.CostModels
  fromEraCbor' = alonzoEraOnwardsConstraints aeo $ L.fromEraCBOR @(ShelleyLedgerEra era)

encodeCborInEraCostModels
  :: forall era
   . AlonzoEraOnwards era
  -> L.CostModels
  -> LBS.ByteString
encodeCborInEraCostModels aeo = CBOR.toLazyByteString . toEraCbor'
 where
  toEraCbor' :: L.CostModels -> CBOR.Encoding
  toEraCbor' = alonzoEraOnwardsConstraints aeo $ L.toEraCBOR @(ShelleyLedgerEra era)

-- * List all test cases

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Genesis"
    [ testProperty "Default Alonzo Genesis roundtrips CBOR" prop_check_default_alonzo_genesis_roundtrips
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - Babbage" $
        prop_reading_plutus_v2_costmodel_cbor_roundtrip_era_sensitive BabbageEra Map175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - Conway" $
        prop_reading_plutus_v2_costmodel_cbor_roundtrip_era_sensitive ConwayEra Map175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - era insensitive" $
        prop_reading_plutus_v2_costmodel_json Map175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - Babbage" $
        prop_reading_plutus_v2_costmodel_cbor_roundtrip_era_sensitive BabbageEra Array175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - Conway" $
        prop_reading_plutus_v2_costmodel_cbor_roundtrip_era_sensitive ConwayEra Array175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - era insensitive" $
        prop_reading_plutus_v2_costmodel_json Array175
    ]
