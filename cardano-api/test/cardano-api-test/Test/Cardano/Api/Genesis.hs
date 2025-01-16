{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Genesis
  ( tests
  )
where

import qualified Cardano.Api as Api (CostModels (..))
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.Genesis
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.Binary as CB
import qualified Cardano.Ledger.Alonzo.Genesis as L
import qualified Cardano.Ledger.Binary as L
import qualified Cardano.Ledger.Plutus as L
import qualified PlutusLedgerApi.V2 as V2

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Either
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           GHC.Stack

import           Hedgehog as H
import qualified Hedgehog.Extras as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- | Test reading and decoding of AlonzoGenesis with cost models - era dependent test
prop_reading_plutus_v2_era_sensitive_costmodel
  :: forall era
   . IsCardanoEra era
  => CardanoEra era
  -- ^ An era in which we read the cost model
  -> PlutusV2CostModelFormat
  -- ^ cost model in genesis variant
  -> Property
prop_reading_plutus_v2_era_sensitive_costmodel era cmf = H.propertyOnce $ do
  H.noteShow_ $ "Era: " <> pshow era
  H.noteShow_ $ "Cost model type: " <> show cmf
  (allCostModels, v2costModelValues) <-
    H.leftFailM $ loadPlutusV2CostModelFromGenesis (Just era) (getGenesisFile cmf)

  H.noteShow_ v2costModelValues

  let isConwayOnwards = isJust $ maybeEon @ConwayEraOnwards @era
      last10CostModelValues = reverse . take 10 $ reverse v2costModelValues
      -- values from @perturbing
      last10CostModelCorrectValues = [1292075, 24469, 74, 0, 1, 936157, 49601, 237, 0, 1]

  if isConwayOnwards
    then do
      length v2costModelValues === 185
      if getCostModelFileParamCount cmf < 185
        then last10CostModelValues === replicate 10 maxBound
        else last10CostModelValues === last10CostModelCorrectValues
    else length v2costModelValues === 175

  -- Make sure that our just read genesis is CBOR encoding roundtripping
  aeo <- H.nothingFail $ maybeEon @AlonzoEraOnwards @era
  let allCostModelsBs = encodeCborInEraCostModels aeo allCostModels
  allCostModels' <- H.leftFail $ decodeCborInEraCostModels aeo allCostModelsBs
  H.note_ "Check that read genesis is CBOR encoding roundtripping"
  allCostModels' === allCostModels

  -- Yeah, let's check the default one if it's roundtripping as well
  let defaultCostModels = L.agCostModels $ alonzoGenesisDefaults era
      defaultCostModelsBs = encodeCborInEraCostModels aeo defaultCostModels
  defaultCostModels' <- H.leftFail $ decodeCborInEraCostModels aeo defaultCostModelsBs
  H.note_ "Check that the default genesis is CBOR encoding roundtripping"
  defaultCostModels' === defaultCostModels

-- | Test reading and decoding of AlonzoGenesis with cost models - an era independent test
prop_reading_plutus_v2_costmodel
  :: PlutusV2CostModelFormat
  -> Property
prop_reading_plutus_v2_costmodel cmf = H.propertyOnce $ do
  H.noteShow_ $ "Cost model type: " <> show cmf
  mCostModelValues <- fmap snd <$> loadPlutusV2CostModelFromGenesis Nothing (getGenesisFile cmf)

  H.noteShow_ mCostModelValues

  if cmf == Map175
    then do
      -- reading a map with 175 params should fail
      H.assertWith mCostModelValues isLeft
    else do
      costModelValues <- H.leftFail mCostModelValues
      length costModelValues === getCostModelFileParamCount cmf

prop_verify_plutus_v2_costmodel :: Property
prop_verify_plutus_v2_costmodel = H.propertyOnce $ do
  let lastParamName = maxBound
      last10Params = (toEnum . subtract 9 $ fromEnum lastParamName) `enumFromTo` lastParamName :: [V2.ParamName]
  H.note_ "Check that last 10 params of PlutusV2 cost models are exactly the ones we expect"
  -- The conditional logic of trimming conway parameters in babbage relies on the fact that last 10 V2 params
  -- are those below
  last10Params
    === [ V2.IntegerToByteString'cpu'arguments'c0
        , V2.IntegerToByteString'cpu'arguments'c1
        , V2.IntegerToByteString'cpu'arguments'c2
        , V2.IntegerToByteString'memory'arguments'intercept
        , V2.IntegerToByteString'memory'arguments'slope
        , V2.ByteStringToInteger'cpu'arguments'c0
        , V2.ByteStringToInteger'cpu'arguments'c1
        , V2.ByteStringToInteger'cpu'arguments'c2
        , V2.ByteStringToInteger'memory'arguments'intercept
        , V2.ByteStringToInteger'memory'arguments'slope
        ]

-- * Utilities

data PlutusV2CostModelFormat
  = Map175
  | Map185
  | Array175
  | Array185
  deriving (Eq, Show)

getGenesisFile :: PlutusV2CostModelFormat -> FilePath
getGenesisFile =
  ("./test/cardano-api-test/files/input/genesis/spec.alonzo-v2-cost-model-" <>) . \case
    Map175 -> "map-175.json"
    Map185 -> "map-185.json"
    Array175 -> "array-175.json"
    Array185 -> "array-185.json"

getCostModelFileParamCount :: PlutusV2CostModelFormat -> Int
getCostModelFileParamCount = \case
  Map175 -> 175
  Map185 -> 185
  Array175 -> 175
  Array185 -> 185

loadPlutusV2CostModelFromGenesis
  :: HasCallStack
  => MonadIO m
  => MonadTest m
  => Maybe (CardanoEra era)
  -> FilePath
  -> m (Either String (L.CostModels, [Int64]))
loadPlutusV2CostModelFromGenesis mEra filePath = withFrozenCallStack . runExceptT $ do
  genesisBs <- H.lbsReadFile filePath
  costModels <- modifyError show $ L.agCostModels <$> decodeAlonzoGenesis mEra genesisBs
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

prop_plutus_costmodel_sizes :: Property
prop_plutus_costmodel_sizes = H.propertyOnce $ do
  -- PV1 tests
  -- Exact expected number of parameters
  testWorks "./test/cardano-api-test/files/input/genesis/pv1-array-166.json" PlutusScriptV1 166
  testWorks "./test/cardano-api-test/files/input/genesis/pv1-map-166.json" PlutusScriptV1 166
  -- TODO This file loads fine, whereas it shouldn't
  -- _testFails "./test/cardano-api-test/files/input/genesis/pv1-array-165.json"
  _testFails "./test/cardano-api-test/files/input/genesis/pv1-map-165.json"

  -- PV2 tests
  -- Babbage has 175 PV2 parameters:
  testWorks "./test/cardano-api-test/files/input/genesis/pv2-array-175.json" PlutusScriptV2 175
  -- Conway has 185 PV2 parameters:
  testWorks "./test/cardano-api-test/files/input/genesis/pv2-array-185.json" PlutusScriptV2 185
  -- TODO This file loads fine, whereas it shouldn't
  -- _testFails "./test/cardano-api-test/files/input/genesis/pv2-array-174.json"
  _testFails "./test/cardano-api-test/files/input/genesis/pv2-map-174.json"

  -- PV3 tests
  testWorks "./test/cardano-api-test/files/input/genesis/pv3-array-297.json" PlutusScriptV3 297
  -- TODO This file loads fine, whereas it shouldn't
  -- _testFails "./test/cardano-api-test/files/input/genesis/pv3-array-296.json"
  testWorks "./test/cardano-api-test/files/input/genesis/pv3-map-297.json" PlutusScriptV3 297
  _testFails "./test/cardano-api-test/files/input/genesis/pv3-map-296.json"
 where
  testWorks filepath whichPlutusVersion expectedSize = do
    genesisBs <- H.lbsReadFile filepath
    let decoded :: Either String Api.CostModels = Aeson.eitherDecode genesisBs
    case decoded of
      Left err -> do
        H.annotateShow err
        H.assert False
      Right (Api.CostModels cms) -> do
        case M.lookup (AnyPlutusScriptVersion whichPlutusVersion) cms of
          Nothing -> do
            H.note_ $ show whichPlutusVersion <> " cost model not found in " <> filepath
            H.assert False
          Just (CostModel model) -> do
            length model H.=== expectedSize
            pure ()
  _testFails filepath = do
    genesisBs <- H.lbsReadFile filepath
    let decoded :: Either String Api.CostModels = Aeson.eitherDecode genesisBs
    case decoded of
      Left _err -> do
        pure ()
      Right _ -> do
        H.note_ $ "Decoding of " <> filepath <> " succeeded, whereas it was expected to fail!"
        H.assert False

-- * List all test cases

-- Execute me with:
-- @cabal test cardano-api-test --test-options '-p "/Test.Cardano.Api.Genesis/"'@
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Genesis"
    [ testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - Babbage" $
        prop_reading_plutus_v2_era_sensitive_costmodel BabbageEra Map175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - Conway" $
        prop_reading_plutus_v2_era_sensitive_costmodel ConwayEra Map175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - era insensitive" $
        prop_reading_plutus_v2_costmodel Map175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 185 params - Babbage" $
        prop_reading_plutus_v2_era_sensitive_costmodel BabbageEra Map185
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 185 params - Conway" $
        prop_reading_plutus_v2_era_sensitive_costmodel ConwayEra Map185
    , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 185 params - era insensitive" $
        prop_reading_plutus_v2_costmodel Map185
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - Babbage" $
        prop_reading_plutus_v2_era_sensitive_costmodel BabbageEra Array175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - Conway" $
        prop_reading_plutus_v2_era_sensitive_costmodel ConwayEra Array175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - era insensitive" $
        prop_reading_plutus_v2_costmodel Array175
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 185 params - Babbage" $
        prop_reading_plutus_v2_era_sensitive_costmodel BabbageEra Array185
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 185 params - Conway" $
        prop_reading_plutus_v2_era_sensitive_costmodel ConwayEra Array185
    , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 185 params - era insensitive" $
        prop_reading_plutus_v2_costmodel Array185
    , testProperty
        "Make sure that last 10 PlutusV2 cost model parameters are the ones we expect"
        prop_verify_plutus_v2_costmodel
    , testProperty
        "Make sure that Plutus cost model sizes are validated correctly"
        prop_plutus_costmodel_sizes
    ]
