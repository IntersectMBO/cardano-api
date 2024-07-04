{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Genesis
  ( tests
  ) where

import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.Genesis
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Alonzo.Genesis as L
import qualified Cardano.Ledger.Binary as L
import qualified Cardano.Ledger.Plutus as L
import qualified PlutusLedgerApi.V2 as V2

import qualified Data.ByteString.Lazy as LBS
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           GHC.Stack

import           Hedgehog as H
import qualified Hedgehog.Extras as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- TODO add comment explaining what's happening here

prop_reading_plutus_v2_era_sensitive_costmodel
  :: forall era. IsCardanoEra era
  => AlonzoEraOnwards era
  -> PlutusV2CostModelFormat
  -> Property
prop_reading_plutus_v2_era_sensitive_costmodel aeo cmf = H.propertyOnce $ do
  H.noteShow_ $ "Era: " <> pshow aeo
  H.noteShow_ $ "Cost model type: " <> show cmf
  (genesis, costModelValues) <- loadPlutusV2CostModelFromGenesis aeo (getGenesisFile cmf)

  H.noteShow_ costModelValues

  let isConwayOnwards = isJust $ maybeEon @ConwayEraOnwards @era
      last10CostModelValues = reverse . take 10 $ reverse costModelValues

  if isConwayOnwards
    then do
      length costModelValues === 185
      if getCostModelFileParamCount cmf < 185
        then last10CostModelValues === replicate 10 maxBound
        else last10CostModelValues === replicate 10 1
    else
      length costModelValues === 175

  let genesisBs = CBOR.serialize genesis
  genesis' <- H.leftFail $ decodeCborInEraAlonzoGenesis aeo genesisBs
  genesis' === genesis

decodeCborInEraAlonzoGenesis
  :: forall era. AlonzoEraOnwards era
  -> LBS.ByteString
  -> Either L.DecoderError L.AlonzoGenesis
decodeCborInEraAlonzoGenesis aeo = CBOR.decodeFullDecoder "AlonzoGenesis" fromEraCbor'
  where
    fromEraCbor' :: CBOR.Decoder s L.AlonzoGenesis
    fromEraCbor' = alonzoEraOnwardsConstraints aeo $ do
      -- error $ show $ eraProtVerLow (alonzoEraOnwardsToShelleyBasedEra aeo)
      L.fromEraCBOR @(ShelleyLedgerEra era)


prop_reading_plutus_v2_costmodel
  :: PlutusV2CostModelFormat
  -> Property
prop_reading_plutus_v2_costmodel cmf = H.propertyOnce $ do
  -- TODO
  True === True

prop_verify_plutus_v2_costmodel :: Property
prop_verify_plutus_v2_costmodel = H.propertyOnce $ do
  let lastParamName = maxBound
      last10Params = (toEnum . subtract 9 $ fromEnum lastParamName) `enumFromTo` lastParamName :: [V2.ParamName]
  H.note_ "Check that last 10 params of PlutusV2 cost models are exactly the ones we expect"
  -- TODO add comment why we need this
  last10Params ===
    [ V2.IntegerToByteString'cpu'arguments'c0
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
  deriving Show

getGenesisFile :: PlutusV2CostModelFormat -> FilePath
getGenesisFile = ("./test/cardano-api-test/files/input/genesis/spec.alonzo-v2-cost-model-" <>) . \case
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
  => AlonzoEraOnwards era
  -> FilePath
  -> m (L.AlonzoGenesis, [Int64])
loadPlutusV2CostModelFromGenesis aeo filePath = withFrozenCallStack $ do
  genesisBs <- H.lbsReadFile filePath
  genesis <- H.leftFailM . runExceptT $ decodeAlonzoGenesis (Just aeo) genesisBs
  fmap ((genesis,) . L.getCostModelParams)
    . H.nothingFail
    . M.lookup L.PlutusV2
    . L.costModelsValid
    $ L.agCostModels genesis

-- * List all test cases

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Genesis"
  [ testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - Babbage" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsBabbage Map175
  , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - Conway" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsConway Map175
  , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 175 params - era insensitive" $ prop_reading_plutus_v2_costmodel Map175
  , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 185 params - Babbage" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsBabbage Map185
  , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 185 params - Conway" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsConway Map185
  , testProperty "Read Alonzo genesis with PlutusV2 cost model map with 185 params - era insensitive" $ prop_reading_plutus_v2_costmodel Map185
  , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - Babbage" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsBabbage Array175
  , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - Conway" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsConway Array175
  , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 175 params - era insensitive" $ prop_reading_plutus_v2_costmodel Array175
  , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 185 params - Babbage" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsBabbage Array185
  , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 185 params - Conway" $ prop_reading_plutus_v2_era_sensitive_costmodel AlonzoEraOnwardsConway Array185
  , testProperty "Read Alonzo genesis with PlutusV2 cost model array with 185 params - era insensitive" $ prop_reading_plutus_v2_costmodel Array185
  , testProperty "Make sure that last 10 PlutusV2 cost model parameters are the ones we expect" prop_verify_plutus_v2_costmodel
  ]

