{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- TODO remove me when ProtocolParameters is deleted
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.ProtocolParameters
  ( tests
  )
where

import           Cardano.Api (CardanoEra (..), ProtocolParametersConversionError, inEonForEra,
                   prettyPrintJSON)
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
import           Cardano.Api.Ledger (PParams (..))
import           Cardano.Api.ProtocolParameters (LedgerProtocolParameters (..),
                   convertToLedgerProtocolParameters, fromLedgerPParams)

import           Control.Monad (void)
import           Data.Aeson (FromJSON, Object, ToJSON, eitherDecode)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable as Foldable (foldl')

import           Test.Gen.Cardano.Api.Typed (genProtocolParameters)

import           Hedgehog (Gen, MonadTest, Property, forAll, property, success, (===))
import           Hedgehog.Extras (leftFail)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- Originally, cardano-api used a different type than cardano-ledger to represent
-- protocol parameters. From conway on, we aim to unify those types and use PParams.
-- These tests aim to ensure backwards compatibility between the legacy type ProtocolParams
-- and PParams for eras before conway. Conway should use PParams directly, so we don't
-- provide any tests for it.
tests :: TestTree
tests =
  testGroup
    "ProtocolParameter tests"
    [ testGroup
        "ToJSON instances produce the same"
        [ testProperty "ShelleyEra" $ protocolParametersSerializeTheSame ShelleyEra
        , testProperty "AlonzoEra" $ protocolParametersSerializeTheSame AlonzoEra
        , testProperty "BabbageEra" $ protocolParametersSerializeTheSame BabbageEra
        ]
    , testGroup
        "ProtocolParameters ToJSON can be read by PParams FromJSON"
        [ testProperty "ShelleyEra" $ protocolParametersAreCompatible ShelleyEra
        , testProperty "AlonzoEra" $ protocolParametersAreCompatible AlonzoEra
        , testProperty "BabbageEra" $ protocolParametersAreCompatible BabbageEra
        ]
    , testGroup
        "PParams roundtrip"
        [ testProperty "ShelleyEra" $ roundtripBetweenPParamsAndLegacyProtocolParameters ShelleyEra
        , testProperty "AlonzoEra" $ roundtripBetweenPParamsAndLegacyProtocolParameters AlonzoEra
        , testProperty "BabbageEra" $ roundtripBetweenPParamsAndLegacyProtocolParameters BabbageEra
        ]
    ]

-- | Compares the JSON serialization of cardano-ledger's PParams and cardano-api's ProtocolParameters and
-- | ensures that they are the same (except for the agreed changes specified in `patchProtocolParamsJSONOrFail`)
protocolParametersSerializeTheSame
  :: forall era. ToJSON (PParams (ShelleyLedgerEra era)) => CardanoEra era -> Property
protocolParametersSerializeTheSame era =
  property $ do
    ValidatedSerializedPair
      { serializedProtocolParameters
      , serializedPParams
      } <-
      forAll $ genValidSerializedPair era
    patchedserializedProtocolParameters <-
      patchProtocolParamsJSONOrFail era serializedProtocolParameters
    serializedPParams === patchedserializedProtocolParameters

-- | Ensure that cardano-api's legacy ProtocolParameter serialization can be deserialized by cardano-ledger's PParams FromJSON instance
protocolParametersAreCompatible
  :: forall era
   . ( ToJSON (PParams (ShelleyLedgerEra era))
     , FromJSON (PParams (ShelleyLedgerEra era))
     )
  => CardanoEra era -> Property
protocolParametersAreCompatible era =
  property $ do
    ValidatedSerializedPair
      { serializedProtocolParameters
      , serializedPParams = _
      } <-
      forAll $ genValidSerializedPair era
    void
      ( leftFail
          (eitherDecode serializedProtocolParameters :: Either String (PParams (ShelleyLedgerEra era)))
      )
    success

-- | This tests that, for protocol parameter sets that can roundtrip between PParams and ProtocolParameters
-- (i.e. sets of parameters that are valid/work according to the constraints in both PParams and ProtocolParameters
-- and their conversion functions), deserializing them using PParams FromJSON instance and then serializing
-- again using PParams ToJSON instance results in the same thing.
roundtripBetweenPParamsAndLegacyProtocolParameters
  :: forall era
   . ( FromJSON (PParams (ShelleyLedgerEra era))
     , ToJSON (PParams (ShelleyLedgerEra era))
     )
  => CardanoEra era -> Property
roundtripBetweenPParamsAndLegacyProtocolParameters era =
  property $ do
    ValidatedSerializedPair
      { serializedProtocolParameters
      , serializedPParams = _
      } <-
      forAll $ genValidSerializedPair era
    patchedserializedProtocolParameters <-
      patchProtocolParamsJSONOrFail era serializedProtocolParameters
    case eitherDecode serializedProtocolParameters :: Either String (PParams (ShelleyLedgerEra era)) of
      Left err -> fail err
      Right pParams -> prettyPrintJSON pParams === LBS.toStrict patchedserializedProtocolParameters

-------------------------
-- Auxiliary generator --
-------------------------

-- | Represents a corresponding pair of serialized protocol parameters in two formats
data ValidatedSerializedPair era = ValidatedSerializedPair
  { serializedProtocolParameters :: LBS.ByteString
  -- ^ Serialized cardano-api's legacy `ProtocolParameters` as a ByteString.
  , serializedPParams :: LBS.ByteString
  -- ^ Serialized cardano-ledger's `PParams` as a ByteString.
  }
  deriving Show

-- | Produces a pair of a valid cardano-api's legacy ProtocolParameters and corresponding cardano-ledger's PParams by doing a round trip
genValidSerializedPair
  :: forall era
   . ToJSON (PParams (ShelleyLedgerEra era)) => CardanoEra era -> Gen (ValidatedSerializedPair era)
genValidSerializedPair era = do
  unrefinedProtocolParameters <- genProtocolParameters era
  let mValidatedSerializedPair =
        do
          unrefinedPParams <-
            convertToLedgerProtocolParameters sbe unrefinedProtocolParameters
              :: (Either ProtocolParametersConversionError (LedgerProtocolParameters era))
          let refinedProtocolParams = fromLedgerPParams sbe $ unLedgerProtocolParameters unrefinedPParams
          refinedPParams <- convertToLedgerProtocolParameters sbe refinedProtocolParams
          return $
            ValidatedSerializedPair
              { serializedProtocolParameters = LBS.fromStrict $ prettyPrintJSON refinedProtocolParams
              , serializedPParams = LBS.fromStrict $ prettyPrintJSON . unLedgerProtocolParameters $ refinedPParams
              }
  case mValidatedSerializedPair of
    Right result -> return result
    Left _ -> genValidSerializedPair era
 where
  sbe :: ShelleyBasedEra era
  sbe = toShelleyBased era

  toShelleyBased :: CardanoEra era -> ShelleyBasedEra era
  toShelleyBased = inEonForEra (error "Not a Shelley-based era") id

-- Legacy representation of 'ProtocolParameters' in cardano-api is not 100% compatible with
-- the 'PParams' representation in cardano-ledger. This functions modifies the JSON object
-- produced by the serialization of 'ProtocolParameters' type to match 'PParams' serialization
-- format.
patchProtocolParamsJSONOrFail
  :: (MonadTest m, MonadFail m) => CardanoEra era -> LBS.ByteString -> m LBS.ByteString
patchProtocolParamsJSONOrFail era s =
  LBS.fromStrict . prettyPrintJSON
    <$> ( patchProtocolParamRepresentation
            =<< leftFail (eitherDecode s)
        )
 where
  -- We are renaming two of the fields to match the spec. Based on discussion here:
  -- https://github.com/IntersectMBO/cardano-ledger/pull/4129#discussion_r1507373498
  patchProtocolParamRepresentation :: MonadFail m => Object -> m Object
  patchProtocolParamRepresentation o = do
    filters <- filtersForEra era
    renameKey "committeeTermLength" "committeeMaxTermLength"
      =<< renameKey
        "minCommitteeSize"
        "committeeMinSize"
        (applyFilters filters o)

  -- Legacy ProtocolParams ToJSON renders all fields from all eras in all eras,
  -- because it is the same data type for every era. But this is not backwards compatible
  -- because it means that new eras can modify the fields in old eras. For this reason, when
  -- comparing to PParams we use this function to filter fields that don't belong to
  -- particular era we are testing.
  filtersForEra :: MonadFail m => CardanoEra era -> m [String]
  filtersForEra ShelleyEra =
    return
      [ "collateralPercentage"
      , "costModels"
      , "executionUnitPrices"
      , "maxBlockExecutionUnits"
      , "maxCollateralInputs"
      , "maxTxExecutionUnits"
      , "maxValueSize"
      , "utxoCostPerByte"
      ]
  filtersForEra AlonzoEra = return ["minUTxOValue"]
  filtersForEra BabbageEra = return ["decentralization", "extraPraosEntropy", "minUTxOValue"]
  filtersForEra era' = fail $ "filtersForEra is not defined for: " <> show era'

  applyFilters :: [String] -> Object -> Object
  applyFilters filters o = Foldable.foldl' (flip Aeson.delete) o (map Aeson.fromString filters)

  -- Renames the key of an entry in a JSON object.
  -- If there already is a key with the new name in the object the function fails.
  renameKey :: MonadFail m => String -> String -> Object -> m Object
  renameKey src dest o =
    let srcKey = Aeson.fromString src
        destKey = Aeson.fromString dest
     in case Aeson.lookup srcKey o of
          Nothing -> return o
          Just v ->
            if Aeson.member destKey o
              then fail $ "renameKey failed because there is already an entry with the new name: " <> dest
              else return $ Aeson.insert destKey v $ Aeson.delete srcKey o
