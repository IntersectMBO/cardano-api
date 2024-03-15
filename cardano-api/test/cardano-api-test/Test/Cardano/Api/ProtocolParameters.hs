{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Api.ProtocolParameters
  ( tests
  ) where

import           Cardano.Api (CardanoEra (..), ProtocolParametersConversionError, inEonForEra,
                   prettyPrintJSON)
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
import           Cardano.Api.Ledger (PParams (..))
import           Cardano.Api.ProtocolParameters (LedgerProtocolParameters (..),
                   convertToLedgerProtocolParameters, fromLedgerPParams)

import           Data.Aeson (FromJSON, Object, ToJSON, decode, eitherDecode)
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (foldl')

import           Test.Gen.Cardano.Api.Typed (genProtocolParameters)

import           Hedgehog (Gen, Property, forAll, property, success, (===))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
    testGroup "ProtocolParameter tests"
              [ testGroup "ToJSON instances produce the same"
                           [ testProperty "ShelleyEra" $ protocolParametersSerializeTheSame ShelleyEra
                           , testProperty "AlonzoEra" $ protocolParametersSerializeTheSame AlonzoEra
                           , testProperty "BabbageEra" $ protocolParametersSerializeTheSame BabbageEra
                           ]
              , testGroup "ProtocolParameters ToJSON can be read by PParams FromJSON"
                          [ testProperty "ShelleyEra" $ protocolParametersAreCompatible ShelleyEra
                          , testProperty "AlonzoEra" $ protocolParametersAreCompatible AlonzoEra
                          , testProperty "BabbageEra" $ protocolParametersAreCompatible BabbageEra
                          ]
              , testGroup "PParams roundtrip"
                          [ testProperty "ShelleyEra" $ ppParamsRoundtrip ShelleyEra
                          , testProperty "AlonzoEra" $ ppParamsRoundtrip AlonzoEra
                          , testProperty "BabbageEra" $ ppParamsRoundtrip BabbageEra
                          ]
              ]

-- | Ensure serialization of cardano-ledger's PParams is the same as that of cardano-api's legacy ProtocolParameters 
protocolParametersSerializeTheSame :: forall era. ToJSON (PParams (ShelleyLedgerEra era)) => CardanoEra era -> Property
protocolParametersSerializeTheSame era =
   property $ do ValidatedSerializedPair { serializedProtocolParams
                                         , serializedPParams
                                         } <- forAll $ genValidSerializedPair era
                 patchedSerializedProtocolParams <- patchProtocolParamsJSONOrFail era serializedProtocolParams
                 serializedPParams === patchedSerializedProtocolParams

-- | Ensure that cardano-api's legacy ProtocolParameter serialization can be deserialized by cardano-ledger's PParams FromJSON instance
protocolParametersAreCompatible :: forall era. ( ToJSON (PParams (ShelleyLedgerEra era))
                                               , FromJSON (PParams (ShelleyLedgerEra era))
                                               ) => CardanoEra era -> Property
protocolParametersAreCompatible era =
   property $ do ValidatedSerializedPair { serializedProtocolParams
                                         , serializedPParams = _
                                         } <- forAll $ genValidSerializedPair era
                 case eitherDecode serializedProtocolParams :: Either String (PParams (ShelleyLedgerEra era)) of
                   Left err -> fail err
                   Right _ -> success

-- | Ensure that deserializing using PParams FromJSON instance and then serializing using PParams ToJSON
-- | instance results in the same thing
ppParamsRoundtrip :: forall era. ( FromJSON (PParams (ShelleyLedgerEra era))
                                 , ToJSON (PParams (ShelleyLedgerEra era))
                                 ) => CardanoEra era -> Property
ppParamsRoundtrip era =
   property $ do ValidatedSerializedPair { serializedProtocolParams
                                         , serializedPParams = _
                                         } <- forAll $ genValidSerializedPair era
                 patchedSerializedProtocolParams <- patchProtocolParamsJSONOrFail era serializedProtocolParams
                 case eitherDecode serializedProtocolParams :: Either String (PParams (ShelleyLedgerEra era)) of
                   Left err -> fail err
                   Right pParams -> prettyPrintJSON pParams === LBS.toStrict patchedSerializedProtocolParams

-------------------------
-- Auxiliary generator --
-------------------------

data ValidatedSerializedPair era = ValidatedSerializedPair { serializedProtocolParams ::  LBS.ByteString
                                                           , serializedPParams ::  LBS.ByteString
                                                           }
  deriving Show


-- | Produces a pair of a valid cardano-api's legacy ProtocolParameters and corresponding cardano-ledger's PParams by doing a round trip
genValidSerializedPair :: forall era. ToJSON (PParams (ShelleyLedgerEra era)) => CardanoEra era -> Gen (ValidatedSerializedPair era)
genValidSerializedPair era = do
  unrefinedProtocolParameters <- genProtocolParameters era
  case (do unrefinedPParams <- convertToLedgerProtocolParameters sbe unrefinedProtocolParameters :: (Either ProtocolParametersConversionError (LedgerProtocolParameters era))
           let refinedProtocolParams = fromLedgerPParams sbe $ unLedgerProtocolParameters unrefinedPParams
           refinedPParams <- convertToLedgerProtocolParameters sbe refinedProtocolParams
           return $ ValidatedSerializedPair { serializedProtocolParams = LBS.fromStrict $ prettyPrintJSON refinedProtocolParams
                                            , serializedPParams = LBS.fromStrict $ prettyPrintJSON . unLedgerProtocolParameters $ refinedPParams
                                            }) of
     Right result -> return result
     Left _ -> genValidSerializedPair era
    where
      sbe :: ShelleyBasedEra era
      sbe = toShelleyBased era

      toShelleyBased :: CardanoEra era -> ShelleyBasedEra era
      toShelleyBased = inEonForEra (error "Not a Shelley-based era") id


patchProtocolParamsJSONOrFail :: MonadFail m => CardanoEra era -> LBS.ByteString -> m LBS.ByteString
patchProtocolParamsJSONOrFail era b = maybe (fail "Cannot fix JSON") return $ patchProtocolParamsJSON b
  where
    patchProtocolParamsJSON :: LBS.ByteString -> Maybe LBS.ByteString
    patchProtocolParamsJSON s = LBS.fromStrict . prettyPrintJSON <$> (patchProtocolParamRepresentation =<< decode s)
      where
        patchProtocolParamRepresentation :: Object -> Maybe Object
        patchProtocolParamRepresentation o = do filters <- filtersForEra era
                                                replace "committeeTermLength" "committeeMaxTermLength"
                                                  =<< replace "minCommitteeSize" "committeeMinSize"
                                                        (applyFilters filters o)

        filtersForEra :: CardanoEra era -> Maybe [String]
        filtersForEra ShelleyEra = Just [ "collateralPercentage", "costModels", "executionUnitPrices"
                                        , "maxBlockExecutionUnits", "maxCollateralInputs", "maxTxExecutionUnits"
                                        , "maxValueSize", "utxoCostPerByte" ]
        filtersForEra AlonzoEra  = Just [ "minUTxOValue" ]
        filtersForEra BabbageEra = Just [ "decentralization", "extraPraosEntropy", "minUTxOValue" ]
        filtersForEra _ = Nothing

        applyFilters :: [String] -> Object -> Object
        applyFilters filters o = foldl' (flip Aeson.delete) o (map Aeson.fromString filters)

        replace :: String -> String -> Object -> Maybe Object
        replace src dest o =
          let srcKey = Aeson.fromString src
              destKey = Aeson.fromString dest in
          case Aeson.lookup srcKey o of
            Nothing -> Just o
            Just v -> if Aeson.member destKey o
                      then Nothing
                      else Just $ Aeson.insert destKey v $ Aeson.delete srcKey o

