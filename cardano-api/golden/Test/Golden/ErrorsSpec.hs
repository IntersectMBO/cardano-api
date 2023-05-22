{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant do" -}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Golden.ErrorsSpec
  ( spec
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley (LeadershipError (..), OperationalCertIssueError (..),
                   ProtocolParametersError (..), ReferenceScript (..))

import           Cardano.Binary as CBOR
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import qualified PlutusLedgerApi.Common as Plutus

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Data
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Stack (withFrozenCallStack)
import           System.FilePath ((</>))

import qualified HaskellWorks.Hspec.Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.Golden as H
import           Test.Hspec

seed1 :: ByteString
seed1 = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

seed2 :: ByteString
seed2 = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"

spec :: Spec
spec = describe "Test.Golden.Errors" $ do
  let json = Aeson.String "<JSON>"
  let text = "<text>" :: Text
  let string = "<string>" :: String
  let bytestring = "<bytestring>" :: ByteString
  let lazyBytestring = "<lazy-bytestring>" :: LBS.ByteString
  let stakePoolVerKey1 = getVerificationKey $ deterministicSigningKey AsStakePoolKey (Crypto.mkSeedFromBytes seed1)
  let stakePoolVerKey2 = getVerificationKey $ deterministicSigningKey AsStakePoolKey (Crypto.mkSeedFromBytes seed2)
  let paymentVerKey1 = getVerificationKey $ deterministicSigningKey AsPaymentKey (Crypto.mkSeedFromBytes seed1)
  let Right txid1 = deserialiseFromRawBytesHex AsTxId "210c0a4bb6391baf606843e67863d1474cc462374ab12c42d55f78a0b55b56e0"
  let txin1 = TxIn txid1 (TxIx 1)
  let scriptData1 = ScriptDataNumber 1
  let Right scriptDataHash1 = deserialiseFromCBOR AsHashableScriptData $ serialiseToCBOR scriptData1
  let hashScriptData1 = hashScriptDataBytes scriptDataHash1 -- ScriptDataHash $ Ledger.unsafeMakeSafeHash hash1
  let scriptHash = hashScript $ SimpleScript $ RequireTimeBefore $ SlotNo 1
  let costModel = CostModel [0 .. 42]
  let changeaddr1 = AddressInEra
        (ShelleyAddressInEra ShelleyBasedEraAllegra)
        (makeShelleyAddress Mainnet
          (PaymentCredentialByKey (verificationKeyHash paymentVerKey1)) NoStakeAddress)
  let txOutValue1 = TxOutAdaOnly AdaOnlyInAllegraEra 1
  let txout1 = TxOut changeaddr1 txOutValue1 TxOutDatumNone ReferenceScriptNone
  let txOutInAnyEra1 = txOutInAnyEra txout1
  let (Right poolId) = deserialiseFromRawBytesHex (AsHash AsStakePoolKey)
        "9e734b6c2263c0917bfc550e9c949f41afa3fe000377243bd29df399"

  testAllErrorMessages @Bech32DecodeError
    [ Bech32DecodingError Bech32.StringToDecodeTooLong
    , Bech32UnexpectedPrefix text $ Set.singleton text
    , Bech32DataPartToBytesError text
    , Bech32DeserialiseFromBytesError bytestring
    , Bech32WrongPrefix text text
    ]

  testAllErrorMessages @InputDecodeError
    [ InputTextEnvelopeError $ TextEnvelopeAesonDecodeError string
    , InputBech32DecodeError $ Bech32WrongPrefix text text
    , InputInvalidError
    ]

  testAllErrorMessages @ProtocolParametersConversionError
    [ PpceOutOfBounds "pparam" 0.1
    , PpceVersionInvalid 99999
    , PpceInvalidCostModel costModel (Plutus.CMInternalWriteError string)
    , PpceMissingParameter "pparam"
    ]

  testAllErrorMessages @JsonDecodeError
    [ JsonDecodeError string
    ]

  testAllErrorMessages_ "Cardano.Api.LedgerState" "LeadershipError"
    [ ("LeaderErrDecodeLedgerStateFailure", LeaderErrDecodeLedgerStateFailure)
    , ("LeaderErrDecodeProtocolStateFailure", LeaderErrDecodeProtocolStateFailure
          ( lazyBytestring
          , CBOR.DecoderErrorVoid
          ))
    , ("LeaderErrDecodeProtocolEpochStateFailure", LeaderErrDecodeProtocolEpochStateFailure CBOR.DecoderErrorVoid)
    , ("LeaderErrGenesisSlot", LeaderErrGenesisSlot)
    , ("LeaderErrStakePoolHasNoStake", LeaderErrStakePoolHasNoStake poolId)
    , ("LeaderErrStakeDistribUnstable", LeaderErrStakeDistribUnstable 1 2 3 4)
    , ("LeaderErrSlotRangeCalculationFailure", LeaderErrSlotRangeCalculationFailure text)
    , ("LeaderErrCandidateNonceStillEvolving", LeaderErrCandidateNonceStillEvolving)
    ]

  testAllErrorMessages_ "Cardano.Api.OperationalCertificate" "OperationalCertIssueError"
      [ ("OperationalCertKeyMismatch", OperationalCertKeyMismatch stakePoolVerKey1 stakePoolVerKey2)
      ]

  testAllErrorMessages_ "Cardano.Api.ProtocolParameters" "ProtocolParametersError"
      [ ("PParamsErrorMissingMinUTxoValue", PParamsErrorMissingMinUTxoValue (AnyCardanoEra ConwayEra))
      , ("PParamsErrorMissingAlonzoProtocolParameter", PParamsErrorMissingAlonzoProtocolParameter)
      ]

  testAllErrorMessages_ "Cardano.Api.SerialiseRaw" "RawBytesHexError"
      [ ("RawBytesHexErrorBase16DecodeFail", RawBytesHexErrorBase16DecodeFail bytestring string)
      , ("RawBytesHexErrorRawBytesDecodeFail", RawBytesHexErrorRawBytesDecodeFail
          bytestring
          (typeRep (AsVerificationKey AsGenesisKey))
          (SerialiseAsRawBytesError string))
      ]

  testAllErrorMessages @ScriptDataJsonBytesError
    [ ScriptDataJsonBytesErrorValue $ ScriptDataJsonSchemaError Aeson.Null ScriptDataJsonNullNotAllowed
    , ScriptDataJsonBytesErrorInvalid $ ScriptDataConstructorOutOfRange 0
    ]

  testAllErrorMessages @ScriptDataJsonError
    [ ScriptDataJsonSchemaError (Aeson.String "<JSON>") ScriptDataJsonNullNotAllowed
    , ScriptDataRangeError (Aeson.String "<JSON>") (ScriptDataConstructorOutOfRange 1)
    ]

  testAllErrorMessages @ScriptDataJsonSchemaError
    [ ScriptDataJsonNullNotAllowed
    , ScriptDataJsonBoolNotAllowed
    , ScriptDataJsonNumberNotInteger 0.0
    , ScriptDataJsonNotObject json
    , ScriptDataJsonBadObject (replicate 5 (text, json))
    , ScriptDataJsonBadMapPair json
    , ScriptDataJsonTypeMismatch text json
    ]

  testAllErrorMessages @ScriptDataRangeError
    [ ScriptDataConstructorOutOfRange 1
    ]

  testAllErrorMessages_ "Cardano.Api.Fees" "ScriptExecutionError"
    [ ("ScriptErrorMissingTxIn", ScriptErrorMissingTxIn txin1)
    , ("ScriptErrorTxInWithoutDatum", ScriptErrorTxInWithoutDatum txin1)
    , ("ScriptErrorWrongDatum", ScriptErrorWrongDatum hashScriptData1)
    , ("ScriptErrorEvaluationFailed", ScriptErrorEvaluationFailed Plutus.CostModelParameterMismatch (replicate 5 text))
    , ("ScriptErrorExecutionUnitsOverflow", ScriptErrorExecutionUnitsOverflow)
    , ("ScriptErrorNotPlutusWitnessedTxIn", ScriptErrorNotPlutusWitnessedTxIn (ScriptWitnessIndexTxIn 0) scriptHash)
    , ("ScriptErrorRedeemerPointsToUnknownScriptHash", ScriptErrorRedeemerPointsToUnknownScriptHash (ScriptWitnessIndexTxIn 0))
    , ("ScriptErrorMissingScript", ScriptErrorMissingScript (Ledger.RdmrPtr Ledger.Mint 0) Map.empty)
    , ("ScriptErrorMissingCostModel", ScriptErrorMissingCostModel Alonzo.PlutusV2)
    ]

  testAllErrorMessages @StakePoolMetadataValidationError
    [ StakePoolMetadataJsonDecodeError string
    , StakePoolMetadataInvalidLengthError 0 1
    ]

  testAllErrorMessages @TextEnvelopeCddlError
    [ TextEnvelopeCddlErrCBORDecodingError CBOR.DecoderErrorVoid
    , TextEnvelopeCddlAesonDecodeError string string
    , TextEnvelopeCddlUnknownKeyWitness
    , TextEnvelopeCddlTypeError [text] text
    , TextEnvelopeCddlErrUnknownType text
    , TextEnvelopeCddlErrByronKeyWitnessUnsupported
    ]

  testAllErrorMessages @TextEnvelopeError
    [ TextEnvelopeTypeError [TextEnvelopeType string, TextEnvelopeType string] (TextEnvelopeType string)
    , TextEnvelopeDecodeError CBOR.DecoderErrorVoid
    , TextEnvelopeAesonDecodeError string
    ]

  testAllErrorMessages_ "Cardano.Api.Fees" "TransactionValidityError"
    [ ("TransactionValidityTranslationError", TransactionValidityTranslationError $ Ledger.TimeTranslationPastHorizon text)
    , ("TransactionValidityCostModelError", TransactionValidityCostModelError
        (Map.fromList [(AnyPlutusScriptVersion PlutusScriptV2, costModel)])
        string)
    -- TODO Implement this when we get access to data constructors of PastHorizon or its fields' types' constructors
    -- or we get a dummy value for such purposes.
    --
    -- , ("TransactionValidityIntervalError", TransactionValidityIntervalError $
    --     Qry.PastHorizon
    --     { Qry.pastHorizonCallStack = GHC.callStack
    --     , Qry.pastHorizonExpression = error "" -- Some $ Qry.ClosedExpr $ Qry.ELit 0
    --     , Qry.pastHorizonSummary = []
    --     })
    ]


  testAllErrorMessages_ "Cardano.Api.TxBody" "TxBodyError"
    [ ("TxBodyEmptyTxIns", TxBodyEmptyTxIns)
    , ("TxBodyEmptyTxInsCollateral", TxBodyEmptyTxInsCollateral)
    , ("TxBodyEmptyTxOuts", TxBodyEmptyTxOuts)
    , ("TxBodyOutputNegative", TxBodyOutputNegative 1 txOutInAnyEra1)
    , ("TxBodyOutputOverflow", TxBodyOutputOverflow 1 txOutInAnyEra1)
    , ("TxBodyMetadataError", TxBodyMetadataError [(1, TxMetadataBytesTooLong 2)])
    , ("TxBodyMintAdaError", TxBodyMintAdaError)
    , ("TxBodyMissingProtocolParams", TxBodyMissingProtocolParams)
    , ("TxBodyInIxOverflow", TxBodyInIxOverflow txin1)
    ]

  testAllErrorMessages_ "Cardano.Api.Fees" "TxBodyErrorAutoBalance"
    [ ("TxBodyError", TxBodyError TxBodyEmptyTxIns)
    , ("TxBodyScriptExecutionError", TxBodyScriptExecutionError [(ScriptWitnessIndexTxIn 1, ScriptErrorExecutionUnitsOverflow)])
    , ("TxBodyScriptBadScriptValidity", TxBodyScriptBadScriptValidity)
    , ("TxBodyErrorAdaBalanceNegative", TxBodyErrorAdaBalanceNegative 1)
    , ("TxBodyErrorAdaBalanceTooSmall", TxBodyErrorAdaBalanceTooSmall txOutInAnyEra1 0 1)
    , ("TxBodyErrorByronEraNotSupported", TxBodyErrorByronEraNotSupported)
    , ("TxBodyErrorMissingParamMinUTxO", TxBodyErrorMissingParamMinUTxO)
    , ("TxBodyErrorValidityInterval", TxBodyErrorValidityInterval $ TransactionValidityCostModelError Map.empty string)
    , ("TxBodyErrorMinUTxONotMet", TxBodyErrorMinUTxONotMet txOutInAnyEra1 1)
    , ("TxBodyErrorNonAdaAssetsUnbalanced", TxBodyErrorNonAdaAssetsUnbalanced (valueFromList [(AdaAssetId, Quantity 1)]))
    , ("TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap", TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap
                (ScriptWitnessIndexTxIn 1)
                (Map.fromList [(ScriptWitnessIndexTxIn 2, ExecutionUnits 1 1)]))
    ]

  testAllErrorMessages @TxMetadataJsonError
    [ TxMetadataJsonToplevelNotMap
    , TxMetadataJsonToplevelBadKey text
    , TxMetadataJsonSchemaError 0 json TxMetadataJsonNullNotAllowed
    , TxMetadataRangeError 0 json (TxMetadataBytesTooLong 0)
    ]

  testAllErrorMessages @TxMetadataRangeError
    [ TxMetadataTextTooLong 0
    , TxMetadataBytesTooLong 0
    , TxMetadataNumberOutOfRange 0
    ]

testAllErrorMessages :: forall a. (HasCallStack, Data a, Error a) => [a] -> Spec
testAllErrorMessages errs = withFrozenCallStack $ do
  let typeName = show $ typeRep (Proxy @a)
  describe typeName $
    mapM_ testErrorMessage errs

-- | Creates error messages for all values and tests them agains the golden files.
--
-- An escape hatch when adding of 'Data' instance gets impossible (like when we embed 'TypeRep' in our error data
-- types) or requires significant multi-package changes and outweights the benefits here.
testAllErrorMessages_ :: forall a. (HasCallStack, Error a)
                      => String -- ^ module name
                      -> String -- ^ type name
                      -> [(String, a)]  -- ^ list of constructor names and values
                      -> Spec
testAllErrorMessages_ moduleName typeName errs = withFrozenCallStack $ do
  describe typeName $
    mapM_ (uncurry $ testErrorMessage_ moduleName typeName) errs

testErrorMessage :: (HasCallStack, Data a, Error a) => a -> Spec
testErrorMessage err = withFrozenCallStack $ do
  let errTypeRep = typeOf err
      typeName = show errTypeRep
      moduleName = tyConModule $ typeRepTyCon errTypeRep
      constructorName = show $ toConstr err
  testErrorMessage_ moduleName typeName constructorName err

testErrorMessage_ :: (HasCallStack, Error a) => String -> String -> String -> a -> Spec
testErrorMessage_ moduleName typeName constructorName err = withFrozenCallStack $ do
  let fqtn = moduleName <> "." <> typeName
  it constructorName $ H.requireTest $ do
    H.note_ "Incorrect error message in golden file"
    displayError err `H.diffVsGoldenFile` ("test/golden/errors" </> fqtn </> constructorName <> ".txt")

