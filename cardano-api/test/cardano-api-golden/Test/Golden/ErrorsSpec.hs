{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Redundant do" -}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Golden.ErrorsSpec
  ( test_Bech32DecodeError
  , test_InputDecodeError
  , test_JsonDecodeError
  , test_LeadershipError
  , test_OperationalCertIssueError
  , test_ProtocolParametersConversionError
  , test_ProtocolParametersError
  , test_RawBytesHexError
  , test_ScriptDataJsonBytesError
  , test_ScriptDataJsonError
  , test_ScriptDataJsonSchemaError
  , test_ScriptDataRangeError
  , test_ScriptExecutionError
  , test_StakePoolMetadataValidationError
  , test_TextEnvelopeCddlError
  , test_TextEnvelopeError
  , test_TransactionValidityError
  , test_TxBodyError
  , test_TxBodyErrorAutoBalance
  , test_TxMetadataJsonError
  , test_TxMetadataRangeError
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Binary as CBOR
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import qualified PlutusLedgerApi.Common as Plutus

import qualified Codec.Binary.Bech32 as Bech32
import           Control.Error.Util (hush)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Data
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Stack (HasCallStack)

import qualified Test.Hedgehog.Golden.ErrorMessage as ErrorMessage
import           Test.Tasty

seed1 :: ByteString
seed1 = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

seed2 :: ByteString
seed2 = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"

json :: Aeson.Value
json = Aeson.String "<JSON>"

text :: Text
text = "<text>" :: Text

string :: String
string = "<string>" :: String

bytestring :: ByteString
bytestring = "<bytestring>" :: ByteString

lazyBytestring :: LBS.ByteString
lazyBytestring = "<lazy-bytestring>" :: LBS.ByteString

stakePoolVerKey1 :: VerificationKey StakePoolKey
stakePoolVerKey1 = getVerificationKey $ deterministicSigningKey AsStakePoolKey (Crypto.mkSeedFromBytes seed1)

stakePoolVerKey2 :: VerificationKey StakePoolKey
stakePoolVerKey2 = getVerificationKey $ deterministicSigningKey AsStakePoolKey (Crypto.mkSeedFromBytes seed2)

paymentVerKey1 :: VerificationKey PaymentKey
paymentVerKey1 = getVerificationKey $ deterministicSigningKey AsPaymentKey (Crypto.mkSeedFromBytes seed1)

txid1 :: TxId
txid1 = fromJust $ hush $ deserialiseFromRawBytesHex AsTxId "210c0a4bb6391baf606843e67863d1474cc462374ab12c42d55f78a0b55b56e0"

txin1 :: TxIn
txin1 = TxIn txid1 (TxIx 1)

scriptData1 :: ScriptData
scriptData1 = ScriptDataNumber 1

scriptDataHash1 :: HashableScriptData
scriptDataHash1 = fromJust $ hush $ deserialiseFromCBOR AsHashableScriptData $ serialiseToCBOR scriptData1

hashScriptData1 :: Hash ScriptData
hashScriptData1 = hashScriptDataBytes scriptDataHash1 -- ScriptDataHash $ Ledger.unsafeMakeSafeHash hash1

scriptHash :: ScriptHash
scriptHash = hashScript $ SimpleScript $ RequireTimeBefore $ SlotNo 1

costModel :: CostModel
costModel = CostModel [0 .. 42]

changeaddr1 :: AddressInEra AllegraEra
changeaddr1 =
  AddressInEra
    (ShelleyAddressInEra ShelleyBasedEraAllegra)
    (makeShelleyAddress Mainnet
      (PaymentCredentialByKey (verificationKeyHash paymentVerKey1)) NoStakeAddress)

txOutValue1 :: TxOutValue AllegraEra
txOutValue1 = TxOutAdaOnly ByronToAllegraEraAllegra 1

txout1 :: TxOut ctx AllegraEra
txout1 = TxOut changeaddr1 txOutValue1 TxOutDatumNone ReferenceScriptNone

txOutInAnyEra1 :: TxOutInAnyEra
txOutInAnyEra1 = txOutInAnyEra AllegraEra txout1

poolId :: Hash StakePoolKey
poolId = fromJust $ hush $ deserialiseFromRawBytesHex (AsHash AsStakePoolKey)
      "9e734b6c2263c0917bfc550e9c949f41afa3fe000377243bd29df399"

test_Bech32DecodeError :: TestTree
test_Bech32DecodeError =
  testAllErrorMessages @Bech32DecodeError
    [ Bech32DecodingError Bech32.StringToDecodeTooLong
    , Bech32UnexpectedPrefix text $ Set.singleton text
    , Bech32DataPartToBytesError text
    , Bech32DeserialiseFromBytesError bytestring
    , Bech32WrongPrefix text text
    ]

test_InputDecodeError :: TestTree
test_InputDecodeError =
  testAllErrorMessages @InputDecodeError
    [ InputTextEnvelopeError $ TextEnvelopeAesonDecodeError string
    , InputBech32DecodeError $ Bech32WrongPrefix text text
    , InputInvalidError
    ]

test_ProtocolParametersConversionError :: TestTree
test_ProtocolParametersConversionError =
  testAllErrorMessages @ProtocolParametersConversionError
    [ PpceOutOfBounds "pparam" 0.1
    , PpceVersionInvalid 99999
    , PpceInvalidCostModel costModel (Plutus.CMInternalWriteError string)
    , PpceMissingParameter "pparam"
    ]

test_JsonDecodeError :: TestTree
test_JsonDecodeError =
  testAllErrorMessages @JsonDecodeError
    [ JsonDecodeError string
    ]

test_LeadershipError :: TestTree
test_LeadershipError =
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

test_OperationalCertIssueError :: TestTree
test_OperationalCertIssueError =
  testAllErrorMessages_ "Cardano.Api.OperationalCertificate" "OperationalCertIssueError"
      [ ("OperationalCertKeyMismatch", OperationalCertKeyMismatch stakePoolVerKey1 stakePoolVerKey2)
      ]

test_ProtocolParametersError :: TestTree
test_ProtocolParametersError =
  testAllErrorMessages_ "Cardano.Api.ProtocolParameters" "ProtocolParametersError"
      [ ("PParamsErrorMissingMinUTxoValue", PParamsErrorMissingMinUTxoValue (AnyCardanoEra ConwayEra))
      , ("PParamsErrorMissingAlonzoProtocolParameter", PParamsErrorMissingAlonzoProtocolParameter)
      ]

test_RawBytesHexError :: TestTree
test_RawBytesHexError =
  testAllErrorMessages_ "Cardano.Api.SerialiseRaw" "RawBytesHexError"
      [ ("RawBytesHexErrorBase16DecodeFail", RawBytesHexErrorBase16DecodeFail bytestring string)
      , ("RawBytesHexErrorRawBytesDecodeFail", RawBytesHexErrorRawBytesDecodeFail
          bytestring
          (typeRep (AsVerificationKey AsGenesisKey))
          (SerialiseAsRawBytesError string))
      ]

test_ScriptDataJsonBytesError :: TestTree
test_ScriptDataJsonBytesError =
  testAllErrorMessages @ScriptDataJsonBytesError
    [ ScriptDataJsonBytesErrorValue $ ScriptDataJsonSchemaError Aeson.Null ScriptDataJsonNullNotAllowed
    , ScriptDataJsonBytesErrorInvalid $ ScriptDataConstructorOutOfRange 0
    ]

test_ScriptDataJsonError :: TestTree
test_ScriptDataJsonError =
  testAllErrorMessages @ScriptDataJsonError
    [ ScriptDataJsonSchemaError (Aeson.String "<JSON>") ScriptDataJsonNullNotAllowed
    , ScriptDataRangeError (Aeson.String "<JSON>") (ScriptDataConstructorOutOfRange 1)
    ]

test_ScriptDataJsonSchemaError :: TestTree
test_ScriptDataJsonSchemaError =
  testAllErrorMessages @ScriptDataJsonSchemaError
    [ ScriptDataJsonNullNotAllowed
    , ScriptDataJsonBoolNotAllowed
    , ScriptDataJsonNumberNotInteger 0.0
    , ScriptDataJsonNotObject json
    , ScriptDataJsonBadObject (replicate 5 (text, json))
    , ScriptDataJsonBadMapPair json
    , ScriptDataJsonTypeMismatch text json
    ]

test_ScriptDataRangeError :: TestTree
test_ScriptDataRangeError =
  testAllErrorMessages @ScriptDataRangeError
    [ ScriptDataConstructorOutOfRange 1
    ]

test_ScriptExecutionError :: TestTree
test_ScriptExecutionError =
  testAllErrorMessages_ "Cardano.Api.Fees" "ScriptExecutionError"
    [ ("ScriptErrorMissingTxIn", ScriptErrorMissingTxIn txin1)
    , ("ScriptErrorTxInWithoutDatum", ScriptErrorTxInWithoutDatum txin1)
    , ("ScriptErrorWrongDatum", ScriptErrorWrongDatum hashScriptData1)
    , ("ScriptErrorEvaluationFailed", ScriptErrorEvaluationFailed Plutus.CostModelParameterMismatch (replicate 5 text))
    , ("ScriptErrorExecutionUnitsOverflow", ScriptErrorExecutionUnitsOverflow)
    , ("ScriptErrorNotPlutusWitnessedTxIn", ScriptErrorNotPlutusWitnessedTxIn (ScriptWitnessIndexTxIn 0) scriptHash)
    , ("ScriptErrorRedeemerPointsToUnknownScriptHash", ScriptErrorRedeemerPointsToUnknownScriptHash (ScriptWitnessIndexTxIn 0))
    , ("ScriptErrorMissingScript", ScriptErrorMissingScript (Ledger.RdmrPtr Ledger.Mint 0) (ResolvablePointers ShelleyBasedEraBabbage Map.empty)) -- TODO CIP-1694 make work in all eras
    , ("ScriptErrorMissingCostModel", ScriptErrorMissingCostModel Alonzo.PlutusV2)
    ]

test_StakePoolMetadataValidationError :: TestTree
test_StakePoolMetadataValidationError =
  testAllErrorMessages @StakePoolMetadataValidationError
    [ StakePoolMetadataJsonDecodeError string
    , StakePoolMetadataInvalidLengthError 0 1
    ]

test_TextEnvelopeCddlError :: TestTree
test_TextEnvelopeCddlError =
  testAllErrorMessages @TextEnvelopeCddlError
    [ TextEnvelopeCddlErrCBORDecodingError CBOR.DecoderErrorVoid
    , TextEnvelopeCddlAesonDecodeError string string
    , TextEnvelopeCddlUnknownKeyWitness
    , TextEnvelopeCddlTypeError [text] text
    , TextEnvelopeCddlErrUnknownType text
    , TextEnvelopeCddlErrByronKeyWitnessUnsupported
    ]

test_TextEnvelopeError :: TestTree
test_TextEnvelopeError =
  testAllErrorMessages @TextEnvelopeError
    [ TextEnvelopeTypeError [TextEnvelopeType string, TextEnvelopeType string] (TextEnvelopeType string)
    , TextEnvelopeDecodeError CBOR.DecoderErrorVoid
    , TextEnvelopeAesonDecodeError string
    ]

test_TransactionValidityError :: TestTree
test_TransactionValidityError =
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

test_TxBodyError :: TestTree
test_TxBodyError =
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

test_TxBodyErrorAutoBalance :: TestTree
test_TxBodyErrorAutoBalance =
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

test_TxMetadataJsonError :: TestTree
test_TxMetadataJsonError =
  testAllErrorMessages @TxMetadataJsonError
    [ TxMetadataJsonToplevelNotMap
    , TxMetadataJsonToplevelBadKey text
    , TxMetadataJsonSchemaError 0 json TxMetadataJsonNullNotAllowed
    , TxMetadataRangeError 0 json (TxMetadataBytesTooLong 0)
    ]

test_TxMetadataRangeError :: TestTree
test_TxMetadataRangeError =
  testAllErrorMessages @TxMetadataRangeError
    [ TxMetadataTextTooLong 0
    , TxMetadataBytesTooLong 0
    , TxMetadataNumberOutOfRange 0
    ]

goldenFilesPath :: FilePath
goldenFilesPath = "test/cardano-api-golden/files/golden/errors"

testAllErrorMessages :: forall a. (HasCallStack, Data a, Error a) => [a] -> TestTree
testAllErrorMessages = ErrorMessage.testAllErrorMessages goldenFilesPath

testAllErrorMessages_ :: forall a. (HasCallStack, Error a)
                      => String -- ^ module name
                      -> String -- ^ type name
                      -> [(String, a)]  -- ^ list of constructor names and values
                      -> TestTree
testAllErrorMessages_ = ErrorMessage.testAllErrorMessages_ goldenFilesPath
