{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
  )
where

import           Cardano.Api
import           Cardano.Api.Internal.Plutus
import           Cardano.Api.Shelley

import           Cardano.Binary as CBOR
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Ledger
import qualified Cardano.Ledger.Api.Era as Ledger
import qualified Cardano.Ledger.Binary.Decoding as Binary
import           Cardano.Ledger.Binary.Version
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Plutus.CostModels as Plutus
import           Cardano.Ledger.Plutus.Evaluate
import           Cardano.Ledger.Plutus.ExUnits
import qualified Cardano.Ledger.Plutus.Language as Language
import qualified PlutusCore.Evaluation.Machine.CostModelInterface as Plutus
import qualified PlutusLedgerApi.Common as Plutus hiding (PlutusV2)

import qualified Codec.Binary.Bech32 as Bech32
import           Control.Error.Util (hush)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import           Data.Data
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Word
import           GHC.Exts (IsList (..))
import           GHC.Stack (HasCallStack)

import qualified Test.Hedgehog.Golden.ErrorMessage as ErrorMessage
import           Test.Tasty

seed1 :: ByteString
seed1 =
  "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

seed2 :: ByteString
seed2 =
  "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"

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
txid1 =
  fromJust $
    hush $
      deserialiseFromRawBytesHex AsTxId "210c0a4bb6391baf606843e67863d1474cc462374ab12c42d55f78a0b55b56e0"

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
    ( makeShelleyAddress
        Mainnet
        (PaymentCredentialByKey (verificationKeyHash paymentVerKey1))
        NoStakeAddress
    )

txOutValue1 :: TxOutValue AllegraEra
txOutValue1 = TxOutValueShelleyBased ShelleyBasedEraAllegra (L.Coin 1)

txout1 :: TxOut ctx AllegraEra
txout1 = TxOut changeaddr1 txOutValue1 TxOutDatumNone ReferenceScriptNone

txOutInAnyEra1 :: TxOutInAnyEra
txOutInAnyEra1 = txOutInAnyEra AllegraEra txout1

poolId :: Hash StakePoolKey
poolId =
  fromJust $
    hush $
      deserialiseFromRawBytesHex
        (AsHash AsStakePoolKey)
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
  testAllErrorMessages_
    "Cardano.Api.Internal.LedgerState"
    "LeadershipError"
    [ ("LeaderErrDecodeLedgerStateFailure", LeaderErrDecodeLedgerStateFailure)
    ,
      ( "LeaderErrDecodeProtocolStateFailure"
      , LeaderErrDecodeProtocolStateFailure
          ( lazyBytestring
          , CBOR.DecoderErrorVoid
          )
      )
    ,
      ( "LeaderErrDecodeProtocolEpochStateFailure"
      , LeaderErrDecodeProtocolEpochStateFailure CBOR.DecoderErrorVoid
      )
    , ("LeaderErrGenesisSlot", LeaderErrGenesisSlot)
    , ("LeaderErrStakePoolHasNoStake", LeaderErrStakePoolHasNoStake poolId)
    , ("LeaderErrStakeDistribUnstable", LeaderErrStakeDistribUnstable 1 2 3 4)
    , ("LeaderErrSlotRangeCalculationFailure", LeaderErrSlotRangeCalculationFailure text)
    , ("LeaderErrCandidateNonceStillEvolving", LeaderErrCandidateNonceStillEvolving)
    ]

test_OperationalCertIssueError :: TestTree
test_OperationalCertIssueError =
  testAllErrorMessages_
    "Cardano.Api.OperationalCertificate"
    "OperationalCertIssueError"
    [ ("OperationalCertKeyMismatch", OperationalCertKeyMismatch stakePoolVerKey1 stakePoolVerKey2)
    ]

test_ProtocolParametersError :: TestTree
test_ProtocolParametersError =
  testAllErrorMessages_
    "Cardano.Api.Internal.ProtocolParameters"
    "ProtocolParametersError"
    [ ("PParamsErrorMissingMinUTxoValue", PParamsErrorMissingMinUTxoValue (AnyCardanoEra ConwayEra))
    , ("PParamsErrorMissingAlonzoProtocolParameter", PParamsErrorMissingAlonzoProtocolParameter)
    ]

test_RawBytesHexError :: TestTree
test_RawBytesHexError =
  testAllErrorMessages_
    "Cardano.Api.SerialiseRaw"
    "RawBytesHexError"
    [ ("RawBytesHexErrorBase16DecodeFail", RawBytesHexErrorBase16DecodeFail bytestring string)
    ,
      ( "RawBytesHexErrorRawBytesDecodeFail"
      , RawBytesHexErrorRawBytesDecodeFail
          bytestring
          (typeRep (AsVerificationKey AsGenesisKey))
          (SerialiseAsRawBytesError string)
      )
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
  testAllErrorMessages_
    "Cardano.Api.Internal.Fees"
    "ScriptExecutionError"
    [ ("ScriptErrorMissingTxIn", ScriptErrorMissingTxIn txin1)
    , ("ScriptErrorTxInWithoutDatum", ScriptErrorTxInWithoutDatum txin1)
    , ("ScriptErrorWrongDatum", ScriptErrorWrongDatum hashScriptData1)
    ,
      ( "ScriptErrorEvaluationFailed"
      , ScriptErrorEvaluationFailed $
          DebugPlutusFailure
            Plutus.CostModelParameterMismatch
            examplePlutusWithContext
            (ExUnits 1 1)
            ["Example logs"]
      )
    , ("ScriptErrorExecutionUnitsOverflow", ScriptErrorExecutionUnitsOverflow)
    ,
      ( "ScriptErrorNotPlutusWitnessedTxIn"
      , ScriptErrorNotPlutusWitnessedTxIn (ScriptWitnessIndexTxIn 0) scriptHash
      )
    ,
      ( "ScriptErrorRedeemerPointsToUnknownScriptHash"
      , ScriptErrorRedeemerPointsToUnknownScriptHash (ScriptWitnessIndexTxIn 0)
      )
    ,
      ( "ScriptErrorMissingScript"
      , ScriptErrorMissingScript
          (ScriptWitnessIndexMint 0)
          (ResolvablePointers ShelleyBasedEraBabbage Map.empty) -- TODO CIP-1694 make work in all eras
      )
    , ("ScriptErrorMissingCostModel", ScriptErrorMissingCostModel Language.PlutusV2)
    , ("ScriptErrorTranslationError", ScriptErrorTranslationError testPastHorizonValue)
    ]

examplePlutusWithContext :: PlutusWithContext StandardCrypto
examplePlutusWithContext =
  PlutusWithContext
    { pwcProtocolVersion = defaultVersion
    , pwcScript = Left examplePlutusScript
    , pwcScriptHash = Language.hashPlutusScript examplePlutusScript
    , pwcArgs = examplePlutusScriptArgs
    , pwcExUnits = ExUnits 1 1
    , pwcCostModel = defaultCostModel
    }

defaultCostModel :: Plutus.CostModel
defaultCostModel =
  fromJust
    $ Plutus.costModelFromMap
      Language.PlutusV3
    $ fromList
    $ map (,0) (Plutus.costModelParamNames Language.PlutusV3)

defaultVersion :: Version
defaultVersion = fromJust $ mkVersion @Word64 9

-- Try decoding to api's PlutusScript first then convert to ledger types
examplePlutusScript :: Language.Plutus Language.PlutusV3
examplePlutusScript =
  let cborBytes = Text.encodeUtf8 hexPlutusScriptBytes
   in case deserialiseFromRawBytes (AsPlutusScript AsPlutusScriptV3) cborBytes of
        Left e -> error $ "examplePlutusScript: Failed to decode Plutus script: " <> show e
        Right (PlutusScriptSerialised script) -> Language.Plutus $ Language.PlutusBinary script

examplePlutusScriptArgs :: Language.PlutusArgs Language.PlutusV3
examplePlutusScriptArgs =
  let cborBytes = B64.decodeLenient $ Text.encodeUtf8 base64PlutusScriptArgsBytes
   in case Binary.decodeFull' defaultVersion cborBytes of
        Left _ -> error "examplePlutusScriptArgs: Failed to decode Plutus script args"
        Right args -> args

base64PlutusScriptArgsBytes :: Text
base64PlutusScriptArgsBytes =
  "2Hmf2Hmfn9h5n9h5n1ggp4nVTEZsdm+0vF4Jy816CkYJdWfk/2BiCVVip0BLnL8A/9h5n9h5n9h6n1gcxhv6HBOFJLafN4vGlQQyLzkonOVU1UnbTR4rUP/YeoD/oUChQBoATEtA2HqfWCADFwoudZe3t+PYTAU5HROaYrFX54eG2MCC8p3PTBETFP/YeoD///+An9h5n9h5n9h5n1gcxbyvlPIHUatyym5imViDpytwsNh06mjqCXkOk//YeoD/okChQBoAHoSAWBzGG/ocE4Uktp83i8aVBDIvOSic5VTVSdtNHitQoUpNaWxsYXJDb2luBdh5gNh6gP/YeZ/YeZ/YeZ9YHMW8r5TyB1GrcspuYplYg6crcLDYdOpo6gl5DpP/2HqA/6FAoUAaAA9CQNh6n1gg7hVazpxAKSB0y2r/jJzN0nPIFkj/EUnvNrzqbruKPiX/2HqA/9h5n9h5n9h5n1gcKSM+X/Qw3SW5FG9fON4SNYYd6scERBygsGr5Ev/YeoD/oUChQBsAAA2kdfg7QNh5gNh6gP//AKFYHMYb+hwThSS2nzeLxpUEMi85KJzlVNVJ200eK1ChSk1pbGxhckNvaW4Fn9h5n9h6n1gcxhv6HBOFJLafN4vGlQQyLzkonOVU1UnbTR4rUP/YeZ8aAAYagP///6DYeZ/YeZ/YeYDYeoD/2Hmf2HuA2HqA//+Ao9h6n9h5n1ggp4nVTEZsdm+0vF4Jy816CkYJdWfk/2BiCVVip0BLnL8A//8A2HmfWBzGG/ocE4Uktp83i8aVBDIvOSic5VTVSdtNHitQ/wDYfJ8A2Hmf2HqfWBzGG/ocE4Uktp83i8aVBDIvOSic5VTVSdtNHitQ/9h5nxoABhqA////AKJYIAMXCi51l7e349hMBTkdE5pisVfnh4bYwILync9MERMUAFgg7hVazpxAKSB0y2r/jJzN0nPIFkj/EUnvNrzqbruKPiUBWCDYftkmm+fgwSsSsBQx2QwFWHhnvtoPcpBPuBHJ7A+Z0KCA2HqA2HqA/wDYfJ8A2Hmf2HqfWBzGG/ocE4Uktp83i8aVBDIvOSic5VTVSdtNHitQ/9h5nxoABhqA/////w=="

hexPlutusScriptBytes :: Text
hexPlutusScriptBytes =
  "590e73590e7001000032323322332233223232323232323232323232323225335533535353232325335333573466e1d200000201301213232323232333222123330010040030023232325335333573466e1d200000201b01a1323232323232323232323232323232323333333333332333233233222222222222222212333333333333333300101101000f00e00d00c00b00a00900800700600500400300230013574202860026ae8404cc0948c8c8c94cd4ccd5cd19b87480000080c40c04cc8848cc00400c008c074d5d080098029aba135744002260589201035054310035573c0046aae74004dd5000998128009aba101123232325335333573466e1d200000203002f13232333322221233330010050040030023232325335333573466e1d2000002035034133221233001003002302e357420026605e4646464a66a666ae68cdc3a4000004072070264244600400660646ae8400454cd4ccd5cd19b87480080080e40e04c8ccc888488ccc00401401000cdd69aba1002375a6ae84004dd69aba1357440026ae880044c0d12401035054310035573c0046aae74004dd50009aba135744002260609201035054310035573c0046aae74004dd51aba1003300735742004646464a66a666ae68cdc3a400000406a068224440062a66a666ae68cdc3a400400406a068264244460020086eb8d5d08008a99a999ab9a3370e900200101a81a099091118010021aba1001130304901035054310035573c0046aae74004dd51aba10013302c75c6ae84d5d10009aba200135744002260569201035054310035573c0046aae74004dd50009bad3574201e60026ae84038c008c009d69981180a9aba100c33302702475a6ae8402cc8c8c94cd4ccd5cd19b87480000080b80b44cc8848cc00400c008c8c8c94cd4ccd5cd19b87480000080c40c04cc8848cc00400c008cc09dd69aba10013026357426ae880044c0b1241035054310035573c0046aae74004dd51aba10013232325335333573466e1d20000020310301332212330010030023302775a6ae84004c098d5d09aba20011302c491035054310035573c0046aae74004dd51aba13574400226052921035054310035573c0046aae74004dd51aba100a3302375c6ae84024ccc09c8c8c8c94cd4ccd5cd19b87480000080bc0b84c84888888c01401cdd71aba100115335333573466e1d200200202f02e13212222223002007301b357420022a66a666ae68cdc3a400800405e05c2642444444600600e60506ae8400454cd4ccd5cd19b87480180080bc0b84cc884888888cc01802001cdd69aba10013019357426ae8800454cd4ccd5cd19b87480200080bc0b84c84888888c00401cc068d5d08008a99a999ab9a3370e9005001017817099910911111198020040039bad3574200260306ae84d5d1000898152481035054310035573c0046aae74004dd500080f9aba10083300201f3574200e6eb8d5d080319981380b198138111191919299a999ab9a3370e9000001017817089110010a99a999ab9a3370e9001001017817089110008a99a999ab9a3370e900200101781708911001898152481035054310035573c0046aae74004dd50009aba1005330230143574200860026ae8400cc004d5d09aba2003302475a604aeb8d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba200113016491035054310035573c0046aae74004dd51aba10063574200a646464a66a666ae68cdc3a40000040360342642444444600a00e6eb8d5d08008a99a999ab9a3370e900100100d80d0999109111111980100400398039aba10013301500f357426ae8800454cd4ccd5cd19b874801000806c0684c84888888c00c01cc050d5d08008a99a999ab9a3370e900300100d80d099910911111198030040039bad35742002600a6ae84d5d10008a99a999ab9a3370e900400100d80d0990911111180080398031aba100115335333573466e1d200a00201b01a13322122222233004008007375a6ae84004c010d5d09aba2001130164901035054310035573c0046aae74004dd51aba13574400a4646464a66a666ae68cdc3a4000004036034264666444246660020080060046eb4d5d0801180a9aba10013232325335333573466e1d200000201f01e1323332221222222233300300a0090083301a017357420046ae84004cc069d71aba1357440026ae8800454cd4ccd5cd19b874800800807c0784cc8848888888cc01c024020cc064058d5d0800991919299a999ab9a3370e90000010110108999109198008018011bad357420026eb4d5d09aba20011301d491035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a400800403e03c266442444444466004012010666036030eb4d5d08009980cbae357426ae8800454cd4ccd5cd19b874801800807c0784c848888888c010020cc064058d5d08008a99a999ab9a3370e900400100f80f09919199991110911111119998008058050048041980d80c1aba10033301901a3574200466603a034eb4d5d08009a991919299a999ab9a3370e90000010120118998149bad357420026eb4d5d09aba20011301f4901035054310035573c0046aae74004dd51aba135744002446602a0040026ae88004d5d10008a99a999ab9a3370e900500100f80f0999109111111198028048041980c80b1aba10013232325335333573466e1d200000202202113301c75c6ae840044c075241035054310035573c0046aae74004dd51aba1357440022a66a666ae68cdc3a401800403e03c22444444400c26034921035054310035573c0046aae74004dd51aba1357440026ae880044c059241035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100d00c899910911111111111980280680618099aba10013301475a6ae84d5d10008a99a999ab9a3370e900100100d00c899910911111111111980100680618099aba10013301475a6ae84d5d10008a9919a999ab9a3370e900200180d80d0999109111111111119805006806180a1aba10023001357426ae8800854cd4ccd5cd19b874801800c06c0684c8ccc888488888888888ccc018038034030c054d5d080198011aba1001375a6ae84d5d10009aba200215335333573466e1d200800301b01a133221222222222223300700d00c3014357420046eb4d5d09aba200215335333573466e1d200a00301b01a132122222222222300100c3014357420042a66a666ae68cdc3a4018006036034266442444444444446600601a01860286ae84008dd69aba1357440042a66a666ae68cdc3a401c006036034266442444444444446601201a0186eb8d5d08011bae357426ae8800854cd4ccd5cd19b874804000c06c0684cc88488888888888cc020034030dd71aba1002375a6ae84d5d10010a99a999ab9a3370e900900180d80d0999109111111111119805806806180a1aba10023014357426ae8800854cd4ccd5cd19b874805000c06c0684c8488888888888c010030c050d5d08010980b2481035054310023232325335333573466e1d200000201e01d13212223003004375c6ae8400454c8cd4ccd5cd19b874800800c07c0784c84888c004010c004d5d08010a99a999ab9a3370e900200180f80f099910911198010028021bae3574200460026ae84d5d10010980d2481035054310023232325335333573466e1d200000202202113212223003004301b357420022a66a666ae68cdc3a4004004044042224440042a66a666ae68cdc3a4008004044042224440022603a921035054310035573c0046aae74004dd50009aab9e00235573a0026ea8004d55cf0011aab9d00137540024646464a66a666ae68cdc3a40000040320302642444600600860246ae8400454cd4ccd5cd19b87480080080640604c84888c008010c048d5d08008a99a999ab9a3370e900200100c80c099091118008021bae3574200226028921035054310035573c0046aae74004dd50009191919299a999ab9a3370e900000100c00b8999109198008018011bae357420026eb4d5d09aba200113013491035054310035573c0046aae74004dd50009aba20011300e491035054310035573c0046aae74004dd50009110019111111111111111180f0031080888078a4c26016921035054350030142225335333573466e1d20000010110101300c491035054330015335333573466e20005200001101013300333702900000119b81480000044c8cc8848cc00400c008cdc200180099b840020013300400200130132225335333573466e1d200000101000f10021330030013370c00400240024646464a66a666ae68cdc3a400000401e01c201c2a66a666ae68cdc3a400400401e01c201e260149201035054310035573c0046aae74004dd500091191919299a999ab9a3370e9000001007807089110010a99a999ab9a3370e90010010078070990911180180218029aba100115335333573466e1d200400200f00e112220011300a4901035054310035573c0046aae74004dd50009191919299a999ab9a3370e90000010068060999109198008018011bae357420026eb4d5d09aba200113008491035054310035573c0046aae74004dd5000919118011bac001300f2233335573e002401c466a01a60086ae84008c00cd5d10010041191919299a999ab9a3370e900000100580509909118010019bae357420022a66a666ae68cdc3a400400401601426424460020066eb8d5d0800898032481035054310035573c0046aae74004dd500091191919299a999ab9a3370e90010010058050a8070a99a999ab9a3370e90000010058050980798029aba1001130064901035054310035573c0046aae74004dd5000919319ab9c00100322322300237560026018446666aae7c004802c8c8cd402ccc03cc018d55ce80098029aab9e0013004357440066ae8400801448004c020894cd40045401c884d4008894cd4ccd5cd19b8f4881210104312775add93ed57c301fab7501f74beb3dbc3a70a659ef36bcea6ebb8a3e25000020080071300c001130060031220021220011220021221223300100400321223002003112200122123300100300223230010012300223300200200101"

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

testPastHorizonValue :: Ledger.AlonzoContextError (Ledger.AlonzoEra StandardCrypto)
testPastHorizonValue = Ledger.TimeTranslationPastHorizon text

test_TransactionValidityError :: TestTree
test_TransactionValidityError =
  testAllErrorMessages_
    "Cardano.Api.Internal.Fees"
    "TransactionValidityError"
    [
      ( "TransactionValidityCostModelError"
      , TransactionValidityCostModelError
          (fromList [(AnyPlutusScriptVersion PlutusScriptV2, costModel)])
          string
      )
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
  testAllErrorMessages_
    "Cardano.Api.Tx.Body"
    "TxBodyError"
    [ ("TxBodyEmptyTxIns", TxBodyEmptyTxIns)
    , ("TxBodyEmptyTxInsCollateral", TxBodyEmptyTxInsCollateral)
    , ("TxBodyEmptyTxOuts", TxBodyEmptyTxOuts)
    , ("TxBodyOutputNegative", TxBodyOutputNegative 1 txOutInAnyEra1)
    , ("TxBodyOutputOverflow", TxBodyOutputOverflow 1 txOutInAnyEra1)
    , ("TxBodyMetadataError", TxBodyMetadataError [(1, TxMetadataBytesTooLong 2)])
    , ("TxBodyMissingProtocolParams", TxBodyMissingProtocolParams)
    , ("TxBodyInIxOverflow", TxBodyInIxOverflow txin1)
    ]

test_TxBodyErrorAutoBalance :: TestTree
test_TxBodyErrorAutoBalance =
  testAllErrorMessages_
    "Cardano.Api.Internal.Fees"
    "TxBodyErrorAutoBalance"
    [ ("TxBodyError", TxBodyError TxBodyEmptyTxIns)
    ,
      ( "TxBodyScriptExecutionError"
      , TxBodyScriptExecutionError [(ScriptWitnessIndexTxIn 1, ScriptErrorExecutionUnitsOverflow)]
      )
    , ("TxBodyScriptBadScriptValidity", TxBodyScriptBadScriptValidity)
    , ("TxBodyErrorAdaBalanceNegative", TxBodyErrorAdaBalanceNegative 1)
    , ("TxBodyErrorAdaBalanceTooSmall", TxBodyErrorAdaBalanceTooSmall txOutInAnyEra1 0 1)
    , ("TxBodyErrorByronEraNotSupported", TxBodyErrorByronEraNotSupported)
    , ("TxBodyErrorMissingParamMinUTxO", TxBodyErrorMissingParamMinUTxO)
    ,
      ( "TxBodyErrorValidityInterval"
      , TxBodyErrorValidityInterval $ TransactionValidityCostModelError Map.empty string
      )
    , ("TxBodyErrorMinUTxONotMet", TxBodyErrorMinUTxONotMet txOutInAnyEra1 1)
    ,
      ( "TxBodyErrorNonAdaAssetsUnbalanced"
      , TxBodyErrorNonAdaAssetsUnbalanced (fromList [(AdaAssetId, Quantity 1)])
      )
    ,
      ( "TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap"
      , TxBodyErrorScriptWitnessIndexMissingFromExecUnitsMap
          (ScriptWitnessIndexTxIn 1)
          (fromList [(ScriptWitnessIndexTxIn 2, ExecutionUnits 1 1)])
      )
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

testAllErrorMessages_
  :: forall a
   . (HasCallStack, Error a)
  => String
  -- ^ module name
  -> String
  -- ^ type name
  -> [(String, a)]
  -- ^ list of constructor names and values
  -> TestTree
testAllErrorMessages_ = ErrorMessage.testAllErrorMessages_ goldenFilesPath
