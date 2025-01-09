{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Gen.Cardano.Api.Typed
  ( genFeaturedInEra
  , genMaybeFeaturedInEra

    -- * Byron
  , genAddressInEraByron
  , genAddressByron
  , genTxBodyByron
  , genTxByron
  , genTxOutByron
  , genWitnessesByron
  , genAddressInEra
  , genAddressShelley
  , genCertificate
  , genCostModel
  , genCostModels
  , genMaybePraosNonce
  , genPraosNonce
  , genValidProtocolParameters
  , genProtocolParameters
  , genValueNestedRep
  , genValueNestedBundle
  , genByronKeyWitness
  , genCardanoKeyWitness
  , genShelleyKeyWitness
  , genTxId
  , genTxIn
  , genTxOutTxContext
  , genTxOutUTxOContext
  , genUTxO

    -- * Scripts
  , genHashableScriptData
  , genReferenceScript
  , genScript
  , genValidScript
  , genSimpleScript
  , genPlutusScript
  , genPlutusV1Script
  , genPlutusV2Script
  , genPlutusV3Script
  , genScriptInAnyLang
  , genScriptInEra
  , genScriptHash
  , genScriptData
  , genScriptDataSchema
  , genScriptValidity
  , genAssetName
  , genAssetId
  , genEpochNo
  , genExecutionUnitPrices
  , genExecutionUnits
  , genHashScriptData
  , genKESPeriod
  , genNat
  , genNetworkId
  , genNetworkMagic
  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genOperationalCertificateWithCounter
  , genPaymentCredential
  , genPolicyId
  , genQuantity
  , genRationalInt64
  , genSeed
  , genShelleyBootstrapWitness
  , genShelleyHash
  , genShelleyWitness
  , genShelleyWitnessSigningKey
  , genSignedQuantity
  , genSignedNonZeroQuantity
  , genSigningKey
  , genSlotNo
  , genStakeAddress
  , genStakeAddressReference
  , genStakeCredential
  , genTtl
  , genTx
  , genTxAuxScripts
  , genTxBody
  , genTxBodyContent
  , genValidTxBody
  , genTxCertificates
  , genTxFee
  , genTxIndex
  , genTxInsCollateral
  , genTxInsReference
  , genTxMetadataInEra
  , genTxMintValue
  , genLovelace
  , genPositiveLovelace
  , genValue
  , genValueDefault
  , genValueForRole
  , genVerificationKey
  , genVerificationKeyHash
  , genUpdateProposal
  , genProtocolParametersUpdate
  , genTxOutDatumHashTxContext
  , genTxOutDatumHashUTxOContext
  , genTxOutValue
  , genTxReturnCollateral
  , genTxScriptValidity
  , genTxTotalCollateral
  , genTxUpdateProposal
  , genTxValidityLowerBound
  , genTxValidityUpperBound
  , genTxWithdrawals
  , genUnsignedQuantity
  , genPositiveQuantity
  , genValueForMinting
  , genValueForTxOut
  , genWitnesses
  , genWitnessNetworkIdOrByronAddress
  , genRational
  , genGovernancePoll
  , genGovernancePollAnswer
  , genProposals
  , genProposal
  , genVotingProcedures
  )
where

import           Cardano.Api hiding (txIns)
import qualified Cardano.Api as Api
import           Cardano.Api.Byron (KeyWitness (ByronKeyWitness),
                   WitnessNetworkIdOrByronAddress (..))
import qualified Cardano.Api.Byron as Byron
import           Cardano.Api.Error
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Ledger.Lens as A
import           Cardano.Api.Script (scriptInEraToRefScript)
import           Cardano.Api.Shelley
import qualified Cardano.Api.Shelley as ShelleyApi

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hash.Class as CRYPTO
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.SafeHash (unsafeMakeSafeHash)

import           Control.Applicative (Alternative (..), optional)
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce
import           Data.Int (Int64)
import           Data.Maybe
import qualified Data.ByteString.Base16 as Base16
import           Data.Ratio (Ratio, (%))
import           Data.String
import           Data.Word (Word16, Word32, Word64)
import           GHC.Exts (IsList (..))
import           GHC.Stack
import           Numeric.Natural (Natural)

import           Test.Gen.Cardano.Api.Era
import           Test.Gen.Cardano.Api.Metadata (genTxMetadata)

import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId)
import           Test.Cardano.Ledger.Conway.Arbitrary ()
import           Test.Cardano.Ledger.Core.Arbitrary ()

import           Hedgehog (Gen, MonadGen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Q
import qualified Hedgehog.Range as Range

genAddressByron :: Gen (Address ByronAddr)
genAddressByron =
  makeByronAddress
    <$> genNetworkId
    <*> genVerificationKey AsByronKey

genAddressShelley :: Gen (Address ShelleyAddr)
genAddressShelley =
  makeShelleyAddress
    <$> genNetworkId
    <*> genPaymentCredential
    <*> genStakeAddressReference

genAddressInEra :: ShelleyBasedEra era -> Gen (AddressInEra era)
genAddressInEra sbe = shelleyAddressInEra sbe <$> genAddressShelley

_genAddressInEraByron :: Gen (AddressInEra era)
_genAddressInEraByron = byronAddressInEra <$> genAddressByron

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genLovelace :: Gen L.Coin
genLovelace = L.Coin <$> Gen.integral (Range.linear 0 5000)

genPositiveLovelace :: Gen L.Coin
genPositiveLovelace = L.Coin <$> Gen.integral (Range.linear 1 5000)

----------------------------------------------------------------------------
-- SimpleScript generators
--

-- This generator does not generate the deprecated double encoded plutus scripts
genValidScript :: ScriptLanguage lang -> Gen (Script lang)
genValidScript SimpleScriptLanguage =
  SimpleScript <$> genSimpleScript
genValidScript (PlutusScriptLanguage lang) =
  PlutusScript lang <$> genValidPlutusScript lang

-- This generator will also generate the deprecated double encoded plutus scripts
genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript SimpleScriptLanguage =
  SimpleScript <$> genSimpleScript
genScript (PlutusScriptLanguage lang) =
  PlutusScript lang <$> genPlutusScript lang

genSimpleScript :: Gen SimpleScript
genSimpleScript =
  genTerm
 where
  genTerm = Gen.recursive Gen.choice nonRecursive recursive

  -- Non-recursive generators
  nonRecursive =
    [ RequireSignature . verificationKeyHash <$> genVerificationKey AsPaymentKey
    , RequireTimeBefore <$> genSlotNo
    , RequireTimeAfter <$> genSlotNo
    ]

  -- Recursive generators
  recursive =
    [ RequireAllOf <$> Gen.list (Range.linear 0 10) genTerm
    , RequireAnyOf <$> Gen.list (Range.linear 0 10) genTerm
    , do
        ts <- Gen.list (Range.linear 0 10) genTerm
        m <- Gen.integral (Range.constant 0 (length ts))
        return (RequireMOf m ts)
    ]

-- | 'genPlutusScript' will generate the deprecated double encoded
-- plutus scripts as well as valid plutus scripts.
genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript l =
  case l of 
    PlutusScriptV1 -> do 
      PlutusScript _ s <- genPlutusV1Script
      return s
    PlutusScriptV2 -> do 
      PlutusScript _ s <- genPlutusV2Script
      return s
    PlutusScriptV3 -> do 
      PlutusScript _ s <- genPlutusV3Script
      return s

genValidPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genValidPlutusScript l =
  case l of 
    PlutusScriptV1 -> do 
      PlutusScript _ s <- genValidPlutusV1Script
      return s
    PlutusScriptV2 -> do 
      PlutusScript _ s <- genValidPlutusV2Script
      return s
    PlutusScriptV3 -> do 
      PlutusScript _ s <- genValidPlutusV3Script
      return s

genPlutusV1Script :: Gen (Script PlutusScriptV1)
genPlutusV1Script = do 
  v1Script <- Gen.element [v1Loop2024PlutusScriptHexDoubleEncoded,v1Loop2024PlutusScriptHex]
  let v1ScriptBytes = Base16.decodeLenient v1Script
  return . PlutusScript PlutusScriptV1 . PlutusScriptSerialised $ SBS.toShort v1ScriptBytes

genValidPlutusV1Script :: Gen (Script PlutusScriptV1)
genValidPlutusV1Script = do 
  v1Script <- Gen.element [v1Loop2024PlutusScriptHex]
  let v1ScriptBytes = Base16.decodeLenient v1Script
  return . PlutusScript PlutusScriptV1 . PlutusScriptSerialised $ SBS.toShort v1ScriptBytes

genPlutusV2Script :: Gen (Script PlutusScriptV2)
genPlutusV2Script = do
  v2Script <- Gen.element [v2EcdsaLoopPlutusScriptHexDoubleEncoded, v2EcdsaLoopPlutusScriptHex]
  let v2ScriptBytes = Base16.decodeLenient v2Script
  return . PlutusScript PlutusScriptV2 . PlutusScriptSerialised $ SBS.toShort v2ScriptBytes

genValidPlutusV2Script :: Gen (Script PlutusScriptV2)
genValidPlutusV2Script = do
  v2Script <- Gen.element [v2EcdsaLoopPlutusScriptHex]
  let v2ScriptBytes = Base16.decodeLenient v2Script
  return . PlutusScript PlutusScriptV2 . PlutusScriptSerialised $ SBS.toShort v2ScriptBytes

genPlutusV3Script :: Gen (Script PlutusScriptV3)
genPlutusV3Script = do
  v3AlwaysSucceedsPlutusScriptHex 
   <- Gen.element [v3AlwaysSucceedsPlutusScriptDoubleEncoded, v3AlwaysSucceedsPlutusScript]
  let v3ScriptBytes = Base16.decodeLenient v3AlwaysSucceedsPlutusScriptHex
  return . PlutusScript PlutusScriptV3 . PlutusScriptSerialised $ SBS.toShort v3ScriptBytes

genValidPlutusV3Script :: Gen (Script PlutusScriptV3)
genValidPlutusV3Script = do
  v3AlwaysSucceedsPlutusScriptHex 
   <- Gen.element [v3AlwaysSucceedsPlutusScript]
  let v3ScriptBytes = Base16.decodeLenient v3AlwaysSucceedsPlutusScriptHex
  return . PlutusScript PlutusScriptV3 . PlutusScriptSerialised $ SBS.toShort v3ScriptBytes

v1Loop2024PlutusScriptHexDoubleEncoded :: ByteString
v1Loop2024PlutusScriptHexDoubleEncoded = "5850584e010000332232222325335333573466e200052080897a0070061613005001375a00464600200244a66a666ae68cdc3a410112f40020080062240022646600600600266e0400520021220021220011"

v1Loop2024PlutusScriptHex :: ByteString
v1Loop2024PlutusScriptHex = BS.drop 4 v1Loop2024PlutusScriptHexDoubleEncoded

v2EcdsaLoopPlutusScriptHexDoubleEncoded :: ByteString 
v2EcdsaLoopPlutusScriptHexDoubleEncoded = "59023f59023c01000033223232322225335332233333233001005225335333573466e1d200000200d00c153323533335573e0044a00c4600e660046ae8400cd5d1001806109a80091299a9999998038011128051280492804918050009280490a99a9999aab9f0022500a2300b33006357420066ae8800c04084d4004894cd4cccccc02c0088940389403494034940348c038004854cd4cccd55cf8011280711807998051aba100335744006028426a00244a66a66666601e00444a0244a0224a0224a0224602400242a66a6666aae7c008940488c8c8c054008d5d10021aba10030182153353333330110012250142501325013250132301400121301412333300100c0080040021501215011150101500d1500c150091500822123300100300215004150042222223333333574800c4646600e6aae74004d55cf0009baa00723005375600e460086eb001c8c00cdd6803918011bae00700e25002250022500225002212230020031122001213500122225335333573466e200112080897a00d00c1300a4911572656465656d6572206973203c20313030303030300013333009004003002001130054911d5472616365206572726f723a20496e76616c69642072656465656d657200323001001222225335333573466e1d2080897a0040090081007153353335734666ed000c0080040240204cccc8cc018018004cdc0802240040060040022600c921245472616365206572726f723a2045434453412076616c69646174696f6e206661696c6564002326335738002004240022440042440021"

v2EcdsaLoopPlutusScriptHex :: ByteString 
v2EcdsaLoopPlutusScriptHex = BS.drop 6 v2EcdsaLoopPlutusScriptHexDoubleEncoded

v3AlwaysSucceedsPlutusScriptDoubleEncoded :: ByteString 
v3AlwaysSucceedsPlutusScriptDoubleEncoded = "590b2c590b29010100323232323232323232232498c8c8c954ccd5cd19b874800000844c8c8c8c8c8c8c8ca002646464aa666ae68cdc3a4000004226464646464646464646464646464646466666666666646664664664444444444444445001010807c03a01b00c805c02a013008803c01a00b004801c00a00230013574202860026ae8404cc0908c8c8c954ccd5cd19b87480000084600260406ae84006600a6ae84d5d1000844c0b52401035054310035573c0046aae74004dd5000998120009aba1011232323255333573466e1d20000021132328009919192a999ab9a3370e900000108c004c08cd5d0800ccc0848c8c8c954ccd5cd19b874800000846002604e6ae8400422aa666ae68cdc3a40040042265003375a6ae8400a6eb4d5d0800cdd69aba1357440023574400222606a9201035054310035573c0046aae74004dd50009aba135744002113031491035054310035573c0046aae74004dd51aba100398039aba10029919192a999ab9a3370e900000108c0004554ccd5cd19b87480080084600a6eb8d5d080084554ccd5cd19b8748010008460066ae840042260629201035054310035573c0046aae74004dd51aba10019980f3ae357426ae880046ae88004d5d1000889816249035054310035573c0046aae74004dd50009bad3574201e60026ae84038c004c005d69981100b1aba100c33301501975a6ae8402cc8c8c954ccd5cd19b874800000846002646464aa666ae68cdc3a4000004230013302b75a6ae8400660546ae84d5d1000844c0b5241035054310035573c0046aae74004dd51aba10019919192a999ab9a3370e900000108c004cc0add69aba100198151aba13574400211302d4901035054310035573c0046aae74004dd51aba13574400211302a4901035054310035573c0046aae74004dd51aba100a3302275c6ae84024ccc0548c8c8c954ccd5cd19b8748000008460066eb8d5d080084554ccd5cd19b874800800846012603c6ae8400422aa666ae68cdc3a400800423007301d357420021155333573466e1d2006002118009bad35742003301a357426ae8800422aa666ae68cdc3a40100042300b301c357420021155333573466e1d200a002118029bad357420033018357426ae880042260569201035054310035573c0046aae74004dd50008119aba1008330010233574200e6eb8d5d080319980a80c1980a81311919192a999ab9a3370e900000108c0084554ccd5cd19b87480080084600822aa666ae68cdc3a40080042300011302b491035054310035573c0046aae74004dd50009aba1005330220143574200860046ae8400cc008d5d09aba2003301475c602aeb4d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba20011130174901035054310035573c0046aae74004dd51aba10099aba10089919192a999ab9a3370e900000108c00cdd71aba100108aa999ab9a3370e900100108c024c028d5d0800ccc01c04cd5d09aba200108aa999ab9a3370e900200108c01cc024d5d080084554ccd5cd19b8748018008460026eb4d5d0800cc018d5d09aba200108aa999ab9a3370e900400108c02cc020d5d080084554ccd5cd19b87480280084600a6eb4d5d0800cc010d5d09aba200108980ba481035054310035573c0046aae74004dd51aba135744010232323255333573466e1d200000211328009bad35742005300a3574200332323255333573466e1d200000211328049980600d9aba10029aba1001998063ae357426ae880046ae880044554ccd5cd19b874800800846002660160346ae84006646464aa666ae68cdc3a400000423001375a6ae840066eb4d5d09aba200108980f2481035054310035573c0046aae74004dd51aba1357440021155333573466e1d200400211805999804806bad357420033300b75c6ae84d5d100084554ccd5cd19b87480180084600e660160346ae8400422aa666ae68cdc3a401000422646500d3300d01c357420073301800f3574200533300b00f75a6ae840072646464aa666ae68cdc3a400000423001375a6ae840066eb4d5d09aba20010898102481035054310035573c0046aae74004dd51aba13574400322330180020010d5d10009aba20011155333573466e1d200a002118029980580d1aba10019919192a999ab9a3370e900000108998073ae3574200222603c9201035054310035573c0046aae74004dd51aba1357440021155333573466e1d200c0021180108980da481035054310035573c0046aae74004dd51aba1357440023574400222602e9201035054310035573c0046aae74004dd50009119118011bab00130152233335573e0025000232801c004c018d55ce800cc014d55cf000a60086ae8800c6ae8400a0004646464aa666ae68cdc3a40000042300d3007357420033300575a6ae84d5d100084554ccd5cd19b874800800846026600e6ae840066600aeb4d5d09aba200108a992999ab9a3370e900200188c00cc020d5d08014c004d5d09aba200208aa999ab9a3370e90030018899402cc024d5d0801cc008d5d0800cdd69aba1357440023574400422aa666ae68cdc3a401000623009300835742005375a6ae84d5d100104554ccd5cd19b874802800c4602a60106ae8400822aa666ae68cdc3a401800623011300835742005375a6ae84d5d100104554ccd5cd19b874803800c4600a6eb8d5d08014dd71aba1357440041155333573466e1d2010003118039bae35742005375a6ae84d5d100104554ccd5cd19b874804800c4600260106ae8400a60106ae84d5d100104554ccd5cd19b874805000c4601e60106ae8400822602c9210350543100232323255333573466e1d2000002118009bae35742002115325333573466e1d20020031180298009aba100208aa999ab9a3370e900200188c00cdd71aba100298009aba13574400411301a49010350543100232323255333573466e1d20000021180098079aba100108aa999ab9a3370e900100108c0084554ccd5cd19b87480100084600822603a9201035054310035573c0046aae74004dd50009aab9e00235573a0026ea8004d55cf0011aab9d001375400244646464aa666ae68cdc3a4004004230021155333573466e1d20000021180098029aba100108980aa49035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c004c014d5d080084554ccd5cd19b874800800846006600a6ae8400422aa666ae68cdc3a400800423005375c6ae840042260269201035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c004dd71aba100108aa999ab9a3370e900100108c00cdd71aba1001089809249035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c004dd71aba10019bad357426ae880042260229201035054310035573c0046aae74004dd50009aba200111300c4901035054310035573c0046aae74004dd500098041112a999ab9a3370e9000000889805248103505433001155333573466e200052000113300333702900000119b814800000444ca00266e1000c00666e1000800466008004002600e444aa666ae68cdc3a400000222004226600600266e180080048c88c008dd60009803911999aab9f00128001400cc010d5d08014c00cd5d1001200040024646464aa666ae68cdc3a4000004230021155333573466e1d200200211800089803a481035054310035573c0046aae74004dd5000911919192a999ab9a3370e900000108c0084554ccd5cd19b874800800846002600a6ae8400422aa666ae68cdc3a400800423004113007491035054310035573c0046aae74004dd500091919192a999ab9a3370e900000108c004dd71aba10019bad357426ae8800422600a9201035054310035573c0046aae74004dd5000919319ab9c0018001191800800918011198010010009"

v3AlwaysSucceedsPlutusScript :: ByteString
v3AlwaysSucceedsPlutusScript = BS.drop 6 v3AlwaysSucceedsPlutusScriptDoubleEncoded

genScriptDataSchema :: Gen ScriptDataJsonSchema
genScriptDataSchema = Gen.element [ScriptDataJsonNoSchema, ScriptDataJsonDetailedSchema]

genHashableScriptData :: HasCallStack => Gen HashableScriptData
genHashableScriptData = do
  sd <- genScriptData
  case deserialiseFromCBOR AsHashableScriptData $ serialiseToCBOR sd of
    Left e -> error $ "genHashableScriptData: " <> show e
    Right r -> return r

{-# DEPRECATED genScriptData "Use genHashableScriptData" #-}
genScriptData :: Gen ScriptData
genScriptData =
  Gen.recursive
    Gen.choice
    [ ScriptDataNumber <$> genInteger
    , ScriptDataBytes <$> genByteString
    ]
    -- The Gen.recursive combinator calls these with the size halved
    [ ScriptDataConstructor
        <$> genConstructorInteger
        <*> genScriptDataList
    , ScriptDataList <$> genScriptDataList
    , ScriptDataMap <$> genScriptDataMap
    ]
 where
  genInteger :: Gen Integer
  genInteger =
    Gen.integral
      ( Range.linear
          (-fromIntegral (maxBound :: Word64) :: Integer)
          (2 * fromIntegral (maxBound :: Word64) :: Integer)
      )

  genConstructorInteger :: Gen Integer
  genConstructorInteger =
    Gen.integral
      ( Range.linear
          0 -- TODO: Alonzo should be -> (-fromIntegral (maxBound :: Word64) :: Integer)
          -- Wrapping bug needs to be fixed in Plutus library
          (fromIntegral (maxBound :: Word64) :: Integer)
      )

  genByteString :: Gen ByteString
  genByteString =
    BS.pack
      <$> Gen.list
        (Range.linear 0 64)
        (Gen.word8 Range.constantBounded)

  genScriptDataList :: Gen [ScriptData]
  genScriptDataList =
    Gen.sized $ \sz ->
      Gen.list (Range.linear 0 (fromIntegral sz)) genScriptData

  genScriptDataMap :: Gen [(ScriptData, ScriptData)]
  genScriptDataMap =
    Gen.sized $ \sz ->
      Gen.list (Range.linear 0 (fromIntegral sz)) $
        (,) <$> genScriptData <*> genScriptData

-- ----------------------------------------------------------------------------
-- Script generators for any language, or any language valid in a specific era
--

genScriptInAnyLang :: Gen ScriptInAnyLang
genScriptInAnyLang =
  Gen.choice
    [ ScriptInAnyLang lang <$> genScript lang
    | AnyScriptLanguage lang <- [minBound .. maxBound]
    ]

genScriptInEra :: ShelleyBasedEra era -> Gen (ScriptInEra era)
genScriptInEra era =
  Gen.choice
    [ ScriptInEra langInEra <$> genValidScript lang
    | AnyScriptLanguage lang <- [minBound .. maxBound]
    , Just langInEra <- [scriptLanguageSupportedInEra era lang]
    ]

genScriptHash :: Gen ScriptHash
genScriptHash = do
  ScriptInAnyLang _ script <- genScriptInAnyLang
  return (hashScript script)

----------------------------------------------------------------------------
-- Multi-asset generators
--

genAssetName :: Gen AssetName
genAssetName =
  Gen.frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element ["", "a", "b", "c"])
    , (1, AssetName <$> Gen.bytes (Range.singleton 32))
    , (1, AssetName <$> Gen.bytes (Range.constant 1 31))
    ]

genPolicyId :: Gen PolicyId
genPolicyId =
  Gen.frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [fromString (x : replicate 55 '0') | x <- ['a' .. 'c']])
    , -- and some from the full range of the type
      (1, PolicyId <$> genScriptHash)
    ]

genAssetId :: Gen AssetId
genAssetId =
  AssetId <$> genPolicyId <*> genAssetName

genQuantity :: Range Integer -> Gen Quantity
genQuantity range = fromInteger <$> Gen.integral range

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = genQuantity (Range.constantFrom 0 (-2) 2)

-- | Generate a positive or negative, but not zero quantity.
genSignedNonZeroQuantity :: Gen Quantity
genSignedNonZeroQuantity =
  Gen.choice
    [ genQuantity (Range.constant (-2) (-1))
    , genQuantity (Range.constant 1 2)
    ]

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = genQuantity (Range.constant 0 2)

genPositiveQuantity :: Gen Quantity
genPositiveQuantity = genQuantity (Range.constant 1 2)

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$> Gen.list
      (Range.constant 0 10)
      ((,) <$> genAId <*> genQuant)

genLedgerValue
  :: MaryEraOnwards era -> Gen AssetId -> Gen Quantity -> Gen (L.Value (ShelleyLedgerEra era))
genLedgerValue w genAId genQuant =
  toLedgerValue w <$> genValue genAId genQuant

-- | Generate a 'Value' with any asset ID and a positive or negative quantity.
genValueDefault :: MaryEraOnwards era -> Gen (L.Value (ShelleyLedgerEra era))
genValueDefault w = genLedgerValue w genAssetId genSignedNonZeroQuantity

genValueForRole :: MaryEraOnwards era -> ParserValueRole -> Gen Value
genValueForRole w =
  \case
    RoleMint ->
      genValueForMinting
    RoleUTxO ->
      fromLedgerValue (convert w) <$> genValueForTxOut (convert w)

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting =
  genValue genAssetIdNoAda genSignedNonZeroQuantity
 where
  genAssetIdNoAda :: Gen AssetId
  genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: ShelleyBasedEra era -> Gen (L.Value (ShelleyLedgerEra era))
genValueForTxOut sbe = do
  -- Generate at least one positive ADA, without it Value in TxOut makes no sense
  -- and will fail deserialization starting with ConwayEra
  ada <- A.mkAdaValue sbe . L.Coin <$> Gen.integral (Range.constant 1 2)

  -- Generate a potentially empty list with multi assets
  caseShelleyToAllegraOrMaryEraOnwards
    (const (pure ada))
    ( \w -> do
        v <- Gen.list (Range.constant 0 5) $ genLedgerValue w genAssetId genPositiveQuantity
        pure $ ada <> mconcat v
    )
    sbe

-- Note that we expect to sometimes generate duplicate policy id keys since we
-- pick 90% of policy ids from a set of just three.
genValueNestedRep :: Gen ValueNestedRep
genValueNestedRep =
  ValueNestedRep <$> Gen.list (Range.constant 0 5) genValueNestedBundle

genValueNestedBundle :: Gen ValueNestedBundle
genValueNestedBundle =
  Gen.choice
    [ ValueNestedBundleAda <$> genSignedQuantity
    , ValueNestedBundle
        <$> genPolicyId
        <*> Gen.map
          (Range.constant 0 5)
          ((,) <$> genAssetName <*> genSignedQuantity)
    ]

genNetworkId :: Gen NetworkId
genNetworkId =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genNetworkMagic
    ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = NetworkMagic <$> Gen.word32 Range.constantBounded

genOperationalCertificate :: Gen OperationalCertificate
genOperationalCertificate = fst <$> genOperationalCertificateWithCounter

genOperationalCertificateIssueCounter :: Gen OperationalCertificateIssueCounter
genOperationalCertificateIssueCounter = snd <$> genOperationalCertificateWithCounter

genOperationalCertificateWithCounter
  :: HasCallStack => Gen (OperationalCertificate, OperationalCertificateIssueCounter)
genOperationalCertificateWithCounter = do
  kesVKey <- genVerificationKey AsKesKey
  stkPoolOrGenDelExtSign <-
    Gen.either (genSigningKey AsStakePoolKey) (genSigningKey AsGenesisDelegateExtendedKey)
  kesP <- genKESPeriod
  c <- Gen.integral $ Range.linear 0 1000
  let stakePoolVer = either getVerificationKey (convert' . getVerificationKey) stkPoolOrGenDelExtSign
      iCounter = OperationalCertificateIssueCounter c stakePoolVer

  case issueOperationalCertificate kesVKey stkPoolOrGenDelExtSign kesP iCounter of
    -- This case should be impossible as we clearly derive the verification
    -- key from the generated signing key.
    Left err -> error $ docToString $ prettyError err
    Right pair -> return pair
 where
  convert'
    :: VerificationKey GenesisDelegateExtendedKey
    -> VerificationKey StakePoolKey
  convert' =
    ( castVerificationKey
        :: VerificationKey GenesisDelegateKey
        -> VerificationKey StakePoolKey
    )
      . ( castVerificationKey
            :: VerificationKey GenesisDelegateExtendedKey
            -> VerificationKey GenesisDelegateKey
        )

-- TODO: Generate payment credential via script
genPaymentCredential :: Gen PaymentCredential
genPaymentCredential = do
  vKey <- genVerificationKey AsPaymentKey
  return . PaymentCredentialByKey $ verificationKeyHash vKey

genSigningKey :: Key keyrole => ShelleyApi.AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
  seed <- genSeed (fromIntegral seedSize)
  let sk = deterministicSigningKey roletoken seed
  return sk
 where
  seedSize :: Word
  seedSize = deterministicSigningKeySeedSize roletoken

genStakeAddress :: Gen StakeAddress
genStakeAddress = makeStakeAddress <$> genNetworkId <*> genStakeCredential

-- TODO: Generate StakeAddressReference via pointer
genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  Gen.choice
    [ StakeAddressByValue <$> genStakeCredential
    , return NoStakeAddress
    ]

-- TODO: Generate StakeCredential via script
genStakeCredential :: Gen StakeCredential
genStakeCredential = do
  vKey <- genVerificationKey AsStakeKey
  return . StakeCredentialByKey $ verificationKeyHash vKey

genShelleyHash :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = TxIx . fromIntegral <$> Gen.word16 Range.constantBounded

genTxOutValue :: ShelleyBasedEra era -> Gen (TxOutValue era)
genTxOutValue sbe = shelleyBasedEraConstraints sbe $ TxOutValueShelleyBased sbe <$> genValueForTxOut sbe

genTxOutTxContext :: ShelleyBasedEra era -> Gen (TxOut CtxTx era)
genTxOutTxContext era =
  TxOut
    <$> genAddressInEra era
    <*> genTxOutValue era
    <*> genTxOutDatumHashTxContext era
    <*> genReferenceScript era

genTxOutUTxOContext :: ShelleyBasedEra era -> Gen (TxOut CtxUTxO era)
genTxOutUTxOContext era =
  TxOut
    <$> genAddressInEra era
    <*> genTxOutValue era
    <*> genTxOutDatumHashUTxOContext era
    <*> genReferenceScript era

genReferenceScript :: ShelleyBasedEra era -> Gen (ReferenceScript era)
genReferenceScript era = scriptInEraToRefScript <$> genScriptInEra era

genUTxO :: ShelleyBasedEra era -> Gen (UTxO era)
genUTxO era =
  UTxO
    <$> Gen.map (Range.constant 0 5) ((,) <$> genTxIn <*> (toCtxUTxOTxOut <$> genTxOutTxContext era))

genTtl :: Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound =
  inEonForEra
    (pure TxValidityNoLowerBound)
    (\w -> TxValidityLowerBound w <$> genTtl)

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: ShelleyBasedEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound sbe =
  TxValidityUpperBound sbe <$> Gen.maybe genTtl

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra =
  inEonForEra
    (pure TxMetadataNone)
    ( \w ->
        Gen.choice
          [ pure TxMetadataNone
          , TxMetadataInEra w <$> genTxMetadata
          ]
    )

genTxAuxScripts :: ShelleyBasedEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  forEraInEon
    (toCardanoEra era)
    (pure TxAuxScriptsNone)
    ( \w ->
        TxAuxScripts w
          <$> Gen.list
            (Range.linear 0 3)
            (genScriptInEra $ convert w)
    )

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals build era)
genTxWithdrawals =
  inEonForEra
    (pure TxWithdrawalsNone)
    ( \w ->
        Gen.choice
          [ pure TxWithdrawalsNone
          , pure (TxWithdrawals w mempty)
          -- TODO: Generate withdrawals
          ]
    )

genTxCertificates :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates =
  inEonForEra
    (pure TxCertificatesNone)
    ( \w -> do
        certs <- Gen.list (Range.constant 0 3) $ genCertificate w
        Gen.choice
          [ pure TxCertificatesNone
          , pure (TxCertificates w certs $ BuildTxWith mempty)
          -- TODO: Generate certificates
          ]
    )

-- TODO: Add remaining certificates
-- TODO: This should be parameterised on ShelleyBasedEra
genCertificate :: ShelleyBasedEra era -> Gen (Certificate era)
genCertificate sbe =
  Gen.choice
    [ makeStakeAddressRegistrationCertificate <$> genStakeAddressRequirements sbe
    , makeStakeAddressUnregistrationCertificate <$> genStakeAddressRequirements sbe
    ]

genStakeAddressRequirements :: ShelleyBasedEra era -> Gen (StakeAddressRequirements era)
genStakeAddressRequirements =
  caseShelleyToBabbageOrConwayEraOnwards
    ( \w ->
        StakeAddrRegistrationPreConway w
          <$> genStakeCredential
    )
    ( \w ->
        StakeAddrRegistrationConway w
          <$> genLovelace
          <*> genStakeCredential
    )

genTxUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal sbe =
  Gen.choice $
    catMaybes
      [ Just $ pure TxUpdateProposalNone
      , forEraInEon sbe Nothing $ \w ->
          Just $ TxUpdateProposal w <$> genUpdateProposal (toCardanoEra w)
      ]

genTxMintValue :: CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue =
  inEonForEra
    (pure TxMintNone)
    $ \w -> do
      policies <- Gen.list (Range.constant 1 3) genPolicyId
      assets <- forM policies $ \policy ->
        (,) policy <$>
          Gen.list
            (Range.constant 1 3)
            ((,,) <$> genAssetName
                  <*> genPositiveQuantity
                  <*> fmap (fmap pure) genScriptWitnessForMint (maryEraOnwardsToShelleyBasedEra w))
      Gen.choice
        [ pure TxMintNone
        , pure $ TxMintValue w (fromList assets)
        ]

genTxBodyContent :: ShelleyBasedEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent sbe = do
  let era = toCardanoEra sbe
  txIns <-
    map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> Gen.list (Range.constant 1 10) genTxIn
  txInsCollateral <- genTxInsCollateral era
  txInsReference <- genTxInsReference era
  txOuts <- Gen.list (Range.constant 1 10) (genTxOutTxContext sbe)
  txTotalCollateral <- genTxTotalCollateral era
  txReturnCollateral <- genTxReturnCollateral sbe
  txFee <- genTxFee sbe
  txValidityLowerBound <- genTxValidityLowerBound era
  txValidityUpperBound <- genTxValidityUpperBound sbe
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts sbe
  let txExtraKeyWits = TxExtraKeyWitnessesNone -- TODO: Alonzo era: Generate witness key hashes
  txProtocolParams <-
    BuildTxWith <$> forEraInEon era (pure Nothing) (Gen.maybe . genValidProtocolParameters)
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era
  txProposalProcedures <- genMaybeFeaturedInEra genProposals era
  txVotingProcedures <- genMaybeFeaturedInEra genVotingProcedures era
  txCurrentTreasuryValue <- genMaybeFeaturedInEra (Gen.maybe . genCurrentTreasuryValue) era
  txTreasuryDonation <- genMaybeFeaturedInEra genTreasuryDonation era
  pure $
    TxBodyContent
      { Api.txIns
      , Api.txInsCollateral
      , Api.txInsReference
      , Api.txOuts
      , Api.txTotalCollateral
      , Api.txReturnCollateral
      , Api.txFee
      , Api.txValidityLowerBound
      , Api.txValidityUpperBound
      , Api.txMetadata
      , Api.txAuxScripts
      , Api.txExtraKeyWits
      , Api.txProtocolParams
      , Api.txWithdrawals
      , Api.txCertificates
      , Api.txUpdateProposal
      , Api.txMintValue
      , Api.txScriptValidity
      , Api.txProposalProcedures
      , Api.txVotingProcedures
      , Api.txCurrentTreasuryValue
      , Api.txTreasuryDonation
      }

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral =
  inEonForEra
    (pure TxInsCollateralNone)
    ( \w ->
        Gen.choice
          [ pure TxInsCollateralNone
          , TxInsCollateral w <$> Gen.list (Range.linear 0 10) genTxIn
          ]
    )

genTxInsReference :: CardanoEra era -> Gen (TxInsReference era)
genTxInsReference =
  caseByronToAlonzoOrBabbageEraOnwards
    (const (pure TxInsReferenceNone))
    (\w -> TxInsReference w <$> Gen.list (Range.linear 0 10) genTxIn)

genTxReturnCollateral :: ShelleyBasedEra era -> Gen (TxReturnCollateral CtxTx era)
genTxReturnCollateral era =
  forEraInEon
    (toCardanoEra era)
    (pure TxReturnCollateralNone)
    (\w -> TxReturnCollateral w <$> genTxOutTxContext era)

genTxTotalCollateral :: CardanoEra era -> Gen (TxTotalCollateral era)
genTxTotalCollateral =
  inEonForEra
    (pure TxTotalCollateralNone)
    (\w -> TxTotalCollateral w <$> genPositiveLovelace)

genTxFee :: ShelleyBasedEra era -> Gen (TxFee era)
genTxFee w = TxFeeExplicit w <$> genLovelace

genAddressInEraByron :: Gen (AddressInEra ByronEra)
genAddressInEraByron = byronAddressInEra <$> genAddressByron

genTxByron :: Gen (Byron.ATxAux ByteString)
genTxByron = do
  makeSignedByronTransaction
    <$> genWitnessesByron
    <*> genTxBodyByron

genTxOutValueByron :: Gen (TxOutValue ByronEra)
genTxOutValueByron = TxOutValueByron <$> genPositiveLovelace

genTxOutByron :: Gen (TxOut CtxTx ByronEra)
genTxOutByron =
  TxOut
    <$> genAddressInEraByron
    <*> genTxOutValueByron
    <*> pure TxOutDatumNone
    <*> pure ReferenceScriptNone

-- | Partial! It will throw if the generated transaction body is invalid.
genTxBodyByron :: HasCallStack => Gen (L.Annotated Byron.Tx ByteString)
genTxBodyByron = do
  txIns <-
    map (,BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> Gen.list (Range.constant 1 10) genTxIn
  txOuts <- Gen.list (Range.constant 1 10) genTxOutByron
  case Api.makeByronTransactionBody txIns txOuts of
    Left err -> error (displayError err)
    Right txBody -> pure txBody

genWitnessesByron :: Gen [KeyWitness ByronEra]
genWitnessesByron = Gen.list (Range.constant 1 10) genByronKeyWitness

-- | This generator validates generated 'TxBodyContent' and backtracks when the generated body
-- fails the validation. That also means that it is quite slow.
genValidTxBody :: ShelleyBasedEra era
               -> Gen (TxBody era, TxBodyContent BuildTx era) -- ^ validated 'TxBody' and 'TxBodyContent'
genValidTxBody sbe =
  Gen.mapMaybe
    (\content ->
        either (const Nothing) (Just . (, content)) $
          createAndValidateTransactionBody sbe content
    )
    (genTxBodyContent sbe)

-- | Partial! This function will throw an error when the generated transaction is invalid.
genTxBody :: HasCallStack => ShelleyBasedEra era -> Gen (TxBody era)
genTxBody era = do
  res <- Api.createTransactionBody era <$> genTxBodyContent era
  case res of
    Left err -> error (docToString (prettyError err))
    Right txBody -> pure txBody

-- | Generate a 'Featured' for the given 'CardanoEra' with the provided generator.
genFeaturedInEra
  :: ()
  => Alternative f
  => eon era
  -> f a
  -> f (Featured eon era a)
genFeaturedInEra witness gen =
  Featured witness <$> gen

-- | Generate a 'Featured' for the given 'CardanoEra' with the provided generator.
genMaybeFeaturedInEra
  :: ()
  => Eon eon
  => Alternative f
  => (eon era -> f a)
  -> CardanoEra era
  -> f (Maybe (Featured eon era a))
genMaybeFeaturedInEra f =
  inEonForEra (pure Nothing) $ \w ->
    Just <$> genFeaturedInEra w (f w)

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity =
  inEonForEra
    (pure TxScriptValidityNone)
    (\w -> TxScriptValidity w <$> genScriptValidity)

genScriptValidity :: Gen ScriptValidity
genScriptValidity = Gen.element [ScriptInvalid, ScriptValid]

genTx
  :: ()
  => ShelleyBasedEra era
  -> Gen (Tx era)
genTx era =
  makeSignedTransaction
    <$> genWitnesses era
    <*> (fst <$> genValidTxBody era)

genWitnesses :: ShelleyBasedEra era -> Gen [KeyWitness era]
genWitnesses sbe = do
  bsWits <- Gen.list (Range.constant 0 10) (genShelleyBootstrapWitness sbe)
  keyWits <- Gen.list (Range.constant 0 10) (genShelleyKeyWitness sbe)
  return $ bsWits ++ keyWits

genVerificationKey
  :: ()
#if MIN_VERSION_base(4,17,0)
  -- GHC 8.10 considers the HasTypeProxy constraint redundant but ghc-9.6 complains if its not
  -- present.
  => HasTypeProxy keyrole
#endif
  => Key keyrole
  => ShelleyApi.AsType keyrole
  -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash
  :: ()
#if MIN_VERSION_base(4,17,0)
  -- GHC 8.10 considers the HasTypeProxy constraint redundant but ghc-9.6 complains if its not
  -- present.
  => HasTypeProxy keyrole
#endif
  => Key keyrole
  => ShelleyApi.AsType keyrole
  -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genByronKeyWitness :: Gen (KeyWitness ByronEra)
genByronKeyWitness = do
  pmId <- genProtocolMagicId
  txinWitness <- genVKWitness pmId
  return $ ByronKeyWitness txinWitness

genWitnessNetworkIdOrByronAddress :: Gen WitnessNetworkIdOrByronAddress
genWitnessNetworkIdOrByronAddress =
  Gen.choice
    [ WitnessNetworkId <$> genNetworkId
    , WitnessByronAddress <$> genAddressByron
    ]

genShelleyBootstrapWitness
  :: ()
  => ShelleyBasedEra era
  -> Gen (KeyWitness era)
genShelleyBootstrapWitness sbe =
  makeShelleyBootstrapWitness sbe
    <$> genWitnessNetworkIdOrByronAddress
    <*> (fst <$> genValidTxBody sbe)
    <*> genSigningKey AsByronKey

genShelleyKeyWitness
  :: ()
  => ShelleyBasedEra era
  -> Gen (KeyWitness era)
genShelleyKeyWitness sbe =
  makeShelleyKeyWitness sbe . fst
    <$> genValidTxBody sbe
    <*> genShelleyWitnessSigningKey

genShelleyWitness
  :: ()
  => ShelleyBasedEra era
  -> Gen (KeyWitness era)
genShelleyWitness sbe =
  Gen.choice
    [ genShelleyKeyWitness sbe
    , genShelleyBootstrapWitness sbe
    ]

genShelleyWitnessSigningKey :: Gen ShelleyWitnessSigningKey
genShelleyWitnessSigningKey =
  Gen.choice
    [ WitnessPaymentKey <$> genSigningKey AsPaymentKey
    , WitnessPaymentExtendedKey <$> genSigningKey AsPaymentExtendedKey
    , WitnessStakeKey <$> genSigningKey AsStakeKey
    , WitnessStakePoolKey <$> genSigningKey AsStakePoolKey
    , WitnessGenesisDelegateKey <$> genSigningKey AsGenesisDelegateKey
    , WitnessGenesisUTxOKey <$> genSigningKey AsGenesisUTxOKey
    ]

genCardanoKeyWitness
  :: ()
  => ShelleyBasedEra era
  -> Gen (KeyWitness era)
genCardanoKeyWitness = genShelleyWitness

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genNat :: Gen Natural
genNat = Gen.integral (Range.linear 0 10)

genWord16 :: Gen Word16
genWord16 = Gen.integral (Range.linear 0 10)

genWord32 :: Gen Word32
genWord32 = Gen.integral (Range.linear 0 10)

genRational :: Gen Rational
genRational =
  (\d -> ratioToRational (1 % d)) <$> genDenominator
 where
  genDenominator :: Gen Word64
  genDenominator = Gen.integral (Range.linear 1 maxBound)

  ratioToRational :: Ratio Word64 -> Rational
  ratioToRational = toRational

-- TODO: consolidate this back to just genRational once this is merged:
-- https://github.com/input-output-hk/cardano-ledger-specs/pull/2330
genRationalInt64 :: Gen Rational
genRationalInt64 =
  (\d -> ratioToRational (1 % d)) <$> genDenominator
 where
  genDenominator :: Gen Int64
  genDenominator = Gen.integral (Range.linear 1 maxBound)

  ratioToRational :: Ratio Int64 -> Rational
  ratioToRational = toRational

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 10)

genEpochInterval :: Gen Ledger.EpochInterval
genEpochInterval = Ledger.EpochInterval <$> Gen.word32 (Range.linear 0 10)

genPraosNonce :: Gen PraosNonce
genPraosNonce = makePraosNonce <$> Gen.bytes (Range.linear 0 32)

genMaybePraosNonce :: Gen (Maybe PraosNonce)
genMaybePraosNonce = Gen.maybe genPraosNonce

genProtocolParameters :: CardanoEra era -> Gen ProtocolParameters
genProtocolParameters era = do
  protocolParamProtocolVersion <- (,) <$> genNat <*> genNat
  protocolParamDecentralization <- Gen.maybe genRational
  protocolParamExtraPraosEntropy <- genMaybePraosNonce
  protocolParamMaxBlockHeaderSize <- genNat
  protocolParamMaxBlockBodySize <- genNat
  protocolParamMaxTxSize <- genNat
  protocolParamTxFeeFixed <- genLovelace
  protocolParamTxFeePerByte <- genLovelace
  protocolParamMinUTxOValue <- Gen.maybe genLovelace
  protocolParamStakeAddressDeposit <- genLovelace
  protocolParamStakePoolDeposit <- genLovelace
  protocolParamMinPoolCost <- genLovelace
  protocolParamPoolRetireMaxEpoch <- genEpochInterval
  protocolParamStakePoolTargetNum <- genWord16
  protocolParamPoolPledgeInfluence <- genRationalInt64
  protocolParamMonetaryExpansion <- genRational
  protocolParamTreasuryCut <- genRational
  let protocolParamCostModels = mempty
  -- TODO: Babbage figure out how to deal with
  -- asymmetric cost model JSON instances
  protocolParamPrices <- Gen.maybe genExecutionUnitPrices
  protocolParamMaxTxExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxBlockExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxValueSize <- Gen.maybe genNat
  protocolParamCollateralPercent <- Gen.maybe genNat
  protocolParamMaxCollateralInputs <- Gen.maybe genNat
  protocolParamUTxOCostPerByte <-
    inEonForEra @BabbageEraOnwards (pure Nothing) (const (Just <$> genLovelace)) era

  pure ProtocolParameters{..}

-- | Generate valid protocol parameters which pass validations in Cardano.Api.ProtocolParameters
genValidProtocolParameters :: ShelleyBasedEra era -> Gen (LedgerProtocolParameters era)
genValidProtocolParameters sbe = shelleyBasedEraTestConstraints sbe $ LedgerProtocolParameters <$> Q.arbitrary

genProtocolParametersUpdate :: CardanoEra era -> Gen ProtocolParametersUpdate
genProtocolParametersUpdate era = do
  protocolUpdateProtocolVersion <- Gen.maybe ((,) <$> genNat <*> genNat)
  protocolUpdateDecentralization <- Gen.maybe genRational
  protocolUpdateExtraPraosEntropy <- Gen.maybe genMaybePraosNonce
  protocolUpdateMaxBlockHeaderSize <- Gen.maybe genWord16
  protocolUpdateMaxBlockBodySize <- Gen.maybe genWord32
  protocolUpdateMaxTxSize <- Gen.maybe genWord32
  protocolUpdateTxFeeFixed <- Gen.maybe genLovelace
  protocolUpdateTxFeePerByte <- Gen.maybe genLovelace
  protocolUpdateMinUTxOValue <- Gen.maybe genLovelace
  protocolUpdateStakeAddressDeposit <- Gen.maybe genLovelace
  protocolUpdateStakePoolDeposit <- Gen.maybe genLovelace
  protocolUpdateMinPoolCost <- Gen.maybe genLovelace
  protocolUpdatePoolRetireMaxEpoch <- Gen.maybe genEpochInterval
  protocolUpdateStakePoolTargetNum <- Gen.maybe genWord16
  protocolUpdatePoolPledgeInfluence <- Gen.maybe genRationalInt64
  protocolUpdateMonetaryExpansion <- Gen.maybe genRational
  protocolUpdateTreasuryCut <- Gen.maybe genRational
  let protocolUpdateCostModels = mempty -- genCostModels
  -- TODO: Babbage figure out how to deal with
  -- asymmetric cost model JSON instances
  protocolUpdatePrices <- Gen.maybe genExecutionUnitPrices
  protocolUpdateMaxTxExUnits <- Gen.maybe genExecutionUnits
  protocolUpdateMaxBlockExUnits <- Gen.maybe genExecutionUnits
  protocolUpdateMaxValueSize <- Gen.maybe genNat
  protocolUpdateCollateralPercent <- Gen.maybe genNat
  protocolUpdateMaxCollateralInputs <- Gen.maybe genNat
  protocolUpdateUTxOCostPerByte <-
    inEonForEra @BabbageEraOnwards (pure Nothing) (const (Just <$> genLovelace)) era

  pure ProtocolParametersUpdate{..}

genUpdateProposal :: CardanoEra era -> Gen UpdateProposal
genUpdateProposal era =
  UpdateProposal
    <$> Gen.map
      (Range.constant 1 3)
      ( (,)
          <$> genVerificationKeyHash AsGenesisKey
          <*> genProtocolParametersUpdate era
      )
    <*> genEpochNo

genCostModel :: MonadGen m => m Alonzo.CostModel
genCostModel = Q.arbitrary

genCostModels :: MonadGen m => m Alonzo.CostModels
genCostModels = Q.arbitrary

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits =
  ExecutionUnits
    <$> Gen.integral (Range.constant 0 1000)
    <*> Gen.integral (Range.constant 0 1000)

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genTxOutDatumHashTxContext :: ShelleyBasedEra era -> Gen (TxOutDatum CtxTx era)
genTxOutDatumHashTxContext era = case era of
  ShelleyBasedEraShelley -> pure TxOutDatumNone
  ShelleyBasedEraAllegra -> pure TxOutDatumNone
  ShelleyBasedEraMary -> pure TxOutDatumNone
  ShelleyBasedEraAlonzo ->
    Gen.choice
      [ pure TxOutDatumNone
      , TxOutDatumHash AlonzoEraOnwardsAlonzo <$> genHashScriptData
      , TxOutSupplementalDatum AlonzoEraOnwardsAlonzo <$> genHashableScriptData
      ]
  ShelleyBasedEraBabbage ->
    Gen.choice
      [ pure TxOutDatumNone
      , TxOutDatumHash AlonzoEraOnwardsBabbage <$> genHashScriptData
      , TxOutSupplementalDatum AlonzoEraOnwardsBabbage <$> genHashableScriptData
      , TxOutDatumInline BabbageEraOnwardsBabbage <$> genHashableScriptData
      ]
  ShelleyBasedEraConway ->
    Gen.choice
      [ pure TxOutDatumNone
      , TxOutDatumHash AlonzoEraOnwardsConway <$> genHashScriptData
      , TxOutSupplementalDatum AlonzoEraOnwardsConway <$> genHashableScriptData
      , TxOutDatumInline BabbageEraOnwardsConway <$> genHashableScriptData
      ]

genTxOutDatumHashUTxOContext :: ShelleyBasedEra era -> Gen (TxOutDatum CtxUTxO era)
genTxOutDatumHashUTxOContext era = case era of
  ShelleyBasedEraShelley -> pure TxOutDatumNone
  ShelleyBasedEraAllegra -> pure TxOutDatumNone
  ShelleyBasedEraMary -> pure TxOutDatumNone
  ShelleyBasedEraAlonzo ->
    Gen.choice
      [ pure TxOutDatumNone
      , TxOutDatumHash AlonzoEraOnwardsAlonzo <$> genHashScriptData
      ]
  ShelleyBasedEraBabbage ->
    Gen.choice
      [ pure TxOutDatumNone
      , TxOutDatumHash AlonzoEraOnwardsBabbage <$> genHashScriptData
      , TxOutDatumInline BabbageEraOnwardsBabbage <$> genHashableScriptData
      ]
  ShelleyBasedEraConway ->
    Gen.choice
      [ pure TxOutDatumNone
      , TxOutDatumHash AlonzoEraOnwardsConway <$> genHashScriptData
      , TxOutDatumInline BabbageEraOnwardsConway <$> genHashableScriptData
      ]

mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: Gen (Cardano.Api.Hash ScriptData)
genHashScriptData = ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> Gen.int (Range.linear 0 10)

genGovernancePoll :: Gen GovernancePoll
genGovernancePoll =
  GovernancePoll
    <$> Gen.text (Range.linear 1 255) Gen.unicodeAll
    <*> Gen.list (Range.constant 1 10) (Gen.text (Range.linear 1 255) Gen.unicodeAll)
    <*> optional (Gen.word (Range.constant 0 100))

genGovernancePollAnswer :: Gen GovernancePollAnswer
genGovernancePollAnswer =
  GovernancePollAnswer
    <$> genGovernancePollHash
    <*> Gen.word (Range.constant 0 10)
 where
  genGovernancePollHash =
    GovernancePollHash . mkDummyHash <$> Gen.int (Range.linear 0 10)

genProposals :: Applicative (BuildTxWith build)
             => ConwayEraOnwards era
             -> Gen (TxProposalProcedures build era)
genProposals w = conwayEraOnwardsConstraints w $ do
  proposals <- Gen.list (Range.constant 0 10) (genProposal w)
  proposalsToBeWitnessed <- Gen.subsequence proposals
  -- We're generating also some extra proposals, purposely not included in the proposals list, which results
  -- in an invalid state of 'TxProposalProcedures'.
  -- We're doing it for the complete representation of possible values space of TxProposalProcedures.
  -- Proposal procedures code in cardano-api should handle such invalid values just fine.
  extraProposals <- Gen.list (Range.constant 0 10) (genProposal w)
  let sbe = convert w
  proposalsWithWitnesses <-
    forM (extraProposals <> proposalsToBeWitnessed) $ \proposal ->
      (proposal,) <$> genScriptWitnessForStake sbe
  pure $ TxProposalProcedures (fromList proposals) (pure $ fromList proposalsWithWitnesses)

genProposal :: ConwayEraOnwards era -> Gen (L.ProposalProcedure (ShelleyLedgerEra era))
genProposal w =
  conwayEraOnwardsTestConstraints w Q.arbitrary

genVotingProcedures :: Applicative (BuildTxWith build)
                    => ConwayEraOnwards era
                    -> Gen (Api.TxVotingProcedures build era)
genVotingProcedures w = conwayEraOnwardsConstraints w $ do
  voters <- Gen.list (Range.constant 0 10) Q.arbitrary
  let sbe = convert w
  votersWithWitnesses <- fmap fromList . forM voters $ \voter ->
    (voter,) <$> genScriptWitnessForStake sbe
  Api.TxVotingProcedures <$> Q.arbitrary <*> pure (pure votersWithWitnesses)

genCurrentTreasuryValue :: ConwayEraOnwards era -> Gen L.Coin
genCurrentTreasuryValue _era = Q.arbitrary

genTreasuryDonation :: ConwayEraOnwards era -> Gen L.Coin
genTreasuryDonation _era = Q.arbitrary

-- | This generator does not generate a valid witness - just a random one.
genScriptWitnessForStake :: ShelleyBasedEra era -> Gen (Api.ScriptWitness WitCtxStake era)
genScriptWitnessForStake sbe = do
  ScriptInEra scriptLangInEra script' <- genScriptInEra sbe
  case script' of
    SimpleScript simpleScript -> do
      simpleScriptOrReferenceInput <- Gen.choice
        [ pure $ SScript simpleScript
        , SReferenceScript <$> genTxIn
        ]
      pure $ Api.SimpleScriptWitness scriptLangInEra simpleScriptOrReferenceInput
    PlutusScript plutusScriptVersion' plutusScript -> do
      plutusScriptOrReferenceInput <- Gen.choice
        [ pure $ PScript plutusScript
        , PReferenceScript <$> genTxIn
        ]
      scriptRedeemer <- genHashableScriptData
      PlutusScriptWitness
        scriptLangInEra
        plutusScriptVersion'
        plutusScriptOrReferenceInput
        NoScriptDatumForStake
        scriptRedeemer
        <$> genExecutionUnits

genScriptWitnessForMint :: ShelleyBasedEra era -> Gen (Api.ScriptWitness WitCtxMint era)
genScriptWitnessForMint sbe = do
  ScriptInEra scriptLangInEra script' <- genScriptInEra sbe
  case script' of
    SimpleScript simpleScript -> do
      simpleScriptOrReferenceInput <- Gen.choice
        [ pure $ SScript simpleScript
        , SReferenceScript <$> genTxIn
        ]
      pure $ Api.SimpleScriptWitness scriptLangInEra simpleScriptOrReferenceInput
    PlutusScript plutusScriptVersion' plutusScript -> do
      plutusScriptOrReferenceInput <- Gen.choice
        [ pure $ PScript plutusScript
        , PReferenceScript <$> genTxIn
        ]
      scriptRedeemer <- genHashableScriptData
      PlutusScriptWitness
        scriptLangInEra
        plutusScriptVersion'
        plutusScriptOrReferenceInput
        NoScriptDatumForMint
        scriptRedeemer
        <$> genExecutionUnits

