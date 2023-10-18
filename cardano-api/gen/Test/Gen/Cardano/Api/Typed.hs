{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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

  , genAddressByron
  , genAddressInEra
  , genAddressShelley
  , genCertificate
  , genCostModel
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
  , genSimpleScript
  , genPlutusScript
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
  , genTxValidityRange
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
  ) where

import           Cardano.Api hiding (txIns)
import qualified Cardano.Api as Api
import           Cardano.Api.Byron (KeyWitness (ByronKeyWitness),
                   WitnessNetworkIdOrByronAddress (..))
import           Cardano.Api.Governance.Actions.VotingProcedure
import           Cardano.Api.Script (scriptInEraToRefScript)
import           Cardano.Api.Shelley

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hash.Class as CRYPTO
import qualified Cardano.Crypto.Seed as Crypto
import           Cardano.Ledger.Alonzo.Language (Language (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.SafeHash (unsafeMakeSafeHash)

import           Control.Applicative (Alternative (..), optional)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Ratio (Ratio, (%))
import           Data.String
import           Data.Word (Word64)
import           Numeric.Natural (Natural)

import           Test.Gen.Cardano.Api.Era
import           Test.Gen.Cardano.Api.Metadata (genTxMetadata)

import           Test.Cardano.Chain.UTxO.Gen (genVKWitness)
import           Test.Cardano.Crypto.Gen (genProtocolMagicId)
import qualified Test.Cardano.Ledger.Alonzo.PlutusScripts as Plutus
import           Test.Cardano.Ledger.Conway.Arbitrary ()
import           Test.Cardano.Ledger.Core.Arbitrary ()

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Q
import qualified Hedgehog.Range as Range

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use let" -}

genAddressByron :: Gen (Address ByronAddr)
genAddressByron = makeByronAddress <$> genNetworkId
                                   <*> genVerificationKey AsByronKey

genAddressShelley :: Gen (Address ShelleyAddr)
genAddressShelley = makeShelleyAddress <$> genNetworkId
                                       <*> genPaymentCredential
                                       <*> genStakeAddressReference

genAddressInEra :: CardanoEra era -> Gen (AddressInEra era)
genAddressInEra era =
  case cardanoEraStyle era of
    LegacyByronEra ->
      byronAddressInEra <$> genAddressByron

    ShelleyBasedEra sbe ->
      Gen.choice
        [ byronAddressInEra       <$> genAddressByron
        , shelleyAddressInEra sbe <$> genAddressShelley
        ]

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genLovelace :: Gen Lovelace
genLovelace = Lovelace <$> Gen.integral (Range.linear 0 5000)

genPositiveLovelace :: Gen Lovelace
genPositiveLovelace = Lovelace <$> Gen.integral (Range.linear 1 5000)


----------------------------------------------------------------------------
-- SimpleScript generators
--

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

      , do ts <- Gen.list (Range.linear 0 10) genTerm
           m  <- Gen.integral (Range.constant 0 (length ts))
           return (RequireMOf m ts)
      ]

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> Gen.bytes (Range.linear 0 32)

genScriptDataSchema :: Gen ScriptDataJsonSchema
genScriptDataSchema = Gen.element [ScriptDataJsonNoSchema, ScriptDataJsonDetailedSchema]

genHashableScriptData :: Gen HashableScriptData
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
        , ScriptDataBytes  <$> genByteString
        ]
        -- The Gen.recursive combinator calls these with the size halved
        [ ScriptDataConstructor <$> genConstructorInteger
                                <*> genScriptDataList
        , ScriptDataList <$> genScriptDataList
        , ScriptDataMap  <$> genScriptDataMap
        ]
  where
    genInteger :: Gen Integer
    genInteger = Gen.integral
                  (Range.linear
                    (-fromIntegral (maxBound :: Word64) :: Integer)
                    (2 * fromIntegral (maxBound :: Word64) :: Integer))


    genConstructorInteger :: Gen Integer
    genConstructorInteger = Gen.integral
                  (Range.linear
                    0 -- TODO: Alonzo should be -> (-fromIntegral (maxBound :: Word64) :: Integer)
                      -- Wrapping bug needs to be fixed in Plutus library
                    (fromIntegral (maxBound :: Word64) :: Integer))

    genByteString :: Gen ByteString
    genByteString = BS.pack <$> Gen.list (Range.linear 0 64)
                                         (Gen.word8 Range.constantBounded)

    genScriptDataList :: Gen [ScriptData]
    genScriptDataList =
      Gen.sized $ \sz ->
        Gen.list (Range.linear 0 (fromIntegral sz)) genScriptData

    genScriptDataMap  :: Gen [(ScriptData, ScriptData)]
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
      | AnyScriptLanguage lang <- [minBound..maxBound] ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    Gen.choice
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

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
    , (1, AssetName <$> Gen.bytes (Range.singleton  32))
    , (1, AssetName <$> Gen.bytes (Range.constant 1 31))
    ]

genPolicyId :: Gen PolicyId
genPolicyId =
  Gen.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, PolicyId <$> genScriptHash)
    ]

genAssetId :: Gen AssetId
genAssetId = Gen.choice [ AssetId <$> genPolicyId <*> genAssetName
                        , return AdaAssetId
                        ]

genQuantity :: Range Integer -> Gen Quantity
genQuantity range = fromInteger <$> Gen.integral range

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = genQuantity (Range.constantFrom 0 (-2) 2)

-- | Generate a positive or negative, but not zero quantity.
genSignedNonZeroQuantity :: Gen Quantity
genSignedNonZeroQuantity =
  Gen.choice [ genQuantity (Range.constant (-2) (-1))
             , genQuantity (Range.constant 1 2)
             ]

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = genQuantity (Range.constant 0 2)

genPositiveQuantity :: Gen Quantity
genPositiveQuantity = genQuantity (Range.constant 1 2)

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$>
    Gen.list (Range.constant 0 10)
             ((,) <$> genAId <*> genQuant)

-- | Generate a 'Value' with any asset ID and a positive or negative quantity.
genValueDefault :: Gen Value
genValueDefault = genValue genAssetId genSignedNonZeroQuantity

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting = genValue genAssetIdNoAda genSignedNonZeroQuantity
  where
    genAssetIdNoAda :: Gen AssetId
    genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = do
  -- Generate a potentially empty list with multi assets
  val <- genValue genAssetId genPositiveQuantity
  -- Generate at least one positive ADA, without it Value in TxOut makes no sense
  -- and will fail deserialization starting with ConwayEra
  ada <- (,) AdaAssetId <$> genPositiveQuantity
  pure $ valueFromList (ada : valueToList val)


-- Note that we expect to sometimes generate duplicate policy id keys since we
-- pick 90% of policy ids from a set of just three.
genValueNestedRep :: Gen ValueNestedRep
genValueNestedRep =
  ValueNestedRep <$> Gen.list (Range.constant 0 5) genValueNestedBundle

genValueNestedBundle :: Gen ValueNestedBundle
genValueNestedBundle =
  Gen.choice
    [ ValueNestedBundleAda <$> genSignedQuantity
    , ValueNestedBundle <$> genPolicyId
                        <*> Gen.map (Range.constant 0 5)
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

genOperationalCertificateWithCounter :: Gen (OperationalCertificate, OperationalCertificateIssueCounter)
genOperationalCertificateWithCounter = do
    kesVKey <- genVerificationKey AsKesKey
    stkPoolOrGenDelExtSign <- Gen.either (genSigningKey AsStakePoolKey) (genSigningKey AsGenesisDelegateExtendedKey)
    kesP <- genKESPeriod
    c <- Gen.integral $ Range.linear 0 1000
    let stakePoolVer = either getVerificationKey (convert . getVerificationKey) stkPoolOrGenDelExtSign
        iCounter = OperationalCertificateIssueCounter c stakePoolVer

    case issueOperationalCertificate kesVKey stkPoolOrGenDelExtSign kesP iCounter of
      -- This case should be impossible as we clearly derive the verification
      -- key from the generated signing key.
      Left err -> fail $ displayError err
      Right pair -> return pair
  where
    convert :: VerificationKey GenesisDelegateExtendedKey
            -> VerificationKey StakePoolKey
    convert = (castVerificationKey :: VerificationKey GenesisDelegateKey
                                   -> VerificationKey StakePoolKey)
            . (castVerificationKey :: VerificationKey GenesisDelegateExtendedKey
                                   -> VerificationKey GenesisDelegateKey)


-- TODO: Generate payment credential via script
genPaymentCredential :: Gen PaymentCredential
genPaymentCredential = do
  vKey <- genVerificationKey AsPaymentKey
  return . PaymentCredentialByKey $ verificationKeyHash vKey

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
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

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue =
  caseByronToAllegraOrMaryEraOnwards
    (\w -> TxOutAdaOnly w <$> genPositiveLovelace)
    (\w -> TxOutValue w <$> genValueForTxOut)

genTxOutTxContext :: CardanoEra era -> Gen (TxOut CtxTx era)
genTxOutTxContext era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHashTxContext era
        <*> genReferenceScript era

genTxOutUTxOContext :: CardanoEra era -> Gen (TxOut CtxUTxO era)
genTxOutUTxOContext era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHashUTxOContext era
        <*> genReferenceScript era

genReferenceScript :: CardanoEra era -> Gen (ReferenceScript era)
genReferenceScript era =
  caseByronToAlonzoOrBabbageEraOnwards
    (const (return ReferenceScriptNone))
    (const (scriptInEraToRefScript <$> genScriptInEra era))
    era

genUTxO :: CardanoEra era -> Gen (UTxO era)
genUTxO era =
  UTxO <$> Gen.map (Range.constant 0 5) ((,) <$> genTxIn <*> (toCtxUTxOTxOut <$> genTxOutTxContext era))

genTtl :: Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound =
  inEonForEra
    (pure TxValidityNoLowerBound)
    (\w -> TxValidityLowerBound w <$> genTtl)

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  forEraInEon era
    ( forEraInEon era
        (error "genTxValidityUpperBound: unexpected era support combination")
        (pure . TxValidityNoUpperBound)
    )
    (\w -> TxValidityUpperBound w <$> genTtl)

genTxValidityRange
  :: CardanoEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra =
  inEonForEra
    (pure TxMetadataNone)
    (\w ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra w <$> genTxMetadata
        ]
    )

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  forEraInEon era
    (pure TxAuxScriptsNone)
    (\w -> TxAuxScripts w <$> Gen.list (Range.linear 0 3) (genScriptInEra era))

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals =
  inEonForEra
    (pure TxWithdrawalsNone)
    (\w ->
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
    (\w -> do
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
    (\w ->
      StakeAddrRegistrationPreConway w
        <$> genStakeCredential)
    (\w -> StakeAddrRegistrationConway w
        <$> genLovelace
        <*> genStakeCredential)

genTxUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal sbe =
  Gen.choice $ catMaybes
    [ Just $ pure TxUpdateProposalNone
    , forEraInEon sbe Nothing $ \w ->
        Just $ TxUpdateProposal w <$> genUpdateProposal (toCardanoEra w)
    ]

genTxMintValue :: CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue =
  caseByronToAllegraOrMaryEraOnwards
    (const (pure TxMintNone))
    (\supported ->
      Gen.choice
        [ pure TxMintNone
        , TxMintValue supported <$> genValueForMinting <*> return (BuildTxWith mempty)
        ]
    )

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = do
  txIns <- map (, BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> Gen.list (Range.constant 1 10) genTxIn
  txInsCollateral <- genTxInsCollateral era
  txInsReference <- genTxInsReference era
  txOuts <- Gen.list (Range.constant 1 10) (genTxOutTxContext era)
  txTotalCollateral <- genTxTotalCollateral era
  txReturnCollateral <- genTxReturnCollateral era
  txFee <- genTxFee era
  txValidityRange <- genTxValidityRange era
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts era
  let txExtraKeyWits = TxExtraKeyWitnessesNone --TODO: Alonzo era: Generate witness key hashes
  txProtocolParams <- BuildTxWith <$> case cardanoEraStyle era of
                                        LegacyByronEra -> return Nothing
                                        ShelleyBasedEra sbe -> Gen.maybe $ genValidProtocolParameters sbe
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era
  txProposalProcedures <- genMaybeFeaturedInEra genProposals era
  txVotingProcedures <- genMaybeFeaturedInEra genVotingProcedures era
  pure $ TxBodyContent
    { Api.txIns
    , Api.txInsCollateral
    , Api.txInsReference
    , Api.txOuts
    , Api.txTotalCollateral
    , Api.txReturnCollateral
    , Api.txFee
    , Api.txValidityRange
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
    }

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral =
  inEonForEra
    (pure TxInsCollateralNone)
    (\w -> Gen.choice
      [ pure TxInsCollateralNone
      , TxInsCollateral w <$> Gen.list (Range.linear 0 10) genTxIn
      ]
    )

genTxInsReference :: CardanoEra era -> Gen (TxInsReference BuildTx era)
genTxInsReference =
  caseByronToAlonzoOrBabbageEraOnwards
    (const (pure TxInsReferenceNone))
    (\w -> TxInsReference w <$> Gen.list (Range.linear 0 10) genTxIn)

genTxReturnCollateral :: CardanoEra era -> Gen (TxReturnCollateral CtxTx era)
genTxReturnCollateral era =
  forEraInEon era
    (pure TxReturnCollateralNone)
    (\w -> TxReturnCollateral w <$>  genTxOutTxContext era)

genTxTotalCollateral :: CardanoEra era -> Gen (TxTotalCollateral era)
genTxTotalCollateral =
  inEonForEra
    (pure TxTotalCollateralNone)
    (\w -> TxTotalCollateral w <$> genPositiveLovelace)

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee =
  caseByronOrShelleyBasedEra
    (pure . TxFeeImplicit)
    (\w -> TxFeeExplicit w <$> genLovelace)

genTxBody :: CardanoEra era -> Gen (TxBody era)
genTxBody era = do
  res <- Api.createAndValidateTransactionBody era <$> genTxBodyContent era
  case res of
    Left err -> fail (displayError err)
    Right txBody -> pure txBody

-- | Generate a 'Featured' for the given 'CardanoEra' with the provided generator.
genFeaturedInEra :: ()
  => Alternative f
  => eon era
  -> f a
  -> f (Featured eon era a)
genFeaturedInEra witness gen =
  Featured witness <$> gen

-- | Generate a 'Featured' for the given 'CardanoEra' with the provided generator.
genMaybeFeaturedInEra :: ()
  => Eon eon
  => Alternative f
  => (eon era -> f a)
  -> CardanoEra era
  -> f (Maybe (Featured eon era a))
genMaybeFeaturedInEra f =
  inEonForEra (pure Nothing) $ \w ->
    pure Nothing <|> fmap Just (genFeaturedInEra w (f w))

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity =
  inEonForEra
    (pure TxScriptValidityNone)
    (\w -> TxScriptValidity w <$> genScriptValidity)

genScriptValidity :: Gen ScriptValidity
genScriptValidity = Gen.element [ScriptInvalid, ScriptValid]

genTx :: ()
  => CardanoEra era
  -> Gen (Tx era)
genTx era =
  makeSignedTransaction
    <$> genWitnesses era
    <*> genTxBody era

genWitnesses :: CardanoEra era -> Gen [KeyWitness era]
genWitnesses era =
  case cardanoEraStyle era of
    LegacyByronEra -> Gen.list (Range.constant 1 10) genByronKeyWitness
    ShelleyBasedEra sbe -> do
      bsWits  <- Gen.list (Range.constant 0 10) (genShelleyBootstrapWitness sbe)
      keyWits <- Gen.list (Range.constant 0 10) (genShelleyKeyWitness sbe)
      return $ bsWits ++ keyWits

genVerificationKey :: ()
#if MIN_VERSION_base(4,17,0)
    -- GHC 8.10 considers the HasTypeProxy constraint redundant but ghc-9.6 complains if its not
    -- present.
    => HasTypeProxy keyrole
#endif
    => Key keyrole
    => AsType keyrole
    -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: ()
#if MIN_VERSION_base(4,17,0)
    -- GHC 8.10 considers the HasTypeProxy constraint redundant but ghc-9.6 complains if its not
    -- present.
    => HasTypeProxy keyrole
#endif
    => Key keyrole
    => AsType keyrole
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

genShelleyBootstrapWitness :: ()
  => ShelleyBasedEra era
  -> Gen (KeyWitness era)
genShelleyBootstrapWitness sbe =
 makeShelleyBootstrapWitness sbe
   <$> genWitnessNetworkIdOrByronAddress
   <*> genTxBody (shelleyBasedToCardanoEra sbe)
   <*> genSigningKey AsByronKey

genShelleyKeyWitness :: ()
  => ShelleyBasedEra era
  -> Gen (KeyWitness era)
genShelleyKeyWitness sbe =
  makeShelleyKeyWitness sbe
    <$> genTxBody (shelleyBasedToCardanoEra sbe)
    <*> genShelleyWitnessSigningKey

genShelleyWitness :: ()
  => ShelleyBasedEra era
  -> Gen (KeyWitness era)
genShelleyWitness sbe =
  Gen.choice
   [ genShelleyKeyWitness sbe
   , genShelleyBootstrapWitness sbe
   ]

genShelleyWitnessSigningKey :: Gen ShelleyWitnessSigningKey
genShelleyWitnessSigningKey =
  Gen.choice [ WitnessPaymentKey <$>  genSigningKey AsPaymentKey
             , WitnessPaymentExtendedKey <$>  genSigningKey AsPaymentExtendedKey
             , WitnessStakeKey <$>  genSigningKey AsStakeKey
             , WitnessStakePoolKey <$>  genSigningKey AsStakePoolKey
             , WitnessGenesisDelegateKey <$>  genSigningKey AsGenesisDelegateKey
             , WitnessGenesisUTxOKey <$>  genSigningKey AsGenesisUTxOKey
             ]

genCardanoKeyWitness :: ()
  => CardanoEra era
  -> Gen (KeyWitness era)
genCardanoKeyWitness era = case cardanoEraStyle era of
  LegacyByronEra -> genByronKeyWitness
  ShelleyBasedEra sbe -> genShelleyWitness sbe

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genNat :: Gen Natural
genNat = Gen.integral (Range.linear 0 10)

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
  protocolParamPoolRetireMaxEpoch <- genEpochNo
  protocolParamStakePoolTargetNum <- genNat
  protocolParamPoolPledgeInfluence <- genRationalInt64
  protocolParamMonetaryExpansion <- genRational
  protocolParamTreasuryCut <- genRational
  protocolParamUTxOCostPerWord <- inEonForEra @ProtocolUTxOCostPerWordFeature (pure Nothing) (const (Just <$> genLovelace)) era
  protocolParamCostModels <- pure mempty
  --TODO: Babbage figure out how to deal with
  -- asymmetric cost model JSON instances
  protocolParamPrices <- Gen.maybe genExecutionUnitPrices
  protocolParamMaxTxExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxBlockExUnits <- Gen.maybe genExecutionUnits
  protocolParamMaxValueSize <- Gen.maybe genNat
  protocolParamCollateralPercent <- Gen.maybe genNat
  protocolParamMaxCollateralInputs <- Gen.maybe genNat
  protocolParamUTxOCostPerByte <- inEonForEra @ProtocolUTxOCostPerByteFeature (pure Nothing) (const (Just <$> genLovelace)) era

  pure ProtocolParameters {..}

-- | Generate valid protocol parameters which pass validations in Cardano.Api.ProtocolParameters
genValidProtocolParameters :: ShelleyBasedEra era -> Gen (LedgerProtocolParameters era)
genValidProtocolParameters sbe = shelleyBasedEraTestConstraints sbe $ LedgerProtocolParameters <$> Q.arbitrary

genProtocolParametersUpdate :: CardanoEra era -> Gen ProtocolParametersUpdate
genProtocolParametersUpdate era = do
  protocolUpdateProtocolVersion     <- Gen.maybe ((,) <$> genNat <*> genNat)
  protocolUpdateDecentralization    <- Gen.maybe genRational
  protocolUpdateExtraPraosEntropy   <- Gen.maybe genMaybePraosNonce
  protocolUpdateMaxBlockHeaderSize  <- Gen.maybe genNat
  protocolUpdateMaxBlockBodySize    <- Gen.maybe genNat
  protocolUpdateMaxTxSize           <- Gen.maybe genNat
  protocolUpdateTxFeeFixed          <- Gen.maybe genLovelace
  protocolUpdateTxFeePerByte        <- Gen.maybe genLovelace
  protocolUpdateMinUTxOValue        <- Gen.maybe genLovelace
  protocolUpdateStakeAddressDeposit <- Gen.maybe genLovelace
  protocolUpdateStakePoolDeposit    <- Gen.maybe genLovelace
  protocolUpdateMinPoolCost         <- Gen.maybe genLovelace
  protocolUpdatePoolRetireMaxEpoch  <- Gen.maybe genEpochNo
  protocolUpdateStakePoolTargetNum  <- Gen.maybe genNat
  protocolUpdatePoolPledgeInfluence <- Gen.maybe genRationalInt64
  protocolUpdateMonetaryExpansion   <- Gen.maybe genRational
  protocolUpdateTreasuryCut         <- Gen.maybe genRational
  protocolUpdateUTxOCostPerWord     <- inEonForEra @ProtocolUTxOCostPerWordFeature (pure Nothing) (const (Just <$> genLovelace)) era
  let protocolUpdateCostModels = mempty -- genCostModels
  --TODO: Babbage figure out how to deal with
  -- asymmetric cost model JSON instances
  protocolUpdatePrices              <- Gen.maybe genExecutionUnitPrices
  protocolUpdateMaxTxExUnits        <- Gen.maybe genExecutionUnits
  protocolUpdateMaxBlockExUnits     <- Gen.maybe genExecutionUnits
  protocolUpdateMaxValueSize        <- Gen.maybe genNat
  protocolUpdateCollateralPercent   <- Gen.maybe genNat
  protocolUpdateMaxCollateralInputs <- Gen.maybe genNat
  protocolUpdateUTxOCostPerByte     <- inEonForEra @ProtocolUTxOCostPerByteFeature (pure Nothing) (const (Just <$> genLovelace)) era

  pure ProtocolParametersUpdate{..}


genUpdateProposal :: CardanoEra era -> Gen UpdateProposal
genUpdateProposal era =
  UpdateProposal
    <$> Gen.map (Range.constant 1 3)
        ( (,)
          <$> genVerificationKeyHash AsGenesisKey
          <*> genProtocolParametersUpdate era
        )
    <*> genEpochNo

genCostModel :: Gen Alonzo.CostModel
genCostModel = do
  let costModelParams = Alonzo.getCostModelParams Plutus.testingCostModelV1
  eCostModel <- Alonzo.mkCostModel <$> genPlutusLanguage
                                   <*> mapM (const $ Gen.integral (Range.linear 0 5000)) costModelParams
  case eCostModel of
    Left err -> error $ "genCostModel: " <> show err
    Right cModel -> return cModel

genPlutusLanguage :: Gen Language
genPlutusLanguage = Gen.element [PlutusV1, PlutusV2]

_genCostModels :: Gen (Map AnyPlutusScriptVersion CostModel)
_genCostModels =
    Gen.map (Range.linear 0 (length plutusScriptVersions))
            ((,) <$> Gen.element plutusScriptVersions
                 <*> (Api.fromAlonzoCostModel <$> genCostModel))
  where
    plutusScriptVersions :: [AnyPlutusScriptVersion]
    plutusScriptVersions = [minBound..maxBound]

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                   <*> Gen.integral (Range.constant 0 1000)

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genTxOutDatumHashTxContext :: CardanoEra era -> Gen (TxOutDatum CtxTx era)
genTxOutDatumHashTxContext era = case era of
    ByronEra   -> pure TxOutDatumNone
    ShelleyEra -> pure TxOutDatumNone
    AllegraEra -> pure TxOutDatumNone
    MaryEra    -> pure TxOutDatumNone
    AlonzoEra  -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash AlonzoEraOnwardsAlonzo <$> genHashScriptData
                    , TxOutDatumInTx AlonzoEraOnwardsAlonzo <$> genHashableScriptData
                    ]
    BabbageEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash AlonzoEraOnwardsBabbage <$> genHashScriptData
                    , TxOutDatumInTx AlonzoEraOnwardsBabbage <$> genHashableScriptData
                    , TxOutDatumInline BabbageEraOnwardsBabbage <$> genHashableScriptData
                    ]
    ConwayEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash AlonzoEraOnwardsConway <$> genHashScriptData
                    , TxOutDatumInTx AlonzoEraOnwardsConway <$> genHashableScriptData
                    , TxOutDatumInline BabbageEraOnwardsConway <$> genHashableScriptData
                    ]

genTxOutDatumHashUTxOContext :: CardanoEra era -> Gen (TxOutDatum CtxUTxO era)
genTxOutDatumHashUTxOContext era = case era of
    ByronEra   -> pure TxOutDatumNone
    ShelleyEra -> pure TxOutDatumNone
    AllegraEra -> pure TxOutDatumNone
    MaryEra    -> pure TxOutDatumNone
    AlonzoEra  -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash AlonzoEraOnwardsAlonzo <$> genHashScriptData
                    ]
    BabbageEra -> Gen.choice
                    [ pure TxOutDatumNone
                    , TxOutDatumHash AlonzoEraOnwardsBabbage <$> genHashScriptData
                    , TxOutDatumInline BabbageEraOnwardsBabbage <$> genHashableScriptData
                    ]
    ConwayEra -> Gen.choice
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

genProposals :: forall era. ConwayEraOnwards era -> Gen [Proposal era]
genProposals w =
  conwayEraOnwardsTestConstraints w
    $ Gen.list (Range.constant 1 10)
    $ genProposal w

genProposal :: ConwayEraOnwards era -> Gen (Proposal era)
genProposal w =
  conwayEraOnwardsTestConstraints w $ fmap Proposal Q.arbitrary

genVotingProcedures :: ConwayEraOnwards era -> Gen (VotingProcedures era)
genVotingProcedures w =
  conwayEraOnwardsConstraints w
    $ VotingProcedures <$> Q.arbitrary
