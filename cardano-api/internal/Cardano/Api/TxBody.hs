{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- HLINT ignore "Avoid lambda using `infix`" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use section" -}

-- | Transaction bodies
--
module Cardano.Api.TxBody (
    parseTxId,
    -- * Transaction bodies
    TxBody(.., TxBody),
    createTransactionBody,
    createAndValidateTransactionBody,
    TxBodyContent(..),
    -- ** Transaction body builders
    defaultTxBodyContent,
    defaultTxFee,
    defaultTxValidityUpperBound,
    setTxIns,
    modTxIns,
    addTxIn,
    setTxInsCollateral,
    setTxInsReference,
    setTxOuts,
    modTxOuts,
    addTxOut,
    setTxTotalCollateral,
    setTxReturnCollateral,
    setTxFee,
    setTxValidityRange,
    setTxMetadata,
    setTxAuxScripts,
    setTxExtraKeyWits,
    setTxProtocolParams,
    setTxWithdrawals,
    setTxCertificates,
    setTxUpdateProposal,
    setTxMintValue,
    setTxScriptValidity,
    TxBodyError(..),
    TxBodyScriptData(..),
    TxScriptValidity(..),
    TxScriptValiditySupportedInEra(..),

    ScriptValidity(..),
    scriptValidityToIsValid,
    isValidToScriptValidity,
    scriptValidityToTxScriptValidity,
    txScriptValidityToIsValid,
    txScriptValidityToScriptValidity,

    -- * Transaction Ids
    TxId(..),
    getTxId,
    getTxIdShelley,

    -- * Transaction inputs
    TxIn(..),
    TxIns,
    TxIx(..),
    genesisUTxOPseudoTxIn,

    -- * Transaction outputs
    CtxTx, CtxUTxO,
    TxOut(..),
    TxOutValue(..),
    TxOutDatum(TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline),
    toCtxUTxOTxOut,
    lovelaceToTxOutValue,
    prettyRenderTxOut,
    txOutValueToLovelace,
    txOutValueToValue,
    parseHash,
    TxOutInAnyEra(..),
    txOutInAnyEra,

    -- * Other transaction body types
    TxInsCollateral(..),
    TxInsReference(..),
    TxReturnCollateral(..),
    TxTotalCollateral(..),
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxExtraKeyWitnesses(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),

    -- ** Building vs viewing transactions
    BuildTxWith(..),
    BuildTx,
    ViewTx,

    -- * Era-dependent transaction body features
    CollateralSupportedInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    AuxScriptsSupportedInEra(..),

    -- ** Feature availability functions
    collateralSupportedInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    auxScriptsSupportedInEra,
    txScriptValiditySupportedInShelleyBasedEra,
    txScriptValiditySupportedInCardanoEra,

    -- * Inspecting 'ScriptWitness'es
    AnyScriptWitness(..),
    ScriptWitnessIndex(..),
    renderScriptWitnessIndex,
    collectTxBodyScriptWitnesses,

    -- * Conversion to inline data
    scriptDataToInlineDatum,

    -- * Internal conversion functions & types
    toByronTxId,
    toShelleyTxId,
    toShelleyTxIn,
    toShelleyTxOut,
    toShelleyTxOutAny,
    fromShelleyTxId,
    fromShelleyTxIn,
    fromShelleyTxOut,
    toAlonzoRdmrPtr,
    fromAlonzoRdmrPtr,
    fromByronTxIn,
    fromLedgerTxOuts,
    renderTxIn,

    -- * Misc helpers
    calculateExecutionUnitsLovelace,
    orderStakeAddrs,
    orderTxIns,

    -- * Data family instances
    AsType(AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody, AsMaryTxBody),

    getTxBodyContent,
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.AllegraEraOnwards
import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ByronEraOnly
import           Cardano.Api.Eon.ByronToAllegraEra
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.EraCast
import           Cardano.Api.Eras.Case
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Eras.Core
import           Cardano.Api.Error
import           Cardano.Api.Feature
import           Cardano.Api.Governance.Actions.ProposalProcedure
import           Cardano.Api.Governance.Actions.VotingProcedure
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.Script
import           Cardano.Api.ScriptData
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxIn
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value
import           Cardano.Api.ValueParser

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import           Cardano.Ledger.Binary (Annotated (..), reAnnotate, recoverBytes)
import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Conway.Core as L
import           Cardano.Ledger.Core ()
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley
import           Cardano.Ledger.Mary.Value as L (MaryValue (..), MultiAsset)
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import qualified Cardano.Ledger.Shelley.TxCert as Shelley
import qualified Cardano.Ledger.TxIn as L
import           Cardano.Ledger.Val as L (isZero)
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardAlonzo, StandardBabbage,
                   StandardConway, StandardMary, StandardShelley)

import           Control.Applicative (some)
import           Control.Monad (guard, unless)
import           Data.Aeson (object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (for_, toList)
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.List (intercalate, sortBy)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, maybeToList)
import           Data.Scientific (toBoundedInteger)
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Word (Word16, Word32, Word64)
import           GHC.Generics
import           Lens.Micro hiding (ix)
import           Lens.Micro.Extras (view)
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>))
import qualified Text.Parsec.String as Parsec


-- | Indicates whether a script is expected to fail or pass validation.
data ScriptValidity
  = ScriptInvalid -- ^ Script is expected to fail validation.
                  -- Transactions marked as such can include scripts that fail validation.
                  -- Such transactions may be submitted to the chain, in which case the
                  -- collateral will be taken upon on chain script validation failure.

  | ScriptValid   -- ^ Script is expected to pass validation.
                  -- Transactions marked as such cannot include scripts that fail validation.

  deriving (Eq, Show)

instance CBOR.EncCBOR ScriptValidity where
  encCBOR = CBOR.encCBOR . scriptValidityToIsValid

instance CBOR.DecCBOR ScriptValidity where
  decCBOR = isValidToScriptValidity <$> CBOR.decCBOR

scriptValidityToIsValid :: ScriptValidity -> L.IsValid
scriptValidityToIsValid ScriptInvalid = L.IsValid False
scriptValidityToIsValid ScriptValid = L.IsValid True

isValidToScriptValidity :: L.IsValid -> ScriptValidity
isValidToScriptValidity (L.IsValid False) = ScriptInvalid
isValidToScriptValidity (L.IsValid True) = ScriptValid

-- | A representation of whether the era supports tx script validity.
--
-- The Alonzo and subsequent eras support script validity.
--
data TxScriptValidity era where
  TxScriptValidityNone :: TxScriptValidity era

  -- | Tx script validity is supported in transactions in the 'Alonzo' era onwards.
  TxScriptValidity
    :: TxScriptValiditySupportedInEra era
    -> ScriptValidity
    -> TxScriptValidity era

deriving instance Eq   (TxScriptValiditySupportedInEra era)
deriving instance Show (TxScriptValiditySupportedInEra era)

data TxScriptValiditySupportedInEra era where
  TxScriptValiditySupportedInAlonzoEra  :: TxScriptValiditySupportedInEra AlonzoEra
  TxScriptValiditySupportedInBabbageEra :: TxScriptValiditySupportedInEra BabbageEra
  TxScriptValiditySupportedInConwayEra  :: TxScriptValiditySupportedInEra ConwayEra

deriving instance Eq   (TxScriptValidity era)
deriving instance Show (TxScriptValidity era)

txScriptValiditySupportedInCardanoEra :: CardanoEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInCardanoEra ByronEra   = Nothing
txScriptValiditySupportedInCardanoEra ShelleyEra = Nothing
txScriptValiditySupportedInCardanoEra AllegraEra = Nothing
txScriptValiditySupportedInCardanoEra MaryEra    = Nothing
txScriptValiditySupportedInCardanoEra AlonzoEra  = Just TxScriptValiditySupportedInAlonzoEra
txScriptValiditySupportedInCardanoEra BabbageEra = Just TxScriptValiditySupportedInBabbageEra
txScriptValiditySupportedInCardanoEra ConwayEra = Just TxScriptValiditySupportedInConwayEra

txScriptValiditySupportedInShelleyBasedEra :: ShelleyBasedEra era -> Maybe (TxScriptValiditySupportedInEra era)
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraShelley = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraAllegra = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraMary    = Nothing
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraAlonzo  = Just TxScriptValiditySupportedInAlonzoEra
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraBabbage = Just TxScriptValiditySupportedInBabbageEra
txScriptValiditySupportedInShelleyBasedEra ShelleyBasedEraConway = Just TxScriptValiditySupportedInConwayEra

txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
txScriptValidityToScriptValidity TxScriptValidityNone = ScriptValid
txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity

scriptValidityToTxScriptValidity :: ShelleyBasedEra era -> ScriptValidity -> TxScriptValidity era
scriptValidityToTxScriptValidity sbe scriptValidity = case txScriptValiditySupportedInShelleyBasedEra sbe of
  Nothing -> TxScriptValidityNone
  Just witness -> TxScriptValidity witness scriptValidity

txScriptValidityToIsValid :: TxScriptValidity era -> L.IsValid
txScriptValidityToIsValid = scriptValidityToIsValid . txScriptValidityToScriptValidity

-- ----------------------------------------------------------------------------
-- Transaction outputs
--

-- | The context is a transaction body
data CtxTx
-- | The context is the UTxO
data CtxUTxO

data TxOut ctx era = TxOut (AddressInEra    era)
                           (TxOutValue      era)
                           (TxOutDatum ctx  era)
                           (ReferenceScript era)

deriving instance Eq   (TxOut ctx era)
deriving instance Show (TxOut ctx era)

instance EraCast (TxOut ctx) where
  eraCast toEra (TxOut addressInEra txOutValue txOutDatum referenceScript) =
    TxOut
      <$> eraCast toEra addressInEra
      <*> eraCast toEra txOutValue
      <*> eraCast toEra txOutDatum
      <*> eraCast toEra referenceScript

data TxOutInAnyEra where
     TxOutInAnyEra :: CardanoEra era
                   -> TxOut CtxTx era
                   -> TxOutInAnyEra

deriving instance Show TxOutInAnyEra

instance Eq TxOutInAnyEra where
  TxOutInAnyEra era1 out1 == TxOutInAnyEra era2 out2 =
    case testEquality era1 era2 of
      Just Refl -> out1 == out2
      Nothing   -> False

-- | Convenience constructor for 'TxOutInAnyEra'
txOutInAnyEra :: IsCardanoEra era => TxOut CtxTx era -> TxOutInAnyEra
txOutInAnyEra = TxOutInAnyEra cardanoEra

toCtxUTxOTxOut :: TxOut CtxTx  era -> TxOut CtxUTxO era
toCtxUTxOTxOut (TxOut addr val d refS) =
  let dat = case d of
              TxOutDatumNone -> TxOutDatumNone
              TxOutDatumHash s h -> TxOutDatumHash s h
              TxOutDatumInTx' s h _ -> TxOutDatumHash s h
              TxOutDatumInline s sd -> TxOutDatumInline s sd
  in TxOut addr val dat refS

instance IsCardanoEra era => ToJSON (TxOut ctx era) where
  toJSON  = txOutToJsonValue cardanoEra

txOutToJsonValue :: CardanoEra era -> TxOut ctx era -> Aeson.Value
txOutToJsonValue era (TxOut addr val dat refScript) =
  case era of
    ByronEra -> object ["address" .= addr, "value" .= val]
    ShelleyEra -> object ["address" .= addr, "value" .= val]
    AllegraEra -> object ["address" .= addr, "value" .= val]
    MaryEra -> object ["address" .= addr, "value" .= val]
    AlonzoEra -> object
                   [ "address" .= addr
                   , "value" .= val
                   , datHashJsonVal dat
                   , "datum" .= datJsonVal dat
                   ]
    BabbageEra ->
      object
        [ "address" .= addr
        , "value" .= val
        , datHashJsonVal dat
        , "datum" .= datJsonVal dat
        , "inlineDatum" .= inlineDatumJsonVal dat
        , "referenceScript" .= refScriptJsonVal refScript
        ]
    ConwayEra ->
      object
        [ "address" .= addr
        , "value" .= val
        , datHashJsonVal dat
        , "datum" .= datJsonVal dat
        , "inlineDatum" .= inlineDatumJsonVal dat
        , "referenceScript" .= refScriptJsonVal refScript
        ]
 where
   datHashJsonVal :: TxOutDatum ctx era -> Aeson.Pair
   datHashJsonVal d =
     case d of
       TxOutDatumNone ->
         "datumhash" .= Aeson.Null
       TxOutDatumHash _ h ->
         "datumhash" .= toJSON h
       TxOutDatumInTx' _ h _ ->
         "datumhash" .= toJSON h
       TxOutDatumInline _ datum ->
         "inlineDatumhash"  .= toJSON (hashScriptDataBytes datum)

   datJsonVal :: TxOutDatum ctx era -> Aeson.Value
   datJsonVal d =
     case d of
       TxOutDatumNone -> Aeson.Null
       TxOutDatumHash _ _ -> Aeson.Null
       TxOutDatumInTx' _ _ datum -> scriptDataToJson ScriptDataJsonDetailedSchema datum
       TxOutDatumInline _ _ -> Aeson.Null

   inlineDatumJsonVal :: TxOutDatum ctx era -> Aeson.Value
   inlineDatumJsonVal d =
     case d of
       TxOutDatumNone -> Aeson.Null
       TxOutDatumHash {} -> Aeson.Null
       TxOutDatumInTx'{} -> Aeson.Null
       TxOutDatumInline _ datum -> scriptDataToJson ScriptDataJsonDetailedSchema datum

   refScriptJsonVal :: ReferenceScript era -> Aeson.Value
   refScriptJsonVal rScript =
     case rScript of
       ReferenceScript _ s -> toJSON s
       ReferenceScriptNone -> Aeson.Null

instance IsShelleyBasedEra era => FromJSON (TxOut CtxTx era) where
      parseJSON = withObject "TxOut" $ \o -> do
        case shelleyBasedEra :: ShelleyBasedEra era of
          ShelleyBasedEraShelley ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraMary ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAllegra ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAlonzo -> alonzoTxOutParser AlonzoEraOnwardsAlonzo o

          ShelleyBasedEraBabbage -> do
            alonzoTxOutInBabbage <- alonzoTxOutParser AlonzoEraOnwardsBabbage o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) -> do
                  case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right hashableData -> do
                      if hashScriptDataBytes hashableData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline BabbageEraOnwardsBabbage hashableData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            mReferenceScript <- o .:? "referenceScript"

            reconcileBabbage alonzoTxOutInBabbage mInlineDatum mReferenceScript

          ShelleyBasedEraConway -> do
            alonzoTxOutInConway <- alonzoTxOutParser AlonzoEraOnwardsConway o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) ->
                  case scriptDataFromJson ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right sData ->
                      if hashScriptDataBytes sData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline BabbageEraOnwardsConway sData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            mReferenceScript <- o .:? "referenceScript"

            reconcileConway alonzoTxOutInConway mInlineDatum mReferenceScript
         where
           reconcileBabbage
             :: TxOut CtxTx BabbageEra -- ^ Alonzo era datum in Babbage era
             -> TxOutDatum CtxTx BabbageEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxTx BabbageEra)
           reconcileBabbage top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (alonzoDat, babbageDat) ->
                             fail $ "Parsed an Alonzo era datum and a Babbage era datum " <>
                                    "TxOut: " <> show top <>
                                    "Alonzo datum: " <> show alonzoDat <>
                                    "Babbage dat: " <> show babbageDat
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript BabbageEraOnwardsBabbage anyScript
             return $ TxOut addr v finalDat finalRefScript

           reconcileConway
             :: TxOut CtxTx ConwayEra -- ^ Alonzo era datum in Conway era
             -> TxOutDatum CtxTx ConwayEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxTx ConwayEra)
           reconcileConway top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (alonzoDat, babbageDat) ->
                             fail $ "Parsed an Alonzo era datum and a Conway era datum " <>
                                    "TxOut: " <> show top <>
                                    "Alonzo datum: " <> show alonzoDat <>
                                    "Conway dat: " <> show babbageDat
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript BabbageEraOnwardsConway anyScript
             return $ TxOut addr v finalDat finalRefScript

           alonzoTxOutParser
             :: AlonzoEraOnwards era -> Aeson.Object -> Aeson.Parser (TxOut CtxTx era)
           alonzoTxOutParser w o = do
            mDatumHash <- o .:? "datumhash"
            mDatumVal <- o .:? "datum"
            case (mDatumVal, mDatumHash) of
               (Nothing,Nothing) -> TxOut <$> o .: "address"
                                          <*> o .: "value"
                                          <*> return TxOutDatumNone
                                          <*> return ReferenceScriptNone
               (Just dVal, Just dHash) -> do
                 case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
                   Left e -> fail $ "Error parsing ScriptData: " <> show e
                   Right hashableData ->
                      TxOut <$> o .: "address"
                            <*> o .: "value"
                            <*> return (TxOutDatumInTx' w dHash hashableData)
                            <*> return ReferenceScriptNone
               (Nothing, Just dHash) ->
                 TxOut <$> o .: "address"
                       <*> o .: "value"
                       <*> return (TxOutDatumHash w dHash)
                       <*> return ReferenceScriptNone
               (Just _dVal, Nothing) -> fail "Only datum JSON was found, this should not be possible."

instance IsShelleyBasedEra era => FromJSON (TxOut CtxUTxO era) where
      parseJSON = withObject "TxOut" $ \o -> do
        case shelleyBasedEra :: ShelleyBasedEra era of
          ShelleyBasedEraShelley ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraMary ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAllegra ->
            TxOut <$> o .: "address"
                  <*> o .: "value"
                  <*> return TxOutDatumNone
                  <*> return ReferenceScriptNone
          ShelleyBasedEraAlonzo -> alonzoTxOutParser AlonzoEraOnwardsAlonzo o

          ShelleyBasedEraBabbage -> do
            alonzoTxOutInBabbage <- alonzoTxOutParser AlonzoEraOnwardsBabbage o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) -> do
                     case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
                        Left err ->
                          fail $ "Error parsing TxOut JSON: " <> displayError err
                        Right hashableData -> do
                          if hashScriptDataBytes hashableData /= h
                          then fail "Inline datum not equivalent to inline datum hash"
                          else return $ TxOutDatumInline BabbageEraOnwardsBabbage hashableData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            -- We check for a reference script
            mReferenceScript <- o .:? "referenceScript"

            reconcileBabbage alonzoTxOutInBabbage mInlineDatum mReferenceScript

          ShelleyBasedEraConway -> do
            alonzoTxOutInConway <- alonzoTxOutParser AlonzoEraOnwardsConway o

            -- We check for the existence of inline datums
            inlineDatumHash <- o .:? "inlineDatumhash"
            inlineDatum <- o .:? "inlineDatum"
            mInlineDatum <-
              case (inlineDatum, inlineDatumHash) of
                (Just dVal, Just h) ->
                  case scriptDataFromJson ScriptDataJsonDetailedSchema dVal of
                    Left err ->
                      fail $ "Error parsing TxOut JSON: " <> displayError err
                    Right sData ->
                      if hashScriptDataBytes sData /= h
                      then fail "Inline datum not equivalent to inline datum hash"
                      else return $ TxOutDatumInline BabbageEraOnwardsConway sData
                (Nothing, Nothing) -> return TxOutDatumNone
                (_,_) -> fail "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

            -- We check for a reference script
            mReferenceScript <- o .:? "referenceScript"

            reconcileConway alonzoTxOutInConway mInlineDatum mReferenceScript
         where
           reconcileBabbage
             :: TxOut CtxUTxO BabbageEra -- ^ Alonzo era datum in Babbage era
             -> TxOutDatum CtxUTxO BabbageEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxUTxO BabbageEra)
           reconcileBabbage (TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (_,_) -> fail "Parsed an Alonzo era datum and a Babbage era datum"
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript BabbageEraOnwardsBabbage anyScript

             return $ TxOut addr v finalDat finalRefScript

           reconcileConway
             :: TxOut CtxUTxO ConwayEra -- ^ Alonzo era datum in Conway era
             -> TxOutDatum CtxUTxO ConwayEra -- ^ Babbage inline datum
             -> Maybe ScriptInAnyLang
             -> Aeson.Parser (TxOut CtxUTxO ConwayEra)
           reconcileConway (TxOut addr v dat r) babbageDatum mBabRefScript = do
             -- We check for conflicting datums
             finalDat <- case (dat, babbageDatum) of
                           (TxOutDatumNone, bDatum) -> return bDatum
                           (anyDat, TxOutDatumNone) -> return anyDat
                           (_,_) -> fail "Parsed an Alonzo era datum and a Conway era datum"
             finalRefScript <- case mBabRefScript of
                                 Nothing -> return r
                                 Just anyScript ->
                                   return $ ReferenceScript BabbageEraOnwardsConway anyScript

             return $ TxOut addr v finalDat finalRefScript

           alonzoTxOutParser :: AlonzoEraOnwards era -> Aeson.Object -> Aeson.Parser (TxOut CtxUTxO era)
           alonzoTxOutParser w o = do
            mDatumHash <- o .:? "datumhash"
            case mDatumHash of
               Nothing -> TxOut <$> o .: "address"
                                          <*> o .: "value"
                                          <*> return TxOutDatumNone
                                          <*> return ReferenceScriptNone
               Just dHash ->
                 TxOut <$> o .: "address"
                        <*> o .: "value"
                        <*> return (TxOutDatumHash w dHash)
                        <*> return ReferenceScriptNone

fromByronTxOut :: Byron.TxOut -> TxOut ctx ByronEra
fromByronTxOut (Byron.TxOut addr value) =
  TxOut
    (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
    (TxOutAdaOnly ByronToAllegraEraByron (fromByronLovelace value))
     TxOutDatumNone ReferenceScriptNone


toByronTxOut :: TxOut ctx ByronEra -> Maybe Byron.TxOut
toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr))
                    (TxOutAdaOnly ByronToAllegraEraByron value) _ _) =
    Byron.TxOut addr <$> toByronLovelace value

toByronTxOut (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
                    (TxOutValue w _) _ _) = case w of {}

toByronTxOut (TxOut (AddressInEra (ShelleyAddressInEra sbe) ShelleyAddress{})
                    _ _ _) = case sbe of {}


toShelleyTxOut :: forall era ledgerera.
                  ShelleyLedgerEra era ~ ledgerera
               => ShelleyBasedEra era
               -> TxOut CtxUTxO era
               -> Ledger.TxOut ledgerera
toShelleyTxOut sbe (TxOut _ (TxOutAdaOnly ByronToAllegraEraByron _) _ _) =
    case sbe of {}

toShelleyTxOut _ (TxOut addr (TxOutAdaOnly ByronToAllegraEraShelley value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutAdaOnly ByronToAllegraEraAllegra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOut _ (TxOut addr (TxOutValue MaryEraOnwardsMary value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOut _ (TxOut addr (TxOutValue MaryEraOnwardsAlonzo value) txoutdata _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
    & L.dataHashTxOutL .~ toAlonzoTxOutDataHash txoutdata

toShelleyTxOut sbe (TxOut addr (TxOutValue MaryEraOnwardsBabbage value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra sbe
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

toShelleyTxOut sbe (TxOut addr (TxOutValue MaryEraOnwardsConway value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra sbe
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

fromShelleyTxOut :: forall era ledgerera ctx. ()
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> Core.TxOut ledgerera
  -> TxOut ctx era
fromShelleyTxOut sbe ledgerTxOut = do
  let txOutValue :: TxOutValue era
      txOutValue =
        caseShelleyToAllegraOrMaryEraOnwards
          (\w -> TxOutAdaOnly (shelleyToAllegraEraToByronToAllegraEra w) (fromShelleyLovelace (ledgerTxOut ^. L.valueTxOutL)))
          (\w -> TxOutValue w (fromMaryValue (ledgerTxOut ^. L.valueTxOutL)))
          sbe

  let addressInEra :: AddressInEra era
      addressInEra = shelleyBasedEraConstraints sbe $ fromShelleyAddr sbe $ ledgerTxOut ^. L.addrTxOutL

  case sbe of
    ShelleyBasedEraShelley ->
      TxOut addressInEra txOutValue TxOutDatumNone ReferenceScriptNone

    ShelleyBasedEraAllegra ->
      TxOut addressInEra txOutValue TxOutDatumNone ReferenceScriptNone

    ShelleyBasedEraMary ->
      TxOut addressInEra txOutValue TxOutDatumNone ReferenceScriptNone

    ShelleyBasedEraAlonzo ->
       TxOut addressInEra
             txOutValue
             (fromAlonzoTxOutDataHash AlonzoEraOnwardsAlonzo datahash)
             ReferenceScriptNone
      where
        datahash = ledgerTxOut ^. L.dataHashTxOutL

    ShelleyBasedEraBabbage ->
       TxOut addressInEra
             txOutValue
             (fromBabbageTxOutDatum
               AlonzoEraOnwardsBabbage
               BabbageEraOnwardsBabbage
               datum)
             (case mRefScript of
                SNothing -> ReferenceScriptNone
                SJust refScript ->
                  fromShelleyScriptToReferenceScript ShelleyBasedEraBabbage refScript)
      where
        datum = ledgerTxOut ^. L.datumTxOutL
        mRefScript = ledgerTxOut ^. L.referenceScriptTxOutL

    ShelleyBasedEraConway ->
       TxOut addressInEra
             txOutValue
             (fromBabbageTxOutDatum
               AlonzoEraOnwardsConway
               BabbageEraOnwardsConway
               datum)
             (case mRefScript of
                SNothing -> ReferenceScriptNone
                SJust refScript ->
                  fromShelleyScriptToReferenceScript ShelleyBasedEraConway refScript)
      where
        datum = ledgerTxOut ^. L.datumTxOutL
        mRefScript = ledgerTxOut ^. L.referenceScriptTxOutL


-- TODO: If ledger creates an open type family for datums
-- we can consolidate this function with the Babbage version
toAlonzoTxOutDataHash
  :: TxOutDatum CtxUTxO AlonzoEra
  -> StrictMaybe (L.DataHash StandardCrypto)
toAlonzoTxOutDataHash  TxOutDatumNone                        = SNothing
toAlonzoTxOutDataHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDataHash (TxOutDatumInline inlineDatumSupp _sd) =
  case inlineDatumSupp :: BabbageEraOnwards AlonzoEra of {}

fromAlonzoTxOutDataHash :: AlonzoEraOnwards era
                        -> StrictMaybe (L.DataHash StandardCrypto)
                        -> TxOutDatum ctx era
fromAlonzoTxOutDataHash _    SNothing  = TxOutDatumNone
fromAlonzoTxOutDataHash s (SJust dh)   = TxOutDatumHash s (ScriptDataHash dh)

-- TODO: If ledger creates an open type family for datums
-- we can consolidate this function with the Alonzo version
toBabbageTxOutDatum
  :: (L.Era (ShelleyLedgerEra era), Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => TxOutDatum CtxUTxO era -> Babbage.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum  TxOutDatumNone = Babbage.NoDatum
toBabbageTxOutDatum (TxOutDatumHash _ (ScriptDataHash dh)) = Babbage.DatumHash dh
toBabbageTxOutDatum (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

fromBabbageTxOutDatum
  :: (L.Era ledgerera, Ledger.EraCrypto ledgerera ~ StandardCrypto)
  => AlonzoEraOnwards era
  -> BabbageEraOnwards era
  -> Babbage.Datum ledgerera
  -> TxOutDatum ctx era
fromBabbageTxOutDatum _ _ Babbage.NoDatum = TxOutDatumNone
fromBabbageTxOutDatum w _ (Babbage.DatumHash dh) =
  TxOutDatumHash w $ ScriptDataHash dh
fromBabbageTxOutDatum _ w (Babbage.Datum binData) =
  TxOutDatumInline w $ binaryDataToScriptData w binData



-- ----------------------------------------------------------------------------
-- Era-dependent transaction body features
--

-- | A representation of whether the era supports transactions with inputs used
-- only for collateral for script fees.
--
-- The Alonzo and subsequent eras support collateral inputs.
--
data CollateralSupportedInEra era where

     CollateralInAlonzoEra  :: CollateralSupportedInEra AlonzoEra
     CollateralInBabbageEra :: CollateralSupportedInEra BabbageEra
     CollateralInConwayEra  :: CollateralSupportedInEra ConwayEra

deriving instance Eq   (CollateralSupportedInEra era)
deriving instance Show (CollateralSupportedInEra era)

collateralSupportedInEra :: CardanoEra era
                         -> Maybe (CollateralSupportedInEra era)
collateralSupportedInEra ByronEra   = Nothing
collateralSupportedInEra ShelleyEra = Nothing
collateralSupportedInEra AllegraEra = Nothing
collateralSupportedInEra MaryEra    = Nothing
collateralSupportedInEra AlonzoEra  = Just CollateralInAlonzoEra
collateralSupportedInEra BabbageEra = Just CollateralInBabbageEra
collateralSupportedInEra ConwayEra = Just CollateralInConwayEra

-- | A representation of whether the era supports transactions with an upper
-- bound on the range of slots in which they are valid.
--
-- The Shelley and subsequent eras support an upper bound on the validity
-- range. In the Shelley era specifically it is actually required. It is
-- optional in later eras.
--
data ValidityUpperBoundSupportedInEra era where

     ValidityUpperBoundInShelleyEra :: ValidityUpperBoundSupportedInEra ShelleyEra
     ValidityUpperBoundInAllegraEra :: ValidityUpperBoundSupportedInEra AllegraEra
     ValidityUpperBoundInMaryEra    :: ValidityUpperBoundSupportedInEra MaryEra
     ValidityUpperBoundInAlonzoEra  :: ValidityUpperBoundSupportedInEra AlonzoEra
     ValidityUpperBoundInBabbageEra :: ValidityUpperBoundSupportedInEra BabbageEra
     ValidityUpperBoundInConwayEra :: ValidityUpperBoundSupportedInEra ConwayEra

deriving instance Eq   (ValidityUpperBoundSupportedInEra era)
deriving instance Show (ValidityUpperBoundSupportedInEra era)

validityUpperBoundSupportedInEra :: CardanoEra era
                                 -> Maybe (ValidityUpperBoundSupportedInEra era)
validityUpperBoundSupportedInEra ByronEra   = Nothing
validityUpperBoundSupportedInEra ShelleyEra = Just ValidityUpperBoundInShelleyEra
validityUpperBoundSupportedInEra AllegraEra = Just ValidityUpperBoundInAllegraEra
validityUpperBoundSupportedInEra MaryEra    = Just ValidityUpperBoundInMaryEra
validityUpperBoundSupportedInEra AlonzoEra  = Just ValidityUpperBoundInAlonzoEra
validityUpperBoundSupportedInEra BabbageEra = Just ValidityUpperBoundInBabbageEra
validityUpperBoundSupportedInEra ConwayEra = Just ValidityUpperBoundInConwayEra


-- | A representation of whether the era supports transactions having /no/
-- upper bound on the range of slots in which they are valid.
--
-- Note that the 'ShelleyEra' /does not support/ omitting a validity upper
-- bound. It was introduced as a /required/ field in Shelley and then made
-- optional in Allegra and subsequent eras.
--
-- The Byron era supports this by virtue of the fact that it does not support
-- validity ranges at all.
--
data ValidityNoUpperBoundSupportedInEra era where

     ValidityNoUpperBoundInByronEra   :: ValidityNoUpperBoundSupportedInEra ByronEra
     ValidityNoUpperBoundInAllegraEra :: ValidityNoUpperBoundSupportedInEra AllegraEra
     ValidityNoUpperBoundInMaryEra    :: ValidityNoUpperBoundSupportedInEra MaryEra
     ValidityNoUpperBoundInAlonzoEra  :: ValidityNoUpperBoundSupportedInEra AlonzoEra
     ValidityNoUpperBoundInBabbageEra :: ValidityNoUpperBoundSupportedInEra BabbageEra
     ValidityNoUpperBoundInConwayEra  :: ValidityNoUpperBoundSupportedInEra ConwayEra

deriving instance Eq   (ValidityNoUpperBoundSupportedInEra era)
deriving instance Show (ValidityNoUpperBoundSupportedInEra era)

validityNoUpperBoundSupportedInEra :: CardanoEra era
                                   -> Maybe (ValidityNoUpperBoundSupportedInEra era)
validityNoUpperBoundSupportedInEra ByronEra   = Just ValidityNoUpperBoundInByronEra
validityNoUpperBoundSupportedInEra ShelleyEra = Nothing
validityNoUpperBoundSupportedInEra AllegraEra = Just ValidityNoUpperBoundInAllegraEra
validityNoUpperBoundSupportedInEra MaryEra    = Just ValidityNoUpperBoundInMaryEra
validityNoUpperBoundSupportedInEra AlonzoEra  = Just ValidityNoUpperBoundInAlonzoEra
validityNoUpperBoundSupportedInEra BabbageEra = Just ValidityNoUpperBoundInBabbageEra
validityNoUpperBoundSupportedInEra ConwayEra = Just ValidityNoUpperBoundInConwayEra

-- | A representation of whether the era supports auxiliary scripts in
-- transactions.
--
-- Auxiliary scripts are supported from the Allegra era onwards.
--
data AuxScriptsSupportedInEra era where

     AuxScriptsInAllegraEra :: AuxScriptsSupportedInEra AllegraEra
     AuxScriptsInMaryEra    :: AuxScriptsSupportedInEra MaryEra
     AuxScriptsInAlonzoEra  :: AuxScriptsSupportedInEra AlonzoEra
     AuxScriptsInBabbageEra :: AuxScriptsSupportedInEra BabbageEra
     AuxScriptsInConwayEra  :: AuxScriptsSupportedInEra ConwayEra

deriving instance Eq   (AuxScriptsSupportedInEra era)
deriving instance Show (AuxScriptsSupportedInEra era)

auxScriptsSupportedInEra :: CardanoEra era
                         -> Maybe (AuxScriptsSupportedInEra era)
auxScriptsSupportedInEra ByronEra   = Nothing
auxScriptsSupportedInEra ShelleyEra = Nothing
auxScriptsSupportedInEra AllegraEra = Just AuxScriptsInAllegraEra
auxScriptsSupportedInEra MaryEra    = Just AuxScriptsInMaryEra
auxScriptsSupportedInEra AlonzoEra  = Just AuxScriptsInAlonzoEra
auxScriptsSupportedInEra BabbageEra = Just AuxScriptsInBabbageEra
auxScriptsSupportedInEra ConwayEra  = Just AuxScriptsInConwayEra


-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data BuildTx
data ViewTx

data BuildTxWith build a where

     ViewTx      ::      BuildTxWith ViewTx  a
     BuildTxWith :: a -> BuildTxWith BuildTx a

instance Functor (BuildTxWith build) where
    fmap _ ViewTx = ViewTx
    fmap f (BuildTxWith x) = BuildTxWith (f x)

deriving instance Eq   a => Eq   (BuildTxWith build a)
deriving instance Show a => Show (BuildTxWith build a)

-- ----------------------------------------------------------------------------
-- Transaction input values (era-dependent)
--

type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

data TxInsCollateral era where

     TxInsCollateralNone :: TxInsCollateral era

     TxInsCollateral     :: CollateralSupportedInEra era
                         -> [TxIn] -- Only key witnesses, no scripts.
                         -> TxInsCollateral era

deriving instance Eq   (TxInsCollateral era)
deriving instance Show (TxInsCollateral era)

data TxInsReference build era where

     TxInsReferenceNone :: TxInsReference build era

     TxInsReference     :: BabbageEraOnwards era
                        -> [TxIn]
                        -> TxInsReference build era

deriving instance Eq   (TxInsReference build era)
deriving instance Show (TxInsReference build era)

-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where

  TxOutAdaOnly :: ByronToAllegraEra era -> Lovelace -> TxOutValue era

  TxOutValue   :: MaryEraOnwards era -> Value -> TxOutValue era

instance EraCast TxOutValue where
  eraCast toEra v = case v of
    TxOutAdaOnly _previousEra lovelace ->
      caseByronToAllegraOrMaryEraOnwards
        (\w -> Right $ TxOutAdaOnly w lovelace)
        (\w -> Right $ TxOutValue w $ lovelaceToValue lovelace)
        toEra
    TxOutValue  (_ :: MaryEraOnwards fromEra) value  ->
      caseByronToAllegraOrMaryEraOnwards
        (const (Left $ EraCastError v (cardanoEra @fromEra) toEra))
        (\w -> Right $ TxOutValue w value)
        toEra

deriving instance Eq   (TxOutValue era)
deriving instance Show (TxOutValue era)
deriving instance Generic (TxOutValue era)

instance ToJSON (TxOutValue era) where
  toJSON (TxOutAdaOnly _ ll) = toJSON ll
  toJSON (TxOutValue _ val) = toJSON val

instance IsCardanoEra era => FromJSON (TxOutValue era) where
  parseJSON = withObject "TxOutValue" $ \o ->
    caseByronToAllegraOrMaryEraOnwards
      (\w -> do
        ll <- o .: "lovelace"
        pure $ TxOutAdaOnly w $ selectLovelace ll
      )
      (\w -> do
        let l = KeyMap.toList o
        vals <- mapM decodeAssetId l
        pure $ TxOutValue w $ mconcat vals
      )
      cardanoEra
    where
     decodeAssetId :: (Aeson.Key, Aeson.Value) -> Aeson.Parser Value
     decodeAssetId (polid, Aeson.Object assetNameHm) = do
       let polId = fromString . Text.unpack $ Aeson.toText polid
       aNameQuantity <- decodeAssets assetNameHm
       pure . valueFromList
         $ map (first $ AssetId polId) aNameQuantity

     decodeAssetId ("lovelace", Aeson.Number sci) =
       case toBoundedInteger sci of
         Just (ll :: Word64) ->
           pure $ valueFromList [(AdaAssetId, Quantity $ toInteger ll)]
         Nothing ->
           fail $ "Expected a Bounded number but got: " <> show sci
     decodeAssetId wrong = fail $ "Expected a policy id and a JSON object but got: " <> show wrong

     decodeAssets :: Aeson.Object -> Aeson.Parser [(AssetName, Quantity)]
     decodeAssets assetNameHm =
       let l = KeyMap.toList assetNameHm
       in mapM (\(aName, q) -> (,) <$> parseAssetName aName <*> decodeQuantity q) l

     parseAssetName :: Aeson.Key -> Aeson.Parser AssetName
     parseAssetName aName = runParsecParser assetName (Aeson.toText aName)

     decodeQuantity :: Aeson.Value -> Aeson.Parser Quantity
     decodeQuantity (Aeson.Number sci) =
       case toBoundedInteger sci of
         Just (ll :: Word64) -> return . Quantity $ toInteger ll
         Nothing -> fail $ "Expected a Bounded number but got: " <> show sci
     decodeQuantity wrong = fail $ "Expected aeson Number but got: " <> show wrong

lovelaceToTxOutValue :: IsCardanoEra era => Lovelace -> TxOutValue era
lovelaceToTxOutValue l =
  caseByronToAllegraOrMaryEraOnwards
    (\w -> TxOutAdaOnly w l)
    (\w -> TxOutValue w (lovelaceToValue l))
    cardanoEra

txOutValueToLovelace :: TxOutValue era -> Lovelace
txOutValueToLovelace tv =
  case tv of
    TxOutAdaOnly _ l -> l
    TxOutValue _ v -> selectLovelace v

txOutValueToValue :: TxOutValue era -> Value
txOutValueToValue tv =
  case tv of
    TxOutAdaOnly _ l -> lovelaceToValue l
    TxOutValue _ v -> v

prettyRenderTxOut :: TxOutInAnyEra -> Text
prettyRenderTxOut (TxOutInAnyEra _ (TxOut (AddressInEra _ addr) txOutVal _ _)) =
     serialiseAddress (toAddressAny addr) <> " + "
  <> renderValue (txOutValueToValue txOutVal)

data TxReturnCollateral ctx era where

  TxReturnCollateralNone
    :: TxReturnCollateral ctx era

  TxReturnCollateral
    :: BabbageEraOnwards era
    -> TxOut ctx era
    -> TxReturnCollateral ctx era

deriving instance Eq   (TxReturnCollateral ctx era)
deriving instance Show (TxReturnCollateral ctx era)

data TxTotalCollateral era where

  TxTotalCollateralNone
    :: TxTotalCollateral era

  TxTotalCollateral
    :: BabbageEraOnwards era
    -> Lovelace
    -> TxTotalCollateral era

deriving instance Eq   (TxTotalCollateral era)
deriving instance Show (TxTotalCollateral era)

-- ----------------------------------------------------------------------------
-- Transaction output datum (era-dependent)
--

data TxOutDatum ctx era where

     TxOutDatumNone   :: TxOutDatum ctx era

     -- | A transaction output that only specifies the hash of the datum, but
     -- not the full datum value.
     --
     TxOutDatumHash   :: AlonzoEraOnwards era
                      -> Hash ScriptData
                      -> TxOutDatum ctx era

     -- | A transaction output that specifies the whole datum value. This can
     -- only be used in the context of the transaction body, and does not occur
     -- in the UTxO. The UTxO only contains the datum hash.
     --
     TxOutDatumInTx'  :: AlonzoEraOnwards era
                      -> Hash ScriptData
                      -> HashableScriptData
                      -> TxOutDatum CtxTx era

     -- | A transaction output that specifies the whole datum instead of the
     -- datum hash. Note that the datum map will not be updated with this datum,
     -- it only exists at the transaction output.
     --
     TxOutDatumInline :: BabbageEraOnwards era
                      -> HashableScriptData
                      -> TxOutDatum ctx era

deriving instance Eq   (TxOutDatum ctx era)
deriving instance Show (TxOutDatum ctx era)

instance EraCast (TxOutDatum ctx)  where
  eraCast toEra v = case v of
    TxOutDatumNone -> pure TxOutDatumNone
    TxOutDatumHash ws hash ->
      caseByronToMaryOrAlonzoEraOnwards
        (const (Left $ EraCastError v (alonzoEraOnwardsToCardanoEra ws) toEra))
        (\wt -> Right $ TxOutDatumHash wt hash)
        toEra
    TxOutDatumInTx' ws scriptData hash ->
      caseByronToMaryOrAlonzoEraOnwards
        (const (Left $ EraCastError v (alonzoEraOnwardsToCardanoEra ws) toEra))
        (\wt -> Right $ TxOutDatumInTx' wt scriptData hash)
        toEra
    TxOutDatumInline ws scriptData ->
      caseByronToAlonzoOrBabbageEraOnwards
        (const (Left $ EraCastError v (babbageEraOnwardsToCardanoEra ws) toEra))
        (\wt -> Right $ TxOutDatumInline wt scriptData)
        toEra

pattern TxOutDatumInTx
  :: AlonzoEraOnwards era
  -> HashableScriptData
  -> TxOutDatum CtxTx era
pattern TxOutDatumInTx w d <- TxOutDatumInTx' w _ d
  where
    TxOutDatumInTx w d = TxOutDatumInTx' w (hashScriptDataBytes d) d

{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx', TxOutDatumInline #-}
{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx , TxOutDatumInline #-}

parseHash :: SerialiseAsRawBytes (Hash a) => AsType (Hash a) -> Parsec.Parser (Hash a)
parseHash asType = do
  str <- some Parsec.hexDigit <?> "hash"
  failEitherWith (\e -> "Failed to parse hash: " ++ displayError e) $
    deserialiseFromRawBytesHex asType (BSC.pack str)

-- ----------------------------------------------------------------------------
-- Transaction fees
--

data TxFee era where
  TxFeeImplicit :: ByronEraOnly era -> TxFee era

  TxFeeExplicit :: ShelleyBasedEra era -> Lovelace -> TxFee era

deriving instance Eq   (TxFee era)
deriving instance Show (TxFee era)

defaultTxFee :: forall era. IsCardanoEra era => TxFee era
defaultTxFee =
  caseByronOrShelleyBasedEra
    TxFeeImplicit
    (\w -> TxFeeExplicit w mempty)
    (cardanoEra @era)

-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
--
data TxValidityUpperBound era where

     TxValidityNoUpperBound :: ValidityNoUpperBoundSupportedInEra era
                            -> TxValidityUpperBound era

     TxValidityUpperBound   :: ValidityUpperBoundSupportedInEra era
                            -> SlotNo
                            -> TxValidityUpperBound era

deriving instance Eq   (TxValidityUpperBound era)
deriving instance Show (TxValidityUpperBound era)

defaultTxValidityUpperBound :: forall era. IsCardanoEra era => TxValidityUpperBound era
defaultTxValidityUpperBound = case cardanoEra @era of
    ByronEra -> TxValidityNoUpperBound ValidityNoUpperBoundInByronEra
    ShelleyEra -> TxValidityUpperBound ValidityUpperBoundInShelleyEra maxBound
    AllegraEra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    MaryEra -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    AlonzoEra -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
    BabbageEra -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
    ConwayEra -> TxValidityNoUpperBound ValidityNoUpperBoundInConwayEra

data TxValidityLowerBound era where

  TxValidityNoLowerBound
    :: TxValidityLowerBound era

  TxValidityLowerBound
    :: AllegraEraOnwards era
    -> SlotNo
    -> TxValidityLowerBound era

deriving instance Eq   (TxValidityLowerBound era)
deriving instance Show (TxValidityLowerBound era)

-- ----------------------------------------------------------------------------
-- Transaction metadata (era-dependent)
--

data TxMetadataInEra era where

  TxMetadataNone
    :: TxMetadataInEra era

  TxMetadataInEra
    :: ShelleyBasedEra era
    -> TxMetadata
    -> TxMetadataInEra era

deriving instance Eq   (TxMetadataInEra era)
deriving instance Show (TxMetadataInEra era)

-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where

     TxAuxScriptsNone :: TxAuxScripts era

     TxAuxScripts     :: AuxScriptsSupportedInEra era
                      -> [ScriptInEra era]
                      -> TxAuxScripts era

deriving instance Eq   (TxAuxScripts era)
deriving instance Show (TxAuxScripts era)

-- ----------------------------------------------------------------------------
-- Optionally required signatures (era-dependent)
--

data TxExtraKeyWitnesses era where

  TxExtraKeyWitnessesNone
    :: TxExtraKeyWitnesses era

  TxExtraKeyWitnesses
    :: AlonzoEraOnwards era
    -> [Hash PaymentKey]
    -> TxExtraKeyWitnesses era

deriving instance Eq   (TxExtraKeyWitnesses era)
deriving instance Show (TxExtraKeyWitnesses era)

-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals build era where

  TxWithdrawalsNone
    :: TxWithdrawals build era

  TxWithdrawals
    :: ShelleyBasedEra era
    -> [(StakeAddress, Lovelace, BuildTxWith build (Witness WitCtxStake era))]
    -> TxWithdrawals build era

deriving instance Eq   (TxWithdrawals build era)
deriving instance Show (TxWithdrawals build era)

-- ----------------------------------------------------------------------------
-- Certificates within transactions (era-dependent)
--

data TxCertificates build era where

  TxCertificatesNone
    :: TxCertificates build era

  TxCertificates
    :: ShelleyBasedEra era
    -> [Certificate era]
    -> BuildTxWith build (Map StakeCredential (Witness WitCtxStake era))
    -> TxCertificates build era

deriving instance Eq   (TxCertificates build era)
deriving instance Show (TxCertificates build era)

-- ----------------------------------------------------------------------------
-- Transaction update proposal (era-dependent)
--

data TxUpdateProposal era where
  TxUpdateProposalNone :: TxUpdateProposal era
  TxUpdateProposal :: ShelleyToBabbageEra era -> UpdateProposal -> TxUpdateProposal era

deriving instance Eq   (TxUpdateProposal era)
deriving instance Show (TxUpdateProposal era)

-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue build era where

     TxMintNone  :: TxMintValue build era

     TxMintValue :: MaryEraOnwards era
                 -> Value
                 -> BuildTxWith build
                      (Map PolicyId (ScriptWitness WitCtxMint era))
                 -> TxMintValue build era

deriving instance Eq   (TxMintValue build era)
deriving instance Show (TxMintValue build era)

-- ----------------------------------------------------------------------------
-- Transaction body content
--

data TxBodyContent build era =
     TxBodyContent {
       txIns                :: TxIns build era,
       txInsCollateral      :: TxInsCollateral era,
       txInsReference       :: TxInsReference build era,
       txOuts               :: [TxOut CtxTx era],
       txTotalCollateral    :: TxTotalCollateral era,
       txReturnCollateral   :: TxReturnCollateral CtxTx era,
       txFee                :: TxFee era,
       txValidityRange      :: (TxValidityLowerBound era,
                              TxValidityUpperBound era),
       txMetadata           :: TxMetadataInEra era,
       txAuxScripts         :: TxAuxScripts era,
       txExtraKeyWits       :: TxExtraKeyWitnesses era,
       txProtocolParams     :: BuildTxWith build (Maybe (LedgerProtocolParameters era)),
       txWithdrawals        :: TxWithdrawals  build era,
       txCertificates       :: TxCertificates build era,
       txUpdateProposal     :: TxUpdateProposal era,
       txMintValue          :: TxMintValue    build era,
       txScriptValidity     :: TxScriptValidity era,
       txProposalProcedures :: Maybe (Featured ConwayEraOnwards era [Proposal era]),
       txVotingProcedures   :: Maybe (Featured ConwayEraOnwards era (VotingProcedures era))
     }
     deriving (Eq, Show)

defaultTxBodyContent :: IsCardanoEra era => TxBodyContent BuildTx era
defaultTxBodyContent = TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txInsReference = TxInsReferenceNone
    , txOuts = []
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee = defaultTxFee
    , txValidityRange = (TxValidityNoLowerBound, defaultTxValidityUpperBound)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    , txScriptValidity = TxScriptValidityNone
    , txProposalProcedures = Nothing
    , txVotingProcedures = Nothing
    }

setTxIns :: TxIns build era -> TxBodyContent build era -> TxBodyContent build era
setTxIns v txBodyContent = txBodyContent { txIns = v }

modTxIns :: (TxIns build era -> TxIns build era) -> TxBodyContent build era -> TxBodyContent build era
modTxIns f txBodyContent = txBodyContent { txIns = f (txIns txBodyContent) }

addTxIn :: (TxIn, BuildTxWith build (Witness WitCtxTxIn era)) -> TxBodyContent build era -> TxBodyContent build era
addTxIn txIn = modTxIns (txIn:)

setTxInsCollateral :: TxInsCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxInsCollateral v txBodyContent = txBodyContent { txInsCollateral = v }

setTxInsReference :: TxInsReference build era -> TxBodyContent build era -> TxBodyContent build era
setTxInsReference v txBodyContent = txBodyContent { txInsReference = v }

setTxOuts :: [TxOut CtxTx era] -> TxBodyContent build era -> TxBodyContent build era
setTxOuts v txBodyContent = txBodyContent { txOuts = v }

modTxOuts :: ([TxOut CtxTx era] -> [TxOut CtxTx era]) -> TxBodyContent build era -> TxBodyContent build era
modTxOuts f txBodyContent = txBodyContent { txOuts = f (txOuts txBodyContent) }

addTxOut :: TxOut CtxTx era -> TxBodyContent build era -> TxBodyContent build era
addTxOut txOut = modTxOuts (txOut:)

setTxTotalCollateral :: TxTotalCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxTotalCollateral v txBodyContent = txBodyContent { txTotalCollateral = v }

setTxReturnCollateral :: TxReturnCollateral CtxTx era -> TxBodyContent build era -> TxBodyContent build era
setTxReturnCollateral v txBodyContent = txBodyContent { txReturnCollateral = v }

setTxFee :: TxFee era -> TxBodyContent build era -> TxBodyContent build era
setTxFee v txBodyContent = txBodyContent { txFee = v }

setTxValidityRange :: (TxValidityLowerBound era, TxValidityUpperBound era) -> TxBodyContent build era -> TxBodyContent build era
setTxValidityRange v txBodyContent = txBodyContent { txValidityRange = v }

setTxMetadata :: TxMetadataInEra era -> TxBodyContent build era -> TxBodyContent build era
setTxMetadata v txBodyContent = txBodyContent { txMetadata = v }

setTxAuxScripts :: TxAuxScripts era -> TxBodyContent build era -> TxBodyContent build era
setTxAuxScripts v txBodyContent = txBodyContent { txAuxScripts = v }

setTxExtraKeyWits :: TxExtraKeyWitnesses era -> TxBodyContent build era -> TxBodyContent build era
setTxExtraKeyWits v txBodyContent = txBodyContent { txExtraKeyWits = v }

setTxProtocolParams :: BuildTxWith build (Maybe (LedgerProtocolParameters era)) -> TxBodyContent build era -> TxBodyContent build era
setTxProtocolParams v txBodyContent = txBodyContent { txProtocolParams = v }

setTxWithdrawals :: TxWithdrawals build era -> TxBodyContent build era -> TxBodyContent build era
setTxWithdrawals v txBodyContent = txBodyContent { txWithdrawals = v }

setTxCertificates :: TxCertificates build era -> TxBodyContent build era -> TxBodyContent build era
setTxCertificates v txBodyContent = txBodyContent { txCertificates = v }

setTxUpdateProposal :: TxUpdateProposal era -> TxBodyContent build era -> TxBodyContent build era
setTxUpdateProposal v txBodyContent = txBodyContent { txUpdateProposal = v }

setTxMintValue :: TxMintValue build era -> TxBodyContent build era -> TxBodyContent build era
setTxMintValue v txBodyContent = txBodyContent { txMintValue = v }

setTxScriptValidity :: TxScriptValidity era -> TxBodyContent build era -> TxBodyContent build era
setTxScriptValidity v txBodyContent = txBodyContent { txScriptValidity = v }

-- ----------------------------------------------------------------------------
-- Transaction bodies
--

data TxBody era where

     ByronTxBody
       :: Annotated Byron.Tx ByteString
       -> TxBody ByronEra

     ShelleyTxBody
       :: ShelleyBasedEra era
       -> Ledger.TxBody (ShelleyLedgerEra era)

          -- We include the scripts along with the tx body, rather than the
          -- witnesses set, since they need to be known when building the body.
       -> [Ledger.Script (ShelleyLedgerEra era)]

          -- The info for each use of each script: the script input data, both
          -- the UTxO input data (called the "datum") and the supplied input
          -- data (called the "redeemer") and the execution units.
       -> TxBodyScriptData era

          -- The 'L.TxAuxData' consists of one or several things,
          -- depending on era:
          -- + transaction metadata  (in Shelley and later)
          -- + auxiliary scripts     (in Allegra and later)
          -- Note that there is no auxiliary script data as such, because the
          -- extra script data has to be passed to scripts and hence is needed
          -- for validation. It is thus part of the witness data, not the
          -- auxiliary data.
       -> Maybe (L.TxAuxData (ShelleyLedgerEra era))

       -> TxScriptValidity era -- ^ Mark script as expected to pass or fail validation

       -> TxBody era
     -- The 'ShelleyBasedEra' GADT tells us what era we are in.
     -- The 'ShelleyLedgerEra' type family maps that to the era type from the
     -- ledger lib. The 'Ledger.TxBody' type family maps that to a specific
     -- tx body type, which is different for each Shelley-based era.


data TxBodyScriptData era where
     TxBodyNoScriptData :: TxBodyScriptData era
     TxBodyScriptData   :: AlonzoEraOnwards era
                        -> Alonzo.TxDats (ShelleyLedgerEra era)
                        -> Alonzo.Redeemers (ShelleyLedgerEra era)
                        -> TxBodyScriptData era

deriving instance Eq   (TxBodyScriptData era)
deriving instance L.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto => Show (TxBodyScriptData era)


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Eq (TxBody era) where
    (==) (ByronTxBody txbodyA)
         (ByronTxBody txbodyB) = txbodyA == txbodyB

    (==) (ShelleyTxBody sbe txbodyA txscriptsA redeemersA txmetadataA scriptValidityA)
         (ShelleyTxBody _   txbodyB txscriptsB redeemersB txmetadataB scriptValidityB) =
         case sbe of
           ShelleyBasedEraShelley -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraAllegra -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraMary    -> txbodyA     == txbodyB
                                  && txscriptsA  == txscriptsB
                                  && txmetadataA == txmetadataB

           ShelleyBasedEraAlonzo  -> txbodyA         == txbodyB
                                  && txscriptsA      == txscriptsB
                                  && redeemersA      == redeemersB
                                  && txmetadataA     == txmetadataB
                                  && scriptValidityA == scriptValidityB

           ShelleyBasedEraBabbage -> txbodyA         == txbodyB
                                  && txscriptsA      == txscriptsB
                                  && redeemersA      == redeemersB
                                  && txmetadataA     == txmetadataB
                                  && scriptValidityA == scriptValidityB

           ShelleyBasedEraConway  -> txbodyA         == txbodyB
                                  && txscriptsA      == txscriptsB
                                  && redeemersA      == redeemersB
                                  && txmetadataA     == txmetadataB
                                  && scriptValidityA == scriptValidityB

    (==) ByronTxBody{} (ShelleyTxBody sbe _ _ _ _ _) = case sbe of {}
    (==) (ShelleyTxBody sbe _ _ _ _ _) ByronTxBody{} = case sbe of {}


-- The GADT in the ShelleyTxBody case requires a custom instance
instance Show (TxBody era) where
    showsPrec p (ByronTxBody txbody) =
      showParen (p >= 11)
        ( showString "ByronTxBody "
        . showsPrec 11 txbody
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraShelley
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraShelley "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAllegra
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAllegra "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraMary
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraMary "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraAlonzo
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraAlonzo "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraBabbage
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraBabbage "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )

    showsPrec p (ShelleyTxBody ShelleyBasedEraConway
                               txbody txscripts redeemers txmetadata scriptValidity) =
      showParen (p >= 11)
        ( showString "ShelleyTxBody ShelleyBasedEraConway "
        . showsPrec 11 txbody
        . showChar ' '
        . showsPrec 11 txscripts
        . showChar ' '
        . showsPrec 11 redeemers
        . showChar ' '
        . showsPrec 11 txmetadata
        . showChar ' '
        . showsPrec 11 scriptValidity
        )


instance HasTypeProxy era => HasTypeProxy (TxBody era) where
    data AsType (TxBody era) = AsTxBody (AsType era)
    proxyToAsType _ = AsTxBody (proxyToAsType (Proxy :: Proxy era))

pattern AsByronTxBody :: AsType (TxBody ByronEra)
pattern AsByronTxBody   = AsTxBody AsByronEra
{-# COMPLETE AsByronTxBody #-}

pattern AsShelleyTxBody :: AsType (TxBody ShelleyEra)
pattern AsShelleyTxBody = AsTxBody AsShelleyEra
{-# COMPLETE AsShelleyTxBody #-}

pattern AsMaryTxBody :: AsType (TxBody MaryEra)
pattern AsMaryTxBody = AsTxBody AsMaryEra
{-# COMPLETE AsMaryTxBody #-}

instance IsCardanoEra era => SerialiseAsCBOR (TxBody era) where

    serialiseToCBOR (ByronTxBody txbody) =
      recoverBytes txbody

    serialiseToCBOR (ShelleyTxBody sbe txbody txscripts redeemers txmetadata scriptValidity) =
      shelleyBasedEraConstraints sbe $
        serialiseShelleyBasedTxBody sbe txbody txscripts redeemers txmetadata scriptValidity

    deserialiseFromCBOR _ bs =
      case cardanoEra :: CardanoEra era of
        ByronEra ->
          ByronTxBody <$>
            CBOR.decodeFullAnnotatedBytes
              CBOR.byronProtVer
              "Byron TxBody"
              CBOR.decCBORAnnotated
              (LBS.fromStrict bs)

        -- Use the same deserialisation impl, but at different types:
        ShelleyEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraShelley bs
        AllegraEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraAllegra bs
        MaryEra    -> deserialiseShelleyBasedTxBody ShelleyBasedEraMary    bs
        AlonzoEra  -> deserialiseShelleyBasedTxBody ShelleyBasedEraAlonzo  bs
        BabbageEra -> deserialiseShelleyBasedTxBody ShelleyBasedEraBabbage bs
        ConwayEra  -> deserialiseShelleyBasedTxBody ShelleyBasedEraConway  bs

-- | The serialisation format for the different Shelley-based eras are not the
-- same, but they can be handled generally with one overloaded implementation.
serialiseShelleyBasedTxBody
  :: forall era ledgerera.
     L.Era ledgerera
  => ShelleyLedgerEra era ~ ledgerera
  => CBOR.EncCBOR (Ledger.TxBody ledgerera)
  => CBOR.EncCBOR (Ledger.Script ledgerera)
  => CBOR.EncCBOR (Alonzo.TxDats ledgerera)
  => CBOR.EncCBOR (Alonzo.Redeemers ledgerera)
  => CBOR.EncCBOR (L.TxAuxData ledgerera)
  => ShelleyBasedEra era
  -> Ledger.TxBody ledgerera
  -> [Ledger.Script ledgerera]
  -> TxBodyScriptData era
  -> Maybe (L.TxAuxData ledgerera)
  -> TxScriptValidity era -- ^ Mark script as expected to pass or fail validation
  -> ByteString
serialiseShelleyBasedTxBody sbe txbody txscripts
                            TxBodyNoScriptData txmetadata scriptValidity =
    -- Backwards compat for pre-Alonzo era tx body files
    case sbe of
      ShelleyBasedEraShelley -> preAlonzo (L.eraProtVerLow @L.Shelley)
      ShelleyBasedEraAllegra -> preAlonzo (L.eraProtVerLow @L.Allegra)
      ShelleyBasedEraMary -> preAlonzo (L.eraProtVerLow @L.Mary)
      ShelleyBasedEraAlonzo ->
        CBOR.serialize' (L.eraProtVerLow @L.Alonzo)
          $ CBOR.encodeListLen 4
         <> CBOR.encCBOR txbody
         <> CBOR.encCBOR txscripts
         <> CBOR.encCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata
      ShelleyBasedEraBabbage ->
        CBOR.serialize' (L.eraProtVerLow @L.Babbage)
          $ CBOR.encodeListLen 4
         <> CBOR.encCBOR txbody
         <> CBOR.encCBOR txscripts
         <> CBOR.encCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata
      ShelleyBasedEraConway ->
        CBOR.serialize' (L.eraProtVerLow @L.Babbage)
          $ CBOR.encodeListLen 4
         <> CBOR.encCBOR txbody
         <> CBOR.encCBOR txscripts
         <> CBOR.encCBOR (txScriptValidityToScriptValidity scriptValidity)
         <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata
 where
   preAlonzo v = CBOR.serialize' v
                 $ CBOR.encodeListLen 3
                <> CBOR.encCBOR txbody
                <> CBOR.encCBOR txscripts
                <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata

serialiseShelleyBasedTxBody _era txbody txscripts
                            (TxBodyScriptData _ datums redeemers)
                            txmetadata txBodyScriptValidity =
    CBOR.serialize' (L.eraProtVerLow @ledgerera) $
        CBOR.encodeListLen 6
     <> CBOR.encCBOR txbody
     <> CBOR.encCBOR txscripts
     <> CBOR.encCBOR datums
     <> CBOR.encCBOR redeemers
     <> CBOR.encCBOR (txScriptValidityToScriptValidity txBodyScriptValidity)
     <> CBOR.encodeNullMaybe CBOR.encCBOR txmetadata

deserialiseShelleyBasedTxBody
  :: forall era ledgerera.
     L.Era ledgerera
  => ShelleyLedgerEra era ~ ledgerera
  => CBOR.DecCBOR (CBOR.Annotator (Ledger.TxBody ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (Ledger.Script ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (Alonzo.TxDats ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (Alonzo.Redeemers ledgerera))
  => CBOR.DecCBOR (CBOR.Annotator (L.TxAuxData ledgerera))
  => ShelleyBasedEra era
  -> ByteString
  -> Either CBOR.DecoderError (TxBody era)
deserialiseShelleyBasedTxBody sbe bs =
    CBOR.decodeFullAnnotator
      (L.eraProtVerLow @ledgerera)
      "Shelley TxBody"
      decodeAnnotatedTuple
      (LBS.fromStrict bs)
  where
    decodeAnnotatedTuple :: CBOR.Decoder s (CBOR.Annotator (TxBody era))
    decodeAnnotatedTuple = do
      len <- CBOR.decodeListLen

      case len of
        -- Backwards compat for pre-Alonzo era tx body files
        2 -> do
          txbody     <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody sbe
              (flip CBOR.runAnnotator fbs txbody)
              [] -- scripts
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return TxScriptValidityNone))
        3 -> do
          txbody     <- CBOR.decCBOR
          txscripts  <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody sbe
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return TxScriptValidityNone))
        4 -> do
          sValiditySupported <-
            case txScriptValiditySupportedInShelleyBasedEra sbe of
              Nothing -> fail $ mconcat
                [ "deserialiseShelleyBasedTxBody: Expected an era that supports the "
                , "script validity flag but got: "
                , show sbe
                ]
              Just supported -> return supported

          txbody     <- CBOR.decCBOR
          txscripts  <- CBOR.decCBOR
          scriptValidity <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR
          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody sbe
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs (return TxBodyNoScriptData))
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return $ TxScriptValidity sValiditySupported scriptValidity))
        6 -> do
          sDataSupported <-
            forEraInEon (shelleyBasedToCardanoEra sbe)
              ( fail $ mconcat
                  [ "deserialiseShelleyBasedTxBody: Expected an era that supports script"
                  , " data but got: "
                  , show sbe
                  ]
              )
              pure

          sValiditySupported <-
            case txScriptValiditySupportedInShelleyBasedEra sbe of
              Nothing -> fail $ mconcat
                [ "deserialiseShelleyBasedTxBody: Expected an era that supports the "
                , "script validity flag but got: "
                , show sbe
                ]
              Just supported -> return supported

          txbody    <- CBOR.decCBOR
          txscripts <- CBOR.decCBOR
          datums    <- CBOR.decCBOR
          redeemers <- CBOR.decCBOR
          scriptValidity <- CBOR.decCBOR
          txmetadata <- CBOR.decodeNullMaybe CBOR.decCBOR

          let txscriptdata = CBOR.Annotator $ \fbs ->
                               TxBodyScriptData sDataSupported
                                 (flip CBOR.runAnnotator fbs datums)
                                 (flip CBOR.runAnnotator fbs redeemers)

          return $ CBOR.Annotator $ \fbs ->
            ShelleyTxBody sbe
              (flip CBOR.runAnnotator fbs txbody)
              (map (flip CBOR.runAnnotator fbs) txscripts)
              (flip CBOR.runAnnotator fbs txscriptdata)
              (fmap (flip CBOR.runAnnotator fbs) txmetadata)
              (flip CBOR.runAnnotator fbs (return $ TxScriptValidity sValiditySupported scriptValidity))
        _ -> fail $ "expected tx body tuple of size 2, 3, 4 or 6, got " <> show len

instance IsCardanoEra era => HasTextEnvelope (TxBody era) where
    textEnvelopeType _ =
      case cardanoEra :: CardanoEra era of
        ByronEra   -> "TxUnsignedByron"
        ShelleyEra -> "TxUnsignedShelley"
        AllegraEra -> "TxBodyAllegra"
        MaryEra    -> "TxBodyMary"
        AlonzoEra  -> "TxBodyAlonzo"
        BabbageEra -> "TxBodyBabbage"
        ConwayEra  -> "TxBodyConway"

-- | Calculate the transaction identifier for a 'TxBody'.
--
getTxId :: forall era. TxBody era -> TxId
getTxId (ByronTxBody tx) =
    TxId
  . fromMaybe impossible
  . Crypto.hashFromBytesShort
  . Byron.abstractHashToShort
  . Byron.hashDecoded
  $ tx
  where
    impossible =
      error "getTxId: byron and shelley hash sizes do not match"

getTxId (ShelleyTxBody sbe tx _ _ _ _) =
  withShelleyBasedEraConstraintsForLedger sbe $ getTxIdShelley sbe tx

getTxIdShelley
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.EraTxBody (ShelleyLedgerEra era)
  => ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxId
getTxIdShelley _ tx =
    TxId
  . Crypto.castHash
  . (\(Ledger.TxId txhash) -> SafeHash.extractHash txhash)
  $ Ledger.txid tx

-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError =
       TxBodyEmptyTxIns
     | TxBodyEmptyTxInsCollateral
     | TxBodyEmptyTxOuts
     | TxBodyOutputNegative !Quantity !TxOutInAnyEra
     | TxBodyOutputOverflow !Quantity !TxOutInAnyEra
     | TxBodyMetadataError ![(Word64, TxMetadataRangeError)]
     | TxBodyMintAdaError
     | TxBodyInIxOverflow !TxIn
     | TxBodyMissingProtocolParams
     | TxBodyProtocolParamsConversionError !ProtocolParametersConversionError
     deriving (Eq, Show)

instance Error TxBodyError where
    displayError TxBodyEmptyTxIns  = "Transaction body has no inputs"
    displayError TxBodyEmptyTxInsCollateral =
      "Transaction body has no collateral inputs, but uses Plutus scripts"
    displayError TxBodyEmptyTxOuts = "Transaction body has no outputs"
    displayError (TxBodyOutputNegative (Quantity q) txout) =
      "Negative quantity (" ++ show q ++ ") in transaction output: " ++
      show txout
    displayError (TxBodyOutputOverflow (Quantity q) txout) =
      "Quantity too large (" ++ show q ++ " >= 2^64) in transaction output: " ++
      show txout
    displayError (TxBodyMetadataError [(k, err)]) =
      "Error in metadata entry " ++ show k ++ ": " ++ displayError err
    displayError (TxBodyMetadataError errs) =
      "Error in metadata entries: " ++
      intercalate "; "
        [ show k ++ ": " ++ displayError err
        | (k, err) <- errs ]
    displayError TxBodyMintAdaError =
      "Transaction cannot mint ada, only non-ada assets"
    displayError TxBodyMissingProtocolParams =
      "Transaction uses Plutus scripts but does not provide the protocol " ++
      "parameters to hash"
    displayError (TxBodyInIxOverflow txin) =
      "Transaction input index is too big, " ++
      "acceptable value is up to 2^32-1, " ++
      "in input " ++ show txin
    displayError (TxBodyProtocolParamsConversionError ppces) =
      "Errors in protocol parameters conversion: " ++ displayError ppces

createTransactionBody
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
createTransactionBody sbe txBodyContent =
  let apiTxOuts = txOuts txBodyContent
      apiScriptWitnesses = collectTxBodyScriptWitnesses sbe txBodyContent
      apiScriptValidity = txScriptValidity txBodyContent
      apiMintValue = txMintValue txBodyContent
      apiProtocolParameters = txProtocolParams txBodyContent
      apiCollateralTxIns = txInsCollateral txBodyContent
      apiReferenceInputs = txInsReference txBodyContent
      apiExtraKeyWitnesses = txExtraKeyWits txBodyContent
      apiReturnCollateral = txReturnCollateral txBodyContent
      apiTotalCollateral = txTotalCollateral txBodyContent

      -- Ledger types
      collTxIns = convCollateralTxIns apiCollateralTxIns
      refTxIns = convReferenceInputs apiReferenceInputs
      returnCollateral = convReturnCollateral sbe apiReturnCollateral
      totalCollateral = convTotalCollateral apiTotalCollateral
      certs = convCertificates sbe $ txCertificates txBodyContent
      txAuxData = toAuxiliaryData sbe (txMetadata txBodyContent) (txAuxScripts txBodyContent)
      scripts = convScripts apiScriptWitnesses
      validityInterval = convValidityInterval $ txValidityRange txBodyContent
      languages = convLanguages apiScriptWitnesses

      mkTxBody :: ()
        => L.EraTxBody (ShelleyLedgerEra era)
        => L.EraTxAuxData (ShelleyLedgerEra era)
        => ShelleyBasedEra era
        -> TxBodyContent BuildTx era
        -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
        -> L.TxBody (ShelleyLedgerEra era)
      mkTxBody sbe' bc = shelleyBasedEraConstraints sbe' $
        mkCommonTxBody
          sbe'
          (txIns bc)
          (txOuts bc)
          (txFee bc)
          (txWithdrawals bc)

  in case sbe of
       ShelleyBasedEraShelley -> do
        update <- convTxUpdateProposal sbe (txUpdateProposal txBodyContent)
        let (_, upperBound) = txValidityRange txBodyContent
            ttl = case upperBound of
                    TxValidityNoUpperBound sbe' -> case sbe' of {}
                    TxValidityUpperBound _ ttl'  -> ttl'
            ledgerTxBody = mkTxBody ShelleyBasedEraShelley txBodyContent txAuxData
                           & L.certsTxBodyL  .~ certs
                           & L.ttlTxBodyL    .~ ttl
                           & L.updateTxBodyL .~ update

            sData = convScriptData (shelleyBasedToCardanoEra sbe) apiTxOuts apiScriptWitnesses

        pure $ ShelleyTxBody sbe
              ledgerTxBody
              scripts
              sData
              txAuxData
              apiScriptValidity

       ShelleyBasedEraAllegra -> do
        update <- convTxUpdateProposal sbe (txUpdateProposal txBodyContent)
        let ledgerTxBody = mkTxBody ShelleyBasedEraAllegra txBodyContent txAuxData
                           & L.certsTxBodyL  .~ certs
                           & L.updateTxBodyL .~ update
                           & L.vldtTxBodyL   .~ validityInterval
        pure $ ShelleyTxBody sbe
              ledgerTxBody
              scripts
              (convScriptData (shelleyBasedToCardanoEra sbe) apiTxOuts apiScriptWitnesses)
              txAuxData
              apiScriptValidity

       ShelleyBasedEraMary -> do
        update <- convTxUpdateProposal sbe (txUpdateProposal txBodyContent)
        let ledgerTxBody = mkTxBody ShelleyBasedEraMary txBodyContent txAuxData
                           & L.certsTxBodyL  .~ certs
                           & L.updateTxBodyL .~ update
                           & L.vldtTxBodyL   .~ validityInterval
                           & L.mintTxBodyL   .~ convMintValue apiMintValue
        pure $ ShelleyTxBody sbe
              ledgerTxBody
              scripts
              (convScriptData (shelleyBasedToCardanoEra sbe) apiTxOuts apiScriptWitnesses)
              txAuxData
              apiScriptValidity

       ShelleyBasedEraAlonzo -> do
        update <- convTxUpdateProposal sbe (txUpdateProposal txBodyContent)
        let sData = convScriptData (shelleyBasedToCardanoEra sbe) apiTxOuts apiScriptWitnesses
        scriptIntegrityHash <-
          case sData of
            TxBodyNoScriptData -> pure SNothing
            TxBodyScriptData _ datums redeemers ->
              convPParamsToScriptIntegrityHash
                sbe
                apiProtocolParameters
                redeemers
                datums
                languages
        let ledgerTxBody = mkTxBody ShelleyBasedEraAlonzo txBodyContent txAuxData
                           & L.certsTxBodyL               .~ certs
                           & L.updateTxBodyL              .~ update
                           & L.vldtTxBodyL                .~ validityInterval
                           & L.collateralInputsTxBodyL    .~ collTxIns
                           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses
                                                               apiExtraKeyWitnesses
                           & L.mintTxBodyL                .~ convMintValue apiMintValue
                           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
                           -- TODO: NetworkId for hardware wallets. We don't always want this
                           -- & L.networkIdTxBodyL .~ ...
        pure $ ShelleyTxBody sbe
              ledgerTxBody
              scripts
              (convScriptData (shelleyBasedToCardanoEra sbe) apiTxOuts apiScriptWitnesses)
              txAuxData
              apiScriptValidity

       ShelleyBasedEraBabbage -> do
        update <- convTxUpdateProposal sbe (txUpdateProposal txBodyContent)
        let sData = convScriptData (shelleyBasedToCardanoEra sbe) apiTxOuts apiScriptWitnesses
        scriptIntegrityHash <-
          case sData of
            TxBodyNoScriptData -> pure SNothing
            TxBodyScriptData _sDataSupported datums redeemers ->
              withShelleyBasedEraConstraintsForLedger sbe
                $ convPParamsToScriptIntegrityHash
                    sbe
                    apiProtocolParameters
                    redeemers
                    datums
                    languages
        let ledgerTxBody = mkTxBody ShelleyBasedEraBabbage txBodyContent txAuxData
                           & L.certsTxBodyL               .~ certs
                           & L.updateTxBodyL              .~ update
                           & L.vldtTxBodyL                .~ validityInterval
                           & L.collateralInputsTxBodyL    .~ collTxIns
                           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses
                                                               apiExtraKeyWitnesses
                           & L.mintTxBodyL                .~ convMintValue apiMintValue
                           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
                           & L.referenceInputsTxBodyL     .~ refTxIns
                           & L.collateralReturnTxBodyL    .~ returnCollateral
                           & L.totalCollateralTxBodyL     .~ totalCollateral
                           -- TODO: NetworkId for hardware wallets. We don't always want this
                           -- & L.networkIdTxBodyL .~ ...
        pure $ ShelleyTxBody sbe
              ledgerTxBody
              scripts
              sData
              txAuxData
              apiScriptValidity

       ShelleyBasedEraConway -> do
        let sData = convScriptData (shelleyBasedToCardanoEra sbe) apiTxOuts apiScriptWitnesses
        scriptIntegrityHash <-
          case sData of
            TxBodyNoScriptData -> pure SNothing
            TxBodyScriptData _sDataSupported datums redeemers ->
              withShelleyBasedEraConstraintsForLedger sbe
                $ convPParamsToScriptIntegrityHash
                    sbe
                    apiProtocolParameters
                    redeemers
                    datums
                    languages
        let ledgerTxBody = mkTxBody ShelleyBasedEraConway txBodyContent txAuxData
                          --  & L.conwayCertsTxBodyL         .~ conwayCerts -- TODO CIP-1694
                           & L.vldtTxBodyL                .~ validityInterval
                           & L.collateralInputsTxBodyL    .~ collTxIns
                           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses
                                                               apiExtraKeyWitnesses
                           & L.mintTxBodyL                .~ convMintValue apiMintValue
                           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
                           & L.referenceInputsTxBodyL     .~ refTxIns
                           & L.collateralReturnTxBodyL    .~ returnCollateral
                           & L.totalCollateralTxBodyL     .~ totalCollateral
                           -- TODO: NetworkId for hardware wallets. We don't always want this
                           -- & L.networkIdTxBodyL .~ ...
        pure $ ShelleyTxBody sbe
          ledgerTxBody
          scripts
          sData
          txAuxData
          apiScriptValidity

validateTxBodyContent
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError ()
validateTxBodyContent sbe txBodContent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txOuts,
                             txProtocolParams,
                             txMintValue,
                             txMetadata} =
  let witnesses = collectTxBodyScriptWitnesses sbe txBodContent
      languages = Set.fromList
                    [ toAlonzoLanguage (AnyPlutusScriptVersion v)
                    | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
                    ]
  in case sbe of
       ShelleyBasedEraShelley -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts sbe txOuts
         validateMetadata txMetadata
       ShelleyBasedEraAllegra -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts sbe txOuts
         validateMetadata txMetadata
       ShelleyBasedEraMary -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts sbe txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
       ShelleyBasedEraAlonzo -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts sbe txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
         validateTxInsCollateral txInsCollateral languages
         validateProtocolParameters txProtocolParams languages
       ShelleyBasedEraBabbage -> do
         validateTxIns txIns
         guardShelleyTxInsOverflow (map fst txIns)
         validateTxOuts sbe txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
         validateTxInsCollateral txInsCollateral languages
         validateProtocolParameters txProtocolParams languages
       ShelleyBasedEraConway -> do
         validateTxIns txIns
         validateTxOuts sbe txOuts
         validateMetadata txMetadata
         validateMintValue txMintValue
         validateTxInsCollateral txInsCollateral languages
         validateProtocolParameters txProtocolParams languages

validateMetadata :: TxMetadataInEra era -> Either TxBodyError ()
validateMetadata txMetadata =
  case txMetadata of
    TxMetadataNone      -> return ()
    TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

validateProtocolParameters
  :: BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Set Alonzo.Language
  -> Either TxBodyError ()
validateProtocolParameters txProtocolParams languages =
  case txProtocolParams of
    BuildTxWith Nothing | not (Set.null languages)
      -> Left TxBodyMissingProtocolParams
    _ -> return () --TODO alonzo: validate protocol params for the Alonzo era.
                     --             All the necessary params must be provided.


validateTxIns :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))] -> Either TxBodyError ()
validateTxIns txIns =
  sequence_ [ inputIndexDoesNotExceedMax txIns
            , txBodyContentHasTxIns txIns
            ]

validateTxInsCollateral
  :: TxInsCollateral era -> Set Alonzo.Language -> Either TxBodyError ()
validateTxInsCollateral txInsCollateral languages =
  case txInsCollateral of
    TxInsCollateralNone ->
      unless (Set.null languages) (Left TxBodyEmptyTxInsCollateral)
    TxInsCollateral _ collateralTxIns ->
      guardShelleyTxInsOverflow collateralTxIns

validateTxOuts :: ShelleyBasedEra era -> [TxOut CtxTx era] -> Either TxBodyError ()
validateTxOuts era txOuts = do
  let cEra = shelleyBasedToCardanoEra era
  cardanoEraConstraints cEra $
    sequence_ [ do positiveOutput (txOutValueToValue v) txout
                   outputDoesNotExceedMax (txOutValueToValue v) txout
                 | txout@(TxOut _ v _ _) <- txOuts
                 ]

validateMintValue :: TxMintValue build era -> Either TxBodyError ()
validateMintValue txMintValue =
  case txMintValue of
    TxMintNone        -> return ()
    TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError


inputIndexDoesNotExceedMax :: [(TxIn, a)] -> Either TxBodyError ()
inputIndexDoesNotExceedMax txIns =
   for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
                guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

outputDoesNotExceedMax
  :: IsCardanoEra era => Value -> TxOut CtxTx era -> Either TxBodyError ()
outputDoesNotExceedMax v txout =
  case [ q | (_,q) <- valueToList v, q > maxTxOut ] of
    []  -> Right ()
    q:_ -> Left (TxBodyOutputOverflow q (txOutInAnyEra txout))

positiveOutput
  :: IsCardanoEra era
  => Value -> TxOut CtxTx era -> Either TxBodyError ()
positiveOutput v txout =
    case [ q | (_, q) <- valueToList v, q < 0 ] of
      []  -> Right ()
      q:_ -> Left (TxBodyOutputNegative q (txOutInAnyEra txout))

txBodyContentHasTxIns :: TxIns BuildTx era -> Either TxBodyError ()
txBodyContentHasTxIns txIns = guard (not (null txIns)) ?! TxBodyEmptyTxIns

maxShelleyTxInIx :: Word
maxShelleyTxInIx = fromIntegral $ maxBound @Word16

maxTxOut :: Quantity
maxTxOut = fromIntegral (maxBound :: Word64)

createAndValidateTransactionBody
  :: forall era.
     IsCardanoEra era
  => TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
createAndValidateTransactionBody =
    case cardanoEraStyle (cardanoEra :: CardanoEra era) of
      LegacyByronEra      -> makeByronTransactionBody
      ShelleyBasedEra sbe -> makeShelleyTransactionBody sbe

pattern TxBody :: TxBodyContent ViewTx era -> TxBody era
pattern TxBody txbodycontent <- (getTxBodyContent -> txbodycontent)
{-# COMPLETE TxBody #-}

getTxBodyContent :: TxBody era -> TxBodyContent ViewTx era
getTxBodyContent (ByronTxBody body) = getByronTxBodyContent body
getTxBodyContent (ShelleyTxBody sbe body _scripts scriptdata mAux scriptValidity) =
    fromLedgerTxBody sbe scriptValidity body scriptdata mAux

fromLedgerTxBody
  :: ShelleyBasedEra era
  -> TxScriptValidity era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxBodyScriptData era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> TxBodyContent ViewTx era
fromLedgerTxBody sbe scriptValidity body scriptdata mAux =
    TxBodyContent
      { txIns                 = fromLedgerTxIns                 sbe body
      , txInsCollateral       = fromLedgerTxInsCollateral       sbe body
      , txInsReference        = fromLedgerTxInsReference        sbe body
      , txOuts                = fromLedgerTxOuts                sbe body scriptdata
      , txTotalCollateral     = fromLedgerTxTotalCollateral     sbe body
      , txReturnCollateral    = fromLedgerTxReturnCollateral    sbe body
      , txFee                 = fromLedgerTxFee                 sbe body
      , txValidityRange       = fromLedgerTxValidityRange       sbe body
      , txWithdrawals         = fromLedgerTxWithdrawals         sbe body
      , txCertificates        = fromLedgerTxCertificates        sbe body
      , txUpdateProposal      = maybeFromLedgerTxUpdateProposal sbe body
      , txMintValue           = fromLedgerTxMintValue           sbe body
      , txExtraKeyWits        = fromLedgerTxExtraKeyWitnesses   sbe body
      , txProtocolParams      = ViewTx
      , txMetadata
      , txAuxScripts
      , txScriptValidity      = scriptValidity
      , txProposalProcedures  = fromLedgerProposalProcedures  sbe body
      , txVotingProcedures    = fromLedgerVotingProcedures    sbe body
      }
  where
    (txMetadata, txAuxScripts) = fromLedgerTxAuxiliaryData sbe mAux

fromLedgerProposalProcedures
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era [Proposal era])
fromLedgerProposalProcedures sbe body =
  inShelleyBasedEraEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w
      $ Featured w
      $ fmap Proposal
      $ toList
      $ body ^. L.proposalProceduresTxBodyL

fromLedgerVotingProcedures :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era (VotingProcedures era))
fromLedgerVotingProcedures sbe body =
  inShelleyBasedEraEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w
      $ Featured w
      $ VotingProcedures
      $ body ^. L.votingProceduresTxBodyL

fromLedgerTxIns
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> [(TxIn,BuildTxWith ViewTx (Witness WitCtxTxIn era))]
fromLedgerTxIns sbe body =
    [ (fromShelleyTxIn input, ViewTx)
    | input <- Set.toList (inputs_ sbe body) ]
  where
    inputs_ :: ShelleyBasedEra era
           -> Ledger.TxBody (ShelleyLedgerEra era)
           -> Set (Ledger.TxIn StandardCrypto)
    inputs_ ShelleyBasedEraShelley = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraAllegra = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraMary    = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraAlonzo  = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraBabbage = view L.inputsTxBodyL
    inputs_ ShelleyBasedEraConway  = view L.inputsTxBodyL


fromLedgerTxInsCollateral
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxInsCollateral era
fromLedgerTxInsCollateral sbe body =
    case collateralSupportedInEra (shelleyBasedToCardanoEra sbe) of
      Nothing        -> TxInsCollateralNone
      Just supported ->
        TxInsCollateral supported $ map fromShelleyTxIn collateral_
  where
    collateral_ :: [Ledger.TxIn StandardCrypto]
    collateral_ = case sbe of
      ShelleyBasedEraShelley -> []
      ShelleyBasedEraAllegra -> []
      ShelleyBasedEraMary    -> []
      ShelleyBasedEraAlonzo  -> toList $ body ^. L.collateralInputsTxBodyL
      ShelleyBasedEraBabbage -> toList $ body ^. L.collateralInputsTxBodyL
      ShelleyBasedEraConway  -> toList $ body ^. L.collateralInputsTxBodyL

fromLedgerTxInsReference
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxInsReference ViewTx era
fromLedgerTxInsReference sbe txBody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxInsReferenceNone)
    (\w -> TxInsReference w $ map fromShelleyTxIn . Set.toList $ txBody ^. L.referenceInputsTxBodyL)
    sbe

fromLedgerTxOuts
  :: forall era.
     ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxBodyScriptData era
  -> [TxOut CtxTx era]
fromLedgerTxOuts sbe body scriptdata =
  case sbe of
    ShelleyBasedEraShelley ->
      [ fromShelleyTxOut sbe txout | txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraAllegra ->
      [ fromShelleyTxOut sbe txout | txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraMary ->
      [ fromShelleyTxOut sbe txout | txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraAlonzo ->
      [ fromAlonzoTxOut
          MaryEraOnwardsAlonzo
          AlonzoEraOnwardsAlonzo
          txdatums
          txout
      | let txdatums = selectTxDatums scriptdata
      , txout <- toList (body ^. L.outputsTxBodyL) ]

    ShelleyBasedEraBabbage ->
      [ fromBabbageTxOut
          MaryEraOnwardsBabbage
          AlonzoEraOnwardsBabbage
          BabbageEraOnwardsBabbage
          txdatums
          txouts
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (body ^. L.outputsTxBodyL)
      ]

    ShelleyBasedEraConway ->
      [ fromBabbageTxOut
          MaryEraOnwardsConway
          AlonzoEraOnwardsConway
          BabbageEraOnwardsConway
          txdatums
          txouts
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (body ^. L.outputsTxBodyL)
      ]
  where
    selectTxDatums  TxBodyNoScriptData                            = Map.empty
    selectTxDatums (TxBodyScriptData _ (Alonzo.TxDats' datums) _) = datums

fromAlonzoTxOut :: forall era ledgerera.
                   IsShelleyBasedEra era
                => L.AlonzoEraTxOut ledgerera
                => Ledger.EraCrypto ledgerera ~ StandardCrypto
                => Ledger.Value ledgerera ~ MaryValue StandardCrypto
                => MaryEraOnwards era
                -> AlonzoEraOnwards era
                -> Map (L.DataHash StandardCrypto)
                       (L.Data ledgerera)
                -> L.TxOut ledgerera
                -> TxOut CtxTx era
fromAlonzoTxOut multiAssetInEra scriptDataInEra txdatums txOut =
   TxOut (fromShelleyAddr shelleyBasedEra (txOut ^. L.addrTxOutL))
         (TxOutValue multiAssetInEra (fromMaryValue (txOut ^. L.valueTxOutL)))
         (fromAlonzoTxOutDatum scriptDataInEra (txOut ^. L.dataHashTxOutL))
         ReferenceScriptNone
  where
    fromAlonzoTxOutDatum :: AlonzoEraOnwards era
                         -> StrictMaybe (L.DataHash StandardCrypto)
                         -> TxOutDatum CtxTx era
    fromAlonzoTxOutDatum _ SNothing = TxOutDatumNone
    fromAlonzoTxOutDatum w (SJust dh)
      | Just d <- Map.lookup dh txdatums  = TxOutDatumInTx' w (ScriptDataHash dh) (fromAlonzoData d)
      | otherwise                         = TxOutDatumHash w (ScriptDataHash dh)

fromBabbageTxOut
  :: forall ledgerera era.
     L.BabbageEraTxOut ledgerera
  => IsShelleyBasedEra era
  => ShelleyLedgerEra era ~ ledgerera
  => Ledger.EraCrypto ledgerera ~ StandardCrypto
  => Ledger.Value ledgerera ~ MaryValue StandardCrypto
  => MaryEraOnwards era
  -> AlonzoEraOnwards era
  -> BabbageEraOnwards era
  -> Map (L.DataHash StandardCrypto)
         (L.Data ledgerera)
  -> L.TxOut ledgerera
  -> TxOut CtxTx era
fromBabbageTxOut multiAssetInEra scriptDataInEra inlineDatumsInEra txdatums txout =
   TxOut
     (fromShelleyAddr shelleyBasedEra (txout ^. L.addrTxOutL))
     (TxOutValue multiAssetInEra (fromMaryValue (txout ^. L.valueTxOutL)))
     babbageTxOutDatum
     (case txout ^. L.referenceScriptTxOutL of
       SNothing -> ReferenceScriptNone
       SJust rScript -> fromShelleyScriptToReferenceScript shelleyBasedEra rScript
     )
 where
   -- NOTE: This is different to 'fromBabbageTxOutDatum' as it may resolve
   -- 'DatumHash' values using the datums included in the transaction.
   babbageTxOutDatum :: TxOutDatum CtxTx era
   babbageTxOutDatum =
     case txout ^. L.datumTxOutL of
       L.NoDatum -> TxOutDatumNone
       L.DatumHash dh -> resolveDatumInTx dh
       L.Datum d ->
         TxOutDatumInline inlineDatumsInEra $
           binaryDataToScriptData inlineDatumsInEra d

   resolveDatumInTx :: L.DataHash StandardCrypto -> TxOutDatum CtxTx era
   resolveDatumInTx dh
      | Just d <- Map.lookup dh txdatums
                  = TxOutDatumInTx' scriptDataInEra (ScriptDataHash dh) (fromAlonzoData d)
      | otherwise = TxOutDatumHash scriptDataInEra (ScriptDataHash dh)


fromLedgerTxTotalCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxTotalCollateral era
fromLedgerTxTotalCollateral sbe txbody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxTotalCollateralNone)
    (\w ->
      case txbody ^. L.totalCollateralTxBodyL of
        SNothing -> TxTotalCollateralNone
        SJust totColl -> TxTotalCollateral w $ fromShelleyLovelace totColl
    )
    sbe

fromLedgerTxReturnCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxReturnCollateral CtxTx era
fromLedgerTxReturnCollateral sbe txbody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxReturnCollateralNone)
    (\w ->
      case txbody ^. L.collateralReturnTxBodyL of
        SNothing -> TxReturnCollateralNone
        SJust collReturnOut -> TxReturnCollateral w $ fromShelleyTxOut sbe collReturnOut
    )
    sbe

fromLedgerTxFee
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxFee era
fromLedgerTxFee sbe body =
  shelleyBasedEraConstraints sbe
    $ TxFeeExplicit sbe
    $ fromShelleyLovelace $ body ^. L.feeTxBodyL

fromLedgerTxValidityRange
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
fromLedgerTxValidityRange sbe body =
  case sbe of
    ShelleyBasedEraShelley ->
      ( TxValidityNoLowerBound
      , TxValidityUpperBound ValidityUpperBoundInShelleyEra $ body ^. L.ttlTxBodyL
      )

    ShelleyBasedEraAllegra ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound AllegraEraOnwardsAllegra s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAllegraEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraMary ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound AllegraEraOnwardsMary s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInMaryEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraAlonzo ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound AllegraEraOnwardsAlonzo s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInAlonzoEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraBabbage ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound AllegraEraOnwardsBabbage s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInBabbageEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInBabbageEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

    ShelleyBasedEraConway ->
      ( case invalidBefore of
          SNothing -> TxValidityNoLowerBound
          SJust s  -> TxValidityLowerBound AllegraEraOnwardsConway s
      , case invalidHereafter of
          SNothing -> TxValidityNoUpperBound ValidityNoUpperBoundInConwayEra
          SJust s  -> TxValidityUpperBound   ValidityUpperBoundInConwayEra s
      )
      where
        L.ValidityInterval{invalidBefore, invalidHereafter} = body ^. L.vldtTxBodyL

fromLedgerAuxiliaryData
  :: ShelleyBasedEra era
  -> L.TxAuxData (ShelleyLedgerEra era)
  -> (Map Word64 TxMetadataValue, [ScriptInEra era])
fromLedgerAuxiliaryData ShelleyBasedEraShelley (L.ShelleyTxAuxData metadata) =
  (fromShelleyMetadata metadata, [])
fromLedgerAuxiliaryData ShelleyBasedEraAllegra (L.AllegraTxAuxData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraAllegra <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraMary (L.AllegraTxAuxData ms ss) =
  ( fromShelleyMetadata ms
  , fromShelleyBasedScript ShelleyBasedEraMary <$> toList ss
  )
fromLedgerAuxiliaryData ShelleyBasedEraAlonzo txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraAlonzo <$>
    toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraBabbage txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraBabbage <$>
    toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraConway txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraConway <$>
    toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )

fromLedgerTxAuxiliaryData
  :: ShelleyBasedEra era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> (TxMetadataInEra era, TxAuxScripts era)
fromLedgerTxAuxiliaryData _ Nothing = (TxMetadataNone, TxAuxScriptsNone)
fromLedgerTxAuxiliaryData sbe (Just auxData) =
  (metadata, auxdata)

  where
    metadata = if null ms then TxMetadataNone else TxMetadataInEra sbe $ TxMetadata ms

    auxdata =
      case sbe of
        ShelleyBasedEraShelley ->
          TxAuxScriptsNone
        ShelleyBasedEraAllegra ->
          case ss of
              [] -> TxAuxScriptsNone
              _  -> TxAuxScripts AuxScriptsInAllegraEra ss
        ShelleyBasedEraMary ->
          case ss of
              [] -> TxAuxScriptsNone
              _  -> TxAuxScripts AuxScriptsInMaryEra ss
        ShelleyBasedEraAlonzo ->
          case ss of
              [] -> TxAuxScriptsNone
              _  -> TxAuxScripts AuxScriptsInAlonzoEra ss
        ShelleyBasedEraBabbage ->
          case ss of
              [] -> TxAuxScriptsNone
              _  -> TxAuxScripts AuxScriptsInBabbageEra ss
        ShelleyBasedEraConway ->
          case ss of
              [] -> TxAuxScriptsNone
              _  -> TxAuxScripts AuxScriptsInConwayEra ss

    (ms, ss) = fromLedgerAuxiliaryData sbe auxData


fromLedgerTxExtraKeyWitnesses :: ShelleyBasedEra era
                              -> Ledger.TxBody (ShelleyLedgerEra era)
                              -> TxExtraKeyWitnesses era
fromLedgerTxExtraKeyWitnesses sbe body =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxExtraKeyWitnessesNone)
    (\w ->
      let keyhashes = body ^. L.reqSignerHashesTxBodyL in
      if Set.null keyhashes
        then TxExtraKeyWitnessesNone
        else
          TxExtraKeyWitnesses w
            [ PaymentKeyHash (Shelley.coerceKeyRole keyhash)
            | keyhash <- Set.toList $ body ^. L.reqSignerHashesTxBodyL
            ]
    )
    sbe

fromLedgerTxWithdrawals
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxWithdrawals ViewTx era
fromLedgerTxWithdrawals sbe body =
  shelleyBasedEraConstraints sbe $
    let withdrawals = body ^. L.withdrawalsTxBodyL in
    if null (L.unWithdrawals withdrawals)
      then TxWithdrawalsNone
      else TxWithdrawals sbe $ fromShelleyWithdrawal withdrawals

fromLedgerTxCertificates
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxCertificates ViewTx era
fromLedgerTxCertificates sbe body =
  shelleyBasedEraConstraints sbe $
    let certificates = body ^. L.certsTxBodyL in
    if null certificates
      then TxCertificatesNone
      else TxCertificates sbe (map (fromShelleyCertificate sbe) $ toList certificates) ViewTx

maybeFromLedgerTxUpdateProposal :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxUpdateProposal era
maybeFromLedgerTxUpdateProposal sbe body =
  caseShelleyToBabbageOrConwayEraOnwards
    (\w ->
      case body ^. L.updateTxBodyL of
        SNothing -> TxUpdateProposalNone
        SJust p -> TxUpdateProposal w (fromLedgerUpdate sbe p)
    )
    (const TxUpdateProposalNone)
    sbe

fromLedgerTxMintValue
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxMintValue ViewTx era
fromLedgerTxMintValue sbe body =
  case sbe of
    ShelleyBasedEraShelley -> TxMintNone
    ShelleyBasedEraAllegra -> TxMintNone
    ShelleyBasedEraMary    -> toMintValue body MaryEraOnwardsMary
    ShelleyBasedEraAlonzo  -> toMintValue body MaryEraOnwardsAlonzo
    ShelleyBasedEraBabbage -> toMintValue body MaryEraOnwardsBabbage
    ShelleyBasedEraConway  -> toMintValue body MaryEraOnwardsConway
  where
    toMintValue txBody maInEra
      | L.isZero mint = TxMintNone
      | otherwise     = TxMintValue maInEra (fromMaryValue mint) ViewTx
      where
        mint = MaryValue 0 (txBody ^. L.mintTxBodyL)


makeByronTransactionBody :: TxBodyContent BuildTx ByronEra
                         -> Either TxBodyError (TxBody ByronEra)
makeByronTransactionBody TxBodyContent { txIns, txOuts } = do
    ins' <- NonEmpty.nonEmpty (map fst txIns) ?! TxBodyEmptyTxIns
    for_ ins' $ \txin@(TxIn _ (TxIx txix)) ->
      guard (fromIntegral txix <= maxByronTxInIx) ?! TxBodyInIxOverflow txin
    let ins'' = fmap toByronTxIn ins'

    outs'  <- NonEmpty.nonEmpty txOuts    ?! TxBodyEmptyTxOuts
    outs'' <- traverse
                (\out -> toByronTxOut out ?! classifyRangeError out)
                outs'
    return $
      ByronTxBody $
        reAnnotate CBOR.byronProtVer $
          Annotated
            (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
            ()
  where
    maxByronTxInIx :: Word
    maxByronTxInIx = fromIntegral (maxBound :: Word32)

    classifyRangeError :: TxOut CtxTx ByronEra -> TxBodyError
    classifyRangeError
      txout@(TxOut (AddressInEra ByronAddressInAnyEra ByronAddress{})
                   (TxOutAdaOnly ByronToAllegraEraByron value) _ _)
      | value < 0        = TxBodyOutputNegative (lovelaceToQuantity value)
                                                (txOutInAnyEra txout)
      | otherwise        = TxBodyOutputOverflow (lovelaceToQuantity value)
                                                (txOutInAnyEra txout)

    classifyRangeError
      (TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _))
             (TxOutValue w _) _ _) = case w of {}

    classifyRangeError
      (TxOut (AddressInEra (ShelleyAddressInEra sbe) ShelleyAddress{})
             _ _ _) = case sbe of {}

getByronTxBodyContent :: Annotated Byron.Tx ByteString
                      -> TxBodyContent ViewTx ByronEra
getByronTxBodyContent (Annotated Byron.UnsafeTx{txInputs, txOutputs} _) =
  TxBodyContent
  { txIns               = [(fromByronTxIn input, ViewTx) | input <- toList txInputs]
  , txInsCollateral     = TxInsCollateralNone
  , txInsReference      = TxInsReferenceNone
  , txOuts              = fromByronTxOut <$> toList txOutputs
  , txReturnCollateral  = TxReturnCollateralNone
  , txTotalCollateral   = TxTotalCollateralNone
  , txFee               = TxFeeImplicit ByronEraOnlyByron
  , txValidityRange     = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInByronEra)
  , txMetadata          = TxMetadataNone
  , txAuxScripts        = TxAuxScriptsNone
  , txExtraKeyWits      = TxExtraKeyWitnessesNone
  , txProtocolParams    = ViewTx
  , txWithdrawals       = TxWithdrawalsNone
  , txCertificates      = TxCertificatesNone
  , txUpdateProposal    = TxUpdateProposalNone
  , txMintValue         = TxMintNone
  , txScriptValidity    = TxScriptValidityNone
  , txProposalProcedures = Nothing
  , txVotingProcedures  = Nothing
  }

convTxIns :: TxIns BuildTx era -> Set (L.TxIn StandardCrypto)
convTxIns txIns = Set.fromList (map (toShelleyTxIn . fst) txIns)

convCollateralTxIns :: TxInsCollateral era -> Set (Ledger.TxIn StandardCrypto)
convCollateralTxIns txInsCollateral =
  case txInsCollateral of
    TxInsCollateralNone -> Set.empty
    TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)

convReturnCollateral
  :: ShelleyBasedEra era
  -> TxReturnCollateral ctx era
  -> StrictMaybe (Ledger.TxOut (ShelleyLedgerEra era))
convReturnCollateral sbe txReturnCollateral =
  case txReturnCollateral of
    TxReturnCollateralNone -> SNothing
    TxReturnCollateral _ colTxOut -> SJust $ withShelleyBasedEraConstraintsForLedger sbe $ toShelleyTxOutAny sbe colTxOut

convTotalCollateral :: TxTotalCollateral era -> StrictMaybe Ledger.Coin
convTotalCollateral txTotalCollateral =
  case txTotalCollateral of
    TxTotalCollateralNone -> SNothing
    TxTotalCollateral _ totCollLovelace -> SJust $ toShelleyLovelace totCollLovelace

convTxOuts
  :: forall ctx era ledgerera. ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era -> [TxOut ctx era] -> Seq.StrictSeq (Ledger.TxOut ledgerera)
convTxOuts sbe txOuts = Seq.fromList $ map (toShelleyTxOutAny sbe) txOuts


convCertificates
  :: ShelleyBasedEra era
  -> TxCertificates build era
  -> Seq.StrictSeq (Shelley.TxCert (ShelleyLedgerEra era))
convCertificates sbe txCertificates = shelleyBasedEraConstraints sbe $
  case txCertificates of
    TxCertificatesNone    -> Seq.empty
    TxCertificates _ cs _ -> Seq.fromList (map toShelleyCertificate cs)


convWithdrawals :: TxWithdrawals build era -> L.Withdrawals StandardCrypto
convWithdrawals txWithdrawals =
  case txWithdrawals of
    TxWithdrawalsNone  -> L.Withdrawals Map.empty
    TxWithdrawals _ ws -> toShelleyWithdrawal ws

convTransactionFee :: ShelleyBasedEra era -> TxFee era -> Ledger.Coin
convTransactionFee sbe = \case
  TxFeeImplicit w  -> noByronEraInShelleyBasedEra sbe w
  TxFeeExplicit _ fee -> toShelleyLovelace fee

convValidityInterval
  :: (TxValidityLowerBound era, TxValidityUpperBound era)
  -> L.ValidityInterval
convValidityInterval (lowerBound, upperBound) =
  L.ValidityInterval
    { invalidBefore = case lowerBound of
                        TxValidityNoLowerBound   -> SNothing
                        TxValidityLowerBound _ s -> SJust s
    , invalidHereafter = case upperBound of
                           TxValidityNoUpperBound _ -> SNothing
                           TxValidityUpperBound _ s -> SJust s
    }

-- | Convert transaction update proposal into ledger update proposal
convTxUpdateProposal
  :: forall era ledgerera. ShelleyLedgerEra era ~ ledgerera
  => Ledger.EraCrypto ledgerera ~ StandardCrypto
  => ShelleyBasedEra era
  -> TxUpdateProposal era
  -> Either TxBodyError (StrictMaybe (Ledger.Update ledgerera))
  -- ^ 'Left' when there's protocol params conversion error, 'Right' otherwise, 'Right SNothing' means that
  -- there's no update proposal
convTxUpdateProposal sbe = \case
  TxUpdateProposalNone -> Right SNothing
  TxUpdateProposal _ p -> bimap TxBodyProtocolParamsConversionError pure $ toLedgerUpdate sbe p

convMintValue :: TxMintValue build era -> MultiAsset StandardCrypto
convMintValue txMintValue =
  case txMintValue of
    TxMintNone        -> mempty
    TxMintValue _ v _ ->
      case toMaryValue v of
        MaryValue _ ma -> ma

convExtraKeyWitnesses :: TxExtraKeyWitnesses era -> Set (Shelley.KeyHash r' StandardCrypto)
convExtraKeyWitnesses txExtraKeyWits =
  case txExtraKeyWits of
    TxExtraKeyWitnessesNone   -> Set.empty
    TxExtraKeyWitnesses _ khs -> Set.fromList
                                   [ Shelley.coerceKeyRole kh
                                   | PaymentKeyHash kh <- khs ]

convScripts
  :: ShelleyLedgerEra era ~ ledgerera
  => [(ScriptWitnessIndex, AnyScriptWitness era)]
  -> [Ledger.Script ledgerera]
convScripts scriptWitnesses =
  catMaybes
    [ toShelleyScript <$> scriptWitnessScript scriptwitness
    | (_, AnyScriptWitness scriptwitness) <- scriptWitnesses
    ]

-- ScriptData collectively refers to datums and/or redeemers
convScriptData
  :: Ledger.Era (ShelleyLedgerEra era)
  => CardanoEra era
  -> [TxOut CtxTx era]
  -> [(ScriptWitnessIndex, AnyScriptWitness era)]
  -> TxBodyScriptData era
convScriptData era txOuts scriptWitnesses =
  forEraInEon era
    TxBodyNoScriptData
    (\w ->
      let redeemers =
            Alonzo.Redeemers $
              Map.fromList
                [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
                | (idx, AnyScriptWitness
                          (PlutusScriptWitness _ _ _ _ d e)) <- scriptWitnesses
                ]
          datums =
            Alonzo.TxDats $
              Map.fromList
                [ (L.hashData d', d')
                | d <- scriptdata
                , let d' = toAlonzoData d
                ]

          scriptdata :: [HashableScriptData]
          scriptdata =
              [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
           ++ [ d | (_, AnyScriptWitness
                          (PlutusScriptWitness
                             _ _ _ (ScriptDatumForTxIn d) _ _)) <- scriptWitnesses
                  ]
      in TxBodyScriptData w datums redeemers
    )

convPParamsToScriptIntegrityHash :: ()
  => ShelleyBasedEra era
  -> L.AlonzoEraPParams (ShelleyLedgerEra era)
  => BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Alonzo.Redeemers (ShelleyLedgerEra era)
  -> Alonzo.TxDats (ShelleyLedgerEra era)
  -> Set Alonzo.Language
  -> Either TxBodyError (StrictMaybe (L.ScriptIntegrityHash (Ledger.EraCrypto (ShelleyLedgerEra era))))
convPParamsToScriptIntegrityHash sbe txProtocolParams redeemers datums languages = do
  lang <- case txProtocolParams of
               BuildTxWith Nothing -> return Nothing
               BuildTxWith (Just (LedgerProtocolParameters pp)) ->
                 case sbe of
                   ShelleyBasedEraShelley ->
                     return Nothing
                   ShelleyBasedEraAllegra ->
                     return Nothing
                   ShelleyBasedEraMary ->
                     return Nothing
                   ShelleyBasedEraAlonzo ->
                     return . Just $ L.getLanguageView pp
                   ShelleyBasedEraBabbage ->
                     return . Just $ L.getLanguageView pp
                   ShelleyBasedEraConway ->
                     return . Just $ L.getLanguageView pp

  case lang of
    Nothing -> return SNothing
    Just l ->
      pure $ Alonzo.hashScriptIntegrity
        (Set.map
           l
           languages
        )
        redeemers
        datums

convLanguages :: [(ScriptWitnessIndex, AnyScriptWitness era)] -> Set Alonzo.Language
convLanguages witnesses =
  Set.fromList
    [ toAlonzoLanguage (AnyPlutusScriptVersion v)
    | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
    ]

convReferenceInputs :: TxInsReference build era -> Set (Ledger.TxIn StandardCrypto)
convReferenceInputs txInsReference =
  case txInsReference of
    TxInsReferenceNone -> mempty
    TxInsReference _ refTxins -> Set.fromList $ map toShelleyTxIn refTxins

guardShelleyTxInsOverflow :: [TxIn] -> Either TxBodyError ()
guardShelleyTxInsOverflow txIns = do
    for_ txIns $ \txin@(TxIn _ (TxIx txix)) ->
      guard (txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

-- | A helper function that constructs a TxBody with all of the fields that are common for
-- all eras
mkCommonTxBody ::
     ( L.EraTxBody (ShelleyLedgerEra era)
     , L.EraTxAuxData (ShelleyLedgerEra era)
     , L.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
     )
  => ShelleyBasedEra era
  -> TxIns BuildTx era
  -> [TxOut ctx era]
  -> TxFee era
  -> TxWithdrawals build era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> L.TxBody (ShelleyLedgerEra era)
mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData =
  L.mkBasicTxBody
  & L.inputsTxBodyL .~ convTxIns txIns
  & L.outputsTxBodyL .~ convTxOuts sbe txOuts
  & L.feeTxBodyL .~ convTransactionFee sbe txFee
  & L.withdrawalsTxBodyL .~ convWithdrawals txWithdrawals
  & L.auxDataHashTxBodyL .~ maybe SNothing (SJust . Ledger.hashTxAuxData) txAuxData


makeShelleyTransactionBody :: forall era. ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeShelleyTransactionBody sbe@ShelleyBasedEraShelley
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (_, upperBound),
                             txMetadata,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    return $
      ShelleyTxBody sbe
        (mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
           & L.certsTxBodyL  .~ convCertificates sbe txCertificates
           & L.updateTxBodyL .~ update
           & L.ttlTxBodyL    .~ case upperBound of
                                  TxValidityNoUpperBound era' -> case era' of {}
                                  TxValidityUpperBound _ ttl  -> ttl
        )
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts_ :: [Ledger.Script StandardShelley]
    scripts_ = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses sbe txbodycontent
      ]

    txAuxData :: Maybe (L.TxAuxData StandardShelley)
    txAuxData = toAuxiliaryData sbe txMetadata TxAuxScriptsNone

makeShelleyTransactionBody sbe@ShelleyBasedEraAllegra
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal
                           } = do

    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    return $
      ShelleyTxBody sbe
        (mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
           & L.certsTxBodyL  .~ convCertificates sbe txCertificates
           & L.vldtTxBodyL   .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL .~ update
        )
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts_ :: [Ledger.Script StandardAllegra]
    scripts_ = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses sbe txbodycontent
      ]

    txAuxData :: Maybe (L.TxAuxData StandardAllegra)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts

makeShelleyTransactionBody sbe@ShelleyBasedEraMary
                           txbodycontent@TxBodyContent {
                             txIns,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue
                           } = do

    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    return $
      ShelleyTxBody sbe
        (mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
           & L.certsTxBodyL  .~ convCertificates sbe txCertificates
           & L.vldtTxBodyL   .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL .~ update
           & L.mintTxBodyL   .~ convMintValue txMintValue
        )
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
  where
    scripts :: [Ledger.Script StandardMary]
    scripts = List.nub $ catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness)
          <- collectTxBodyScriptWitnesses sbe txbodycontent
      ]

    txAuxData :: Maybe (L.TxAuxData StandardMary)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts

makeShelleyTransactionBody sbe@ShelleyBasedEraAlonzo
                           txbodycontent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txExtraKeyWits,
                             txProtocolParams,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue,
                             txScriptValidity
                           } = do

    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    scriptIntegrityHash <- convPParamsToScriptIntegrityHash sbe txProtocolParams redeemers datums languages
    return $
      ShelleyTxBody sbe
        (mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
           & L.collateralInputsTxBodyL    .~ convCollateralTxIns txInsCollateral
           & L.certsTxBodyL               .~ convCertificates sbe txCertificates
           & L.vldtTxBodyL                .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL              .~ update
           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses txExtraKeyWits
           & L.mintTxBodyL                .~ convMintValue txMintValue
           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
           -- TODO Alonzo: support optional network id in TxBodyContent
           -- & L.networkIdTxBodyL .~ SNothing
        )
        scripts
        (TxBodyScriptData AlonzoEraOnwardsAlonzo datums redeemers)
        txAuxData
        txScriptValidity
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AlonzoEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script StandardAlonzo]
    scripts = List.nub $ catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    datums :: Alonzo.TxDats StandardAlonzo
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
        [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
     ++ [ d | (_, AnyScriptWitness
                    (PlutusScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Alonzo.Redeemers StandardAlonzo
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set Alonzo.Language
    languages =
      Set.fromList
        [ toAlonzoLanguage (AnyPlutusScriptVersion v)
        | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
        ]

    txAuxData :: Maybe (L.TxAuxData StandardAlonzo)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts

makeShelleyTransactionBody sbe@ShelleyBasedEraBabbage
                            txbodycontent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txInsReference,
                             txReturnCollateral,
                             txTotalCollateral,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txExtraKeyWits,
                             txProtocolParams,
                             txWithdrawals,
                             txCertificates,
                             txUpdateProposal,
                             txMintValue,
                             txScriptValidity
                           } = do

    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    scriptIntegrityHash <- convPParamsToScriptIntegrityHash sbe txProtocolParams redeemers datums languages
    return $
      ShelleyTxBody sbe
        (mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
           & L.collateralInputsTxBodyL .~
               case txInsCollateral of
                TxInsCollateralNone     -> Set.empty
                TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)
           & L.referenceInputsTxBodyL     .~ convReferenceInputs txInsReference
           & L.collateralReturnTxBodyL    .~ convReturnCollateral sbe txReturnCollateral
           & L.totalCollateralTxBodyL     .~ convTotalCollateral txTotalCollateral
           & L.certsTxBodyL               .~ convCertificates sbe txCertificates
           & L.vldtTxBodyL                .~ convValidityInterval (lowerBound, upperBound)
           & L.updateTxBodyL              .~ update
           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses txExtraKeyWits
           & L.mintTxBodyL                .~ convMintValue txMintValue
           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
           -- TODO Babbage: support optional network id in TxBodyContent
           -- & L.networkIdTxBodyL .~ SNothing
        )
        scripts
        (TxBodyScriptData AlonzoEraOnwardsBabbage
          datums redeemers)
        txAuxData
        txScriptValidity
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness BabbageEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script StandardBabbage]
    scripts = List.nub $ catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats StandardBabbage
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (L.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
        [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
     ++ [ d | (_, AnyScriptWitness
                    (PlutusScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Alonzo.Redeemers StandardBabbage
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set Alonzo.Language
    languages =
      Set.fromList $ catMaybes
        [ getScriptLanguage sw
        | (_, AnyScriptWitness sw) <- witnesses
        ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Alonzo.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData StandardBabbage)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts


makeShelleyTransactionBody sbe@ShelleyBasedEraConway
                            txbodycontent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txInsReference,
                             txReturnCollateral,
                             txTotalCollateral,
                             txOuts,
                             txFee,
                             txValidityRange = (lowerBound, upperBound),
                             txMetadata,
                             txAuxScripts,
                             txExtraKeyWits,
                             txProtocolParams,
                             txWithdrawals,
                             txCertificates,
                             txMintValue,
                             txScriptValidity,
                             txProposalProcedures,
                             txVotingProcedures
                           } = do

    validateTxBodyContent sbe txbodycontent
    scriptIntegrityHash <- convPParamsToScriptIntegrityHash sbe txProtocolParams redeemers datums languages
    return $
      ShelleyTxBody sbe
        (mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
           & L.collateralInputsTxBodyL .~
               case txInsCollateral of
                TxInsCollateralNone     -> Set.empty
                TxInsCollateral _ txins -> Set.fromList (map toShelleyTxIn txins)
           & L.referenceInputsTxBodyL     .~ convReferenceInputs txInsReference
           & L.collateralReturnTxBodyL    .~ convReturnCollateral sbe txReturnCollateral
           & L.totalCollateralTxBodyL     .~ convTotalCollateral txTotalCollateral
           & L.certsTxBodyL               .~ convCertificates sbe txCertificates
           & L.vldtTxBodyL                .~ convValidityInterval (lowerBound, upperBound)
           & L.reqSignerHashesTxBodyL     .~ convExtraKeyWitnesses txExtraKeyWits
           & L.mintTxBodyL                .~ convMintValue txMintValue
           & L.scriptIntegrityHashTxBodyL .~ scriptIntegrityHash
           & L.votingProceduresTxBodyL    .~ unVotingProcedures @era (maybe emptyVotingProcedures unFeatured txVotingProcedures)
           & L.proposalProceduresTxBodyL  .~ Seq.fromList (fmap unProposal (maybe [] unFeatured txProposalProcedures))
           -- TODO Conway: support optional network id in TxBodyContent
           -- & L.networkIdTxBodyL .~ SNothing
        )
        scripts
        (TxBodyScriptData AlonzoEraOnwardsConway
          datums redeemers)
        txAuxData
        txScriptValidity
  where
    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness ConwayEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script StandardConway]
    scripts = catMaybes
      [ toShelleyScript <$> scriptWitnessScript scriptwitness
      | (_, AnyScriptWitness scriptwitness) <- witnesses
      ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats StandardConway
    datums =
      Alonzo.TxDats $
        Map.fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
        [ d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts ]
     ++ [ d | (_, AnyScriptWitness
                    (PlutusScriptWitness
                       _ _ _ (ScriptDatumForTxIn d) _ _)) <- witnesses
            ]

    redeemers :: Alonzo.Redeemers StandardConway
    redeemers =
      Alonzo.Redeemers $
        Map.fromList
          [ (toAlonzoRdmrPtr idx, (toAlonzoData d, toAlonzoExUnits e))
          | (idx, AnyScriptWitness
                    (PlutusScriptWitness _ _ _ _ d e)) <- witnesses
          ]

    languages :: Set Alonzo.Language
    languages =
      Set.fromList $ catMaybes
        [ getScriptLanguage sw
        | (_, AnyScriptWitness sw) <- witnesses
        ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Alonzo.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData StandardConway)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts


-- | A variant of 'toShelleyTxOutAny that is used only internally to this module
-- that works with a 'TxOut' in any context (including CtxTx) by ignoring
-- embedded datums (taking only their hash).
--
toShelleyTxOutAny :: forall ctx era ledgerera.
                   ShelleyLedgerEra era ~ ledgerera
                => ShelleyBasedEra era
                -> TxOut ctx era
                -> Ledger.TxOut ledgerera
toShelleyTxOutAny sbe (TxOut _ (TxOutAdaOnly ByronToAllegraEraByron _) _ _) =
    case sbe of {}

toShelleyTxOutAny _ (TxOut addr (TxOutAdaOnly ByronToAllegraEraShelley value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOutAny _ (TxOut addr (TxOutAdaOnly ByronToAllegraEraAllegra value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toShelleyLovelace value)

toShelleyTxOutAny _ (TxOut addr (TxOutValue MaryEraOnwardsMary value) _ _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)

toShelleyTxOutAny _ (TxOut addr (TxOutValue MaryEraOnwardsAlonzo value) txoutdata _) =
    L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
    & L.dataHashTxOutL .~ toAlonzoTxOutDataHash' txoutdata

toShelleyTxOutAny sbe (TxOut addr (TxOutValue MaryEraOnwardsBabbage value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra sbe
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum' txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

toShelleyTxOutAny sbe (TxOut addr (TxOutValue MaryEraOnwardsConway value) txoutdata refScript) =
    let cEra = shelleyBasedToCardanoEra sbe
    in L.mkBasicTxOut (toShelleyAddr addr) (toMaryValue value)
       & L.datumTxOutL .~ toBabbageTxOutDatum' txoutdata
       & L.referenceScriptTxOutL .~ refScriptToShelleyScript cEra refScript

toAlonzoTxOutDataHash' :: TxOutDatum ctx AlonzoEra
                       -> StrictMaybe (L.DataHash StandardCrypto)
toAlonzoTxOutDataHash'  TxOutDatumNone                          = SNothing
toAlonzoTxOutDataHash' (TxOutDatumHash _ (ScriptDataHash dh))   = SJust dh
toAlonzoTxOutDataHash' (TxOutDatumInTx' _ (ScriptDataHash dh) _) = SJust dh
toAlonzoTxOutDataHash' (TxOutDatumInline inlineDatumSupp _sd) =
  case inlineDatumSupp :: BabbageEraOnwards AlonzoEra of {}

-- TODO: Consolidate with alonzo function and rename
toBabbageTxOutDatum'
  :: (L.Era (ShelleyLedgerEra era), Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => TxOutDatum ctx era -> Babbage.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum'  TxOutDatumNone = Babbage.NoDatum
toBabbageTxOutDatum' (TxOutDatumHash _ (ScriptDataHash dh)) = Babbage.DatumHash dh
toBabbageTxOutDatum' (TxOutDatumInTx' _ (ScriptDataHash dh) _) = Babbage.DatumHash dh
toBabbageTxOutDatum' (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd


-- ----------------------------------------------------------------------------
-- Script witnesses within the tx body
--

-- | A 'ScriptWitness' in any 'WitCtx'. This lets us handle heterogeneous
-- collections of script witnesses from multiple contexts.
--
data AnyScriptWitness era where
     AnyScriptWitness :: ScriptWitness witctx era -> AnyScriptWitness era

deriving instance Show (AnyScriptWitness era)

-- | Identify the location of a 'ScriptWitness' within the context of a
-- 'TxBody'. These are indexes of the objects within the transaction that
-- need or can use script witnesses: inputs, minted assets, withdrawals and
-- certificates. These are simple numeric indices, enumerated from zero.
-- Thus the indices are not stable if the transaction body is modified.
--
data ScriptWitnessIndex =

     -- | The n'th transaction input, in the order of the 'TxId's.
     ScriptWitnessIndexTxIn !Word

     -- | The n'th minting 'PolicyId', in the order of the 'PolicyId's.
   | ScriptWitnessIndexMint !Word

     -- | The n'th certificate, in the list order of the certificates.
   | ScriptWitnessIndexCertificate !Word

     -- | The n'th withdrawal, in the order of the 'StakeAddress's.
   | ScriptWitnessIndexWithdrawal !Word
  deriving (Eq, Ord, Show)

instance ToJSON ScriptWitnessIndex where
  toJSON = \case
    ScriptWitnessIndexTxIn n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexTxIn"
      , "value" .= n
      ]
    ScriptWitnessIndexMint n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexMint"
      , "value" .= n
      ]
    ScriptWitnessIndexCertificate n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexCertificate"
      , "value" .= n
      ]
    ScriptWitnessIndexWithdrawal n ->
      object
      [ "kind" .= Aeson.String "ScriptWitnessIndexWithdrawal"
      , "value" .= n
      ]

renderScriptWitnessIndex :: ScriptWitnessIndex -> String
renderScriptWitnessIndex (ScriptWitnessIndexTxIn index) =
  "transaction input " <> show index <> " (in ascending order of the TxIds)"
renderScriptWitnessIndex (ScriptWitnessIndexMint index) =
  "policyId " <> show index <> " (in ascending order of the PolicyIds)"
renderScriptWitnessIndex (ScriptWitnessIndexCertificate index) =
  "certificate " <> show index <> " (in the list order of the certificates)"
renderScriptWitnessIndex (ScriptWitnessIndexWithdrawal index) =
  "withdrawal " <> show index <> " (in ascending order of the StakeAddresses)"

toAlonzoRdmrPtr :: ScriptWitnessIndex -> Alonzo.RdmrPtr
toAlonzoRdmrPtr widx =
    case widx of
      ScriptWitnessIndexTxIn        n -> Alonzo.RdmrPtr Alonzo.Spend (fromIntegral n)
      ScriptWitnessIndexMint        n -> Alonzo.RdmrPtr Alonzo.Mint  (fromIntegral n)
      ScriptWitnessIndexCertificate n -> Alonzo.RdmrPtr Alonzo.Cert  (fromIntegral n)
      ScriptWitnessIndexWithdrawal  n -> Alonzo.RdmrPtr Alonzo.Rewrd (fromIntegral n)

fromAlonzoRdmrPtr :: Alonzo.RdmrPtr -> ScriptWitnessIndex
fromAlonzoRdmrPtr (Alonzo.RdmrPtr tag n) =
    case tag of
      Alonzo.Spend -> ScriptWitnessIndexTxIn        (fromIntegral n)
      Alonzo.Mint  -> ScriptWitnessIndexMint        (fromIntegral n)
      Alonzo.Cert  -> ScriptWitnessIndexCertificate (fromIntegral n)
      Alonzo.Rewrd -> ScriptWitnessIndexWithdrawal  (fromIntegral n)

collectTxBodyScriptWitnesses :: forall era. ShelleyBasedEra era
                             -> TxBodyContent BuildTx era
                             -> [(ScriptWitnessIndex, AnyScriptWitness era)]
collectTxBodyScriptWitnesses sbe TxBodyContent {
                               txIns,
                               txWithdrawals,
                               txCertificates,
                               txMintValue
                             } =
    concat
      [ scriptWitnessesTxIns        txIns
      , scriptWitnessesWithdrawals  txWithdrawals
      , scriptWitnessesCertificates txCertificates
      , scriptWitnessesMinting      txMintValue
      ]
  where
    scriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesTxIns txins =
        [ (ScriptWitnessIndexTxIn ix, AnyScriptWitness witness)
          -- The tx ins are indexed in the map order by txid
        | (ix, (_, BuildTxWith (ScriptWitness _ witness)))
            <- zip [0..] (orderTxIns txins)
        ]

    scriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesWithdrawals  TxWithdrawalsNone = []
    scriptWitnessesWithdrawals (TxWithdrawals _ withdrawals) =
        [ (ScriptWitnessIndexWithdrawal ix, AnyScriptWitness witness)
          -- The withdrawals are indexed in the map order by stake credential
        | (ix, (_, _, BuildTxWith (ScriptWitness _ witness)))
             <- zip [0..] (orderStakeAddrs withdrawals)
        ]

    scriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesCertificates  TxCertificatesNone = []
    scriptWitnessesCertificates (TxCertificates _ certs (BuildTxWith witnesses)) =
        [ (ScriptWitnessIndexCertificate ix, AnyScriptWitness witness)
          -- The certs are indexed in list order
        | (ix, cert) <- zip [0..] certs
        , ScriptWitness _ witness <- maybeToList $ do
                                       stakecred <- shelleyBasedEraConstraints sbe $ selectStakeCredential cert
                                       Map.lookup stakecred witnesses
        ]

    scriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesMinting  TxMintNone = []
    scriptWitnessesMinting (TxMintValue _ value (BuildTxWith witnesses)) =
        [ (ScriptWitnessIndexMint ix, AnyScriptWitness witness)
          -- The minting policies are indexed in policy id order in the value
        | let ValueNestedRep bundle = valueToNestedRep value
        , (ix, ValueNestedBundle policyid _) <- zip [0..] bundle
        , witness <- maybeToList (Map.lookup policyid witnesses)
        ]

-- This relies on the TxId Ord instance being consistent with the
-- Ledger.TxId Ord instance via the toShelleyTxId conversion
-- This is checked by prop_ord_distributive_TxId
orderTxIns :: [(TxIn, v)] -> [(TxIn, v)]
orderTxIns = sortBy (compare `on` fst)

-- This relies on the StakeAddress Ord instance being consistent with the
-- Shelley.RewardAcnt Ord instance via the toShelleyStakeAddr conversion
-- This is checked by prop_ord_distributive_StakeAddress
orderStakeAddrs :: [(StakeAddress, x, v)] -> [(StakeAddress, x, v)]
orderStakeAddrs = sortBy (compare `on` (\(k, _, _) -> k))


toShelleyWithdrawal :: [(StakeAddress, Lovelace, a)] -> L.Withdrawals StandardCrypto
toShelleyWithdrawal withdrawals =
    L.Withdrawals $
      Map.fromList
        [ (toShelleyStakeAddr stakeAddr, toShelleyLovelace value)
        | (stakeAddr, value, _) <- withdrawals ]


fromShelleyWithdrawal
  :: L.Withdrawals StandardCrypto
  -> [(StakeAddress, Lovelace, BuildTxWith ViewTx (Witness WitCtxStake era))]
fromShelleyWithdrawal (L.Withdrawals withdrawals) =
  [ (fromShelleyStakeAddr stakeAddr, fromShelleyLovelace value, ViewTx)
  | (stakeAddr, value) <- Map.assocs withdrawals
  ]


-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts. In the Alonzo and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
--
toAuxiliaryData
  :: ShelleyBasedEra era
  -> TxMetadataInEra era
  -> TxAuxScripts era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
toAuxiliaryData sbe txMetadata txAuxScripts =
  let ms = case txMetadata of
             TxMetadataNone                     -> Map.empty
             TxMetadataInEra _ (TxMetadata ms') -> toShelleyMetadata ms'
      ss = case txAuxScripts of
             TxAuxScriptsNone   -> []
             TxAuxScripts _ ss' -> map toShelleyScript ss'
  in case sbe of
       ShelleyBasedEraShelley ->
         guard (not (Map.null ms)) $> L.ShelleyTxAuxData ms
       ShelleyBasedEraAllegra ->
         guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (Seq.fromList ss)
       ShelleyBasedEraMary ->
         guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (Seq.fromList ss)
       ShelleyBasedEraAlonzo ->
         guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
       ShelleyBasedEraBabbage ->
         guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss
       ShelleyBasedEraConway ->
         guard (not (Map.null ms && null ss)) $> L.mkAlonzoTxAuxData ms ss

-- ----------------------------------------------------------------------------
-- Other utilities helpful with making transaction bodies
--

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
--
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
    --TODO: should handle Byron UTxO case too.
    fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
  where
    addr :: L.Addr StandardCrypto
    addr = L.Addr
             (toShelleyNetwork nw)
             (Shelley.KeyHashObj kh)
             Shelley.StakeRefNull

calculateExecutionUnitsLovelace :: Ledger.Prices -> ExecutionUnits -> Maybe Lovelace
calculateExecutionUnitsLovelace prices eUnits =
  return . fromShelleyLovelace $ Alonzo.txscriptfee prices (toAlonzoExUnits eUnits)

-- ----------------------------------------------------------------------------
-- Inline data
--
-- | Conversion of ScriptData to binary data which allows for the storage of data
-- onchain within a transaction output.
--

scriptDataToInlineDatum :: L.Era ledgerera => HashableScriptData -> L.Datum ledgerera
scriptDataToInlineDatum d =
  L.Datum . L.dataToBinaryData $ toAlonzoData d

binaryDataToScriptData
  :: L.Era ledgerera
  => BabbageEraOnwards era
  -> L.BinaryData ledgerera -> HashableScriptData
binaryDataToScriptData BabbageEraOnwardsBabbage d =
  fromAlonzoData $ L.binaryDataToData d
binaryDataToScriptData BabbageEraOnwardsConway  d =
  fromAlonzoData $ L.binaryDataToData d
