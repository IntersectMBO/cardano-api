{-# LANGUAGE DataKinds #-}
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

-- | Transaction bodies
module Cardano.Api.Tx.Body
  ( parseTxId

    -- * Transaction bodies
  , TxBody (.., TxBody)
  , createTransactionBody
  , createAndValidateTransactionBody
  , TxBodyContent (..)

    -- * Byron only
  , makeByronTransactionBody

    -- ** Transaction body builders
  , defaultTxBodyContent
  , defaultTxFee
  , defaultTxValidityUpperBound
  , setTxIns
  , modTxIns
  , addTxIn
  , setTxInsCollateral
  , setTxInsReference
  , setTxOuts
  , modTxOuts
  , addTxOut
  , setTxTotalCollateral
  , setTxReturnCollateral
  , setTxFee
  , setTxValidityLowerBound
  , setTxValidityUpperBound
  , setTxMetadata
  , setTxAuxScripts
  , setTxExtraKeyWits
  , setTxProtocolParams
  , setTxWithdrawals
  , setTxCertificates
  , setTxUpdateProposal
  , setTxProposalProcedures
  , setTxVotingProcedures
  , setTxMintValue
  , setTxScriptValidity
  , setTxCurrentTreasuryValue
  , setTxTreasuryDonation
  , TxBodyError (..)
  , TxBodyScriptData (..)
  , TxScriptValidity (..)
  , ScriptValidity (..)
  , scriptValidityToIsValid
  , isValidToScriptValidity
  , txScriptValidityToIsValid
  , txScriptValidityToScriptValidity

    -- * Transaction Ids
  , TxId (..)
  , getTxId
  , getTxIdByron
  , getTxIdShelley

    -- * Transaction inputs
  , TxIn (..)
  , TxIns
  , TxIx (..)
  , genesisUTxOPseudoTxIn
  , getReferenceInputsSizeForTxIds

    -- * Transaction outputs
  , CtxTx
  , CtxUTxO
  , TxOut (..)
  , TxOutValue (..)
  , TxOutDatum (TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline)
  , toCtxUTxOTxOut
  , lovelaceToTxOutValue
  , prettyRenderTxOut
  , txOutValueToLovelace
  , txOutValueToValue
  , parseHash
  , TxOutInAnyEra (..)
  , txOutInAnyEra

    -- * Other transaction body types
  , TxInsCollateral (..)
  , TxInsReference (..)
  , TxReturnCollateral (..)
  , TxTotalCollateral (..)
  , TxFee (..)
  , TxValidityLowerBound (..)
  , TxValidityUpperBound (..)
  , TxMetadataInEra (..)
  , TxAuxScripts (..)
  , TxExtraKeyWitnesses (..)
  , TxWithdrawals (..)
  , TxCertificates (..)
  , TxUpdateProposal (..)
  , TxMintValue (..)
  , TxVotingProcedures (..)
  , mkTxVotingProcedures
  , TxProposalProcedures (..)
  , mkTxProposalProcedures
  , convProposalProcedures

    -- ** Building vs viewing transactions
  , BuildTxWith (..)
  , BuildTx
  , ViewTx
  , buildTxWithToMaybe

    -- * Inspecting 'ScriptWitness'es
  , AnyScriptWitness (..)
  , ScriptWitnessIndex (..)
  , renderScriptWitnessIndex
  , collectTxBodyScriptWitnesses
  , toScriptIndex

    -- * Conversion to inline data
  , scriptDataToInlineDatum

    -- * Internal conversion functions & types
  , toByronTxId
  , toShelleyTxId
  , toShelleyTxIn
  , toShelleyTxOut
  , toShelleyTxOutAny
  , fromShelleyTxId
  , fromShelleyTxIn
  , fromShelleyTxOut
  , fromByronTxIn
  , fromLedgerTxOuts
  , renderTxIn

    -- * Misc helpers
  , calculateExecutionUnitsLovelace
  , orderStakeAddrs
  , orderTxIns

    -- * Data family instances
  , AsType (AsTxId, AsTxBody, AsByronTxBody, AsShelleyTxBody, AsMaryTxBody)
  , getTxBodyContent
  -- Temp
  , validateTxIns
  , guardShelleyTxInsOverflow
  , validateTxOuts
  , validateMetadata
  , validateMintValue
  , validateTxInsCollateral
  , validateProtocolParameters
  )
where

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.AllegraEraOnwards
import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyToAllegraEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.Eras.Case
import           Cardano.Api.Eras.Core
import           Cardano.Api.Error (Error (..), displayError)
import           Cardano.Api.Feature
import           Cardano.Api.Governance.Actions.VotingProcedure
import           Cardano.Api.Hash
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Shelley
import qualified Cardano.Api.Ledger.Lens as A
import           Cardano.Api.NetworkId
import           Cardano.Api.Pretty
import           Cardano.Api.ProtocolParameters
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.Script
import           Cardano.Api.ScriptData
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.Tx.Sign
import           Cardano.Api.TxIn
import           Cardano.Api.TxMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value
import           Cardano.Api.ValueParser

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Crypto.Hashing as Byron
import qualified Cardano.Ledger.Allegra.Core as L
import qualified Cardano.Ledger.Alonzo.Core as L
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (hashScriptIntegrity)
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage.UTxO as L
import           Cardano.Ledger.BaseTypes (StrictMaybe (..))
import           Cardano.Ledger.Binary (Annotated (..))
import qualified Cardano.Ledger.Binary as CBOR
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Conway.Core as L
import           Cardano.Ledger.Core ()
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley
import           Cardano.Ledger.Mary.Value as L (MaryValue (..), MultiAsset)
import qualified Cardano.Ledger.Plutus.Data as Plutus
import qualified Cardano.Ledger.Plutus.Language as Plutus
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import qualified Cardano.Ledger.Shelley.TxCert as Shelley
import qualified Cardano.Ledger.TxIn as L
import           Cardano.Ledger.Val as L (isZero)
import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardAlonzo, StandardBabbage,
                   StandardConway, StandardMary, StandardShelley)

import           Control.Applicative
import           Control.Monad
import           Data.Aeson (object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (Bifunctor (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.DList as DList
import           Data.Foldable (for_)
import qualified Data.Foldable as Foldable
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.List (sortBy)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.OSet.Strict (OSet, (|><))
import qualified Data.OSet.Strict as OSet
import           Data.Scientific (toBoundedInteger)
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Word (Word16, Word32, Word64)
import           GHC.Exts (IsList (..))
import           Lens.Micro hiding (ix)
import           Lens.Micro.Extras (view)
import qualified Text.Parsec as Parsec
import           Text.Parsec ((<?>))
import qualified Text.Parsec.String as Parsec

-- ----------------------------------------------------------------------------
-- Transaction outputs
--

-- | The context is a transaction body
data CtxTx

-- | The context is the UTxO
data CtxUTxO

data TxOut ctx era
  = TxOut
      (AddressInEra era)
      (TxOutValue era)
      (TxOutDatum ctx era)
      (ReferenceScript era)

deriving instance Eq (TxOut ctx era)

deriving instance Show (TxOut ctx era)

data TxOutInAnyEra where
  TxOutInAnyEra
    :: CardanoEra era
    -> TxOut CtxTx era
    -> TxOutInAnyEra

deriving instance Show TxOutInAnyEra

instance Eq TxOutInAnyEra where
  TxOutInAnyEra era1 out1 == TxOutInAnyEra era2 out2 =
    case testEquality era1 era2 of
      Just Refl -> out1 == out2
      Nothing -> False

deriving via (ShowOf TxOutInAnyEra) instance Pretty TxOutInAnyEra

-- | Convenience constructor for 'TxOutInAnyEra'
txOutInAnyEra :: CardanoEra era -> TxOut CtxTx era -> TxOutInAnyEra
txOutInAnyEra = TxOutInAnyEra

toCtxUTxOTxOut :: TxOut CtxTx era -> TxOut CtxUTxO era
toCtxUTxOTxOut (TxOut addr val d refS) =
  let dat = case d of
        TxOutDatumNone -> TxOutDatumNone
        TxOutDatumHash s h -> TxOutDatumHash s h
        TxOutDatumInTx' s h _ -> TxOutDatumHash s h
        TxOutDatumInline s sd -> TxOutDatumInline s sd
   in TxOut addr val dat refS

instance IsCardanoEra era => ToJSON (TxOut ctx era) where
  toJSON = txOutToJsonValue cardanoEra

txOutToJsonValue :: CardanoEra era -> TxOut ctx era -> Aeson.Value
txOutToJsonValue era (TxOut addr val dat refScript) =
  case era of
    ByronEra -> object ["address" .= addr, "value" .= val]
    ShelleyEra -> object ["address" .= addr, "value" .= val]
    AllegraEra -> object ["address" .= addr, "value" .= val]
    MaryEra -> object ["address" .= addr, "value" .= val]
    AlonzoEra ->
      object
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
        "inlineDatumhash" .= toJSON (hashScriptDataBytes datum)

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
      TxOutDatumHash{} -> Aeson.Null
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
        TxOut
          <$> o .: "address"
          <*> o .: "value"
          <*> return TxOutDatumNone
          <*> return ReferenceScriptNone
      ShelleyBasedEraMary ->
        TxOut
          <$> o .: "address"
          <*> o .: "value"
          <*> return TxOutDatumNone
          <*> return ReferenceScriptNone
      ShelleyBasedEraAllegra ->
        TxOut
          <$> o .: "address"
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
            (_, _) ->
              fail
                "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

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
            (_, _) ->
              fail
                "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

        mReferenceScript <- o .:? "referenceScript"

        reconcileConway alonzoTxOutInConway mInlineDatum mReferenceScript
   where
    reconcileBabbage
      :: TxOut CtxTx BabbageEra
      -- \^ Alonzo era datum in Babbage era
      -> TxOutDatum CtxTx BabbageEra
      -- \^ Babbage inline datum
      -> Maybe ScriptInAnyLang
      -> Aeson.Parser (TxOut CtxTx BabbageEra)
    reconcileBabbage top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
      -- We check for conflicting datums
      finalDat <- case (dat, babbageDatum) of
        (TxOutDatumNone, bDatum) -> return bDatum
        (anyDat, TxOutDatumNone) -> return anyDat
        (alonzoDat, babbageDat) ->
          fail $
            "Parsed an Alonzo era datum and a Babbage era datum "
              <> "TxOut: "
              <> show top
              <> "Alonzo datum: "
              <> show alonzoDat
              <> "Babbage dat: "
              <> show babbageDat
      finalRefScript <- case mBabRefScript of
        Nothing -> return r
        Just anyScript ->
          return $ ReferenceScript BabbageEraOnwardsBabbage anyScript
      return $ TxOut addr v finalDat finalRefScript

    reconcileConway
      :: TxOut CtxTx ConwayEra
      -- \^ Alonzo era datum in Conway era
      -> TxOutDatum CtxTx ConwayEra
      -- \^ Babbage inline datum
      -> Maybe ScriptInAnyLang
      -> Aeson.Parser (TxOut CtxTx ConwayEra)
    reconcileConway top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
      -- We check for conflicting datums
      finalDat <- case (dat, babbageDatum) of
        (TxOutDatumNone, bDatum) -> return bDatum
        (anyDat, TxOutDatumNone) -> return anyDat
        (alonzoDat, babbageDat) ->
          fail $
            "Parsed an Alonzo era datum and a Conway era datum "
              <> "TxOut: "
              <> show top
              <> "Alonzo datum: "
              <> show alonzoDat
              <> "Conway dat: "
              <> show babbageDat
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
        (Nothing, Nothing) ->
          TxOut
            <$> o .: "address"
            <*> o .: "value"
            <*> return TxOutDatumNone
            <*> return ReferenceScriptNone
        (Just dVal, Just dHash) -> do
          case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
            Left e -> fail $ "Error parsing ScriptData: " <> show e
            Right hashableData ->
              TxOut
                <$> o .: "address"
                <*> o .: "value"
                <*> return (TxOutDatumInTx' w dHash hashableData)
                <*> return ReferenceScriptNone
        (Nothing, Just dHash) ->
          TxOut
            <$> o .: "address"
            <*> o .: "value"
            <*> return (TxOutDatumHash w dHash)
            <*> return ReferenceScriptNone
        (Just _dVal, Nothing) -> fail "Only datum JSON was found, this should not be possible."

instance IsShelleyBasedEra era => FromJSON (TxOut CtxUTxO era) where
  parseJSON = withObject "TxOut" $ \o -> do
    case shelleyBasedEra :: ShelleyBasedEra era of
      ShelleyBasedEraShelley ->
        TxOut
          <$> o .: "address"
          <*> o .: "value"
          <*> return TxOutDatumNone
          <*> return ReferenceScriptNone
      ShelleyBasedEraMary ->
        TxOut
          <$> o .: "address"
          <*> o .: "value"
          <*> return TxOutDatumNone
          <*> return ReferenceScriptNone
      ShelleyBasedEraAllegra ->
        TxOut
          <$> o .: "address"
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
            (_, _) ->
              fail
                "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

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
            (_, _) ->
              fail
                "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

        -- We check for a reference script
        mReferenceScript <- o .:? "referenceScript"

        reconcileConway alonzoTxOutInConway mInlineDatum mReferenceScript
   where
    reconcileBabbage
      :: TxOut CtxUTxO BabbageEra
      -- \^ Alonzo era datum in Babbage era
      -> TxOutDatum CtxUTxO BabbageEra
      -- \^ Babbage inline datum
      -> Maybe ScriptInAnyLang
      -> Aeson.Parser (TxOut CtxUTxO BabbageEra)
    reconcileBabbage (TxOut addr v dat r) babbageDatum mBabRefScript = do
      -- We check for conflicting datums
      finalDat <- case (dat, babbageDatum) of
        (TxOutDatumNone, bDatum) -> return bDatum
        (anyDat, TxOutDatumNone) -> return anyDat
        (_, _) -> fail "Parsed an Alonzo era datum and a Babbage era datum"
      finalRefScript <- case mBabRefScript of
        Nothing -> return r
        Just anyScript ->
          return $ ReferenceScript BabbageEraOnwardsBabbage anyScript

      return $ TxOut addr v finalDat finalRefScript

    reconcileConway
      :: TxOut CtxUTxO ConwayEra
      -- \^ Alonzo era datum in Conway era
      -> TxOutDatum CtxUTxO ConwayEra
      -- \^ Babbage inline datum
      -> Maybe ScriptInAnyLang
      -> Aeson.Parser (TxOut CtxUTxO ConwayEra)
    reconcileConway (TxOut addr v dat r) babbageDatum mBabRefScript = do
      -- We check for conflicting datums
      finalDat <- case (dat, babbageDatum) of
        (TxOutDatumNone, bDatum) -> return bDatum
        (anyDat, TxOutDatumNone) -> return anyDat
        (_, _) -> fail "Parsed an Alonzo era datum and a Conway era datum"
      finalRefScript <- case mBabRefScript of
        Nothing -> return r
        Just anyScript ->
          return $ ReferenceScript BabbageEraOnwardsConway anyScript

      return $ TxOut addr v finalDat finalRefScript

    alonzoTxOutParser :: AlonzoEraOnwards era -> Aeson.Object -> Aeson.Parser (TxOut CtxUTxO era)
    alonzoTxOutParser w o = do
      mDatumHash <- o .:? "datumhash"
      case mDatumHash of
        Nothing ->
          TxOut
            <$> o .: "address"
            <*> o .: "value"
            <*> return TxOutDatumNone
            <*> return ReferenceScriptNone
        Just dHash ->
          TxOut
            <$> o .: "address"
            <*> o .: "value"
            <*> return (TxOutDatumHash w dHash)
            <*> return ReferenceScriptNone

toByronTxOut :: TxOut ctx ByronEra -> Maybe Byron.TxOut
toByronTxOut = \case
  TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr)) (TxOutValueByron value) _ _ ->
    Byron.TxOut addr <$> toByronLovelace value
  TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _)) (TxOutValueShelleyBased w _) _ _ ->
    case w of {}
  TxOut (AddressInEra (ShelleyAddressInEra sbe) ShelleyAddress{}) _ _ _ ->
    case sbe of {}

toShelleyTxOut
  :: forall era ledgerera
   . ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> TxOut CtxUTxO era
  -> Ledger.TxOut ledgerera
toShelleyTxOut _ = \case
  -- jky simplify
  TxOut _ (TxOutValueByron _) _ _ ->
    -- TODO: Temporary until we have basic tx
    -- construction functionality
    error "toShelleyTxOut: Expected a Shelley value"
  TxOut addr (TxOutValueShelleyBased sbe value) txoutdata refScript ->
    caseShelleyToMaryOrAlonzoEraOnwards
      (const $ L.mkBasicTxOut (toShelleyAddr addr) value)
      ( \case
          AlonzoEraOnwardsAlonzo ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.dataHashTxOutL .~ toAlonzoTxOutDatumHashUTxO txoutdata
          AlonzoEraOnwardsBabbage ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL .~ toBabbageTxOutDatum txoutdata
              & L.referenceScriptTxOutL .~ refScriptToShelleyScript sbe refScript
          AlonzoEraOnwardsConway ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL .~ toBabbageTxOutDatumUTxO txoutdata
              & L.referenceScriptTxOutL .~ refScriptToShelleyScript sbe refScript
      )
      sbe

toAlonzoTxOutDatumHashUTxO
  :: TxOutDatum CtxUTxO era -> StrictMaybe (Plutus.DataHash StandardCrypto)
toAlonzoTxOutDatumHashUTxO TxOutDatumNone = SNothing
toAlonzoTxOutDatumHashUTxO (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDatumHashUTxO (TxOutDatumInline{}) = SNothing

toBabbageTxOutDatumUTxO
  :: (L.Era (ShelleyLedgerEra era), Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => TxOutDatum CtxUTxO era
  -> Plutus.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatumUTxO TxOutDatumNone = Plutus.NoDatum
toBabbageTxOutDatumUTxO (TxOutDatumHash _ (ScriptDataHash dh)) = Plutus.DatumHash dh
toBabbageTxOutDatumUTxO (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

fromShelleyTxOut
  :: forall era ctx
   . ()
  => ShelleyBasedEra era
  -> Core.TxOut (ShelleyLedgerEra era)
  -> TxOut ctx era
fromShelleyTxOut sbe ledgerTxOut = shelleyBasedEraConstraints sbe $ do
  let txOutValue = TxOutValueShelleyBased sbe $ ledgerTxOut ^. A.valueTxOutL sbe
  let addressInEra = fromShelleyAddr sbe $ ledgerTxOut ^. L.addrTxOutL

  case sbe of
    ShelleyBasedEraShelley ->
      TxOut addressInEra txOutValue TxOutDatumNone ReferenceScriptNone
    ShelleyBasedEraAllegra ->
      TxOut addressInEra txOutValue TxOutDatumNone ReferenceScriptNone
    ShelleyBasedEraMary ->
      TxOut addressInEra txOutValue TxOutDatumNone ReferenceScriptNone
    ShelleyBasedEraAlonzo ->
      TxOut addressInEra txOutValue TxOutDatumNone ReferenceScriptNone
    ShelleyBasedEraBabbage ->
      TxOut
        addressInEra
        txOutValue
        ( fromBabbageTxOutDatum
            AlonzoEraOnwardsBabbage
            BabbageEraOnwardsBabbage
            datum
        )
        ( case mRefScript of
            SNothing -> ReferenceScriptNone
            SJust refScript ->
              fromShelleyScriptToReferenceScript ShelleyBasedEraBabbage refScript
        )
     where
      datum = ledgerTxOut ^. L.datumTxOutL
      mRefScript = ledgerTxOut ^. L.referenceScriptTxOutL
    ShelleyBasedEraConway ->
      TxOut
        addressInEra
        txOutValue
        ( fromBabbageTxOutDatum
            AlonzoEraOnwardsConway
            BabbageEraOnwardsConway
            datum
        )
        ( case mRefScript of
            SNothing -> ReferenceScriptNone
            SJust refScript ->
              fromShelleyScriptToReferenceScript ShelleyBasedEraConway refScript
        )
     where
      datum = ledgerTxOut ^. L.datumTxOutL
      mRefScript = ledgerTxOut ^. L.referenceScriptTxOutL

toAlonzoTxOutDatumHash
  :: TxOutDatum ctx era -> StrictMaybe (Plutus.DataHash StandardCrypto)
toAlonzoTxOutDatumHash TxOutDatumNone = SNothing
toAlonzoTxOutDatumHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDatumHash (TxOutDatumInline{}) = SNothing
toAlonzoTxOutDatumHash (TxOutDatumInTx' _ (ScriptDataHash dh) _) = SJust dh

toBabbageTxOutDatum
  :: (L.Era (ShelleyLedgerEra era), Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => TxOutDatum ctx era
  -> Plutus.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum TxOutDatumNone = Plutus.NoDatum
toBabbageTxOutDatum (TxOutDatumHash _ (ScriptDataHash dh)) = Plutus.DatumHash dh
toBabbageTxOutDatum (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd
toBabbageTxOutDatum (TxOutDatumInTx' _ (ScriptDataHash dh) _) = Plutus.DatumHash dh

fromBabbageTxOutDatum
  :: (L.Era ledgerera, Ledger.EraCrypto ledgerera ~ StandardCrypto)
  => AlonzoEraOnwards era
  -> BabbageEraOnwards era
  -> Plutus.Datum ledgerera
  -> TxOutDatum ctx era
fromBabbageTxOutDatum _ _ Plutus.NoDatum = TxOutDatumNone
fromBabbageTxOutDatum w _ (Plutus.DatumHash dh) =
  TxOutDatumHash w $ ScriptDataHash dh
fromBabbageTxOutDatum _ w (Plutus.Datum binData) =
  TxOutDatumInline w $ binaryDataToScriptData w binData

-- ----------------------------------------------------------------------------
-- Building vs viewing transactions
--

data ViewTx

data BuildTx

data BuildTxWith build a where
  ViewTx :: BuildTxWith ViewTx a
  BuildTxWith :: a -> BuildTxWith BuildTx a

instance Functor (BuildTxWith build) where
  fmap _ ViewTx = ViewTx
  fmap f (BuildTxWith x) = BuildTxWith (f x)

instance Applicative (BuildTxWith ViewTx) where
  pure _ = ViewTx
  _ <*> _ = ViewTx

instance Applicative (BuildTxWith BuildTx) where
  pure = BuildTxWith
  (BuildTxWith f) <*> (BuildTxWith a) = BuildTxWith (f a)

buildTxWithToMaybe :: BuildTxWith build a -> Maybe a
buildTxWithToMaybe ViewTx = Nothing
buildTxWithToMaybe (BuildTxWith a) = Just a

deriving instance Eq a => Eq (BuildTxWith build a)

deriving instance Show a => Show (BuildTxWith build a)

-- ----------------------------------------------------------------------------
-- Transaction input values (era-dependent)
--

type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

data TxInsCollateral era where
  TxInsCollateralNone
    :: TxInsCollateral era
  TxInsCollateral
    :: AlonzoEraOnwards era
    -> [TxIn] -- Only key witnesses, no scripts.
    -> TxInsCollateral era

deriving instance Eq (TxInsCollateral era)

deriving instance Show (TxInsCollateral era)

data TxInsReference era where
  TxInsReferenceNone :: TxInsReference era
  TxInsReference
    :: BabbageEraOnwards era
    -> [TxIn]
    -> TxInsReference era

deriving instance Eq (TxInsReference era)

deriving instance Show (TxInsReference era)

-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where
  TxOutValueByron
    :: L.Coin
    -> TxOutValue era
  TxOutValueShelleyBased
    :: ( Eq (Ledger.Value (ShelleyLedgerEra era))
       , Show (Ledger.Value (ShelleyLedgerEra era))
       )
    => ShelleyBasedEra era
    -> L.Value (ShelleyLedgerEra era)
    -> TxOutValue era

deriving instance Eq (TxOutValue era)

deriving instance Show (TxOutValue era)

instance IsCardanoEra era => ToJSON (TxOutValue era) where
  toJSON = \case
    TxOutValueByron ll ->
      toJSON ll
    TxOutValueShelleyBased sbe val ->
      shelleyBasedEraConstraints sbe $ toJSON (fromLedgerValue sbe val)

instance IsShelleyBasedEra era => FromJSON (TxOutValue era) where
  parseJSON = withObject "TxOutValue" $ \o ->
    caseShelleyToAllegraOrMaryEraOnwards
      ( \shelleyToAlleg -> do
          ll <- o .: "lovelace"
          pure $
            shelleyBasedEraConstraints (shelleyToAllegraEraToShelleyBasedEra shelleyToAlleg) $
              TxOutValueShelleyBased (shelleyToAllegraEraToShelleyBasedEra shelleyToAlleg) $
                A.mkAdaValue (shelleyToAllegraEraToShelleyBasedEra shelleyToAlleg) ll
      )
      ( \w -> do
          let l = toList o
          vals <- mapM decodeAssetId l
          pure $
            shelleyBasedEraConstraints (maryEraOnwardsToShelleyBasedEra w) $
              TxOutValueShelleyBased (maryEraOnwardsToShelleyBasedEra w) $
                toLedgerValue w $
                  mconcat vals
      )
      (shelleyBasedEra @era)
   where
    decodeAssetId :: (Aeson.Key, Aeson.Value) -> Aeson.Parser Value
    decodeAssetId (polid, Aeson.Object assetNameHm) = do
      let polId = fromString . Text.unpack $ Aeson.toText polid
      aNameQuantity <- decodeAssets assetNameHm
      pure . valueFromList $
        map (first $ AssetId polId) aNameQuantity
    decodeAssetId ("lovelace", Aeson.Number sci) =
      case toBoundedInteger sci of
        Just (ll :: Word64) ->
          pure $ valueFromList [(AdaAssetId, Quantity $ toInteger ll)]
        Nothing ->
          fail $ "Expected a Bounded number but got: " <> show sci
    decodeAssetId wrong = fail $ "Expected a policy id and a JSON object but got: " <> show wrong

    decodeAssets :: Aeson.Object -> Aeson.Parser [(AssetName, Quantity)]
    decodeAssets assetNameHm =
      let l = toList assetNameHm
       in mapM (\(aName, q) -> (,) <$> parseAssetName aName <*> decodeQuantity q) l

    parseAssetName :: Aeson.Key -> Aeson.Parser AssetName
    parseAssetName aName = runParsecParser assetName (Aeson.toText aName)

    decodeQuantity :: Aeson.Value -> Aeson.Parser Quantity
    decodeQuantity (Aeson.Number sci) =
      case toBoundedInteger sci of
        Just (ll :: Word64) -> return . Quantity $ toInteger ll
        Nothing -> fail $ "Expected a Bounded number but got: " <> show sci
    decodeQuantity wrong = fail $ "Expected aeson Number but got: " <> show wrong

lovelaceToTxOutValue
  :: ()
  => ShelleyBasedEra era
  -> L.Coin
  -> TxOutValue era
lovelaceToTxOutValue era ll =
  shelleyBasedEraConstraints era $
    TxOutValueShelleyBased era $
      A.mkAdaValue era ll

txOutValueToLovelace :: TxOutValue era -> L.Coin
txOutValueToLovelace tv =
  case tv of
    TxOutValueByron l -> l
    TxOutValueShelleyBased sbe v -> v ^. A.adaAssetL sbe

txOutValueToValue :: TxOutValue era -> Value
txOutValueToValue tv =
  case tv of
    TxOutValueByron l -> lovelaceToValue l
    TxOutValueShelleyBased sbe v -> fromLedgerValue sbe v

prettyRenderTxOut :: TxOutInAnyEra -> Text
prettyRenderTxOut (TxOutInAnyEra _ (TxOut (AddressInEra _ addr) txOutVal _ _)) =
  serialiseAddress (toAddressAny addr)
    <> " + "
    <> renderValue (txOutValueToValue txOutVal)

data TxReturnCollateral ctx era where
  TxReturnCollateralNone
    :: TxReturnCollateral ctx era
  TxReturnCollateral
    :: BabbageEraOnwards era
    -> TxOut ctx era
    -> TxReturnCollateral ctx era

deriving instance Eq (TxReturnCollateral ctx era)

deriving instance Show (TxReturnCollateral ctx era)

data TxTotalCollateral era where
  TxTotalCollateralNone
    :: TxTotalCollateral era
  TxTotalCollateral
    :: BabbageEraOnwards era
    -> L.Coin
    -> TxTotalCollateral era

deriving instance Eq (TxTotalCollateral era)

deriving instance Show (TxTotalCollateral era)

-- ----------------------------------------------------------------------------
-- Transaction output datum (era-dependent)
--

data TxOutDatum ctx era where
  TxOutDatumNone :: TxOutDatum ctx era
  -- | A transaction output that only specifies the hash of the datum, but
  -- not the full datum value.
  TxOutDatumHash
    :: AlonzoEraOnwards era
    -> Hash ScriptData
    -> TxOutDatum ctx era
  -- | A transaction output that specifies the whole datum value. This can
  -- only be used in the context of the transaction body, and does not occur
  -- in the UTxO. The UTxO only contains the datum hash.
  TxOutDatumInTx'
    :: AlonzoEraOnwards era
    -> Hash ScriptData
    -> HashableScriptData
    -> TxOutDatum CtxTx era
  -- | A transaction output that specifies the whole datum instead of the
  -- datum hash. Note that the datum map will not be updated with this datum,
  -- it only exists at the transaction output.
  TxOutDatumInline
    :: BabbageEraOnwards era
    -> HashableScriptData
    -> TxOutDatum ctx era

deriving instance Eq (TxOutDatum ctx era)

deriving instance Show (TxOutDatum ctx era)

pattern TxOutDatumInTx
  :: AlonzoEraOnwards era
  -> HashableScriptData
  -> TxOutDatum CtxTx era
pattern TxOutDatumInTx w d <- TxOutDatumInTx' w _ d
  where
    TxOutDatumInTx w d = TxOutDatumInTx' w (hashScriptDataBytes d) d

{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx', TxOutDatumInline #-}

{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline #-}

parseHash :: SerialiseAsRawBytes (Hash a) => AsType (Hash a) -> Parsec.Parser (Hash a)
parseHash asType = do
  str <- some Parsec.hexDigit <?> "hash"
  failEitherWith (\e -> "Failed to parse hash: " ++ displayError e) $
    deserialiseFromRawBytesHex asType (BSC.pack str)

-- ----------------------------------------------------------------------------
-- Transaction fees
--

data TxFee era where
  TxFeeExplicit :: ShelleyBasedEra era -> L.Coin -> TxFee era

deriving instance Eq (TxFee era)

deriving instance Show (TxFee era)

defaultTxFee :: ShelleyBasedEra era -> TxFee era
defaultTxFee w = TxFeeExplicit w mempty

-- ----------------------------------------------------------------------------
-- Transaction validity range
--

-- | This was formerly known as the TTL.
data TxValidityUpperBound era where
  TxValidityUpperBound
    :: ShelleyBasedEra era
    -> Maybe SlotNo
    -> TxValidityUpperBound era

deriving instance Eq (TxValidityUpperBound era)

deriving instance Show (TxValidityUpperBound era)

defaultTxValidityUpperBound
  :: ()
  => ShelleyBasedEra era
  -> TxValidityUpperBound era
defaultTxValidityUpperBound sbe = TxValidityUpperBound sbe Nothing

data TxValidityLowerBound era where
  TxValidityNoLowerBound
    :: TxValidityLowerBound era
  TxValidityLowerBound
    :: AllegraEraOnwards era
    -> SlotNo
    -> TxValidityLowerBound era

deriving instance Eq (TxValidityLowerBound era)

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

deriving instance Eq (TxMetadataInEra era)

deriving instance Show (TxMetadataInEra era)

-- ----------------------------------------------------------------------------
-- Auxiliary scripts (era-dependent)
--

data TxAuxScripts era where
  TxAuxScriptsNone
    :: TxAuxScripts era
  TxAuxScripts
    :: AllegraEraOnwards era
    -> [ScriptInEra era]
    -> TxAuxScripts era

deriving instance Eq (TxAuxScripts era)

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

deriving instance Eq (TxExtraKeyWitnesses era)

deriving instance Show (TxExtraKeyWitnesses era)

-- ----------------------------------------------------------------------------
-- Withdrawals within transactions (era-dependent)
--

data TxWithdrawals build era where
  TxWithdrawalsNone
    :: TxWithdrawals build era
  TxWithdrawals
    :: ShelleyBasedEra era
    -> [(StakeAddress, L.Coin, BuildTxWith build (Witness WitCtxStake era))]
    -> TxWithdrawals build era

deriving instance Eq (TxWithdrawals build era)

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
    -> BuildTxWith build [(StakeCredential, Witness WitCtxStake era)]
    -- ^ There can be more than one script witness per stake credential
    -> TxCertificates build era

deriving instance Eq (TxCertificates build era)

deriving instance Show (TxCertificates build era)

-- ----------------------------------------------------------------------------
-- Transaction update proposal (era-dependent)
--

data TxUpdateProposal era where
  TxUpdateProposalNone :: TxUpdateProposal era
  TxUpdateProposal :: ShelleyToBabbageEra era -> UpdateProposal -> TxUpdateProposal era

deriving instance Eq (TxUpdateProposal era)

deriving instance Show (TxUpdateProposal era)

-- ----------------------------------------------------------------------------
-- Value minting within transactions (era-dependent)
--

data TxMintValue build era where
  TxMintNone :: TxMintValue build era
  TxMintValue
    :: MaryEraOnwards era
    -> Value
    -> BuildTxWith
        build
        (Map PolicyId (ScriptWitness WitCtxMint era))
    -> TxMintValue build era

deriving instance Eq (TxMintValue build era)

deriving instance Show (TxMintValue build era)

-- ----------------------------------------------------------------------------
-- Votes within transactions (era-dependent)
--

data TxVotingProcedures build era where
  TxVotingProceduresNone :: TxVotingProcedures build era
  TxVotingProcedures
    :: L.VotingProcedures (ShelleyLedgerEra era)
    -> BuildTxWith
        build
        (Map (Ledger.Voter (Ledger.EraCrypto (ShelleyLedgerEra era))) (ScriptWitness WitCtxStake era))
    -> TxVotingProcedures build era

deriving instance Eq (TxVotingProcedures build era)

deriving instance Show (TxVotingProcedures build era)

-- | Create voting procedures from map of voting procedures and optional witnesses.
-- Validates the function argument, to make sure the list of votes is legal.
-- See 'mergeVotingProcedures' for validation rules.
mkTxVotingProcedures
  :: Applicative (BuildTxWith build)
  => [(VotingProcedures era, Maybe (ScriptWitness WitCtxStake era))]
  -> Either (VotesMergingConflict era) (TxVotingProcedures build era)
mkTxVotingProcedures votingProcedures = do
  VotingProcedures procedure <-
    foldM f emptyVotingProcedures votingProcedures
  pure $ TxVotingProcedures procedure (pure votingScriptWitnessMap)
 where
  votingScriptWitnessMap =
    foldl
      (\acc next -> acc `Map.union` uncurry votingScriptWitnessSingleton next)
      Map.empty
      votingProcedures
  f acc (procedure, _witness) = mergeVotingProcedures acc procedure

  votingScriptWitnessSingleton
    :: VotingProcedures era
    -> Maybe (ScriptWitness WitCtxStake era)
    -> Map (L.Voter (L.EraCrypto (ShelleyLedgerEra era))) (ScriptWitness WitCtxStake era)
  votingScriptWitnessSingleton _ Nothing = Map.empty
  votingScriptWitnessSingleton votingProcedures' (Just scriptWitness) = do
    let voter = fromJust $ getVotingScriptCredentials votingProcedures'
    Map.singleton voter scriptWitness

  getVotingScriptCredentials
    :: VotingProcedures era
    -> Maybe (L.Voter (L.EraCrypto (ShelleyLedgerEra era)))
  getVotingScriptCredentials (VotingProcedures (L.VotingProcedures m)) =
    listToMaybe $ Map.keys m

-- ----------------------------------------------------------------------------
-- Proposals within transactions (era-dependent)
--

data TxProposalProcedures build era where
  TxProposalProceduresNone :: TxProposalProcedures build era
  -- | Create Tx proposal procedures. Prefer 'mkTxProposalProcedures' smart constructor to using this constructor
  -- directly.
  TxProposalProcedures
    :: Ledger.EraPParams (ShelleyLedgerEra era)
    => OSet (L.ProposalProcedure (ShelleyLedgerEra era))
    -- ^ a set of proposals
    -> BuildTxWith build (Map (L.ProposalProcedure (ShelleyLedgerEra era)) (ScriptWitness WitCtxStake era))
    -- ^ a map of witnesses for the proposals. If the proposals are not added to the first constructor
    -- parameter too, the sky will fall on your head.
    -> TxProposalProcedures build era

deriving instance Eq (TxProposalProcedures build era)

deriving instance Show (TxProposalProcedures build era)

-- | A smart constructor for 'TxProposalProcedures'. It makes sure that the value produced is consistent - the
-- witnessed proposals are also present in the first constructor parameter.
mkTxProposalProcedures
  :: forall era build
   . Applicative (BuildTxWith build)
  => IsShelleyBasedEra era
  => [(L.ProposalProcedure (ShelleyLedgerEra era), Maybe (ScriptWitness WitCtxStake era))]
  -> TxProposalProcedures build era
mkTxProposalProcedures proposalsWithWitnessesList = do
  let (proposals, proposalsWithWitnesses) =
        bimap toList toList $
          Foldable.foldl' partitionProposals mempty proposalsWithWitnessesList
  shelleyBasedEraConstraints (shelleyBasedEra @era) $
    TxProposalProcedures (fromList proposals) (pure $ fromList proposalsWithWitnesses)
 where
  partitionProposals (ps, pws) (p, Nothing) =
    (DList.snoc ps p, pws) -- add a proposal to the list
  partitionProposals (ps, pws) (p, Just w) =
    (DList.snoc ps p, DList.snoc pws (p, w)) -- add a proposal both to the list and to the witnessed list

-- ----------------------------------------------------------------------------
-- Transaction body content
--

-- If you extend this type, consider updating:
-- - the 'makeShelleyTransactionBody' function of the relevant era below, and
-- - the @friendly*@ family of functions in cardano-cli.
data TxBodyContent build era
  = TxBodyContent
  { txIns :: TxIns build era
  , txInsCollateral :: TxInsCollateral era
  , txInsReference :: TxInsReference era
  , txOuts :: [TxOut CtxTx era]
  , txTotalCollateral :: TxTotalCollateral era
  , txReturnCollateral :: TxReturnCollateral CtxTx era
  , txFee :: TxFee era
  , txValidityLowerBound :: TxValidityLowerBound era
  , txValidityUpperBound :: TxValidityUpperBound era
  , txMetadata :: TxMetadataInEra era
  , txAuxScripts :: TxAuxScripts era
  , txExtraKeyWits :: TxExtraKeyWitnesses era
  , txProtocolParams :: BuildTxWith build (Maybe (LedgerProtocolParameters era))
  , txWithdrawals :: TxWithdrawals build era
  , txCertificates :: TxCertificates build era
  , txUpdateProposal :: TxUpdateProposal era
  , txMintValue :: TxMintValue build era
  , txScriptValidity :: TxScriptValidity era
  , txProposalProcedures :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures build era))
  , txVotingProcedures :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era))
  , txCurrentTreasuryValue :: Maybe (Featured ConwayEraOnwards era (Maybe L.Coin))
  -- ^ Current treasury value
  , txTreasuryDonation :: Maybe (Featured ConwayEraOnwards era L.Coin)
  -- ^ Treasury donation to perform
  }
  deriving (Eq, Show)

defaultTxBodyContent
  :: ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
defaultTxBodyContent era =
  TxBodyContent
    { txIns = []
    , txInsCollateral = TxInsCollateralNone
    , txInsReference = TxInsReferenceNone
    , txOuts = []
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee = defaultTxFee era
    , txValidityLowerBound = TxValidityNoLowerBound
    , txValidityUpperBound = defaultTxValidityUpperBound era
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
    , txCurrentTreasuryValue = Nothing
    , txTreasuryDonation = Nothing
    }

setTxIns :: TxIns build era -> TxBodyContent build era -> TxBodyContent build era
setTxIns v txBodyContent = txBodyContent{txIns = v}

modTxIns
  :: (TxIns build era -> TxIns build era) -> TxBodyContent build era -> TxBodyContent build era
modTxIns f txBodyContent = txBodyContent{txIns = f (txIns txBodyContent)}

addTxIn
  :: (TxIn, BuildTxWith build (Witness WitCtxTxIn era))
  -> TxBodyContent build era
  -> TxBodyContent build era
addTxIn txIn = modTxIns (txIn :)

setTxInsCollateral :: TxInsCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxInsCollateral v txBodyContent = txBodyContent{txInsCollateral = v}

setTxInsReference :: TxInsReference era -> TxBodyContent build era -> TxBodyContent build era
setTxInsReference v txBodyContent = txBodyContent{txInsReference = v}

setTxOuts :: [TxOut CtxTx era] -> TxBodyContent build era -> TxBodyContent build era
setTxOuts v txBodyContent = txBodyContent{txOuts = v}

modTxOuts
  :: ([TxOut CtxTx era] -> [TxOut CtxTx era]) -> TxBodyContent build era -> TxBodyContent build era
modTxOuts f txBodyContent = txBodyContent{txOuts = f (txOuts txBodyContent)}

addTxOut :: TxOut CtxTx era -> TxBodyContent build era -> TxBodyContent build era
addTxOut txOut = modTxOuts (txOut :)

setTxTotalCollateral :: TxTotalCollateral era -> TxBodyContent build era -> TxBodyContent build era
setTxTotalCollateral v txBodyContent = txBodyContent{txTotalCollateral = v}

setTxReturnCollateral
  :: TxReturnCollateral CtxTx era -> TxBodyContent build era -> TxBodyContent build era
setTxReturnCollateral v txBodyContent = txBodyContent{txReturnCollateral = v}

setTxFee :: TxFee era -> TxBodyContent build era -> TxBodyContent build era
setTxFee v txBodyContent = txBodyContent{txFee = v}

setTxValidityLowerBound
  :: TxValidityLowerBound era -> TxBodyContent build era -> TxBodyContent build era
setTxValidityLowerBound v txBodyContent = txBodyContent{txValidityLowerBound = v}

setTxValidityUpperBound
  :: TxValidityUpperBound era -> TxBodyContent build era -> TxBodyContent build era
setTxValidityUpperBound v txBodyContent = txBodyContent{txValidityUpperBound = v}

setTxMetadata :: TxMetadataInEra era -> TxBodyContent build era -> TxBodyContent build era
setTxMetadata v txBodyContent = txBodyContent{txMetadata = v}

setTxAuxScripts :: TxAuxScripts era -> TxBodyContent build era -> TxBodyContent build era
setTxAuxScripts v txBodyContent = txBodyContent{txAuxScripts = v}

setTxExtraKeyWits :: TxExtraKeyWitnesses era -> TxBodyContent build era -> TxBodyContent build era
setTxExtraKeyWits v txBodyContent = txBodyContent{txExtraKeyWits = v}

setTxProtocolParams
  :: BuildTxWith build (Maybe (LedgerProtocolParameters era))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxProtocolParams v txBodyContent = txBodyContent{txProtocolParams = v}

setTxWithdrawals :: TxWithdrawals build era -> TxBodyContent build era -> TxBodyContent build era
setTxWithdrawals v txBodyContent = txBodyContent{txWithdrawals = v}

setTxCertificates :: TxCertificates build era -> TxBodyContent build era -> TxBodyContent build era
setTxCertificates v txBodyContent = txBodyContent{txCertificates = v}

setTxUpdateProposal :: TxUpdateProposal era -> TxBodyContent build era -> TxBodyContent build era
setTxUpdateProposal v txBodyContent = txBodyContent{txUpdateProposal = v}

setTxMintValue :: TxMintValue build era -> TxBodyContent build era -> TxBodyContent build era
setTxMintValue v txBodyContent = txBodyContent{txMintValue = v}

setTxScriptValidity :: TxScriptValidity era -> TxBodyContent build era -> TxBodyContent build era
setTxScriptValidity v txBodyContent = txBodyContent{txScriptValidity = v}

setTxProposalProcedures
  :: Maybe (Featured ConwayEraOnwards era (TxProposalProcedures build era))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxProposalProcedures v txBodyContent = txBodyContent{txProposalProcedures = v}

setTxVotingProcedures
  :: Maybe (Featured ConwayEraOnwards era (TxVotingProcedures build era))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxVotingProcedures v txBodyContent = txBodyContent{txVotingProcedures = v}

setTxCurrentTreasuryValue
  :: Maybe (Featured ConwayEraOnwards era (Maybe L.Coin))
  -> TxBodyContent build era
  -> TxBodyContent build era
setTxCurrentTreasuryValue v txBodyContent = txBodyContent{txCurrentTreasuryValue = v}

setTxTreasuryDonation
  :: Maybe (Featured ConwayEraOnwards era L.Coin) -> TxBodyContent build era -> TxBodyContent build era
setTxTreasuryDonation v txBodyContent = txBodyContent{txTreasuryDonation = v}

getTxIdByron :: Byron.ATxAux ByteString -> TxId
getTxIdByron (Byron.ATxAux{Byron.aTaTx = txbody}) =
  TxId
    . fromMaybe impossible
    . Crypto.hashFromBytesShort
    . Byron.abstractHashToShort
    . Byron.hashDecoded
    $ txbody
 where
  impossible =
    error "getTxIdByron: byron and shelley hash sizes do not match"

-- | Calculate the transaction identifier for a 'TxBody'.
getTxId :: TxBody era -> TxId
getTxId (ShelleyTxBody sbe tx _ _ _ _) =
  shelleyBasedEraConstraints sbe $ getTxIdShelley sbe tx

getTxIdShelley
  :: Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Ledger.EraTxBody (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxId
getTxIdShelley _ tx =
  TxId
    . Crypto.castHash
    . (\(Ledger.TxId txhash) -> SafeHash.extractHash txhash)
    $ Ledger.txIdTxBody tx

-- ----------------------------------------------------------------------------
-- Constructing transaction bodies
--

data TxBodyError
  = TxBodyEmptyTxIns
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
  prettyError = \case
    TxBodyEmptyTxIns ->
      "Transaction body has no inputs"
    TxBodyEmptyTxInsCollateral ->
      "Transaction body has no collateral inputs, but uses Plutus scripts"
    TxBodyEmptyTxOuts ->
      "Transaction body has no outputs"
    TxBodyOutputNegative (Quantity q) txout ->
      "Negative quantity ("
        <> pretty q
        <> ") in transaction output: "
        <> pretty txout
    TxBodyOutputOverflow (Quantity q) txout ->
      "Quantity too large ("
        <> pretty q
        <> " >= 2^64) in transaction output: "
        <> pretty txout
    TxBodyMetadataError [(k, err)] ->
      "Error in metadata entry " <> pretty k <> ": " <> prettyError err
    TxBodyMetadataError errs ->
      mconcat
        [ "Error in metadata entries: "
        , mconcat $
            List.intersperse
              "; "
              [ pretty k <> ": " <> prettyError err
              | (k, err) <- errs
              ]
        ]
    TxBodyMintAdaError ->
      "Transaction cannot mint ada, only non-ada assets"
    TxBodyMissingProtocolParams ->
      "Transaction uses Plutus scripts but does not provide the protocol "
        <> "parameters to hash"
    TxBodyInIxOverflow txin ->
      "Transaction input index is too big, "
        <> "acceptable value is up to 2^32-1, "
        <> "in input "
        <> pretty txin
    TxBodyProtocolParamsConversionError ppces ->
      "Errors in protocol parameters conversion: " <> prettyError ppces

createTransactionBody
  :: ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
createTransactionBody sbe bc =
  shelleyBasedEraConstraints sbe $ do
    let era = toCardanoEra sbe
        apiTxOuts = txOuts bc
        apiScriptWitnesses = collectTxBodyScriptWitnesses sbe bc
        apiScriptValidity = txScriptValidity bc
        apiMintValue = txMintValue bc
        apiProtocolParameters = txProtocolParams bc
        apiCollateralTxIns = txInsCollateral bc
        apiReferenceInputs = txInsReference bc
        apiExtraKeyWitnesses = txExtraKeyWits bc
        apiReturnCollateral = txReturnCollateral bc
        apiTotalCollateral = txTotalCollateral bc

        -- Ledger types
        collTxIns = convCollateralTxIns apiCollateralTxIns
        refTxIns = convReferenceInputs apiReferenceInputs
        returnCollateral = convReturnCollateral sbe apiReturnCollateral
        totalCollateral = convTotalCollateral apiTotalCollateral
        certs = convCertificates sbe $ txCertificates bc
        txAuxData = toAuxiliaryData sbe (txMetadata bc) (txAuxScripts bc)
        scripts = convScripts apiScriptWitnesses
        languages = convLanguages apiScriptWitnesses
        sData = convScriptData sbe apiTxOuts apiScriptWitnesses
        proposalProcedures = convProposalProcedures $ maybe TxProposalProceduresNone unFeatured (txProposalProcedures bc)
        votingProcedures = convVotingProcedures $ maybe TxVotingProceduresNone unFeatured (txVotingProcedures bc)
        currentTreasuryValue = Ledger.maybeToStrictMaybe $ unFeatured =<< txCurrentTreasuryValue bc
        treasuryDonation = maybe 0 unFeatured $ txTreasuryDonation bc

    setUpdateProposal <- monoidForEraInEonA era $ \w ->
      Endo . (A.updateTxBodyL w .~) <$> convTxUpdateProposal sbe (txUpdateProposal bc)

    setInvalidBefore <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.invalidBeforeTxBodyL w .~ convValidityLowerBound (txValidityLowerBound bc)

    setMint <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.mintTxBodyL w .~ convMintValue apiMintValue

    setScriptIntegrityHash <- monoidForEraInEonA era $ \w ->
      pure $
        Endo $
          A.scriptIntegrityHashTxBodyL w .~ getScriptIntegrityHash apiProtocolParameters languages sData

    setCollateralInputs <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.collateralInputsTxBodyL w .~ collTxIns

    setReqSignerHashes <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.reqSignerHashesTxBodyL w .~ convExtraKeyWitnesses apiExtraKeyWitnesses

    setReferenceInputs <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.referenceInputsTxBodyL w .~ refTxIns

    setCollateralReturn <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.collateralReturnTxBodyL w .~ returnCollateral

    setTotalCollateral <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.totalCollateralTxBodyL w .~ totalCollateral

    setProposalProcedures <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.proposalProceduresTxBodyL w .~ proposalProcedures

    setVotingProcedures <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.votingProceduresTxBodyL w .~ votingProcedures

    setCurrentTreasuryValue <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.currentTreasuryValueTxBodyL w .~ currentTreasuryValue

    setTreasuryDonation <- monoidForEraInEonA era $ \w ->
      pure $ Endo $ A.treasuryDonationTxBodyL w .~ treasuryDonation

    let ledgerTxBody =
          mkCommonTxBody sbe (txIns bc) (txOuts bc) (txFee bc) (txWithdrawals bc) txAuxData
            & A.certsTxBodyL sbe .~ certs
            & A.invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe (txValidityUpperBound bc)
            & appEndo
              ( mconcat
                  [ setUpdateProposal
                  , setInvalidBefore
                  , setMint
                  , setScriptIntegrityHash
                  , setCollateralInputs
                  , setReqSignerHashes
                  , setReferenceInputs
                  , setCollateralReturn
                  , setTotalCollateral
                  , setProposalProcedures
                  , setVotingProcedures
                  , setCurrentTreasuryValue
                  , setTreasuryDonation
                  ]
              )

    -- TODO: NetworkId for hardware wallets. We don't always want this
    -- & L.networkIdTxBodyL .~ ...

    pure $ ShelleyTxBody sbe (ledgerTxBody ^. A.txBodyL) scripts sData txAuxData apiScriptValidity

getScriptIntegrityHash
  :: ()
  => BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Set Plutus.Language
  -> TxBodyScriptData era
  -> StrictMaybe (L.ScriptIntegrityHash (Ledger.EraCrypto (ShelleyLedgerEra era)))
getScriptIntegrityHash apiProtocolParameters languages = \case
  TxBodyNoScriptData -> SNothing
  TxBodyScriptData w datums redeemers ->
    convPParamsToScriptIntegrityHash w apiProtocolParameters redeemers datums languages

validateTxBodyContent
  :: ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError ()
validateTxBodyContent
  sbe
  txBodContent@TxBodyContent
    { txIns
    , txInsCollateral
    , txOuts
    , txProtocolParams
    , txMintValue
    , txMetadata
    } =
    let witnesses = collectTxBodyScriptWitnesses sbe txBodContent
        languages =
          fromList
            [ toAlonzoLanguage (AnyPlutusScriptVersion v)
            | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
            ]
            :: Set Plutus.Language
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
    TxMetadataNone -> return ()
    TxMetadataInEra _ m -> first TxBodyMetadataError (validateTxMetadata m)

validateProtocolParameters
  :: BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Set Plutus.Language
  -> Either TxBodyError ()
validateProtocolParameters txProtocolParams languages =
  case txProtocolParams of
    BuildTxWith Nothing
      | not (Set.null languages) ->
          Left TxBodyMissingProtocolParams
    _ -> return () -- TODO alonzo: validate protocol params for the Alonzo era.
    --             All the necessary params must be provided.

validateTxIns :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))] -> Either TxBodyError ()
validateTxIns txIns =
  sequence_
    [ inputIndexDoesNotExceedMax txIns
    , txBodyContentHasTxIns txIns
    ]

validateTxInsCollateral
  :: TxInsCollateral era -> Set Plutus.Language -> Either TxBodyError ()
validateTxInsCollateral txInsCollateral languages =
  case txInsCollateral of
    TxInsCollateralNone ->
      unless (Set.null languages) (Left TxBodyEmptyTxInsCollateral)
    TxInsCollateral _ collateralTxIns ->
      guardShelleyTxInsOverflow collateralTxIns

validateTxOuts :: ShelleyBasedEra era -> [TxOut CtxTx era] -> Either TxBodyError ()
validateTxOuts sbe txOuts = do
  let era = toCardanoEra sbe
  cardanoEraConstraints era $
    sequence_
      [ do
          positiveOutput era (txOutValueToValue v) txout
          outputDoesNotExceedMax era (txOutValueToValue v) txout
      | txout@(TxOut _ v _ _) <- txOuts
      ]

validateMintValue :: TxMintValue build era -> Either TxBodyError ()
validateMintValue txMintValue =
  case txMintValue of
    TxMintNone -> return ()
    TxMintValue _ v _ -> guard (selectLovelace v == 0) ?! TxBodyMintAdaError

inputIndexDoesNotExceedMax :: [(TxIn, a)] -> Either TxBodyError ()
inputIndexDoesNotExceedMax txIns =
  for_ txIns $ \(txin@(TxIn _ (TxIx txix)), _) ->
    guard (fromIntegral txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

outputDoesNotExceedMax
  :: ()
  => CardanoEra era
  -> Value
  -> TxOut CtxTx era
  -> Either TxBodyError ()
outputDoesNotExceedMax era v txout =
  case [q | (_, q) <- valueToList v, q > maxTxOut] of
    [] -> Right ()
    q : _ -> Left (TxBodyOutputOverflow q (txOutInAnyEra era txout))

positiveOutput
  :: ()
  => CardanoEra era
  -> Value
  -> TxOut CtxTx era
  -> Either TxBodyError ()
positiveOutput era v txout =
  case [q | (_, q) <- valueToList v, q < 0] of
    [] -> Right ()
    q : _ -> Left (TxBodyOutputNegative q (txOutInAnyEra era txout))

txBodyContentHasTxIns :: TxIns BuildTx era -> Either TxBodyError ()
txBodyContentHasTxIns txIns = guard (not (null txIns)) ?! TxBodyEmptyTxIns

maxShelleyTxInIx :: Word
maxShelleyTxInIx = fromIntegral $ maxBound @Word16

maxTxOut :: Quantity
maxTxOut = fromIntegral (maxBound :: Word64)

createAndValidateTransactionBody
  :: ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
createAndValidateTransactionBody = makeShelleyTransactionBody

pattern TxBody :: TxBodyContent ViewTx era -> TxBody era
pattern TxBody txbodycontent <- (getTxBodyContent -> txbodycontent)

{-# COMPLETE TxBody #-}

getTxBodyContent :: TxBody era -> TxBodyContent ViewTx era
getTxBodyContent = \case
  ShelleyTxBody sbe body _scripts scriptdata mAux scriptValidity ->
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
    { txIns = fromLedgerTxIns sbe body
    , txInsCollateral = fromLedgerTxInsCollateral sbe body
    , txInsReference = fromLedgerTxInsReference sbe body
    , txOuts = fromLedgerTxOuts sbe body scriptdata
    , txTotalCollateral = fromLedgerTxTotalCollateral sbe body
    , txReturnCollateral = fromLedgerTxReturnCollateral sbe body
    , txFee = fromLedgerTxFee sbe body
    , txValidityLowerBound = fromLedgerTxValidityLowerBound sbe (A.TxBody body)
    , txValidityUpperBound = fromLedgerTxValidityUpperBound sbe (A.TxBody body)
    , txWithdrawals = fromLedgerTxWithdrawals sbe body
    , txCertificates = fromLedgerTxCertificates sbe body
    , txUpdateProposal = maybeFromLedgerTxUpdateProposal sbe body
    , txMintValue = fromLedgerTxMintValue sbe body
    , txExtraKeyWits = fromLedgerTxExtraKeyWitnesses sbe body
    , txProtocolParams = ViewTx
    , txMetadata
    , txAuxScripts
    , txScriptValidity = scriptValidity
    , txProposalProcedures = fromLedgerProposalProcedures sbe body
    , txVotingProcedures = fromLedgerVotingProcedures sbe body
    , txCurrentTreasuryValue = fromLedgerCurrentTreasuryValue sbe body
    , txTreasuryDonation = fromLedgerTreasuryDonation sbe body
    }
 where
  (txMetadata, txAuxScripts) = fromLedgerTxAuxiliaryData sbe mAux

fromLedgerProposalProcedures
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era (TxProposalProcedures ViewTx era))
fromLedgerProposalProcedures sbe body =
  forShelleyBasedEraInEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w $
      Featured w $
        TxProposalProcedures
          (body ^. L.proposalProceduresTxBodyL)
          ViewTx

fromLedgerVotingProcedures
  :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era (TxVotingProcedures ViewTx era))
fromLedgerVotingProcedures sbe body =
  forShelleyBasedEraInEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w $
      Featured w $
        TxVotingProcedures
          (body ^. L.votingProceduresTxBodyL)
          ViewTx

fromLedgerCurrentTreasuryValue
  :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era (Maybe Coin))
fromLedgerCurrentTreasuryValue sbe body = forEraInEonMaybe (toCardanoEra sbe) $ \ceo ->
  conwayEraOnwardsConstraints ceo $
    Featured ceo . Ledger.strictMaybeToMaybe $
      body ^. L.currentTreasuryValueTxBodyL

fromLedgerTreasuryDonation
  :: ()
  => ShelleyBasedEra era
  -> L.TxBody (ShelleyLedgerEra era)
  -> Maybe (Featured ConwayEraOnwards era Coin)
fromLedgerTreasuryDonation sbe body =
  forShelleyBasedEraInEonMaybe sbe $ \w ->
    conwayEraOnwardsConstraints w $
      Featured w (body ^. L.treasuryDonationTxBodyL)

fromLedgerTxIns
  :: forall era
   . ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> [(TxIn, BuildTxWith ViewTx (Witness WitCtxTxIn era))]
fromLedgerTxIns sbe body =
  [ (fromShelleyTxIn input, ViewTx)
  | input <- toList (inputs_ sbe body)
  ]
 where
  inputs_
    :: ShelleyBasedEra era
    -> Ledger.TxBody (ShelleyLedgerEra era)
    -> Set (Ledger.TxIn StandardCrypto)
  inputs_ ShelleyBasedEraShelley = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraAllegra = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraMary = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraAlonzo = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraBabbage = view L.inputsTxBodyL
  inputs_ ShelleyBasedEraConway = view L.inputsTxBodyL

fromLedgerTxInsCollateral
  :: forall era
   . ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxInsCollateral era
fromLedgerTxInsCollateral sbe body =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxInsCollateralNone)
    (\w -> TxInsCollateral w $ map fromShelleyTxIn $ toList $ body ^. L.collateralInputsTxBodyL)
    sbe

fromLedgerTxInsReference
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxInsReference era
fromLedgerTxInsReference sbe txBody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxInsReferenceNone)
    (\w -> TxInsReference w $ map fromShelleyTxIn . toList $ txBody ^. L.referenceInputsTxBodyL)
    sbe

fromLedgerTxOuts
  :: forall era
   . ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxBodyScriptData era
  -> [TxOut CtxTx era]
fromLedgerTxOuts sbe body scriptdata =
  case sbe of
    ShelleyBasedEraShelley ->
      [fromShelleyTxOut sbe txout | txout <- toList (body ^. L.outputsTxBodyL)]
    ShelleyBasedEraAllegra ->
      [fromShelleyTxOut sbe txout | txout <- toList (body ^. L.outputsTxBodyL)]
    ShelleyBasedEraMary ->
      [fromShelleyTxOut sbe txout | txout <- toList (body ^. L.outputsTxBodyL)]
    ShelleyBasedEraAlonzo ->
      [ fromAlonzoTxOut
          AlonzoEraOnwardsAlonzo
          txout
      | txout <- toList (body ^. L.outputsTxBodyL)
      ]
    ShelleyBasedEraBabbage ->
      [ fromBabbageTxOut
          BabbageEraOnwardsBabbage
          txdatums
          txouts
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (body ^. L.outputsTxBodyL)
      ]
    ShelleyBasedEraConway ->
      [ fromBabbageTxOut
          BabbageEraOnwardsConway
          txdatums
          txouts
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (body ^. L.outputsTxBodyL)
      ]

selectTxDatums
  :: TxBodyScriptData era
  -> Map (L.DataHash StandardCrypto) (L.Data (ShelleyLedgerEra era))
selectTxDatums TxBodyNoScriptData = Map.empty
selectTxDatums (TxBodyScriptData _ (Alonzo.TxDats' datums) _) = datums

fromAlonzoTxOut
  :: ()
  => AlonzoEraOnwards era
  -> L.TxOut (ShelleyLedgerEra era)
  -> TxOut CtxTx era
fromAlonzoTxOut w txOut =
  alonzoEraOnwardsConstraints w $
    TxOut
      (fromShelleyAddr shelleyBasedEra (txOut ^. L.addrTxOutL))
      (TxOutValueShelleyBased sbe (txOut ^. L.valueTxOutL))
      TxOutDatumNone
      ReferenceScriptNone
 where
  sbe = alonzoEraOnwardsToShelleyBasedEra w

fromBabbageTxOut
  :: forall era
   . ()
  => BabbageEraOnwards era
  -> Map (L.DataHash StandardCrypto) (L.Data (ShelleyLedgerEra era))
  -> L.TxOut (ShelleyLedgerEra era)
  -> TxOut CtxTx era
fromBabbageTxOut w txdatums txout =
  babbageEraOnwardsConstraints w $
    TxOut
      (fromShelleyAddr shelleyBasedEra (txout ^. L.addrTxOutL))
      (TxOutValueShelleyBased sbe (txout ^. L.valueTxOutL))
      babbageTxOutDatum
      ( case txout ^. L.referenceScriptTxOutL of
          SNothing -> ReferenceScriptNone
          SJust rScript -> fromShelleyScriptToReferenceScript shelleyBasedEra rScript
      )
 where
  sbe = babbageEraOnwardsToShelleyBasedEra w

  -- NOTE: This is different to 'fromBabbageTxOutDatum' as it may resolve
  -- 'DatumHash' values using the datums included in the transaction.
  babbageTxOutDatum :: TxOutDatum CtxTx era
  babbageTxOutDatum =
    babbageEraOnwardsConstraints w $
      case txout ^. L.datumTxOutL of
        L.NoDatum -> TxOutDatumNone
        L.DatumHash dh -> resolveDatumInTx dh
        L.Datum d -> TxOutDatumInline w $ binaryDataToScriptData w d

  resolveDatumInTx :: L.DataHash StandardCrypto -> TxOutDatum CtxTx era
  resolveDatumInTx dh
    | Just d <- Map.lookup dh txdatums =
        TxOutDatumInTx' (babbageEraOnwardsToAlonzoEraOnwards w) (ScriptDataHash dh) (fromAlonzoData d)
    | otherwise = TxOutDatumHash (babbageEraOnwardsToAlonzoEraOnwards w) (ScriptDataHash dh)

fromLedgerTxTotalCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxTotalCollateral era
fromLedgerTxTotalCollateral sbe txbody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxTotalCollateralNone)
    ( \w ->
        case txbody ^. L.totalCollateralTxBodyL of
          SNothing -> TxTotalCollateralNone
          SJust totColl -> TxTotalCollateral w totColl
    )
    sbe

fromLedgerTxReturnCollateral
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxReturnCollateral CtxTx era
fromLedgerTxReturnCollateral sbe txbody =
  caseShelleyToAlonzoOrBabbageEraOnwards
    (const TxReturnCollateralNone)
    ( \w ->
        case txbody ^. L.collateralReturnTxBodyL of
          SNothing -> TxReturnCollateralNone
          SJust collReturnOut -> TxReturnCollateral w $ fromShelleyTxOut sbe collReturnOut
    )
    sbe

fromLedgerTxFee
  :: ShelleyBasedEra era -> Ledger.TxBody (ShelleyLedgerEra era) -> TxFee era
fromLedgerTxFee sbe body =
  shelleyBasedEraConstraints sbe $
    TxFeeExplicit sbe $
      body ^. L.feeTxBodyL

fromLedgerTxValidityLowerBound
  :: ShelleyBasedEra era
  -> A.TxBody era
  -> TxValidityLowerBound era
fromLedgerTxValidityLowerBound sbe body =
  caseShelleyEraOnlyOrAllegraEraOnwards
    (const TxValidityNoLowerBound)
    ( \w ->
        let mInvalidBefore = body ^. A.invalidBeforeTxBodyL w
         in case mInvalidBefore of
              Nothing -> TxValidityNoLowerBound
              Just s -> TxValidityLowerBound w s
    )
    sbe

fromLedgerTxValidityUpperBound
  :: ShelleyBasedEra era
  -> A.TxBody era
  -> TxValidityUpperBound era
fromLedgerTxValidityUpperBound sbe body =
  TxValidityUpperBound sbe $ body ^. A.invalidHereAfterTxBodyL sbe

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
  , fromShelleyBasedScript ShelleyBasedEraAlonzo
      <$> toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraBabbage txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraBabbage
      <$> toList (L.getAlonzoTxAuxDataScripts txAuxData)
  )
fromLedgerAuxiliaryData ShelleyBasedEraConway txAuxData =
  ( fromShelleyMetadata (L.atadMetadata txAuxData)
  , fromShelleyBasedScript ShelleyBasedEraConway
      <$> toList (L.getAlonzoTxAuxDataScripts txAuxData)
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
    caseShelleyEraOnlyOrAllegraEraOnwards
      (const TxAuxScriptsNone)
      ( \w ->
          case ss of
            [] -> TxAuxScriptsNone
            _ -> TxAuxScripts w ss
      )
      sbe

  (ms, ss) = fromLedgerAuxiliaryData sbe auxData

fromLedgerTxExtraKeyWitnesses
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxExtraKeyWitnesses era
fromLedgerTxExtraKeyWitnesses sbe body =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxExtraKeyWitnessesNone)
    ( \w ->
        let keyhashes = body ^. L.reqSignerHashesTxBodyL
         in if Set.null keyhashes
              then TxExtraKeyWitnessesNone
              else
                TxExtraKeyWitnesses
                  w
                  [ PaymentKeyHash (Shelley.coerceKeyRole keyhash)
                  | keyhash <- toList $ body ^. L.reqSignerHashesTxBodyL
                  ]
    )
    sbe

fromLedgerTxWithdrawals
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxWithdrawals ViewTx era
fromLedgerTxWithdrawals sbe body =
  shelleyBasedEraConstraints sbe $
    let withdrawals = body ^. L.withdrawalsTxBodyL
     in if null (L.unWithdrawals withdrawals)
          then TxWithdrawalsNone
          else TxWithdrawals sbe $ fromShelleyWithdrawal withdrawals

fromLedgerTxCertificates
  :: ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxCertificates ViewTx era
fromLedgerTxCertificates sbe body =
  shelleyBasedEraConstraints sbe $
    let certificates = body ^. L.certsTxBodyL
     in if null certificates
          then TxCertificatesNone
          else TxCertificates sbe (map (fromShelleyCertificate sbe) $ toList certificates) ViewTx

maybeFromLedgerTxUpdateProposal
  :: ()
  => ShelleyBasedEra era
  -> Ledger.TxBody (ShelleyLedgerEra era)
  -> TxUpdateProposal era
maybeFromLedgerTxUpdateProposal sbe body =
  caseShelleyToBabbageOrConwayEraOnwards
    ( \w ->
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
    ShelleyBasedEraMary -> toMintValue body MaryEraOnwardsMary
    ShelleyBasedEraAlonzo -> toMintValue body MaryEraOnwardsAlonzo
    ShelleyBasedEraBabbage -> toMintValue body MaryEraOnwardsBabbage
    ShelleyBasedEraConway -> toMintValue body MaryEraOnwardsConway
 where
  toMintValue txBody maInEra
    | L.isZero mint = TxMintNone
    | otherwise = TxMintValue maInEra (fromMaryValue mint) ViewTx
   where
    mint = MaryValue (Ledger.Coin 0) (txBody ^. L.mintTxBodyL)

makeByronTransactionBody
  :: ()
  => TxIns BuildTx ByronEra
  -> [TxOut CtxTx ByronEra]
  -> Either TxBodyError (Annotated Byron.Tx ByteString)
makeByronTransactionBody txIns txOuts = do
  ins' <- NonEmpty.nonEmpty (map fst txIns) ?! TxBodyEmptyTxIns
  for_ ins' $ \txin@(TxIn _ (TxIx txix)) ->
    guard (fromIntegral txix <= maxByronTxInIx) ?! TxBodyInIxOverflow txin
  let ins'' = fmap toByronTxIn ins'

  outs' <- NonEmpty.nonEmpty txOuts ?! TxBodyEmptyTxOuts
  outs'' <-
    traverse
      (\out -> toByronTxOut out ?! classifyRangeError out)
      outs'
  return $
    CBOR.reAnnotate CBOR.byronProtVer $
      Annotated
        (Byron.UnsafeTx ins'' outs'' (Byron.mkAttributes ()))
        ()
 where
  maxByronTxInIx :: Word
  maxByronTxInIx = fromIntegral (maxBound :: Word32)

classifyRangeError :: TxOut CtxTx ByronEra -> TxBodyError
classifyRangeError txout =
  case txout of
    TxOut (AddressInEra ByronAddressInAnyEra ByronAddress{}) (TxOutValueByron value) _ _
      | value < 0 -> TxBodyOutputNegative (lovelaceToQuantity value) (txOutInAnyEra ByronEra txout)
      | otherwise -> TxBodyOutputOverflow (lovelaceToQuantity value) (txOutInAnyEra ByronEra txout)
    TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _)) (TxOutValueShelleyBased w _) _ _ -> case w of {}
    TxOut (AddressInEra (ShelleyAddressInEra sbe) ShelleyAddress{}) _ _ _ -> case sbe of {}

convTxIns :: TxIns BuildTx era -> Set (L.TxIn StandardCrypto)
convTxIns txIns = fromList (map (toShelleyTxIn . fst) txIns)

convCollateralTxIns :: TxInsCollateral era -> Set (Ledger.TxIn StandardCrypto)
convCollateralTxIns txInsCollateral =
  case txInsCollateral of
    TxInsCollateralNone -> Set.empty
    TxInsCollateral _ txins -> fromList (map toShelleyTxIn txins)

convReturnCollateral
  :: ShelleyBasedEra era
  -> TxReturnCollateral ctx era
  -> StrictMaybe (Ledger.TxOut (ShelleyLedgerEra era))
convReturnCollateral sbe txReturnCollateral =
  case txReturnCollateral of
    TxReturnCollateralNone -> SNothing
    TxReturnCollateral _ colTxOut -> SJust $ toShelleyTxOutAny sbe colTxOut

convTotalCollateral :: TxTotalCollateral era -> StrictMaybe Ledger.Coin
convTotalCollateral txTotalCollateral =
  case txTotalCollateral of
    TxTotalCollateralNone -> SNothing
    TxTotalCollateral _ totCollLovelace -> SJust totCollLovelace

convTxOuts
  :: forall ctx era ledgerera
   . ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> [TxOut ctx era]
  -> Seq.StrictSeq (Ledger.TxOut ledgerera)
convTxOuts sbe txOuts = fromList $ map (toShelleyTxOutAny sbe) txOuts

convCertificates
  :: ShelleyBasedEra era
  -> TxCertificates build era
  -> Seq.StrictSeq (Shelley.TxCert (ShelleyLedgerEra era))
convCertificates _ = \case
  TxCertificatesNone -> Seq.empty
  TxCertificates _ cs _ -> fromList (map toShelleyCertificate cs)

convWithdrawals :: TxWithdrawals build era -> L.Withdrawals StandardCrypto
convWithdrawals txWithdrawals =
  case txWithdrawals of
    TxWithdrawalsNone -> L.Withdrawals Map.empty
    TxWithdrawals _ ws -> toShelleyWithdrawal ws

convTransactionFee :: ShelleyBasedEra era -> TxFee era -> Ledger.Coin
convTransactionFee _ (TxFeeExplicit _ fee) = fee

convValidityLowerBound
  :: ()
  => TxValidityLowerBound era
  -> Maybe SlotNo
convValidityLowerBound = \case
  TxValidityNoLowerBound -> Nothing
  TxValidityLowerBound _ s -> Just s

convValidityUpperBound
  :: ()
  => ShelleyBasedEra era
  -> TxValidityUpperBound era
  -> Maybe SlotNo
convValidityUpperBound _ = \case
  TxValidityUpperBound _ ms -> ms

-- | Convert transaction update proposal into ledger update proposal
convTxUpdateProposal
  :: ()
  => ShelleyBasedEra era
  -> TxUpdateProposal era
  -> Either TxBodyError (StrictMaybe (Ledger.Update (ShelleyLedgerEra era)))
  -- ^ 'Left' when there's protocol params conversion error, 'Right' otherwise, 'Right SNothing' means that
  -- there's no update proposal
convTxUpdateProposal sbe = \case
  TxUpdateProposalNone -> Right SNothing
  TxUpdateProposal _ p -> bimap TxBodyProtocolParamsConversionError pure $ toLedgerUpdate sbe p

convMintValue :: TxMintValue build era -> MultiAsset StandardCrypto
convMintValue txMintValue =
  case txMintValue of
    TxMintNone -> mempty
    TxMintValue _ v _ ->
      case toMaryValue v of
        MaryValue _ ma -> ma

convExtraKeyWitnesses
  :: TxExtraKeyWitnesses era -> Set (Shelley.KeyHash Shelley.Witness StandardCrypto)
convExtraKeyWitnesses txExtraKeyWits =
  case txExtraKeyWits of
    TxExtraKeyWitnessesNone -> Set.empty
    TxExtraKeyWitnesses _ khs ->
      fromList
        [ Shelley.asWitness kh
        | PaymentKeyHash kh <- khs
        ]

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
  :: ()
  => ShelleyBasedEra era
  -> [TxOut CtxTx era]
  -> [(ScriptWitnessIndex, AnyScriptWitness era)]
  -> TxBodyScriptData era
convScriptData sbe txOuts scriptWitnesses =
  caseShelleyToMaryOrAlonzoEraOnwards
    (const TxBodyNoScriptData)
    ( \w ->
        let redeemers =
              Alonzo.Redeemers $
                fromList
                  [ (i, (toAlonzoData d, toAlonzoExUnits e))
                  | ( idx
                      , AnyScriptWitness
                          (PlutusScriptWitness _ _ _ _ d e)
                      ) <-
                      scriptWitnesses
                  , Just i <- [fromScriptWitnessIndex w idx]
                  ]

            datums =
              Alonzo.TxDats $
                fromList
                  [ (L.hashData d', d')
                  | d <- scriptdata
                  , let d' = toAlonzoData d
                  ]

            scriptdata :: [HashableScriptData]
            scriptdata =
              [d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts]
                ++ [ d
                   | ( _
                      , AnyScriptWitness
                          ( PlutusScriptWitness
                              _
                              _
                              _
                              (ScriptDatumForTxIn (Just d))
                              _
                              _
                            )
                      ) <-
                      scriptWitnesses
                   ]
         in TxBodyScriptData w datums redeemers
    )
    sbe

convPParamsToScriptIntegrityHash
  :: ()
  => AlonzoEraOnwards era
  -> BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))
  -> Alonzo.Redeemers (ShelleyLedgerEra era)
  -> Alonzo.TxDats (ShelleyLedgerEra era)
  -> Set Plutus.Language
  -> StrictMaybe (L.ScriptIntegrityHash (Ledger.EraCrypto (ShelleyLedgerEra era)))
convPParamsToScriptIntegrityHash w txProtocolParams redeemers datums languages =
  alonzoEraOnwardsConstraints w $
    case txProtocolParams of
      BuildTxWith Nothing -> SNothing
      BuildTxWith (Just (LedgerProtocolParameters pp)) ->
        Alonzo.hashScriptIntegrity (Set.map (L.getLanguageView pp) languages) redeemers datums

convLanguages :: [(ScriptWitnessIndex, AnyScriptWitness era)] -> Set Plutus.Language
convLanguages witnesses =
  fromList
    [ toAlonzoLanguage (AnyPlutusScriptVersion v)
    | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
    ]

convReferenceInputs :: TxInsReference era -> Set (Ledger.TxIn StandardCrypto)
convReferenceInputs txInsReference =
  case txInsReference of
    TxInsReferenceNone -> mempty
    TxInsReference _ refTxins -> fromList $ map toShelleyTxIn refTxins

-- | Returns an OSet of proposals from 'TxProposalProcedures'.
--
-- If 'pws' in 'TxProposalProcedures pps (BuildTxWith pws)' contained proposals not present in 'pps', the'll
-- be sorted ascendingly and snoc-ed to 'pps' if they're not present in 'pps'.
convProposalProcedures
  :: TxProposalProcedures build era -> OSet (L.ProposalProcedure (ShelleyLedgerEra era))
convProposalProcedures TxProposalProceduresNone = OSet.empty
convProposalProcedures (TxProposalProcedures pp bWits) = do
  let wits = fromMaybe mempty $ buildTxWithToMaybe bWits
  pp |>< fromList (Map.keys wits)

convVotingProcedures :: TxVotingProcedures build era -> L.VotingProcedures (ShelleyLedgerEra era)
convVotingProcedures txVotingProcedures =
  case txVotingProcedures of
    TxVotingProceduresNone -> L.VotingProcedures Map.empty
    TxVotingProcedures vps _ -> vps

guardShelleyTxInsOverflow :: [TxIn] -> Either TxBodyError ()
guardShelleyTxInsOverflow txIns = do
  for_ txIns $ \txin@(TxIn _ (TxIx txix)) ->
    guard (txix <= maxShelleyTxInIx) ?! TxBodyInIxOverflow txin

-- | A helper function that constructs a TxBody with all of the fields that are common for
-- all eras
mkCommonTxBody
  :: ()
  => ShelleyBasedEra era
  -> TxIns BuildTx era
  -> [TxOut ctx era]
  -> TxFee era
  -> TxWithdrawals build era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
  -> A.TxBody era
mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData =
  shelleyBasedEraConstraints sbe $
    A.TxBody $
      L.mkBasicTxBody
        & L.inputsTxBodyL .~ convTxIns txIns
        & L.outputsTxBodyL .~ convTxOuts sbe txOuts
        & L.feeTxBodyL .~ convTransactionFee sbe txFee
        & L.withdrawalsTxBodyL .~ convWithdrawals txWithdrawals
        & L.auxDataHashTxBodyL .~ maybe SNothing (SJust . Ledger.hashTxAuxData) txAuxData

makeShelleyTransactionBody
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> Either TxBodyError (TxBody era)
makeShelleyTransactionBody
  sbe@ShelleyBasedEraShelley
  txbodycontent@TxBodyContent
    { txIns
    , txOuts
    , txFee
    , txValidityUpperBound
    , txMetadata
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    } = do
    let s2b = ShelleyToBabbageEraShelley
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.certsTxBodyL sbe .~ convCertificates sbe txCertificates
              & A.updateTxBodyL s2b .~ update
              & A.invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe txValidityUpperBound
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
   where
    scripts_ :: [Ledger.Script StandardShelley]
    scripts_ =
      catMaybes
        [ toShelleyScript <$> scriptWitnessScript scriptwitness
        | (_, AnyScriptWitness scriptwitness) <-
            collectTxBodyScriptWitnesses sbe txbodycontent
        ]

    txAuxData :: Maybe (L.TxAuxData StandardShelley)
    txAuxData = toAuxiliaryData sbe txMetadata TxAuxScriptsNone
makeShelleyTransactionBody
  sbe@ShelleyBasedEraAllegra
  txbodycontent@TxBodyContent
    { txIns
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    } = do
    let aOn = AllegraEraOnwardsAllegra
    let s2b = ShelleyToBabbageEraAllegra
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.certsTxBodyL sbe .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b .~ update
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts_
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
   where
    scripts_ :: [Ledger.Script StandardAllegra]
    scripts_ =
      catMaybes
        [ toShelleyScript <$> scriptWitnessScript scriptwitness
        | (_, AnyScriptWitness scriptwitness) <-
            collectTxBodyScriptWitnesses sbe txbodycontent
        ]

    txAuxData :: Maybe (L.TxAuxData StandardAllegra)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraMary
  txbodycontent@TxBodyContent
    { txIns
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    , txMintValue
    } = do
    let aOn = AllegraEraOnwardsMary
    let s2b = ShelleyToBabbageEraMary
    let mOn = MaryEraOnwardsMary
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.certsTxBodyL sbe .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b .~ update
              & A.mintTxBodyL mOn .~ convMintValue txMintValue
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        TxBodyNoScriptData
        txAuxData
        TxScriptValidityNone
   where
    scripts :: [Ledger.Script StandardMary]
    scripts =
      List.nub $
        catMaybes
          [ toShelleyScript <$> scriptWitnessScript scriptwitness
          | (_, AnyScriptWitness scriptwitness) <-
              collectTxBodyScriptWitnesses sbe txbodycontent
          ]

    txAuxData :: Maybe (L.TxAuxData StandardMary)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraAlonzo
  txbodycontent@TxBodyContent
    { txIns
    , txInsCollateral
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txExtraKeyWits
    , txProtocolParams
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    , txMintValue
    , txScriptValidity
    } = do
    let aOn = AllegraEraOnwardsAlonzo
    let s2b = ShelleyToBabbageEraAlonzo
    let mOn = MaryEraOnwardsAlonzo
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let scriptIntegrityHash =
          convPParamsToScriptIntegrityHash AlonzoEraOnwardsAlonzo txProtocolParams redeemers datums languages
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.collateralInputsTxBodyL azOn .~ convCollateralTxIns txInsCollateral
              & A.certsTxBodyL sbe .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b .~ update
              & A.reqSignerHashesTxBodyL azOn .~ convExtraKeyWitnesses txExtraKeyWits
              & A.mintTxBodyL mOn .~ convMintValue txMintValue
              & A.scriptIntegrityHashTxBodyL azOn .~ scriptIntegrityHash
              -- TODO Alonzo: support optional network id in TxBodyContent
              -- & L.networkIdTxBodyL .~ SNothing
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        (TxBodyScriptData AlonzoEraOnwardsAlonzo datums redeemers)
        txAuxData
        txScriptValidity
   where
    azOn = AlonzoEraOnwardsAlonzo

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness AlonzoEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script StandardAlonzo]
    scripts =
      List.nub $
        catMaybes
          [ toShelleyScript <$> scriptWitnessScript scriptwitness
          | (_, AnyScriptWitness scriptwitness) <- witnesses
          ]

    datums :: Alonzo.TxDats StandardAlonzo
    datums =
      Alonzo.TxDats $
        fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
      [d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts]
        ++ [ d
           | ( _
              , AnyScriptWitness
                  ( PlutusScriptWitness
                      _
                      _
                      _
                      (ScriptDatumForTxIn (Just d))
                      _
                      _
                    )
              ) <-
              witnesses
           ]

    redeemers :: Alonzo.Redeemers StandardAlonzo
    redeemers =
      Alonzo.Redeemers $
        fromList
          [ (i, (toAlonzoData d, toAlonzoExUnits e))
          | ( idx
              , AnyScriptWitness
                  (PlutusScriptWitness _ _ _ _ d e)
              ) <-
              witnesses
          , Just i <- [fromScriptWitnessIndex azOn idx]
          ]

    languages :: Set Plutus.Language
    languages =
      fromList
        [ toAlonzoLanguage (AnyPlutusScriptVersion v)
        | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
        ]

    txAuxData :: Maybe (L.TxAuxData StandardAlonzo)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraBabbage
  txbodycontent@TxBodyContent
    { txIns
    , txInsCollateral
    , txInsReference
    , txReturnCollateral
    , txTotalCollateral
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txExtraKeyWits
    , txProtocolParams
    , txWithdrawals
    , txCertificates
    , txUpdateProposal
    , txMintValue
    , txScriptValidity
    } = do
    let aOn = AllegraEraOnwardsBabbage
    let mOn = MaryEraOnwardsBabbage
    let bOn = BabbageEraOnwardsBabbage
    let s2b = ShelleyToBabbageEraBabbage
    validateTxBodyContent sbe txbodycontent
    update <- convTxUpdateProposal sbe txUpdateProposal
    let scriptIntegrityHash =
          convPParamsToScriptIntegrityHash AlonzoEraOnwardsBabbage txProtocolParams redeemers datums languages
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.collateralInputsTxBodyL azOn
                .~ case txInsCollateral of
                  TxInsCollateralNone -> Set.empty
                  TxInsCollateral _ txins -> fromList (map toShelleyTxIn txins)
              & A.referenceInputsTxBodyL bOn .~ convReferenceInputs txInsReference
              & A.collateralReturnTxBodyL bOn .~ convReturnCollateral sbe txReturnCollateral
              & A.totalCollateralTxBodyL bOn .~ convTotalCollateral txTotalCollateral
              & A.certsTxBodyL sbe .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe txValidityUpperBound
              & A.updateTxBodyL s2b .~ update
              & A.reqSignerHashesTxBodyL azOn .~ convExtraKeyWitnesses txExtraKeyWits
              & A.mintTxBodyL mOn .~ convMintValue txMintValue
              & A.scriptIntegrityHashTxBodyL azOn .~ scriptIntegrityHash
              -- TODO Babbage: support optional network id in TxBodyContent
              -- & L.networkIdTxBodyL .~ SNothing
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        ( TxBodyScriptData
            AlonzoEraOnwardsBabbage
            datums
            redeemers
        )
        txAuxData
        txScriptValidity
   where
    azOn = AlonzoEraOnwardsBabbage

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness BabbageEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script StandardBabbage]
    scripts =
      List.nub $
        catMaybes
          [ toShelleyScript <$> scriptWitnessScript scriptwitness
          | (_, AnyScriptWitness scriptwitness) <- witnesses
          ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats StandardBabbage
    datums =
      Alonzo.TxDats $
        fromList
          [ (L.hashData d', d')
          | d <- scriptdata
          , let d' = toAlonzoData d
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
      [d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts]
        ++ [ d
           | ( _
              , AnyScriptWitness
                  ( PlutusScriptWitness
                      _
                      _
                      _
                      (ScriptDatumForTxIn (Just d))
                      _
                      _
                    )
              ) <-
              witnesses
           ]

    redeemers :: Alonzo.Redeemers StandardBabbage
    redeemers =
      Alonzo.Redeemers $
        fromList
          [ (i, (toAlonzoData d, toAlonzoExUnits e))
          | ( idx
              , AnyScriptWitness
                  (PlutusScriptWitness _ _ _ _ d e)
              ) <-
              witnesses
          , Just i <- [fromScriptWitnessIndex azOn idx]
          ]

    languages :: Set Plutus.Language
    languages =
      fromList $
        catMaybes
          [ getScriptLanguage sw
          | (_, AnyScriptWitness sw) <- witnesses
          ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Plutus.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData StandardBabbage)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts
makeShelleyTransactionBody
  sbe@ShelleyBasedEraConway
  txbodycontent@TxBodyContent
    { txIns
    , txInsCollateral
    , txInsReference
    , txReturnCollateral
    , txTotalCollateral
    , txOuts
    , txFee
    , txValidityLowerBound
    , txValidityUpperBound
    , txMetadata
    , txAuxScripts
    , txExtraKeyWits
    , txProtocolParams
    , txWithdrawals
    , txCertificates
    , txMintValue
    , txScriptValidity
    , txProposalProcedures
    , txVotingProcedures
    , txCurrentTreasuryValue
    , txTreasuryDonation
    } = do
    let aOn = AllegraEraOnwardsConway
    let cOn = ConwayEraOnwardsConway
    let mOn = MaryEraOnwardsConway
    let bOn = BabbageEraOnwardsConway
    validateTxBodyContent sbe txbodycontent
    let scriptIntegrityHash =
          convPParamsToScriptIntegrityHash AlonzoEraOnwardsConway txProtocolParams redeemers datums languages
    let txbody =
          ( mkCommonTxBody sbe txIns txOuts txFee txWithdrawals txAuxData
              & A.collateralInputsTxBodyL azOn
                .~ case txInsCollateral of
                  TxInsCollateralNone -> Set.empty
                  TxInsCollateral _ txins -> fromList (map toShelleyTxIn txins)
              & A.referenceInputsTxBodyL bOn .~ convReferenceInputs txInsReference
              & A.collateralReturnTxBodyL bOn .~ convReturnCollateral sbe txReturnCollateral
              & A.totalCollateralTxBodyL bOn .~ convTotalCollateral txTotalCollateral
              & A.certsTxBodyL sbe .~ convCertificates sbe txCertificates
              & A.invalidBeforeTxBodyL aOn .~ convValidityLowerBound txValidityLowerBound
              & A.invalidHereAfterTxBodyL sbe .~ convValidityUpperBound sbe txValidityUpperBound
              & A.reqSignerHashesTxBodyL azOn .~ convExtraKeyWitnesses txExtraKeyWits
              & A.mintTxBodyL mOn .~ convMintValue txMintValue
              & A.scriptIntegrityHashTxBodyL azOn .~ scriptIntegrityHash
              & A.votingProceduresTxBodyL cOn
                .~ convVotingProcedures (maybe TxVotingProceduresNone unFeatured txVotingProcedures)
              & A.proposalProceduresTxBodyL cOn
                .~ convProposalProcedures (maybe TxProposalProceduresNone unFeatured txProposalProcedures)
              & A.currentTreasuryValueTxBodyL cOn
                .~ Ledger.maybeToStrictMaybe (unFeatured =<< txCurrentTreasuryValue)
              & A.treasuryDonationTxBodyL cOn .~ maybe (L.Coin 0) unFeatured txTreasuryDonation
              -- TODO Conway: support optional network id in TxBodyContent
              -- & L.networkIdTxBodyL .~ SNothing
          )
            ^. A.txBodyL
    return $
      ShelleyTxBody
        sbe
        txbody
        scripts
        ( TxBodyScriptData
            AlonzoEraOnwardsConway
            datums
            redeemers
        )
        txAuxData
        txScriptValidity
   where
    azOn = AlonzoEraOnwardsConway

    witnesses :: [(ScriptWitnessIndex, AnyScriptWitness ConwayEra)]
    witnesses = collectTxBodyScriptWitnesses sbe txbodycontent

    scripts :: [Ledger.Script StandardConway]
    scripts =
      catMaybes
        [ toShelleyScript <$> scriptWitnessScript scriptwitness
        | (_, AnyScriptWitness scriptwitness) <- witnesses
        ]

    -- Note these do not include inline datums!
    datums :: Alonzo.TxDats StandardConway
    datums =
      Alonzo.TxDats $
        fromList
          [ (L.hashData d, d)
          | d <- toAlonzoData <$> scriptdata
          ]

    scriptdata :: [HashableScriptData]
    scriptdata =
      [d | TxOut _ _ (TxOutDatumInTx _ d) _ <- txOuts]
        ++ [ d
           | ( _
              , AnyScriptWitness
                  ( PlutusScriptWitness
                      _
                      _
                      _
                      (ScriptDatumForTxIn (Just d))
                      _
                      _
                    )
              ) <-
              witnesses
           ]

    redeemers :: Alonzo.Redeemers StandardConway
    redeemers =
      Alonzo.Redeemers $
        fromList
          [ (i, (toAlonzoData d, toAlonzoExUnits e))
          | ( idx
              , AnyScriptWitness
                  (PlutusScriptWitness _ _ _ _ d e)
              ) <-
              witnesses
          , Just i <- [fromScriptWitnessIndex azOn idx]
          ]

    languages :: Set Plutus.Language
    languages =
      fromList $
        catMaybes
          [ getScriptLanguage sw
          | (_, AnyScriptWitness sw) <- witnesses
          ]

    getScriptLanguage :: ScriptWitness witctx era -> Maybe Plutus.Language
    getScriptLanguage (PlutusScriptWitness _ v _ _ _ _) =
      Just $ toAlonzoLanguage (AnyPlutusScriptVersion v)
    getScriptLanguage SimpleScriptWitness{} = Nothing

    txAuxData :: Maybe (L.TxAuxData StandardConway)
    txAuxData = toAuxiliaryData sbe txMetadata txAuxScripts

-- | A variant of 'toShelleyTxOutAny that is used only internally to this module
-- that works with a 'TxOut' in any context (including CtxTx) by ignoring
-- embedded datums (taking only their hash).
toShelleyTxOutAny
  :: forall ctx era ledgerera
   . ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> TxOut ctx era
  -> Ledger.TxOut ledgerera
toShelleyTxOutAny _ = \case
  TxOut _ (TxOutValueByron _) _ _ ->
    -- TODO: Temporary until we have basic tx
    -- construction functionality
    error "toShelleyTxOutAny: Expected a Shelley value"
  TxOut addr (TxOutValueShelleyBased sbe value) txoutdata refScript ->
    caseShelleyToMaryOrAlonzoEraOnwards
      (const $ L.mkBasicTxOut (toShelleyAddr addr) value)
      ( \case
          AlonzoEraOnwardsAlonzo ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.dataHashTxOutL .~ toAlonzoTxOutDatumHash txoutdata
          AlonzoEraOnwardsBabbage ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL .~ toBabbageTxOutDatum txoutdata
              & L.referenceScriptTxOutL .~ refScriptToShelleyScript sbe refScript
          AlonzoEraOnwardsConway ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL .~ toBabbageTxOutDatum txoutdata
              & L.referenceScriptTxOutL .~ refScriptToShelleyScript sbe refScript
      )
      sbe

-- ----------------------------------------------------------------------------
-- Script witnesses within the tx body
--

-- | A 'ScriptWitness' in any 'WitCtx'. This lets us handle heterogeneous
-- collections of script witnesses from multiple contexts.
data AnyScriptWitness era where
  AnyScriptWitness :: ScriptWitness witctx era -> AnyScriptWitness era

deriving instance Show (AnyScriptWitness era)

-- | Identify the location of a 'ScriptWitness' within the context of a
-- 'TxBody'. These are indexes of the objects within the transaction that
-- need or can use script witnesses: inputs, minted assets, withdrawals and
-- certificates. These are simple numeric indices, enumerated from zero.
-- Thus the indices are not stable if the transaction body is modified.
data ScriptWitnessIndex
  = -- | The n'th transaction input, in the order of the 'TxId's.
    ScriptWitnessIndexTxIn !Word32
  | -- | The n'th minting 'PolicyId', in the order of the 'PolicyId's.
    ScriptWitnessIndexMint !Word32
  | -- | The n'th certificate, in the list order of the certificates.
    ScriptWitnessIndexCertificate !Word32
  | -- | The n'th withdrawal, in the order of the 'StakeAddress's.
    ScriptWitnessIndexWithdrawal !Word32
  | -- | The n'th vote, in the order of the votes.
    ScriptWitnessIndexVoting !Word32
  | -- | The n'th proposal, in the order of the proposals.
    ScriptWitnessIndexProposing !Word32
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
    ScriptWitnessIndexVoting n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexVoting"
        , "value" .= n
        ]
    ScriptWitnessIndexProposing n ->
      object
        [ "kind" .= Aeson.String "ScriptWitnessIndexProposing"
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
renderScriptWitnessIndex (ScriptWitnessIndexVoting index) =
  "vote " <> show index <> " (in ascending order of the votes)"
renderScriptWitnessIndex (ScriptWitnessIndexProposing index) =
  "proposal " <> show index <> " (in ascending order of the proposals)"

fromScriptWitnessIndex
  :: AlonzoEraOnwards era
  -> ScriptWitnessIndex
  -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
fromScriptWitnessIndex aOnwards widx =
  case aOnwards of
    AlonzoEraOnwardsAlonzo -> fromScriptWitnessIndexAlonzo widx
    AlonzoEraOnwardsBabbage -> fromScriptWitnessIndexBabbage widx
    AlonzoEraOnwardsConway -> fromScriptWitnessIndexConway widx

fromScriptWitnessIndexAlonzo
  :: ScriptWitnessIndex -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra AlonzoEra))
fromScriptWitnessIndexAlonzo i =
  case i of
    ScriptWitnessIndexTxIn n -> Just $ L.AlonzoSpending (L.AsIx n)
    ScriptWitnessIndexMint n -> Just $ L.AlonzoMinting (L.AsIx n)
    ScriptWitnessIndexCertificate n -> Just $ L.AlonzoCertifying (L.AsIx n)
    ScriptWitnessIndexWithdrawal n -> Just $ L.AlonzoRewarding (L.AsIx n)
    _ -> Nothing

fromScriptWitnessIndexBabbage
  :: ScriptWitnessIndex -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra BabbageEra))
fromScriptWitnessIndexBabbage i =
  case i of
    ScriptWitnessIndexTxIn n -> Just $ L.AlonzoSpending (L.AsIx n)
    ScriptWitnessIndexMint n -> Just $ L.AlonzoMinting (L.AsIx n)
    ScriptWitnessIndexCertificate n -> Just $ L.AlonzoCertifying (L.AsIx n)
    ScriptWitnessIndexWithdrawal n -> Just $ L.AlonzoRewarding (L.AsIx n)
    _ -> Nothing

fromScriptWitnessIndexConway
  :: ScriptWitnessIndex -> Maybe (L.PlutusPurpose L.AsIx (ShelleyLedgerEra ConwayEra))
fromScriptWitnessIndexConway i =
  case i of
    ScriptWitnessIndexTxIn n -> Just $ L.ConwaySpending (L.AsIx n)
    ScriptWitnessIndexMint n -> Just $ L.ConwayMinting (L.AsIx n)
    ScriptWitnessIndexCertificate n -> Just $ L.ConwayCertifying (L.AsIx n)
    ScriptWitnessIndexWithdrawal n -> Just $ L.ConwayRewarding (L.AsIx n)
    ScriptWitnessIndexVoting n -> Just $ L.ConwayVoting (L.AsIx n)
    ScriptWitnessIndexProposing n -> Just $ L.ConwayProposing (L.AsIx n)

toScriptIndex
  :: AlonzoEraOnwards era
  -> L.PlutusPurpose L.AsIx (ShelleyLedgerEra era)
  -> ScriptWitnessIndex
toScriptIndex sbe scriptPurposeIndex =
  case sbe of
    AlonzoEraOnwardsAlonzo -> toScriptIndexAlonzo scriptPurposeIndex
    AlonzoEraOnwardsBabbage -> toScriptIndexAlonzo scriptPurposeIndex
    AlonzoEraOnwardsConway -> toScriptIndexConway scriptPurposeIndex

toScriptIndexAlonzo
  :: L.AlonzoPlutusPurpose L.AsIx (ShelleyLedgerEra era)
  -> ScriptWitnessIndex
toScriptIndexAlonzo scriptPurposeIndex =
  case scriptPurposeIndex of
    L.AlonzoSpending (L.AsIx i) -> ScriptWitnessIndexTxIn i
    L.AlonzoMinting (L.AsIx i) -> ScriptWitnessIndexMint i
    L.AlonzoCertifying (L.AsIx i) -> ScriptWitnessIndexCertificate i
    L.AlonzoRewarding (L.AsIx i) -> ScriptWitnessIndexWithdrawal i

toScriptIndexConway
  :: L.ConwayPlutusPurpose L.AsIx (ShelleyLedgerEra era)
  -> ScriptWitnessIndex
toScriptIndexConway scriptPurposeIndex =
  case scriptPurposeIndex of
    L.ConwaySpending (L.AsIx i) -> ScriptWitnessIndexTxIn i
    L.ConwayMinting (L.AsIx i) -> ScriptWitnessIndexMint i
    L.ConwayCertifying (L.AsIx i) -> ScriptWitnessIndexCertificate i
    L.ConwayRewarding (L.AsIx i) -> ScriptWitnessIndexWithdrawal i
    L.ConwayVoting (L.AsIx i) -> ScriptWitnessIndexVoting i
    L.ConwayProposing (L.AsIx i) -> ScriptWitnessIndexProposing i

collectTxBodyScriptWitnesses
  :: forall era
   . ShelleyBasedEra era
  -> TxBodyContent BuildTx era
  -> [(ScriptWitnessIndex, AnyScriptWitness era)]
collectTxBodyScriptWitnesses
  _
  TxBodyContent
    { txIns
    , txWithdrawals
    , txCertificates
    , txMintValue
    , txVotingProcedures
    , txProposalProcedures
    } =
    concat
      [ scriptWitnessesTxIns txIns
      , scriptWitnessesWithdrawals txWithdrawals
      , scriptWitnessesCertificates txCertificates
      , scriptWitnessesMinting txMintValue
      , scriptWitnessesVoting (maybe TxVotingProceduresNone unFeatured txVotingProcedures)
      , scriptWitnessesProposing (maybe TxProposalProceduresNone unFeatured txProposalProcedures)
      ]
   where
    scriptWitnessesTxIns
      :: [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesTxIns txins =
      [ (ScriptWitnessIndexTxIn ix, AnyScriptWitness witness)
      | -- The tx ins are indexed in the map order by txid
      (ix, (_, BuildTxWith (ScriptWitness _ witness))) <-
        zip [0 ..] (orderTxIns txins)
      ]

    scriptWitnessesWithdrawals
      :: TxWithdrawals BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesWithdrawals TxWithdrawalsNone = []
    scriptWitnessesWithdrawals (TxWithdrawals _ withdrawals) =
      [ (ScriptWitnessIndexWithdrawal ix, AnyScriptWitness witness)
      | -- The withdrawals are indexed in the map order by stake credential
      (ix, (_, _, BuildTxWith (ScriptWitness _ witness))) <-
        zip [0 ..] (orderStakeAddrs withdrawals)
      ]

    scriptWitnessesCertificates
      :: TxCertificates BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesCertificates TxCertificatesNone = []
    scriptWitnessesCertificates (TxCertificates _ certs (BuildTxWith witnesses)) =
      [ (ScriptWitnessIndexCertificate ix, AnyScriptWitness witness)
      | -- The certs are indexed in list order
      (ix, cert) <- zip [0 ..] certs
      , ScriptWitness _ witness <- maybeToList $ do
          stakecred <- selectStakeCredentialWitness cert
          List.lookup stakecred witnesses
      ]

    scriptWitnessesMinting
      :: TxMintValue BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesMinting TxMintNone = []
    scriptWitnessesMinting (TxMintValue _ value (BuildTxWith witnesses)) =
      [ (ScriptWitnessIndexMint ix, AnyScriptWitness witness)
      | -- The minting policies are indexed in policy id order in the value
      let ValueNestedRep bundle = valueToNestedRep value
      , (ix, ValueNestedBundle policyid _) <- zip [0 ..] bundle
      , witness <- maybeToList (Map.lookup policyid witnesses)
      ]

    scriptWitnessesVoting
      :: TxVotingProcedures BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesVoting TxVotingProceduresNone = []
    scriptWitnessesVoting (TxVotingProcedures (L.VotingProcedures votes) (BuildTxWith witnesses)) =
      [ (ScriptWitnessIndexVoting ix, AnyScriptWitness witness)
      | let voterList = toList votes
      , (ix, (voter, _)) <- zip [0 ..] voterList
      , witness <- maybeToList (Map.lookup voter witnesses)
      ]

    scriptWitnessesProposing
      :: TxProposalProcedures BuildTx era
      -> [(ScriptWitnessIndex, AnyScriptWitness era)]
    scriptWitnessesProposing TxProposalProceduresNone = []
    scriptWitnessesProposing (TxProposalProcedures proposalProcedures (BuildTxWith mScriptWitnesses))
      | Map.null mScriptWitnesses = []
      | otherwise =
          [ (ScriptWitnessIndexProposing ix, AnyScriptWitness witness)
          | let proposalsList = toList proposalProcedures
          , (ix, proposal) <- zip [0 ..] proposalsList
          , witness <- maybeToList (Map.lookup proposal mScriptWitnesses)
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

-- TODO: Investigate if we need
toShelleyWithdrawal :: [(StakeAddress, L.Coin, a)] -> L.Withdrawals StandardCrypto
toShelleyWithdrawal withdrawals =
  L.Withdrawals $
    fromList
      [ (toShelleyStakeAddr stakeAddr, value)
      | (stakeAddr, value, _) <- withdrawals
      ]

fromShelleyWithdrawal
  :: L.Withdrawals StandardCrypto
  -> [(StakeAddress, L.Coin, BuildTxWith ViewTx (Witness WitCtxStake era))]
fromShelleyWithdrawal (L.Withdrawals withdrawals) =
  [ (fromShelleyStakeAddr stakeAddr, value, ViewTx)
  | (stakeAddr, value) <- Map.assocs withdrawals
  ]

-- | In the Allegra and Mary eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts. In the Alonzo and later eras the auxiliary data consists of the tx metadata
-- and the axiliary scripts, and the axiliary script data.
toAuxiliaryData
  :: ShelleyBasedEra era
  -> TxMetadataInEra era
  -> TxAuxScripts era
  -> Maybe (L.TxAuxData (ShelleyLedgerEra era))
toAuxiliaryData sbe txMetadata txAuxScripts =
  let ms = case txMetadata of
        TxMetadataNone -> Map.empty
        TxMetadataInEra _ (TxMetadata ms') -> toShelleyMetadata ms'
      ss = case txAuxScripts of
        TxAuxScriptsNone -> []
        TxAuxScripts _ ss' -> map toShelleyScript ss'
   in case sbe of
        ShelleyBasedEraShelley ->
          guard (not (Map.null ms)) $> L.ShelleyTxAuxData ms
        ShelleyBasedEraAllegra ->
          guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (fromList ss)
        ShelleyBasedEraMary ->
          guard (not (Map.null ms && null ss)) $> L.AllegraTxAuxData ms (fromList ss)
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
genesisUTxOPseudoTxIn :: NetworkId -> Hash GenesisUTxOKey -> TxIn
genesisUTxOPseudoTxIn nw (GenesisUTxOKeyHash kh) =
  -- TODO: should handle Byron UTxO case too.
  fromShelleyTxIn (Shelley.initialFundsPseudoTxIn addr)
 where
  addr :: L.Addr StandardCrypto
  addr =
    L.Addr
      (toShelleyNetwork nw)
      (Shelley.KeyHashObj kh)
      Shelley.StakeRefNull

-- | Calculate the reference inputs size in bytes for provided set of transaction IDs and UTXOs.
getReferenceInputsSizeForTxIds
  :: ShelleyLedgerEra era ~ ledgerera
  => BabbageEraOnwards era
  -> Ledger.UTxO ledgerera
  -> Set TxIn
  -> Int
getReferenceInputsSizeForTxIds beo utxo txIds = babbageEraOnwardsConstraints beo $ do
  let refScripts = L.getReferenceScriptsNonDistinct utxo (Set.map toShelleyTxIn txIds)
  getSum $ foldMap (Sum . SafeHash.originalBytesSize . snd) refScripts

calculateExecutionUnitsLovelace :: Ledger.Prices -> ExecutionUnits -> Maybe L.Coin
calculateExecutionUnitsLovelace prices eUnits =
  return $ Alonzo.txscriptfee prices (toAlonzoExUnits eUnits)

-- ----------------------------------------------------------------------------
-- Inline data
--

-- | Conversion of ScriptData to binary data which allows for the storage of data
-- onchain within a transaction output.
scriptDataToInlineDatum :: L.Era ledgerera => HashableScriptData -> L.Datum ledgerera
scriptDataToInlineDatum d =
  L.Datum . L.dataToBinaryData $ toAlonzoData d

binaryDataToScriptData
  :: L.Era ledgerera
  => BabbageEraOnwards era
  -> L.BinaryData ledgerera
  -> HashableScriptData
binaryDataToScriptData BabbageEraOnwardsBabbage d =
  fromAlonzoData $ L.binaryDataToData d
binaryDataToScriptData BabbageEraOnwardsConway d =
  fromAlonzoData $ L.binaryDataToData d
