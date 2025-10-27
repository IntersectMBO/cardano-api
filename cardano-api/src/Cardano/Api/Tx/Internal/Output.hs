{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Tx.Internal.Output
  ( -- * Transaction outputs
    TxOut (..)

    -- ** Transaction output contexts
  , CtxTx
  , CtxUTxO
  , toCtxUTxOTxOut
  , fromCtxUTxOTxOut

    -- ** Ledger conversion functions for outputs
  , fromShelleyTxOut
  , toShelleyTxOut
  , toShelleyTxOutAny
  , convTxOuts
  , fromLedgerTxOuts
  , toByronTxOut
  --  ** An Output Value
  , TxOutValue (..)
  , lovelaceToTxOutValue
  , txOutValueToLovelace
  , txOutValueToValue

    -- ** Datum
  , TxOutDatum (..)
  , binaryDataToScriptData
  , scriptDataToInlineDatum

    -- ** Existential type over an era
  , TxOutInAnyEra (..)
  , txOutInAnyEra

    -- ** Utilities
  , validateTxOuts
  , prettyRenderTxOut

    -- ** Error types
  , TxOutputError (..)
  )
where

import Cardano.Api.Address
import Cardano.Api.Era.Internal.Case
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Error (Error (..), displayError)
import Cardano.Api.HasTypeProxy qualified as HTP
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger
import Cardano.Api.Monad.Error
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Plutus.Internal.ScriptData
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Json
import Cardano.Api.Tx.Internal.Body.Lens qualified as A
import Cardano.Api.Tx.Internal.Sign
import Cardano.Api.Value.Internal

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Ledger.Allegra.Core qualified as L
import Cardano.Ledger.Alonzo.Core qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Plutus.Data qualified as Plutus

import Data.Aeson (object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific (toBoundedInteger)
import Data.Sequence.Strict qualified as Seq
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Type.Equality
import Data.Typeable (Typeable)
import Data.Word
import GHC.Exts (IsList (..))
import GHC.Stack
import Lens.Micro hiding (ix)

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

instance (Typeable ctx, IsShelleyBasedEra era) => HTP.HasTypeProxy (TxOut ctx era) where
  data AsType (TxOut ctx era) = AsTxOut (AsType era)
  proxyToAsType :: HTP.Proxy (TxOut ctx era) -> AsType (TxOut ctx era)
  proxyToAsType _ = AsTxOut (HTP.asType @era)

-- | We do not provide a 'ToCBOR' instance for 'TxOut' because 'TxOut's can contain
-- supplemental datums and the ledger's CBOR representation does not support this.
-- For this reason, if we were to serialise a 'TxOut' with a supplemental datum,
-- we would lose information and the roundtrip property would not hold.
instance (Typeable ctx, IsShelleyBasedEra era) => FromCBOR (TxOut ctx era) where
  fromCBOR :: Ledger.Decoder s (TxOut ctx era)
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      pure (fromShelleyTxOut shelleyBasedEra) <*> L.fromEraCBOR @(ShelleyLedgerEra era)

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
        TxOutSupplementalDatum s datum -> TxOutDatumHash s $ hashScriptDataBytes datum
        TxOutDatumInline s sd -> TxOutDatumInline s sd
   in TxOut addr val dat refS

fromCtxUTxOTxOut :: TxOut CtxUTxO era -> TxOut CtxTx era
fromCtxUTxOTxOut (TxOut addr val d refS) =
  let dat = case d of
        TxOutDatumNone -> TxOutDatumNone
        TxOutDatumHash s h -> TxOutDatumHash s h
        TxOutDatumInline s sd -> TxOutDatumInline s sd
   in TxOut addr val dat refS

convTxOuts
  :: forall ctx era ledgerera
   . HasCallStack
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> [TxOut ctx era]
  -> Seq.StrictSeq (Ledger.TxOut ledgerera)
convTxOuts sbe txOuts = fromList $ map (toShelleyTxOutAny sbe) txOuts

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
          txdatums
          txout
      | let txdatums = selectTxDatums scriptdata
      , txout <- toList (body ^. L.outputsTxBodyL)
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
    ShelleyBasedEraDijkstra ->
      [ fromBabbageTxOut
          BabbageEraOnwardsDijkstra
          txdatums
          txouts
      | let txdatums = selectTxDatums scriptdata
      , txouts <- toList (body ^. L.outputsTxBodyL)
      ]

validateTxOuts :: ShelleyBasedEra era -> [TxOut CtxTx era] -> Either TxOutputError ()
validateTxOuts sbe txOuts = do
  let era = toCardanoEra sbe
  cardanoEraConstraints era $
    sequence_
      [ do
          positiveOutput era (txOutValueToValue v) txout
          outputDoesNotExceedMax era (txOutValueToValue v) txout
      | txout@(TxOut _ v _ _) <- txOuts
      ]

outputDoesNotExceedMax
  :: ()
  => CardanoEra era
  -> Value
  -> TxOut CtxTx era
  -> Either TxOutputError ()
outputDoesNotExceedMax era v txout =
  case [q | (_, q) <- toList v, q > maxTxOut] of
    [] -> Right ()
    q : _ -> Left (TxOutputOverflow q (txOutInAnyEra era txout))

positiveOutput
  :: ()
  => CardanoEra era
  -> Value
  -> TxOut CtxTx era
  -> Either TxOutputError ()
positiveOutput era v txout =
  case [q | (_, q) <- toList v, q < 0] of
    [] -> Right ()
    q : _ -> Left (TxOutputNegative q (txOutInAnyEra era txout))

maxTxOut :: Quantity
maxTxOut = fromIntegral (maxBound :: Word64)

fromAlonzoTxOut
  :: forall era ledgerera
   . AlonzoEraOnwards era
  -> Map L.DataHash (L.Data ledgerera)
  -> L.TxOut (ShelleyLedgerEra era)
  -> TxOut CtxTx era
fromAlonzoTxOut w txdatums txOut =
  alonzoEraOnwardsConstraints w $
    TxOut
      (fromShelleyAddr shelleyBasedEra (txOut ^. L.addrTxOutL))
      (TxOutValueShelleyBased sbe (txOut ^. L.valueTxOutL))
      (fromAlonzoTxOutDatum w txdatums (txOut ^. L.dataHashTxOutL))
      ReferenceScriptNone
 where
  sbe :: ShelleyBasedEra era = convert w

fromAlonzoTxOutDatum
  :: ()
  => AlonzoEraOnwards era
  -> Map L.DataHash (L.Data ledgerera)
  -> StrictMaybe L.DataHash
  -> TxOutDatum CtxTx era
fromAlonzoTxOutDatum w txdatums = \case
  SNothing -> TxOutDatumNone
  SJust dh
    | Just d <- Map.lookup dh txdatums ->
        TxOutSupplementalDatum w (fromAlonzoData d)
    | otherwise -> TxOutDatumHash w (ScriptDataHash dh)

fromBabbageTxOut
  :: forall era
   . BabbageEraOnwards era
  -> Map L.DataHash (L.Data (ShelleyLedgerEra era))
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
  sbe :: ShelleyBasedEra era = convert w

  -- NOTE: This is different to 'fromBabbageTxOutDatum' as it may resolve
  -- 'DatumHash' values using the datums included in the transaction.
  babbageTxOutDatum :: TxOutDatum CtxTx era
  babbageTxOutDatum =
    babbageEraOnwardsConstraints w $
      case txout ^. L.datumTxOutL of
        L.NoDatum -> TxOutDatumNone
        L.DatumHash dh -> resolveDatumInTx dh
        L.Datum d -> TxOutDatumInline w $ binaryDataToScriptData w d

  resolveDatumInTx :: L.DataHash -> TxOutDatum CtxTx era
  resolveDatumInTx dh
    | Just d <- Map.lookup dh txdatums =
        TxOutSupplementalDatum
          (convert w)
          (fromAlonzoData d)
    | otherwise = TxOutDatumHash (convert w) (ScriptDataHash dh)

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
        , "inlineDatumRaw" .= inlineDatumRawJsonCbor dat
        , "referenceScript" .= refScriptJsonVal refScript
        ]
    ConwayEra ->
      object
        [ "address" .= addr
        , "value" .= val
        , datHashJsonVal dat
        , "datum" .= datJsonVal dat
        , "inlineDatum" .= inlineDatumJsonVal dat
        , "inlineDatumRaw" .= inlineDatumRawJsonCbor dat
        , "referenceScript" .= refScriptJsonVal refScript
        ]
    DijkstraEra ->
      object
        [ "address" .= addr
        , "value" .= val
        , datHashJsonVal dat
        , "datum" .= datJsonVal dat
        , "inlineDatum" .= inlineDatumJsonVal dat
        , "inlineDatumRaw" .= inlineDatumRawJsonCbor dat
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
      TxOutSupplementalDatum _ datum ->
        "datumhash" .= toJSON (hashScriptDataBytes datum)
      TxOutDatumInline _ datum ->
        "inlineDatumhash" .= toJSON (hashScriptDataBytes datum)

  datJsonVal :: TxOutDatum ctx era -> Aeson.Value
  datJsonVal d =
    case d of
      TxOutDatumNone -> Aeson.Null
      TxOutDatumHash _ _ -> Aeson.Null
      TxOutSupplementalDatum _ datum -> scriptDataToJson ScriptDataJsonDetailedSchema datum
      TxOutDatumInline _ _ -> Aeson.Null

  inlineDatumJsonVal :: TxOutDatum ctx era -> Aeson.Value
  inlineDatumJsonVal d =
    case d of
      TxOutDatumNone -> Aeson.Null
      TxOutDatumHash{} -> Aeson.Null
      TxOutSupplementalDatum{} -> Aeson.Null
      TxOutDatumInline _ datum -> scriptDataToJson ScriptDataJsonDetailedSchema datum

  inlineDatumRawJsonCbor :: TxOutDatum ctx era -> Aeson.Value
  inlineDatumRawJsonCbor d =
    case d of
      TxOutDatumNone -> Aeson.Null
      TxOutDatumHash{} -> Aeson.Null
      TxOutSupplementalDatum{} -> Aeson.Null
      TxOutDatumInline _ datum ->
        Aeson.String
          . Text.decodeUtf8
          . Base16.encode
          . serialiseToCBOR
          $ datum

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

        reconcileConway ConwayEraOnwardsConway alonzoTxOutInConway mInlineDatum mReferenceScript
      ShelleyBasedEraDijkstra -> do
        alonzoTxOutInConway <- alonzoTxOutParser AlonzoEraOnwardsDijkstra o

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
                    else return $ TxOutDatumInline BabbageEraOnwardsDijkstra sData
            (Nothing, Nothing) -> return TxOutDatumNone
            (_, _) ->
              fail
                "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

        mReferenceScript <- o .:? "referenceScript"

        reconcileConway ConwayEraOnwardsDijkstra alonzoTxOutInConway mInlineDatum mReferenceScript
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
      :: ConwayEraOnwards era
      -> TxOut CtxTx era
      -- \^ Alonzo era datum in Conway era
      -> TxOutDatum CtxTx era
      -- \^ Babbage inline datum
      -> Maybe ScriptInAnyLang
      -> Aeson.Parser (TxOut CtxTx era)
    reconcileConway w top@(TxOut addr v dat r) babbageDatum mBabRefScript = do
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
          return $ ReferenceScript (convert w) anyScript
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
        (Just dVal, Just{}) -> do
          case scriptDataJsonToHashable ScriptDataJsonDetailedSchema dVal of
            Left e -> fail $ "Error parsing ScriptData: " <> show e
            Right hashableData ->
              TxOut
                <$> o .: "address"
                <*> o .: "value"
                <*> return (TxOutSupplementalDatum w hashableData)
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

        reconcileConway ConwayEraOnwardsConway alonzoTxOutInConway mInlineDatum mReferenceScript
      ShelleyBasedEraDijkstra -> do
        alonzoTxOutInConway <- alonzoTxOutParser AlonzoEraOnwardsDijkstra o

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
                    else return $ TxOutDatumInline BabbageEraOnwardsDijkstra sData
            (Nothing, Nothing) -> return TxOutDatumNone
            (_, _) ->
              fail
                "Should not be possible to create a tx output with either an inline datum hash or an inline datum"

        -- We check for a reference script
        mReferenceScript <- o .:? "referenceScript"

        reconcileConway ConwayEraOnwardsDijkstra alonzoTxOutInConway mInlineDatum mReferenceScript
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
      :: ConwayEraOnwards era
      -> TxOut CtxUTxO era
      -- \^ Alonzo era datum in Conway era
      -> TxOutDatum CtxUTxO era
      -- \^ Babbage inline datum
      -> Maybe ScriptInAnyLang
      -> Aeson.Parser (TxOut CtxUTxO era)
    reconcileConway w (TxOut addr v dat r) babbageDatum mBabRefScript = do
      -- We check for conflicting datums
      finalDat <- case (dat, babbageDatum) of
        (TxOutDatumNone, bDatum) -> return bDatum
        (anyDat, TxOutDatumNone) -> return anyDat
        (_, _) -> fail "Parsed an Alonzo era datum and a Conway era datum"
      finalRefScript <- case mBabRefScript of
        Nothing -> return r
        Just anyScript ->
          return $ ReferenceScript (convert w) anyScript

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

toByronTxOut :: TxOut CtxTx ByronEra -> Either TxOutputError Byron.TxOut
toByronTxOut = \case
  txout@(TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress addr)) (TxOutValueByron value) _ _) ->
    Byron.TxOut addr <$> (toByronLovelace value ?! classifyRangeError txout value)
  TxOut (AddressInEra ByronAddressInAnyEra (ByronAddress _)) (TxOutValueShelleyBased w _) _ _ ->
    case w of {}
  TxOut (AddressInEra (ShelleyAddressInEra sbe) ShelleyAddress{}) _ _ _ ->
    case sbe of {}
 where
  classifyRangeError :: TxOut CtxTx ByronEra -> L.Coin -> TxOutputError
  classifyRangeError txout value
    | value < 0 = TxOutputNegative (lovelaceToQuantity value) (txOutInAnyEra ByronEra txout)
    | otherwise = TxOutputOverflow (lovelaceToQuantity value) (txOutInAnyEra ByronEra txout)

toShelleyTxOut
  :: forall era ledgerera
   . HasCallStack
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> TxOut CtxUTxO era
  -> Ledger.TxOut ledgerera
toShelleyTxOut sbe = shelleyBasedEraConstraints sbe $ \case
  TxOut addr (TxOutValueShelleyBased _ value) txoutdata refScript ->
    caseShelleyToMaryOrAlonzoEraOnwards
      (const $ L.mkBasicTxOut (toShelleyAddr addr) value)
      ( \case
          AlonzoEraOnwardsAlonzo ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.dataHashTxOutL
                .~ toAlonzoTxOutDatumHashUTxO txoutdata
          AlonzoEraOnwardsBabbage ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL
                .~ toBabbageTxOutDatum txoutdata
              & L.referenceScriptTxOutL
                .~ refScriptToShelleyScript sbe refScript
          AlonzoEraOnwardsConway ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL
                .~ toBabbageTxOutDatumUTxO txoutdata
              & L.referenceScriptTxOutL
                .~ refScriptToShelleyScript sbe refScript
          AlonzoEraOnwardsDijkstra ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL
                .~ toBabbageTxOutDatumUTxO txoutdata
              & L.referenceScriptTxOutL
                .~ refScriptToShelleyScript sbe refScript
      )
      sbe

-- | A variant of 'toShelleyTxOutAny that is used only internally to this module
-- that works with a 'TxOut' in any context (including CtxTx) by ignoring
-- embedded datums (taking only their hash).
toShelleyTxOutAny
  :: forall ctx era ledgerera
   . HasCallStack
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> TxOut ctx era
  -> Ledger.TxOut ledgerera
toShelleyTxOutAny sbe = shelleyBasedEraConstraints sbe $ \case
  TxOut addr (TxOutValueShelleyBased _ value) txoutdata refScript ->
    caseShelleyToMaryOrAlonzoEraOnwards
      (const $ L.mkBasicTxOut (toShelleyAddr addr) value)
      ( \case
          AlonzoEraOnwardsAlonzo ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.dataHashTxOutL
                .~ toAlonzoTxOutDatumHash txoutdata
          AlonzoEraOnwardsBabbage ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL
                .~ toBabbageTxOutDatum txoutdata
              & L.referenceScriptTxOutL
                .~ refScriptToShelleyScript sbe refScript
          AlonzoEraOnwardsConway ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL
                .~ toBabbageTxOutDatum txoutdata
              & L.referenceScriptTxOutL
                .~ refScriptToShelleyScript sbe refScript
          AlonzoEraOnwardsDijkstra ->
            L.mkBasicTxOut (toShelleyAddr addr) value
              & L.datumTxOutL
                .~ toBabbageTxOutDatum txoutdata
              & L.referenceScriptTxOutL
                .~ refScriptToShelleyScript sbe refScript
      )
      sbe

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
      TxOut
        addressInEra
        txOutValue
        (fromAlonzoTxOutDatumHash AlonzoEraOnwardsAlonzo mDatumHash)
        ReferenceScriptNone
     where
      mDatumHash = ledgerTxOut ^. L.dataHashTxOutL
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
    ShelleyBasedEraDijkstra ->
      TxOut
        addressInEra
        txOutValue
        ( fromBabbageTxOutDatum
            AlonzoEraOnwardsDijkstra
            BabbageEraOnwardsDijkstra
            datum
        )
        ( case mRefScript of
            SNothing -> ReferenceScriptNone
            SJust refScript ->
              fromShelleyScriptToReferenceScript ShelleyBasedEraDijkstra refScript
        )
     where
      datum = ledgerTxOut ^. L.datumTxOutL
      mRefScript = ledgerTxOut ^. L.referenceScriptTxOutL

-- ----------------------------------------------------------------------------
-- Transaction output values (era-dependent)
--

data TxOutValue era where
  TxOutValueByron
    :: L.Coin
    -> TxOutValue ByronEra
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
          let sbe = convert shelleyToAlleg
          pure $
            shelleyBasedEraConstraints sbe $
              TxOutValueShelleyBased sbe $
                A.mkAdaValue sbe ll
      )
      ( \w -> do
          let l = toList o
              sbe = convert w
          vals <- mapM decodeAssetId l
          pure $
            shelleyBasedEraConstraints sbe $
              TxOutValueShelleyBased sbe $
                toLedgerValue w $
                  mconcat vals
      )
      (shelleyBasedEra @era)
   where
    decodeAssetId :: (Aeson.Key, Aeson.Value) -> Aeson.Parser Value
    decodeAssetId (polid, Aeson.Object assetNameHm) = do
      polId <- P.runParserFail parsePolicyId $ Aeson.toText polid
      aNameQuantity <- decodeAssets assetNameHm
      pure . fromList $
        map (first $ AssetId polId) aNameQuantity
    decodeAssetId ("lovelace", Aeson.Number sci) =
      case toBoundedInteger sci of
        Just (ll :: Word64) ->
          pure $ fromList [(AdaAssetId, Quantity $ toInteger ll)]
        Nothing ->
          fail $ "Expected a Bounded number but got: " <> show sci
    decodeAssetId wrong = fail $ "Expected a policy id and a JSON object but got: " <> show wrong

    decodeAssets :: Aeson.Object -> Aeson.Parser [(AssetName, Quantity)]
    decodeAssets assetNameHm =
      let l = toList assetNameHm
       in mapM (\(aName, q) -> (,) <$> parseKeyAsAssetName aName <*> decodeQuantity q) l

    parseKeyAsAssetName :: Aeson.Key -> Aeson.Parser AssetName
    parseKeyAsAssetName aName = P.runParserFail parseAssetName (Aeson.toText aName)

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
  -- only be used in the context of the transaction body (i.e this is a supplemental datum),
  -- and does not occur in the UTxO. The UTxO only contains the datum hash.
  TxOutSupplementalDatum
    :: AlonzoEraOnwards era
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

toAlonzoTxOutDatumHash
  :: TxOutDatum ctx era -> StrictMaybe Plutus.DataHash
toAlonzoTxOutDatumHash TxOutDatumNone = SNothing
toAlonzoTxOutDatumHash (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDatumHash (TxOutDatumInline{}) = SNothing
toAlonzoTxOutDatumHash (TxOutSupplementalDatum _ d) =
  let ScriptDataHash dh = hashScriptDataBytes d
   in SJust dh

fromAlonzoTxOutDatumHash
  :: AlonzoEraOnwards era
  -> StrictMaybe Plutus.DataHash
  -> TxOutDatum ctx era
fromAlonzoTxOutDatumHash _ SNothing = TxOutDatumNone
fromAlonzoTxOutDatumHash w (SJust hash) = TxOutDatumHash w $ ScriptDataHash hash

toBabbageTxOutDatum
  :: L.Era (ShelleyLedgerEra era)
  => TxOutDatum ctx era
  -> Plutus.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatum TxOutDatumNone = Plutus.NoDatum
toBabbageTxOutDatum (TxOutDatumHash _ (ScriptDataHash dh)) = Plutus.DatumHash dh
toBabbageTxOutDatum (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd
toBabbageTxOutDatum (TxOutSupplementalDatum _ d) =
  let ScriptDataHash dh = hashScriptDataBytes d
   in Plutus.DatumHash dh

fromBabbageTxOutDatum
  :: L.Era ledgerera
  => AlonzoEraOnwards era
  -> BabbageEraOnwards era
  -> Plutus.Datum ledgerera
  -> TxOutDatum ctx era
fromBabbageTxOutDatum _ _ Plutus.NoDatum = TxOutDatumNone
fromBabbageTxOutDatum w _ (Plutus.DatumHash dh) =
  TxOutDatumHash w $ ScriptDataHash dh
fromBabbageTxOutDatum _ w (Plutus.Datum binData) =
  TxOutDatumInline w $ binaryDataToScriptData w binData

toAlonzoTxOutDatumHashUTxO
  :: TxOutDatum CtxUTxO era -> StrictMaybe Plutus.DataHash
toAlonzoTxOutDatumHashUTxO TxOutDatumNone = SNothing
toAlonzoTxOutDatumHashUTxO (TxOutDatumHash _ (ScriptDataHash dh)) = SJust dh
toAlonzoTxOutDatumHashUTxO (TxOutDatumInline{}) = SNothing

toBabbageTxOutDatumUTxO
  :: L.Era (ShelleyLedgerEra era)
  => TxOutDatum CtxUTxO era
  -> Plutus.Datum (ShelleyLedgerEra era)
toBabbageTxOutDatumUTxO TxOutDatumNone = Plutus.NoDatum
toBabbageTxOutDatumUTxO (TxOutDatumHash _ (ScriptDataHash dh)) = Plutus.DatumHash dh
toBabbageTxOutDatumUTxO (TxOutDatumInline _ sd) = scriptDataToInlineDatum sd

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
binaryDataToScriptData BabbageEraOnwardsDijkstra d =
  fromAlonzoData $ L.binaryDataToData d

data TxOutputError
  = TxOutputNegative !Quantity !TxOutInAnyEra
  | TxOutputOverflow !Quantity !TxOutInAnyEra
  deriving (Eq, Show)

instance Error TxOutputError where
  prettyError = \case
    TxOutputNegative (Quantity q) txout ->
      "Negative quantity ("
        <> pretty q
        <> ") in transaction output: "
        <> pretty txout
    TxOutputOverflow (Quantity q) txout ->
      "Quantity too large ("
        <> pretty q
        <> " >= 2^64) in transaction output: "
        <> pretty txout
