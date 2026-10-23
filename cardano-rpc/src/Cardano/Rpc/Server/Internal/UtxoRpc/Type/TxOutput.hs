{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.TxOutput
  ( policyAssetsToUtxoRpcMultiassets
  , txOutToUtxoRpcTxOutput
  , utxoRpcTxOutputToTxOut
  , txoRefUtxoRpcToTxIn
  , UtxoFields (..)
  , utxoToUtxoRpcAnyUtxoData
  , txInTxOutToAnyUtxoData
  , utxoToUtxoFields
  , txInTxOutToUtxoFields
  , mkAnyUtxoData
  , anyUtxoDataUtxoRpcToUtxo
  )
where

import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.FieldMask (FieldMaskTree (..), maskedSet)
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.BigInt
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.PlutusData
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Script

import Cardano.Ledger.Api qualified as L

import RIO hiding (toList)

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.ProtoLens (defMessage)
import GHC.IsList
import Network.GRPC.Spec

-- | Per-UTxO inputs of 'mkAnyUtxoData'. The fields are lazy: a field the mask
-- does not select is never computed.
data UtxoFields = UtxoFields
  { utxoFieldsRef :: UtxoRpc.TxoRef
  , utxoFieldsNativeBytes :: ByteString
  , utxoFieldsCardano :: UtxoRpc.TxOutput
  }

utxoToUtxoRpcAnyUtxoData :: forall era. IsEra era => UTxO era -> [Proto UtxoRpc.AnyUtxoData]
utxoToUtxoRpcAnyUtxoData = map (uncurry txInTxOutToAnyUtxoData) . toList

txInTxOutToAnyUtxoData
  :: forall era. IsEra era => TxIn -> TxOut CtxUTxO era -> Proto UtxoRpc.AnyUtxoData
txInTxOutToAnyUtxoData txIn = Proto . mkAnyUtxoData SelectAll . txInTxOutToUtxoFields txIn

utxoToUtxoFields :: forall era. IsEra era => UTxO era -> [UtxoFields]
utxoToUtxoFields = map (uncurry txInTxOutToUtxoFields) . toList

-- | The lazy per-field inputs of the @AnyUtxoData@ message of a UTxO entry.
txInTxOutToUtxoFields
  :: forall era. IsEra era => TxIn -> TxOut CtxUTxO era -> UtxoFields
txInTxOutToUtxoFields txIn txOut = do
  let era = useEra @era
  UtxoFields
    { utxoFieldsRef = getProto $ inject txIn
    , utxoFieldsNativeBytes =
        obtainCommonConstraints era $
          L.serialize' (Exp.eraProtVerHigh era) $
            toShelleyTxOut (convert era) txOut
    , utxoFieldsCardano = getProto $ txOutToUtxoRpcTxOutput (convert era) txOut
    }

-- | Build the message from the fields the mask selects. An unselected field is never evaluated.
mkAnyUtxoData :: FieldMaskTree UtxoRpc.AnyUtxoData -> UtxoFields -> UtxoRpc.AnyUtxoData
mkAnyUtxoData fieldMask fields =
  defMessage
    & maskedSet @"nativeBytes" fieldMask (utxoFieldsNativeBytes fields)
    & maskedSet @"txoRef" fieldMask (utxoFieldsRef fields)
    & maskedSet @"cardano" fieldMask (utxoFieldsCardano fields)

anyUtxoDataUtxoRpcToUtxo
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => Era era
  -> [Proto UtxoRpc.AnyUtxoData]
  -> m (UTxO era)
anyUtxoDataUtxoRpcToUtxo era = fmap fromList . foldM f mempty
 where
  f
    :: [(TxIn, TxOut CtxUTxO era)]
    -> Proto UtxoRpc.AnyUtxoData
    -> m [(TxIn, TxOut CtxUTxO era)]
  f acc e = do
    txOut <- obtainCommonConstraints era $ utxoRpcTxOutputToTxOut $ e ^. U5c.cardano
    txIn <- txoRefUtxoRpcToTxIn $ e ^. U5c.txoRef
    pure $ (txIn, txOut) : acc

txoRefUtxoRpcToTxIn
  :: forall m
   . HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.TxoRef
  -> m TxIn
txoRefUtxoRpcToTxIn txoRef = do
  txId' <-
    liftEitherError $
      deserialiseFromRawBytes asType $
        txoRef ^. U5c.hash
  pure $ TxIn txId' (TxIx . fromIntegral $ txoRef ^. U5c.index)

-- | Convert per-policy asset bundles to UTxO RPC 'UtxoRpc.Multiasset' messages.
policyAssetsToUtxoRpcMultiassets :: Map PolicyId PolicyAssets -> [Proto UtxoRpc.Multiasset]
policyAssetsToUtxoRpcMultiassets policyAssetsMap =
  toList policyAssetsMap <&> \(pId, policyAssets) -> do
    let assets =
          toList policyAssets <&> \(assetName, Quantity qty) -> do
            defMessage
              & U5c.name .~ serialiseToRawBytes assetName
              -- we don't have access to info if the coin was minted in the transaction,
              -- maybe we should add it later
              -- & U5c.maybe'mintCoin .~ Nothing
              & U5c.quantity .~ inject qty
    defMessage
      & U5c.policyId .~ serialiseToRawBytes pId
      & U5c.assets .~ assets

txOutToUtxoRpcTxOutput
  :: forall era
   . ShelleyBasedEra era
  -> TxOut CtxUTxO era
  -> Proto UtxoRpc.TxOutput
txOutToUtxoRpcTxOutput sbe txOut@(TxOut addressInEra txOutValue datum script) = do
  let multiAsset = policyAssetsToUtxoRpcMultiassets . valueToPolicyAssets $ txOutValueToValue txOutValue
      -- CAVEAT: this is a canonical re-encoding, not guaranteed to be the
      -- original on-chain bytes. The ledger does not memoise TxOut (unlike
      -- TxBody, Data and scripts), and its decoders accept non-canonical
      -- encodings, so decode-then-encode may differ from the submitter's
      -- bytes for historical outputs.
      originalCbor =
        shelleyBasedEraConstraints sbe $
          L.serialize' (L.eraProtVerHigh @(ShelleyLedgerEra era)) $
            toShelleyTxOut sbe txOut
      datumRpc = case datum of
        TxOutDatumNone ->
          Nothing
        TxOutDatumHash _ scriptDataHash ->
          Just $
            defMessage
              & U5c.hash .~ serialiseToRawBytes scriptDataHash
              & U5c.maybe'payload .~ Nothing -- we don't have it
              & U5c.maybe'originalCbor .~ Nothing
        TxOutDatumInline _ hashableScriptData ->
          Just $
            defMessage
              & U5c.hash .~ serialiseToRawBytes (hashScriptDataBytes hashableScriptData)
              & U5c.payload .~ scriptDataToUtxoRpcPlutusData (getScriptData hashableScriptData)
              & U5c.originalCbor .~ getOriginalScriptDataBytes hashableScriptData

  defMessage
    & U5c.address .~ shelleyBasedEraConstraints sbe (serialiseToRawBytes addressInEra)
    & U5c.coin .~ inject (L.unCoin (txOutValueToLovelace txOutValue))
    & U5c.assets .~ multiAsset
    & U5c.maybe'datum .~ datumRpc
    & U5c.script .~ referenceScriptToUtxoRpcScript script
    & U5c.originalCbor .~ originalCbor

utxoRpcTxOutputToTxOut
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => IsEra era
  => Proto UtxoRpc.TxOutput
  -> m (TxOut CtxUTxO era)
utxoRpcTxOutputToTxOut txOutput = do
  let era = useEra @era
  let addressBytes = txOutput ^. U5c.address
      annotateError (SerialiseAsRawBytesError msg) =
        SerialiseAsRawBytesError $
          msg <> ", address (hex): " <> BSC.unpack (Base16.encode addressBytes)
  address <-
    obtainCommonConstraints era $
      liftEitherError $
        first annotateError $
          deserialiseFromRawBytes asType addressBytes
  datum <-
    case txOutput ^. U5c.maybe'datum of
      Just datumRpc ->
        case datumRpc ^. U5c.maybe'originalCbor of
          Just cbor ->
            liftEitherError $
              TxOutDatumInline (convert era)
                <$> deserialiseFromCBOR asType cbor
          Nothing ->
            liftEitherError $
              TxOutDatumHash (convert era)
                <$> deserialiseFromRawBytes asType (datumRpc ^. U5c.hash)
      Nothing -> pure TxOutDatumNone
  referenceScript <- utxoRpcScriptToReferenceScript (txOutput ^. U5c.script)
  coinValue <- lovelaceToValue . L.Coin <$> txOutput ^. U5c.coin . to utxoRpcBigIntToInteger
  multiAssetValue <- fmap (fromList @Value . join) . forM (txOutput ^. U5c.assets) $ \policyAssets -> do
    pId <-
      liftEitherError $ deserialiseFromRawBytes AsPolicyId (policyAssets ^. U5c.policyId)
    forM (policyAssets ^. U5c.assets) $ \asset -> do
      assetName <-
        liftEitherError $
          deserialiseFromRawBytes AsAssetName (asset ^. U5c.name)
      coin <- Quantity <$> asset ^. U5c.quantity . to utxoRpcBigIntToInteger
      pure (AssetId pId assetName, coin)
  pure $
    TxOut
      address
      ( obtainCommonConstraints era $
          TxOutValueShelleyBased (convert era) (toMaryValue $ coinValue <> multiAssetValue)
      )
      datum
      referenceScript
