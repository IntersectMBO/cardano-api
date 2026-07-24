{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.TxOutput
  ( policyAssetsToUtxoRpcMultiassets
  , txOutToUtxoRpcTxOutput
  , utxoRpcTxOutputToTxOut
  , txoRefUtxoRpcToTxIn
  , utxoToUtxoRpcAnyUtxoData
  , txInTxOutToAnyUtxoData
  , anyUtxoDataUtxoRpcToUtxo
  )
where

import Cardano.Api.Address
import Cardano.Api.Era
import Cardano.Api.Error
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
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.BigInt
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.PlutusData
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Script

import Cardano.Binary qualified as CBOR

import RIO hiding (toList)

import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.IsList
import Network.GRPC.Spec

utxoToUtxoRpcAnyUtxoData :: forall era. IsEra era => UTxO era -> [Proto UtxoRpc.AnyUtxoData]
utxoToUtxoRpcAnyUtxoData = map (uncurry txInTxOutToAnyUtxoData) . toList

txInTxOutToAnyUtxoData
  :: forall era. IsEra era => TxIn -> TxOut CtxUTxO era -> Proto UtxoRpc.AnyUtxoData
txInTxOutToAnyUtxoData txIn txOut = do
  let era = useEra @era
      txOutCbor =
        obtainCommonConstraints era $
          CBOR.serialize' $
            toShelleyTxOut (convert era) txOut
  defMessage
    & U5c.nativeBytes .~ txOutCbor
    & U5c.txoRef .~ inject txIn
    & U5c.cardano .~ txOutToUtxoRpcTxOutput (convert era) txOut

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
  :: ShelleyBasedEra era
  -> TxOut CtxUTxO era
  -> Proto UtxoRpc.TxOutput
txOutToUtxoRpcTxOutput sbe (TxOut addressInEra txOutValue datum script) = do
  let multiAsset = policyAssetsToUtxoRpcMultiassets . valueToPolicyAssets $ txOutValueToValue txOutValue
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
              & U5c.hash .~ serialiseToCBOR hashableScriptData
              & U5c.payload .~ scriptDataToUtxoRpcPlutusData (getScriptData hashableScriptData)
              & U5c.originalCbor .~ getOriginalScriptDataBytes hashableScriptData

  defMessage
    & U5c.address .~ T.encodeUtf8 (shelleyBasedEraConstraints sbe $ serialiseAddress addressInEra)
    & U5c.coin .~ inject (L.unCoin (txOutValueToLovelace txOutValue))
    & U5c.assets .~ multiAsset
    & U5c.maybe'datum .~ datumRpc
    & U5c.script .~ referenceScriptToUtxoRpcScript script

utxoRpcTxOutputToTxOut
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => IsEra era
  => Proto UtxoRpc.TxOutput
  -> m (TxOut CtxUTxO era)
utxoRpcTxOutputToTxOut txOutput = do
  let era = useEra @era
  addrUtf8 <- liftEitherError $ T.decodeUtf8' (txOutput ^. U5c.address)
  address <-
    maybe (throwM . stringException $ "Cannot decode address: " <> T.unpack addrUtf8) pure $
      obtainCommonConstraints era $
        deserialiseAddress asType addrUtf8
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
