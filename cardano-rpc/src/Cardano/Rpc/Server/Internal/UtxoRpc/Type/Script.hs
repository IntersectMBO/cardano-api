{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Script
  ( simpleScriptToUtxoRpcNativeScript
  , utxoRpcNativeScriptToSimpleScript
  , referenceScriptToUtxoRpcScript
  , utxoRpcScriptToReferenceScript
  )
where

import Cardano.Api.Block
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Plutus
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Raw
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import RIO

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

simpleScriptToUtxoRpcNativeScript :: SimpleScript -> Proto UtxoRpc.NativeScript
simpleScriptToUtxoRpcNativeScript = \case
  RequireSignature paymentKeyHash ->
    defMessage & U5c.scriptPubkeyHash .~ serialiseToRawBytes paymentKeyHash
  RequireTimeBefore (SlotNo slotNo) ->
    defMessage & U5c.invalidHereafter .~ slotNo
  RequireTimeAfter (SlotNo slotNo) ->
    defMessage & U5c.invalidBefore .~ slotNo
  RequireAllOf scripts ->
    defMessage & U5c.scriptAll . U5c.items .~ map simpleScriptToUtxoRpcNativeScript scripts
  RequireAnyOf scripts ->
    defMessage & U5c.scriptAny . U5c.items .~ map simpleScriptToUtxoRpcNativeScript scripts
  RequireMOf k scripts -> do
    let nScriptsOf =
          defMessage
            & U5c.k .~ fromIntegral k
            & U5c.scripts .~ map simpleScriptToUtxoRpcNativeScript scripts
    defMessage & U5c.scriptNOfK .~ nScriptsOf

utxoRpcNativeScriptToSimpleScript
  :: HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.NativeScript
  -> m SimpleScript
utxoRpcNativeScriptToSimpleScript scriptRpc
  | Just paymentKeyHash <- scriptRpc ^. U5c.maybe'scriptPubkeyHash =
      RequireSignature <$> liftEitherError (deserialiseFromRawBytes asType paymentKeyHash)
  | Just slotNo <- scriptRpc ^. U5c.maybe'invalidHereafter =
      pure . RequireTimeBefore $ SlotNo slotNo
  | Just slotNo <- scriptRpc ^. U5c.maybe'invalidBefore =
      pure . RequireTimeAfter $ SlotNo slotNo
  | Just scriptsRpc <- scriptRpc ^. U5c.maybe'scriptAll = do
      fmap RequireAllOf $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. U5c.items
  | Just scriptsRpc <- scriptRpc ^. U5c.maybe'scriptAny = do
      fmap RequireAnyOf $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. U5c.items
  | Just scriptsRpc <- scriptRpc ^. U5c.maybe'scriptNOfK = do
      fmap (RequireMOf . fromIntegral $ scriptsRpc ^. U5c.k) $
        mapM utxoRpcNativeScriptToSimpleScript $
          scriptsRpc ^. U5c.scripts
  | otherwise = throwM . stringException $ "Cannot decode UTxORPC NativeScript"

referenceScriptToUtxoRpcScript :: ReferenceScript era -> Proto UtxoRpc.Script
referenceScriptToUtxoRpcScript ReferenceScriptNone = defMessage
referenceScriptToUtxoRpcScript (ReferenceScript _ (ScriptInAnyLang _ script)) =
  case script of
    SimpleScript ss ->
      defMessage & U5c.native .~ simpleScriptToUtxoRpcNativeScript ss
    PlutusScript PlutusScriptV1 ps ->
      defMessage & U5c.plutusV1 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV2 ps ->
      defMessage & U5c.plutusV2 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV3 ps ->
      defMessage & U5c.plutusV3 .~ serialiseToRawBytes ps
    PlutusScript PlutusScriptV4 ps ->
      defMessage & U5c.plutusV4 .~ serialiseToRawBytes ps

utxoRpcScriptToReferenceScript
  :: forall era m
   . HasCallStack
  => MonadThrow m
  => IsEra era
  => Proto UtxoRpc.Script
  -> m (ReferenceScript era)
utxoRpcScriptToReferenceScript protoScript
  | Just script <- protoScript ^. U5c.maybe'native =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang SimpleScriptLanguage . SimpleScript
        <$> utxoRpcNativeScriptToSimpleScript script
  | Just script <- protoScript ^. U5c.maybe'plutusV1 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. U5c.maybe'plutusV2 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. U5c.maybe'plutusV3 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | Just script <- protoScript ^. U5c.maybe'plutusV4 =
      ReferenceScript (convert $ useEra @era) . ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV4)
        <$> liftEitherError (deserialiseFromCBOR asType script)
  | otherwise = pure ReferenceScriptNone
