{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Script
  ( simpleScriptToUtxoRpcNativeScript
  , utxoRpcNativeScriptToSimpleScript
  , referenceScriptToUtxoRpcScript
  , scriptToUtxoRpcScript
  , utxoRpcScriptToReferenceScript
  , ledgerScriptToUtxoRpcScript
  , ledgerNativeScriptToUtxoRpcNativeScript
  )
where

import Cardano.Api.Block
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Raw
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import Cardano.Ledger.Allegra.Scripts qualified as L
import Cardano.Ledger.Alonzo.Scripts qualified as L
  ( AlonzoScript (NativeScript, PlutusScript)
  , plutusScriptBinary
  )
import Cardano.Ledger.Api qualified as L (ShelleyEra)
import Cardano.Ledger.Core qualified as L (EraScript (NativeScript))
import Cardano.Ledger.Dijkstra.Scripts qualified as L
import Cardano.Ledger.Plutus.Language qualified as L (Language (..), PlutusBinary (..))
import Cardano.Ledger.Shelley.Scripts qualified as L

import RIO

import Data.ByteString.Short qualified as SBS
import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | TODO: remove together with the old-API TxOutput pipeline,
-- https://github.com/IntersectMBO/cardano-api/issues/1264
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
  scriptToUtxoRpcScript script

-- | Convert a script to the UTxO RPC 'UtxoRpc.Script' message, dispatching on
-- the script language to select the corresponding oneof field.
--
-- TODO: remove together with the old-API TxOutput pipeline,
-- https://github.com/IntersectMBO/cardano-api/issues/1264
scriptToUtxoRpcScript :: Script lang -> Proto UtxoRpc.Script
scriptToUtxoRpcScript = \case
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

-- | Convert a ledger script to the UTxO RPC 'UtxoRpc.Script' message,
-- dispatching native scripts to the 'UtxoRpc.NativeScript' oneof field and
-- plutus scripts to the field of their language version.
ledgerScriptToUtxoRpcScript
  :: ShelleyBasedEra era
  -> L.Script (ShelleyLedgerEra era)
  -> Proto UtxoRpc.Script
ledgerScriptToUtxoRpcScript sbe script = case sbe of
  ShelleyBasedEraShelley ->
    defMessage & U5c.native .~ ledgerNativeScriptToUtxoRpcNativeScript sbe script
  ShelleyBasedEraAllegra ->
    defMessage & U5c.native .~ ledgerNativeScriptToUtxoRpcNativeScript sbe script
  ShelleyBasedEraMary ->
    defMessage & U5c.native .~ ledgerNativeScriptToUtxoRpcNativeScript sbe script
  ShelleyBasedEraAlonzo -> alonzoScriptToUtxoRpcScript sbe script
  ShelleyBasedEraBabbage -> alonzoScriptToUtxoRpcScript sbe script
  ShelleyBasedEraConway -> alonzoScriptToUtxoRpcScript sbe script
  ShelleyBasedEraDijkstra -> alonzoScriptToUtxoRpcScript sbe script

alonzoScriptToUtxoRpcScript
  :: L.AlonzoEraScript (ShelleyLedgerEra era)
  => ShelleyBasedEra era
  -> L.AlonzoScript (ShelleyLedgerEra era)
  -> Proto UtxoRpc.Script
alonzoScriptToUtxoRpcScript sbe = \case
  L.NativeScript nativeScript ->
    defMessage & U5c.native .~ ledgerNativeScriptToUtxoRpcNativeScript sbe nativeScript
  L.PlutusScript plutusScript -> plutusScriptToUtxoRpcScript plutusScript

-- | Convert a ledger Plutus script to the UTxO RPC 'UtxoRpc.Script' message,
-- dispatching on the Plutus language version to select the corresponding oneof
-- field. This is a pure projection of the script bytes: no CBOR decoding involved.
plutusScriptToUtxoRpcScript :: L.AlonzoEraScript era => L.PlutusScript era -> Proto UtxoRpc.Script
plutusScriptToUtxoRpcScript plutusScript = do
  let bytes = SBS.fromShort . L.unPlutusBinary $ L.plutusScriptBinary plutusScript
  case L.plutusScriptLanguage plutusScript of
    L.PlutusV1 -> defMessage & U5c.plutusV1 .~ bytes
    L.PlutusV2 -> defMessage & U5c.plutusV2 .~ bytes
    L.PlutusV3 -> defMessage & U5c.plutusV3 .~ bytes
    L.PlutusV4 -> defMessage & U5c.plutusV4 .~ bytes

-- | Convert a ledger native script directly to the UTxO RPC 'UtxoRpc.NativeScript'
-- message.
--
-- Each group of eras has its own concrete native script type: 'MultiSig' at
-- Shelley, 'Timelock' from Allegra up to Conway (all sharing the same six view
-- patterns), and Dijkstra's own type, which additionally supports a guard script.
ledgerNativeScriptToUtxoRpcNativeScript
  :: ShelleyBasedEra era
  -> L.NativeScript (ShelleyLedgerEra era)
  -> Proto UtxoRpc.NativeScript
ledgerNativeScriptToUtxoRpcNativeScript sbe nativeScript = case sbe of
  ShelleyBasedEraShelley -> shelleyMultiSigToUtxoRpcNativeScript nativeScript
  ShelleyBasedEraAllegra -> allegraEraScriptToUtxoRpcNativeScript nativeScript
  ShelleyBasedEraMary -> allegraEraScriptToUtxoRpcNativeScript nativeScript
  ShelleyBasedEraAlonzo -> allegraEraScriptToUtxoRpcNativeScript nativeScript
  ShelleyBasedEraBabbage -> allegraEraScriptToUtxoRpcNativeScript nativeScript
  ShelleyBasedEraConway -> allegraEraScriptToUtxoRpcNativeScript nativeScript
  ShelleyBasedEraDijkstra -> dijkstraNativeScriptToUtxoRpcNativeScript nativeScript

-- | Shelley's 'MultiSig' has only these four constructors, and ledger declares a
-- 'COMPLETE' pragma for exactly this set at 'L.ShelleyEra'. GHC 9.12.4 does not
-- resolve that pragma here in practice (verified directly: the same match without
-- a wildcard triggers @-Wincomplete-patterns@, apparently because the pragma is
-- keyed to the type family application 'L.NativeScript' 'L.ShelleyEra' rather than
-- its reduction), so a defensive wildcard is required.
shelleyMultiSigToUtxoRpcNativeScript :: L.NativeScript L.ShelleyEra -> Proto UtxoRpc.NativeScript
shelleyMultiSigToUtxoRpcNativeScript = \case
  L.RequireSignature keyHash ->
    defMessage & U5c.scriptPubkeyHash .~ L.hashToBytes (L.unKeyHash keyHash)
  L.RequireAllOf scripts ->
    defMessage & U5c.scriptAll . U5c.items .~ map shelleyMultiSigToUtxoRpcNativeScript (toList scripts)
  L.RequireAnyOf scripts ->
    defMessage & U5c.scriptAny . U5c.items .~ map shelleyMultiSigToUtxoRpcNativeScript (toList scripts)
  L.RequireMOf k scripts -> do
    let nScriptsOf =
          defMessage
            & U5c.k .~ fromIntegral k
            & U5c.scripts .~ map shelleyMultiSigToUtxoRpcNativeScript (toList scripts)
    defMessage & U5c.scriptNOfK .~ nScriptsOf
  _ ->
    error
      "shelleyMultiSigToUtxoRpcNativeScript: impossible - every ShelleyEra MultiSig \
      \constructor is matched above"

-- | Shared body for the Allegra, Mary, Alonzo, Babbage and Conway native script
-- representations, all of which are 'Timelock' under the hood. The view patterns
-- have no 'COMPLETE' pragma at this polymorphic use (only at each concrete era), so
-- the wildcard below is unreachable in practice but required by the compiler.
allegraEraScriptToUtxoRpcNativeScript
  :: L.AllegraEraScript era'
  => L.NativeScript era'
  -> Proto UtxoRpc.NativeScript
allegraEraScriptToUtxoRpcNativeScript = \case
  L.RequireSignature keyHash ->
    defMessage & U5c.scriptPubkeyHash .~ L.hashToBytes (L.unKeyHash keyHash)
  L.RequireAllOf scripts ->
    defMessage & U5c.scriptAll . U5c.items .~ map allegraEraScriptToUtxoRpcNativeScript (toList scripts)
  L.RequireAnyOf scripts ->
    defMessage & U5c.scriptAny . U5c.items .~ map allegraEraScriptToUtxoRpcNativeScript (toList scripts)
  L.RequireMOf k scripts -> do
    let nScriptsOf =
          defMessage
            & U5c.k .~ fromIntegral k
            & U5c.scripts .~ map allegraEraScriptToUtxoRpcNativeScript (toList scripts)
    defMessage & U5c.scriptNOfK .~ nScriptsOf
  L.RequireTimeExpire (SlotNo slotNo) ->
    defMessage & U5c.invalidHereafter .~ slotNo
  L.RequireTimeStart (SlotNo slotNo) ->
    defMessage & U5c.invalidBefore .~ slotNo
  _ ->
    error
      "allegraEraScriptToUtxoRpcNativeScript: impossible - every AllegraEraScript \
      \NativeScript constructor is matched above"

-- | Dijkstra's native script type has its own seven constructors: the six classic
-- ones plus a guard script. Upstream ships no 'COMPLETE' pragma for this set
-- (tracked as an upstream nit), so a wildcard is required even though it is
-- unreachable.
dijkstraNativeScriptToUtxoRpcNativeScript
  :: L.DijkstraEraScript era'
  => L.NativeScript era'
  -> Proto UtxoRpc.NativeScript
dijkstraNativeScriptToUtxoRpcNativeScript = \case
  L.RequireSignature keyHash ->
    defMessage & U5c.scriptPubkeyHash .~ L.hashToBytes (L.unKeyHash keyHash)
  L.RequireAllOf scripts ->
    defMessage
      & U5c.scriptAll . U5c.items .~ map dijkstraNativeScriptToUtxoRpcNativeScript (toList scripts)
  L.RequireAnyOf scripts ->
    defMessage
      & U5c.scriptAny . U5c.items .~ map dijkstraNativeScriptToUtxoRpcNativeScript (toList scripts)
  L.RequireMOf k scripts -> do
    let nScriptsOf =
          defMessage
            & U5c.k .~ fromIntegral k
            & U5c.scripts .~ map dijkstraNativeScriptToUtxoRpcNativeScript (toList scripts)
    defMessage & U5c.scriptNOfK .~ nScriptsOf
  L.RequireTimeExpire (SlotNo slotNo) ->
    defMessage & U5c.invalidHereafter .~ slotNo
  L.RequireTimeStart (SlotNo slotNo) ->
    defMessage & U5c.invalidBefore .~ slotNo
  L.RequireGuard _credential ->
    -- utxorpc has no guard-script variant yet (tracked as
    -- https://github.com/IntersectMBO/cardano-api/issues/1263); emit the empty
    -- message rather than omitting the script, to keep script-list positions stable.
    defMessage
  _ ->
    error
      "dijkstraNativeScriptToUtxoRpcNativeScript: impossible - every DijkstraEraScript \
      \NativeScript constructor is matched above"
