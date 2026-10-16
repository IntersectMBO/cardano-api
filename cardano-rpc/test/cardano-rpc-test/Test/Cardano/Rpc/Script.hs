module Test.Cardano.Rpc.Script where

import Cardano.Api.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Script
  ( ledgerNativeScriptToUtxoRpcNativeScript
  , ledgerScriptToUtxoRpcScript
  , scriptToUtxoRpcScript
  , simpleScriptToUtxoRpcNativeScript
  )

import Cardano.Ledger.Dijkstra.Scripts qualified as L
import Cardano.Ledger.Hashes qualified as L (ScriptHash (..))
import Cardano.Ledger.Shelley.Scripts qualified as L

import RIO

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)

import Test.Gen.Cardano.Api.Typed (genScriptInEra)

import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen

-- | The ledger-direct script encoder must agree with the existing conversion that
-- goes through the old API's 'ScriptInEra' and 'toShelleyScript'.
--
-- Shelley is excluded: its ledger 'MultiSig' representation has no time-lock
-- constructors, so 'toShelleyScript' crashes (via 'toShelleyMultiSig') on the
-- 'RequireTimeBefore'\/'RequireTimeAfter' nodes the generator can produce. Allegra
-- onwards is enough to pin the time-pattern orientation (@Expire@ ->
-- @invalidHereafter@, @Start@ -> @invalidBefore@), which is the one real inversion
-- risk in this conversion.
hprop_ledger_script_to_utxo_rpc_script_matches_old_api :: Property
hprop_ledger_script_to_utxo_rpc_script_matches_old_api = H.property $ do
  AnyShelleyBasedEra sbe <-
    forAll $
      Gen.element
        [ AnyShelleyBasedEra ShelleyBasedEraAllegra
        , AnyShelleyBasedEra ShelleyBasedEraMary
        , AnyShelleyBasedEra ShelleyBasedEraAlonzo
        , AnyShelleyBasedEra ShelleyBasedEraBabbage
        , AnyShelleyBasedEra ShelleyBasedEraConway
        ]
  scriptInEra@(ScriptInEra _ oldScript) <- forAll $ genScriptInEra sbe
  let ledgerScript = toShelleyScript scriptInEra

  ledgerScriptToUtxoRpcScript sbe ledgerScript === scriptToUtxoRpcScript oldScript

-- | A Dijkstra guard script has no utxorpc counterpart yet, so it must convert to
-- the empty 'NativeScript' message rather than crashing.
hprop_dijkstra_guard_script_converts_to_empty_native_script :: Property
hprop_dijkstra_guard_script_converts_to_empty_native_script = H.propertyOnce $ do
  rawHash <- H.nothingFail $ L.hashFromBytes (BS.replicate 28 0)
  let guardScript = L.RequireGuard (L.KeyHashObj (L.KeyHash rawHash))

  ledgerNativeScriptToUtxoRpcNativeScript ShelleyBasedEraDijkstra guardScript === defMessage

hprop_simple_script_guards_preserve_rpc_list_positions :: Property
hprop_simple_script_guards_preserve_rpc_list_positions = H.propertyOnce $ do
  rawKeyHash <- H.nothingFail $ L.hashFromBytes (BS.replicate 28 0)
  rawScriptHash <- H.nothingFail $ L.hashFromBytes (BS.replicate 28 1)
  let keyGuard = RequireGuard (L.KeyHashObj (L.KeyHash rawKeyHash))
      scriptGuard = RequireGuard (L.ScriptHashObj (L.ScriptHash rawScriptHash))
      scripts = [keyGuard, RequireTimeAfter (L.SlotNo 42), scriptGuard]
      expected = [defMessage, defMessage & U5c.invalidBefore .~ 42, defMessage]
      encodeScript = simpleScriptToUtxoRpcNativeScript
  encodeScript keyGuard === defMessage
  encodeScript scriptGuard === defMessage
  encodeScript (RequireAllOf scripts) ^. U5c.scriptAll . U5c.items === expected
  encodeScript (RequireAnyOf scripts) ^. U5c.scriptAny . U5c.items === expected
  encodeScript (RequireMOf 2 scripts) ^. U5c.scriptNOfK . U5c.scripts === expected
  encodeScript (RequireMOf 2 scripts) ^. U5c.scriptNOfK . U5c.k === 2

-- | A Dijkstra native script that isn't a guard script still converts via the
-- classic view patterns shared with earlier eras.
hprop_dijkstra_signature_script_sets_pubkey_hash :: Property
hprop_dijkstra_signature_script_sets_pubkey_hash = H.propertyOnce $ do
  rawHash <- H.nothingFail $ L.hashFromBytes (BS.replicate 28 0)
  let signatureScript = L.RequireSignature (L.KeyHash rawHash)
      result = ledgerNativeScriptToUtxoRpcNativeScript ShelleyBasedEraDijkstra signatureScript

  result ^. U5c.scriptPubkeyHash === L.hashToBytes rawHash
