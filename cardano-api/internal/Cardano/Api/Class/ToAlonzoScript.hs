{-# LANGUAGE MultiParamTypeClasses #-}
module Cardano.Api.Class.ToAlonzoScript where

import Cardano.Api.Eras (BabbageEra, ConwayEra)
import Cardano.Api.Eon.ShelleyBasedEra (ShelleyLedgerEra)
import Cardano.Api.Script as Script (PlutusScriptV1, PlutusScriptV2, PlutusScriptV3, PlutusScript(..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Conway.Scripts (PlutusScript (..))
import Cardano.Ledger.Plutus.Language (Plutus (..), PlutusBinary (..))

class ToAlonzoScript lang era where
  toLedgerScript ::
    Script.PlutusScript lang ->
    AlonzoScript (ShelleyLedgerEra era)

instance ToAlonzoScript PlutusScriptV1 BabbageEra where
  toLedgerScript (PlutusScriptSerialised bytes) =
    PlutusScript $ BabbagePlutusV1 $ Plutus $ PlutusBinary bytes

instance ToAlonzoScript PlutusScriptV2 BabbageEra where
  toLedgerScript (PlutusScriptSerialised bytes) =
    PlutusScript $ BabbagePlutusV2 $ Plutus $ PlutusBinary bytes

instance ToAlonzoScript PlutusScriptV1 ConwayEra where
  toLedgerScript (PlutusScriptSerialised bytes) =
    PlutusScript $ ConwayPlutusV1 $ Plutus $ PlutusBinary bytes

instance ToAlonzoScript PlutusScriptV2 ConwayEra where
  toLedgerScript (PlutusScriptSerialised bytes) =
    PlutusScript $ ConwayPlutusV2 $ Plutus $ PlutusBinary bytes

instance ToAlonzoScript PlutusScriptV3 ConwayEra where
  toLedgerScript (PlutusScriptSerialised bytes) =
    PlutusScript $ ConwayPlutusV3 $ Plutus $ PlutusBinary bytes
