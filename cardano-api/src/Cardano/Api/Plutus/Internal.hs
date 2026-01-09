{-# LANGUAGE GADTs #-}

-- | This module provides utilities to render the result of plutus execution.
module Cardano.Api.Plutus.Internal
  ( DebugPlutusFailure (..)
  , renderDebugPlutusFailure
  , collectPlutusScriptHashes
  )
where

import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
  ( AlonzoEraOnwards (..)
  , alonzoEraOnwardsConstraints
  )
import Cardano.Api.Era.Internal.Eon.Convert (convert)
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra (ShelleyLedgerEra)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script qualified as Api
import Cardano.Api.Pretty (Pretty (pretty), docToText)
import Cardano.Api.Query.Internal.Type.QueryInMode (toLedgerUTxO)
import Cardano.Api.Tx.Internal.Body (ScriptWitnessIndex (..), toScriptIndex)
import Cardano.Api.Tx.Internal.Sign (Tx (..))
import Cardano.Api.UTxO (UTxO)

import Cardano.Ledger.Alonzo.UTxO qualified as Alonzo
import Cardano.Ledger.Binary.Encoding (serialize')
import Cardano.Ledger.Binary.Plain (serializeAsHexText)
import Cardano.Ledger.Plutus.Evaluate qualified as Plutus
import Cardano.Ledger.Plutus.ExUnits qualified as Plutus
import Cardano.Ledger.Plutus.Language qualified as Plutus
import PlutusLedgerApi.V1 qualified as Plutus

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Short qualified as BSS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Lens.Micro ((^.))
import Prettyprinter (indent, line)

-- | A structured representation of Plutus script validation failures,
-- providing detailed information about the failed execution for debugging purposes.
-- This type contains the same information as the data constructor
-- 'Cardano.Ledger.Alonzo.Plutus.Evaluate.TransactionScriptFailure.ValidationFailure'
-- but with named fields and fixed crypto parameters for easier debugging and
-- error reporting.
data DebugPlutusFailure
  = DebugPlutusFailure
  { dpfEvaluationError :: Plutus.EvaluationError
  , dpfScriptWithContext :: Plutus.PlutusWithContext
  , dpfExecutionUnits :: Plutus.ExUnits
  , dpfExecutionLogs :: [Text]
  }
  deriving (Eq, Show)

renderDebugPlutusFailure :: DebugPlutusFailure -> Text
renderDebugPlutusFailure dpf =
  let pwc = dpfScriptWithContext dpf
      lang = case pwc of
        Plutus.PlutusWithContext{Plutus.pwcScript = script} ->
          either Plutus.plutusLanguage Plutus.plutusLanguage script

      scriptArgs = case pwc of
        Plutus.PlutusWithContext{Plutus.pwcArgs = args} ->
          line <> indent 3 (pretty args)
      protocolVersion = Plutus.pwcProtocolVersion pwc
      scriptArgsBase64 = case pwc of
        Plutus.PlutusWithContext{Plutus.pwcArgs = args} ->
          Text.decodeUtf8 $ B64.encode $ serialize' protocolVersion args
      evalError = dpfEvaluationError dpf
      binaryScript = case pwc of
        Plutus.PlutusWithContext{Plutus.pwcScript = scr} ->
          let Plutus.Plutus bytes = either id Plutus.plutusFromRunnable scr
           in Text.decodeUtf8 . B64.encode . BSS.fromShort $ Plutus.unPlutusBinary bytes
   in Text.unlines
        [ "Script hash: " <> serializeAsHexText (Plutus.pwcScriptHash pwc)
        , "Script language: " <> Text.pack (show lang)
        , "Protocol version: " <> Text.pack (show protocolVersion)
        , "Script arguments: " <> docToText scriptArgs
        , "Script evaluation error: " <> docToText (pretty evalError)
        , "Script execution logs: " <> Text.unlines (dpfExecutionLogs dpf)
        , "Script base64 encoded arguments: " <> scriptArgsBase64
        , "Script base64 encoded bytes: " <> binaryScript
        ]

{-
-- Should be used on `dpfExecutionLogs dpf`. Disabled until next plutus release.
See: https://github.com/IntersectMBO/cardano-api/pull/672#issuecomment-2455909946

PlutusTx.ErrorCodes.plutusPreludeErrorCodes

lookupPlutusErrorCode :: Text -> Text
lookupPlutusErrorCode code =
  let codeString = PlutusTx.stringToBuiltinString $ Text.unpack code
   in case Map.lookup codeString plutusPreludeErrorCodes of
        Just err -> Text.pack err
        Nothing -> "Unknown error code: " <> code
-}

-- | Collect all plutus script hashes that are needed to validate the given transaction
-- and return them in a map with their corresponding 'ScriptWitnessIndex' as key.
collectPlutusScriptHashes
  :: AlonzoEraOnwards era
  -> Tx era
  -> UTxO era
  -> Map ScriptWitnessIndex Api.ScriptHash
collectPlutusScriptHashes aeo tx utxo =
  alonzoEraOnwardsConstraints aeo $
    let ShelleyTx _ ledgerTx' = tx
        ledgerUTxO = toLedgerUTxO (convert aeo) utxo
     in getPurposes aeo $ L.getScriptsNeeded ledgerUTxO (ledgerTx' ^. L.bodyTxL)
 where
  getPurposes
    :: AlonzoEraOnwards era
    -> Alonzo.AlonzoScriptsNeeded (ShelleyLedgerEra era)
    -> Map ScriptWitnessIndex Api.ScriptHash
  getPurposes aeo' (Alonzo.AlonzoScriptsNeeded purposes) =
    alonzoEraOnwardsConstraints aeo $
      Map.fromList $
        Prelude.map
          (bimap (toScriptIndex aeo' . purposeAsIxItemToAsIx aeo') Api.fromShelleyScriptHash)
          purposes

  purposeAsIxItemToAsIx
    :: AlonzoEraOnwards era
    -> L.PlutusPurpose L.AsIxItem (ShelleyLedgerEra era)
    -> L.PlutusPurpose L.AsIx (ShelleyLedgerEra era)
  purposeAsIxItemToAsIx onwards purpose =
    alonzoEraOnwardsConstraints onwards $
      L.hoistPlutusPurpose L.toAsIx purpose
