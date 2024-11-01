-- | This module provides an error to conveniently render plutus related failures.
module Cardano.Api.Plutus
  ( DebugPlutusFailure (..)
  , renderDebugPlutusFailure
  )
where

import           Cardano.Api.Pretty

import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.Binary.Encoding (serialize')
import           Cardano.Ledger.Binary.Plain (serializeAsHexText)
import qualified Cardano.Ledger.Plutus.Evaluate as Plutus
import qualified Cardano.Ledger.Plutus.ExUnits as Plutus
import qualified Cardano.Ledger.Plutus.Language as Plutus
import qualified PlutusLedgerApi.V1 as Plutus

import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Short as BSS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Prettyprinter

-- | A structured representation of Plutus script validation failures,
-- providing detailed information about the failed execution for debugging purposes.
-- This type contains the same information as the data constructor
-- 'Cardano.Ledger.Alonzo.Plutus.Evaluate.TransactionScriptFailure.ValidationFailure'
-- but with named fields and fixed crypto parameters for easier debugging and
-- error reporting.
data DebugPlutusFailure
  = DebugPlutusFailure
  { dpfEvaluationError :: Plutus.EvaluationError
  , dpfScriptWithContext :: Plutus.PlutusWithContext L.StandardCrypto
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
