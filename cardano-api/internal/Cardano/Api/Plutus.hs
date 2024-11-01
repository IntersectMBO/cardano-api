{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Plutus
  ( DebugPlutusFailure (..)
  , renderDebugPlutusFailure
  )
where

import           Cardano.Api.Pretty

import qualified Cardano.Ledger.Api as L
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
          pretty args

      evalError = dpfEvaluationError dpf
      binaryScript = case pwc of
        Plutus.PlutusWithContext{Plutus.pwcScript = scr} ->
          let Plutus.Plutus bytes = either id Plutus.plutusFromRunnable scr
           in Text.decodeUtf8 . B64.encode . BSS.fromShort $ Plutus.unPlutusBinary bytes
   in Text.unlines
        [ "Script hash: " <> serializeAsHexText (Plutus.pwcScriptHash pwc)
        , "Script language: " <> Text.pack (show lang)
        , "Protocol version: " <> Text.pack (show (Plutus.pwcProtocolVersion pwc))
        , "Script arguments: " <> docToText scriptArgs
        , "Script evaluation error: " <> docToText (pretty evalError)
        , "Script execution logs: " <> Text.unlines (dpfExecutionLogs dpf)
        , "Script base64 encoded bytes: " <> binaryScript
        ]
