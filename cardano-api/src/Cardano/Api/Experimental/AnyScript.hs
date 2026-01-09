{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Experimental.AnyScript
  ( AnyScript (..)
  , deserialiseAnyPlutusScriptOfLanguage
  , deserialiseAnySimpleScript
  , hashAnyScript
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.Script hiding (AnyPlutusScript)
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger.Internal.Reexport qualified as L

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Data.ByteString qualified as BS

data AnyScript era where
  AnySimpleScript :: SimpleScript era -> AnyScript era
  AnyPlutusScript :: Plutus.PlutusLanguage lang => PlutusScriptInEra lang era -> AnyScript era

hashAnyScript :: forall era. IsEra era => AnyScript (LedgerEra era) -> L.ScriptHash
hashAnyScript (AnySimpleScript ss) =
  hashSimpleScript ss
hashAnyScript (AnyPlutusScript ps) =
  hashPlutusScriptInEra ps

deserialiseAnySimpleScript
  :: forall era. IsEra era => BS.ByteString -> Either CBOR.DecoderError (AnyScript (LedgerEra era))
deserialiseAnySimpleScript bs =
  AnySimpleScript <$> obtainCommonConstraints (useEra @era) (deserialiseSimpleScript bs)

deserialiseAnyPlutusScriptOfLanguage
  :: forall era lang
   . (IsEra era, Plutus.PlutusLanguage lang, HasTypeProxy (Plutus.SLanguage lang))
  => BS.ByteString -> L.SLanguage lang -> Either CBOR.DecoderError (AnyScript (LedgerEra era))
deserialiseAnyPlutusScriptOfLanguage bs lang = do
  s :: (PlutusScriptInEra lang (LedgerEra era)) <-
    obtainCommonConstraints (useEra @era) (deserialisePlutusScriptInEra lang bs)
  return $ AnyPlutusScript s
