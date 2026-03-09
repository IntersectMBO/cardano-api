{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Experimental.AnyScript
  ( AnyScript (..)
  , AsType (..)
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
import Cardano.Api.Serialise.Cbor

import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable, eqT)
import Prettyprinter (pretty)

data AnyScript era where
  AnySimpleScript :: SimpleScript era -> AnyScript era
  AnyPlutusScript :: (Plutus.PlutusLanguage lang, Typeable lang) => PlutusScriptInEra lang era -> AnyScript era

instance L.Era era => HasTypeProxy (AnyScript era) where
  data AsType (AnyScript era) = AsAnyScript
  proxyToAsType _ = AsAnyScript

instance Eq (AnyScript era) where
  AnySimpleScript s1 == AnySimpleScript s2 = s1 == s2
  AnyPlutusScript (ps1 :: PlutusScriptInEra lang1 era) == AnyPlutusScript (ps2 :: PlutusScriptInEra lang2 era) =
    case eqT @lang1 @lang2 of
      Just Refl -> ps1 == ps2
      Nothing -> False
  _ == _ = False

instance
  L.AlonzoEraScript era
  => SerialiseAsCBOR (AnyScript era)
  where
  serialiseToCBOR (AnySimpleScript (SimpleScript ns)) =
    L.serialize' (L.eraProtVerHigh @era) (L.fromNativeScript ns :: L.Script era)
  serialiseToCBOR (AnyPlutusScript ps) =
    L.serialize' (L.eraProtVerHigh @era) (plutusScriptInEraToScript ps)

  deserialiseFromCBOR _ bs = do
    script <- decodeScript
    case L.getNativeScript script of
      Just ns -> Right $ AnySimpleScript (SimpleScript ns)
      Nothing ->
        case L.toPlutusScript script of
          Just ps ->
            L.withPlutusScript ps $ \(plutus :: Plutus.Plutus l) ->
              case Plutus.decodePlutusRunnable (L.eraProtVerHigh @era) plutus of
                Left e ->
                  Left $
                    CBOR.DecoderErrorCustom
                      ( mconcat
                          [ "AnyScript PlutusScript ("
                          , Text.pack (show (Plutus.plutusLanguage plutus))
                          , ")"
                          ]
                      )
                      (Text.pack . show $ pretty e)
                Right runnable -> Right $ AnyPlutusScript (PlutusScriptInEra runnable)
          Nothing ->
            Left $
              CBOR.DecoderErrorCustom
                "AnyScript"
                "Decoded Script era is neither a NativeScript nor a PlutusScript"
   where
    decodeScript :: Either CBOR.DecoderError (L.Script era)
    decodeScript = do
      r <- CBOR.runAnnotator <$> CBOR.decodeFull' (L.eraProtVerHigh @era) bs
      return $ r $ CBOR.Full $ BS.fromStrict bs

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
