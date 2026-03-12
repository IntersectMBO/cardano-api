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
import Data.Foldable (asum)
import Data.Type.Equality ((:~:) (..))
import Data.Typeable (Typeable, eqT)

data AnyScript era where
  AnySimpleScript :: SimpleScript era -> AnyScript era
  AnyPlutusScript
    :: (Plutus.PlutusLanguage lang, Typeable lang) => PlutusScriptInEra lang era -> AnyScript era

instance L.Era era => HasTypeProxy (AnyScript era) where
  data AsType (AnyScript era) = AsAnyScript
  proxyToAsType _ = AsAnyScript

instance Show (AnyScript era) where
  show (AnySimpleScript ss) = "AnySimpleScript " ++ show ss
  show (AnyPlutusScript ps) = "AnyPlutusScript " ++ show ps

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
    maybe
      ( Left $
          CBOR.DecoderErrorCustom
            "AnyScript"
            "Decoded Script era is neither a NativeScript nor a PlutusScript"
      )
      Right
      $ asum
        [ AnySimpleScript . SimpleScript <$> L.getNativeScript script
        , do
            ps <- L.toPlutusScript script
            L.withPlutusScript ps $ \(plutus :: Plutus.Plutus l) ->
              AnyPlutusScript . PlutusScriptInEra
                <$> either (const Nothing) Just (Plutus.decodePlutusRunnable (L.eraProtVerHigh @era) plutus)
        ]
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
