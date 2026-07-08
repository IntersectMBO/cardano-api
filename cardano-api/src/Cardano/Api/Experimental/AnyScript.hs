{-# LANGUAGE DataKinds #-}
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
  , AnyScriptDecodeError (..)
  , deserialiseAnyPlutusScriptOfLanguage
  , deserialiseAnySimpleScript
  , hashAnyScript
  , readAnyScriptBytes
  , readFileAnyScript
  )
where

import Cardano.Api.Error (Error (..), FileError (..), fileIOExceptT)
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.Script hiding (AnyPlutusScript)
import Cardano.Api.Experimental.Plutus.Internal.Script qualified as PlutusScript
import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.HasTypeProxy
import Cardano.Api.IO (File (..), FileDirection (In), readFileBlocking)
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script (toAllegraTimelock)
import Cardano.Api.Plutus.Internal.Script qualified as OldScript
  ( AsType (AsScript, AsSimpleScript)
  , SimpleScript
  )
import Cardano.Api.Pretty (pretty, vsep, (<+>))
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Json (JsonDecodeError (..), deserialiseFromJSON)
import Cardano.Api.Serialise.TextEnvelope (TextEnvelope (..), TextEnvelopeType (..))
import Cardano.Api.Serialise.TextEnvelope.Internal (textEnvelopeType)

import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either.Combinators (maybeToRight, rightToMaybe)
import Data.Foldable (asum)
import Data.Text qualified as Text
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
    maybeToRight noParseError $
      asum
        [ tryNativeScript script
        , tryPlutusScript script
        ]
   where
    decodeScript :: Either CBOR.DecoderError (L.Script era)
    decodeScript = do
      r <- CBOR.runAnnotator <$> CBOR.decodeFull' (L.eraProtVerHigh @era) bs
      r $ CBOR.Full $ BS.fromStrict bs

    tryNativeScript :: L.Script era -> Maybe (AnyScript era)
    tryNativeScript = fmap (AnySimpleScript . SimpleScript) . L.getNativeScript

    tryPlutusScript :: L.Script era -> Maybe (AnyScript era)
    tryPlutusScript script = do
      ps <- L.toPlutusScript script
      L.withPlutusScript ps $ \(plutus :: Plutus.Plutus l) ->
        AnyPlutusScript . PlutusScriptInEra
          <$> rightToMaybe (Plutus.decodePlutusRunnable (L.eraProtVerHigh @era) plutus)

    noParseError :: CBOR.DecoderError
    noParseError =
      CBOR.DecoderErrorCustom
        "AnyScript"
        "Decoded Script era is neither a NativeScript nor a PlutusScript"

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

data AnyScriptDecodeError
  = -- | A text envelope was decoded, but its Plutus CBOR payload could not be.
    AnyScriptPlutusCborError AnyPlutusScriptLanguage CBOR.DecoderError
  | -- | A text envelope was decoded, but its simple-script CBOR payload could not be.
    AnyScriptSimpleCborError CBOR.DecoderError
  | -- | The input could not be decoded as a text envelope, nor as a simple
    -- script. Both failures are retained so the caller can see why each format
    -- was rejected.
    AnyScriptJsonError
      JsonDecodeError
      -- ^ Why the input could not be decoded as a text envelope.
      JsonDecodeError
      -- ^ Why the input could not be decoded as a simple script.
  | -- | The text envelope's type is not a recognised script type.
    AnyScriptUnknownTextEnvelopeType TextEnvelopeType
  deriving Show

instance Error AnyScriptDecodeError where
  prettyError (AnyScriptPlutusCborError lang e) =
    "Failed to decode Plutus script ("
      <> pretty (plutusLanguageToText lang)
      <> ") CBOR:"
        <+> prettyError e
  prettyError (AnyScriptSimpleCborError e) =
    "Failed to decode simple script CBOR:" <+> prettyError e
  prettyError (AnyScriptJsonError teErr simpleErr) =
    vsep
      [ "Input could not be decoded as a text envelope, nor as a simple script."
      , "As a text envelope:" <+> prettyError teErr
      , "As a simple script:" <+> prettyError simpleErr
      ]
  prettyError (AnyScriptUnknownTextEnvelopeType (TextEnvelopeType t)) =
    "Unrecognised script text envelope type:" <+> pretty t

-- | The text envelope type used by simple scripts, read from the canonical
-- 'HasTextEnvelope' instance so it stays in step with the type that producers
-- emit rather than duplicating the string.
simpleScriptTextEnvelopeType :: TextEnvelopeType
simpleScriptTextEnvelopeType =
  textEnvelopeType (OldScript.AsScript OldScript.AsSimpleScript)

-- | Decode an 'AnyScript' from its serialised form: a text envelope wrapping
-- the CBOR encoding of either a simple or a Plutus script, or (as a fallback)
-- the legacy JSON-only encoding of a simple script.
readAnyScriptBytes
  :: forall era
   . Era era
  -> ByteString
  -> Either AnyScriptDecodeError (AnyScript (LedgerEra era))
readAnyScriptBytes era bs =
  case deserialiseFromJSON bs :: Either JsonDecodeError TextEnvelope of
    Right te ->
      let scriptBs = teRawCBOR te
          TextEnvelopeType anyScriptType = teType te
       in case textToPlutusLanguage (Text.pack anyScriptType) of
            Just lang ->
              case obtainCommonConstraints era $
                     decodeAnyPlutusScript @(LedgerEra era) scriptBs lang
                     :: Either CBOR.DecoderError (PlutusScript.AnyPlutusScript (LedgerEra era)) of
                Left e -> Left (AnyScriptPlutusCborError lang e)
                Right (PlutusScript.AnyPlutusScript ps) -> Right (AnyPlutusScript ps)
            Nothing
              | teType te == simpleScriptTextEnvelopeType ->
                  bimap AnyScriptSimpleCborError AnySimpleScript $
                    obtainCommonConstraints era (deserialiseSimpleScript scriptBs)
              | otherwise ->
                  Left (AnyScriptUnknownTextEnvelopeType (teType te))
    Left teErr ->
      case deserialiseFromJSON bs :: Either JsonDecodeError OldScript.SimpleScript of
        Left e -> Left (AnyScriptJsonError teErr e)
        Right script ->
          case era of
            DijkstraEra -> error "TODO Dijkstra: Simple script not supported"
            ConwayEra ->
              obtainConwayConstraints era $
                Right . AnySimpleScript . SimpleScript $
                  toAllegraTimelock script

readFileAnyScript
  :: Era era
  -> File content In
  -> IO (Either (FileError AnyScriptDecodeError) (AnyScript (LedgerEra era)))
readFileAnyScript era path =
  runExceptT $ do
    content <- fileIOExceptT (unFile path) readFileBlocking
    firstExceptT (FileError (unFile path)) $
      hoistEither $
        readAnyScriptBytes era content
