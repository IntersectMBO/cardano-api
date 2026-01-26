{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Experimental.Plutus.Internal.Script
  ( AnyPlutusScript (..)
  , decodeAnyPlutusScript
  , AnyPlutusScriptLanguage (..)
  , PlutusScriptInEra (..)
  , PlutusScriptOrReferenceInput (..)
  , AsType (..)
  , deserialisePlutusScriptInEra
  , hashPlutusScriptInEra
  , plutusScriptInEraLanguage
  , plutusScriptInEraSLanguage
  , plutusScriptInEraToScript
  , plutusLanguageToText
  , textToPlutusLanguage
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script (removePlutusScriptDoubleEncoding)
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.TextEnvelope.Internal
import Cardano.Api.Tx.Internal.TxIn (TxIn)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language (PlutusRunnable)
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable
import Prettyprinter

-- | A Plutus script in a particular era.
-- Why PlutusRunnable? Mainly for deserialization benefits.
-- The deserialization of this type looks at the
-- major protocol version and the script language to determine if
-- indeed the script is runnable. This is a dramatic improvement over the old api
-- which essentially read a 'ByteString' and hoped for the best.
-- Any failures due to malformed/invalid scripts were caught upon transaction
-- submission or running the script when attempting to predict the necessary execution units.
--
-- Where do we get the major protocol version from?
-- In order to access the major protocol version we pass in an 'era` type parameter which
-- can be translated to the major protocol version.
--
-- Where do we get the script language from?
-- The serialized version of 'PlutusRunnable' encodes the script language.
-- See `DecCBOR (PlutusRunnable l)` in cardano-ledger for more details.
data PlutusScriptInEra (lang :: L.Language) era where
  PlutusScriptInEra :: L.PlutusLanguage lang => PlutusRunnable lang -> PlutusScriptInEra lang era

deriving instance Show (PlutusScriptInEra lang era)

deriving instance Eq (PlutusScriptInEra lang era)

instance
  (Typeable era, Typeable lang, HasTypeProxy (Plutus.SLanguage lang))
  => HasTypeProxy (PlutusScriptInEra lang era)
  where
  data AsType (PlutusScriptInEra lang era) = AsPlutusScriptInEra (AsType (L.SLanguage lang))
  proxyToAsType _ = AsPlutusScriptInEra (proxyToAsType (Proxy @(L.SLanguage lang)))

instance
  (Plutus.PlutusLanguage lang, L.Era era, HasTypeProxy (Plutus.SLanguage lang))
  => HasTextEnvelope (PlutusScriptInEra lang era)
  where
  textEnvelopeType _ =
    fromString . Text.unpack . plutusLanguageToText $
      AnyPlutusScriptLanguage $
        L.plutusSLanguage (Proxy @lang)

-- TODO: Round trip
plutusLanguageToText :: AnyPlutusScriptLanguage -> Text
plutusLanguageToText (AnyPlutusScriptLanguage slang) =
  case slang of
    L.SPlutusV1 -> "PlutusScriptV1"
    L.SPlutusV2 -> "PlutusScriptV2"
    L.SPlutusV3 -> "PlutusScriptV3"
    L.SPlutusV4 -> "PlutusScriptV4"

textToPlutusLanguage :: Text -> Maybe AnyPlutusScriptLanguage
textToPlutusLanguage txt =
  case txt of
    "PlutusScriptV1" -> Just $ AnyPlutusScriptLanguage L.SPlutusV1
    "PlutusScriptV2" -> Just $ AnyPlutusScriptLanguage L.SPlutusV2
    "PlutusScriptV3" -> Just $ AnyPlutusScriptLanguage L.SPlutusV3
    "PlutusScriptV4" -> Just $ AnyPlutusScriptLanguage L.SPlutusV4
    _ -> Nothing

instance
  ( L.Era era
  , Typeable era
  , Typeable lang
  , Plutus.PlutusLanguage lang
  , HasTypeProxy (Plutus.SLanguage lang)
  )
  => SerialiseAsCBOR (PlutusScriptInEra (lang :: L.Language) era)
  where
  serialiseToCBOR (PlutusScriptInEra s) =
    L.serialize' (L.eraProtVerHigh @era) s

  deserialiseFromCBOR _ bs = do
    let v = L.eraProtVerHigh @era
        scriptShortBs = SBS.toShort $ removePlutusScriptDoubleEncoding $ LBS.fromStrict bs
    let plutusScript :: Plutus.Plutus lang
        plutusScript = L.Plutus $ L.PlutusBinary scriptShortBs

    case Plutus.decodePlutusRunnable v plutusScript of
      Left e ->
        Left $
          CBOR.DecoderErrorCustom "PlutusLedgerApi.Common.ScriptDecodeError" (Text.pack . show $ pretty e)
      Right s -> Right $ PlutusScriptInEra s

deserialisePlutusScriptInEra
  :: forall era lang
   . (Plutus.PlutusLanguage lang, HasTypeProxy (Plutus.SLanguage lang))
  => L.Era era
  => L.SLanguage lang
  -> BS.ByteString
  -> Either CBOR.DecoderError (PlutusScriptInEra lang era)
deserialisePlutusScriptInEra _ bs =
  deserialiseFromCBOR (AsPlutusScriptInEra (proxyToAsType (Proxy @(L.SLanguage lang)))) bs

hashPlutusScriptInEra
  :: forall era lang. IsEra era => PlutusScriptInEra lang (LedgerEra era) -> L.ScriptHash
hashPlutusScriptInEra (PlutusScriptInEra pr) =
  case useEra @era of
    ConwayEra -> L.hashPlutusScript $ L.plutusFromRunnable pr
    DijkstraEra -> L.hashPlutusScript $ L.plutusFromRunnable pr

plutusScriptInEraSLanguage
  :: forall lang era. L.PlutusLanguage lang => PlutusScriptInEra lang era -> L.SLanguage lang
plutusScriptInEraSLanguage (PlutusScriptInEra _) =
  L.plutusSLanguage (Proxy @lang)

plutusScriptInEraLanguage
  :: forall lang era. L.PlutusLanguage lang => PlutusScriptInEra lang era -> L.Language
plutusScriptInEraLanguage (PlutusScriptInEra _) =
  L.plutusLanguage (Proxy @lang)

plutusScriptInEraToScript
  :: forall lang era. L.AlonzoEraScript era => PlutusScriptInEra lang era -> L.Script era
plutusScriptInEraToScript (PlutusScriptInEra pr) =
  case L.fromPlutusScript <$> L.mkPlutusScript (L.plutusFromRunnable pr) of
    Nothing ->
      error
        "plutusScriptInEraToScript: Impossible as the failure would have occurred at the point of deserialising the PlutusRunnable value."
    Just script -> script

-- | You can provide the plutus script directly in the transaction
-- or a reference input that points to the script in the UTxO.
-- Using a reference script saves space in your transaction.
data PlutusScriptOrReferenceInput lang era
  = PScript (PlutusScriptInEra lang era)
  | PReferenceScript TxIn
  deriving (Show, Eq)

data AnyPlutusScript era where
  AnyPlutusScript
    :: (L.Era era, Typeable lang, L.PlutusLanguage lang)
    => PlutusScriptInEra lang era -> AnyPlutusScript era

decodeAnyPlutusScript
  :: L.Era era
  => ByteString
  -> AnyPlutusScriptLanguage
  -> Either CBOR.DecoderError (AnyPlutusScript era)
decodeAnyPlutusScript bs (AnyPlutusScriptLanguage lang) =
  AnyPlutusScript
    <$> obtainLangConstraints lang (deserialisePlutusScriptInEra lang bs)

obtainLangConstraints
  :: L.SLanguage lang
  -> ((Plutus.PlutusLanguage lang, Typeable lang, HasTypeProxy (Plutus.SLanguage lang)) => a)
  -> a
obtainLangConstraints L.SPlutusV1 f = f
obtainLangConstraints L.SPlutusV2 f = f
obtainLangConstraints L.SPlutusV3 f = f
obtainLangConstraints L.SPlutusV4 f = f

data AnyPlutusScriptLanguage where
  AnyPlutusScriptLanguage
    :: L.PlutusLanguage lang
    => L.SLanguage lang -> AnyPlutusScriptLanguage
