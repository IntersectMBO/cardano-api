{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Experimental.Plutus.Internal.Script
  ( AnyPlutusScript (..)
  , decodeAnyPlutusScript
  , serialiseAnyPlutusScriptToTextEnvelope
  , deserialiseAnyPlutusScriptFromTextEnvelope
  , AnyPlutusScriptLanguage (..)
  , PlutusScriptInEra (..)
  , mkPlutusScriptInEra
  , PlutusLangInEra (..)
  , plutusLangInEra
  , plutusLangInShelleyBasedEra
  , PlutusScriptOrReferenceInput (..)
  , AsType (..)
  , deserialisePlutusScriptInEra
  , plutusLanguageNotSupportedInEraError
  , hashPlutusScriptInEra
  , plutusScriptInEraLanguage
  , plutusScriptInEraSLanguage
  , plutusScriptInEraToScript
  , plutusLanguageToText
  , textToPlutusLanguage
  , obtainLangConstraints
  )
where

import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
  ( IsShelleyBasedEra (..)
  , ShelleyLedgerEra
  , shelleyBasedEraConstraints
  )
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Plutus.Internal.Language
import Cardano.Api.HasTypeProxy
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Plutus.Internal.Script (removePlutusScriptDoubleEncoding)
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.TextEnvelope.Internal
import Cardano.Api.Tx.Internal.TxIn (TxIn)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.Plutus.Context qualified as L
  ( EraPlutusTxInfo
  , mkSupportedPlutusScript
  )
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
--
-- The second field is the same script as the ledger holds it. Build values with
-- 'mkPlutusScriptInEra', since only a supported language and era pairing has one.
data PlutusScriptInEra (lang :: L.Language) era where
  PlutusScriptInEra
    :: (L.PlutusLanguage lang, L.AlonzoEraScript era)
    => PlutusRunnable lang
    -> L.PlutusScript era
    -> PlutusScriptInEra lang era

deriving instance Show (PlutusScriptInEra lang era)

deriving instance Eq (PlutusScriptInEra lang era)

-- | Build a script the ledger accepts in the era.
mkPlutusScriptInEra
  :: L.EraPlutusTxInfo lang era => PlutusRunnable lang -> PlutusScriptInEra lang era
mkPlutusScriptInEra runnable =
  PlutusScriptInEra runnable $ L.mkSupportedPlutusScript (L.plutusFromRunnable runnable)

instance
  (Typeable era, Typeable lang, HasTypeProxy (Plutus.SLanguage lang))
  => HasTypeProxy (PlutusScriptInEra lang era)
  where
  data AsType (PlutusScriptInEra lang era) = AsPlutusScriptInEra (AsType (L.SLanguage lang))
  proxyToAsType _ = AsPlutusScriptInEra (proxyToAsType (Proxy @(L.SLanguage lang)))

instance
  ( Plutus.PlutusLanguage lang
  , L.Era era
  , L.EraPlutusTxInfo lang era
  , HasTypeProxy (Plutus.SLanguage lang)
  )
  => HasTextEnvelope (PlutusScriptInEra lang era)
  where
  textEnvelopeTypes _ =
    pure . fromString . Text.unpack . plutusLanguageToText $
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
  , L.EraPlutusTxInfo lang era
  , HasTypeProxy (Plutus.SLanguage lang)
  )
  => SerialiseAsCBOR (PlutusScriptInEra (lang :: L.Language) era)
  where
  -- The 'PlutusBinary' stored in the 'PlutusRunnable' already contains
  -- CBOR-wrapped Flat-encoded UPLC bytes (see 'Cardano.Ledger.Plutus.Language'),
  -- so we extract them directly rather than re-encoding with 'L.serialize''.
  serialiseToCBOR (PlutusScriptInEra s _) =
    SBS.fromShort . L.unPlutusBinary . L.plutusBinary $ L.plutusFromRunnable s

  deserialiseFromCBOR _ = deserialisePlutusScriptInEra

-- | Decode a script of a language the era supports. The 'L.EraPlutusTxInfo'
-- constraint is the proof, so only malformed bytes fail.
deserialisePlutusScriptInEra
  :: forall era lang
   . L.EraPlutusTxInfo lang era
  => BS.ByteString
  -> Either CBOR.DecoderError (PlutusScriptInEra lang era)
deserialisePlutusScriptInEra bs = do
  let v = L.eraProtVerHigh @era
      scriptShortBs = SBS.toShort $ removePlutusScriptDoubleEncoding $ LBS.fromStrict bs
  let plutusScript :: Plutus.Plutus lang
      plutusScript = L.Plutus $ L.PlutusBinary scriptShortBs

  let plutusRunnable = Plutus.decodePlutusRunnable v plutusScript
  case Plutus.plutusRunnableResult plutusRunnable of
    Left e ->
      Left $
        CBOR.DecoderErrorCustom "PlutusLedgerApi.Common.ScriptDecodeError" (Text.show $ pretty e)
    Right{} -> pure $ mkPlutusScriptInEra plutusRunnable

hashPlutusScriptInEra
  :: forall era lang. IsEra era => PlutusScriptInEra lang (LedgerEra era) -> L.ScriptHash
hashPlutusScriptInEra (PlutusScriptInEra pr _) =
  case useEra @era of
    ConwayEra -> L.hashPlutusScript $ L.plutusFromRunnable pr
    DijkstraEra -> L.hashPlutusScript $ L.plutusFromRunnable pr

plutusScriptInEraSLanguage
  :: forall lang era. L.PlutusLanguage lang => PlutusScriptInEra lang era -> L.SLanguage lang
plutusScriptInEraSLanguage PlutusScriptInEra{} =
  L.plutusSLanguage (Proxy @lang)

plutusScriptInEraLanguage
  :: forall lang era. L.PlutusLanguage lang => PlutusScriptInEra lang era -> L.Language
plutusScriptInEraLanguage PlutusScriptInEra{} =
  L.plutusLanguage (Proxy @lang)

plutusScriptInEraToScript
  :: forall lang era. PlutusScriptInEra lang era -> L.Script era
plutusScriptInEraToScript (PlutusScriptInEra _ script) =
  L.fromPlutusScript script

-- | You can provide the plutus script directly in the transaction
-- or a reference input that points to the script in the UTxO.
-- Using a reference script saves space in your transaction.
data PlutusScriptOrReferenceInput lang era
  = PScript (PlutusScriptInEra lang era)
  | PReferenceScript TxIn
  deriving (Show, Eq)

data AnyPlutusScript era where
  AnyPlutusScript
    :: (L.Era era, L.EraPlutusTxInfo lang era, Typeable lang, L.PlutusLanguage lang)
    => PlutusScriptInEra lang era -> AnyPlutusScript era

instance Show (AnyPlutusScript era) where
  show (AnyPlutusScript ps) = "AnyPlutusScript " ++ show ps

instance Eq (AnyPlutusScript era) where
  AnyPlutusScript (ps1 :: PlutusScriptInEra lang1 era) == AnyPlutusScript (ps2 :: PlutusScriptInEra lang2 era) =
    case eqT @lang1 @lang2 of
      Just Refl -> ps1 == ps2
      Nothing -> False

-- | The ledger era @era@ comes first for type applications; @apiEra@ follows from it.
decodeAnyPlutusScript
  :: forall era apiEra
   . (IsShelleyBasedEra apiEra, era ~ ShelleyLedgerEra apiEra)
  => ByteString
  -> AnyPlutusScriptLanguage
  -> Either CBOR.DecoderError (AnyPlutusScript era)
decodeAnyPlutusScript bs (AnyPlutusScriptLanguage (lang :: L.SLanguage lang)) =
  case plutusLangInEra @apiEra lang of
    Nothing ->
      shelleyBasedEraConstraints (shelleyBasedEra @apiEra) $
        Left $
          plutusLanguageNotSupportedInEraError @era lang
    Just (PlutusLangInEra _) ->
      AnyPlutusScript
        <$> obtainLangConstraints lang (deserialisePlutusScriptInEra @era @lang bs)

-- | The decoder error reported when a script's language is not in the era's
-- 'PlutusLangInEra' table. Names both the unsupported language and the era.
plutusLanguageNotSupportedInEraError
  :: forall era lang. L.Era era => L.SLanguage lang -> CBOR.DecoderError
plutusLanguageNotSupportedInEraError lang =
  CBOR.DecoderErrorCustom "PlutusScriptInEra" $
    "Plutus language "
      <> plutusLanguageToText (obtainLangConstraints lang (AnyPlutusScriptLanguage lang))
      <> " is not supported in the "
      <> Text.pack (L.eraName @era)
      <> " era"

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

instance Show AnyPlutusScriptLanguage where
  show = Text.unpack . plutusLanguageToText

-- | Serialise an 'AnyPlutusScript' to a 'TextEnvelope'. The text envelope type
-- is determined by the Plutus language version of the script.
serialiseAnyPlutusScriptToTextEnvelope
  :: Maybe TextEnvelopeDescr -> AnyPlutusScript era -> TextEnvelope
serialiseAnyPlutusScriptToTextEnvelope mbDescr (AnyPlutusScript script@PlutusScriptInEra{}) =
  obtainLangConstraints (plutusScriptInEraSLanguage script) $
    serialiseToTextEnvelope mbDescr script

-- | Deserialise an 'AnyPlutusScript' from a 'TextEnvelope'. The text envelope type
-- is matched against the Plutus language versions from 'Plutus.nonNativeLanguages'
-- that the era supports, so new language versions are picked up automatically.
-- The ledger era @era@ comes first for type applications; @apiEra@ follows from it.
deserialiseAnyPlutusScriptFromTextEnvelope
  :: forall era apiEra
   . (IsShelleyBasedEra apiEra, era ~ ShelleyLedgerEra apiEra)
  => TextEnvelope
  -> Either TextEnvelopeError (AnyPlutusScript era)
deserialiseAnyPlutusScriptFromTextEnvelope te =
  case textToPlutusLanguage (Text.pack envType) of
    Just (AnyPlutusScriptLanguage slang)
      | Nothing <- plutusLangInEra @apiEra slang ->
          shelleyBasedEraConstraints (shelleyBasedEra @apiEra) $
            Left . TextEnvelopeDecodeError $
              plutusLanguageNotSupportedInEraError @era slang
    _ -> deserialiseFromTextEnvelopeAnyOf textEnvTypes te
 where
  TextEnvelopeType envType = teType te

  -- Only the era's languages are listed; an envelope for another language
  -- is rejected above with 'plutusLanguageNotSupportedInEraError'.
  textEnvTypes :: [FromSomeType HasTextEnvelope (AnyPlutusScript era)]
  textEnvTypes =
    concatMap
      ( \l ->
          Plutus.withSLanguage l $ \(slang :: Plutus.SLanguage l) ->
            case plutusLangInEra @apiEra slang of
              Just (PlutusLangInEra _) ->
                obtainLangConstraints slang [FromSomeType (asType @(PlutusScriptInEra l era)) AnyPlutusScript]
              Nothing -> []
      )
      Plutus.nonNativeLanguages
