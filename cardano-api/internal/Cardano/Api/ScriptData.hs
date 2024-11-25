{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.ScriptData
  ( -- * Script data
    HashableScriptData
  , hashScriptDataBytes
  , getOriginalScriptDataBytes
  , getScriptData
  , unsafeHashableScriptData
  , ScriptData (..)
  , friendlyScript
  , friendlyDatum

    -- * Validating metadata
  , validateScriptData
  , ScriptDataRangeError (..)

    -- * Conversion to\/from JSON
  , ScriptDataJsonSchema (..)
  , scriptDataFromJson
  , scriptDataToJson
  , ScriptDataJsonError (..)
  , ScriptDataJsonSchemaError (..)
  , scriptDataFromJsonDetailedSchema
  , scriptDataToJsonDetailedSchema
  , ScriptBytesError (..)
  , ScriptDataJsonBytesError (..)
  , scriptDataJsonToHashable

    -- * Internal conversion functions
  , toPlutusData
  , fromPlutusData
  , toAlonzoData
  , fromAlonzoData

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  )
where

import           Cardano.Api.Eon.AlonzoEraOnwards (AlonzoEraOnwardsConstraints)
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyLedgerEra)
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Pretty
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.TxMetadata (pBytes, pSigned, parseAll)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Allegra.Scripts (showTimelock)
import           Cardano.Ledger.Alonzo.Core (AlonzoEraScript (..))
import           Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage)
import qualified Cardano.Ledger.Api as Alonzo
import           Cardano.Ledger.Binary.Plain (serializeAsHexText)
import           Cardano.Ledger.Core (Era, EraScript (..), Script)
import           Cardano.Ledger.Plutus (Language)
import qualified Cardano.Ledger.Plutus.Data as Plutus
import           Cardano.Ledger.Plutus.Language (Plutus (plutusBinary), languageToText)
import qualified Cardano.Ledger.SafeHash as Ledger
import           Ouroboros.Consensus.Shelley.Eras (StandardAlonzo, StandardCrypto)
import qualified PlutusLedgerApi.V1 as PlutusAPI

import           Codec.Serialise.Class (Serialise (..))
import           Control.Applicative (Alternative (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SB
import qualified Data.Char as Char
import           Data.Data (Data)
import           Data.Either.Combinators
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import qualified Data.Scientific as Scientific
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import           Data.Word
import           GHC.Exts (IsList (..))

-- Original script data bytes
data HashableScriptData
  = HashableScriptData
      !BS.ByteString
      -- ^ Original 'ScriptData' bytes
      !ScriptData
  deriving (Eq, Show)

instance HasTypeProxy HashableScriptData where
  data AsType HashableScriptData = AsHashableScriptData
  proxyToAsType _ = AsHashableScriptData

instance SerialiseAsCBOR HashableScriptData where
  serialiseToCBOR (HashableScriptData origBytes _) = origBytes
  deserialiseFromCBOR AsHashableScriptData bs =
    HashableScriptData bs
      <$> CBOR.decodeFullDecoder "ScriptData" fromCBOR (LBS.fromStrict bs)

getOriginalScriptDataBytes :: HashableScriptData -> BS.ByteString
getOriginalScriptDataBytes (HashableScriptData bs _) = bs

getScriptData :: HashableScriptData -> ScriptData
getScriptData (HashableScriptData _ sd) = sd

-- | Warning: Creating 'HashableScriptData' from a 'ScriptData' value pretty
-- much guarantees the original bytes used to create the 'ScriptData'
-- value will be different if we serialize `HashableScriptData` again.
-- Do not use this.
unsafeHashableScriptData :: ScriptData -> HashableScriptData
unsafeHashableScriptData sd = HashableScriptData (serialiseToCBOR sd) sd

-- ----------------------------------------------------------------------------
-- Script data - Allows us to represent script data as JSON
--

data ScriptData
  = ScriptDataConstructor
      Integer
      -- ^ Tag for the constructor
      [ScriptData]
      -- ^ Constructor arguments
  | -- | Key value pairs
    ScriptDataMap [(ScriptData, ScriptData)]
  | -- | Elements
    ScriptDataList [ScriptData]
  | ScriptDataNumber Integer
  | ScriptDataBytes BS.ByteString
  deriving (Eq, Ord, Show)

-- Note the order of constructors is the same as the Plutus definitions
-- so that the Ord instance is consistent with the Plutus one.
-- This is checked by prop_ord_distributive_ScriptData

instance HasTypeProxy ScriptData where
  data AsType ScriptData = AsScriptData
  proxyToAsType _ = AsScriptData

-- ----------------------------------------------------------------------------
-- Script data hash
--

newtype instance Hash ScriptData
  = ScriptDataHash (Plutus.DataHash StandardCrypto)
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via UsingRawBytesHex (Hash ScriptData)
  deriving (ToJSON, FromJSON) via UsingRawBytesHex (Hash ScriptData)
  deriving (ToJSONKey, FromJSONKey) via UsingRawBytesHex (Hash ScriptData)

instance SerialiseAsRawBytes (Hash ScriptData) where
  serialiseToRawBytes (ScriptDataHash dh) =
    Crypto.hashToBytes (Ledger.extractHash dh)

  deserialiseFromRawBytes (AsHash AsScriptData) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash ScriptData") $
      ScriptDataHash . Ledger.unsafeMakeSafeHash <$> Crypto.hashFromBytes bs

instance SerialiseAsCBOR ScriptData where
  serialiseToCBOR = CBOR.serialize'
  deserialiseFromCBOR AsScriptData bs =
    CBOR.decodeFullDecoder "ScriptData" fromCBOR (LBS.fromStrict bs)
      :: Either CBOR.DecoderError ScriptData

instance ToCBOR ScriptData where
  toCBOR = encode @PlutusAPI.Data . toPlutusData

instance FromCBOR ScriptData where
  fromCBOR :: CBOR.Decoder s ScriptData
  fromCBOR = fromPlutusData <$> decode @PlutusAPI.Data

hashScriptDataBytes :: HashableScriptData -> Hash ScriptData
hashScriptDataBytes =
  ScriptDataHash
    . Plutus.hashData
    . (toAlonzoData :: HashableScriptData -> Plutus.Data StandardAlonzo)

-- ----------------------------------------------------------------------------
-- Conversion functions
--

newtype ScriptBytesError = ScriptBytesError String deriving Show

-- There is a subtlety here. We must use the original bytes
-- when converting to and from `HashableScriptData`/`Data`. This
-- avoids problems that arise due to reserialization of the script
-- data i.e differing script data hashes due to the re-encoding being slightly
-- different to the original encoding. See: https://github.com/input-output-hk/cardano-ledger/issues/2943

toAlonzoData :: Era ledgerera => HashableScriptData -> Plutus.Data ledgerera
toAlonzoData =
  either
    (\e -> error $ "toAlonzoData: " <> show e)
    Plutus.binaryDataToData
    . first ScriptBytesError
    . Plutus.makeBinaryData
    . SB.toShort
    . getOriginalScriptDataBytes

fromAlonzoData :: Plutus.Data ledgerera -> HashableScriptData
fromAlonzoData d =
  HashableScriptData
    (Ledger.originalBytes d)
    (fromPlutusData $ Plutus.getPlutusData d)

toPlutusData :: ScriptData -> PlutusAPI.Data
toPlutusData (ScriptDataConstructor int xs) =
  PlutusAPI.Constr
    int
    [toPlutusData x | x <- xs]
toPlutusData (ScriptDataMap kvs) =
  PlutusAPI.Map
    [ (toPlutusData k, toPlutusData v)
    | (k, v) <- kvs
    ]
toPlutusData (ScriptDataList xs) =
  PlutusAPI.List
    [toPlutusData x | x <- xs]
toPlutusData (ScriptDataNumber n) = PlutusAPI.I n
toPlutusData (ScriptDataBytes bs) = PlutusAPI.B bs

fromPlutusData :: PlutusAPI.Data -> ScriptData
fromPlutusData (PlutusAPI.Constr int xs) =
  ScriptDataConstructor
    int
    [fromPlutusData x | x <- xs]
fromPlutusData (PlutusAPI.Map kvs) =
  ScriptDataMap
    [ (fromPlutusData k, fromPlutusData v)
    | (k, v) <- kvs
    ]
fromPlutusData (PlutusAPI.List xs) =
  ScriptDataList
    [fromPlutusData x | x <- xs]
fromPlutusData (PlutusAPI.I n) = ScriptDataNumber n
fromPlutusData (PlutusAPI.B bs) = ScriptDataBytes bs

-- | Friendly script JSON
friendlyScript :: AlonzoEraOnwardsConstraints era => Script (ShelleyLedgerEra era) -> Aeson.Value
friendlyScript script = Aeson.Object $
  KeyMap.fromList $
    case getNativeScript script of
      Just nativeScript ->
        [ ("type", "native")
        , ("script", Aeson.String $ T.pack $ showTimelock nativeScript)
        ]
      Nothing ->
        ( case toPlutusScript script of
            Just plutusScript -> withPlutusScript plutusScript $ friendlyPlutusScript $ plutusScriptLanguage plutusScript
            Nothing -> [("error", Aeson.String "Unsupported script type")]
        )
 where
  friendlyPlutusScript :: Language -> Plutus l -> [(KeyMap.Key, Aeson.Value)]
  friendlyPlutusScript language plutusScript =
    [ ("type", "plutus")
    , ("plutus version", Aeson.String $ languageToText language)
    , ("script", Aeson.String $ serializeAsHexText $ plutusBinary plutusScript)
    ]

-- | Friendly dats JSON
friendlyDatum
  :: AlonzoEraOnwardsConstraints era => Alonzo.Data (ShelleyLedgerEra era) -> Aeson.Value
friendlyDatum (Alonzo.Data datum) = Aeson.String (T.pack $ show datum)

-- ----------------------------------------------------------------------------
-- Validate script data
--

-- | Validate script data. This is for use with existing constructed script
-- data values, e.g. constructed manually or decoded from CBOR directly.
validateScriptData :: ScriptData -> Either ScriptDataRangeError ()
validateScriptData d =
  case collect d of
    [] -> Right ()
    err : _ -> Left err
 where
  -- Arbitrary size numbers are fine
  collect (ScriptDataNumber _) = []
  -- Arbitrary sized bytes are fine
  collect (ScriptDataBytes _) = []
  collect (ScriptDataList xs) =
    foldMap collect xs
  collect (ScriptDataMap kvs) =
    foldMap
      ( \(k, v) ->
          collect k
            <> collect v
      )
      kvs
  -- Constr tags do need to be less than a Word64
  collect (ScriptDataConstructor n xs) =
    [ ScriptDataConstructorOutOfRange n
    | n > fromIntegral (maxBound :: Word64) || n < 0
    ]
      <> foldMap collect xs

-- | An error in script data due to an out-of-range value.
newtype ScriptDataRangeError
  = -- | The constructor number is outside the maximum range of @-2^64-1 .. 2^64-1@.
    ScriptDataConstructorOutOfRange Integer
  deriving (Eq, Show, Data)

instance Error ScriptDataRangeError where
  prettyError (ScriptDataConstructorOutOfRange n) =
    mconcat
      [ "Constructor numbers in script data value "
      , pretty n
      , " is outside the range 0 .. 2^64-1."
      ]

-- ----------------------------------------------------------------------------
-- JSON conversion
--

-- | Script data is similar to JSON but not exactly the same. It has some
-- deliberate limitations such as no support for floating point numbers or
-- special forms for null or boolean values. It also has limitations on the
-- length of strings. On the other hand, unlike JSON, it distinguishes between
-- byte strings and text strings. It also supports any value as map keys rather
-- than just string. It also supports alternatives \/ tagged unions, used for
-- representing constructors for Plutus data values.
--
-- We provide two different mappings between script data and JSON, useful
-- for different purposes:
--
-- 1. A mapping that allows almost any JSON value to be converted into script
--    data. This does not require a specific JSON schema for the input. It does
--    not expose the full representation capability of script data.
--
-- 2. A mapping that exposes the full representation capability of script data,
--    but relies on a specific JSON schema for the input JSON.
--
-- In the \"no schema"\ mapping, the idea is that (almost) any JSON can be
-- turned into script data and then converted back, without loss. That is, we
-- can round-trip the JSON.
--
-- The subset of JSON supported is all JSON except:
-- * No null or bool values
-- * No floating point, only integers in the range of a 64bit signed integer
-- * A limitation on string lengths
--
-- The approach for this mapping is to use whichever representation as script
-- data is most compact. In particular:
--
-- * JSON lists and maps represented as CBOR lists and maps
-- * JSON strings represented as CBOR strings
-- * JSON hex strings with \"0x\" prefix represented as CBOR byte strings
-- * JSON integer numbers represented as CBOR signed or unsigned numbers
-- * JSON maps with string keys that parse as numbers or hex byte strings,
--   represented as CBOR map keys that are actually numbers or byte strings.
--
-- The string length limit depends on whether the hex string representation
-- is used or not. For text strings the limit is 64 bytes for the UTF8
-- representation of the text string. For byte strings the limit is 64 bytes
-- for the raw byte form (ie not the input hex, but after hex decoding).
--
-- In the \"detailed schema\" mapping, the idea is that we expose the full
-- representation capability of the script data in the form of a JSON schema.
-- This means the full representation is available and can be controlled
-- precisely. It also means any script data can be converted into the JSON and
-- back without loss. That is we can round-trip the script data via the JSON and
-- also round-trip schema-compliant JSON via script data.
data ScriptDataJsonSchema
  = -- | Use the \"no schema\" mapping between JSON and script data as
    -- described above.
    ScriptDataJsonNoSchema
  | -- | Use the \"detailed schema\" mapping between JSON and script data as
    -- described above.
    ScriptDataJsonDetailedSchema
  deriving (Eq, Show)

-- | Convert a value from JSON into script data, using the given choice of
-- mapping between JSON and script data.
--
-- This may fail with a conversion error if the JSON is outside the supported
-- subset for the chosen mapping. See 'ScriptDataJsonSchema' for the details.
scriptDataFromJson
  :: ScriptDataJsonSchema
  -> Aeson.Value
  -> Either ScriptDataJsonError HashableScriptData
scriptDataFromJson schema v = do
  d <- first (ScriptDataJsonSchemaError v) (scriptDataFromJson' v)
  first (ScriptDataRangeError v) (validateScriptData $ getScriptData d)
  return d
 where
  scriptDataFromJson' =
    case schema of
      ScriptDataJsonNoSchema -> scriptDataFromJsonNoSchema
      ScriptDataJsonDetailedSchema -> scriptDataFromJsonDetailedSchema

-- | Convert a script data value into JSON , using the given choice of mapping
-- between JSON and script data.
--
-- This conversion is total but is not necessarily invertible.
-- See 'ScriptDataJsonSchema' for the details.
scriptDataToJson
  :: ScriptDataJsonSchema
  -> HashableScriptData
  -> Aeson.Value
scriptDataToJson schema =
  case schema of
    ScriptDataJsonNoSchema -> scriptDataToJsonNoSchema
    ScriptDataJsonDetailedSchema -> scriptDataToJsonDetailedSchema

-- ----------------------------------------------------------------------------
-- JSON conversion using the the "no schema" style
--

scriptDataToJsonNoSchema :: HashableScriptData -> Aeson.Value
scriptDataToJsonNoSchema = conv . getScriptData
 where
  conv :: ScriptData -> Aeson.Value
  conv (ScriptDataNumber n) = Aeson.Number (fromInteger n)
  conv (ScriptDataBytes bs)
    | Right s <- Text.decodeUtf8' bs
    , Text.all Char.isPrint s =
        Aeson.String s
    | otherwise =
        Aeson.String (bytesPrefix <> Text.decodeLatin1 (Base16.encode bs))
  conv (ScriptDataList vs) = Aeson.Array (fromList (map conv vs))
  conv (ScriptDataMap kvs) =
    Aeson.object
      [ (convKey k, conv v)
      | (k, v) <- kvs
      ]
  conv (ScriptDataConstructor n vs) =
    Aeson.Array $
      fromList
        [ Aeson.Number (fromInteger n)
        , Aeson.Array (fromList (map conv vs))
        ]

  -- Script data allows any value as a key, not just string as JSON does.
  -- For simple types we just convert them to string directly.
  -- For structured keys we render them as JSON and use that as the string.
  convKey :: ScriptData -> Aeson.Key
  convKey (ScriptDataNumber n) = Aeson.fromText $ Text.pack (show n)
  convKey (ScriptDataBytes bs) =
    Aeson.fromText $
      bytesPrefix
        <> Text.decodeLatin1 (Base16.encode bs)
  convKey v =
    Aeson.fromText
      . Text.Lazy.toStrict
      . Aeson.Text.encodeToLazyText
      . conv
      $ v

scriptDataFromJsonNoSchema
  :: Aeson.Value
  -> Either
      ScriptDataJsonSchemaError
      HashableScriptData
scriptDataFromJsonNoSchema = fmap (\sd -> HashableScriptData (serialiseToCBOR sd) sd) . conv
 where
  conv
    :: Aeson.Value
    -> Either ScriptDataJsonSchemaError ScriptData
  conv Aeson.Null = Left ScriptDataJsonNullNotAllowed
  conv Aeson.Bool{} = Left ScriptDataJsonBoolNotAllowed
  conv (Aeson.Number d) =
    case Scientific.floatingOrInteger d :: Either Double Integer of
      Left n -> Left (ScriptDataJsonNumberNotInteger n)
      Right n -> Right (ScriptDataNumber n)
  conv (Aeson.String s)
    | Just s' <- Text.stripPrefix bytesPrefix s
    , let bs' = Text.encodeUtf8 s'
    , Right bs <- Base16.decode bs'
    , not (BSC.any (\c -> c >= 'A' && c <= 'F') bs') =
        Right (ScriptDataBytes bs)
    | otherwise =
        Right (ScriptDataBytes (Text.encodeUtf8 s))
  conv (Aeson.Array vs) =
    fmap ScriptDataList
      . traverse conv
      $ toList vs
  conv (Aeson.Object kvs) =
    fmap ScriptDataMap
      . traverse (\(k, v) -> (,) (convKey k) <$> conv v)
      . List.sortOn fst
      . fmap (first Aeson.toText)
      $ toList kvs

  convKey :: Text -> ScriptData
  convKey s =
    fromMaybe (ScriptDataBytes (Text.encodeUtf8 s)) $
      parseAll
        ( (fmap ScriptDataNumber pSigned <* Atto.endOfInput)
            <|> (fmap ScriptDataBytes pBytes <* Atto.endOfInput)
        )
        s

-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"

data ScriptDataJsonBytesError
  = ScriptDataJsonBytesErrorValue ScriptDataJsonError
  | ScriptDataJsonBytesErrorInvalid ScriptDataRangeError
  deriving (Show, Data)

instance Error ScriptDataJsonBytesError where
  prettyError (ScriptDataJsonBytesErrorValue e) =
    "Error decoding ScriptData JSON value: " <> prettyError e
  prettyError (ScriptDataJsonBytesErrorInvalid e) =
    "ScriptData is invalid: " <> prettyError e

-- | This allows us to take JSON formatted ScriptData and encode it in the CDDL format
-- whilst preserving the original bytes.
scriptDataJsonToHashable
  :: ScriptDataJsonSchema
  -> Aeson.Value
  -- ^ ScriptData Value
  -> Either ScriptDataJsonBytesError HashableScriptData
scriptDataJsonToHashable schema scriptDataVal = do
  sData <- first ScriptDataJsonBytesErrorValue $ scriptDataFromJson schema scriptDataVal
  first ScriptDataJsonBytesErrorInvalid $ validateScriptData $ getScriptData sData
  return sData

-- ----------------------------------------------------------------------------
-- JSON conversion using the "detailed schema" style
--

scriptDataToJsonDetailedSchema :: HashableScriptData -> Aeson.Value
scriptDataToJsonDetailedSchema = conv . getScriptData
 where
  conv :: ScriptData -> Aeson.Value
  conv (ScriptDataNumber n) =
    singleFieldObject "int"
      . Aeson.Number
      $ fromInteger n
  conv (ScriptDataBytes bs) =
    singleFieldObject "bytes"
      . Aeson.String
      $ Text.decodeLatin1 (Base16.encode bs)
  conv (ScriptDataList vs) =
    singleFieldObject "list"
      . Aeson.Array
      $ fromList (map conv vs)
  conv (ScriptDataMap kvs) =
    singleFieldObject "map"
      . Aeson.Array
      $ fromList
        [ Aeson.object [("k", conv k), ("v", conv v)]
        | (k, v) <- kvs
        ]
  conv (ScriptDataConstructor n vs) =
    Aeson.object
      [ ("constructor", Aeson.Number (fromInteger n))
      , ("fields", Aeson.Array (fromList (map conv vs)))
      ]

  singleFieldObject name v = Aeson.object [(name, v)]

scriptDataFromJsonDetailedSchema
  :: Aeson.Value
  -> Either
      ScriptDataJsonSchemaError
      HashableScriptData
scriptDataFromJsonDetailedSchema = fmap (\sd -> HashableScriptData (serialiseToCBOR sd) sd) . conv
 where
  conv
    :: Aeson.Value
    -> Either ScriptDataJsonSchemaError ScriptData
  conv (Aeson.Object m) =
    case List.sort $ toList m of
      [("int", Aeson.Number d)] ->
        case Scientific.floatingOrInteger d :: Either Double Integer of
          Left n -> Left (ScriptDataJsonNumberNotInteger n)
          Right n -> Right (ScriptDataNumber n)
      [("bytes", Aeson.String s)]
        | Right bs <- Base16.decode (Text.encodeUtf8 s) ->
            Right (ScriptDataBytes bs)
      [("list", Aeson.Array vs)] ->
        fmap ScriptDataList
          . traverse conv
          $ toList vs
      [("map", Aeson.Array kvs)] ->
        fmap ScriptDataMap
          . traverse convKeyValuePair
          $ toList kvs
      [ ("constructor", Aeson.Number d)
        , ("fields", Aeson.Array vs)
        ] ->
          case Scientific.floatingOrInteger d :: Either Double Integer of
            Left n -> Left (ScriptDataJsonNumberNotInteger n)
            Right n ->
              fmap (ScriptDataConstructor n)
                . traverse conv
                $ toList vs
      (key, v) : _
        | key `elem` ["int", "bytes", "list", "map", "constructor"] ->
            Left (ScriptDataJsonTypeMismatch (Aeson.toText key) v)
      kvs -> Left (ScriptDataJsonBadObject $ first Aeson.toText <$> kvs)
  conv v = Left (ScriptDataJsonNotObject v)

  convKeyValuePair
    :: Aeson.Value
    -> Either
        ScriptDataJsonSchemaError
        (ScriptData, ScriptData)
  convKeyValuePair (Aeson.Object m)
    | KeyMap.size m == 2
    , Just k <- KeyMap.lookup "k" m
    , Just v <- KeyMap.lookup "v" m =
        (,) <$> conv k <*> conv v
  convKeyValuePair v = Left (ScriptDataJsonBadMapPair v)

-- ----------------------------------------------------------------------------
-- Shared JSON conversion error types
--

data ScriptDataJsonError
  = ScriptDataJsonSchemaError !Aeson.Value !ScriptDataJsonSchemaError
  | ScriptDataRangeError !Aeson.Value !ScriptDataRangeError
  deriving (Eq, Show, Data)

data ScriptDataJsonSchemaError
  = -- Only used for 'ScriptDataJsonNoSchema'
    ScriptDataJsonNullNotAllowed
  | ScriptDataJsonBoolNotAllowed
  | -- Used by both mappings
    ScriptDataJsonNumberNotInteger !Double
  | -- Only used for 'ScriptDataJsonDetailedSchema'
    ScriptDataJsonNotObject !Aeson.Value
  | ScriptDataJsonBadObject ![(Text, Aeson.Value)]
  | ScriptDataJsonBadMapPair !Aeson.Value
  | ScriptDataJsonTypeMismatch !Text !Aeson.Value
  deriving (Eq, Show, Data)

instance Error ScriptDataJsonError where
  prettyError = \case
    ScriptDataJsonSchemaError v detail ->
      mconcat
        [ "JSON schema error within the script data: "
        , pretty (LBS.unpack (Aeson.encode v)) <> "\n" <> prettyError detail
        ]
    ScriptDataRangeError v detail ->
      mconcat
        [ "Value out of range within the script data: "
        , pretty (LBS.unpack (Aeson.encode v)) <> "\n" <> prettyError detail
        ]

instance Error ScriptDataJsonSchemaError where
  prettyError = \case
    ScriptDataJsonNullNotAllowed ->
      "JSON null values are not supported."
    ScriptDataJsonBoolNotAllowed ->
      "JSON bool values are not supported."
    ScriptDataJsonNumberNotInteger d ->
      "JSON numbers must be integers. Unexpected value: " <> pretty d
    ScriptDataJsonNotObject v ->
      "JSON object expected. Unexpected value: " <> pretty (LBS.unpack (Aeson.encode v))
    ScriptDataJsonBadObject v ->
      mconcat
        [ "JSON object does not match the schema.\nExpected a single field named "
        , "\"int\", \"bytes\", \"list\" or \"map\".\n"
        , "Unexpected object field(s): "
        , pretty (LBS.unpack (Aeson.encode (fromList @Aeson.Object $ first Aeson.fromText <$> v)))
        ]
    ScriptDataJsonBadMapPair v ->
      mconcat
        [ "Expected a list of key/value pair { \"k\": ..., \"v\": ... } objects."
        , "\nUnexpected value: " <> pretty (LBS.unpack (Aeson.encode v))
        ]
    ScriptDataJsonTypeMismatch k v ->
      mconcat
        [ "The value in the field " <> pshow k <> " does not have the type "
        , "required by the schema.\nUnexpected value: "
        , pretty (LBS.unpack (Aeson.encode v))
        ]
