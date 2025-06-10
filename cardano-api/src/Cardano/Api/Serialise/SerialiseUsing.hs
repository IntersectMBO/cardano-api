{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
module Cardano.Api.Serialise.SerialiseUsing
  ( UsingRawBytes (..)
  , UsingRawBytesHex (..)
  , UsingBech32 (..)
  )
where

import Cardano.Api.Error
import Cardano.Api.HasTypeProxy
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Bech32
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Json
import Cardano.Api.Serialise.Raw

import Data.Aeson.Types qualified as Aeson
import Data.Text.Encoding qualified as Text
import Data.Typeable (tyConName, typeRep, typeRepTyCon)

-- | For use with @deriving via@, to provide 'ToCBOR' and 'FromCBOR' instances,
-- based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (ToCBOR, FromCBOR) via (UsingRawBytes Blah)
newtype UsingRawBytes a = UsingRawBytes a

instance SerialiseAsRawBytes a => ToCBOR (UsingRawBytes a) where
  toCBOR (UsingRawBytes x) = toCBOR (serialiseToRawBytes x)

instance SerialiseAsRawBytes a => FromCBOR (UsingRawBytes a) where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes ttoken bs of
      Right x -> return (UsingRawBytes x)
      Left (SerialiseAsRawBytesError msg) -> fail ("cannot deserialise as a " ++ tname ++ ".  The error was: " ++ msg)
   where
    ttoken = proxyToAsType (Proxy :: Proxy a)
    tname = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a hex
-- encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, Pretty) via (UsingRawBytesHex Blah)
-- > deriving (ToJSON, FromJSON) via (UsingRawBytesHex Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingRawBytesHex Blah)
newtype UsingRawBytesHex a = UsingRawBytesHex a

-- | Quotes the representation
instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
  show (UsingRawBytesHex x) = show $ serialiseToRawBytesHex x

instance SerialiseAsRawBytes a => Pretty (UsingRawBytesHex a) where
  pretty (UsingRawBytesHex a) = pretty $ serialiseToRawBytesHexText a

instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
  toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

instance SerialiseAsRawBytes a => FromJSON (UsingRawBytesHex a) where
  parseJSON =
    fmap (fmap UsingRawBytesHex) . Aeson.withText tname $
      failEitherError . deserialiseFromRawBytesHex . Text.encodeUtf8
   where
    tname = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a) where
  toJSONKey =
    Aeson.toJSONKeyText $ \(UsingRawBytesHex x) -> serialiseToRawBytesHexText x

instance SerialiseAsRawBytes a => FromJSONKey (UsingRawBytesHex a) where
  fromJSONKey =
    fmap UsingRawBytesHex . Aeson.FromJSONKeyTextParser $
      failEitherError . deserialiseFromRawBytesHex . Text.encodeUtf8

-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a bech32
-- encoding, based on the 'SerialiseAsBech32' instance.
--
-- > deriving (Show, Pretty) via (UsingBech32 Blah)
-- > deriving (ToJSON, FromJSON) via (UsingBech32 Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingBech32 Blah)
newtype UsingBech32 a = UsingBech32 a

-- | Quotes the representation
instance SerialiseAsBech32 a => Show (UsingBech32 a) where
  show (UsingBech32 x) = show $ serialiseToBech32 x

instance SerialiseAsBech32 a => Pretty (UsingBech32 a) where
  pretty (UsingBech32 a) = pretty $ serialiseToBech32 a

instance SerialiseAsBech32 a => ToJSON (UsingBech32 a) where
  toJSON (UsingBech32 x) = toJSON (serialiseToBech32 x)

instance SerialiseAsBech32 a => FromJSON (UsingBech32 a) where
  parseJSON =
    Aeson.withText tname $ \str ->
      case deserialiseFromBech32 str of
        Right x -> return (UsingBech32 x)
        Left e -> fail $ docToString $ pretty str <> ": " <> prettyError e
   where
    tname = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsBech32 a => ToJSONKey (UsingBech32 a)

instance SerialiseAsBech32 a => FromJSONKey (UsingBech32 a)
