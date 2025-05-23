{-# LANGUAGE ScopedTypeVariables #-}

-- | Raw binary serialisation
module Cardano.Api.Internal.SerialiseUsing
  ( UsingRawBytes (..)
  , UsingRawBytesHex (..)
  , UsingBech32 (..)
  )
where

import Cardano.Api.Internal.Error
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Pretty
import Cardano.Api.Internal.Serialise.Cbor
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseJSON
import Cardano.Api.Internal.SerialiseRaw

import Data.Aeson.Types qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.String (IsString (..))
import Data.Text qualified as Text
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
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a hex
-- encoding, based on the 'SerialiseAsRawBytes' instance.
--
-- > deriving (Show, IsString) via (UsingRawBytesHex Blah)
-- > deriving (ToJSON, FromJSON) via (UsingRawBytesHex Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingRawBytesHex Blah)
newtype UsingRawBytesHex a = UsingRawBytesHex a

instance SerialiseAsRawBytes a => Show (UsingRawBytesHex a) where
  show (UsingRawBytesHex x) = show (serialiseToRawBytesHex x)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
  fromString = either error id . deserialiseFromRawBytesBase16 . BSC.pack

instance SerialiseAsRawBytes a => ToJSON (UsingRawBytesHex a) where
  toJSON (UsingRawBytesHex x) = toJSON (serialiseToRawBytesHexText x)

instance SerialiseAsRawBytes a => FromJSON (UsingRawBytesHex a) where
  parseJSON =
    Aeson.withText tname $
      either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8
   where
    tname = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

instance SerialiseAsRawBytes a => ToJSONKey (UsingRawBytesHex a) where
  toJSONKey =
    Aeson.toJSONKeyText $ \(UsingRawBytesHex x) -> serialiseToRawBytesHexText x

instance SerialiseAsRawBytes a => FromJSONKey (UsingRawBytesHex a) where
  fromJSONKey =
    Aeson.FromJSONKeyTextParser $
      either fail pure . deserialiseFromRawBytesBase16 . Text.encodeUtf8

deserialiseFromRawBytesBase16
  :: SerialiseAsRawBytes a => ByteString -> Either String (UsingRawBytesHex a)
deserialiseFromRawBytesBase16 str =
  case Base16.decode str of
    Right raw -> case deserialiseFromRawBytes ttoken raw of
      Right x -> Right (UsingRawBytesHex x)
      Left (SerialiseAsRawBytesError msg) -> Left ("cannot deserialise " ++ show str ++ ".  The error was: " <> msg)
    Left msg -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
 where
  ttoken = proxyToAsType (Proxy :: Proxy a)

-- | For use with @deriving via@, to provide instances for any\/all of 'Show',
-- 'IsString', 'ToJSON', 'FromJSON', 'ToJSONKey', FromJSONKey' using a bech32
-- encoding, based on the 'SerialiseAsBech32' instance.
--
-- > deriving (Show, IsString) via (UsingBech32 Blah)
-- > deriving (ToJSON, FromJSON) via (UsingBech32 Blah)
-- > deriving (ToJSONKey, FromJSONKey) via (UsingBech32 Blah)
newtype UsingBech32 a = UsingBech32 a

instance SerialiseAsBech32 a => Show (UsingBech32 a) where
  show (UsingBech32 x) = show (serialiseToBech32 x)

instance SerialiseAsBech32 a => IsString (UsingBech32 a) where
  fromString str =
    case deserialiseFromBech32 (Text.pack str) of
      Right x -> UsingBech32 x
      Left e ->
        error $
          docToString $
            "fromString: " <> pretty str <> ": " <> prettyError e

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
