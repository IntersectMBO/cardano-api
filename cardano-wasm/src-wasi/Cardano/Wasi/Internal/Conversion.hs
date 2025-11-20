{-# LANGUAGE CPP #-}

module Cardano.Wasi.Internal.Conversion
  ( toCJSON
  , fromCJSON
  , intToCStr
  , cstrToInt
  , txIdToString
  , stringToSigningKey
  )
where

import Cardano.Api qualified as Api

import Cardano.Wasm.ExceptionHandling (justOrError, rightOrError)

import Data.Aeson qualified as Aeson
import Data.ByteString.UTF8 qualified as BS
import Data.String (fromString)
import Data.Text qualified as Text
import GHC.Base (when)
import Text.Read (readMaybe)

import Foreign.C (CString)
import Foreign.C.String (newCString, peekCString)
import Foreign.Marshal (free)

toCJSON :: Api.ToJSON a => a -> IO CString
toCJSON x = newCString $ BS.toString $ Api.serialiseToJSON x

fromCJSON :: Api.FromJSON a => Bool -> String -> CString -> IO a
fromCJSON shouldFree expectedType cstr = do
  str <- peekCString cstr
  when shouldFree $ free cstr
  case Aeson.eitherDecodeStrict' (BS.fromString str) of
    Left err ->
      error
        ( "Wrong format for argument when decoding JSON for parameter of type "
            ++ expectedType
            ++ ": "
            ++ show (Api.JsonDecodeError err)
        )
    Right a -> return a

intToCStr :: Integer -> IO CString
intToCStr = newCString . show

cstrToInt :: String -> CString -> IO Integer
cstrToInt paramName cstr = do
  str <- peekCString cstr
  justOrError ("Could not parse " ++ paramName ++ " as an integer number") $ readMaybe str

txIdToString :: CString -> IO Api.TxId
txIdToString txIdCString = do
  txId <- peekCString txIdCString
  rightOrError $ Api.deserialiseFromRawBytesHex (fromString txId)

stringToSigningKey :: CString -> IO (Api.SigningKey Api.PaymentKey)
stringToSigningKey signingKeyCString = do
  string <- peekCString signingKeyCString
  rightOrError $ Api.deserialiseFromBech32 (Text.pack string)
