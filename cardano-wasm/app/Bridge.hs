{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
module Bridge where
#else

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bridge where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Internal.Script as Script
import qualified Cardano.Api.Ledger as Ledger

import qualified Data.Aeson as Aeson
import Data.ByteString.UTF8 (fromString, toString)
import Data.Function ((&))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Wasm.Prim

import qualified WasmApi as WasmApi

-- JS helper functions

foreign import javascript unsafe "JSON.parse($1)"
  js_parse :: JSString -> IO JSVal

foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify :: JSVal -> IO JSString

foreign import javascript unsafe "($1).toString()"
  js_toString :: JSVal -> IO JSString

-- Pure Haskell JS conversion functions

fromJSBigInt :: JSVal -> IO Integer
fromJSBigInt val = do
  jsString <- js_toString val
  let str = fromJSString jsString
  case reads str of
    [(n, "")] -> return n
    _ -> error ("Wrong format for argument when deserialising, expected integer: " ++ show str)

jsonToJSVal :: Api.ToJSON a => a -> IO JSVal
jsonToJSVal a = do
  js_parse (toJSString (toString (Api.serialiseToJSON a)))

jsValToJSON :: Api.FromJSON a => JSVal -> IO a
jsValToJSON val = do
  jsString <- js_stringify val
  let jsonString = fromJSString jsString
  let asType = typeProxy
  case either (Left . Api.JsonDecodeError) Right $ Aeson.eitherDecodeStrict' (fromString jsonString) of
    Left err -> error ("Wrong format for argument when deserialising: " ++ show err)
    Right a -> return a
 where
  typeProxy :: Proxy a
  typeProxy = Proxy

jsValToType :: Api.HasTextEnvelope a => Api.AsType a -> JSVal -> IO a
jsValToType asType val = do
  envelope <- jsValToJSON val
  case Api.deserialiseFromTextEnvelope asType envelope of
    Left err -> error ("Error deserialising envelope in parameter: " ++ show err)
    Right type_ -> return type_

-- Conversion functions between Haskell and JS

class ToJSVal haskellType jsType where
  toJSVal :: haskellType -> IO jsType

instance ToJSVal (Api.TxBody Api.ConwayEra) JSVal where
  toJSVal txBody = do
    let envelope = Api.serialiseToTextEnvelope (Just "Ledger Cddl Format") txBody
    jsonToJSVal envelope

instance ToJSVal (Api.Tx Api.ConwayEra) JSVal where
  toJSVal txBody = do
    let envelope = Api.serialiseToTextEnvelope (Just "Ledger Cddl Format") txBody
    jsonToJSVal envelope

class FromJSVal jsType haskellType where
  fromJSVal :: jsType -> IO haskellType

instance FromJSVal JSVal Api.TxIn where
  fromJSVal = jsValToJSON

instance FromJSVal JSString Text where
  fromJSVal = pure . Text.pack . fromJSString

instance FromJSVal JSVal Integer where
  fromJSVal = fromJSBigInt

instance FromJSVal JSVal (Api.TxBody Api.ConwayEra) where
  fromJSVal = jsValToType (Api.AsTxBody Api.AsConwayEra)

instance FromJSVal JSVal (Api.Tx Api.ConwayEra) where
  fromJSVal = jsValToType (Api.AsTx Api.AsConwayEra)

instance FromJSVal JSString (Api.SigningKey Api.PaymentKey) where
  fromJSVal jsString = do
    let (Right signingKey) = Api.deserialiseFromBech32 (Api.AsSigningKey Api.AsPaymentKey) (Text.pack (fromJSString jsString))
    return signingKey

instance FromJSVal JSString Api.TxId where
  fromJSVal jsString = do
    let (Right txId) = Api.deserialiseFromRawBytesHex Api.AsTxId (fromString (fromJSString jsString))
    return txId

instance FromJSVal Int Api.TxIx where
  fromJSVal = return . Api.TxIx . fromIntegral

-- API functions

foreign export javascript "mkTxIn"
  mkTxIn :: JSString -> Int -> IO JSVal

mkTxIn txId txIx =
  jsonToJSVal =<< Api.TxIn <$> fromJSVal txId <*> fromJSVal txIx

foreign export javascript "mkTransaction"
  mkTransaction :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal

mkTransaction txIn destAddr bigIntAmount bigIntFees =
  toJSVal
    =<< WasmApi.mkTransactionImpl
      <$> fromJSVal txIn
      <*> fromJSVal destAddr
      <*> fromJSVal bigIntAmount
      <*> fromJSVal bigIntFees

foreign export javascript "signTransaction"
  signTransaction :: JSVal -> JSString -> IO JSVal

signTransaction txBody privKey =
  toJSVal
    =<< WasmApi.signTransactionImpl
      <$> fromJSVal txBody
      <*> fromJSVal privKey

#endif