{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
module Bridge where
#else

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
import GHC.Stack (HasCallStack)

import ExceptionHandling (rightOrError)
import qualified WasmApi as WasmApi

-- * JS helper functions

-- | Parse the JSON stored in a JavaScript string (@JSString@)
-- and return it as a JavaScript object (@JSVal@).
foreign import javascript unsafe "JSON.parse($1)"
  js_parse :: JSString -> IO JSVal

-- | Serialise a JavaScript object (@JSVal@) to a JSON string (@JSString@).
foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify :: JSVal -> IO JSString

-- | Call the @toString@ method on a JavaScript object and return it as a @JSString@.
-- This can be used to convert a @BigInt@ to its string representation.
foreign import javascript unsafe "($1).toString()"
  js_toString :: JSVal -> IO JSString

-- * Primitive Haskell JS conversion functions.

-- | Convert a @BigInt@ (@JSVal@) to a Haskell @Integer@ without loss of precision.
fromJSBigInt :: HasCallStack => JSVal -> IO Integer
fromJSBigInt val = do
  jsString <- js_toString val
  let str = fromJSString jsString
  case reads str of
    [(n, "")] -> return n
    _ -> error ("Wrong format for argument when deserialising, expected integer: " ++ show str)

-- | Convert a Haskell value with @ToJSON@ instance to a JavaScript object (@JSVal)
jsonToJSVal :: (HasCallStack, Api.ToJSON a) => a -> IO JSVal
jsonToJSVal a = do
  js_parse (toJSString (toString (Api.serialiseToJSON a)))

-- | Convert a JavaScript object (@JSVal@) to a Haskell value with @FromJSON@ instance.
jsValToJSON :: (HasCallStack, Api.FromJSON a) => String -> JSVal -> IO a
jsValToJSON expectedType val = do
  jsString <- js_stringify val
  let jsonString = fromJSString jsString
  let asType = typeProxy
  case either (Left . Api.JsonDecodeError) Right $ Aeson.eitherDecodeStrict' (fromString jsonString) of
    Left err ->
      error
        ( "Wrong format for argument when decoding JSON for parameter of type "
            ++ expectedType
            ++ ": "
            ++ show err
        )
    Right a -> return a
 where
  typeProxy :: Proxy a
  typeProxy = Proxy

-- | Convert a JavaScript object (@JSVal@) to a Haskell type that has a @TextEnvelope@ instance.
jsValToType :: (HasCallStack, Api.HasTextEnvelope a) => String -> JSVal -> IO a
jsValToType expectedType val = do
  envelope <- jsValToJSON expectedType val
  case Api.deserialiseFromTextEnvelope envelope of
    Left err ->
      error
        ("Error deserialising text envelope for parameter of type " ++ expectedType ++ ": " ++ show err)
    Right type_ -> return type_

-- * High-level definitions for conversion between Haskell and JS

-- | Type class that provides functions to convert values from Haskell to JavaScript.
class ToJSVal haskellType jsType where
  toJSVal :: haskellType -> IO jsType

type JSTxBody = JSVal

instance ToJSVal (Api.TxBody Api.ConwayEra) JSTxBody where
  toJSVal txBody = do
    let envelope = Api.serialiseToTextEnvelope (Just "Ledger Cddl Format") txBody
    jsonToJSVal envelope

type JSTx = JSVal

instance ToJSVal (Api.Tx Api.ConwayEra) JSTx where
  toJSVal txBody = do
    let envelope = Api.serialiseToTextEnvelope (Just "Ledger Cddl Format") txBody
    jsonToJSVal envelope

-- | Type class that provides functions to convert values from JavaScript to Haskell.
class FromJSVal jsType haskellType where
  fromJSVal :: jsType -> IO haskellType

type JSTxIn = JSVal

instance FromJSVal JSTxIn Api.TxIn where
  fromJSVal = jsValToJSON "TxIn"

type JSText = JSString

instance FromJSVal JSText Text where
  fromJSVal = pure . Text.pack . fromJSString

type JSBigInt = JSVal

instance FromJSVal JSBigInt Integer where
  fromJSVal = fromJSBigInt

type JSCoin = JSVal

instance FromJSVal JSCoin Ledger.Coin where
  fromJSVal = fmap fromInteger . fromJSBigInt

instance FromJSVal JSTxBody (Api.TxBody Api.ConwayEra) where
  fromJSVal = jsValToType "TxBody ConwayEra"

instance FromJSVal JSTx (Api.Tx Api.ConwayEra) where
  fromJSVal = jsValToType "Tx ConwayEra"

type JSSigningKey = JSString

instance FromJSVal JSSigningKey (Api.SigningKey Api.PaymentKey) where
  fromJSVal :: HasCallStack => JSSigningKey -> IO (Api.SigningKey Api.PaymentKey)
  fromJSVal jsString = do
    return $ rightOrError $ Api.deserialiseFromBech32 (Text.pack (fromJSString jsString))

type JSTxId = JSString

instance FromJSVal JSTxId Api.TxId where
  fromJSVal :: HasCallStack => JSTxId -> IO Api.TxId
  fromJSVal jsString = do
    return $ rightOrError $ Api.deserialiseFromRawBytesHex (fromString (fromJSString jsString))

type JSTxIx = Int

instance FromJSVal JSTxIx Api.TxIx where
  fromJSVal = return . Api.TxIx . fromIntegral

-- * API functions to expose to JavaScript

-- | Combine a transaction ID and index into a transaction input.
foreign export javascript "mkTxIn"
  mkTxIn :: JSTxId -> JSTxIx -> IO JSTxIn

mkTxIn txId txIx =
  jsonToJSVal =<< Api.TxIn <$> fromJSVal txId <*> fromJSVal txIx

-- | Create a transaction body from a transaction input, destination address, amount, and fees.
foreign export javascript "mkTransaction"
  mkTransaction :: JSTxIn -> JSText -> JSCoin -> JSCoin -> IO JSTxBody
mkTransaction txIn destAddr bigIntAmount bigIntFees =
  toJSVal
    =<< WasmApi.mkTransactionImpl
      <$> fromJSVal txIn
      <*> fromJSVal destAddr
      <*> fromJSVal bigIntAmount
      <*> fromJSVal bigIntFees

-- | Sign a transaction body with a private key.
foreign export javascript "signTransaction"
  signTransaction :: JSTxBody -> JSSigningKey -> IO JSTx
signTransaction txBody privKey =
  toJSVal
    =<< WasmApi.signTransactionImpl
      <$> fromJSVal txBody
      <*> fromJSVal privKey

#endif