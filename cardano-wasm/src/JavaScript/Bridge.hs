{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
module JavaScript.Bridge where
import qualified Data.ByteString as Text
#else

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JavaScript.Bridge where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Ledger as Ledger -- For Ledger.Coin

-- For explicit JSString conversion

import qualified Data.Aeson as Aeson
import Data.ByteString.UTF8 (fromString, toString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)
import GHC.Wasm.Prim

import General.ExceptionHandling (rightOrError)
import qualified WasmApi.Tx as Wasm
import qualified WasmApi.WasmApi as WasmApi

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
  case either (Left . Api.JsonDecodeError) Right $ Aeson.eitherDecodeStrict' (fromString jsonString) of
    Left err ->
      error
        ( "Wrong format for argument when decoding JSON for parameter of type "
            ++ expectedType
            ++ ": "
            ++ show err
        )
    Right a -> return a

-- | Convert a JavaScript object (@JSVal@) to a Haskell type that has a @TextEnvelope@ instance.
jsValToType :: (HasCallStack, Api.HasTextEnvelope a) => String -> JSVal -> IO a
jsValToType expectedType val = do
  envelope <- jsValToJSON expectedType val
  case Api.deserialiseFromTextEnvelope envelope of
    Left err ->
      error
        ("Error deserialising text envelope for parameter of type " ++ expectedType ++ ": " ++ show err)
    Right type_ -> return type_

-- * Type Synonyms for JSVal representations

type JSUnsignedTx = JSVal

type JSSignedTx = JSVal

type JSTxId = JSSigningKey

type JSTxIx = Int

type JSCoin = JSVal

type JSSigningKey = JSString

-- * High-level definitions for conversion between Haskell and JS

-- | Type class that provides functions to convert values from Haskell to JavaScript.
class ToJSVal haskellType jsType where
  toJSVal :: haskellType -> IO jsType

instance Api.ToJSON a => ToJSVal a JSVal where
  toJSVal = jsonToJSVal

instance ToJSVal String JSString where
  toJSVal :: String -> IO JSString
  toJSVal = return . toJSString

-- |  Type class that provides functions to convert values from JavaScript to Haskell.
class FromJSVal jsType haskellType where
  fromJSVal :: jsType -> IO haskellType

instance {-# OVERLAPPING #-} FromJSVal JSCoin Ledger.Coin where
  fromJSVal = fmap (Ledger.Coin . fromInteger) . fromJSBigInt

instance FromJSVal JSString Text where
  fromJSVal :: JSString -> IO Text
  fromJSVal = return . Text.pack . fromJSString

instance FromJSVal JSString String where
  fromJSVal :: JSString -> IO String
  fromJSVal = return . fromJSString

instance (Api.FromJSON a, Typeable a) => FromJSVal JSVal a where
  fromJSVal = jsValToJSON (show . typeRep $ (Api.Proxy :: Api.Proxy a))

instance FromJSVal JSSigningKey (Api.SigningKey Api.PaymentKey) where
  fromJSVal :: HasCallStack => JSSigningKey -> IO (Api.SigningKey Api.PaymentKey)
  fromJSVal jsString = do
    return $ rightOrError $ Api.deserialiseFromBech32 (Text.pack (fromJSString jsString))

instance FromJSVal JSTxId Api.TxId where
  fromJSVal :: HasCallStack => JSTxId -> IO Api.TxId
  fromJSVal jsString = do
    return $ rightOrError $ Api.deserialiseFromRawBytesHex (fromString (fromJSString jsString))

instance FromJSVal JSTxIx Api.TxIx where
  fromJSVal = return . Api.TxIx . fromIntegral

-- * UnsignedTxObject

foreign export javascript "newConwayTx"
  newConwayTx :: IO JSUnsignedTx

foreign export javascript "addTxInput"
  addTxInput :: JSUnsignedTx -> JSTxId -> JSTxIx -> IO JSUnsignedTx

foreign export javascript "addSimpleTxOut"
  addSimpleTxOut :: JSUnsignedTx -> JSString -> JSCoin -> IO JSUnsignedTx

foreign export javascript "setFee"
  setFee :: JSUnsignedTx -> JSCoin -> IO JSUnsignedTx

foreign export javascript "addSigningKey"
  addSigningKey :: JSUnsignedTx -> JSSigningKey -> IO JSUnsignedTx

foreign export javascript "signTx"
  signTx :: JSUnsignedTx -> IO JSSignedTx

-- | Create a new Conway era unsigned transaction.
newConwayTx :: IO JSUnsignedTx
newConwayTx = toJSVal Wasm.newConwayTxImpl

-- | Add a transaction input to an unsigned transaction.
addTxInput :: JSUnsignedTx -> JSTxId -> JSTxIx -> IO JSUnsignedTx
addTxInput jsUnsignedTx jsTxId jsTxIx =
  toJSVal
    =<< Wasm.addTxInputImpl
      <$> fromJSVal jsUnsignedTx
      <*> fromJSVal jsTxId
      <*> fromJSVal jsTxIx

-- | Add a simple transaction output (address and lovelace amount) to an unsigned transaction.
addSimpleTxOut :: JSUnsignedTx -> JSString -> JSCoin -> IO JSUnsignedTx
addSimpleTxOut jsUnsignedTx jsDestAddr jsCoin = do
  toJSVal
    =<< ( Wasm.addSimpleTxOutImpl
            <$> fromJSVal jsUnsignedTx
            <*> fromJSVal jsDestAddr
            <*> fromJSVal jsCoin
        )

-- | Set the transaction fee for an unsigned transaction.
setFee :: JSUnsignedTx -> JSCoin -> IO JSUnsignedTx
setFee jsUnsignedTx jsCoin =
  toJSVal
    =<< ( Wasm.setFeeImpl
            <$> fromJSVal jsUnsignedTx
            <*> fromJSVal jsCoin
        )

-- | Add a payment signing key to an unsigned transaction.
addSigningKey :: JSUnsignedTx -> JSSigningKey -> IO JSUnsignedTx
addSigningKey jsUnsignedTx jsSigningKey =
  toJSVal
    =<< ( Wasm.addSigningKeyImpl
            <$> fromJSVal jsUnsignedTx
            <*> fromJSVal jsSigningKey
        )

-- | Sign an unsigned transaction.
signTx :: JSUnsignedTx -> IO JSSignedTx
signTx jsUnsignedTx =
  toJSVal =<< Wasm.signTxImpl <$> fromJSVal jsUnsignedTx

-- *  SignedTxObject

foreign export javascript "txToCbor"
  txToCbor :: JSSignedTx -> IO JSString

-- | Convert a signed transaction to its CBOR representation (hex-encoded string).
txToCbor :: JSSignedTx -> IO JSString
txToCbor jsSignedTx =
  toJSVal =<< Wasm.toCborImpl <$> fromJSVal jsSignedTx

#endif