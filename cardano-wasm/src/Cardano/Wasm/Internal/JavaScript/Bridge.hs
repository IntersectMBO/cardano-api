{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
module Cardano.Wasm.Internal.JavaScript.Bridge where
#else

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wasm.Internal.JavaScript.Bridge where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger

import Cardano.Wasm.Internal.Api.GRPC qualified as Wasm
import Cardano.Wasm.Internal.Api.Info (apiInfo)
import Cardano.Wasm.Internal.Api.Tx qualified as Wasm
import Cardano.Wasm.Internal.Api.Wallet qualified as Wasm
import Cardano.Wasm.Internal.ExceptionHandling (rightOrError)
import Cardano.Wasm.Internal.JavaScript.GRPC
  ( js_getEra
  , js_getProtocolParams
  , js_newWebGrpcClient
  , js_readAllUtxos
  , js_readUtxosForAddress
  , js_submitTx
  )
import Cardano.Wasm.Internal.JavaScript.GRPCTypes (JSGRPCClient, JSUtxos)

import Control.Exception (evaluate)
import Control.Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.UTF8 (fromString, toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable, typeRep)
import GHC.Stack (HasCallStack)
import GHC.Wasm.Prim

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
    _ -> error ("Wrong format for argument when deserialising, expected integer but got: " ++ show str)

-- | Convert a Haskell @String@ to a JavaScript @BigInt@ (@JSVal@).
foreign import javascript unsafe "BigInt($1)"
  js_toBigInt :: JSString -> IO JSVal

-- | Convert a Haskell value with @ToJSON@ instance to a JavaScript object (@JSVal)
jsonToJSVal :: Api.ToJSON a => a -> IO JSVal
jsonToJSVal =
  js_parse . toJSString . toString . Api.serialiseToJSON

-- | Convert a JavaScript object (@JSVal@) to a Haskell value with @FromJSON@ instance.
jsValToJSON :: (Api.FromJSON a, HasCallStack) => String -> JSVal -> IO a
jsValToJSON expectedType val = do
  jsString <- js_stringify val
  let jsonString = fromJSString jsString
  case Aeson.eitherDecodeStrict' (fromString jsonString) of
    Left err ->
      error
        ( "Wrong format for argument when decoding JSON for parameter of type "
            ++ expectedType
            ++ ": "
            ++ show (Api.JsonDecodeError err)
        )
    Right a -> evaluate a

-- | Convert a JavaScript object (@JSVal@) to a JSON @String@ but don't deserialise.
jsValToJSONString :: JSVal -> IO String
jsValToJSONString val = do
  jsString <- js_stringify val
  return $ fromJSString jsString

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

type JSApiInfo = JSVal

type JSUnsignedTx = JSVal

type JSSignedTx = JSVal

type JSTxId = JSString

type JSWallet = JSVal

type JSTxIx = Int

type JSCoin = JSVal

type JSSigningKey = JSString

type JSProtocolParams = JSVal

type JSGrpc = JSGRPCClient

-- * High-level definitions for conversion between Haskell and JS

-- | Type class that provides functions to convert values from Haskell to JavaScript.
class ToJSVal haskellType jsType where
  toJSVal :: HasCallStack => haskellType -> IO jsType

instance {-# OVERLAPPABLE #-} Api.ToJSON a => ToJSVal a JSVal where
  toJSVal :: a -> IO JSVal
  toJSVal = jsonToJSVal

instance ToJSVal String JSString where
  toJSVal :: String -> IO JSString
  toJSVal = return . toJSString

instance ToJSVal Wasm.ProtocolParamsJSON JSProtocolParams where
  toJSVal (Wasm.ProtocolParamsJSON json) = js_parse $ toJSString json

instance ToJSVal Wasm.GrpcObject JSGrpc where
  toJSVal :: Wasm.GrpcObject -> IO JSGrpc
  toJSVal (Wasm.GrpcObject client) = return client

instance ToJSVal Ledger.Coin JSCoin where
  toJSVal :: Ledger.Coin -> IO JSCoin
  toJSVal (Ledger.Coin n) = js_toBigInt $ toJSString $ show n

-- |  Type class that provides functions to convert values from JavaScript to Haskell.
class FromJSVal jsType haskellType where
  fromJSVal :: HasCallStack => jsType -> IO haskellType

instance {-# OVERLAPPABLE #-} (Api.FromJSON a, Typeable a) => FromJSVal JSVal a where
  fromJSVal :: HasCallStack => JSVal -> IO a
  fromJSVal = jsValToJSON (show . typeRep $ (Api.Proxy :: Api.Proxy a))

instance FromJSVal JSString Text where
  fromJSVal :: JSString -> IO Text
  fromJSVal = return . Text.pack . fromJSString

instance FromJSVal JSCoin Ledger.Coin where
  fromJSVal :: HasCallStack => JSCoin -> IO Ledger.Coin
  fromJSVal = fmap (Ledger.Coin . fromInteger) . fromJSBigInt

instance FromJSVal JSString String where
  fromJSVal :: JSString -> IO String
  fromJSVal = return . fromJSString

instance FromJSVal JSSigningKey (Api.SigningKey Api.PaymentKey) where
  fromJSVal :: HasCallStack => JSSigningKey -> IO (Api.SigningKey Api.PaymentKey)
  fromJSVal jsString = do
    rightOrError $ Api.deserialiseFromBech32 (Text.pack (fromJSString jsString))

instance FromJSVal JSTxId Api.TxId where
  fromJSVal :: HasCallStack => JSTxId -> IO Api.TxId
  fromJSVal jsString = do
    rightOrError $ Api.deserialiseFromRawBytesHex (fromString (fromJSString jsString))

instance FromJSVal JSTxIx Api.TxIx where
  fromJSVal :: JSTxIx -> IO Api.TxIx
  fromJSVal = return . Api.TxIx . fromIntegral

instance FromJSVal JSProtocolParams Wasm.ProtocolParamsJSON where
  fromJSVal = fmap Wasm.ProtocolParamsJSON . jsValToJSONString

instance FromJSVal JSGrpc Wasm.GrpcObject where
  fromJSVal :: JSGrpc -> IO Wasm.GrpcObject
  fromJSVal jsVal = return $ Wasm.GrpcObject jsVal

-- * WalletObject

foreign export javascript "generatePaymentWallet"
  generatePaymentWallet :: IO JSWallet

foreign export javascript "restorePaymentWalletFromSigningKeyBech32"
  restorePaymentWalletFromSigningKeyBech32 :: JSString -> IO JSWallet

foreign export javascript "generateTestnetPaymentWallet"
  generateTestnetPaymentWallet :: Int -> IO JSWallet

foreign export javascript "restoreTestnetPaymentWalletFromSigningKeyBech32"
  restoreTestnetPaymentWalletFromSigningKeyBech32 :: Int -> JSString -> IO JSWallet

foreign export javascript "getAddressBech32"
  getAddressBech32 :: JSWallet -> IO JSString

foreign export javascript "getBech32ForVerificationKey"
  getBech32ForVerificationKey :: JSWallet -> IO JSString

foreign export javascript "getBech32ForSigningKey"
  getBech32ForSigningKey :: JSWallet -> IO JSString

foreign export javascript "getBase16ForVerificationKeyHash"
  getBase16ForVerificationKeyHash :: JSWallet -> IO JSString

-- | Generate a simple payment wallet for mainnet.
generatePaymentWallet :: HasCallStack => IO JSWallet
generatePaymentWallet = toJSVal =<< Wasm.generatePaymentWalletImpl

-- | Restore a mainnet payment wallet from a Bech32 encoded signing key.
restorePaymentWalletFromSigningKeyBech32 :: HasCallStack => JSString -> IO JSWallet
restorePaymentWalletFromSigningKeyBech32 jsSigningKeyBech32 =
  toJSVal
    =<< join
      ( Wasm.restorePaymentWalletFromSigningKeyBech32Impl
          <$> fromJSVal jsSigningKeyBech32
      )

-- | Generate a simple payment wallet for testnet, given the testnet's network magic.
generateTestnetPaymentWallet :: HasCallStack => Int -> IO JSWallet
generateTestnetPaymentWallet networkMagic =
  toJSVal
    =<< Wasm.generateTestnetPaymentWalletImpl networkMagic

-- | Restore a testnet payment wallet from a Bech32 encoded signing key.
restoreTestnetPaymentWalletFromSigningKeyBech32 :: HasCallStack => Int -> JSString -> IO JSWallet
restoreTestnetPaymentWalletFromSigningKeyBech32 networkMagic jsSigningKeyBech32 =
  toJSVal
    =<< join
      ( Wasm.restoreTestnetPaymentWalletFromSigningKeyBech32Impl networkMagic
          <$> fromJSVal jsSigningKeyBech32
      )

-- | Get the Bech32 representation of the address. (Can be shared for receiving funds.)
getAddressBech32 :: HasCallStack => JSWallet -> IO JSString
getAddressBech32 jsWallet =
  toJSVal . Wasm.getAddressBech32 =<< fromJSVal jsWallet

-- | Get the Bech32 representation of the verification key of the wallet. (Can be shared for verification.)
getBech32ForVerificationKey :: HasCallStack => JSWallet -> IO JSString
getBech32ForVerificationKey jsWallet =
  toJSVal . Wasm.getBech32ForVerificationKeyImpl =<< fromJSVal jsWallet

-- | Get the Bech32 representation of the signing key of the wallet. (Must be kept secret.)
getBech32ForSigningKey :: HasCallStack => JSWallet -> IO JSString
getBech32ForSigningKey jsWallet =
  toJSVal . Wasm.getBech32ForSigningKeyImpl =<< fromJSVal jsWallet

-- | Get the base16 representation of the hash of the verification key of the wallet.
getBase16ForVerificationKeyHash :: HasCallStack => JSWallet -> IO JSString
getBase16ForVerificationKeyHash jsWallet =
  toJSVal . Wasm.getBase16ForVerificationKeyHashImpl =<< fromJSVal jsWallet

-- * UnsignedTxObject

foreign export javascript "newConwayTx"
  newConwayTx :: IO JSUnsignedTx

foreign export javascript "addTxInput"
  addTxInput :: JSUnsignedTx -> JSTxId -> JSTxIx -> IO JSUnsignedTx

foreign export javascript "addSimpleTxOut"
  addSimpleTxOut :: JSUnsignedTx -> JSString -> JSCoin -> IO JSUnsignedTx

foreign export javascript "setFee"
  setFee :: JSUnsignedTx -> JSCoin -> IO JSUnsignedTx

foreign export javascript "estimateMinFee"
  estimateMinFee :: JSUnsignedTx -> JSProtocolParams -> Int -> Int -> Int -> IO JSCoin

foreign export javascript "signWithPaymentKey"
  signWithPaymentKey :: JSUnsignedTx -> JSSigningKey -> IO JSSignedTx

-- | Create a new Conway era unsigned transaction.
newConwayTx :: HasCallStack => IO JSUnsignedTx
newConwayTx = toJSVal Wasm.newConwayTxImpl

-- | Add a transaction input to an unsigned transaction.
addTxInput :: HasCallStack => JSUnsignedTx -> JSTxId -> JSTxIx -> IO JSUnsignedTx
addTxInput jsUnsignedTx jsTxId jsTxIx =
  toJSVal
    =<< Wasm.addTxInputImpl
      <$> fromJSVal jsUnsignedTx
      <*> fromJSVal jsTxId
      <*> fromJSVal jsTxIx

-- | Add a simple transaction output (address and lovelace amount) to an unsigned transaction.
addSimpleTxOut :: HasCallStack => JSUnsignedTx -> JSString -> JSCoin -> IO JSUnsignedTx
addSimpleTxOut jsUnsignedTx jsDestAddr jsCoin =
  toJSVal
    =<< join
      ( Wasm.addSimpleTxOutImpl
          <$> fromJSVal jsUnsignedTx
          <*> fromJSVal jsDestAddr
          <*> fromJSVal jsCoin
      )

-- | Set the transaction fee for an unsigned transaction.
setFee :: HasCallStack => JSUnsignedTx -> JSCoin -> IO JSUnsignedTx
setFee jsUnsignedTx jsCoin =
  toJSVal
    =<< ( Wasm.setFeeImpl
            <$> fromJSVal jsUnsignedTx
            <*> fromJSVal jsCoin
        )

-- | Estimate the minimum fee for an unsigned transaction.
estimateMinFee :: JSUnsignedTx -> JSProtocolParams -> Int -> Int -> Int -> IO JSCoin
estimateMinFee jsUnsignedTx jsProtocolParams numExtraKeyWitnesses numExtraByronKeyWitnesses totalRefScriptSize = do
  toJSVal
    =<< join
      ( Wasm.estimateMinFeeImpl
          <$> fromJSVal jsUnsignedTx
          <*> fromJSVal jsProtocolParams
          <*> pure numExtraKeyWitnesses
          <*> pure numExtraByronKeyWitnesses
          <*> pure totalRefScriptSize
      )

-- | Sign an unsigned transaction with a payment key.
signWithPaymentKey :: HasCallStack => JSUnsignedTx -> JSSigningKey -> IO JSSignedTx
signWithPaymentKey jsUnsignedTx jsSigningKey =
  toJSVal
    =<< ( Wasm.signWithPaymentKeyImpl
            <$> fromJSVal jsUnsignedTx
            <*> fromJSVal jsSigningKey
        )

-- * SignedTxObject

foreign export javascript "alsoSignWithPaymentKey"
  alsoSignWithPaymentKey :: JSUnsignedTx -> JSSigningKey -> IO JSSignedTx

foreign export javascript "txToCbor"
  txToCbor :: JSSignedTx -> IO JSString

-- | Sign an unsigned transaction with a payment key.
alsoSignWithPaymentKey :: HasCallStack => JSUnsignedTx -> JSSigningKey -> IO JSSignedTx
alsoSignWithPaymentKey jsUnsignedTx jsSigningKey =
  toJSVal
    =<< ( Wasm.alsoSignWithPaymentKeyImpl
            <$> fromJSVal jsUnsignedTx
            <*> fromJSVal jsSigningKey
        )

-- | Convert a signed transaction to its CBOR representation (hex-encoded string).
txToCbor :: HasCallStack => JSSignedTx -> IO JSString
txToCbor jsSignedTx =
  toJSVal . Wasm.toCborImpl =<< fromJSVal jsSignedTx

-- * GrpcObject

foreign export javascript "newGrpcConnection"
  newGrpcConnection :: JSString -> IO JSGrpc

foreign export javascript "getEra"
  getEra :: JSGrpc -> IO Int

foreign export javascript "getProtocolParams"
  getProtocolParams :: JSGrpc -> IO JSVal

foreign export javascript "getAllUtxos"
  getAllUtxos :: JSGrpc -> IO JSUtxos

foreign export javascript "getUtxosForAddress"
  getUtxosForAddress :: JSGrpc -> JSString -> IO JSUtxos

foreign export javascript "submitTx"
  submitTx :: JSGrpc -> JSString -> IO JSString

-- | Create a new gRPC object for making Conway era transactions.
newGrpcConnection :: HasCallStack => JSString -> IO JSGrpc
newGrpcConnection webGrpcUrl = toJSVal =<< join (Wasm.newGrpcConnectionImpl js_newWebGrpcClient <$> fromJSVal webGrpcUrl)

-- | Get the era from the Cardano Node using GRPC-web.
getEra :: HasCallStack => JSGrpc -> IO Int
getEra grpcObject = Wasm.getEraImpl js_getEra =<< fromJSVal grpcObject

-- | Get the protocol parameters from the Cardano Node using GRPC-web.
getProtocolParams :: HasCallStack => JSGrpc -> IO JSProtocolParams
getProtocolParams = toJSVal <=< Wasm.getProtocolParamsImpl js_getProtocolParams <=< fromJSVal

-- | Get all UTXOs from the node using a GRPC-web client.
getAllUtxos :: HasCallStack => JSGrpc -> IO JSUtxos
getAllUtxos grpcObject =
  Wasm.getAllUtxosImpl js_readAllUtxos =<< fromJSVal grpcObject

-- | Get UTXOs for a given address using a GRPC-web client.
getUtxosForAddress :: HasCallStack => JSGrpc -> JSString -> IO JSUtxos
getUtxosForAddress grpcObject address =
  join $ Wasm.getUtxosForAddressImpl js_readUtxosForAddress <$> fromJSVal grpcObject <*> fromJSVal address

-- | Submit a transaction to the Cardano Node using GRPC-web.
submitTx :: HasCallStack => JSGrpc -> JSString -> IO JSString
submitTx grpcObject tx = toJSVal =<< join (Wasm.submitTxImpl js_submitTx <$> fromJSVal grpcObject <*> fromJSVal tx)

-- * API Information

foreign export javascript "getApiInfo"
  getApiInfo :: IO JSApiInfo

getApiInfo :: IO JSApiInfo
getApiInfo = toJSVal apiInfo

#endif
