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
import qualified Data.Text as Text
import GHC.Wasm.Prim

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

-- API functions

foreign export javascript "mkTxIn"
  mkTxIn :: JSString -> Int -> IO JSVal

mkTxIn txId txIx = do
  let (Right srcTxId) = Api.deserialiseFromRawBytesHex Api.AsTxId (fromString (fromJSString txId))
  let srcTxIx = Api.TxIx (fromIntegral txIx)
  let txIn = Api.TxIn srcTxId srcTxIx
  jsonToJSVal txIn

foreign export javascript "mkTransaction"
  mkTransaction :: JSVal -> JSString -> JSVal -> JSVal -> IO JSVal

mkTransaction txIn destAddr bigIntAmount bigIntFees = do
  -- Convert JS params to Haskell types
  srcTxIx <- jsValToJSON txIn
  let destAddrText = Text.pack (fromJSString destAddr)
  amount <- fromJSBigInt bigIntAmount
  fees <- fromJSBigInt bigIntFees
  -- Call the function
  txBody <- mkTransactionImpl srcTxIx destAddrText amount fees
  -- Convert Haskell type to JS
  let envelope = Api.serialiseToTextEnvelope (Just "Ledger Cddl Format") txBody
  jsonToJSVal envelope
 where
  mkTransactionImpl :: Api.TxIn -> Text.Text -> Integer -> Integer -> IO (Api.TxBody Api.ConwayEra)
  mkTransactionImpl srcTxIn destAddr amount fees = do
    let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
    let txIn =
          ( srcTxIn
          , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
          )
    let (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra Api.AsConwayEra) destAddr
    let txOut =
          Api.TxOut
            destAddress
            (Api.lovelaceToTxOutValue sbe (fromInteger amount))
            Api.TxOutDatumNone
            Script.ReferenceScriptNone
    let txFee = Api.TxFeeExplicit sbe (fromInteger fees)

    let txBodyContent =
          Api.defaultTxBodyContent sbe
            & Api.setTxIns [txIn]
            & Api.setTxOuts [txOut]
            & Api.setTxFee txFee
    let (Right txBody) = Api.createTransactionBody sbe txBodyContent
    return txBody

foreign export javascript "signTransaction"
  signTransaction :: JSVal -> JSString -> IO JSVal

signTransaction txBody privKey = do
  -- Convert JS params to Haskell types
  unsignedTx <- jsValToType (Api.AsTxBody Api.AsConwayEra) txBody
  let (Right signingKey) = Api.deserialiseFromBech32 (Api.AsSigningKey Api.AsPaymentKey) (Text.pack (fromJSString privKey))
  -- Call the function
  oldApiSignedTx <- signTransactionImpl unsignedTx signingKey
  -- Convert Haskell type to JS
  let envelope = Api.serialiseToTextEnvelope (Just "Ledger Cddl Format") oldApiSignedTx
  jsonToJSVal envelope
 where
  signTransactionImpl
    :: Api.TxBody Api.ConwayEra -> Api.SigningKey Api.PaymentKey -> IO (Api.Tx Api.ConwayEra)
  signTransactionImpl unsignedTx signingKey = do
    let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
    let witness = Api.WitnessPaymentKey signingKey
    let oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe unsignedTx [witness]
    return oldApiSignedTx
