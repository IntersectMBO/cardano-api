{-# LANGUAGE CPP #-}

#if !defined(wasm32_HOST_ARCH)
module Cardano.Wasm.Internal.JavaScript.GRPC where
#else

module Cardano.Wasm.Internal.JavaScript.GRPC (js_newWebGrpcClient, js_getEra, js_submitTx, js_getProtocolParams, js_readAllUtxos, js_readUtxosForAddress) where

import GHC.Wasm.Prim
import Cardano.Wasm.Internal.Api.Tx (ProtocolParamsJSON(..))

-- | Create a GRPC-web client for the Cardano API.
foreign import javascript safe "globalThis.createClient($1)"
  js_newWebGrpcClientImpl :: JSString -> IO JSVal

js_newWebGrpcClient :: String -> IO JSVal
js_newWebGrpcClient = js_newWebGrpcClientImpl . toJSString

-- | Get the era from the Cardano API using a GRPC-web client.
foreign import javascript safe "($1).node.getEra(new proto.Empty(), {})"
  js_getEra :: JSVal -> IO Int

foreign import javascript safe "atob((await ($1).node.getProtocolParamsJson(new proto.Empty(), {})).toObject().json)"
  js_getProtocolParamsImpl :: JSVal -> IO JSString

js_getProtocolParams :: JSVal -> IO ProtocolParamsJSON
js_getProtocolParams client =
  ProtocolParamsJSON . fromJSString <$> js_getProtocolParamsImpl client

-- | Get all UTXOs using a GRPC-web client.
foreign import javascript safe
  "{ let req = new cardano_node.query.ReadUtxosRequest(); \
     let pres = await ($1).query.readUtxos(req, {}); \
     let res = pres.toObject(); \
     return res.itemsList.map(utxo => { \
       return { \
         address: atob(utxo.cardano.address), \
         txId: globalThis.cardanoWasm.base64ToHex(utxo.txoRef.hash), \
         txIndex: utxo.txoRef.index, \
         lovelace: BigInt(utxo.cardano.coin), \
         assets: utxo.cardano.assetsList, \
         datum: utxo.cardano.datum, \
         script: utxo.cardano.script, \
       }; \
     });\
  }"
  js_readAllUtxos :: JSVal -> IO JSVal

-- | Get UTXOs for a given address using a GRPC-web client.
foreign import javascript safe
  "{ let req = new cardano_node.query.ReadUtxosRequest(); \
     let addresses = new proto.AddressArray(); \
     addresses.addItems(btoa($2)); \
     req.setCardanoAddresses(addresses); \
     let pres = await ($1).query.readUtxos(req, {}); \
     let res = pres.toObject(); \
     return res.itemsList.map(utxo => { \
       return { \
         txId: globalThis.cardanoWasm.base64ToHex(utxo.txoRef.hash), \
         txIndex: utxo.txoRef.index, \
         lovelace: BigInt(utxo.cardano.coin), \
         assets: utxo.cardano.assetsList, \
         datum: utxo.cardano.datum, \
         script: utxo.cardano.script, \
       }; \
     }); \
  }"
  js_readUtxosForAddressImpl :: JSVal -> JSString -> IO JSVal

js_readUtxosForAddress :: JSVal -> String -> IO JSVal
js_readUtxosForAddress client address =
  js_readUtxosForAddressImpl client (toJSString address)

-- | Submit a transaction to the Cardano API using a GRPC-web client.
foreign import javascript safe "{ let tx = new cardano_node.submit.AnyChainTx(); \
                                  tx.setRaw(new Uint8Array($2.match(/[0-9a-fA-F]{2}/g).map((byte) => parseInt(byte, 16)))); \
                                  let txList = new cardano_node.submit.SubmitTxRequest(); \
                                  txList.addTx(tx); \
                                  return (($1).submit.submitTx(txList, {}).then((val) => (val.toObject().resultsList[0].ref == '')?('error: ' + val.toObject().resultsList[0].errorMessage):('ok: ' +val.toObject().resultsList[0].ref))) }"
  js_submitTxImpl :: JSVal -> JSString -> IO JSString

js_submitTx :: JSVal -> String -> IO (Either String String)
js_submitTx client txBytes = do
  result <- js_submitTxImpl client (toJSString txBytes)
  let resultStr = fromJSString result
  case resultStr of
    ('o':'k':':':' ':res) -> return $ Right res
    ('e':'r':'r':'o':'r':':':' ':err) -> return $ Left err
    _ -> return $ Left "Transaction submission failed"

#endif
