{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Wasm.Api.GRPC where

import Cardano.Wasm.Api.GRPC
  ( GrpcObject (..)
  , Transport
  , base64Transport
  , bigIntToInteger
  , getAllUtxosImpl
  , getEraImpl
  , getProtocolParamsImpl
  , getUtxosForAddressImpl
  , rpc
  , submitTxImpl
  )
import Cardano.Wasm.Api.Tx (ProtocolParamsJSON (..))

import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', modifyIORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.Word (Word32)
import Lens.Micro ((&), (.~), (^.))

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

import Proto.Cardano.Rpc.Node qualified as Node
import Proto.Cardano.Rpc.Node_Fields qualified as Node
import Proto.Utxorpc.V1beta.Cardano.Cardano_Fields qualified as Cardano
import Proto.Utxorpc.V1beta.Query.Query qualified as Query
import Proto.Utxorpc.V1beta.Query.Query_Fields qualified as Query
import Proto.Utxorpc.V1beta.Submit.Submit qualified as Submit
import Proto.Utxorpc.V1beta.Submit.Submit_Fields qualified as Submit

hprop_rpc_sends_the_method_path_and_the_encoded_request :: Property
hprop_rpc_sends_the_method_path_and_the_encoded_request = H.propertyOnce $ do
  calls <- H.evalIO $ newIORef []
  let request = defMessage & Query.maxItems .~ 7 :: Query.SearchUtxosRequest
      response = defMessage & Query.nextToken .~ "next" :: Query.SearchUtxosResponse
      transport path bytes = do
        writeIORef calls [(path, bytes)]
        pure $ encodeMessage response
  received <- H.evalIO $ rpc @Query.QueryService @"searchUtxos" transport request
  received === response
  sent <- H.evalIO $ readIORef calls
  map fst sent === ["/utxorpc.v1beta.query.QueryService/SearchUtxos"]
  map (decodeMessage . snd) sent === [Right request]

hprop_rpc_fails_when_the_response_does_not_decode :: Property
hprop_rpc_fails_when_the_response_does_not_decode = H.propertyOnce $ do
  result <-
    H.evalIO . try @SomeException $
      rpc @Query.QueryService @"searchUtxos" (\_ _ -> pure "\255") defMessage
  H.assert $ isLeft result

hprop_base64Transport_sends_and_reads_base64 :: Property
hprop_base64Transport_sends_and_reads_base64 = H.propertyOnce $ do
  calls <- H.evalIO $ newIORef []
  let send path request = do
        writeIORef calls [(path, request)]
        pure "BAUG"
  response <- H.evalIO $ base64Transport send "/a.B/C" "\1\2\3"
  response === "\4\5\6"
  sent <- H.evalIO $ readIORef calls
  sent === [("/a.B/C", "AQID")]

hprop_getEra_returns_the_era_number :: Property
hprop_getEra_returns_the_era_number = H.propertyOnce $ do
  (transport, calls) <-
    H.evalIO $ fakeTransport [encodeMessage (defMessage & Node.era .~ Node.Conway :: Node.CurrentEra)]
  era <- H.evalIO $ getEraImpl (const transport) (GrpcObject ())
  era === 6
  sent <- H.evalIO calls
  sent === [("/cardano.rpc.Node/GetEra", "")]

hprop_getProtocolParams_returns_the_json :: Property
hprop_getProtocolParams_returns_the_json = H.propertyOnce $ do
  (transport, calls) <-
    H.evalIO $
      fakeTransport
        [encodeMessage (defMessage & Node.json .~ "{\"txFeePerByte\":44}" :: Node.ProtocolParamsJson)]
  ProtocolParamsJSON json <- H.evalIO $ getProtocolParamsImpl (const transport) (GrpcObject ())
  json === "{\"txFeePerByte\":44}"
  sent <- H.evalIO calls
  map fst sent === ["/cardano.rpc.Node/GetProtocolParamsJson"]

hprop_getAllUtxos_fails :: Property
hprop_getAllUtxos_fails = H.propertyOnce $ do
  result <- H.evalIO . try @SomeException $ (getAllUtxosImpl (GrpcObject ()) :: IO ())
  H.assert $ isLeft result

hprop_getUtxosForAddress_reads_all_the_pages :: Property
hprop_getUtxosForAddress_reads_all_the_pages = H.propertyOnce $ do
  let firstPage =
        defMessage
          & Query.items
            .~ [ utxo (BS.replicate 32 0xab) 1 2000000
                   & Query.cardano . Cardano.assets
                     .~ [ defMessage
                            & Cardano.policyId .~ BS.replicate 28 0x01
                            & Cardano.assets
                              .~ [ defMessage
                                     & Cardano.name .~ "tok"
                                     & Cardano.quantity . Cardano.bigUInt .~ BS.pack [1, 0, 0, 0, 0, 0, 0, 0, 0]
                                 ]
                        ]
                   & Query.cardano . Cardano.datum
                     .~ (defMessage & Cardano.hash .~ BS.replicate 32 0xcd & Cardano.originalCbor .~ "\xd8\x79\x80")
                   & Query.cardano . Cardano.script . Cardano.plutusV2 .~ "\x4e\x4d\x01"
               ]
          & Query.nextToken .~ "page-2"
          :: Query.SearchUtxosResponse
      secondPage = defMessage & Query.items .~ [utxo (BS.replicate 32 0xef) 0 1] :: Query.SearchUtxosResponse
  (transport, calls) <-
    H.evalIO $
      fakeTransport [encodeMessage firstPage, encodeMessage secondPage]
  utxos <-
    H.evalIO $
      getUtxosForAddressImpl
        (const transport)
        (GrpcObject ())
        "addr_test1vp93p9em3regvgylxuvet6fgr3e9sn259pcejgrk4ykystcs7v8j6"
  expected <-
    H.evalEither . Aeson.eitherDecodeStrict' $
      mconcat
        [ "[{\"txId\":\"abababababababababababababababababababababababababababababababab\","
        , "\"txIndex\":1,\"lovelace\":\"2000000\","
        , "\"assets\":[{\"policyId\":\"01010101010101010101010101010101010101010101010101010101\","
        , "\"assets\":[{\"name\":\"746f6b\",\"quantity\":\"18446744073709551616\"}]}],"
        , "\"datum\":{\"hash\":\"cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd\","
        , "\"originalCbor\":\"d87980\"},"
        , "\"script\":{\"language\":\"plutusV2\",\"bytes\":\"4e4d01\"}},"
        , "{\"txId\":\"efefefefefefefefefefefefefefefefefefefefefefefefefefefefefefefef\","
        , "\"txIndex\":0,\"lovelace\":\"1\",\"assets\":[]}]"
        ]
  utxos === expected
  sent <- H.evalIO calls
  map fst sent === replicate 2 "/utxorpc.v1beta.query.QueryService/SearchUtxos"
  requests <- H.evalEither $ mapM (decodeMessage . snd) sent
  map
    ( Base16.encode
        . (^. Query.predicate . Query.match . Query.cardano . Cardano.address . Cardano.exactAddress)
    )
    requests
    === replicate 2 "604b10973b88f286209f371995e9281c72584d542871992076a92c482f"
  map (^. Query.maybe'startToken) (requests :: [Query.SearchUtxosRequest])
    === [Nothing, Just "page-2"]

hprop_bigIntToInteger_reads_all_the_forms :: Property
hprop_bigIntToInteger_reads_all_the_forms = H.propertyOnce $ do
  bigIntToInteger (defMessage & Cardano.int .~ (-5)) === -5
  bigIntToInteger (defMessage & Cardano.bigUInt .~ BS.pack [1, 0, 0, 0, 0, 0, 0, 0, 0])
    === 2 ^ (64 :: Int)
  bigIntToInteger (defMessage & Cardano.bigNInt .~ BS.pack [1, 0, 0, 0, 0, 0, 0, 0, 0])
    === -1 - 2 ^ (64 :: Int)
  bigIntToInteger defMessage === 0

hprop_submitTx_sends_the_transaction_and_returns_its_id :: Property
hprop_submitTx_sends_the_transaction_and_returns_its_id = H.propertyOnce $ do
  (transport, calls) <-
    H.evalIO $
      fakeTransport
        [encodeMessage (defMessage & Submit.ref .~ BS.replicate 32 0x12 :: Submit.SubmitTxResponse)]
  txId <- H.evalIO $ submitTxImpl (const transport) (GrpcObject ()) "84a0a0f5f6"
  txId === concat (replicate 32 "12")
  sent <- H.evalIO calls
  map fst sent === ["/utxorpc.v1beta.submit.SubmitService/SubmitTx"]
  requests <- H.evalEither $ mapM (decodeMessage . snd) sent
  map (Base16.encode . (^. Submit.tx . Submit.raw)) (requests :: [Submit.SubmitTxRequest])
    === ["84a0a0f5f6"]

-- | A UTxO with the given transaction hash, output index and lovelace.
utxo :: ByteString -> Word32 -> Int64 -> Query.AnyUtxoData
utxo txHash txIndex lovelace =
  defMessage
    & Query.txoRef . Query.hash .~ txHash
    & Query.txoRef . Query.index .~ txIndex
    & Query.cardano . Cardano.coin . Cardano.int .~ lovelace

-- | A transport that answers the requests with the given responses in order, and
-- an action that returns the method paths and the requests that it got.
fakeTransport :: [ByteString] -> IO (Transport, IO [(String, ByteString)])
fakeTransport responses = do
  pending <- newIORef responses
  calls <- newIORef []
  let transport path request = do
        modifyIORef calls (<> [(path, request)])
        atomicModifyIORef' pending $ \case
          response : rest -> (rest, response)
          [] -> ([], "")
  return (transport, readIORef calls)
