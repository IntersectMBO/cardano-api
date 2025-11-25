{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitService where

import Cardano.Wasm.Gen.GrpcClient
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitTxRequest
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitTxResponse

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | Call SubmitTx
submitTx :: GrpcExecutor -> SubmitTxRequest -> IO (Either String SubmitTxResponse)
submitTx exec req = do
  let jsonReq = decodeUtf8 $ BSL.toStrict $ encode req
  resp <- exec service method (T.unpack jsonReq)
  return $ eitherDecode (BSL.fromStrict $ encodeUtf8 $ T.pack resp)
 where
  service = "Utxorpc.V1alpha.Submit.SubmitService"
  method = "SubmitTx"
