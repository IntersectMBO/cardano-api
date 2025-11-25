{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.QueryService where

import Cardano.Wasm.Gen.GrpcClient
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadParamsRequest
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadParamsResponse
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadUtxosRequest
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadUtxosResponse

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | Call ReadUtxos
readUtxos :: GrpcExecutor -> ReadUtxosRequest -> IO (Either String ReadUtxosResponse)
readUtxos exec req = do
  let jsonReq = decodeUtf8 $ BSL.toStrict $ encode req
  resp <- exec service method (T.unpack jsonReq)
  return $ eitherDecode (BSL.fromStrict $ encodeUtf8 $ T.pack resp)
 where
  service = "Utxorpc.V1alpha.Query.QueryService"
  method = "ReadUtxos"

-- | Call ReadParams
readParams :: GrpcExecutor -> ReadParamsRequest -> IO (Either String ReadParamsResponse)
readParams exec req = do
  let jsonReq = decodeUtf8 $ BSL.toStrict $ encode req
  resp <- exec service method (T.unpack jsonReq)
  return $ eitherDecode (BSL.fromStrict $ encodeUtf8 $ T.pack resp)
 where
  service = "Utxorpc.V1alpha.Query.QueryService"
  method = "ReadParams"
