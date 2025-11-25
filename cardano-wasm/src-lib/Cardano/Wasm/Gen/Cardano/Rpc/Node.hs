{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wasm.Gen.Cardano.Rpc.Node where

import Cardano.Wasm.Gen.Cardano.Rpc.CurrentEra
import Cardano.Wasm.Gen.Cardano.Rpc.ProtocolParamsJson
import Cardano.Wasm.Gen.Google.Protobuf.Empty (Empty)
import Cardano.Wasm.Gen.GrpcClient

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding qualified as T

-- | Call GetEra
getEra :: GrpcExecutor -> Empty -> IO (Either String CurrentEra)
getEra exec req = do
  let jsonReq = decodeUtf8 $ BSL.toStrict $ encode req
  resp <- exec service method (T.unpack jsonReq)
  return $ eitherDecode (BSL.fromStrict $ T.encodeUtf8 $ T.pack resp)
 where
  service = "Cardano.Rpc.Node"
  method = "GetEra"

-- | Call GetProtocolParamsJson
getProtocolParamsJson
  :: GrpcExecutor -> Empty -> IO (Either String ProtocolParamsJson)
getProtocolParamsJson exec req = do
  let jsonReq = decodeUtf8 $ BSL.toStrict $ encode req
  resp <- exec service method (T.unpack jsonReq)
  return $ eitherDecode (BSL.fromStrict $ T.encodeUtf8 $ T.pack resp)
 where
  service = "Cardano.Rpc.Node"
  method = "GetProtocolParamsJson"
