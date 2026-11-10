{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Wasm.Api.GRPC where

import Cardano.Wasm.Api.GRPC (rpc)

import Control.Exception (SomeException, try)
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Lens.Micro ((&), (.~))

import Hedgehog (Property, (===))
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H

import Proto.Utxorpc.V1beta.Query.Query qualified as Query
import Proto.Utxorpc.V1beta.Query.Query_Fields qualified as Query

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
