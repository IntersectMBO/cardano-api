{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- The 'NonStreaming' constraint of 'rpc' only rejects streaming methods, so GHC sees it as redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Wasm.Api.GRPC where

import Cardano.Api qualified as Api

import Cardano.Wasm.Api.Tx qualified as Wasm
import Cardano.Wasm.ExceptionHandling (justOrError, rightOrError, throwError)

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BS
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.ProtoLens.Service.Types
  ( HasMethod
  , HasMethodImpl (..)
  , Service (..)
  , StreamingType (..)
  )
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import GHC.TypeLits (symbolVal)
import Lens.Micro ((&), (.~), (^.))

import Proto.Cardano.Rpc.Node qualified as Node
import Proto.Cardano.Rpc.Node_Fields qualified as Node
import Proto.Utxorpc.V1beta.Cardano.Cardano qualified as Cardano
import Proto.Utxorpc.V1beta.Cardano.Cardano_Fields qualified as Cardano
import Proto.Utxorpc.V1beta.Query.Query qualified as Query
import Proto.Utxorpc.V1beta.Query.Query_Fields qualified as Query
import Proto.Utxorpc.V1beta.Submit.Submit qualified as Submit
import Proto.Utxorpc.V1beta.Submit.Submit_Fields qualified as Submit

-- | Internal data for the GrpcConnection virtual object. Currently, it is just a wrapper around the grpcClient,
-- which is some object that allows us to interact with the Cardano Node via gRPC or GRPC-web.
newtype GrpcObject grpcClient
  = GrpcObject grpcClient

-- | Create a new gRPC or GRPC-web connection to the Cardano Node.
newGrpcConnectionImpl :: (String -> IO grpcClient) -> String -> IO (GrpcObject grpcClient)
newGrpcConnectionImpl createClientFunc host = GrpcObject <$> createClientFunc host

-- | Send one unary gRPC request: it takes the path of the method (like
-- @\/cardano.rpc.Node\/GetEra@) and the encoded request, and returns the encoded response.
type Transport = String -> ByteString -> IO ByteString

-- | Call the unary gRPC method @m@ of the service @s@ using a 'Transport'.
-- The method fixes the types of the request and the response, so a request
-- that does not belong to the method does not compile.
rpc
  :: forall s m
   . (HasCallStack, HasMethod s m, MethodStreamingType s m ~ 'NonStreaming)
  => Transport
  -> MethodInput s m
  -> IO (MethodOutput s m)
rpc transport request = do
  response <- transport path (encodeMessage request)
  either (\err -> throwError $ "Failed to decode the response of " <> path <> ": " <> err) pure $
    decodeMessage response
 where
  path =
    "/"
      <> symbolVal (Proxy @(ServicePackage s))
      <> "."
      <> symbolVal (Proxy @(ServiceName s))
      <> "/"
      <> symbolVal (Proxy @(MethodName s m))

-- | Make a 'Transport' from a function that takes the request and returns the
-- response as base64 text, which is how they cross over to JavaScript.
base64Transport :: HasCallStack => (String -> String -> IO String) -> Transport
base64Transport send path request = do
  response <- send path (BS.unpack $ Base64.encode request)
  rightOrError $ Base64.decode $ BS.pack response

-- | Get the era from the Cardano Node using gRPC or GRPC-web.
getEraImpl :: HasCallStack => (grpcClient -> Transport) -> GrpcObject grpcClient -> IO Int
getEraImpl transport (GrpcObject client) = do
  response <- rpc @Node.Node @"getEra" (transport client) defMessage
  return $ fromEnum $ response ^. Node.era

-- | Get the protocol parameters from the Cardano Node using gRPC or GRPC-web.
getProtocolParamsImpl
  :: HasCallStack
  => (grpcClient -> Transport)
  -> GrpcObject grpcClient
  -> IO Wasm.ProtocolParamsJSON
getProtocolParamsImpl transport (GrpcObject client) = do
  response <- rpc @Node.Node @"getProtocolParamsJson" (transport client) defMessage
  Wasm.ProtocolParamsJSON . Text.unpack <$> rightOrError (Text.decodeUtf8' $ response ^. Node.json)

-- | Get all UTXOs from the node using a gRPC or GRPC-web client. This always fails,
-- because the node only returns the UTxOs of given addresses or references.
getAllUtxosImpl :: HasCallStack => GrpcObject grpcClient -> IO utxos
getAllUtxosImpl _ =
  throwError
    "getAllUtxos is not supported, because the node only returns the UTxOs of given addresses. Use getUtxosForAddress instead."

-- | Get UTXOs for a given address using a gRPC or GRPC-web client.
-- It asks the node for all the pages of results, and converts each UTxO with 'utxoToJson'.
getUtxosForAddressImpl
  :: HasCallStack
  => (grpcClient -> Transport)
  -> GrpcObject grpcClient
  -> String
  -> IO [Aeson.Value]
getUtxosForAddressImpl transport (GrpcObject client) address = do
  addressAny <-
    justOrError ("Invalid address: " <> address) $
      Api.deserialiseAddress Api.AsAddressAny (Text.pack address)
  let predicate =
        defMessage
          & Query.match . Query.cardano . Cardano.address . Cardano.exactAddress
            .~ Api.serialiseToRawBytes addressAny
      searchFrom token = do
        response <-
          rpc @Query.QueryService @"searchUtxos" (transport client) $
            defMessage
              & Query.predicate .~ predicate
              & Query.maybe'startToken .~ token
        utxos <- mapM utxoToJson $ response ^. Query.items
        case response ^. Query.maybe'nextToken of
          Nothing -> return utxos
          Just next -> (utxos <>) <$> searchFrom (Just next)
  searchFrom Nothing

-- | Convert a UTxO from the node into the object returned to JavaScript: bytes are in
-- hexadecimal, and amounts are decimal strings, so that JavaScript can read them as
-- @BigInt@s without losing precision.
utxoToJson :: HasCallStack => Query.AnyUtxoData -> IO Aeson.Value
utxoToJson utxo = do
  output <- justOrError "The UTxO has no Cardano output" $ utxo ^. Query.maybe'cardano
  return . Aeson.object $
    [ "txId" .= toHex (utxo ^. Query.txoRef . Query.hash)
    , "txIndex" .= (utxo ^. Query.txoRef . Query.index)
    , "lovelace" .= show (bigIntToInteger $ output ^. Cardano.coin)
    , "assets"
        .= [ Aeson.object
               [ "policyId" .= toHex (policy ^. Cardano.policyId)
               , "assets"
                   .= [ Aeson.object
                          [ "name" .= toHex (asset ^. Cardano.name)
                          , "quantity" .= show (bigIntToInteger $ asset ^. Cardano.quantity)
                          ]
                      | asset <- policy ^. Cardano.assets
                      ]
               ]
           | policy <- output ^. Cardano.assets
           ]
    ]
      <> [ "datum"
             .= Aeson.object
               ( ("hash" .= toHex (datum ^. Cardano.hash))
                   : ["originalCbor" .= toHex cbor | Just cbor <- [datum ^. Cardano.maybe'originalCbor]]
               )
         | Just datum <- [output ^. Cardano.maybe'datum]
         ]
      <> [ "script" .= scriptToJson script
         | Just script <- [output ^. Cardano.maybe'script >>= (^. Cardano.maybe'script)]
         ]
 where
  scriptToJson :: Cardano.Script'Script -> Aeson.Value
  scriptToJson script = case script of
    Cardano.Script'Native _ -> Aeson.object ["language" .= ("native" :: String)]
    Cardano.Script'PlutusV1 bytes -> plutusScript "plutusV1" bytes
    Cardano.Script'PlutusV2 bytes -> plutusScript "plutusV2" bytes
    Cardano.Script'PlutusV3 bytes -> plutusScript "plutusV3" bytes
    Cardano.Script'PlutusV4 bytes -> plutusScript "plutusV4" bytes

  plutusScript :: String -> ByteString -> Aeson.Value
  plutusScript language bytes = Aeson.object ["language" .= language, "bytes" .= toHex bytes]

-- | Read a UTxO RPC @BigInt@: an @int64@, or the big-endian bytes of a larger
-- natural number @n@, which stand for @n@ or for @-1 - n@.
bigIntToInteger :: Cardano.BigInt -> Integer
bigIntToInteger bigInt = case bigInt ^. Cardano.maybe'bigInt of
  Nothing -> 0
  Just (Cardano.BigInt'Int int) -> toInteger int
  Just (Cardano.BigInt'BigUInt bytes) -> fromBigEndian bytes
  Just (Cardano.BigInt'BigNInt bytes) -> -1 - fromBigEndian bytes
 where
  fromBigEndian = BS.foldl' (\acc byte -> acc * 256 + toInteger (fromEnum byte)) 0

-- | Submit a transaction to the Cardano Node using gRPC or GRPC-web. It takes the
-- transaction as CBOR in hexadecimal, and returns the transaction ID in hexadecimal.
submitTxImpl
  :: HasCallStack
  => (grpcClient -> Transport)
  -> GrpcObject grpcClient
  -> String
  -> IO String
submitTxImpl transport (GrpcObject client) txCbor = do
  tx <- rightOrError $ Base16.decode $ BS.pack txCbor
  response <-
    rpc @Submit.SubmitService @"submitTx" (transport client) $
      defMessage & Submit.tx . Submit.raw .~ tx
  return $ toHex $ response ^. Submit.ref

toHex :: ByteString -> String
toHex = BS.unpack . Base16.encode
