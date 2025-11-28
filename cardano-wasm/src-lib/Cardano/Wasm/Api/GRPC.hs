module Cardano.Wasm.Api.GRPC where

import Cardano.Wasm.Api.Tx qualified as Wasm
import Cardano.Wasm.ExceptionHandling (justOrError, rightOrError, throwError)
import Cardano.Wasm.Gen.Cardano.Rpc.CurrentEra (CurrentEra (..))
import Cardano.Wasm.Gen.Cardano.Rpc.Node (getEra, getProtocolParamsJson)
import Cardano.Wasm.Gen.Cardano.Rpc.ProtocolParamsJson (ProtocolParamsJson (..))
import Cardano.Wasm.Gen.Google.Protobuf.Empty (Empty (Empty))
import Cardano.Wasm.Gen.GrpcClient (GrpcExecutor)
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.AddressArray (AddressArray (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Cardano.TxOutput (TxOutput (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.AnyUtxoData (AnyUtxoData (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.QueryService (readUtxos)
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadUtxosRequest (ReadUtxosRequest (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.ReadUtxosResponse (ReadUtxosResponse (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Query.TxoRef (TxoRef (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.AnyChainTx (AnyChainTx (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitService (submitTx)
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitTxRequest (SubmitTxRequest (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.SubmitTxResponse (SubmitTxResponse (..))
import Cardano.Wasm.Gen.Utxorpc.V1alpha.Submit.TxSubmitResult (TxSubmitResult (..))

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as Text

-- | Create a new unsigned transaction object for making a Conway era transaction.
newGrpcConnectionImpl :: (String -> IO executor) -> String -> IO executor
newGrpcConnectionImpl createClientFunc host = createClientFunc host

-- | Get the era from the Cardano Node using gRPC or GRPC-web.
getEraImpl :: GrpcExecutor -> IO String
getEraImpl caller = do
  CurrentEra{currentEraEra = maybeEra} <- rightOrError =<< getEra caller Empty
  era <- justOrError "getEra returned Nothing" maybeEra
  return $ show era

-- | Get the protocol parameters from the Cardano Node using gRPC or GRPC-web.
getProtocolParamsImpl
  :: GrpcExecutor
  -> IO Wasm.ProtocolParamsJSON
getProtocolParamsImpl caller = do
  ppjson <- rightOrError =<< getProtocolParamsJson caller Empty
  bs <- rightOrError . Base64.decode . BS.pack . Text.unpack . protocolParamsJsonJson $ ppjson
  return $ Wasm.ProtocolParamsJSON $ BS.unpack bs

-- | Get all UTXOs from the node using a gRPC or GRPC-web client.
getAllUtxosImpl :: GrpcExecutor -> IO [(String, Int, String, String)]
getAllUtxosImpl caller = do
  ReadUtxosResponse
    { readUtxosResponseItems = utxoDataList
    } <-
    rightOrError
      =<< readUtxos
        caller
        ( ReadUtxosRequest
            { readUtxosRequestTxoRefs = Nothing
            , readUtxosRequestCardanoAddresses = Nothing
            , readUtxosRequestFieldMask = Nothing
            }
        )
  return
    [ (Text.unpack refHash, refIndex, Text.unpack address, Text.unpack coin)
    | Just
        ( AnyUtxoData
            { anyUtxoDataTxoRef =
              Just
                ( TxoRef
                    { txoRefHash = refHash
                    , txoRefIndex = refIndex
                    }
                  )
            , anyUtxoDataCardano =
              Just
                ( TxOutput
                    { txOutputAddress = address
                    , txOutputCoin = coin
                    }
                  )
            }
          ) <-
        utxoDataList
    ]

-- | Get UTXOs for a given address using a gRPC or GRPC-web client.
getUtxosForAddressImpl
  :: GrpcExecutor
  -> String
  -> IO [(String, Int, String)]
getUtxosForAddressImpl caller address = do
  ReadUtxosResponse
    { readUtxosResponseItems = utxoDataList
    } <-
    rightOrError
      =<< readUtxos
        caller
        ( ReadUtxosRequest
            { readUtxosRequestTxoRefs = Nothing
            , readUtxosRequestCardanoAddresses = Just (AddressArray [Text.pack address])
            , readUtxosRequestFieldMask = Nothing
            }
        )
  return
    [ (Text.unpack refHash, refIndex, Text.unpack coin)
    | Just
        ( AnyUtxoData
            { anyUtxoDataTxoRef =
              Just
                ( TxoRef
                    { txoRefHash = refHash
                    , txoRefIndex = refIndex
                    }
                  )
            , anyUtxoDataCardano =
              Just
                ( TxOutput
                    { txOutputCoin = coin
                    }
                  )
            }
          ) <-
        utxoDataList
    ]

-- | Submit a transaction to the Cardano Node using gRPC or GRPC-web.
submitTxImpl
  :: GrpcExecutor
  -> String
  -> IO String
submitTxImpl caller tx = do
  submissionResult <-
    submitTxResponseResults
      <$> ( rightOrError
              =<< submitTx
                caller
                ( SubmitTxRequest
                    { submitTxRequestTx = [Just (AnyChainTx (Text.pack tx))]
                    }
                )
          )
  txRef <- case submissionResult of
    [] -> throwError "submitTx returned no results"
    [Nothing] -> throwError "submitTx returned no results"
    [ Just
        ( TxSubmitResult
            { txSubmitResultRef = ref
            , txSubmitResultErrorMessage = ""
            }
          )
      ] -> return $ Text.unpack ref
    [ Just
        ( TxSubmitResult
            { txSubmitResultErrorMessage = errMsg
            }
          )
      ] -> throwError $ "submitTx error: " ++ Text.unpack errMsg
    _ -> throwError "submitTx returned more than one result"
  rightOrError $ base64ToBase16 txRef
 where
  -- We reencode as Base16 because it is a more common format for txIds
  base64ToBase16 :: String -> Either String String
  base64ToBase16 encoded = do
    decoded <- Base64.decode $ BS.pack encoded
    return $ BS.unpack $ Base16.encode decoded
