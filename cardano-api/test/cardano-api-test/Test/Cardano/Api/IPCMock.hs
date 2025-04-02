{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.IPCMock
  ( tests
  )
where

import Cardano.Api
import Cardano.Api qualified as Api
import Cardano.Api.Consensus qualified as Consensus
import Cardano.Api.Network qualified as Network
import Cardano.Api.Shelley qualified as Shelley

import Control.Concurrent (MVar, killThread, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Lifted (forkFinally)
import Control.Exception.Lifted (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.Monoid (Any)
import Data.Set qualified as Set
import Lens.Micro ((&))
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.Directory (removeFile)
import System.FilePath ((</>))

import Test.Cardano.Ledger.Common (HasCallStack, when)

import Hedgehog (MonadTest, Property, success)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

prop_mockInteractionWithNode :: Property
prop_mockInteractionWithNode =
  H.propertyOnce . H.moduleWorkspace "ipc" $ \work -> do
    -- We set the connection parameters
    let connectionInfo socketFile =
          Api.LocalNodeConnectInfo
            { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 86_400)
            , Api.localNodeNetworkId = Api.Testnet (Api.NetworkMagic 2)
            , Api.localNodeSocketPath = socketFile
            }

    -- We make a query to obtain the current era
    eEra <-
      mockNode
        (work </> "ms1")
        "test/cardano-api-test/files/input/ipc-mock/server-client-1.raw"
        ( \socketFile ->
            H.evalIO $
              runExceptT $
                Api.queryNodeLocalState (connectionInfo (File socketFile)) Network.VolatileTip Api.QueryCurrentEra
        )

    -- We unwrap the error and make sure it is not Byron
    Api.AnyShelleyBasedEra sbe :: Api.AnyShelleyBasedEra <- case eEra of
      Right (Api.AnyCardanoEra era) ->
        Api.caseByronOrShelleyBasedEra
          (fail "We are in Byron era")
          (return . Api.AnyShelleyBasedEra)
          era
      Left Shelley.AFPointTooOld -> fail "Error, point queried in the chain is too old!"
      Left Shelley.AFPointNotOnChain -> fail "Error, point queried is not on chain!"

    srcAddress <- case Api.deserialiseFromBech32
      (Api.AsAddress Api.AsShelleyAddr)
      "addr_test1qp69sr2cw5dwftdpycvqaqkf6vz4jmms226ywvqwc99zm5a0w93czr7h5djdpvlh7edpm77yk20gzqp8dt559a64r9dqyq4r9y" of
      Right addr -> return addr
      Left err -> fail $ "Error deserialising source address: " ++ show err

    destAddress <- case Api.deserialiseFromBech32
      (Api.AsAddress Api.AsShelleyAddr)
      "addr_test1qqc536zmzggkvkes4lf2jpjxw5g0488f656yqlwpuw0uu3fk9tjxma0kpssjkg2e9ltgrl0tvfqay0lr7k4qzdyqya3s6udgu7" of
      Right addr -> return addr
      Left err -> fail $ "Error deserialising destination address: " ++ show err

    -- We make a query to obtain the UTxOs for the given address
    eUtxo <-
      mockNode
        (work </> "ms2")
        "test/cardano-api-test/files/input/ipc-mock/server-client-2.raw"
        ( \socketFile ->
            H.evalIO $
              runExceptT $
                Api.queryNodeLocalState
                  (connectionInfo (File socketFile))
                  Network.VolatileTip
                  ( Api.QueryInEra
                      ( Api.QueryInShelleyBasedEra
                          sbe
                          (Api.QueryUTxO (Api.QueryUTxOByAddress $ Set.singleton $ Api.AddressShelley srcAddress))
                      )
                  )
        )

    -- We unwrap the error and print the number of UTxOs
    utxo <- case eUtxo of
      Right (Right (Api.UTxO utxo)) -> do
        return utxo
      Right (Left (Consensus.EraMismatch{Consensus.ledgerEraName, Consensus.otherEraName})) ->
        fail
          ( "Error, we assumed era was "
              ++ show otherEraName
              ++ " but it was "
              ++ show ledgerEraName
          )
      Left Shelley.AFPointTooOld -> fail "Error, point queried in the chain is too old!"
      Left Shelley.AFPointNotOnChain -> fail "Error, point queried is not on chain!"

    when (Map.null utxo) $
      fail "Error, no UTxOs found for the given address!"

    let txIns =
          [ ( txIn
            , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
            )
          | (txIn, _) <- Map.toList utxo
          ]

    let feeAmount = 200_000

    let totalValue =
          sum
            [ Api.txOutValueToLovelace txOutValue
            | (_, Api.TxOut _ txOutValue _ _) <- Map.toList utxo
            ]
            - feeAmount

    let txOut =
          Api.TxOut
            ( Api.AddressInEra
                (Api.ShelleyAddressInEra sbe)
                destAddress
            )
            (Api.lovelaceToTxOutValue sbe totalValue)
            Api.TxOutDatumNone
            Shelley.ReferenceScriptNone

    let txFee = Api.TxFeeExplicit sbe feeAmount

    let txBodyContent =
          Api.defaultTxBodyContent sbe
            & Api.setTxIns txIns
            & Api.setTxOuts [txOut]
            & Api.setTxFee txFee
    eSigningKey <-
      H.evalIO $
        Api.readFileTextEnvelope
          (Api.AsSigningKey Api.AsPaymentKey)
          "test/cardano-api-test/files/input/ipc-mock/key.skey"

    let signingKey = case eSigningKey of
          Right sk -> sk
          Left err -> error $ "Error deserialising signing key: " ++ show err

    let witness = Api.WitnessPaymentKey signingKey

    let eTxBody = Api.createTransactionBody sbe txBodyContent

    let txBody = case eTxBody of
          Right txBody' -> txBody'
          Left err -> error $ "Error creating transaction body: " ++ show err

    let signedTx = Api.signShelleyTransaction sbe txBody [witness]

    result <-
      mockNode
        (work </> "ms3")
        "test/cardano-api-test/files/input/ipc-mock/server-client-3.raw"
        ( \socketFile ->
            H.evalIO $ Api.submitTxToNodeLocal (connectionInfo (File socketFile)) (Api.TxInMode sbe signedTx)
        )

    case result of
      Api.SubmitSuccess -> success
      Api.SubmitFail reason -> error $ "Error submitting transaction: " ++ show reason

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.IPCMock"
    [ testProperty "Test mock interation with node" prop_mockInteractionWithNode
    ]

-- | Creates a mock node socket that replays the binary data from the given file.
-- To capture the raw data you could run @socat@ like this:
--
-- @
-- socat -b999999999 -R server-client.raw -v UNIX-LISTEN:fake.socket,fork UNIX-CONNECT:node.socket
-- @
--
-- This will connect to @node.socket@ (that should be the socket of a working node)
-- and the command will create a socket file called 'fake.socket' that you can use to
-- connect to the node with the client or the tests. A file 'server-client.raw' will
-- be created that contains the raw data sent from the server to the client and can
-- be used to replay it with this function.
--
-- Make sure to create one file per socket connection. It may not work if you try to
-- use the same file for replaying multiple connections.
mockNode
  :: (MonadBaseControl IO m, MonadTest m, MonadIO m, HasCallStack)
  => FilePath
  -- ^ Directory to create the socket file in.
  -> FilePath
  -- ^ File to read the raw replay data from.
  -> (FilePath -> m a)
  -- ^ Action to run while the server is being mocked.
  -> m a
mockNode work rawReplayData action = do
  actionFinished :: MVar () <- H.evalIO newEmptyMVar
  bracket
    -- Server set-up (resource acquisition)
    ( do
        _ <- H.createDirectoryIfMissing work
        socketFile <- H.noteTempFile work "mock-node.socket"
        sock <- H.evalIO $ do
          sock <- socket AF_UNIX Stream defaultProtocol
          bind sock (SockAddrUnix socketFile)
          listen sock 1
          return sock
        threadId <-
          forkFinally
            ( do
                (conn, _) <- H.evalIO $ accept sock
                handleClient conn rawReplayData
                H.evalIO $ readMVar actionFinished
                return conn
            )
            (\eConn -> do either (const $ return ()) (H.evalIO . close) eConn)
        return (socketFile, sock, threadId)
    )
    -- Server teardown (resource release)
    ( \(socketFile, sock, threadId) -> H.evalIO $ do
        putMVar actionFinished ()
        close sock
        killThread threadId
        removeFile socketFile
    )
    -- Test action
    ( \(socketFile, _, _) ->
        action socketFile
    )
 where
  handleClient :: (MonadTest m, MonadIO m) => Socket -> FilePath -> m ()
  handleClient sock path = do
    (eFileContents :: Either (FileError Any) ByteString) <- readByteStringFile (File path)
    fileContents <- H.evalEither eFileContents
    H.evalIO $ sendAll sock fileContents
