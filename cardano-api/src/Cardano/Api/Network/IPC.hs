-- | Node IPC protocols
--
-- This module provides the client side of the node-to-client interprocess
-- communication (IPC) for interacting with a local Cardano node. It supports
-- querying the node for information, submitting transactions, monitoring
-- the local mempool, and retrieving historical chain data using the
-- @ChainSync@ protocol.
module Cardano.Api.Network.IPC
  ( -- * Examples

    -- | This section provides two examples:
    --
    --    1. Querying the node to obtain basic information
    --    2. Submitting a transaction to the node.
    --
    -- For details on how to create a transaction, see the
    -- "Cardano.Api.Tx.Internal.Body" documentation.
    --
    -- The following qualified imports from @cardano-api@ are
    -- used in the examples below:
    --
    -- @
    -- import qualified Cardano.Api as Api
    -- import qualified Cardano.Api.Consensus as Consensus
    -- import qualified Cardano.Api.Network as Network
    -- @
    --
    -- The following explicit import from @base@ is also required:
    --
    -- @
    -- import Control.Monad.Except (runExceptT)
    -- @
    --
    -- The examples assume the use of the @IO@ monad and unqualified
    -- access to the @Prelude@ module.

    -- ** Constructing connection information

    -- | Regardless of whether the goal is to query the node or submit transactions,
    -- the first step is to gather the connection information.
    --
    -- The following information is required:
    --
    --    * __The number of slots per epoch__. This value depends on the network the
    --      node is connected to. It can be obtained by inspecting the @epochLength@
    --      key in the @shelley-genesis.json@ file used by the node. On the preview
    --      network, the value is, at the time of writing, @86_400@, but it can
    --      change. This value and other genesis parameters can also be obtained
    --      using the 'QueryGenesisParameters' query.
    --    * __Network identifier__. When connecting to a testnet, the network identifier
    --      is also required. It can be obtained by looking for the @networkId@
    --      obtained by looking up the @networkMagic@ key in the @shelley-genesis.json@
    --      file that the node uses to connect to the network. For the preview
    --      network, the current identifier is @2@.
    --    * __Socket path__. The path to the node's socket file. It can be set using
    --      the @--socket-path@ parameter when starting the node. By default, it is
    --      typically located in the @db@ subfolder of the node's working directory.
    --
    --  Then, combine all the required information into a 'LocalNodeConnectInfo' value.
    --
    --  For example, assume the node is connected to the preview network and the socket
    --  file is located at @\/home\/user\/cardano-node\/db\/node.socket@. The
    --  'LocalNodeConnectInfo' value can then be constructed as follows:
    --
    -- @
    -- let connectionInfo =
    --       Api.LocalNodeConnectInfo
    --          { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 86_400)
    --          , Api.localNodeNetworkId = Api.Testnet (Api.NetworkMagic 2)
    --          , Api.localNodeSocketPath = Api.File "\/home\/user\/cardano-node\/db\/node.socket"
    --          }
    -- @

    -- ** Querying the node for the UTXO set

    -- | To obtain the set of unspent transaction outputs (UTXO set) from the network,
    -- the current era must first be determined.

    -- *** Obtaining the current era

    -- | Many queries require knowing the current era. This can be hardcoded using one of
    -- the 'ShelleyBasedEra' constructors, but it is also possible to retreive it from
    -- the node:
    --
    -- @
    -- eEra <- runExceptT $ Api.queryNodeLocalState connectionInfo Network.VolatileTip Api.QueryCurrentEra
    -- @
    --
    -- 'VolatileTip' requests information from the most recent block the node is aware of.
    -- It is important to note that this information is not guaranteed to remain valid, as
    -- it may be rolled back.
    --
    -- Alternatively, 'ImmutableTip' can be used to obtain information from the most recent
    -- block considered as final by the consensus algorithm. While this data is stable and will
    -- not be rolled back, it is less recent – on mainnet, it is typically about 36 hours
    -- behind the current time.
    --
    -- 'QueryCurrentEra' is the constructor of the query that retrieves the node's current
    -- era.
    --
    -- The query returns an 'ExceptT' monad, which can be run using the 'runExceptT'
    -- function. This yields an @eEra@ value of type @Either AcquiringFailure AnyCardanoEra@.
    --
    -- Below is an example of how to unwrap this value into a 'ShelleyBasedEra' based era, assuming the node
    -- is not running Byron:
    --
    -- @
    -- Api.AnyShelleyBasedEra sbe :: Api.AnyShelleyBasedEra <- case eEra of
    --   Right (Api.AnyCardanoEra era) ->
    --     Api.caseByronOrShelleyBasedEra
    --       (error "Error, we are in Byron era")
    --       (return . Api.AnyShelleyBasedEra)
    --       era
    --   Left Shelley.AFPointTooOld -> error "Error, point queried in the chain is too old!"
    --   Left Shelley.AFPointNotOnChain -> error "Error, point queried is not on chain!"
    -- @
    --
    -- 'AFPointTooOld' and 'AFPointNotOnChain' errors should not occur when querying with
    -- either 'VolatileTip' or 'ImmutableTip'.

    -- *** Obtaining the UTXO set

    -- | After determining the current era, the node can be queried for the UTXO set using
    -- the 'QueryUTxO' query as follows:
    --
    -- @
    -- eUtxo <-
    --   runExceptT $
    --     Api.queryNodeLocalState
    --       connectionInfo
    --       Network.VolatileTip
    --       (Api.QueryInEra (Api.QueryInShelleyBasedEra sbe (Api.QueryUTxO Api.QueryUTxOWhole)))
    -- @
    --
    -- This returns, a nested type of @Either AcquiringFailure (Either EraMismatch (UTXO era))@.
    -- You can unwrap it as follows:
    --
    -- @
    -- utxo <- case eUtxo of
    --   Right (Right (Api.UTxO utxo)) -> do
    --     return utxo
    --   Right (Left (Consensus.EraMismatch{Consensus.ledgerEraName, Consensus.otherEraName})) ->
    --     error
    --       ( "Error, we assumed era was "
    --           ++ show otherEraName
    --           ++ " but it was "
    --           ++ show ledgerEraName
    --       )
    --   Left Shelley.AFPointTooOld -> error "Error, point queried in the chain is too old!"
    --   Left Shelley.AFPointNotOnChain -> error "Error, point queried is not on chain!"
    -- @
    --
    -- Alternatively, to avoid nested result types, you can use convenience
    -- functions and types from "Cardano.Api.Query.Internal.Convenience".
    -- It is also posible to combine several queries into a single connection by using
    -- the monadic interface that can be found in the "Cardano.Api.Network.IPC.Internal.Monad"
    -- documentation.
    --
    -- The obtained @utxo@ variable is a standard @Map@ of type @Map TxIn (TxOut CtxUTxO era)@.

    -- ** Submitting a transaction

    -- | Assume there is a signed transaction in the latest era that you would like to submit
    -- to the node. Assume it is stored in the variable @signedTx@ of type @Tx era@.
    --
    -- For details on how to create such a transaction, see the "Cardano.Api.Tx.Internal.Body"
    -- documentation.
    --
    -- To submit the transaction to the node, use the 'submitTxToNodeLocal' function as follows:
    --
    -- @
    -- result <- Api.submitTxToNodeLocal connectionInfo (Api.TxInMode sbe signedTx)
    -- @
    --
    -- The result is a 'SubmitResult' value, which can be inspected as follows:
    --
    -- @
    -- case result of
    --   Api.SubmitSuccess -> putStrLn "Transaction submitted successfully!"
    --   Api.SubmitFail reason -> error $ "Error submitting transaction: " ++ show reason
    -- @
    --
    -- If the command succeeds, the transaction gets into the node's mempool, ready
    -- to be included in a block.

    -- * Node interaction

    -- | Operations that involve talking to a local Cardano node.
    connectToLocalNode
  , connectToLocalNodeWithVersion
  , LocalNodeConnectInfo (..)
  , LocalNodeClientParams (..)
  , mkLocalNodeClientParams
  , LocalNodeClientProtocols (..)
  , LocalChainSyncClient (..)
  , LocalNodeClientProtocolsInMode

    -- *** Chain sync protocol
  , ChainSyncClient (..)
  , ChainSyncClientPipelined (..)
  , BlockInMode (..)

    -- *** Local tx submission
  , LocalTxSubmissionClient (..)
  , TxInMode (..)
  , TxValidationErrorInCardanoMode
  , TxValidationError
  , submitTxToNodeLocal
  , SubmitResult (..)

    -- *** Local state query
  , LocalStateQueryClient (..)
  , AcquiringFailure (..)
  , QueryInMode (..)
  , QueryInEra (..)
  , QueryInShelleyBasedEra (..)
  , queryNodeLocalState

    -- **** Query monad
  , LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr

    -- *** Local tx monitoring
  , LocalTxMonitorClient (..)
  , LocalTxMonitoringQuery (..)
  , LocalTxMonitoringResult (..)
  , MempoolSizeAndCapacity (..)
  , queryTxMonitoringLocal
  , EraHistory (..)
  , getProgress

    -- *** Common queries
  , getLocalChainTip

    -- *** Query versioning
  , isQuerySupportedInNtcVersion
  , NodeToClientVersion (..)

    -- *** Error types
  , UnsupportedNtcVersionError (..)

    -- *** Helpers
  , toAcquiringFailure
  )
where

import Cardano.Api.Network.IPC.Internal
import Cardano.Api.Network.IPC.Internal.Monad
import Cardano.Api.Network.IPC.Internal.Version
