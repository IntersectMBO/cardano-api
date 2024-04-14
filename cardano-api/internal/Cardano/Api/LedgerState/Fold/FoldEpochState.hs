{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LedgerState.Fold.FoldEpochState
  ( foldEpochState
  ) where

import           Cardano.Api.Block
import           Cardano.Api.Eras.Core (forEraMaybeEon)
import           Cardano.Api.IO
import           Cardano.Api.IPC (ConsensusModeParams (..),
                   LocalChainSyncClient (LocalChainSyncClientPipelined),
                   LocalNodeClientProtocols (..), LocalNodeClientProtocolsInMode,
                   LocalNodeConnectInfo (..), connectToLocalNode)
import           Cardano.Api.LedgerState.Core
import           Cardano.Api.LedgerState.Fold.Core
import           Cardano.Api.Modes (EpochSlots (..))
import           Cardano.Api.Monad.Error
import           Cardano.Api.NetworkId (NetworkId (..), NetworkMagic (NetworkMagic))

import qualified Cardano.Chain.Genesis
import           Cardano.Crypto (ProtocolMagicId (unProtocolMagicId), RequiresNetworkMagic (..))
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as HFC
import qualified Ouroboros.Network.Protocol.ChainSync.ClientPipelined as CSP
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision

import           Control.Concurrent
import           Control.Exception.Safe
import           Control.Monad.State.Strict
import           Data.IORef
import qualified Data.Sequence as Seq
import           Data.SOP.Strict (NP (..))
import           Data.Word
import           Network.TypedProtocol.Pipelined (Nat (..))

-- | Reconstructs the ledger's new epoch state and applies a supplied condition to it for every block. This
-- function only terminates if the condition is met or we have reached the termination epoch. We need to
-- provide a termination epoch otherwise blocks would be applied indefinitely.
foldEpochState
  :: forall t m a. ()
  => MonadIOTransError FoldBlocksError t m
  => NodeConfigFile 'In
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> SocketPath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> ValidationMode
  -> EpochNo
  -- ^ Termination epoch
  -> a
  -- ^ an initial value for the condition state
  -> ( AnyNewEpochState
         -> SlotNo
         -> BlockNo
         -> StateT a IO LedgerStateCondition
     )
  -- ^ Condition you want to check against the new epoch state.
  --
  --   'SlotNo' - Current (not necessarily the tip) slot number
  --
  --   'BlockNo' - Current (not necessarily the tip) block number
  --
  -- Note: This function can safely assume no rollback will occur even though
  -- internally this is implemented with a client protocol that may require
  -- rollback. This is achieved by only calling the accumulator on states/blocks
  -- that are older than the security parameter, k. This has the side effect of
  -- truncating the last k blocks before the node's tip.
  -> t m (LedgerStateCondition, a)
  -- ^ The final state
foldEpochState nodeConfigFilePath socketPath validationMode terminationEpoch initialResult checkCondition = handleFoldBlocksIOExceptions $ do
  -- NOTE this was originally implemented with a non-pipelined client then
  -- changed to a pipelined client for a modest speedup:
  --  * Non-pipelined: 1h  0m  19s
  --  * Pipelined:        46m  23s

  (env, ledgerState) <- modifyError FoldBlocksInitialLedgerStateError $ initialLedgerState nodeConfigFilePath

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  errorIORef <- liftIO $ newIORef Nothing

  -- This needs to be a full MVar by default. It serves as a mutual exclusion lock when executing
  -- 'checkCondition' to ensure that states 's' are processed in order. This is assured by MVar fairness.
  stateMv <- liftIO $ newMVar (ConditionNotMet, initialResult)

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig
        = (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
        . HFC.getPerEraLedgerConfig
        . HFC.hardForkLedgerConfigPerEra
        $ envLedgerConfig env

      networkMagic
        = NetworkMagic
        $ unProtocolMagicId
        $ Cardano.Chain.Genesis.gdProtocolMagicId
        $ Cardano.Chain.Genesis.configGenesisData byronConfig

      networkId' = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic -> Testnet networkMagic

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo = LocalNodeConnectInfo
        { localConsensusModeParams  = cardanoModeParams
        , localNodeNetworkId        = networkId'
        , localNodeSocketPath       = socketPath
        }

  liftIO $ connectToLocalNode
    connectInfo
    (protocols stateMv errorIORef env ledgerState)

  liftIO (readIORef errorIORef) >>= \case
    Just err -> throwError $ FoldBlocksApplyBlockError err
    Nothing -> liftIO $ readMVar stateMv
  where
    protocols :: ()
      => MVar (LedgerStateCondition, a)
      -> IORef (Maybe LedgerStateError)
      -> Env
      -> LedgerState
      -> LocalNodeClientProtocolsInMode
    protocols stateMv errorIORef env ledgerState =
        LocalNodeClientProtocols {
          localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50 stateMv errorIORef env ledgerState),
          localTxSubmissionClient = Nothing,
          localStateQueryClient   = Nothing,
          localTxMonitoringClient = Nothing
        }

    -- | Defines the client side of the chain sync protocol.
    chainSyncClient :: Word16
                    -- ^ The maximum number of concurrent requests.
                    -> MVar (LedgerStateCondition, a)
                    -- ^ State accumulator. Written to on every block.
                    -> IORef (Maybe LedgerStateError)
                    -- ^ Resulting error if any. Written to once on protocol
                    -- completion.
                    -> Env
                    -> LedgerState
                    -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
                    -- ^ Client returns maybe an error.
    chainSyncClient pipelineSize stateMv errorIORef' env ledgerState0
      = CSP.ChainSyncClientPipelined $ pure $ clientIdle_RequestMoreN Origin Origin Zero initialLedgerStateHistory
      where
          initialLedgerStateHistory = Seq.singleton (0, (ledgerState0, []), Origin)

          clientIdle_RequestMoreN
            :: WithOrigin BlockNo
            -> WithOrigin BlockNo
            -> Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
          clientIdle_RequestMoreN clientTip serverTip n knownLedgerStates
            = case pipelineDecisionMax pipelineSize n clientTip serverTip  of
                Collect -> case n of
                  Succ predN -> CSP.CollectResponse Nothing (clientNextN predN knownLedgerStates)
                _ -> CSP.SendMsgRequestNextPipelined (pure ()) (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownLedgerStates)

          clientNextN
            :: Nat n -- Number of requests inflight.
            -> LedgerStateHistory
            -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
          clientNextN n knownLedgerStates =
            CSP.ClientStNext {
                CSP.recvMsgRollForward = \blockInMode@(BlockInMode era block@(Block (BlockHeader slotNo _ currBlockNo) _)) serverChainTip -> do
                  let newLedgerStateE = applyBlock
                        env
                        (maybe
                          (error "Impossible! Missing Ledger state")
                          (\(_,(ledgerState, _),_) -> ledgerState)
                          (Seq.lookup 0 knownLedgerStates)
                        )
                        validationMode
                        block

                  case newLedgerStateE of
                    Left err -> clientIdle_DoneNwithMaybeError n (Just err)
                    Right new@(newLedgerState, ledgerEvents) ->
                      case forEraMaybeEon era of
                        Nothing -> let !err = Just ByronEraUnsupported
                                  in clientIdle_DoneNwithMaybeError n err
                        Just sbe ->do
                          let (knownLedgerStates', _) = pushLedgerState env knownLedgerStates slotNo new blockInMode
                              newClientTip = At currBlockNo
                              newServerTip = fromChainTip serverChainTip
                          case getNewEpochState sbe $ clsState newLedgerState of
                            Left e ->
                              let !err = Just e
                              in clientIdle_DoneNwithMaybeError n err
                            Right lState -> do
                              let newEpochState = AnyNewEpochState sbe lState
                              -- Run the condition function in an exclusive lock.
                              -- There can be only one place where `takeMVar stateMv` exists otherwise this
                              -- code will deadlock!
                              condition <- bracket (takeMVar stateMv) (tryPutMVar stateMv) $ \(_prevCondition, previousState) -> do
                                updatedState@(!newCondition, !_) <- runStateT (checkCondition newEpochState slotNo currBlockNo) previousState
                                putMVar stateMv updatedState
                                pure newCondition
                              -- Have we reached the termination epoch?
                              case atTerminationEpoch terminationEpoch ledgerEvents of
                                Just !currentEpoch -> do
                                   -- confirmed this works: error $ "atTerminationEpoch: Terminated at: " <> show currentEpoch
                                   let !err = Just $ TerminationEpochReached currentEpoch
                                   clientIdle_DoneNwithMaybeError n err
                                Nothing -> do
                                  case condition of
                                    ConditionMet ->
                                       let !noError = Nothing
                                       in clientIdle_DoneNwithMaybeError n noError
                                    ConditionNotMet -> return $ clientIdle_RequestMoreN newClientTip newServerTip n knownLedgerStates'

              , CSP.recvMsgRollBackward = \chainPoint serverChainTip -> do
                  let newClientTip = Origin -- We don't actually keep track of blocks so we temporarily "forget" the tip.
                      newServerTip = fromChainTip serverChainTip
                      truncatedKnownLedgerStates = case chainPoint of
                          ChainPointAtGenesis -> initialLedgerStateHistory
                          ChainPoint slotNo _ -> rollBackLedgerStateHist knownLedgerStates slotNo
                  return (clientIdle_RequestMoreN newClientTip newServerTip n truncatedKnownLedgerStates)
              }

          clientIdle_DoneNwithMaybeError
            :: Nat n -- Number of requests inflight.
            -> Maybe LedgerStateError -- Return value (maybe an error)
            -> IO (CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ())
          clientIdle_DoneNwithMaybeError n errorMay = case n of
            Succ predN -> do
              return (CSP.CollectResponse Nothing (clientNext_DoneNwithMaybeError predN errorMay)) -- Ignore remaining message responses
            Zero -> do
              atomicModifyIORef' errorIORef' . const $ (errorMay, ())
              return (CSP.SendMsgDone ())

          clientNext_DoneNwithMaybeError
            :: Nat n -- Number of requests inflight.
            -> Maybe LedgerStateError -- Return value (maybe an error)
            -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
          clientNext_DoneNwithMaybeError n errorMay =
            CSP.ClientStNext {
                CSP.recvMsgRollForward = \_ _ -> clientIdle_DoneNwithMaybeError n errorMay
              , CSP.recvMsgRollBackward = \_ _ -> clientIdle_DoneNwithMaybeError n errorMay
              }

          fromChainTip :: ChainTip -> WithOrigin BlockNo
          fromChainTip ct = case ct of
            ChainTipAtGenesis -> Origin
            ChainTip _ _ bno -> At bno
