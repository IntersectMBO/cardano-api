{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.LedgerState.Fold.FoldBlocks
  ( foldBlocks
  ) where

import           Cardano.Api.Block
import           Cardano.Api.IO
import           Cardano.Api.IPC (ConsensusModeParams (..),
                   LocalChainSyncClient (LocalChainSyncClientPipelined),
                   LocalNodeClientProtocols (..), LocalNodeClientProtocolsInMode,
                   LocalNodeConnectInfo (..), connectToLocalNode)
import           Cardano.Api.LedgerEvents.ConvertLedgerEvent
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

import           Control.DeepSeq
import           Data.Foldable
import           Data.IORef
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.SOP.Strict (NP (..))
import           Data.Word
import           Network.TypedProtocol.Pipelined (Nat (..))

-- | Monadic fold over all blocks and ledger states. Stopping @k@ blocks before
-- the node's tip where @k@ is the security parameter.
foldBlocks :: forall a t m. ()
  => Show a
  => MonadIOTransError FoldBlocksError t m
  => NodeConfigFile 'In
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> SocketPath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> ValidationMode
  -> a
  -- ^ The initial accumulator state.
  -> (Env -> LedgerState -> [LedgerEvent] -> BlockInMode -> a -> IO (a, FoldStatus))
  -- ^ Accumulator function Takes:
  --
  --  * Environment (this is a constant over the whole fold).
  --  * The Ledger state (with block @i@ applied) at block @i@.
  --  * The Ledger events resulting from applying block @i@.
  --  * Block @i@.
  --  * The accumulator state at block @i - 1@.
  --
  -- And returns:
  --
  --  * The accumulator state at block @i@
  --  * A type indicating whether to stop or continue folding.
  --
  -- Note: This function can safely assume no rollback will occur even though
  -- internally this is implemented with a client protocol that may require
  -- rollback. This is achieved by only calling the accumulator on states/blocks
  -- that are older than the security parameter, k. This has the side effect of
  -- truncating the last k blocks before the node's tip.
  -> t m a
  -- ^ The final state
foldBlocks nodeConfigFilePath socketPath validationMode state0 accumulate = do
  foldBlocks' nodeConfigFilePath socketPath validationMode state0 accumulate

-- | Monadic fold over all blocks and ledger states. Stopping @k@ blocks before
-- the node's tip where @k@ is the security parameter.
foldBlocks' :: forall a t m. ()
  => Show a
  => MonadIOTransError FoldBlocksError t m
  => NodeConfigFile 'In
  -> SocketPath
  -> ValidationMode
  -> a
  -> (Env -> LedgerState -> [LedgerEvent] -> BlockInMode -> a -> IO (a, FoldStatus))
  -> t m a
foldBlocks' nodeConfigFilePath socketPath validationMode state0 accumulate = handleFoldBlocksIOExceptions $ do
  -- NOTE this was originally implemented with a non-pipelined client then
  -- changed to a pipelined client for a modest speedup:
  --  * Non-pipelined: 1h  0m  19s
  --  * Pipelined:        46m  23s

  (env, ledgerState) <- modifyError FoldBlocksInitialLedgerStateError $ initialLedgerState nodeConfigFilePath

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  errorIORef <- liftIO $ newIORef Nothing
  stateIORef <- liftIO $ newIORef state0

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
    (protocols stateIORef errorIORef env ledgerState)

  liftIO (readIORef errorIORef) >>= \case
    Just err -> throwError $ FoldBlocksApplyBlockError err
    Nothing -> liftIO $ readIORef stateIORef
  where
    protocols :: ()
      => IORef a
      -> IORef (Maybe LedgerStateError)
      -> Env
      -> LedgerState
      -> LocalNodeClientProtocolsInMode
    protocols stateIORef errorIORef env ledgerState =
        LocalNodeClientProtocols {
          localChainSyncClient    = LocalChainSyncClientPipelined (chainSyncClient 50 stateIORef errorIORef env ledgerState),
          localTxSubmissionClient = Nothing,
          localStateQueryClient   = Nothing,
          localTxMonitoringClient = Nothing
        }

    -- | Defines the client side of the chain sync protocol.
    chainSyncClient :: Word16
                    -- ^ The maximum number of concurrent requests.
                    -> IORef a
                    -- ^ State accumulator. Written to on every block.
                    -> IORef (Maybe LedgerStateError)
                    -- ^ Resulting error if any. Written to once on protocol
                    -- completion.
                    -> Env
                    -> LedgerState
                    -> CSP.ChainSyncClientPipelined BlockInMode ChainPoint ChainTip IO ()
                    -- ^ Client returns maybe an error.
    chainSyncClient pipelineSize stateIORef errorIORef env ledgerState0
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
                CSP.recvMsgRollForward = \blockInMode@(BlockInMode _ block@(Block (BlockHeader slotNo _ currBlockNo) _)) serverChainTip -> do
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
                    Right newLedgerState -> do
                      let (knownLedgerStates', committedStates) = pushLedgerState env knownLedgerStates slotNo newLedgerState blockInMode
                          newClientTip = At currBlockNo
                          newServerTip = fromChainTip serverChainTip

                          ledgerStateSingleFold
                            :: (SlotNo, (LedgerState, [LedgerEvent]), WithOrigin BlockInMode) -- Ledger events for a single block
                            -> IO FoldStatus
                          ledgerStateSingleFold (_, _, Origin) = return ContinueFold
                          ledgerStateSingleFold (_, (ledgerState, ledgerEvents), At currBlock) = do
                            accumulatorState <- readIORef stateIORef
                            (newState, foldStatus) <- accumulate
                                                      env
                                                      ledgerState
                                                      ledgerEvents
                                                      currBlock
                                                      accumulatorState
                            atomicWriteIORef stateIORef newState
                            return foldStatus

                          ledgerStateRecurser
                            :: Seq (SlotNo, LedgerStateEvents, WithOrigin BlockInMode) -- Ledger events for multiple blocks
                            -> IO FoldStatus
                          ledgerStateRecurser states = go (toList states) ContinueFold
                            where
                              go [] foldStatus = return foldStatus
                              go (s : rest) ContinueFold = do
                                newFoldStatus <- ledgerStateSingleFold s
                                go rest newFoldStatus
                              go _          StopFold    = go [] StopFold
                              go _          DebugFold   = go [] DebugFold

                      -- NB: knownLedgerStates' is the new ledger state history i.e k blocks from the tip
                      -- or also known as the mutable blocks. We default to using the mutable blocks.
                      finalFoldStatus <- ledgerStateRecurser knownLedgerStates'

                      case finalFoldStatus of
                        StopFold ->
                          -- We return StopFold in our accumulate function if we want to terminate the fold.
                          -- This allow us to check for a specific condition in our accumulate function
                          -- and then terminate e.g a specific stake pool was registered
                          let noError = Nothing
                          in clientIdle_DoneNwithMaybeError n noError

                        DebugFold -> do
                          currentIORefState <- readIORef stateIORef

                          -- Useful for debugging:
                          let !ioRefErr = DebugError . force
                                            $ unlines [ "newClientTip: " <> show newClientTip
                                                      , "newServerTip: " <> show newServerTip
                                                      , "newLedgerState: " <> show (snd newLedgerState)
                                                      , "knownLedgerStates: " <> show (extractHistory knownLedgerStates)
                                                      , "committedStates: " <> show (extractHistory committedStates)
                                                      , "numberOfRequestsInFlight: " <> show n
                                                      , "k: " <> show (envSecurityParam env)
                                                      , "Current IORef State: " <> show currentIORefState
                                                      ]
                          clientIdle_DoneNwithMaybeError n $ Just ioRefErr

                        ContinueFold -> return $ clientIdle_RequestMoreN newClientTip newServerTip n knownLedgerStates'

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
            Succ predN -> return (CSP.CollectResponse Nothing (clientNext_DoneNwithMaybeError predN errorMay)) -- Ignore remaining message responses
            Zero -> do
              writeIORef errorIORef errorMay
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
