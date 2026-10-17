{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.LedgerState
  ( -- * Initialization / Accumulation
    envSecurityParam
  , LedgerState
    ( ..
    , LedgerStateByron
    , LedgerStateShelley
    , LedgerStateAllegra
    , LedgerStateMary
    , LedgerStateAlonzo
    , LedgerStateBabbage
    , LedgerStateConway
    )
  , encodeLedgerState
  , decodeLedgerState
  , initialLedgerState
  , applyBlock
  , ValidationMode (..)
  , applyBlockWithEvents
  , AnyNewEpochState (..)
  , getAnyNewEpochState
  , getLedgerTablesUTxOValues

    -- * Traversing the block chain
  , foldBlocks
  , FoldStatus (..)
  , chainSyncClientWithLedgerState
  , chainSyncClientPipelinedWithLedgerState

    -- * Ledger state conditions
  , ConditionResult (..)
  , fromConditionResult
  , toConditionResult
  , foldEpochState

    -- * Ledger events
  , LedgerEvent (..)
  , AnyProposals (..)
  , AnyRatificationState (..)
  , MIRDistributionDetails (..)
  , PoolReapDetails (..)
  , convertRetiredPoolsMap

    -- * Errors
  , LedgerStateError (..)
  , FoldBlocksError (..)
  , GenesisConfigError (..)
  , InitialLedgerStateError (..)
  , NodeConfigError (..)

    -- * Leadership schedule
  , LeadershipError (..)
  , constructGlobals
  , currentEpochEligibleLeadershipSlots
  , nextEpochEligibleLeadershipSlots

    -- * Node Config
  , NodeConfig (..)
  , ncPBftSignatureThreshold
  , ncByronGenesisFile
  , ncByronGenesisHash
  , ncShelleyGenesisFile
  , ncShelleyGenesisHash
  , ncAlonzoGenesisFile
  , ncAlonzoGenesisHash
  , ncConwayGenesisFile
  , ncConwayGenesisHash
  , ncRequiresNetworkMagic
  , ncByronProtocolVersion
  , ncHardForkTriggers

    -- ** Network Config
  , NodeConfigFile
  , readNodeConfig

    -- ** Genesis Config
  , GenesisConfig (..)
  , readCardanoGenesisConfig
  , mkProtocolInfoCardano
  , resolveShelleyInitialFunds

    -- *** Byron Genesis Config
  , readByronGenesisConfig

    -- *** Shelley Genesis Config
  , ShelleyConfig (..)
  , GenesisHashShelley (..)
  , readShelleyGenesisConfig
  , readShelleyGenesis
  , ShelleyGenesisError (..)
  , renderShelleyGenesisError
  , shelleyPraosNonce

    -- *** Alonzo Genesis Config
  , GenesisHashAlonzo (..)
  , readAlonzoGenesisConfig

    -- *** Conway Genesis Config
  , GenesisHashConway (..)
  , readConwayGenesisConfig

    -- ** Environment
  , Env (..)
  , genesisConfigToEnv
  )
where

import Cardano.Api.Block
import Cardano.Api.Byron.Internal.Proposal as Byron
import Cardano.Api.Certificate.Internal
import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Consensus.Internal.Mode qualified as Api
import Cardano.Api.Era.Internal.Core (forEraInEon, forEraMaybeEon, toCardanoEra)
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.ConwayEraOnwards (ConwayEraOnwards)
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Error as Api
import Cardano.Api.Genesis.Internal
import Cardano.Api.IO
import Cardano.Api.Key.Internal.Praos
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger
import Cardano.Api.LedgerState.Internal.ConvertLedgerEvent
import Cardano.Api.LedgerState.Internal.LedgerEvent
import Cardano.Api.Monad.Error
import Cardano.Api.Network (Serialised (..))
import Cardano.Api.Network.IPC.Internal
  ( LocalChainSyncClient (LocalChainSyncClientPipelined)
  , LocalNodeClientProtocols (..)
  , LocalNodeClientProtocolsInMode
  , LocalNodeConnectInfo (..)
  , connectToLocalNode
  )
import Cardano.Api.Network.Internal.NetworkId (NetworkId (..), NetworkMagic (NetworkMagic))
import Cardano.Api.Pretty
import Cardano.Api.Query.Internal.Type.QueryInMode
  ( CurrentEpochState (..)
  , PoolDistribution (unPoolDistr)
  , ProtocolState
  , SerialisedCurrentEpochState (..)
  , decodeCurrentEpochState
  , decodePoolDistribution
  , decodeProtocolState
  )
import Cardano.Api.Tx.Internal.Body

import Cardano.Binary qualified as CBOR
import Cardano.Chain.Genesis qualified
import Cardano.Chain.Update qualified
import Cardano.Configuration qualified as Cfg
import Cardano.Crypto (ProtocolMagicId (unProtocolMagicId), RequiresNetworkMagic (..))
import Cardano.Crypto.Hash.Blake2b qualified
import Cardano.Crypto.Hash.Class qualified
import Cardano.Crypto.ProtocolMagic qualified
import Cardano.Crypto.VRF qualified as Crypto
import Cardano.Crypto.VRF.Class qualified as VRF
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Api (Language (PlutusV4))
import Cardano.Ledger.Api.Era qualified as Ledger
import Cardano.Ledger.Api.Transition qualified as Ledger
import Cardano.Ledger.BaseTypes
  ( Globals (..)
  , Milliseconds32 (Milliseconds32)
  , Nonce
  , ProtVer (..)
  , boundRational
  , knownNonZeroBounded
  , natVersion
  , (⭒)
  )
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Binary (DecoderError)
import Cardano.Ledger.Coin qualified as Coin
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Dijkstra.PParams qualified as Ledger
import Cardano.Ledger.Dijkstra.Tx qualified as Ledger
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Keys qualified as SL
import Cardano.Ledger.Plutus (OrdExUnits (..))
import Cardano.Ledger.Shelley.API qualified as ShelleyAPI
import Cardano.Ledger.Shelley.Core qualified as Core
import Cardano.Ledger.Shelley.Genesis qualified as Ledger
import Cardano.Ledger.Shelley.Transition qualified as Ledger
import Cardano.Ledger.Slot qualified as Ledger
import Cardano.Ledger.State qualified as SL
import Cardano.Protocol.Crypto qualified as Crypto
import Cardano.Protocol.Praos.VRF (mkInputVRF, vrfLeaderValue)
import Cardano.Protocol.TPraos.API qualified as TPraos
import Cardano.Protocol.TPraos.BlockHeader (checkLeaderNatValue)
import Cardano.Protocol.TPraos.BlockHeader qualified as TPraos
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.EpochInfo.API qualified as Slot
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Cardano.Slotting.Slot qualified as Slot
import Ouroboros.Consensus.Block.Abstract qualified as Consensus
import Ouroboros.Consensus.Block.Forging (MkBlockForging (..))
import Ouroboros.Consensus.Byron.ByronHFC qualified as Consensus
import Ouroboros.Consensus.Byron.Ledger qualified as Byron
import Ouroboros.Consensus.Cardano qualified as Consensus
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node qualified as Consensus
import Ouroboros.Consensus.Config qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.Basics qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.Ledger qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common qualified as HFC
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.Ledger.Abstract qualified as Ledger
import Ouroboros.Consensus.Ledger.Extended qualified as Ledger
import Ouroboros.Consensus.Ledger.Tables.Utils qualified as Ledger
import Ouroboros.Consensus.Node.ProtocolInfo qualified as Consensus
import Ouroboros.Consensus.Protocol.Abstract (ChainDepState, ConsensusProtocol (..))
import Ouroboros.Consensus.Protocol.Praos qualified as Praos
import Ouroboros.Consensus.Protocol.Praos.AgentClient
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos2 qualified as Praos2
import Ouroboros.Consensus.Protocol.TPraos qualified as TPraos
import Ouroboros.Consensus.Shelley.HFEras qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Block qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.Ledger qualified as Shelley
import Ouroboros.Consensus.TypeFamilyWrappers (WrapLedgerEvent (WrapLedgerEvent))
import Ouroboros.Network.Block (blockNo)
import Ouroboros.Network.Block qualified
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision

import Control.Concurrent
import Control.DeepSeq
import Control.Error.Util (note)
import Control.Exception.Safe hiding (MonadThrow)
import Control.Monad
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.State.Strict
import Control.Tracer qualified as Tracer
import Data.Aeson as Aeson (eitherDecodeStrict')
import Data.Bifunctor
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short as BSS
import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Proxy (Proxy (Proxy))
import Data.SOP.BasicFunctors
import Data.SOP.Functors
import Data.SOP.Index
import Data.SOP.Strict
import Data.SOP.Telescope qualified as Telescope
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word
import Formatting.Buildable (build)
import GHC.Exts (IsList (..))
import GHC.Stack (HasCallStack)
import Lens.Micro
import Network.Mux qualified as Mux
import Network.TypedProtocol.Core (Nat (..))
import System.FS.API (SomeHasFS (..))
import System.FilePath

import Test.Cardano.Ledger.Plutus (testingCostModel)

data InitialLedgerStateError
  = -- | Failed to read or parse the network config file.
    ILSEConfigFile Text
  | -- | Failed to read or parse a genesis file linked from the network config file.
    ILSEGenesisFile GenesisConfigError
  | -- | Failed to derive the Ledger or Consensus config.
    ILSELedgerConsensusConfig GenesisConfigError
  deriving Show

instance Exception InitialLedgerStateError

instance Error InitialLedgerStateError where
  prettyError = \case
    ILSEConfigFile err ->
      "Failed to read or parse the network config file:" <+> pretty err
    ILSEGenesisFile err ->
      "Failed to read or parse a genesis file linked from the network config file:" <+> prettyError err
    ILSELedgerConsensusConfig err ->
      "Failed to derive the Ledger or Consensus config:" <+> prettyError err

data LedgerStateError
  = -- | When using QuickValidation, the block hash did not match the expected
    -- block hash after applying a new block to the current ledger state.
    ApplyBlockHashMismatch Text
  | -- | When using FullValidation, an error occurred when applying a new block
    -- to the current ledger state.
    ApplyBlockError (Consensus.CardanoLedgerError Consensus.StandardCrypto)
  | -- | Encountered a rollback larger than the security parameter.
    InvalidRollback
      SlotNo
      -- ^ Oldest known slot number that we can roll back to.
      ChainPoint
      -- ^ Rollback was attempted to this point.
  | -- | The ledger state condition you were interested in was not met
    -- prior to the termination epoch.
    TerminationEpochReached EpochNo
  | UnexpectedLedgerState
      AnyShelleyBasedEra
      -- ^ Expected era
      ( NS
          (Current (Flip Consensus.LedgerState Ledger.EmptyMK))
          (Consensus.CardanoEras Consensus.StandardCrypto)
      )
      -- ^ Ledgerstate from an unexpected era
  | ByronEraUnsupported
  | DebugError !String
  deriving Show

instance Exception LedgerStateError

instance Error LedgerStateError where
  prettyError = \case
    DebugError e -> pretty e
    ApplyBlockHashMismatch err -> "Applying a block did not result in the expected block hash:" <+> pretty err
    ApplyBlockError hardForkLedgerError -> "Applying a block resulted in an error:" <+> pshow hardForkLedgerError
    InvalidRollback oldestSupported rollbackPoint ->
      "Encountered a rollback larger than the security parameter. Attempted to roll back to"
        <+> pshow rollbackPoint
        <> ", but oldest supported slot is"
          <+> pshow oldestSupported
    TerminationEpochReached epochNo ->
      mconcat
        [ "The ledger state condition you were interested in was not met "
        , "prior to the termination epoch:" <+> pshow epochNo
        ]
    UnexpectedLedgerState expectedEra unexpectedLS ->
      mconcat
        [ "Expected ledger state from the "
        , pshow expectedEra
        , " era, but got "
        , pshow unexpectedLS
        ]
    ByronEraUnsupported -> "Byron era is not supported"

-- | Get the environment and initial ledger state.
initialLedgerState
  :: MonadIOTransError InitialLedgerStateError t m
  => SomeHasFS IO
  -- ^ Filesystem capability used by Ledger for testing/benchmarking,
  -- to stream Shelley/Conway genesis data from the extraConfig.
  -> NodeConfigFile 'In
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> t m (Env, LedgerState)
  -- ^ The environment and initial ledger state
initialLedgerState fs nodeConfigFile = do
  -- TODO Once support for querying the ledger config is added to the node, we
  -- can remove the nodeConfigFile argument and much of the code in this
  -- module.
  (config, _warnings) <-
    modifyError (ILSEConfigFile . docToText . prettyError) (readNodeConfig nodeConfigFile)
  genesisConfig <- modifyError ILSEGenesisFile (readCardanoGenesisConfig config)
  env <- hoistIOEither (first ILSELedgerConsensusConfig <$> genesisConfigToEnv fs genesisConfig)
  ledgerState <- liftIO (initLedgerStateVar fs genesisConfig)
  return (env, ledgerState)

-- | Apply a single block to the current ledger state.
applyBlock
  :: Env
  -- ^ The environment returned by @initialLedgerState@
  -> LedgerState
  -- ^ The current ledger state
  -> ValidationMode
  -> BlockInMode
  -- ^ Some block to apply
  -> Either LedgerStateError (LedgerState, [LedgerEvent])
  -- ^ The new ledger state (or an error).
applyBlock env oldState validationMode =
  applyBlock' env oldState validationMode . toConsensusBlock

{-# DEPRECATED LedgerStateByron "Use 'LedgerState $ Consensus.LedgerStateByron' instead" #-}
pattern LedgerStateByron
  :: Ledger.LedgerState Byron.ByronBlock Ledger.EmptyMK
  -> LedgerState
pattern LedgerStateByron st <- LedgerState (Consensus.LedgerStateByron st) _

{-# DEPRECATED LedgerStateShelley "Use 'LedgerState $ Consensus.LedgerStateShelley' instead" #-}
pattern LedgerStateShelley
  :: Ledger.LedgerState Shelley.StandardShelleyBlock Ledger.EmptyMK
  -> LedgerState
pattern LedgerStateShelley st <- LedgerState (Consensus.LedgerStateShelley st) _

{-# DEPRECATED LedgerStateAllegra "Use 'LedgerState $ Consensus.LedgerStateAllegra' instead" #-}
pattern LedgerStateAllegra
  :: Ledger.LedgerState Shelley.StandardAllegraBlock Ledger.EmptyMK
  -> LedgerState
pattern LedgerStateAllegra st <- LedgerState (Consensus.LedgerStateAllegra st) _

{-# DEPRECATED LedgerStateMary "Use 'LedgerState $ Consensus.LedgerStateMary' instead" #-}
pattern LedgerStateMary
  :: Ledger.LedgerState Shelley.StandardMaryBlock Ledger.EmptyMK
  -> LedgerState
pattern LedgerStateMary st <- LedgerState (Consensus.LedgerStateMary st) _

{-# DEPRECATED LedgerStateAlonzo "Use 'LedgerState $ Consensus.LedgerAlonzo' instead" #-}
pattern LedgerStateAlonzo
  :: Ledger.LedgerState Shelley.StandardAlonzoBlock Ledger.EmptyMK
  -> LedgerState
pattern LedgerStateAlonzo st <- LedgerState (Consensus.LedgerStateAlonzo st) _

{-# DEPRECATED LedgerStateBabbage "Use 'LedgerState $ Consensus.LedgerBabbage' instead" #-}
pattern LedgerStateBabbage
  :: Ledger.LedgerState Shelley.StandardBabbageBlock Ledger.EmptyMK
  -> LedgerState
pattern LedgerStateBabbage st <- LedgerState (Consensus.LedgerStateBabbage st) _

{-# DEPRECATED LedgerStateConway "Use 'LedgerState $ Consensus.LedgerConway' instead" #-}
pattern LedgerStateConway
  :: Ledger.LedgerState Shelley.StandardConwayBlock Ledger.EmptyMK
  -> LedgerState
pattern LedgerStateConway st <- LedgerState (Consensus.LedgerStateConway st) _

{-# COMPLETE
  LedgerStateByron
  , LedgerStateShelley
  , LedgerStateAllegra
  , LedgerStateMary
  , LedgerStateAlonzo
  , LedgerStateBabbage
  , LedgerStateConway
  #-}

data FoldBlocksError
  = FoldBlocksInitialLedgerStateError !InitialLedgerStateError
  | FoldBlocksApplyBlockError !LedgerStateError
  | FoldBlocksIOException !IOException
  | FoldBlocksMuxError !Mux.Error
  deriving Show

instance Error FoldBlocksError where
  prettyError = \case
    FoldBlocksInitialLedgerStateError err -> prettyError err
    FoldBlocksApplyBlockError err -> "Failed when applying a block:" <+> prettyError err
    FoldBlocksIOException err -> "IOException:" <+> prettyException err
    FoldBlocksMuxError err -> "FoldBlocks error:" <+> prettyException err

-- | Type that lets us decide whether to continue or stop
-- the fold from within our accumulation function.
data FoldStatus
  = ContinueFold
  | StopFold
  | DebugFold
  deriving (Show, Eq)

-- | Monadic fold over all blocks and ledger states. Stopping @k@ blocks before
-- the node's tip where @k@ is the security parameter.
foldBlocks
  :: forall a t m
   . HasCallStack
  => Show a
  => MonadIOTransError FoldBlocksError t m
  => SomeHasFS IO
  -- ^ Filesystem capability used by Ledger for testing/benchmarking,
  -- to stream Shelley/Conway genesis data from the extraConfig.
  -> NodeConfigFile 'In
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
foldBlocks fs nodeConfigFilePath socketPath validationMode state0 accumulate = handleExceptions $ do
  -- NOTE this was originally implemented with a non-pipelined client then
  -- changed to a pipelined client for a modest speedup:
  --  * Non-pipelined: 1h  0m  19s
  --  * Pipelined:        46m  23s

  (env, ledgerState) <-
    modifyError FoldBlocksInitialLedgerStateError $ initialLedgerState fs nodeConfigFilePath

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  errorIORef <- liftIO $ newIORef Nothing
  stateIORef <- liftIO $ newIORef state0

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig =
        (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
          . HFC.getPerEraLedgerConfig
          . HFC.hardForkLedgerConfigPerEra
          $ envLedgerConfig env

      networkMagic =
        NetworkMagic $
          unProtocolMagicId $
            Cardano.Chain.Genesis.gdProtocolMagicId $
              Cardano.Chain.Genesis.configGenesisData byronConfig

      networkId' = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic -> Testnet networkMagic

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = cardanoModeParams
          , localNodeNetworkId = networkId'
          , localNodeSocketPath = socketPath
          }

  liftIO $
    connectToLocalNode
      connectInfo
      (protocols stateIORef errorIORef env ledgerState)

  liftIO (readIORef errorIORef) >>= \case
    Just err -> throwError (FoldBlocksApplyBlockError err)
    Nothing -> liftIO $ readIORef stateIORef
 where
  protocols
    :: ()
    => IORef a
    -> IORef (Maybe LedgerStateError)
    -> Env
    -> LedgerState
    -> LocalNodeClientProtocolsInMode
  protocols stateIORef errorIORef env ledgerState =
    LocalNodeClientProtocols
      { localChainSyncClient =
          LocalChainSyncClientPipelined (chainSyncClient 50 stateIORef errorIORef env ledgerState)
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

  -- \| Defines the client side of the chain sync protocol.
  chainSyncClient
    :: Word16
    -- \^ The maximum number of concurrent requests.
    -> IORef a
    -- \^ State accumulator. Written to on every block.
    -> IORef (Maybe LedgerStateError)
    -- \^ Resulting error if any. Written to once on protocol
    -- completion.
    -> Env
    -> LedgerState
    -> CSP.ChainSyncClientPipelined
         BlockInMode
         ChainPoint
         ChainTip
         IO
         ()
  -- \^ Client returns maybe an error.
  chainSyncClient pipelineSize stateIORef errorIORef env ledgerState0 =
    CSP.ChainSyncClientPipelined $
      pure $
        clientIdle_RequestMoreN Origin Origin Zero initialLedgerStateHistory
   where
    initialLedgerStateHistory = Seq.singleton (0, (ledgerState0, []), Origin)

    clientIdle_RequestMoreN
      :: WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> Nat n -- Number of requests inflight.
      -> LedgerStateHistory
      -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip serverTip n knownLedgerStates =
      case pipelineDecisionMax pipelineSize n clientTip serverTip of
        Collect -> case n of
          Succ predN -> CSP.CollectResponse Nothing (clientNextN predN knownLedgerStates)
        _ ->
          CSP.SendMsgRequestNextPipelined
            (pure ())
            (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownLedgerStates)

    clientNextN
      :: Nat n -- Number of requests inflight.
      -> LedgerStateHistory
      -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
    clientNextN n knownLedgerStates =
      CSP.ClientStNext
        { CSP.recvMsgRollForward = \blockInMode@(BlockInMode _ block) serverChainTip -> do
            let newLedgerStateE =
                  applyBlock
                    env
                    ( maybe
                        (error "Impossible! Missing Ledger state")
                        (\(_, (ledgerState, _), _) -> ledgerState)
                        (Seq.lookup 0 knownLedgerStates)
                    )
                    validationMode
                    blockInMode
            case newLedgerStateE of
              Left err -> clientIdle_DoneNwithMaybeError n (Just err)
              Right newLedgerState -> do
                let BlockHeader slotNo _ currBlockNo = getBlockHeader block
                    (knownLedgerStates', committedStates) = pushLedgerState env knownLedgerStates slotNo newLedgerState blockInMode
                    newClientTip = At currBlockNo
                    newServerTip = fromChainTip serverChainTip

                    ledgerStateSingleFold
                      :: (SlotNo, (LedgerState, [LedgerEvent]), WithOrigin BlockInMode) -- Ledger events for a single block
                      -> IO FoldStatus
                    ledgerStateSingleFold (_, _, Origin) = return ContinueFold
                    ledgerStateSingleFold (_, (ledgerState, ledgerEvents), At currBlock) = do
                      accumulatorState <- readIORef stateIORef
                      (newState, foldStatus) <-
                        accumulate
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
                      go _ StopFold = go [] StopFold
                      go _ DebugFold = go [] DebugFold

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
                    let !ioRefErr =
                          DebugError . force $
                            unlines
                              [ "newClientTip: " <> show newClientTip
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
      CSP.ClientStNext
        { CSP.recvMsgRollForward = \_ _ -> clientIdle_DoneNwithMaybeError n errorMay
        , CSP.recvMsgRollBackward = \_ _ -> clientIdle_DoneNwithMaybeError n errorMay
        }

    fromChainTip :: ChainTip -> WithOrigin BlockNo
    fromChainTip ct = case ct of
      ChainTipAtGenesis -> Origin
      ChainTip _ _ bno -> At bno

-- | Wrap a 'ChainSyncClient' with logic that tracks the ledger state.
chainSyncClientWithLedgerState
  :: forall m a
   . HasCallStack
  => Monad m
  => Env
  -> LedgerState
  -- ^ Initial ledger state
  -> ValidationMode
  -> CS.ChainSyncClient
       (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
       ChainPoint
       ChainTip
       m
       a
  -- ^ A client to wrap. The block is annotated with a 'Either LedgerStateError
  -- LedgerState'. This is either an error from validating a block or
  -- the current 'LedgerState' from applying the current block. If we
  -- trust the node, then we generally expect blocks to validate. Also note that
  -- after a block fails to validate we may still roll back to a validated
  -- block, in which case the valid 'LedgerState' will be passed here again.
  -> CS.ChainSyncClient
       BlockInMode
       ChainPoint
       ChainTip
       m
       a
  -- ^ A client that acts just like the wrapped client but doesn't require the
  -- 'LedgerState' annotation on the block type.
chainSyncClientWithLedgerState env ledgerState0 validationMode (CS.ChainSyncClient clientTop) =
  CS.ChainSyncClient (goClientStIdle (Right initialLedgerStateHistory) <$> clientTop)
 where
  goClientStIdle
    :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
    -> CS.ClientStIdle
         (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
         ChainPoint
         ChainTip
         m
         a
    -> CS.ClientStIdle
         BlockInMode
         ChainPoint
         ChainTip
         m
         a
  goClientStIdle history client = case client of
    CS.SendMsgRequestNext a b -> CS.SendMsgRequestNext a (goClientStNext history b)
    CS.SendMsgFindIntersect ps a -> CS.SendMsgFindIntersect ps (goClientStIntersect history a)
    CS.SendMsgDone a -> CS.SendMsgDone a

  -- This is where the magic happens. We intercept the blocks and rollbacks
  -- and use it to maintain the correct ledger state.
  goClientStNext
    :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
    -> CS.ClientStNext
         (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
         ChainPoint
         ChainTip
         m
         a
    -> CS.ClientStNext
         BlockInMode
         ChainPoint
         ChainTip
         m
         a
  goClientStNext (Left err) (CS.ClientStNext recvMsgRollForward recvMsgRollBackward) =
    CS.ClientStNext
      ( \blkInMode tip ->
          CS.ChainSyncClient $
            goClientStIdle (Left err)
              <$> CS.runChainSyncClient
                (recvMsgRollForward (blkInMode, Left err) tip)
      )
      ( \point tip ->
          CS.ChainSyncClient $
            goClientStIdle (Left err) <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )
  goClientStNext (Right history) (CS.ClientStNext recvMsgRollForward recvMsgRollBackward) =
    CS.ClientStNext
      ( \blkInMode@(BlockInMode _ block) tip ->
          CS.ChainSyncClient $
            let
              BlockHeader slotNo _ _ = getBlockHeader block
              newLedgerStateE = case Seq.lookup 0 history of
                Nothing -> error "Impossible! History should always be non-empty"
                Just (_, Left err, _) -> Left err
                Just (_, Right (oldLedgerState, _), _) ->
                  applyBlock
                    env
                    oldLedgerState
                    validationMode
                    blkInMode
              (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
             in
              goClientStIdle (Right history')
                <$> CS.runChainSyncClient
                  (recvMsgRollForward (blkInMode, newLedgerStateE) tip)
      )
      ( \point tip ->
          let
            oldestSlot = case history of
              _ Seq.:|> (s, _, _) -> s
              Seq.Empty -> error "Impossible! History should always be non-empty"
            history' = ( \h ->
                           if Seq.null h
                             then Left (InvalidRollback oldestSlot point)
                             else Right h
                       )
              $ case point of
                ChainPointAtGenesis -> initialLedgerStateHistory
                ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
           in
            CS.ChainSyncClient $
              goClientStIdle history' <$> CS.runChainSyncClient (recvMsgRollBackward point tip)
      )

  goClientStIntersect
    :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
    -> CS.ClientStIntersect
         (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
         ChainPoint
         ChainTip
         m
         a
    -> CS.ClientStIntersect
         BlockInMode
         ChainPoint
         ChainTip
         m
         a
  goClientStIntersect history (CS.ClientStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) =
    CS.ClientStIntersect
      ( \point tip ->
          CS.ChainSyncClient
            (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectFound point tip))
      )
      ( \tip ->
          CS.ChainSyncClient (goClientStIdle history <$> CS.runChainSyncClient (recvMsgIntersectNotFound tip))
      )

  initialLedgerStateHistory :: History (Either LedgerStateError LedgerStateEvents)
  initialLedgerStateHistory = Seq.singleton (0, Right (ledgerState0, []), Origin)

-- | See 'chainSyncClientWithLedgerState'.
chainSyncClientPipelinedWithLedgerState
  :: forall m a
   . HasCallStack
  => Monad m
  => Env
  -> LedgerState
  -> ValidationMode
  -> CSP.ChainSyncClientPipelined
       (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
       ChainPoint
       ChainTip
       m
       a
  -> CSP.ChainSyncClientPipelined
       BlockInMode
       ChainPoint
       ChainTip
       m
       a
chainSyncClientPipelinedWithLedgerState env ledgerState0 validationMode (CSP.ChainSyncClientPipelined clientTop) =
  CSP.ChainSyncClientPipelined
    (goClientPipelinedStIdle (Right initialLedgerStateHistory) Zero <$> clientTop)
 where
  goClientPipelinedStIdle
    :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
    -> Nat n
    -> CSP.ClientPipelinedStIdle
         n
         (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
         ChainPoint
         ChainTip
         m
         a
    -> CSP.ClientPipelinedStIdle
         n
         BlockInMode
         ChainPoint
         ChainTip
         m
         a
  goClientPipelinedStIdle history n client = case client of
    CSP.SendMsgRequestNext a b -> CSP.SendMsgRequestNext a (goClientStNext history n b)
    CSP.SendMsgRequestNextPipelined m a -> CSP.SendMsgRequestNextPipelined m (goClientPipelinedStIdle history (Succ n) a)
    CSP.SendMsgFindIntersect ps a -> CSP.SendMsgFindIntersect ps (goClientPipelinedStIntersect history n a)
    CSP.CollectResponse a b -> case n of
      Succ nPrev ->
        CSP.CollectResponse
          ((fmap . fmap) (goClientPipelinedStIdle history n) a)
          (goClientStNext history nPrev b)
    CSP.SendMsgDone a -> CSP.SendMsgDone a

  -- This is where the magic happens. We intercept the blocks and rollbacks
  -- and use it to maintain the correct ledger state.
  goClientStNext
    :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
    -> Nat n
    -> CSP.ClientStNext
         n
         (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
         ChainPoint
         ChainTip
         m
         a
    -> CSP.ClientStNext
         n
         BlockInMode
         ChainPoint
         ChainTip
         m
         a
  goClientStNext (Left err) n (CSP.ClientStNext recvMsgRollForward recvMsgRollBackward) =
    CSP.ClientStNext
      ( \blkInMode tip ->
          goClientPipelinedStIdle (Left err) n
            <$> recvMsgRollForward
              (blkInMode, Left err)
              tip
      )
      ( \point tip ->
          goClientPipelinedStIdle (Left err) n <$> recvMsgRollBackward point tip
      )
  goClientStNext (Right history) n (CSP.ClientStNext recvMsgRollForward recvMsgRollBackward) =
    CSP.ClientStNext
      ( \blkInMode@(BlockInMode _ block) tip ->
          let
            BlockHeader slotNo _ _ = getBlockHeader block
            newLedgerStateE = case Seq.lookup 0 history of
              Nothing -> error "Impossible! History should always be non-empty"
              Just (_, Left err, _) -> Left err
              Just (_, Right (oldLedgerState, _), _) ->
                applyBlock
                  env
                  oldLedgerState
                  validationMode
                  blkInMode
            (history', _) = pushLedgerState env history slotNo newLedgerStateE blkInMode
           in
            goClientPipelinedStIdle (Right history') n
              <$> recvMsgRollForward
                (blkInMode, newLedgerStateE)
                tip
      )
      ( \point tip ->
          let
            oldestSlot = case history of
              _ Seq.:|> (s, _, _) -> s
              Seq.Empty -> error "Impossible! History should always be non-empty"
            history' = ( \h ->
                           if Seq.null h
                             then Left (InvalidRollback oldestSlot point)
                             else Right h
                       )
              $ case point of
                ChainPointAtGenesis -> initialLedgerStateHistory
                ChainPoint slotNo _ -> rollBackLedgerStateHist history slotNo
           in
            goClientPipelinedStIdle history' n <$> recvMsgRollBackward point tip
      )

  goClientPipelinedStIntersect
    :: Either LedgerStateError (History (Either LedgerStateError LedgerStateEvents))
    -> Nat n
    -> CSP.ClientPipelinedStIntersect
         (BlockInMode, Either LedgerStateError (LedgerState, [LedgerEvent]))
         ChainPoint
         ChainTip
         m
         a
    -> CSP.ClientPipelinedStIntersect
         BlockInMode
         ChainPoint
         ChainTip
         m
         a
  goClientPipelinedStIntersect history _ (CSP.ClientPipelinedStIntersect recvMsgIntersectFound recvMsgIntersectNotFound) =
    CSP.ClientPipelinedStIntersect
      (\point tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectFound point tip)
      (\tip -> goClientPipelinedStIdle history Zero <$> recvMsgIntersectNotFound tip)

  initialLedgerStateHistory :: History (Either LedgerStateError LedgerStateEvents)
  initialLedgerStateHistory = Seq.singleton (0, Right (ledgerState0, []), Origin)

extractHistory
  :: History LedgerStateEvents
  -> [(SlotNo, [LedgerEvent], BlockNo)]
extractHistory historySeq =
  let histList = toList historySeq
   in List.map
        (\(slotNo, (_ledgerState, ledgerEvents), block) -> (slotNo, ledgerEvents, getBlockNo block))
        histList

getBlockNo :: WithOrigin BlockInMode -> BlockNo
getBlockNo = Consensus.withOrigin (BlockNo 0) (blockNo . toConsensusBlock)

-- | A history of k (security parameter) recent ledger states. The head is the
-- most recent item. Elements are:
--
-- * Slot number that a new block occurred
-- * The ledger state and events after applying the new block
-- * The new block
type LedgerStateHistory = History LedgerStateEvents

type History a = Seq (SlotNo, a, WithOrigin BlockInMode)

-- | Add a new ledger state to the history
pushLedgerState
  :: Env
  -- ^ Environment used to get the security param, k.
  -> History a
  -- ^ History of k items.
  -> SlotNo
  -- ^ Slot number of the new item.
  -> a
  -- ^ New item to add to the history
  -> BlockInMode
  -- ^ The block that (when applied to the previous
  -- item) resulted in the new item.
  -> (History a, History a)
  -- ^ ( The new history with the new item appended
  --   , Any existing items that are now past the security parameter
  --      and hence can no longer be rolled back.
  --   )
pushLedgerState env hist sn st block =
  Seq.splitAt
    (fromIntegral $ envSecurityParam env + 1)
    ((sn, st, At block) Seq.:<| hist)

rollBackLedgerStateHist :: History a -> SlotNo -> History a
rollBackLedgerStateHist hist maxInc = Seq.dropWhileL ((> maxInc) . (\(x, _, _) -> x)) hist

genesisConfigToEnv
  :: SomeHasFS IO
  -> GenesisConfig
  -> IO (Either GenesisConfigError Env)
genesisConfigToEnv fs genCfg =
  case genCfg of
    GenesisCardano _ bCfg _ transCfg
      | Cardano.Crypto.ProtocolMagic.unProtocolMagicId (Cardano.Chain.Genesis.configProtocolMagicId bCfg)
          /= Ledger.sgNetworkMagic shelleyGenesis ->
          pure . Left . NECardanoConfig $
            mconcat
              [ "ProtocolMagicId "
              , textShow
                  (Cardano.Crypto.ProtocolMagic.unProtocolMagicId $ Cardano.Chain.Genesis.configProtocolMagicId bCfg)
              , " /= "
              , textShow (Ledger.sgNetworkMagic shelleyGenesis)
              ]
      -- byron start time is stored in seconds precision (using `canonical-json` library)
      -- shelley start time is stored in fracions of a seconds precision (using `aeson` library)
      -- Because of the representation precision difference, we need to round the values to the number of seconds before comparison.
      | let byronSystemStart =
              round . utcTimeToPOSIXSeconds . Cardano.Chain.Genesis.gdStartTime $
                Cardano.Chain.Genesis.configGenesisData bCfg
                :: Int
      , let shelleySystemStart = round . utcTimeToPOSIXSeconds $ Ledger.sgSystemStart shelleyGenesis :: Int
      , byronSystemStart /= shelleySystemStart ->
          pure . Left . NECardanoConfig $
            mconcat
              [ "SystemStart "
              , textShow (Cardano.Chain.Genesis.gdStartTime $ Cardano.Chain.Genesis.configGenesisData bCfg)
              , " /= "
              , textShow (Ledger.sgSystemStart shelleyGenesis)
              ]
      | otherwise -> do
          (pinfo, _) <- mkProtocolInfoCardano fs genCfg
          let topLevelConfig = Consensus.pInfoConfig pinfo
          pure $
            Right $
              Env
                { envLedgerConfig = Consensus.topLevelConfigLedger topLevelConfig
                , envConsensusConfig = Consensus.topLevelConfigProtocol topLevelConfig
                }
     where
      shelleyGenesis = transCfg ^. Ledger.tcShelleyGenesisL

-- | Why 'readNodeConfig' could not read a node configuration.
data NodeConfigError
  = -- | @cardano-config@ could not read, decode or check the configuration or a
    -- genesis file it names. The error says which file, section and key are at
    -- fault.
    NodeConfigParsingError Cfg.ConfigurationParsingError
  | -- | @cardano-config@ read the configuration, but could not resolve it.
    NodeConfigResolutionError Cfg.ConfigResolutionError
  | -- | Any other failure while reading the configuration. @trace-dispatcher@,
    -- which reads the tracing section, throws its own exceptions, and reading a
    -- file can fail with an IO error.
    NodeConfigOtherError SomeException
  deriving Show

instance Exception NodeConfigError

instance Error NodeConfigError where
  prettyError = \case
    NodeConfigParsingError err -> prettyException err
    NodeConfigResolutionError err -> prettyException err
    NodeConfigOtherError (SomeException err) -> prettyException err

-- | Read a node configuration file through @cardano-config@, in either the flat
-- format or the @cardano-config@ envelope. Every genesis file the
-- configuration names is read, decoded, and checked against the hash the
-- configuration gives for it, so a configuration without genesis hashes is
-- rejected.
--
-- Also returns the warnings @cardano-config@ raised while reading the
-- configuration, for example about keys it does not know, or a configuration in
-- an older format. 'Cfg.renderConfigWarning' renders them.
readNodeConfig
  :: MonadError NodeConfigError m
  => MonadIO m
  => NodeConfigFile 'In
  -> m (NodeConfig, [Cfg.ConfigWarning])
readNodeConfig (File ncf) = do
  (fromFile, fileWarnings) <-
    liftEither . first readError <=< liftIO . tryAny $
      Cfg.parseConfigurationFiles ncf
  -- The first argument of 'Cfg.resolveConfigurationWith' is the list of
  -- consistency checks to run ('Cfg.resolveConfiguration' runs cardano-config's
  -- default list, 'Cfg.defaultConfigChecks'). We run none: those checks are
  -- about running a node (gRPC, connection limits, peer targets, snapshots),
  -- and some need the node's command line, which we don't have here.
  (nc, resolutionWarnings) <-
    liftEither . first NodeConfigResolutionError $
      Cfg.resolveConfigurationWith [] (Cfg.defaultCliArgs ncf) fromFile
  pure (NodeConfig nc, fileWarnings <> resolutionWarnings)
 where
  -- cardano-config throws a 'Cfg.ConfigurationParsingError', but it hands the
  -- tracing section to trace-dispatcher, which throws yaml's @ParseException@ or
  -- an 'ErrorCall' instead, and reading a file can fail with an IO error.
  readError :: SomeException -> NodeConfigError
  readError e = maybe (NodeConfigOtherError e) NodeConfigParsingError $ fromException e

-- | The hard fork triggers of a testing configuration: for each era, the epoch
-- the configuration gives, and otherwise the era's default protocol version.
-- Like the node, it ignores the Dijkstra epoch unless
-- @ExperimentalHardForksEnabled@ is true.
hardForkTriggers
  :: Cfg.TestingConfiguration Identity
  -> Consensus.CardanoHardForkTriggers
hardForkTriggers testing =
  Consensus.CardanoHardForkTriggers'
    (atEpoch $ Cfg.testShelleyHardForkAtEpoch testing)
    (atEpoch $ Cfg.testAllegraHardForkAtEpoch testing)
    (atEpoch $ Cfg.testMaryHardForkAtEpoch testing)
    (atEpoch $ Cfg.testAlonzoHardForkAtEpoch testing)
    (atEpoch $ Cfg.testBabbageHardForkAtEpoch testing)
    (atEpoch $ Cfg.testConwayHardForkAtEpoch testing)
    (atEpoch dijkstraEpoch)
 where
  dijkstraEpoch
    | runIdentity (Cfg.experimentalHardForksEnabled testing) = Cfg.testDijkstraHardForkAtEpoch testing
    | otherwise = Ledger.SNothing
  atEpoch :: Ledger.StrictMaybe Word64 -> Consensus.CardanoHardForkTrigger blk
  atEpoch =
    maybe
      Consensus.CardanoTriggerHardForkAtDefaultVersion
      (Consensus.CardanoTriggerHardForkAtEpoch . Slot.EpochNo)
      . Ledger.strictMaybeToMaybe

-- | A node configuration, as @cardano-config@ read it. The functions below give
-- the parts of it this library uses.
newtype NodeConfig = NodeConfig
  { ncConfiguration :: Cfg.NodeConfiguration
  -- ^ The configuration as @cardano-config@ read it, genesis files included.
  }

-- | The Byron PBFT signature threshold: always 'Nothing', so the consensus
-- default applies. A @cardano-config@ configuration has no such setting (reading
-- a flat configuration drops its @PBftSignatureThreshold@). It does not affect
-- reading or applying blocks.
ncPBftSignatureThreshold :: NodeConfig -> Maybe Double
ncPBftSignatureThreshold _ = Nothing
{-# DEPRECATED
  ncPBftSignatureThreshold
  "No longer read from the configuration: always Nothing, the consensus default"
  #-}

-- | The Byron protocol version a node endorses: always 1.0.0. A
-- @cardano-config@ configuration has no such setting (reading a flat
-- configuration drops its @LastKnownBlockVersion-Major@, @-Minor@ and @-Alt@).
-- It does not affect reading or applying blocks.
ncByronProtocolVersion :: NodeConfig -> Cardano.Chain.Update.ProtocolVersion
ncByronProtocolVersion _ = fixedByronProtocolVersion
{-# DEPRECATED ncByronProtocolVersion "No longer read from the configuration: always 1.0.0" #-}

-- | The Byron protocol version given to consensus, as a @cardano-config@
-- configuration does not set one.
fixedByronProtocolVersion :: Cardano.Chain.Update.ProtocolVersion
fixedByronProtocolVersion = Cardano.Chain.Update.ProtocolVersion 1 0 0

-- | The Byron genesis file, relative to the directory of the configuration file
-- if the configuration names a relative path.
ncByronGenesisFile :: NodeConfig -> File ByronGenesisConfig 'In
ncByronGenesisFile = genesisFile (Cfg.byronGenesisFile . Cfg.byronGenesis)

-- | The hash of the Byron genesis file, which 'readNodeConfig' has checked.
ncByronGenesisHash :: NodeConfig -> GenesisHashByron
ncByronGenesisHash =
  GenesisHashByron
    . Cardano.Crypto.Hash.Class.hashToTextAsHex
    . Cfg.hash
    . genesisReference (Cfg.byronGenesisFile . Cfg.byronGenesis)

-- | The Shelley genesis file, relative to the directory of the configuration
-- file if the configuration names a relative path.
ncShelleyGenesisFile :: NodeConfig -> File ShelleyGenesisConfig 'In
ncShelleyGenesisFile = genesisFile Cfg.shelleyGenesis

-- | The hash of the Shelley genesis file, which 'readNodeConfig' has checked.
ncShelleyGenesisHash :: NodeConfig -> GenesisHashShelley
ncShelleyGenesisHash = GenesisHashShelley . Cfg.hash . genesisReference Cfg.shelleyGenesis

-- | The Alonzo genesis file, relative to the directory of the configuration file
-- if the configuration names a relative path.
ncAlonzoGenesisFile :: NodeConfig -> File AlonzoGenesis 'In
ncAlonzoGenesisFile = genesisFile Cfg.alonzoGenesis

-- | The hash of the Alonzo genesis file, which 'readNodeConfig' has checked.
ncAlonzoGenesisHash :: NodeConfig -> GenesisHashAlonzo
ncAlonzoGenesisHash = GenesisHashAlonzo . Cfg.hash . genesisReference Cfg.alonzoGenesis

-- | The Conway genesis file, relative to the directory of the configuration file
-- if the configuration names a relative path.
ncConwayGenesisFile :: NodeConfig -> File ConwayGenesisConfig 'In
ncConwayGenesisFile = genesisFile Cfg.conwayGenesis

-- | The hash of the Conway genesis file, which 'readNodeConfig' has checked.
ncConwayGenesisHash :: NodeConfig -> GenesisHashConway
ncConwayGenesisHash = GenesisHashConway . Cfg.hash . genesisReference Cfg.conwayGenesis

-- | Whether the Byron genesis requires the network magic in addresses and
-- transactions. A configuration that does not say requires none.
ncRequiresNetworkMagic :: NodeConfig -> Cardano.Crypto.RequiresNetworkMagic
ncRequiresNetworkMagic enc =
  case Ledger.fromSMaybe Cfg.RequiresNoMagic
    . Cfg.byronReqNetworkMagic
    . Cfg.byronGenesis
    . Cfg.protocolConfiguration
    $ ncConfiguration enc of
    Cfg.RequiresMagic -> Cardano.Crypto.RequiresMagic
    Cfg.RequiresNoMagic -> Cardano.Crypto.RequiresNoMagic

-- | The hard fork triggers of the configuration's @Testing@ section.
ncHardForkTriggers :: NodeConfig -> Consensus.CardanoHardForkTriggers
ncHardForkTriggers = hardForkTriggers . Cfg.testingConfiguration . ncConfiguration

-- | A genesis file reference of the configuration: the path and its hash.
genesisReference
  :: (Cfg.ProtocolConfiguration Identity -> Cfg.Hashed FilePath)
  -> NodeConfig
  -> Cfg.Hashed FilePath
genesisReference ref = ref . Cfg.protocolConfiguration . ncConfiguration

-- | A genesis file of the configuration, relative to the directory of the
-- configuration file if the configuration names a relative path.
genesisFile
  :: (Cfg.ProtocolConfiguration Identity -> Cfg.Hashed FilePath)
  -> NodeConfig
  -> File content 'In
genesisFile ref enc =
  File . mkAdjustPath (Cfg.configFilePath (ncConfiguration enc)) . Cfg.hashed $
    genesisReference ref enc

mkAdjustPath :: FilePath -> (FilePath -> FilePath)
mkAdjustPath nodeConfigFilePath fp = takeDirectory nodeConfigFilePath </> fp

initLedgerStateVar :: SomeHasFS IO -> GenesisConfig -> IO LedgerState
initLedgerStateVar fs genesisConfig = do
  (pinfo, _) <- mkProtocolInfoCardano fs genesisConfig
  pure
    LedgerState
      { clsState =
          Ledger.ledgerState $
            Ledger.forgetLedgerTables $
              Consensus.pInfoInitLedger pinfo
      , clsTables =
          Ledger.projectLedgerTables $
            Ledger.ledgerState $
              Consensus.pInfoInitLedger pinfo
      }

data LedgerState = LedgerState
  { clsState :: Consensus.CardanoLedgerState Consensus.StandardCrypto Ledger.EmptyMK
  , clsTables
      :: Ledger.LedgerTables
           (Consensus.HardForkBlock (Consensus.CardanoEras Consensus.StandardCrypto))
           Ledger.ValuesMK
  }
  deriving Show

-- | Retrieve new epoch state from the ledger state, or an error on failure
getAnyNewEpochState
  :: ShelleyBasedEra era
  -> LedgerState
  -> Either LedgerStateError AnyNewEpochState
getAnyNewEpochState sbe (LedgerState ls tbs) =
  flip (AnyNewEpochState sbe) tbs <$> getNewEpochState sbe ls

getNewEpochState
  :: ShelleyBasedEra era
  -> Consensus.CardanoLedgerState Consensus.StandardCrypto Ledger.EmptyMK
  -> Either LedgerStateError (ShelleyAPI.NewEpochState (ShelleyLedgerEra era))
getNewEpochState era x = do
  let tip = Telescope.tip $ getHardForkState $ HFC.hardForkLedgerStatePerEra x
      err = UnexpectedLedgerState (shelleyBasedEraConstraints era $ AnyShelleyBasedEra era) tip
  case era of
    ShelleyBasedEraShelley ->
      case tip of
        ShelleyLedgerState shelleyCurrent ->
          pure $ Shelley.shelleyLedgerState $ unFlip $ currentState shelleyCurrent
        _ -> Left err
    ShelleyBasedEraAllegra ->
      case tip of
        AllegraLedgerState allegraCurrent ->
          pure $ Shelley.shelleyLedgerState $ unFlip $ currentState allegraCurrent
        _ -> Left err
    ShelleyBasedEraMary ->
      case tip of
        MaryLedgerState maryCurrent ->
          pure $ Shelley.shelleyLedgerState $ unFlip $ currentState maryCurrent
        _ -> Left err
    ShelleyBasedEraAlonzo ->
      case tip of
        AlonzoLedgerState alonzoCurrent ->
          pure $ Shelley.shelleyLedgerState $ unFlip $ currentState alonzoCurrent
        _ -> Left err
    ShelleyBasedEraBabbage ->
      case tip of
        BabbageLedgerState babbageCurrent ->
          pure $ Shelley.shelleyLedgerState $ unFlip $ currentState babbageCurrent
        _ -> Left err
    ShelleyBasedEraConway ->
      case tip of
        ConwayLedgerState conwayCurrent ->
          pure $ Shelley.shelleyLedgerState $ unFlip $ currentState conwayCurrent
        _ -> Left err
    ShelleyBasedEraDijkstra ->
      case tip of
        DijkstraLedgerState dijkstraCurrent ->
          pure $ Shelley.shelleyLedgerState $ unFlip $ currentState dijkstraCurrent
        _ -> Left err

{-# COMPLETE
  ShelleyLedgerState
  , AllegraLedgerState
  , MaryLedgerState
  , AlonzoLedgerState
  , BabbageLedgerState
  , ConwayLedgerState
  #-}

pattern ShelleyLedgerState
  :: Current
       (Flip Consensus.LedgerState mk)
       ( Shelley.ShelleyBlock
           (TPraos.TPraos Ledger.StandardCrypto)
           Consensus.ShelleyEra
       )
  -> NS (Current (Flip Consensus.LedgerState mk)) (Consensus.CardanoEras Consensus.StandardCrypto)
pattern ShelleyLedgerState x = S (Z x)

pattern AllegraLedgerState
  :: Current
       (Flip Consensus.LedgerState mk)
       ( Shelley.ShelleyBlock
           (TPraos.TPraos Ledger.StandardCrypto)
           Consensus.AllegraEra
       )
  -> NS (Current (Flip Consensus.LedgerState mk)) (Consensus.CardanoEras Consensus.StandardCrypto)
pattern AllegraLedgerState x = S (S (Z x))

pattern MaryLedgerState
  :: Current
       (Flip Consensus.LedgerState mk)
       ( Shelley.ShelleyBlock
           (TPraos.TPraos Ledger.StandardCrypto)
           Consensus.MaryEra
       )
  -> NS (Current (Flip Consensus.LedgerState mk)) (Consensus.CardanoEras Consensus.StandardCrypto)
pattern MaryLedgerState x = S (S (S (Z x)))

pattern AlonzoLedgerState
  :: Current
       (Flip Consensus.LedgerState mk)
       ( Shelley.ShelleyBlock
           (TPraos.TPraos Ledger.StandardCrypto)
           Consensus.AlonzoEra
       )
  -> NS (Current (Flip Consensus.LedgerState mk)) (Consensus.CardanoEras Consensus.StandardCrypto)
pattern AlonzoLedgerState x = S (S (S (S (Z x))))

pattern BabbageLedgerState
  :: Current
       (Flip Consensus.LedgerState mk)
       ( Shelley.ShelleyBlock
           (Praos.Praos Ledger.StandardCrypto)
           Consensus.BabbageEra
       )
  -> NS (Current (Flip Consensus.LedgerState mk)) (Consensus.CardanoEras Consensus.StandardCrypto)
pattern BabbageLedgerState x = S (S (S (S (S (Z x)))))

pattern ConwayLedgerState
  :: Current
       (Flip Consensus.LedgerState mk)
       ( Shelley.ShelleyBlock
           (Praos.Praos Ledger.StandardCrypto)
           Consensus.ConwayEra
       )
  -> NS (Current (Flip Consensus.LedgerState mk)) (Consensus.CardanoEras Consensus.StandardCrypto)
pattern ConwayLedgerState x = S (S (S (S (S (S (Z x))))))

pattern DijkstraLedgerState
  :: Current
       (Flip Consensus.LedgerState mk)
       ( Shelley.ShelleyBlock
           (Praos2.Praos2 Ledger.StandardCrypto)
           Consensus.DijkstraEra
       )
  -> NS (Current (Flip Consensus.LedgerState mk)) (Consensus.CardanoEras Consensus.StandardCrypto)
pattern DijkstraLedgerState x = S (S (S (S (S (S (S (Z x)))))))

encodeLedgerState :: LedgerState -> CBOR.Encoding
encodeLedgerState (LedgerState hst@(HFC.HardForkLedgerState st) tbs) =
  mconcat
    [ CBOR.encodeListLen 2
    , HFC.encodeTelescope
        (byron :* shelley :* allegra :* mary :* alonzo :* babbage :* conway :* dijkstra :* Nil)
        st
    , Ledger.valuesMKEncoder hst tbs
    ]
 where
  byron = fn (K . Byron.encodeByronLedgerState . unFlip)
  shelley = fn (K . Shelley.encodeShelleyLedgerState . unFlip)
  allegra = fn (K . Shelley.encodeShelleyLedgerState . unFlip)
  mary = fn (K . Shelley.encodeShelleyLedgerState . unFlip)
  alonzo = fn (K . Shelley.encodeShelleyLedgerState . unFlip)
  babbage = fn (K . Shelley.encodeShelleyLedgerState . unFlip)
  conway = fn (K . Shelley.encodeShelleyLedgerState . unFlip)
  dijkstra = fn (K . Shelley.encodeShelleyLedgerState . unFlip)

decodeLedgerState :: forall s. CBOR.Decoder s LedgerState
decodeLedgerState = do
  2 <- CBOR.decodeListLen
  hst <-
    HFC.HardForkLedgerState
      <$> HFC.decodeTelescope
        (byron :* shelley :* allegra :* mary :* alonzo :* babbage :* conway :* dijkstra :* Nil)
  tbs <- Ledger.valuesMKDecoder hst
  pure (LedgerState hst tbs)
 where
  byron = Comp $ Flip <$> Byron.decodeByronLedgerState
  shelley = Comp $ Flip <$> Shelley.decodeShelleyLedgerState
  allegra = Comp $ Flip <$> Shelley.decodeShelleyLedgerState
  mary = Comp $ Flip <$> Shelley.decodeShelleyLedgerState
  alonzo = Comp $ Flip <$> Shelley.decodeShelleyLedgerState
  babbage = Comp $ Flip <$> Shelley.decodeShelleyLedgerState
  conway = Comp $ Flip <$> Shelley.decodeShelleyLedgerState
  dijkstra = Comp $ Flip <$> Shelley.decodeShelleyLedgerState

type LedgerStateEvents = (LedgerState, [LedgerEvent])

toLedgerStateEvents
  :: Ledger.LedgerResult
       (Consensus.HardForkBlock (Consensus.CardanoEras Consensus.StandardCrypto))
       LedgerState
  -> LedgerStateEvents
toLedgerStateEvents lr = (ledgerState, ledgerEvents)
 where
  ledgerState = Ledger.lrResult lr
  ledgerEvents =
    mapMaybe
      ( toLedgerEvent
          . WrapLedgerEvent @(Consensus.CardanoBlock Consensus.StandardCrypto)
      )
      $ Ledger.lrEvents lr

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano
      !NodeConfig
      !Cardano.Chain.Genesis.Config
      !GenesisHashShelley
      !(Ledger.TransitionConfig Ledger.LatestKnownEra)

newtype LedgerStateDir = LedgerStateDir
  { unLedgerStateDir :: FilePath
  }
  deriving Show

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  }
  deriving Show

type NodeConfigFile = File NodeConfig

mkProtocolInfoCardano
  :: SomeHasFS IO
  -> GenesisConfig
  -> IO
       ( Consensus.ProtocolInfo
           (Consensus.CardanoBlock Consensus.StandardCrypto)
       , Tracer.Tracer IO KESAgentClientTrace
         -> IO [MkBlockForging IO (Consensus.CardanoBlock Consensus.StandardCrypto)]
       )
mkProtocolInfoCardano fs (GenesisCardano dnc byronGenesis shelleyGenesisHash transCfg) =
  Consensus.protocolInfoCardano
    fs
    Consensus.CardanoProtocolParams
      { Consensus.byronProtocolParams =
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = byronGenesis
            , -- Neither is set by a cardano-config configuration (see
              -- 'ncPBftSignatureThreshold' and 'ncByronProtocolVersion').
              Consensus.byronPbftSignatureThreshold = Nothing
            , Consensus.byronProtocolVersion = fixedByronProtocolVersion
            , Consensus.byronSoftwareVersion = Byron.softwareVersion
            , Consensus.byronLeaderCredentials = Nothing
            }
      , Consensus.shelleyBasedProtocolParams =
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesisHash
            , Consensus.shelleyBasedLeaderCredentials = []
            }
      , Consensus.cardanoHardForkTriggers = ncHardForkTriggers dnc
      , Consensus.cardanoLedgerTransitionConfig = transCfg
      , -- NOTE: this can become a parameter once https://github.com/IntersectMBO/cardano-node/issues/5730 is implemented.
        Consensus.cardanoCheckpoints = Consensus.emptyCheckpointsMap
      , Consensus.cardanoProtocolVersion = ProtVer (natVersion @10) 0
      }

-- | Compute the Nonce from the hash of the Genesis file.
shelleyPraosNonce :: GenesisHashShelley -> Ledger.Nonce
shelleyPraosNonce genesisHash =
  Ledger.Nonce (Cardano.Crypto.Hash.Class.castHash $ unGenesisHashShelley genesisHash)

-- | The genesis configuration of a node configuration, built from the genesis
-- files that 'readNodeConfig' has read and checked.
--
-- The ledger always needs a Dijkstra genesis, even when Dijkstra is not used,
-- but @cardano-config@ only reads the @DijkstraGenesisFile@ when
-- @ExperimentalHardForksEnabled@ is true. When it is false, this function
-- supplies a placeholder Dijkstra genesis with example values instead, and
-- 'ncHardForkTriggers' ignores @TestDijkstraHardForkAtEpoch@.
readCardanoGenesisConfig
  :: MonadIOTransError GenesisConfigError t m
  => NodeConfig
  -> t m GenesisConfig
readCardanoGenesisConfig enc = do
  let byronGenesis = readByronGenesisConfig enc
      ShelleyConfig shelleyGenesis shelleyGenesisHash = readShelleyGenesisConfig enc
      alonzoGenesis = readAlonzoGenesisConfig enc
      conwayGenesis = readConwayGenesisConfig enc
      dijkstraGenesis =
        Ledger.fromSMaybe exampleDijkstraGenesis . Cfg.experimentalGenesisConfig $ ncConfiguration enc
      transCfg = Ledger.mkLatestTransitionConfig shelleyGenesis alonzoGenesis conwayGenesis dijkstraGenesis
  pure $ GenesisCardano enc byronGenesis shelleyGenesisHash transCfg

-- | Resolve a Shelley genesis' 'Ledger.sgInitialFunds' against its
-- 'Ledger.sgExtraConfig', mirroring the resolution ledger's own
-- @registerInitialFunds@ performs when building the initial ledger state
-- from genesis.
--
-- A 'Ledger.ShelleyGenesis' carries two sources of initial funds: the legacy
-- 'Ledger.sgInitialFunds' field, and 'Ledger.sgExtraConfig', which is where
-- @cardano-cli create-testnet-data@ puts the funded addresses, embedded or in
-- an external hash-verified file. The ledger reconciles the two only while
-- building the initial ledger state and never writes the result back into the
-- 'Ledger.ShelleyGenesis' value, so a parsed genesis read outside that path
-- must repeat the resolution. Once the ledger drops the legacy field, the
-- two-source reconciliation here can go, but turning 'Ledger.secInitialFunds'
-- into actual funds (including reading and hash-checking an injection file)
-- is still needed.
--
-- Throws 'Ledger.InjectionConflictingSources' if the genesis specifies initial
-- funds through both the legacy field and the extra config, and
-- 'Ledger.InjectionHashMismatch' if an 'Ledger.InjectionFromFile' source does
-- not hash to the value the genesis declares for it.
resolveShelleyInitialFunds
  :: (MonadST m, MonadThrow m)
  => SomeHasFS m
  -- ^ Filesystem capability used to stream an 'Ledger.InjectionFromFile'
  -- source, mounted at the Shelley genesis file's directory.
  -> Ledger.ShelleyGenesis
  -- ^ The Shelley genesis whose initial funds to resolve.
  -> m Ledger.ShelleyGenesis
resolveShelleyInitialFunds (SomeHasFS hasFS) genesis = do
  initialFundsSource <-
    Ledger.resolveInjectionSource
      "initialFunds"
      (Ledger.sgExtraConfig genesis)
      Ledger.secInitialFunds
      (Ledger.sgInitialFunds genesis)
  resolvedInitialFunds <-
    fromList <$> Ledger.foldInjectionData hasFS initialFundsSource (flip (:)) []
  pure $
    genesis
      & Ledger.sgInitialFundsL .~ resolvedInitialFunds
      -- The source has now been folded into 'sgInitialFunds'; null it out
      -- so that re-resolving this (already-resolved) genesis later does
      -- not trip 'Ledger.InjectionConflictingSources'.
      & Ledger.sgExtraConfigL .~ (clearInitialFundsSource <$> Ledger.sgExtraConfig genesis)
 where
  clearInitialFundsSource extraConfig = extraConfig{Ledger.secInitialFunds = Ledger.NoInjection}

exampleDijkstraGenesis :: Ledger.DijkstraGenesis
exampleDijkstraGenesis =
  Ledger.DijkstraGenesis
    { Ledger.dgUpgradePParams =
        Ledger.UpgradeDijkstraPParams
          { Ledger.udppMaxRefScriptSizePerBlock = 1024 * 1024 -- 1MiB
          , Ledger.udppMaxRefScriptSizePerTx = 200 * 1024 -- 200KiB
          , Ledger.udppRefScriptCostStride = knownNonZeroBounded @25600 -- 25 KiB
          , Ledger.udppRefScriptCostMultiplier = fromJust $ boundRational 1.2
          , Ledger.udppMaxPledgeLeverage = Core.MaxPledgeLeverage ShelleyAPI.SNothing
          , Ledger.udppMinPoolMargin = fromJust $ boundRational 0.015
          , Ledger.udppPlutusV4CostModel = testingCostModel PlutusV4
          , Ledger.udppLeiosAnnouncementPeriodLength = Milliseconds32 1000 -- L_hdr
          , Ledger.udppLeiosVotePeriodLength = Milliseconds32 4000 -- L_vote
          , Ledger.udppLeiosDiffusionPeriodLength = Milliseconds32 7000 -- L_diff
          , Ledger.udppLeiosCommitteeSize = 900 -- N_c
          , Ledger.udppLeiosQuorumStakeThreshold = fromJust $ boundRational 0.75 -- tau
          , Ledger.udppMaxEndorserBlockReferencesSize = 512 * 1024 -- 512 KiB
          , Ledger.udppMaxEndorserBlockTxsSize = 12 * 1024 * 1024 -- 12 MiB
          , Ledger.udppMaxEndorserBlockExUnits = OrdExUnits $ Ledger.ExUnits 7000000000 2000000000000
          , Ledger.udppMaxRefScriptSizePerEndorserBlock = 12 * 1024 * 1024 -- 12 MiB
          , Ledger.udppPerasMinCandidateBlockAge = Slot.SlotInterval 90
          , Ledger.udppPerasHealingFactor = fromJust $ boundRational 0.5
          , Ledger.udppPerasCertBoost = 15
          , Ledger.udppPerasTargetCommitteeSize = 800
          , Ledger.udppPerasBootstrapRound = ShelleyAPI.SJust 0
          , Ledger.udppPerasQuorumThresholdSafetyMargin = fromJust $ boundRational 0.05
          , Ledger.udppRefInputsCostPerMultiAssetPolicy = Ledger.Coin 0
          , Ledger.udppRefInputsCostPerDatumByte = Ledger.CoinPerByte (Coin.CompactCoin 0)
          }
    }

newtype GenesisConfigError
  = NECardanoConfig Text
  deriving Show

instance Exception GenesisConfigError

instance Error GenesisConfigError where
  prettyError = \case
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   "
        , pretty err
        ]

-- | The Byron genesis of a node configuration, which 'readNodeConfig' has read
-- and checked against its hash.
readByronGenesisConfig :: NodeConfig -> Cardano.Chain.Genesis.Config
readByronGenesisConfig = Cfg.byronGenesisConfig . ncConfiguration

-- | The Shelley genesis of a node configuration, and its hash, which
-- 'readNodeConfig' has read and checked.
readShelleyGenesisConfig :: NodeConfig -> ShelleyConfig
readShelleyGenesisConfig enc =
  ShelleyConfig (Cfg.shelleyGenesisConfig $ ncConfiguration enc) (ncShelleyGenesisHash enc)

-- | The Alonzo genesis of a node configuration, which 'readNodeConfig' has read
-- and checked against its hash.
readAlonzoGenesisConfig :: NodeConfig -> AlonzoGenesis
readAlonzoGenesisConfig = Cfg.alonzoGenesisConfig . ncConfiguration

-- | The Conway genesis of a node configuration, which 'readNodeConfig' has read
-- and checked against its hash.
readConwayGenesisConfig :: NodeConfig -> ConwayGenesis
readConwayGenesisConfig = Cfg.conwayGenesisConfig . ncConfiguration

readShelleyGenesis
  :: forall m t
   . MonadIOTransError ShelleyGenesisError t m
  => ShelleyGenesisFile 'In
  -> Maybe GenesisHashShelley
  -> t m ShelleyConfig
readShelleyGenesis (File file) mExpectedGenesisHash = do
  content <-
    modifyError id $ handleIOExceptT (ShelleyGenesisReadError file . textShow) $ BS.readFile file
  let genesisHash = GenesisHashShelley (Cardano.Crypto.Hash.Class.hashWith id content)
  checkExpectedGenesisHash genesisHash
  genesis <-
    liftEither
      . first (ShelleyGenesisDecodeError file . Text.pack)
      $ Aeson.eitherDecodeStrict' content
  pure $ ShelleyConfig genesis genesisHash
 where
  checkExpectedGenesisHash :: GenesisHashShelley -> t m ()
  checkExpectedGenesisHash actual = forM_ mExpectedGenesisHash $ \expectedGenesisHash ->
    when (actual /= expectedGenesisHash) $
      throwError (ShelleyGenesisHashMismatch actual expectedGenesisHash)

data ShelleyGenesisError
  = ShelleyGenesisReadError !FilePath !Text
  | ShelleyGenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
  | ShelleyGenesisDecodeError !FilePath !Text
  deriving Show

instance Exception ShelleyGenesisError

renderShelleyGenesisError :: ShelleyGenesisError -> Text
renderShelleyGenesisError sge =
  case sge of
    ShelleyGenesisReadError fp err ->
      mconcat
        [ "There was an error reading the genesis file: "
        , Text.pack fp
        , " Error: "
        , err
        ]
    ShelleyGenesisHashMismatch actual expected ->
      mconcat
        [ "Wrong Shelley genesis file: the actual hash is "
        , renderHash (unGenesisHashShelley actual)
        , ", but the expected Shelley genesis hash given in the node "
        , "configuration file is "
        , renderHash (unGenesisHashShelley expected)
        , "."
        ]
    ShelleyGenesisDecodeError fp err ->
      mconcat
        [ "There was an error parsing the genesis file: "
        , Text.pack fp
        , " Error: "
        , err
        ]

renderHash
  :: Cardano.Crypto.Hash.Class.Hash Cardano.Crypto.Hash.Blake2b.Blake2b_256 ByteString -> Text
renderHash h = Text.decodeUtf8 $ Base16.encode (Cardano.Crypto.Hash.Class.hashToBytes h)

newtype StakeCred = StakeCred {_unStakeCred :: Ledger.Credential L.Staking}
  deriving (Eq, Ord)

data Env = Env
  { envLedgerConfig :: Consensus.CardanoLedgerConfig Consensus.StandardCrypto
  , envConsensusConfig :: Consensus.CardanoConsensusConfig Consensus.StandardCrypto
  }

envSecurityParam :: Env -> Word64
envSecurityParam env = Ledger.unNonZero k
 where
  Consensus.SecurityParam k =
    HFC.hardForkConsensusConfigK $
      envConsensusConfig env

-- | How to do validation when applying a block to a ledger state.
data ValidationMode
  = -- | Do all validation implied by the ledger layer's 'applyBlock`.
    FullValidation
  | -- | Only check that the previous hash from the block matches the head hash of
    -- the ledger state.
    QuickValidation

-- The function 'tickThenReapply' does zero validation, so add minimal
-- validation ('blockPrevHash' matches the tip hash of the 'LedgerState'). This
-- was originally for debugging but the check is cheap enough to keep.
applyBlock'
  :: Env
  -> LedgerState
  -> ValidationMode
  -> Consensus.CardanoBlock Consensus.StandardCrypto
  -> Either LedgerStateError LedgerStateEvents
applyBlock' env oldState validationMode block = do
  let config = envLedgerConfig env
  case validationMode of
    FullValidation -> tickThenApply config block oldState
    QuickValidation -> tickThenReapplyCheckHash config block oldState

applyBlockWithEvents
  :: Env
  -> LedgerState
  -> Bool
  -- ^ True to validate
  -> Consensus.CardanoBlock Consensus.StandardCrypto
  -> Either LedgerStateError LedgerStateEvents
applyBlockWithEvents env oldState enableValidation block = do
  let config = envLedgerConfig env
  if enableValidation
    then tickThenApply config block oldState
    else tickThenReapplyCheckHash config block oldState

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenReapplyCheckHash
  :: Consensus.CardanoLedgerConfig Consensus.StandardCrypto
  -> Consensus.CardanoBlock Consensus.StandardCrypto
  -> LedgerState
  -> Either LedgerStateError LedgerStateEvents
tickThenReapplyCheckHash cfg block (LedgerState st tbs) =
  if Consensus.blockPrevHash block == Ledger.ledgerTipHash st
    then
      let
        keys
          :: Ledger.LedgerTables
               (Consensus.CardanoBlock Consensus.StandardCrypto)
               Ledger.KeysMK
        keys = Ledger.getBlockKeySets block

        restrictedTables =
          Ledger.LedgerTables
            (Ledger.restrictValuesMK (Ledger.getLedgerTables tbs) (Ledger.getLedgerTables keys))

        ledgerResult =
          Ledger.tickThenReapplyLedgerResult Ledger.ComputeLedgerEvents cfg block $
            st `Ledger.withLedgerTables` restrictedTables
       in
        Right
          . toLedgerStateEvents
          . fmap
            ( \stt ->
                LedgerState
                  (Ledger.forgetLedgerTables stt)
                  ( Ledger.LedgerTables
                      . Ledger.applyDiffsMK (Ledger.getLedgerTables tbs)
                      . Ledger.getLedgerTables
                      . Ledger.projectLedgerTables
                      $ stt
                  )
            )
          $ ledgerResult
    else
      Left $
        ApplyBlockHashMismatch $
          mconcat
            [ "Ledger state hash mismatch. Ledger head is slot "
            , textShow $
                Slot.unSlotNo $
                  Slot.fromWithOrigin
                    (Slot.SlotNo 0)
                    (Ledger.ledgerTipSlot st)
            , " hash "
            , renderByteArray $
                unChainHash $
                  Ledger.ledgerTipHash st
            , " but block previous hash is "
            , renderByteArray (unChainHash $ Consensus.blockPrevHash block)
            , " and block current hash is "
            , renderByteArray $
                BSS.fromShort $
                  HFC.getOneEraHash $
                    Ouroboros.Network.Block.blockHash block
            , "."
            ]

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from
-- the block matches the head hash of the ledger state.
tickThenApply
  :: Consensus.CardanoLedgerConfig Consensus.StandardCrypto
  -> Consensus.CardanoBlock Consensus.StandardCrypto
  -> LedgerState
  -> Either LedgerStateError LedgerStateEvents
tickThenApply cfg block (LedgerState st tbs) =
  let
    keys
      :: Ledger.LedgerTables
           (Consensus.CardanoBlock Consensus.StandardCrypto)
           Ledger.KeysMK
    keys = Ledger.getBlockKeySets block

    restrictedTables =
      Ledger.LedgerTables
        (Ledger.restrictValuesMK (Ledger.getLedgerTables tbs) (Ledger.getLedgerTables keys))

    eLedgerResult =
      runExcept $
        Ledger.tickThenApplyLedgerResult Ledger.ComputeLedgerEvents cfg block $
          st `Ledger.withLedgerTables` restrictedTables
   in
    either
      (Left . ApplyBlockError)
      ( Right
          . toLedgerStateEvents
          . fmap
            ( \stt ->
                LedgerState
                  (Ledger.forgetLedgerTables stt)
                  ( Ledger.LedgerTables
                      . Ledger.applyDiffsMK (Ledger.getLedgerTables tbs)
                      . Ledger.getLedgerTables
                      . Ledger.projectLedgerTables
                      $ stt
                  )
            )
      )
      eLedgerResult

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

unChainHash :: Ouroboros.Network.Block.ChainHash (Consensus.CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    Ouroboros.Network.Block.GenesisHash -> "genesis"
    Ouroboros.Network.Block.BlockHash bh -> BSS.fromShort (HFC.getOneEraHash bh)

data LeadershipError
  = LeaderErrDecodeLedgerStateFailure
  | LeaderErrDecodeProtocolStateFailure (LBS.ByteString, DecoderError)
  | LeaderErrDecodeProtocolEpochStateFailure DecoderError
  | LeaderErrDecodePoolDistributionFailure DecoderError
  | LeaderErrGenesisSlot
  | LeaderErrStakePoolHasNoStake PoolId
  | LeaderErrStakeDistribUnstable
      SlotNo
      -- ^ Current slot
      SlotNo
      -- ^ Stable after
      SlotNo
      -- ^ Stability window size
      SlotNo
      -- ^ Predicted last slot of the epoch
  | LeaderErrSlotRangeCalculationFailure Text
  | LeaderErrCandidateNonceStillEvolving
  deriving Show

instance Api.Error LeadershipError where
  prettyError = \case
    LeaderErrDecodeLedgerStateFailure ->
      "Failed to successfully decode ledger state"
    LeaderErrDecodeProtocolStateFailure (_, decErr) ->
      mconcat
        [ "Failed to successfully decode protocol state: "
        , pretty (LT.toStrict . toLazyText $ build decErr)
        ]
    LeaderErrGenesisSlot ->
      "Leadership schedule currently cannot be calculated from genesis"
    LeaderErrStakePoolHasNoStake poolId ->
      "The stake pool: " <> pshow poolId <> " has no stake"
    LeaderErrDecodePoolDistributionFailure decoderError ->
      "Failed to successfully decode the pool stake distribution: " <> pshow decoderError
    LeaderErrDecodeProtocolEpochStateFailure decoderError ->
      "Failed to successfully decode the current epoch state: " <> pshow decoderError
    LeaderErrStakeDistribUnstable curSlot stableAfterSlot stabWindow predictedLastSlot ->
      mconcat
        [ "The current stake distribution is currently unstable and therefore we cannot predict "
        , "the following epoch's leadership schedule. Please wait until : " <> pshow stableAfterSlot
        , " before running the leadership-schedule command again. \nCurrent slot: " <> pshow curSlot
        , " \nStability window: " <> pshow stabWindow
        , " \nCalculated last slot of current epoch: " <> pshow predictedLastSlot
        ]
    LeaderErrSlotRangeCalculationFailure e ->
      "Error while calculating the slot range: " <> pretty e
    LeaderErrCandidateNonceStillEvolving ->
      "Candidate nonce is still evolving"

nextEpochEligibleLeadershipSlots
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> ShelleyGenesis
  -> SerialisedCurrentEpochState era
  -- ^ We need the mark stake distribution in order to predict
  --   the following epoch's leadership schedule
  -> ProtocolState era
  -> PoolId
  -- ^ Potential slot leading stake pool
  -> SigningKey VrfKey
  -- ^ VRF signing key of the stake pool
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> EpochInfo (Either Text)
  -> (ChainTip, EpochNo)
  -> Either LeadershipError (Set SlotNo)
nextEpochEligibleLeadershipSlots sbe sGen serCurrEpochState ptclState poolid (VrfSigningKey vrfSkey) pp eInfo (cTip, currentEpoch) =
  shelleyBasedEraConstraints sbe $ do
    (_, currentEpochLastSlot) <-
      first LeaderErrSlotRangeCalculationFailure $
        Slot.epochInfoRange eInfo currentEpoch

    (firstSlotOfEpoch, lastSlotofEpoch) <-
      first LeaderErrSlotRangeCalculationFailure $
        Slot.epochInfoRange eInfo (currentEpoch `Slot.addEpochInterval` Slot.EpochInterval 1)

    -- First we check if we are within 3k/f slots of the end of the current epoch.
    -- In Conway era onwards, we use 4k/f slots instead of 3k/f slots.
    -- see: https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/Glossary#epoch-structure
    -- Why? Because the stake distribution is stable at this point.
    -- k is the security parameter
    -- f is the active slot coefficient
    let stabilityWindowR :: Rational
        stabilityWindowR =
          fromIntegral (stabilityWindowConst * Ledger.unNonZero (sgSecurityParam sGen))
            / Ledger.unboundRational (sgActiveSlotsCoeff sGen)
        stabilityWindowSlots :: SlotNo
        stabilityWindowSlots = fromIntegral @Word64 $ floor $ fromRational @Double stabilityWindowR
        stableStakeDistribSlot = currentEpochLastSlot - stabilityWindowSlots
        stabilityWindowConst = inEonForShelleyBasedEra @ConwayEraOnwards 3 (const 4) sbe

    case cTip of
      ChainTipAtGenesis -> Left LeaderErrGenesisSlot
      ChainTip tip _ _ ->
        if tip > stableStakeDistribSlot
          then return ()
          else
            Left $
              LeaderErrStakeDistribUnstable tip stableStakeDistribSlot stabilityWindowSlots currentEpochLastSlot

    chainDepState <-
      first LeaderErrDecodeProtocolStateFailure $
        decodeProtocolState ptclState

    -- We need the candidate nonce, the previous epoch's last block header hash
    -- and the extra entropy from the protocol parameters. We then need to combine them
    -- with the (⭒) operator.
    let Consensus.PraosNonces{Consensus.candidateNonce, Consensus.evolvingNonce} =
          Consensus.getPraosNonces (Proxy @(Api.ConsensusProtocol era)) chainDepState

    -- Let's do a nonce check. The candidate nonce and the evolving nonce should not be equal.
    when (evolvingNonce == candidateNonce) $
      Left LeaderErrCandidateNonceStillEvolving

    -- Get the previous epoch's last block header hash nonce
    let previousLabNonce =
          Consensus.previousLabNonce
            (Consensus.getPraosNonces (Proxy @(Api.ConsensusProtocol era)) chainDepState)
        -- 'ppExtraEntropyL' requires 'ProtVerAtMost 6' (Shelley to Alonzo), evidence
        -- that 'forEraInEon' cannot provide in its negative branch, so we match on
        -- 'ShelleyBasedEra' directly to obtain it for the pre-Babbage eras.
        extraEntropy :: Nonce
        extraEntropy = case sbe of
          ShelleyBasedEraShelley -> pp ^. Core.ppExtraEntropyL
          ShelleyBasedEraAllegra -> pp ^. Core.ppExtraEntropyL
          ShelleyBasedEraMary -> pp ^. Core.ppExtraEntropyL
          ShelleyBasedEraAlonzo -> pp ^. Core.ppExtraEntropyL
          ShelleyBasedEraBabbage -> Ledger.NeutralNonce
          ShelleyBasedEraConway -> Ledger.NeutralNonce
          ShelleyBasedEraDijkstra -> Ledger.NeutralNonce

        nextEpochsNonce = candidateNonce ⭒ previousLabNonce ⭒ extraEntropy

    -- Then we get the "mark" snapshot. This snapshot will be used for the next
    -- epoch's leadership schedule.
    CurrentEpochState cEstate <-
      first LeaderErrDecodeProtocolEpochStateFailure $
        decodeCurrentEpochState sbe serCurrEpochState

    let snapshot :: ShelleyAPI.SnapShot
        snapshot = SL.msSnapShot . ShelleyAPI.ssStakeMark $ ShelleyAPI.esSnapshots cEstate
        markSnapshotPoolDistr
          :: Map
               (SL.KeyHash SL.StakePool)
               SL.IndividualPoolStake
        markSnapshotPoolDistr = ShelleyAPI.unPoolDistr . ShelleyAPI.calculatePoolDistr $ snapshot

    let slotRangeOfInterest :: Core.EraPParams ledgerera => Core.PParams ledgerera -> Set SlotNo
        slotRangeOfInterest pp' =
          Set.filter
            (not . Ledger.isOverlaySlot firstSlotOfEpoch (pp' ^. Core.ppDG))
            $ fromList [firstSlotOfEpoch .. lastSlotofEpoch]

    forEraInEon
      (toCardanoEra sbe)
      ( shelleyBasedEraConstraints sbe $
          isLeadingSlotsTPraos (slotRangeOfInterest pp) poolid markSnapshotPoolDistr nextEpochsNonce vrfSkey f
      )
      ( \w ->
          babbageEraOnwardsConstraints w $
            isLeadingSlotsPraos (slotRangeOfInterest pp) poolid markSnapshotPoolDistr nextEpochsNonce vrfSkey f
      )
 where
  globals = shelleyBasedEraConstraints sbe $ constructGlobals sGen eInfo

  f :: Ledger.ActiveSlotCoeff
  f = activeSlotCoeff globals

-- | Return slots a given stake pool operator is leading.
-- See Leader Value Calculation in the Shelley ledger specification.
-- We need the certified natural value from the VRF, active slot coefficient
-- and the stake proportion of the stake pool.
isLeadingSlotsTPraos
  :: forall v
   . ()
  => Crypto.Signable v Ledger.Seed
  => Crypto.VRFAlgorithm v
  => Crypto.ContextVRF v ~ ()
  => Set SlotNo
  -> PoolId
  -> Map
       (SL.KeyHash SL.StakePool)
       SL.IndividualPoolStake
  -> Consensus.Nonce
  -> Crypto.SignKeyVRF v
  -> Ledger.ActiveSlotCoeff
  -> Either LeadershipError (Set SlotNo)
isLeadingSlotsTPraos slotRangeOfInterest poolid snapshotPoolDistr eNonce vrfSkey activeSlotCoeff' = do
  let StakePoolKeyHash poolHash = poolid

  let certifiedVrf s = Crypto.evalCertified () (TPraos.mkSeed TPraos.seedL s eNonce) vrfSkey

  stakePoolStake <-
    ShelleyAPI.individualPoolStake
      <$> Map.lookup poolHash snapshotPoolDistr
      & note (LeaderErrStakePoolHasNoStake poolid)

  let isLeader s = TPraos.checkLeaderValue (Crypto.certifiedOutput (certifiedVrf s)) stakePoolStake activeSlotCoeff'

  return $ Set.filter isLeader slotRangeOfInterest

isLeadingSlotsPraos
  :: ()
  => Set SlotNo
  -> PoolId
  -> Map
       (SL.KeyHash SL.StakePool)
       SL.IndividualPoolStake
  -> Consensus.Nonce
  -> Crypto.SignKeyVRF (Crypto.VRF Consensus.StandardCrypto)
  -> Ledger.ActiveSlotCoeff
  -> Either LeadershipError (Set SlotNo)
isLeadingSlotsPraos slotRangeOfInterest poolid snapshotPoolDistr eNonce vrfSkey activeSlotCoeff' = do
  let StakePoolKeyHash poolHash = poolid

  stakePoolStake <-
    note (LeaderErrStakePoolHasNoStake poolid) $
      ShelleyAPI.individualPoolStake <$> Map.lookup poolHash snapshotPoolDistr

  let isLeader slotNo = checkLeaderNatValue certifiedNatValue stakePoolStake activeSlotCoeff'
       where
        rho = VRF.evalCertified () (mkInputVRF slotNo eNonce) vrfSkey
        certifiedNatValue = vrfLeaderValue (Proxy @Consensus.StandardCrypto) rho

  Right $ Set.filter isLeader slotRangeOfInterest

-- | Return the slots at which a particular stake pool operator is
-- expected to mint a block.
currentEpochEligibleLeadershipSlots
  :: forall era
   . ()
  => ShelleyBasedEra era
  -> ShelleyGenesis
  -> EpochInfo (Either Text)
  -> Ledger.PParams (ShelleyLedgerEra era)
  -> ProtocolState era
  -> PoolId
  -> SigningKey VrfKey
  -> Serialised (PoolDistribution era)
  -> EpochNo
  -- ^ Current EpochInfo
  -> Either LeadershipError (Set SlotNo)
currentEpochEligibleLeadershipSlots sbe sGen eInfo pp ptclState poolid (VrfSigningKey vrkSkey) serPoolDistr currentEpoch =
  shelleyBasedEraConstraints sbe $ do
    chainDepState :: ChainDepState (Api.ConsensusProtocol era) <-
      first LeaderErrDecodeProtocolStateFailure $ decodeProtocolState ptclState

    -- We use the current epoch's nonce for the current leadership schedule
    -- calculation because the TICKN transition updates the epoch nonce
    -- at the start of the epoch.
    let epochNonce :: Nonce =
          Consensus.epochNonce (Consensus.getPraosNonces (Proxy @(Api.ConsensusProtocol era)) chainDepState)

    (firstSlotOfEpoch, lastSlotofEpoch) :: (SlotNo, SlotNo) <-
      first LeaderErrSlotRangeCalculationFailure $
        Slot.epochInfoRange eInfo currentEpoch

    -- FUCKED UP HERE:
    setSnapshotPoolDistr <-
      first LeaderErrDecodePoolDistributionFailure
        . fmap (SL.unPoolDistr . unPoolDistr)
        $ decodePoolDistribution sbe serPoolDistr

    let slotRangeOfInterest :: Core.EraPParams ledgerera => Core.PParams ledgerera -> Set SlotNo
        slotRangeOfInterest pp' =
          Set.filter
            (not . Ledger.isOverlaySlot firstSlotOfEpoch (pp' ^. Core.ppDG))
            $ fromList [firstSlotOfEpoch .. lastSlotofEpoch]

    forEraInEon
      (toCardanoEra sbe)
      ( shelleyBasedEraConstraints sbe $
          isLeadingSlotsTPraos (slotRangeOfInterest pp) poolid setSnapshotPoolDistr epochNonce vrkSkey f
      )
      ( \w ->
          babbageEraOnwardsConstraints w $
            isLeadingSlotsPraos (slotRangeOfInterest pp) poolid setSnapshotPoolDistr epochNonce vrkSkey f
      )
 where
  globals = shelleyBasedEraConstraints sbe $ constructGlobals sGen eInfo

  f :: Ledger.ActiveSlotCoeff
  f = activeSlotCoeff globals

-- TODO remove me?
constructGlobals
  :: ShelleyGenesis
  -> EpochInfo (Either Text)
  -> Globals
constructGlobals = Ledger.mkShelleyGlobals

--------------------------------------------------------------------------

-- | Type isomorphic to bool, representing condition check result
data ConditionResult
  = ConditionNotMet
  | ConditionMet
  deriving (Read, Show, Enum, Bounded, Ord, Eq)

toConditionResult :: Bool -> ConditionResult
toConditionResult False = ConditionNotMet
toConditionResult True = ConditionMet

fromConditionResult :: ConditionResult -> Bool
fromConditionResult ConditionNotMet = False
fromConditionResult ConditionMet = True

data AnyNewEpochState where
  AnyNewEpochState
    :: ShelleyBasedEra era
    -> ShelleyAPI.NewEpochState (ShelleyLedgerEra era)
    -> Ledger.LedgerTables
         (Consensus.CardanoBlock Consensus.StandardCrypto)
         Ledger.ValuesMK
    -> AnyNewEpochState

instance Show AnyNewEpochState where
  showsPrec p (AnyNewEpochState sbe ledgerNewEpochState _) =
    shelleyBasedEraConstraints sbe $ showsPrec p ledgerNewEpochState

getLedgerTablesUTxOValues
  :: forall era
   . ShelleyBasedEra era
  -> Ledger.LedgerTables
       (Consensus.CardanoBlock Consensus.StandardCrypto)
       Ledger.ValuesMK
  -> Map TxIn (TxOut CtxUTxO era)
getLedgerTablesUTxOValues sbe tbs =
  let
    ejectTables
      :: Index
           (Consensus.CardanoEras Consensus.StandardCrypto)
           (Shelley.ShelleyBlock proto (ShelleyLedgerEra era))
      -> Map TxIn (TxOut CtxUTxO era)
    ejectTables idx =
      let Ledger.LedgerTables (Ledger.ValuesMK values) = HFC.ejectLedgerTables idx tbs
       in Map.mapKeys (fromShelleyTxIn . Shelley.getOriginalTxIn) $ Map.map (fromShelleyTxOut sbe) values
   in
    case sbe of
      ShelleyBasedEraShelley -> ejectTables (IS IZ)
      ShelleyBasedEraAllegra -> ejectTables (IS (IS IZ))
      ShelleyBasedEraMary -> ejectTables (IS (IS (IS IZ)))
      ShelleyBasedEraAlonzo -> ejectTables (IS (IS (IS (IS IZ))))
      ShelleyBasedEraBabbage -> ejectTables (IS (IS (IS (IS (IS IZ)))))
      ShelleyBasedEraConway -> ejectTables (IS (IS (IS (IS (IS (IS IZ))))))
      ShelleyBasedEraDijkstra -> ejectTables (IS (IS (IS (IS (IS (IS (IS IZ)))))))

-- | Reconstructs the ledger's new epoch state and applies a supplied condition to it for every block. This
-- function only terminates if the condition is met or we have reached the termination epoch. We need to
-- provide a termination epoch otherwise blocks would be applied indefinitely.
foldEpochState
  :: forall t m s
   . HasCallStack
  => MonadIOTransError FoldBlocksError t m
  => SomeHasFS IO
  -- ^ Filesystem capability used by Ledger for testing/benchmarking,
  -- to stream Shelley/Conway genesis data from the extraConfig.
  -> NodeConfigFile 'In
  -- ^ Path to the cardano-node config file (e.g. <path to cardano-node project>/configuration/cardano/mainnet-config.json)
  -> SocketPath
  -- ^ Path to local cardano-node socket. This is the path specified by the @--socket-path@ command line option when running the node.
  -> ValidationMode
  -> EpochNo
  -- ^ Termination epoch
  -> s
  -- ^ an initial value for the condition state
  -> ( AnyNewEpochState
       -> SlotNo
       -> BlockNo
       -> StateT s IO ConditionResult
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
  -> t m (ConditionResult, s)
  -- ^ The final state
foldEpochState fs nodeConfigFilePath socketPath validationMode terminationEpoch initialResult checkCondition = handleExceptions $ do
  -- NOTE this was originally implemented with a non-pipelined client then
  -- changed to a pipelined client for a modest speedup:
  --  * Non-pipelined: 1h  0m  19s
  --  * Pipelined:        46m  23s

  (env, ledgerState) <-
    modifyError FoldBlocksInitialLedgerStateError $
      initialLedgerState fs nodeConfigFilePath

  -- Place to store the accumulated state
  -- This is a bit ugly, but easy.
  errorIORef <- modifyError FoldBlocksIOException . liftIO $ newIORef Nothing
  -- This needs to be a full MVar by default. It serves as a mutual exclusion lock when executing
  -- 'checkCondition' to ensure that states 's' are processed in order. This is assured by MVar fairness.
  stateMv <- modifyError FoldBlocksIOException . liftIO $ newMVar (ConditionNotMet, initialResult)

  -- Derive the NetworkId as described in network-magic.md from the
  -- cardano-ledger-specs repo.
  let byronConfig =
        (\(Consensus.WrapPartialLedgerConfig (Consensus.ByronPartialLedgerConfig bc _) :* _) -> bc)
          . HFC.getPerEraLedgerConfig
          . HFC.hardForkLedgerConfigPerEra
          $ envLedgerConfig env

      networkMagic =
        NetworkMagic $
          unProtocolMagicId $
            Cardano.Chain.Genesis.gdProtocolMagicId $
              Cardano.Chain.Genesis.configGenesisData byronConfig

      networkId' = case Cardano.Chain.Genesis.configReqNetMagic byronConfig of
        RequiresNoMagic -> Mainnet
        RequiresMagic -> Testnet networkMagic

      cardanoModeParams = CardanoModeParams . EpochSlots $ 10 * envSecurityParam env

  -- Connect to the node.
  let connectInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = cardanoModeParams
          , localNodeNetworkId = networkId'
          , localNodeSocketPath = socketPath
          }

  modifyError FoldBlocksIOException $
    liftIO $
      connectToLocalNode
        connectInfo
        (protocols stateMv errorIORef env ledgerState)

  liftIO (readIORef errorIORef) >>= \case
    Just err -> throwError $ FoldBlocksApplyBlockError err
    Nothing -> modifyError FoldBlocksIOException . liftIO $ readMVar stateMv
 where
  protocols
    :: ()
    => MVar (ConditionResult, s)
    -> IORef (Maybe LedgerStateError)
    -> Env
    -> LedgerState
    -> LocalNodeClientProtocolsInMode
  protocols stateMv errorIORef env ledgerState =
    LocalNodeClientProtocols
      { localChainSyncClient =
          LocalChainSyncClientPipelined (chainSyncClient 50 stateMv errorIORef env ledgerState)
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

  -- \| Defines the client side of the chain sync protocol.
  chainSyncClient
    :: Word16
    -- \^ The maximum number of concurrent requests.
    -> MVar (ConditionResult, s)
    -- \^ State accumulator. Written to on every block.
    -> IORef (Maybe LedgerStateError)
    -- \^ Resulting error if any. Written to once on protocol
    -- completion.
    -> Env
    -> LedgerState
    -> CSP.ChainSyncClientPipelined
         BlockInMode
         ChainPoint
         ChainTip
         IO
         ()
  -- \^ Client returns maybe an error.
  chainSyncClient pipelineSize stateMv errorIORef' env ledgerState0 =
    CSP.ChainSyncClientPipelined $
      pure $
        clientIdle_RequestMoreN Origin Origin Zero initialLedgerStateHistory
   where
    initialLedgerStateHistory = Seq.singleton (0, (ledgerState0, []), Origin)

    clientIdle_RequestMoreN
      :: WithOrigin BlockNo
      -> WithOrigin BlockNo
      -> Nat n -- Number of requests inflight.
      -> LedgerStateHistory
      -> CSP.ClientPipelinedStIdle n BlockInMode ChainPoint ChainTip IO ()
    clientIdle_RequestMoreN clientTip serverTip n knownLedgerStates =
      case pipelineDecisionMax pipelineSize n clientTip serverTip of
        Collect -> case n of
          Succ predN -> CSP.CollectResponse Nothing (clientNextN predN knownLedgerStates)
        _ ->
          CSP.SendMsgRequestNextPipelined
            (pure ())
            (clientIdle_RequestMoreN clientTip serverTip (Succ n) knownLedgerStates)

    clientNextN
      :: Nat n -- Number of requests inflight.
      -> LedgerStateHistory
      -> CSP.ClientStNext n BlockInMode ChainPoint ChainTip IO ()
    clientNextN n knownLedgerStates =
      CSP.ClientStNext
        { CSP.recvMsgRollForward = \blockInMode@(BlockInMode era block) serverChainTip -> do
            let BlockHeader slotNo _ currBlockNo = getBlockHeader block
                newLedgerStateE =
                  applyBlock
                    env
                    ( maybe
                        (error "Impossible! Missing Ledger state")
                        (\(_, (ledgerState, _), _) -> ledgerState)
                        (Seq.lookup 0 knownLedgerStates)
                    )
                    validationMode
                    blockInMode
            case forEraMaybeEon era of
              Nothing ->
                let !err = Just ByronEraUnsupported
                 in clientIdle_DoneNwithMaybeError n err
              Just sbe ->
                case newLedgerStateE of
                  Left err -> clientIdle_DoneNwithMaybeError n (Just err)
                  Right new@(newLedgerState, ledgerEvents) -> do
                    let (knownLedgerStates', _) = pushLedgerState env knownLedgerStates slotNo new blockInMode
                        newClientTip = At currBlockNo
                        newServerTip = fromChainTip serverChainTip
                    case getNewEpochState sbe $ clsState newLedgerState of
                      Left e ->
                        let !err = Just e
                         in clientIdle_DoneNwithMaybeError n err
                      Right lState -> do
                        let newEpochState = AnyNewEpochState sbe lState (clsTables newLedgerState)
                        -- Run the condition function in an exclusive lock.
                        -- There can be only one place where `takeMVar stateMv` exists otherwise this
                        -- code will deadlock!
                        condition <- bracket (takeMVar stateMv) (tryPutMVar stateMv) $ \(_prevCondition, previousState) -> do
                          updatedState@(!newCondition, !_) <-
                            runStateT (checkCondition newEpochState slotNo currBlockNo) previousState
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
      CSP.ClientStNext
        { CSP.recvMsgRollForward = \_ _ -> clientIdle_DoneNwithMaybeError n errorMay
        , CSP.recvMsgRollBackward = \_ _ -> clientIdle_DoneNwithMaybeError n errorMay
        }

    fromChainTip :: ChainTip -> WithOrigin BlockNo
    fromChainTip ct = case ct of
      ChainTipAtGenesis -> Origin
      ChainTip _ _ bno -> At bno

atTerminationEpoch :: EpochNo -> [LedgerEvent] -> Maybe EpochNo
atTerminationEpoch terminationEpoch events =
  listToMaybe
    [ currentEpoch'
    | PoolReap poolReapDets <- events
    , let currentEpoch' = prdEpochNo poolReapDets
    , currentEpoch' >= terminationEpoch
    ]

handleExceptions
  :: MonadIOTransError FoldBlocksError t m
  => ExceptT FoldBlocksError IO a
  -> t m a
handleExceptions = liftEither <=< liftIO . runExceptT . flip catches handlers
 where
  handlers =
    [ Handler $ throwError . FoldBlocksIOException
    , Handler $ throwError . FoldBlocksMuxError
    ]
