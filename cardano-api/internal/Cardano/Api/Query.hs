{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}


-- | Queries from local clients to the node.
--
module Cardano.Api.Query (

    -- * Queries
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),
    QueryUTxOFilter(..),
    UTxO(..),
    UTxOInAnyEra(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,

    -- * Wrapper types used in queries
    SerialisedDebugLedgerState(..),
    ProtocolState(..),
    decodeProtocolState,

    DebugLedgerState(..),
    decodeDebugLedgerState,

    SerialisedCurrentEpochState(..),
    CurrentEpochState(..),
    decodeCurrentEpochState,

    SerialisedPoolState(..),
    PoolState(..),
    decodePoolState,

    SerialisedPoolDistribution(..),
    PoolDistribution(..),
    decodePoolDistribution,

    SerialisedStakeSnapshots(..),
    StakeSnapshot(..),
    decodeStakeSnapshot,

    EraHistory(..),
    SystemStart(..),

    LedgerEpochInfo(..),
    toLedgerEpochInfo,

    SlotsInEpoch(..),
    SlotsToEpochEnd(..),

    slotToEpoch,

    LedgerState(..),

    getProgress,
    getSlotForRelativeTime,

    -- * Internal conversion functions
    toLedgerUTxO,
    fromLedgerUTxO,
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Case
import           Cardano.Api.Eras.Core
import           Cardano.Api.GenesisParameters
import           Cardano.Api.IPC.Version
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query.Types
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.Tx.Body
import           Cardano.Api.Value

import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Api.State.Query as L
import           Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.CertState as L
import qualified Cardano.Ledger.Credential as Shelley
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import           Cardano.Slotting.EpochInfo (hoistEpochInfo)
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Cardano.Slotting.Time (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime, SlotLength)
import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import qualified Ouroboros.Consensus.HardFork.History as History
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Consensus.Ledger.Query as Consensus
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import           Control.Monad.Trans.Except
import           Data.Aeson (FromJSON (..), ToJSON (..), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (Parser)
import           Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy as LBS
import           Data.Either.Combinators (rightToMaybe)
import qualified Data.HashMap.Strict as HMS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.SOP.Constraint (SListI)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)
import           GHC.Stack


-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode result where
  QueryCurrentEra
    :: QueryInMode AnyCardanoEra

  QueryInEra
    :: QueryInEra era result
    -> QueryInMode (Either EraMismatch result)

  QueryEraHistory
    :: QueryInMode EraHistory

  QuerySystemStart
    :: QueryInMode SystemStart

  QueryChainBlockNo
    :: QueryInMode (WithOrigin BlockNo)

  QueryChainPoint
    :: QueryInMode ChainPoint

instance NodeToClientVersionOf (QueryInMode result) where
  nodeToClientVersionOf = \case
    QueryCurrentEra   -> NodeToClientV_9
    QueryInEra q      -> nodeToClientVersionOf q
    QueryEraHistory   -> NodeToClientV_9
    QuerySystemStart  -> NodeToClientV_9
    QueryChainBlockNo -> NodeToClientV_10
    QueryChainPoint   -> NodeToClientV_10

data EraHistory where
  EraHistory
    :: Consensus.CardanoBlock L.StandardCrypto ~ Consensus.HardForkBlock xs
    => History.Interpreter xs
    -> EraHistory

getProgress :: ()
  => SlotNo
  -> EraHistory
  -> Either Qry.PastHorizonException (RelativeTime, SlotLength)
getProgress slotNo (EraHistory interpreter) = Qry.interpretQuery interpreter (Qry.slotToWallclock slotNo)

-- | Returns the slot number for provided relative time from 'SystemStart'
getSlotForRelativeTime :: ()
  => RelativeTime
  -> EraHistory
  -> Either Qry.PastHorizonException SlotNo
getSlotForRelativeTime relTime (EraHistory interpreter) = do
  (slotNo, _, _) <- Qry.interpretQuery interpreter $ Qry.wallclockToSlot relTime
  pure slotNo

newtype LedgerEpochInfo = LedgerEpochInfo { unLedgerEpochInfo :: Consensus.EpochInfo (Either Text) }

toLedgerEpochInfo :: ()
  => EraHistory
  -> LedgerEpochInfo
toLedgerEpochInfo (EraHistory interpreter) =
  LedgerEpochInfo $ hoistEpochInfo (first (Text.pack . show) . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

newtype SlotsInEpoch = SlotsInEpoch Word64

newtype SlotsToEpochEnd = SlotsToEpochEnd Word64

slotToEpoch :: ()
  => SlotNo
  -> EraHistory
  -> Either Qry.PastHorizonException (EpochNo, SlotsInEpoch, SlotsToEpochEnd)
slotToEpoch slotNo (EraHistory interpreter) = case Qry.interpretQuery interpreter (Qry.slotToEpoch slotNo) of
  Right (epochNumber, slotsInEpoch, slotsToEpochEnd) -> Right (epochNumber, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd)
  Left e -> Left e

deriving instance Show (QueryInMode result)

data QueryInEra era result where
     QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra era result
                            -> QueryInEra era result

instance NodeToClientVersionOf (QueryInEra era result) where
  nodeToClientVersionOf QueryByronUpdateState = NodeToClientV_9
  nodeToClientVersionOf (QueryInShelleyBasedEra _ q) = nodeToClientVersionOf q

deriving instance Show (QueryInEra era result)


data QueryInShelleyBasedEra era result where
  QueryEpoch
    :: QueryInShelleyBasedEra era EpochNo

  QueryGenesisParameters
    :: QueryInShelleyBasedEra era (GenesisParameters ShelleyEra)

  QueryProtocolParameters
    :: QueryInShelleyBasedEra era (Ledger.PParams (ShelleyLedgerEra era))

  QueryProtocolParametersUpdate
    :: QueryInShelleyBasedEra era
            (Map (Hash GenesisKey) ProtocolParametersUpdate)

  QueryStakeDistribution
    :: QueryInShelleyBasedEra era (Map (Hash StakePoolKey) Rational)

  QueryUTxO
    :: QueryUTxOFilter
    -> QueryInShelleyBasedEra era (UTxO era)

  QueryStakeAddresses
    :: Set StakeCredential
    -> NetworkId
    -> QueryInShelleyBasedEra era (Map StakeAddress Lovelace, Map StakeAddress PoolId)

  QueryStakePools
    :: QueryInShelleyBasedEra era (Set PoolId)

  QueryStakePoolParameters
    :: Set PoolId
    -> QueryInShelleyBasedEra era (Map PoolId StakePoolParameters)

     -- TODO: add support for RewardProvenance
     -- QueryPoolRanking
     --   :: QueryInShelleyBasedEra era RewardProvenance

  QueryDebugLedgerState
    :: QueryInShelleyBasedEra era (SerialisedDebugLedgerState era)

  QueryProtocolState
    :: QueryInShelleyBasedEra era (ProtocolState era)

  QueryCurrentEpochState
    :: QueryInShelleyBasedEra era (SerialisedCurrentEpochState era)

  QueryPoolState
    :: Maybe (Set PoolId)
    -> QueryInShelleyBasedEra era (SerialisedPoolState era)

  QueryPoolDistribution
    :: Maybe (Set PoolId)
    -> QueryInShelleyBasedEra era (SerialisedPoolDistribution era)

  QueryStakeSnapshot
    :: Maybe (Set PoolId)
    -> QueryInShelleyBasedEra era (SerialisedStakeSnapshots era)

  QueryStakeDelegDeposits
    :: Set StakeCredential
    -> QueryInShelleyBasedEra era (Map StakeCredential Lovelace)

  QueryConstitution
    :: QueryInShelleyBasedEra era (Maybe (L.Constitution (ShelleyLedgerEra era)))

  QueryGovState
    :: QueryInShelleyBasedEra era (L.GovState (ShelleyLedgerEra era))

  QueryDRepState
    :: Set (Shelley.Credential Shelley.DRepRole StandardCrypto)
    -> QueryInShelleyBasedEra era (Map (Shelley.Credential Shelley.DRepRole StandardCrypto) (L.DRepState StandardCrypto))

  QueryDRepStakeDistr
    :: Set (Ledger.DRep StandardCrypto)
    -> QueryInShelleyBasedEra era (Map (Ledger.DRep StandardCrypto) Lovelace)

  QueryCommitteeMembersState
    :: Set (Shelley.Credential Shelley.ColdCommitteeRole StandardCrypto)
    -> Set (Shelley.Credential Shelley.HotCommitteeRole StandardCrypto)
    -> Set L.MemberStatus
    -> QueryInShelleyBasedEra era (Maybe (L.CommitteeMembersState StandardCrypto))

  QueryStakeVoteDelegatees
    :: Set StakeCredential
    -> QueryInShelleyBasedEra era (Map StakeCredential (Ledger.DRep StandardCrypto))


-- | Mapping for queries in Shelley-based eras returning minimal node-to-client protocol versions. More
-- information about queries versioning can be found:
--   * https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-NodeToClient.html#t:NodeToClientVersion
--   * https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/QueryVersioning/#implementation
instance NodeToClientVersionOf (QueryInShelleyBasedEra era result) where
  nodeToClientVersionOf QueryEpoch = NodeToClientV_9
  nodeToClientVersionOf QueryGenesisParameters = NodeToClientV_9
  nodeToClientVersionOf QueryProtocolParameters = NodeToClientV_9
  nodeToClientVersionOf QueryProtocolParametersUpdate = NodeToClientV_9
  nodeToClientVersionOf QueryStakeDistribution = NodeToClientV_9
  nodeToClientVersionOf (QueryUTxO f) = nodeToClientVersionOf f
  nodeToClientVersionOf (QueryStakeAddresses _ _) = NodeToClientV_9
  nodeToClientVersionOf QueryStakePools = NodeToClientV_9
  nodeToClientVersionOf (QueryStakePoolParameters _) = NodeToClientV_9
  nodeToClientVersionOf QueryDebugLedgerState = NodeToClientV_9
  nodeToClientVersionOf QueryProtocolState = NodeToClientV_9
  nodeToClientVersionOf QueryCurrentEpochState = NodeToClientV_9
  -- Babbage >= v13
  nodeToClientVersionOf (QueryPoolState _) = NodeToClientV_14
  nodeToClientVersionOf (QueryPoolDistribution _) = NodeToClientV_14
  nodeToClientVersionOf (QueryStakeSnapshot _) = NodeToClientV_14
  nodeToClientVersionOf (QueryStakeDelegDeposits _) = NodeToClientV_15
  -- Conway >= v16
  nodeToClientVersionOf QueryConstitution = NodeToClientV_16
  nodeToClientVersionOf QueryGovState = NodeToClientV_16
  nodeToClientVersionOf QueryDRepState{} = NodeToClientV_16
  nodeToClientVersionOf QueryDRepStakeDistr{} = NodeToClientV_16
  nodeToClientVersionOf QueryCommitteeMembersState{} = NodeToClientV_16
  nodeToClientVersionOf QueryStakeVoteDelegatees{} = NodeToClientV_16

deriving instance Show (QueryInShelleyBasedEra era result)


-- ----------------------------------------------------------------------------
-- Wrapper types used in queries
--

-- | Getting the /whole/ UTxO is obviously not efficient since the result can
-- be huge. Filtering by address is also not efficient because it requires a
-- linear search.
--
-- The 'QueryUTxOFilterByTxIn' is efficient since it fits with the structure of
-- the UTxO (which is indexed by 'TxIn').
--
data QueryUTxOFilter =
     -- | /O(n) time and space/ for utxo size n
     QueryUTxOWhole

     -- | /O(n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByAddress (Set AddressAny)

     -- | /O(m log n) time, O(m) space/ for utxo size n, and address set size m
   | QueryUTxOByTxIn (Set TxIn)
  deriving (Eq, Show)

instance NodeToClientVersionOf QueryUTxOFilter where
  nodeToClientVersionOf QueryUTxOWhole = NodeToClientV_9
  nodeToClientVersionOf (QueryUTxOByAddress _) = NodeToClientV_9
  nodeToClientVersionOf (QueryUTxOByTxIn _) = NodeToClientV_9

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show

newtype UTxO era = UTxO { unUTxO :: Map TxIn (TxOut CtxUTxO era) }
  deriving (Eq, Show)

data UTxOInAnyEra where
  UTxOInAnyEra :: CardanoEra era
               -> UTxO era
               -> UTxOInAnyEra

deriving instance Show UTxOInAnyEra

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m
  toEncoding (UTxO m) = toEncoding m

instance (IsShelleyBasedEra era, FromJSON (TxOut CtxUTxO era))
  => FromJSON (UTxO era) where
    parseJSON = withObject "UTxO" $ \hm -> do
      let l = HMS.toList $ KeyMap.toHashMapText hm
      res <- mapM toTxIn l
      pure . UTxO $ Map.fromList res
     where
      toTxIn :: (Text, Aeson.Value) -> Parser (TxIn, TxOut CtxUTxO era)
      toTxIn (txinText, txOutVal) = do
        (,) <$> parseJSON (Aeson.String txinText)
            <*> parseJSON txOutVal

newtype SerialisedDebugLedgerState era
  = SerialisedDebugLedgerState (Serialised (Shelley.NewEpochState (ShelleyLedgerEra era)))

decodeDebugLedgerState :: forall era. ()
  => FromCBOR (DebugLedgerState era)
  => SerialisedDebugLedgerState era
  -> Either (LBS.ByteString, DecoderError) (DebugLedgerState era)
decodeDebugLedgerState (SerialisedDebugLedgerState (Serialised ls)) =
  first (ls,) (Plain.decodeFull ls)

newtype ProtocolState era
  = ProtocolState (Serialised (Consensus.ChainDepState (ConsensusProtocol era)))

-- ChainDepState can use Praos or TPraos crypto
decodeProtocolState
  :: FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  => ProtocolState era
  -> Either (LBS.ByteString, DecoderError) (Consensus.ChainDepState (ConsensusProtocol era))
decodeProtocolState (ProtocolState (Serialised pbs)) = first (pbs,) $ Plain.decodeFull pbs

newtype SerialisedCurrentEpochState era
  = SerialisedCurrentEpochState (Serialised (Shelley.EpochState (ShelleyLedgerEra era)))

newtype CurrentEpochState era = CurrentEpochState (Shelley.EpochState (ShelleyLedgerEra era))

decodeCurrentEpochState
  :: ShelleyBasedEra era
  -> SerialisedCurrentEpochState era
  -> Either DecoderError (CurrentEpochState era)
decodeCurrentEpochState sbe (SerialisedCurrentEpochState (Serialised ls)) =
  shelleyBasedEraConstraints sbe $ CurrentEpochState <$> Plain.decodeFull ls

newtype SerialisedPoolState era
  = SerialisedPoolState (Serialised (Shelley.PState (ShelleyLedgerEra era)))

newtype PoolState era = PoolState (Shelley.PState (ShelleyLedgerEra era))

decodePoolState
  :: forall era. ()
  => Core.Era (ShelleyLedgerEra era)
  => DecCBOR (Shelley.PState (ShelleyLedgerEra era))
  => SerialisedPoolState era
  -> Either DecoderError (PoolState era)
decodePoolState (SerialisedPoolState (Serialised ls)) =
  PoolState <$> decodeFull (Core.eraProtVerLow @(ShelleyLedgerEra era)) ls

newtype SerialisedPoolDistribution era
  = SerialisedPoolDistribution (Serialised (Shelley.PoolDistr (Core.EraCrypto (ShelleyLedgerEra era))))

newtype PoolDistribution era = PoolDistribution
  { unPoolDistr :: Shelley.PoolDistr (Core.EraCrypto (ShelleyLedgerEra era))
  }

decodePoolDistribution
  :: forall era. (Crypto (Core.EraCrypto (ShelleyLedgerEra era)))
  => ShelleyBasedEra era
  -> SerialisedPoolDistribution era
  -> Either DecoderError (PoolDistribution era)
decodePoolDistribution sbe (SerialisedPoolDistribution (Serialised ls)) =
  PoolDistribution <$> decodeFull (eraProtVerLow sbe) ls

newtype SerialisedStakeSnapshots era
  = SerialisedStakeSnapshots (Serialised (Consensus.StakeSnapshots (Core.EraCrypto (ShelleyLedgerEra era))))

newtype StakeSnapshot era = StakeSnapshot (Consensus.StakeSnapshots (Core.EraCrypto (ShelleyLedgerEra era)))

decodeStakeSnapshot
  :: forall era. ()
  => FromCBOR (Consensus.StakeSnapshots (Core.EraCrypto (ShelleyLedgerEra era)))
  => SerialisedStakeSnapshots era
  -> Either DecoderError (StakeSnapshot era)
decodeStakeSnapshot (SerialisedStakeSnapshots (Serialised ls)) = StakeSnapshot <$> Plain.decodeFull ls

toShelleyAddrSet :: CardanoEra era
                 -> Set AddressAny
                 -> Set (Shelley.Addr Consensus.StandardCrypto)
toShelleyAddrSet era =
    Set.fromList
  . map toShelleyAddr
    -- Ignore any addresses that are not appropriate for the era,
    -- e.g. Shelley addresses in the Byron era, as these would not
    -- appear in the UTxO anyway.
  . mapMaybe (rightToMaybe . anyAddressInEra era)
  . Set.toList


toLedgerUTxO :: ()
  => ShelleyBasedEra era
  -> UTxO era
  -> Shelley.UTxO (ShelleyLedgerEra era)
toLedgerUTxO sbe (UTxO utxo) =
  shelleyBasedEraConstraints sbe
    $ Shelley.UTxO
    . Map.fromList
    . map (bimap toShelleyTxIn (toShelleyTxOut sbe))
    . Map.toList
    $ utxo

fromLedgerUTxO :: ()
  => ShelleyBasedEra era
  -> Shelley.UTxO (ShelleyLedgerEra era)
  -> UTxO era
fromLedgerUTxO sbe (Shelley.UTxO utxo) =
  shelleyBasedEraConstraints sbe
    $ UTxO
    . Map.fromList
    . map (bimap fromShelleyTxIn (fromShelleyTxOut sbe))
    . Map.toList
    $ utxo

fromShelleyPoolDistr :: Shelley.PoolDistr StandardCrypto
                     -> Map (Hash StakePoolKey) Rational
fromShelleyPoolDistr =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap StakePoolKeyHash Shelley.individualPoolStake)
  . Map.toList
  . Shelley.unPoolDistr

fromShelleyDelegations :: Map (Shelley.Credential Shelley.Staking StandardCrypto)
                              (Shelley.KeyHash Shelley.StakePool StandardCrypto)
                       -> Map StakeCredential PoolId
fromShelleyDelegations =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    -- In this case it may not be: the Ord instances for Shelley.Credential
    -- do not match the one for StakeCredential
    Map.fromList
  . map (bimap fromShelleyStakeCredential StakePoolKeyHash)
  . Map.toList

fromShelleyRewardAccounts :: Shelley.RewardAccounts Consensus.StandardCrypto
                          -> Map StakeCredential Lovelace
fromShelleyRewardAccounts =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap fromShelleyStakeCredential fromShelleyLovelace)
  . Map.toList


-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery :: forall block result. ()
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => QueryInMode result
  -> Some (Consensus.Query block)
toConsensusQuery QueryCurrentEra =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra

toConsensusQuery QueryEraHistory =
    Some $ Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter

toConsensusQuery QuerySystemStart = Some Consensus.GetSystemStart

toConsensusQuery QueryChainBlockNo = Some Consensus.GetChainBlockNo

toConsensusQuery QueryChainPoint = Some Consensus.GetChainPoint

toConsensusQuery (QueryInEra QueryByronUpdateState) =
  Some $ Consensus.BlockQuery $
    Consensus.QueryIfCurrentByron
      Consensus.GetUpdateInterfaceState

toConsensusQuery (QueryInEra (QueryInShelleyBasedEra sbe q)) =
  shelleyBasedEraConstraints sbe $ toConsensusQueryShelleyBased sbe q

toConsensusQueryShelleyBased :: forall era protocol block result. ()
  => ConsensusBlockForEra era ~ Consensus.ShelleyBlock protocol (ShelleyLedgerEra era)
  => Core.EraCrypto (ShelleyLedgerEra era) ~ Consensus.StandardCrypto
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => ShelleyBasedEra era
  -> QueryInShelleyBasedEra era result
  -> Some (Consensus.Query block)
toConsensusQueryShelleyBased sbe = \case
  QueryEpoch ->
    Some (consensusQueryInEraInMode era Consensus.GetEpochNo)

  QueryConstitution ->
    caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "toConsensusQueryShelleyBased: QueryConstitution is only available in the Conway era")
    (const $ Some (consensusQueryInEraInMode era Consensus.GetConstitution))
    sbe

  QueryGenesisParameters ->
    Some (consensusQueryInEraInMode era Consensus.GetGenesisConfig)

  QueryProtocolParameters ->
    Some (consensusQueryInEraInMode era Consensus.GetCurrentPParams)

  QueryProtocolParametersUpdate ->
    Some (consensusQueryInEraInMode era Consensus.GetProposedPParamsUpdates)

  QueryStakeDistribution ->
    Some (consensusQueryInEraInMode era Consensus.GetStakeDistribution)

  QueryUTxO QueryUTxOWhole ->
    Some (consensusQueryInEraInMode era Consensus.GetUTxOWhole)

  QueryUTxO (QueryUTxOByAddress addrs) ->
    Some (consensusQueryInEraInMode era (Consensus.GetUTxOByAddress addrs'))
    where
      addrs' :: Set (Shelley.Addr Consensus.StandardCrypto)
      addrs' = toShelleyAddrSet era addrs

  QueryUTxO (QueryUTxOByTxIn txins) ->
    Some (consensusQueryInEraInMode era (Consensus.GetUTxOByTxIn txins'))
    where
      txins' :: Set (Shelley.TxIn Consensus.StandardCrypto)
      txins' = Set.map toShelleyTxIn txins

  QueryStakeAddresses creds _nId ->
    Some (consensusQueryInEraInMode era
            (Consensus.GetFilteredDelegationsAndRewardAccounts creds'))
    where
      creds' :: Set (Shelley.Credential Shelley.Staking StandardCrypto)
      creds' = Set.map toShelleyStakeCredential creds

  QueryStakePools ->
    Some (consensusQueryInEraInMode era Consensus.GetStakePools)

  QueryStakePoolParameters poolids ->
    Some (consensusQueryInEraInMode era (Consensus.GetStakePoolParams poolids'))
    where
      poolids' :: Set (Shelley.KeyHash Shelley.StakePool Consensus.StandardCrypto)
      poolids' = Set.map unStakePoolKeyHash poolids

  QueryDebugLedgerState ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugNewEpochState))

  QueryProtocolState ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugChainDepState))

  QueryCurrentEpochState ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugEpochState))

  QueryPoolState poolIds ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR (Consensus.GetPoolState (Set.map unStakePoolKeyHash <$> poolIds))))

  QueryStakeSnapshot mPoolIds ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR (Consensus.GetStakeSnapshots (fmap (Set.map unStakePoolKeyHash) mPoolIds))))

  QueryPoolDistribution poolIds ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR (Consensus.GetPoolDistr (getPoolIds <$> poolIds))))
    where
      getPoolIds :: Set PoolId -> Set (Shelley.KeyHash Shelley.StakePool Consensus.StandardCrypto)
      getPoolIds = Set.map (\(StakePoolKeyHash kh) -> kh)

  QueryStakeDelegDeposits creds ->
    Some (consensusQueryInEraInMode era (Consensus.GetStakeDelegDeposits creds'))
    where
      creds' = Set.map toShelleyStakeCredential creds

  QueryGovState ->
    Some (consensusQueryInEraInMode era Consensus.GetGovState)

  QueryDRepState creds ->
    caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "toConsensusQueryShelleyBased: QueryDRepState is only available in the Conway era")
    (const $ Some (consensusQueryInEraInMode era (Consensus.GetDRepState creds)))
    sbe

  QueryDRepStakeDistr dreps ->
    caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "toConsensusQueryShelleyBased: QueryDRepStakeDistr is only available in the Conway era")
    (const $ Some (consensusQueryInEraInMode era (Consensus.GetDRepStakeDistr dreps)))
    sbe

  QueryCommitteeMembersState coldCreds hotCreds statuses ->
    caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "toConsensusQueryShelleyBased: QueryCommitteeMembersState is only available in the Conway era")
    (const $ Some (consensusQueryInEraInMode era (Consensus.GetCommitteeMembersState coldCreds hotCreds statuses)))
    sbe

  QueryStakeVoteDelegatees creds ->
    caseShelleyToBabbageOrConwayEraOnwards
    (const $ error "toConsensusQueryShelleyBased: QueryStakeVoteDelegatees is only available in the Conway era")
    (const $ Some (consensusQueryInEraInMode era
            (Consensus.GetFilteredVoteDelegatees creds')))
    sbe
    where
      creds' :: Set (Shelley.Credential Shelley.Staking StandardCrypto)
      creds' = Set.map toShelleyStakeCredential creds

  where
    era = shelleyBasedToCardanoEra sbe

consensusQueryInEraInMode
  :: forall era erablock modeblock result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => Consensus.CardanoBlock L.StandardCrypto ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => CardanoEra era
  -> Consensus.BlockQuery erablock  result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode era =
    Consensus.BlockQuery
  . case era of
      ByronEra    -> Consensus.QueryIfCurrentByron
      ShelleyEra  -> Consensus.QueryIfCurrentShelley
      AllegraEra  -> Consensus.QueryIfCurrentAllegra
      MaryEra     -> Consensus.QueryIfCurrentMary
      AlonzoEra   -> Consensus.QueryIfCurrentAlonzo
      BabbageEra  -> Consensus.QueryIfCurrentBabbage
      ConwayEra   -> Consensus.QueryIfCurrentConway

-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult :: forall block result result'. ()
  => HasCallStack
  => Consensus.CardanoBlock L.StandardCrypto ~ block
  => QueryInMode result
  -> Consensus.Query block result'
  -> result'
  -> result
fromConsensusQueryResult QueryEraHistory q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetInterpreter)
        -> EraHistory r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QuerySystemStart q' r' =
    case q' of
      Consensus.GetSystemStart
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryChainBlockNo q' r' =
    case q' of
      Consensus.GetChainBlockNo
        -> r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryChainPoint q' r' =
    case q' of
      Consensus.GetChainPoint
        -> fromConsensusPointHF r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult QueryCurrentEra q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetCurrentEra)
        -> fromConsensusEraIndex r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra QueryByronUpdateState) q' r' =
    case q' of
      Consensus.BlockQuery
        (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState)
        -> bimap fromConsensusEraMismatch ByronUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraShelley q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentShelley q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraShelley q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraAllegra q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAllegra q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraAllegra q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraMary q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentMary q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraMary q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraAlonzo q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentAlonzo q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraAlonzo q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraBabbage q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentBabbage q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraBabbage q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraConway q)) q' r' =
    case q' of
      Consensus.BlockQuery (Consensus.QueryIfCurrentConway q'')
        -> bimap fromConsensusEraMismatch
                 (fromConsensusQueryResultShelleyBased
                    ShelleyBasedEraConway q q'')
                 r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased
  :: forall era ledgerera protocol result result'.
     HasCallStack
  => ShelleyLedgerEra era ~ ledgerera
  => Core.EraCrypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusProtocol era ~ protocol
  => ShelleyBasedEra era
  -> QueryInShelleyBasedEra era result
  -> Consensus.BlockQuery (Consensus.ShelleyBlock protocol ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultShelleyBased _ QueryEpoch q' epoch =
    case q' of
      Consensus.GetEpochNo -> epoch
      _                    -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryConstitution q' mConstitution =
    case q' of
      Consensus.GetConstitution -> mConstitution
      _                         -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryGenesisParameters q' r' =
    case q' of
      Consensus.GetGenesisConfig -> fromShelleyGenesis
                                      (Consensus.getCompactGenesis r')
      _                          -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolParameters q' r' =
    case q' of
      Consensus.GetCurrentPParams -> r'
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased sbe QueryProtocolParametersUpdate q' r' =
    case q' of
      Consensus.GetProposedPParamsUpdates -> fromLedgerProposedPPUpdates sbe r'
      _                                   -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeDistribution q' r' =
    case q' of
      Consensus.GetStakeDistribution -> fromShelleyPoolDistr r'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased sbe (QueryUTxO QueryUTxOWhole) q' utxo' =
    case q' of
      Consensus.GetUTxOWhole -> fromLedgerUTxO sbe utxo'
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased sbe (QueryUTxO QueryUTxOByAddress{}) q' utxo' =
    case q' of
      Consensus.GetUTxOByAddress{} -> fromLedgerUTxO sbe utxo'
      _                            -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased sbe (QueryUTxO QueryUTxOByTxIn{}) q' utxo' =
    case q' of
      Consensus.GetUTxOByTxIn{} -> fromLedgerUTxO sbe utxo'
      _                         -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ (QueryStakeAddresses _ nId) q' r' =
    case q' of
      Consensus.GetFilteredDelegationsAndRewardAccounts{}
        -> let (delegs, rwaccs) = r'
           in ( Map.mapKeys (makeStakeAddress nId) $ fromShelleyRewardAccounts rwaccs
              , Map.mapKeys (makeStakeAddress nId) $ fromShelleyDelegations delegs
              )
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakePools q' poolids' =
    case q' of
      Consensus.GetStakePools -> Set.map StakePoolKeyHash poolids'
      _                       -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakePoolParameters{} q' poolparams' =
    case q' of
      Consensus.GetStakePoolParams{} -> Map.map fromShelleyPoolParams
                                      . Map.mapKeysMonotonic StakePoolKeyHash
                                      $ poolparams'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryDebugLedgerState{} q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugNewEpochState -> SerialisedDebugLedgerState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolState q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugChainDepState -> ProtocolState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryCurrentEpochState q' r' =
  case q' of
    Consensus.GetCBOR Consensus.DebugEpochState -> SerialisedCurrentEpochState r'
    _                                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryPoolState{} q' r' =
  case q' of
    Consensus.GetCBOR Consensus.GetPoolState {} -> SerialisedPoolState r'
    _                                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryPoolDistribution{} q' r' =
  case q' of
    Consensus.GetCBOR Consensus.GetPoolDistr {} -> SerialisedPoolDistribution r'
    _                                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeSnapshot{} q' r' =
  case q' of
    Consensus.GetCBOR Consensus.GetStakeSnapshots {} -> SerialisedStakeSnapshots r'
    _                                                -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeDelegDeposits{} q' stakeCreds' =
    case q' of
      Consensus.GetStakeDelegDeposits{} -> Map.map fromShelleyLovelace
                                         . Map.mapKeysMonotonic fromShelleyStakeCredential
                                         $ stakeCreds'
      _                                 -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryGovState{} q' govState' =
  case q' of
    Consensus.GetGovState{} -> govState'
    _                       -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryDRepState{} q' drepState'  =
  case q' of
    Consensus.GetDRepState{} -> drepState'
    _                        -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryDRepStakeDistr{} q' stakeDistr' =
  case q' of
    Consensus.GetDRepStakeDistr{} -> Map.map fromShelleyLovelace stakeDistr'
    _                             -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryCommitteeMembersState{} q' committeeMembersState' =
  case q' of
    Consensus.GetCommitteeMembersState{} -> committeeMembersState'
    _                                    -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeVoteDelegatees{} q' delegs' =
    case q' of
      Consensus.GetFilteredVoteDelegatees {}
        -> Map.mapKeys fromShelleyStakeCredential delegs'
      _ -> fromConsensusQueryResultMismatch

-- | This should /only/ happen if we messed up the mapping in 'toConsensusQuery'
-- and 'fromConsensusQueryResult' so they are inconsistent with each other.
--
-- If we do encounter this error it means that 'toConsensusQuery' maps a
-- API query constructor to a certain consensus query constructor but that
-- 'fromConsensusQueryResult' apparently expects a different pairing.
--
-- For example, imagine if 'toConsensusQuery would (incorrectly) map
-- 'QueryChainPoint' to 'Consensus.GetEpochNo' but 'fromConsensusQueryResult'
-- (correctly) expected to find 'Consensus.GetLedgerTip'. This mismatch would
-- trigger this error.
--
-- Such mismatches should be preventable with an appropriate property test.
--
fromConsensusQueryResultMismatch :: HasCallStack => a
fromConsensusQueryResultMismatch =
  withFrozenCallStack $
    error "fromConsensusQueryResult: internal query mismatch"


fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch
