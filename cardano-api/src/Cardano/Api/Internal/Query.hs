{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Queries from local clients to the node.
module Cardano.Api.Internal.Query
  ( -- * Queries
    QueryInMode (..)
  , QueryInEra (..)
  , QueryInShelleyBasedEra (..)
  , QueryUTxOFilter (..)
  , UTxOInAnyEra (..)

    -- * Internal conversion functions
  , toConsensusQuery
  , fromConsensusQueryResult

    -- * Wrapper types used in queries
  , SerialisedDebugLedgerState (..)
  , ProtocolState (..)
  , decodeProtocolState
  , DebugLedgerState (..)
  , decodeDebugLedgerState
  , SerialisedCurrentEpochState (..)
  , CurrentEpochState (..)
  , decodeCurrentEpochState
  , SerialisedPoolState (..)
  , PoolState (..)
  , decodePoolState
  , SerialisedPoolDistribution (..)
  , PoolDistribution (..)
  , decodePoolDistribution
  , SerialisedStakeSnapshots (..)
  , StakeSnapshot (..)
  , decodeStakeSnapshot
  , EraHistory (..)
  , SystemStart (..)
  , LedgerEpochInfo (..)
  , toLedgerEpochInfo
  , SlotsInEpoch (..)
  , SlotsToEpochEnd (..)
  , slotToEpoch
  , LedgerState (..)
  , getProgress
  , getSlotForRelativeTime
  , decodeBigLedgerPeerSnapshot

    -- * Internal conversion functions
  , toLedgerUTxO
  , fromLedgerUTxO
  )
where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Block
import Cardano.Api.Internal.Certificate
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras.Case
import Cardano.Api.Internal.Eras.Core
import Cardano.Api.Internal.GenesisParameters
import Cardano.Api.Internal.HasTypeProxy (HasTypeProxy (..), Proxy)
import Cardano.Api.Internal.Keys.Shelley
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.NetworkId
import Cardano.Api.Internal.Query.Types
import Cardano.Api.Internal.ReexposeLedger qualified as Ledger
import Cardano.Api.Internal.Serialise.Cbor (SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR))
import Cardano.Api.Internal.SerialiseTextEnvelope
  ( HasTextEnvelope (textEnvelopeType)
  , TextEnvelopeType
  )
import Cardano.Api.Internal.Tx.Body
import Cardano.Api.Internal.Tx.UTxO (UTxO (..))

import Cardano.Binary qualified as CBOR
import Cardano.Chain.Update.Validation.Interface qualified as Byron.Update
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Plain qualified as Plain
import Cardano.Ledger.CertState qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Credential qualified as Shelley
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Shelley.Core qualified as Core
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Cardano.Slotting.Slot (WithOrigin (..))
import Cardano.Slotting.Time (SystemStart (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime, SlotLength)
import Ouroboros.Consensus.Byron.Ledger qualified as Consensus
import Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras qualified as Consensus
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Consensus.HardFork.History qualified as History
import Ouroboros.Consensus.HardFork.History.Qry qualified as Qry
import Ouroboros.Consensus.Ledger.Query qualified as Consensus
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.Query.Types qualified as Consensus
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import Ouroboros.Network.Block (Serialised (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot)
import Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import Codec.Serialise qualified as CBOR
import Control.Monad.Trans.Except
import Data.Bifunctor (bimap, first)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either.Combinators (rightToMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.SOP.Constraint (SListI)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Singletons qualified as Singletons
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Exts (IsList (..))
import GHC.Stack

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
  QueryLedgerConfig
    :: QueryInMode (Consensus.CardanoLedgerConfig StandardCrypto)

data EraHistory where
  EraHistory
    :: Consensus.CardanoBlock StandardCrypto ~ Consensus.HardForkBlock xs
    => History.Interpreter xs
    -> EraHistory

instance HasTypeProxy EraHistory where
  data AsType EraHistory = AsEraHistory

  proxyToAsType :: Proxy EraHistory -> AsType EraHistory
  proxyToAsType _ = AsEraHistory

instance SerialiseAsCBOR EraHistory where
  serialiseToCBOR :: EraHistory -> BS.ByteString
  serialiseToCBOR (EraHistory interpreter) = CBOR.toStrictByteString (CBOR.encode interpreter)

  deserialiseFromCBOR :: AsType EraHistory -> BS.ByteString -> Either DecoderError EraHistory
  deserialiseFromCBOR _ bs =
    EraHistory
      <$> CBOR.decodeFullDecoder' "EraHistory" CBOR.decode bs

-- | The @HasTextEnvelope@ instance for @EraHistory@ is required by the
-- @transaction calculate-plutus-script-cost@ command in @cartdano-cli and it
-- can be obtained through the @query era-history@ command.
instance HasTextEnvelope EraHistory where
  textEnvelopeType :: AsType EraHistory -> TextEnvelopeType
  textEnvelopeType _ = "EraHistory"

getProgress
  :: ()
  => SlotNo
  -> EraHistory
  -> Either Qry.PastHorizonException (RelativeTime, SlotLength)
getProgress slotNo (EraHistory interpreter) = Qry.interpretQuery interpreter (Qry.slotToWallclock slotNo)

-- | Returns the slot number for provided relative time from 'SystemStart'
getSlotForRelativeTime
  :: ()
  => RelativeTime
  -> EraHistory
  -> Either Qry.PastHorizonException SlotNo
getSlotForRelativeTime relTime (EraHistory interpreter) = do
  (slotNo, _, _) <- Qry.interpretQuery interpreter $ Qry.wallclockToSlot relTime
  pure slotNo

newtype LedgerEpochInfo = LedgerEpochInfo {unLedgerEpochInfo :: Consensus.EpochInfo (Either Text)}

toLedgerEpochInfo
  :: ()
  => EraHistory
  -> LedgerEpochInfo
toLedgerEpochInfo (EraHistory interpreter) =
  LedgerEpochInfo $
    hoistEpochInfo (first (Text.pack . show) . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

newtype SlotsInEpoch = SlotsInEpoch Word64

newtype SlotsToEpochEnd = SlotsToEpochEnd Word64

slotToEpoch
  :: ()
  => SlotNo
  -> EraHistory
  -> Either Qry.PastHorizonException (EpochNo, SlotsInEpoch, SlotsToEpochEnd)
slotToEpoch slotNo (EraHistory interpreter) = case Qry.interpretQuery interpreter (Qry.slotToEpoch slotNo) of
  Right (epochNumber, slotsInEpoch, slotsToEpochEnd) -> Right (epochNumber, SlotsInEpoch slotsInEpoch, SlotsToEpochEnd slotsToEpochEnd)
  Left e -> Left e

deriving instance Show (QueryInMode result)

data QueryInEra era result where
  QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState
  QueryInShelleyBasedEra
    :: ShelleyBasedEra era
    -> QueryInShelleyBasedEra era result
    -> QueryInEra era result

deriving instance Show (QueryInEra era result)

data QueryInShelleyBasedEra era result where
  QueryEpoch
    :: QueryInShelleyBasedEra era EpochNo
  QueryGenesisParameters
    :: QueryInShelleyBasedEra era (GenesisParameters ShelleyEra)
  QueryProtocolParameters
    :: QueryInShelleyBasedEra era (Ledger.PParams (ShelleyLedgerEra era))
  QueryStakeDistribution
    :: QueryInShelleyBasedEra era (Map (Hash StakePoolKey) Rational)
  QueryUTxO
    :: QueryUTxOFilter
    -> QueryInShelleyBasedEra era (UTxO era)
  QueryStakeAddresses
    :: Set StakeCredential
    -> NetworkId
    -> QueryInShelleyBasedEra era (Map StakeAddress L.Coin, Map StakeAddress PoolId)
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
    -> QueryInShelleyBasedEra era (Map StakeCredential L.Coin)
  QueryAccountState
    :: QueryInShelleyBasedEra era L.AccountState
  QueryConstitution
    :: QueryInShelleyBasedEra era (L.Constitution (ShelleyLedgerEra era))
  QueryGovState
    :: QueryInShelleyBasedEra era (L.GovState (ShelleyLedgerEra era))
  QueryRatifyState
    :: QueryInShelleyBasedEra era (L.RatifyState (ShelleyLedgerEra era))
  QueryFuturePParams
    :: QueryInShelleyBasedEra era (Maybe (Core.PParams (ShelleyLedgerEra era)))
  QueryDRepState
    :: Set (Shelley.Credential Shelley.DRepRole)
    -> QueryInShelleyBasedEra
         era
         (Map (Shelley.Credential Shelley.DRepRole) L.DRepState)
  QueryDRepStakeDistr
    :: Set Ledger.DRep
    -> QueryInShelleyBasedEra era (Map Ledger.DRep L.Coin)
  QuerySPOStakeDistr
    :: Set (Ledger.KeyHash 'Ledger.StakePool)
    -> QueryInShelleyBasedEra era (Map (Ledger.KeyHash 'Ledger.StakePool) L.Coin)
  QueryCommitteeMembersState
    :: Set (Shelley.Credential Shelley.ColdCommitteeRole)
    -> Set (Shelley.Credential Shelley.HotCommitteeRole)
    -> Set L.MemberStatus
    -> QueryInShelleyBasedEra era L.CommitteeMembersState
  QueryStakeVoteDelegatees
    :: Set StakeCredential
    -> QueryInShelleyBasedEra era (Map StakeCredential Ledger.DRep)
  QueryProposals
    :: Set L.GovActionId
    -> QueryInShelleyBasedEra era (Seq (L.GovActionState (ShelleyLedgerEra era)))
  QueryLedgerPeerSnapshot
    :: QueryInShelleyBasedEra era (Serialised LedgerPeerSnapshot)
  QueryStakePoolDefaultVote
    :: Ledger.KeyHash 'Ledger.StakePool
    -> QueryInShelleyBasedEra era L.DefaultVote

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
data QueryUTxOFilter
  = -- | /O(n) time and space/ for utxo size n
    QueryUTxOWhole
  | -- | /O(n) time, O(m) space/ for utxo size n, and address set size m
    QueryUTxOByAddress (Set AddressAny)
  | -- | /O(m log n) time, O(m) space/ for utxo size n, and address set size m
    QueryUTxOByTxIn (Set TxIn)
  deriving (Eq, Show)

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show

data UTxOInAnyEra where
  UTxOInAnyEra
    :: CardanoEra era
    -> UTxO era
    -> UTxOInAnyEra

deriving instance Show UTxOInAnyEra

newtype SerialisedDebugLedgerState era
  = SerialisedDebugLedgerState (Serialised (Shelley.NewEpochState (ShelleyLedgerEra era)))

decodeDebugLedgerState
  :: forall era
   . ()
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
  :: forall era
   . ()
  => Core.Era (ShelleyLedgerEra era)
  => DecCBOR (Shelley.PState (ShelleyLedgerEra era))
  => SerialisedPoolState era
  -> Either DecoderError (PoolState era)
decodePoolState (SerialisedPoolState (Serialised ls)) =
  PoolState <$> decodeFull (Core.eraProtVerLow @(ShelleyLedgerEra era)) ls

newtype SerialisedPoolDistribution era
  = SerialisedPoolDistribution
      (Serialised (Consensus.PoolDistr StandardCrypto))

newtype PoolDistribution era = PoolDistribution
  { unPoolDistr :: Consensus.PoolDistr StandardCrypto
  }

decodePoolDistribution
  :: forall era
   . ShelleyBasedEra era
  -> SerialisedPoolDistribution era
  -> Either DecoderError (PoolDistribution era)
decodePoolDistribution sbe (SerialisedPoolDistribution (Serialised ls)) =
  PoolDistribution <$> decodeFull (eraProtVerLow sbe) ls

newtype SerialisedStakeSnapshots era
  = SerialisedStakeSnapshots
      (Serialised Consensus.StakeSnapshots)

newtype StakeSnapshot era = StakeSnapshot Consensus.StakeSnapshots

decodeStakeSnapshot
  :: forall era
   . SerialisedStakeSnapshots era
  -> Either DecoderError (StakeSnapshot era)
decodeStakeSnapshot (SerialisedStakeSnapshots (Serialised ls)) = StakeSnapshot <$> Plain.decodeFull ls

decodeBigLedgerPeerSnapshot
  :: Serialised LedgerPeerSnapshot
  -> Either (LBS.ByteString, DecoderError) LedgerPeerSnapshot
decodeBigLedgerPeerSnapshot (Serialised lps) = first (lps,) (Plain.decodeFull lps)

toShelleyAddrSet
  :: CardanoEra era
  -> Set AddressAny
  -> Set Shelley.Addr
toShelleyAddrSet era =
  fromList
    . map toShelleyAddr
    -- Ignore any addresses that are not appropriate for the era,
    -- e.g. Shelley addresses in the Byron era, as these would not
    -- appear in the UTxO anyway.
    . mapMaybe (rightToMaybe . anyAddressInEra era)
    . toList

toLedgerUTxO
  :: ()
  => ShelleyBasedEra era
  -> UTxO era
  -> Shelley.UTxO (ShelleyLedgerEra era)
toLedgerUTxO sbe (UTxO utxo) =
  shelleyBasedEraConstraints sbe
    $ Shelley.UTxO
      . fromList
      . map (bimap toShelleyTxIn (toShelleyTxOut sbe))
      . toList
    $ utxo

fromLedgerUTxO
  :: ()
  => ShelleyBasedEra era
  -> Shelley.UTxO (ShelleyLedgerEra era)
  -> UTxO era
fromLedgerUTxO sbe (Shelley.UTxO utxo) =
  shelleyBasedEraConstraints sbe
    $ UTxO
      . fromList
      . map (bimap fromShelleyTxIn (fromShelleyTxOut sbe))
      . toList
    $ utxo

fromShelleyPoolDistr
  :: Consensus.PoolDistr StandardCrypto
  -> Map (Hash StakePoolKey) Rational
fromShelleyPoolDistr =
  -- TODO: write an appropriate property to show it is safe to use
  -- Map.fromListAsc or to use Map.mapKeysMonotonic
  fromList
    . map (bimap StakePoolKeyHash Consensus.individualPoolStake)
    . toList
    . Consensus.unPoolDistr

fromShelleyDelegations
  :: Map
       (Shelley.Credential Shelley.Staking)
       (Shelley.KeyHash Shelley.StakePool)
  -> Map StakeCredential PoolId
fromShelleyDelegations =
  -- TODO: write an appropriate property to show it is safe to use
  -- Map.fromListAsc or to use Map.mapKeysMonotonic
  -- In this case it may not be: the Ord instances for Shelley.Credential
  -- do not match the one for StakeCredential
  fromList
    . map (bimap fromShelleyStakeCredential StakePoolKeyHash)
    . toList

fromShelleyRewardAccounts
  :: Map (Shelley.Credential 'Core.Staking) L.Coin
  -> Map StakeCredential L.Coin
fromShelleyRewardAccounts =
  -- TODO: write an appropriate property to show it is safe to use
  -- Map.fromListAsc or to use Map.mapKeysMonotonic
  fromList
    . map (first fromShelleyStakeCredential)
    . toList

-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery
  :: forall block result
   . ()
  => Consensus.CardanoBlock StandardCrypto ~ block
  => QueryInMode result
  -> Some (Consensus.Query block)
toConsensusQuery QueryCurrentEra =
  Some $
    Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetCurrentEra
toConsensusQuery QueryEraHistory =
  Some $
    Consensus.BlockQuery $
      Consensus.QueryHardFork
        Consensus.GetInterpreter
toConsensusQuery QuerySystemStart = Some Consensus.GetSystemStart
toConsensusQuery QueryChainBlockNo = Some Consensus.GetChainBlockNo
toConsensusQuery QueryChainPoint = Some Consensus.GetChainPoint
toConsensusQuery (QueryInEra QueryByronUpdateState) =
  Some $
    Consensus.BlockQuery $
      Consensus.QueryIfCurrentByron
        Consensus.GetUpdateInterfaceState
toConsensusQuery (QueryInEra (QueryInShelleyBasedEra sbe q)) =
  shelleyBasedEraConstraints sbe $ toConsensusQueryShelleyBased sbe q
toConsensusQuery QueryLedgerConfig = Some Consensus.DebugLedgerConfig

toConsensusQueryShelleyBased
  :: forall era protocol block result
   . ()
  => ConsensusBlockForEra era ~ Consensus.ShelleyBlock protocol (ShelleyLedgerEra era)
  => Consensus.CardanoBlock StandardCrypto ~ block
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
  QueryStakeDistribution ->
    Some (consensusQueryInEraInMode era Consensus.GetStakeDistribution)
  QueryUTxO QueryUTxOWhole ->
    Some (consensusQueryInEraInMode era Consensus.GetUTxOWhole)
  QueryUTxO (QueryUTxOByAddress addrs) ->
    Some (consensusQueryInEraInMode era (Consensus.GetUTxOByAddress addrs'))
   where
    addrs' :: Set Shelley.Addr
    addrs' = toShelleyAddrSet era addrs
  QueryUTxO (QueryUTxOByTxIn txins) ->
    Some (consensusQueryInEraInMode era (Consensus.GetUTxOByTxIn txins'))
   where
    txins' :: Set Shelley.TxIn
    txins' = Set.map toShelleyTxIn txins
  QueryStakeAddresses creds _nId ->
    Some
      ( consensusQueryInEraInMode
          era
          (Consensus.GetFilteredDelegationsAndRewardAccounts creds')
      )
   where
    creds' :: Set (Shelley.Credential Shelley.Staking)
    creds' = Set.map toShelleyStakeCredential creds
  QueryStakePools ->
    Some (consensusQueryInEraInMode era Consensus.GetStakePools)
  QueryStakePoolParameters poolids ->
    Some (consensusQueryInEraInMode era (Consensus.GetStakePoolParams poolids'))
   where
    poolids' :: Set (Shelley.KeyHash Shelley.StakePool)
    poolids' = Set.map unStakePoolKeyHash poolids
  QueryDebugLedgerState ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugNewEpochState))
  QueryProtocolState ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugChainDepState))
  QueryCurrentEpochState ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.DebugEpochState))
  QueryPoolState poolIds ->
    Some
      ( consensusQueryInEraInMode
          era
          (Consensus.GetCBOR (Consensus.GetPoolState (Set.map unStakePoolKeyHash <$> poolIds)))
      )
  QueryStakeSnapshot mPoolIds ->
    Some
      ( consensusQueryInEraInMode
          era
          (Consensus.GetCBOR (Consensus.GetStakeSnapshots (fmap (Set.map unStakePoolKeyHash) mPoolIds)))
      )
  QueryPoolDistribution poolIds ->
    Some
      (consensusQueryInEraInMode era (Consensus.GetCBOR (Consensus.GetPoolDistr (getPoolIds <$> poolIds))))
   where
    getPoolIds :: Set PoolId -> Set (Shelley.KeyHash Shelley.StakePool)
    getPoolIds = Set.map (\(StakePoolKeyHash kh) -> kh)
  QueryStakeDelegDeposits creds ->
    Some (consensusQueryInEraInMode era (Consensus.GetStakeDelegDeposits creds'))
   where
    creds' = Set.map toShelleyStakeCredential creds
  QueryAccountState ->
    Some (consensusQueryInEraInMode era Consensus.GetAccountState)
  QueryGovState ->
    Some (consensusQueryInEraInMode era Consensus.GetGovState)
  QueryRatifyState ->
    caseShelleyToBabbageOrConwayEraOnwards
      (const $ error "toConsensusQueryShelleyBased: QueryRatifyState is only available in the Conway era")
      (const $ Some (consensusQueryInEraInMode era Consensus.GetRatifyState))
      sbe
  QueryFuturePParams ->
    caseShelleyToBabbageOrConwayEraOnwards
      ( const $
          error "toConsensusQueryShelleyBased: QueryFuturePParams is only available in the Conway era onwards"
      )
      (const $ Some (consensusQueryInEraInMode era Consensus.GetFuturePParams))
      sbe
  QueryDRepState creds ->
    caseShelleyToBabbageOrConwayEraOnwards
      (const $ error "toConsensusQueryShelleyBased: QueryDRepState is only available in the Conway era")
      (const $ Some (consensusQueryInEraInMode era (Consensus.GetDRepState creds)))
      sbe
  QueryDRepStakeDistr dreps ->
    caseShelleyToBabbageOrConwayEraOnwards
      ( const $
          error "toConsensusQueryShelleyBased: QueryDRepStakeDistr is only available in the Conway era"
      )
      (const $ Some (consensusQueryInEraInMode era (Consensus.GetDRepStakeDistr dreps)))
      sbe
  QuerySPOStakeDistr spos ->
    caseShelleyToBabbageOrConwayEraOnwards
      ( const $
          error "toConsensusQueryShelleyBased: QuerySPOStakeDistr is only available in the Conway era"
      )
      (const $ Some (consensusQueryInEraInMode era (Consensus.GetSPOStakeDistr spos)))
      sbe
  QueryCommitteeMembersState coldCreds hotCreds statuses ->
    caseShelleyToBabbageOrConwayEraOnwards
      ( const $
          error "toConsensusQueryShelleyBased: QueryCommitteeMembersState is only available in the Conway era"
      )
      ( const $
          Some
            (consensusQueryInEraInMode era (Consensus.GetCommitteeMembersState coldCreds hotCreds statuses))
      )
      sbe
  QueryStakeVoteDelegatees creds ->
    caseShelleyToBabbageOrConwayEraOnwards
      ( const $
          error "toConsensusQueryShelleyBased: QueryStakeVoteDelegatees is only available in the Conway era"
      )
      ( const $
          Some
            ( consensusQueryInEraInMode
                era
                (Consensus.GetFilteredVoteDelegatees creds')
            )
      )
      sbe
   where
    creds' :: Set (Shelley.Credential Shelley.Staking)
    creds' = Set.map toShelleyStakeCredential creds
  QueryProposals govActs ->
    caseShelleyToBabbageOrConwayEraOnwards
      ( const $
          error "toConsensusQueryShelleyBased: QueryProposals is only available in the Conway era"
      )
      ( const $
          Some
            (consensusQueryInEraInMode era (Consensus.GetProposals govActs))
      )
      sbe
  QueryLedgerPeerSnapshot ->
    Some (consensusQueryInEraInMode era (Consensus.GetCBOR Consensus.GetBigLedgerPeerSnapshot))
  QueryStakePoolDefaultVote govActs ->
    caseShelleyToBabbageOrConwayEraOnwards
      ( const $
          error "toConsensusQueryShelleyBased: QueryStakePoolDefaultVote is only available in the Conway era"
      )
      ( const $
          Some
            (consensusQueryInEraInMode era (Consensus.QueryStakePoolDefaultVote govActs))
      )
      sbe
 where
  era = toCardanoEra sbe

consensusQueryInEraInMode
  :: forall era erablock modeblock result result' fp xs
   . ConsensusBlockForEra era ~ erablock
  => Consensus.CardanoBlock StandardCrypto ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => Singletons.SingI fp
  => CardanoEra era
  -> Consensus.BlockQuery erablock fp result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode erainmode b =
  Consensus.BlockQuery @fp $
    case erainmode of
      ByronEra -> Consensus.QueryIfCurrentByron b
      ShelleyEra -> Consensus.QueryIfCurrentShelley b
      AllegraEra -> Consensus.QueryIfCurrentAllegra b
      MaryEra -> Consensus.QueryIfCurrentMary b
      AlonzoEra -> Consensus.QueryIfCurrentAlonzo b
      BabbageEra -> Consensus.QueryIfCurrentBabbage b
      ConwayEra -> Consensus.QueryIfCurrentConway b

-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult
  :: forall block result result'
   . ()
  => HasCallStack
  => Consensus.CardanoBlock StandardCrypto ~ block
  => QueryInMode result
  -> Consensus.Query block result'
  -> result'
  -> result
fromConsensusQueryResult QueryEraHistory q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetInterpreter) ->
      EraHistory r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult QuerySystemStart q' r' =
  case q' of
    Consensus.GetSystemStart ->
      r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult QueryChainBlockNo q' r' =
  case q' of
    Consensus.GetChainBlockNo ->
      r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult QueryChainPoint q' r' =
  case q' of
    Consensus.GetChainPoint ->
      fromConsensusPointHF r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult QueryCurrentEra q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryHardFork Consensus.GetCurrentEra) ->
      fromConsensusEraIndex r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult QueryLedgerConfig q' r' =
  case q' of
    Consensus.DebugLedgerConfig ->
      r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult (QueryInEra QueryByronUpdateState) q' r' =
  case q' of
    Consensus.BlockQuery
      (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState) ->
        bimap fromConsensusEraMismatch ByronUpdateState r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraShelley q)) q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryIfCurrentShelley q'') ->
      bimap
        fromConsensusEraMismatch
        ( fromConsensusQueryResultShelleyBased
            ShelleyBasedEraShelley
            q
            q''
        )
        r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraAllegra q)) q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryIfCurrentAllegra q'') ->
      bimap
        fromConsensusEraMismatch
        ( fromConsensusQueryResultShelleyBased
            ShelleyBasedEraAllegra
            q
            q''
        )
        r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraMary q)) q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryIfCurrentMary q'') ->
      bimap
        fromConsensusEraMismatch
        ( fromConsensusQueryResultShelleyBased
            ShelleyBasedEraMary
            q
            q''
        )
        r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraAlonzo q)) q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryIfCurrentAlonzo q'') ->
      bimap
        fromConsensusEraMismatch
        ( fromConsensusQueryResultShelleyBased
            ShelleyBasedEraAlonzo
            q
            q''
        )
        r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraBabbage q)) q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryIfCurrentBabbage q'') ->
      bimap
        fromConsensusEraMismatch
        ( fromConsensusQueryResultShelleyBased
            ShelleyBasedEraBabbage
            q
            q''
        )
        r'
    _ -> fromConsensusQueryResultMismatch
fromConsensusQueryResult (QueryInEra (QueryInShelleyBasedEra ShelleyBasedEraConway q)) q' r' =
  case q' of
    Consensus.BlockQuery (Consensus.QueryIfCurrentConway q'') ->
      bimap
        fromConsensusEraMismatch
        ( fromConsensusQueryResultShelleyBased
            ShelleyBasedEraConway
            q
            q''
        )
        r'
    _ -> fromConsensusQueryResultMismatch

-- This function is written like this so that we have exhaustive pattern checking
-- on the @QueryInShelleyBasedEra era result@ value. Don't change the top-level
-- @case sbeQuery of ...@!
fromConsensusQueryResultShelleyBased
  :: forall era ledgerera protocol result fp result'
   . HasCallStack
  => ShelleyLedgerEra era ~ ledgerera
  => ConsensusProtocol era ~ protocol
  => ProtoCrypto protocol ~ StandardCrypto
  => ShelleyBasedEra era
  -> QueryInShelleyBasedEra era result
  -> Consensus.BlockQuery (Consensus.ShelleyBlock protocol ledgerera) fp result'
  -> result'
  -> result
fromConsensusQueryResultShelleyBased sbe sbeQuery q' r' =
  case sbeQuery of
    QueryEpoch ->
      case q' of
        Consensus.GetEpochNo -> r'
        _ -> fromConsensusQueryResultMismatch
    QueryConstitution ->
      case q' of
        Consensus.GetConstitution -> r'
        _ -> fromConsensusQueryResultMismatch
    QueryGenesisParameters ->
      case q' of
        Consensus.GetGenesisConfig -> fromShelleyGenesis (Consensus.getCompactGenesis r')
        _ -> fromConsensusQueryResultMismatch
    QueryProtocolParameters ->
      case q' of
        Consensus.GetCurrentPParams -> r'
        _ -> fromConsensusQueryResultMismatch
    QueryStakeDistribution ->
      case q' of
        Consensus.GetStakeDistribution -> fromShelleyPoolDistr r'
        _ -> fromConsensusQueryResultMismatch
    QueryUTxO QueryUTxOWhole ->
      case q' of
        Consensus.GetUTxOWhole -> fromLedgerUTxO sbe r'
        _ -> fromConsensusQueryResultMismatch
    QueryUTxO QueryUTxOByAddress{} ->
      case q' of
        Consensus.GetUTxOByAddress{} -> fromLedgerUTxO sbe r'
        _ -> fromConsensusQueryResultMismatch
    QueryUTxO QueryUTxOByTxIn{} ->
      case q' of
        Consensus.GetUTxOByTxIn{} -> fromLedgerUTxO sbe r'
        _ -> fromConsensusQueryResultMismatch
    QueryStakeAddresses _ nId ->
      case q' of
        Consensus.GetFilteredDelegationsAndRewardAccounts{} ->
          let (delegs, rwaccs) = r'
           in ( Map.mapKeys (makeStakeAddress nId) $ fromShelleyRewardAccounts rwaccs
              , Map.mapKeys (makeStakeAddress nId) $ fromShelleyDelegations delegs
              )
        _ -> fromConsensusQueryResultMismatch
    QueryStakePools ->
      case q' of
        Consensus.GetStakePools -> Set.map StakePoolKeyHash r'
        _ -> fromConsensusQueryResultMismatch
    QueryStakePoolParameters{} ->
      case q' of
        Consensus.GetStakePoolParams{} ->
          Map.map fromShelleyPoolParams
            . Map.mapKeysMonotonic StakePoolKeyHash
            $ r'
        _ -> fromConsensusQueryResultMismatch
    QueryDebugLedgerState{} ->
      case q' of
        Consensus.GetCBOR Consensus.DebugNewEpochState ->
          SerialisedDebugLedgerState r'
        _ -> fromConsensusQueryResultMismatch
    QueryProtocolState ->
      case q' of
        Consensus.GetCBOR Consensus.DebugChainDepState ->
          ProtocolState r'
        _ -> fromConsensusQueryResultMismatch
    QueryCurrentEpochState ->
      case q' of
        Consensus.GetCBOR Consensus.DebugEpochState ->
          SerialisedCurrentEpochState r'
        _ -> fromConsensusQueryResultMismatch
    QueryPoolState{} ->
      case q' of
        Consensus.GetCBOR Consensus.GetPoolState{} ->
          SerialisedPoolState r'
        _ -> fromConsensusQueryResultMismatch
    QueryPoolDistribution{} ->
      case q' of
        Consensus.GetCBOR Consensus.GetPoolDistr{} ->
          SerialisedPoolDistribution r'
        _ -> fromConsensusQueryResultMismatch
    QueryStakeSnapshot{} ->
      case q' of
        Consensus.GetCBOR Consensus.GetStakeSnapshots{} ->
          SerialisedStakeSnapshots r'
        _ -> fromConsensusQueryResultMismatch
    QueryStakeDelegDeposits{} ->
      case q' of
        Consensus.GetStakeDelegDeposits{} ->
          Map.mapKeysMonotonic fromShelleyStakeCredential r'
        _ -> fromConsensusQueryResultMismatch
    QueryAccountState{} ->
      case q' of
        Consensus.GetAccountState{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryGovState{} ->
      case q' of
        Consensus.GetGovState{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryRatifyState{} ->
      case q' of
        Consensus.GetRatifyState{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryFuturePParams{} ->
      case q' of
        Consensus.GetFuturePParams{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryDRepState{} ->
      case q' of
        Consensus.GetDRepState{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryDRepStakeDistr{} ->
      case q' of
        Consensus.GetDRepStakeDistr{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QuerySPOStakeDistr{} ->
      case q' of
        Consensus.GetSPOStakeDistr{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryCommitteeMembersState{} ->
      case q' of
        Consensus.GetCommitteeMembersState{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryStakeVoteDelegatees{} ->
      case q' of
        Consensus.GetFilteredVoteDelegatees{} ->
          Map.mapKeys fromShelleyStakeCredential r'
        _ -> fromConsensusQueryResultMismatch
    QueryProposals{} ->
      case q' of
        Consensus.GetProposals{} ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryLedgerPeerSnapshot{} ->
      case q' of
        Consensus.GetCBOR Consensus.GetBigLedgerPeerSnapshot ->
          r'
        _ -> fromConsensusQueryResultMismatch
    QueryStakePoolDefaultVote{} ->
      case q' of
        Consensus.QueryStakePoolDefaultVote{} ->
          r'
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
fromConsensusQueryResultMismatch :: HasCallStack => a
fromConsensusQueryResultMismatch =
  withFrozenCallStack $
    error "fromConsensusQueryResult: internal query mismatch"

fromConsensusEraMismatch
  :: SListI xs
  => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch
