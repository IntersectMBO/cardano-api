{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Query.Internal.Expr
  ( queryAccountState
  , queryChainBlockNo
  , queryChainPoint
  , queryConstitution
  , queryCurrentEpochState
  , queryCurrentEra
  , queryDebugLedgerState
  , queryEpoch
  , queryConstitutionHash
  , queryEraHistory
  , queryGenesisParameters
  , queryPoolDistribution
  , queryPoolState
  , queryProtocolParameters
  , queryProtocolState
  , queryStakeAddresses
  , queryStakeDelegDeposits
  , queryStakeDistribution
  , queryStakePoolParameters
  , queryStakePools
  , queryStakeSnapshot
  , querySystemStart
  , queryUtxo
  , queryLedgerPeerSnapshot
  , L.MemberStatus (..)
  , L.CommitteeMembersState (..)
  , queryCommitteeMembersState
  , queryDRepStakeDistribution
  , querySPOStakeDistribution
  , queryDRepState
  , queryGovState
  , queryRatifyState
  , queryFuturePParams
  , queryStakeVoteDelegatees
  , queryProposals
  , queryStakePoolDefaultVote
  , queryLedgerConfig
  )
where

import Cardano.Api.Address
import Cardano.Api.Block
import Cardano.Api.Certificate.Internal
import Cardano.Api.Era
import Cardano.Api.Genesis.Internal.Parameters
import Cardano.Api.Internal.Utils ((<<<$>>>))
import Cardano.Api.Key.Internal
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger
import Cardano.Api.Network.IPC
import Cardano.Api.Network.Internal.NetworkId
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.UTxO

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.State qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Hashes hiding (Hash)
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.State qualified as L
import Cardano.Slotting.Slot
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import Ouroboros.Network.Block (Serialised)
import Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeerSnapshot)

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as S

queryChainBlockNo
  :: ()
  => LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (WithOrigin BlockNo))
queryChainBlockNo =
  queryExpr QueryChainBlockNo

queryChainPoint
  :: ()
  => LocalStateQueryExpr block point QueryInMode r IO (Either UnsupportedNtcVersionError ChainPoint)
queryChainPoint =
  queryExpr QueryChainPoint

queryLedgerConfig
  :: ()
  => LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Consensus.CardanoLedgerConfig Ledger.StandardCrypto)
       )
queryLedgerConfig =
  queryExpr QueryLedgerConfig

queryCurrentEra
  :: ()
  => LocalStateQueryExpr block point QueryInMode r IO (Either UnsupportedNtcVersionError AnyCardanoEra)
queryCurrentEra =
  queryExpr QueryCurrentEra

queryCurrentEpochState
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedCurrentEpochState era)))
queryCurrentEpochState eon = querySbe eon QueryCurrentEpochState

queryEpoch
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch EpochNo))
queryEpoch eon = querySbe eon QueryEpoch

queryDebugLedgerState
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedDebugLedgerState era)))
queryDebugLedgerState eon = querySbe eon QueryDebugLedgerState

queryLedgerPeerSnapshot
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (Serialised LedgerPeerSnapshot)))
queryLedgerPeerSnapshot eon = querySbe eon QueryLedgerPeerSnapshot

queryEraHistory
  :: ()
  => LocalStateQueryExpr block point QueryInMode r IO (Either UnsupportedNtcVersionError EraHistory)
queryEraHistory =
  queryExpr QueryEraHistory

queryGenesisParameters
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (GenesisParameters ShelleyEra)))
queryGenesisParameters eon = querySbe eon QueryGenesisParameters

queryPoolDistribution
  :: ()
  => BabbageEraOnwards era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolDistribution era)))
queryPoolDistribution eon = querySbe eon . QueryPoolDistribution

queryPoolState
  :: ()
  => BabbageEraOnwards era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolState era)))
queryPoolState eon = querySbe eon . QueryPoolState

queryProtocolParameters
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (Ledger.PParams (ShelleyLedgerEra era))))
queryProtocolParameters eon = querySbe eon QueryProtocolParameters

queryConstitutionHash
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch (SafeHash L.AnchorData))
       )
queryConstitutionHash eon =
  (L.anchorDataHash . L.constitutionAnchor)
    <<<$>>> querySbe eon QueryConstitution

queryProtocolState
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (ProtocolState era)))
queryProtocolState eon = querySbe eon QueryProtocolState

queryStakeAddresses
  :: ()
  => ShelleyBasedEra era
  -> Set StakeCredential
  -> NetworkId
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch (Map StakeAddress L.Coin, Map StakeAddress PoolId))
       )
queryStakeAddresses eon stakeCredentials networkId = querySbe eon $ QueryStakeAddresses stakeCredentials networkId

queryStakeDelegDeposits
  :: BabbageEraOnwards era
  -> Set StakeCredential
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either Consensus.EraMismatch (Map StakeCredential L.Coin)))
queryStakeDelegDeposits eon stakeCreds
  | S.null stakeCreds = pure . pure $ pure mempty
  | otherwise = querySbe eon $ QueryStakeDelegDeposits stakeCreds

queryStakeDistribution
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (Map (Hash StakePoolKey) Rational)))
queryStakeDistribution eon = querySbe eon QueryStakeDistribution

queryStakePoolParameters
  :: ()
  => ShelleyBasedEra era
  -> Set PoolId
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (Map PoolId StakePoolParameters)))
queryStakePoolParameters eon poolIds
  | S.null poolIds = pure . pure $ pure mempty
  | otherwise =
      querySbe eon $ QueryStakePoolParameters poolIds

queryStakePools
  :: ()
  => ShelleyBasedEra era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (Set PoolId)))
queryStakePools eon = querySbe eon QueryStakePools

queryStakeSnapshot
  :: ()
  => BabbageEraOnwards era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedStakeSnapshots era)))
queryStakeSnapshot eon = querySbe eon . QueryStakeSnapshot

querySystemStart
  :: ()
  => LocalStateQueryExpr block point QueryInMode r IO (Either UnsupportedNtcVersionError SystemStart)
querySystemStart =
  queryExpr QuerySystemStart

queryUtxo
  :: ()
  => ShelleyBasedEra era
  -> QueryUTxOFilter
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (UTxO era)))
queryUtxo eon = querySbe eon . QueryUTxO

queryConstitution
  :: ()
  => ConwayEraOnwards era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (L.Constitution (ShelleyLedgerEra era))))
queryConstitution eon = querySbe eon QueryConstitution

queryGovState
  :: ()
  => ConwayEraOnwards era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (L.GovState (ShelleyLedgerEra era))))
queryGovState eon = querySbe eon QueryGovState

queryRatifyState
  :: ()
  => ConwayEraOnwards era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (L.RatifyState (ShelleyLedgerEra era))))
queryRatifyState eon = querySbe eon QueryRatifyState

queryFuturePParams
  :: ()
  => ConwayEraOnwards era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (Maybe (L.PParams (ShelleyLedgerEra era)))))
queryFuturePParams eon = querySbe eon QueryFuturePParams

queryDRepState
  :: ConwayEraOnwards era
  -> Set (L.Credential L.DRepRole)
  -- ^ An empty credentials set means that states for all DReps will be returned
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch (Map (L.Credential L.DRepRole) L.DRepState))
       )
queryDRepState eon = querySbe eon . QueryDRepState

queryDRepStakeDistribution
  :: ConwayEraOnwards era
  -> Set L.DRep
  -- ^ An empty DRep set means that distributions for all DReps will be returned
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch (Map L.DRep L.Coin)))
queryDRepStakeDistribution eon = querySbe eon . QueryDRepStakeDistr

querySPOStakeDistribution
  :: ConwayEraOnwards era
  -> Set (L.KeyHash 'L.StakePool)
  -- ^ An empty SPO key hash set means that distributions for all SPOs will be returned
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch (Map (L.KeyHash 'L.StakePool) L.Coin))
       )
querySPOStakeDistribution eon = querySbe eon . QuerySPOStakeDistr

-- | Returns info about committee members filtered by: cold credentials, hot credentials and statuses.
-- If empty sets are passed as filters, then no filtering is done.
queryCommitteeMembersState
  :: ConwayEraOnwards era
  -> Set (L.Credential L.ColdCommitteeRole)
  -> Set (L.Credential L.HotCommitteeRole)
  -> Set L.MemberStatus
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch L.CommitteeMembersState))
queryCommitteeMembersState eon coldCreds hotCreds memberStatuses =
  querySbe eon $ QueryCommitteeMembersState coldCreds hotCreds memberStatuses

queryStakeVoteDelegatees
  :: ConwayEraOnwards era
  -> Set StakeCredential
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch (Map StakeCredential L.DRep))
       )
queryStakeVoteDelegatees eon = querySbe eon . QueryStakeVoteDelegatees

queryAccountState
  :: ConwayEraOnwards era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch L.ChainAccountState))
queryAccountState eon = querySbe eon QueryAccountState

queryProposals
  :: forall era block point r
   . ConwayEraOnwards era
  -- Specify a set of Governance Action IDs to filter the proposals. When this set is
  -- empty, all the proposals considered for ratification will be returned.
  -> Set L.GovActionId
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch (Seq (L.GovActionState (ShelleyLedgerEra era))))
       )
queryProposals eon = querySbe eon . QueryProposals

queryStakePoolDefaultVote
  :: forall era block point r
   . ConwayEraOnwards era
  -> L.KeyHash 'L.StakePool
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch L.DefaultVote)
       )
queryStakePoolDefaultVote eon = querySbe eon . QueryStakePoolDefaultVote

querySbe
  :: Convert eon ShelleyBasedEra
  => eon era
  -> QueryInShelleyBasedEra era result
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           UnsupportedNtcVersionError
           (Either EraMismatch result)
       )
querySbe eon queryInSbe =
  shelleyBasedEraConstraints (convert eon) $
    queryExpr . QueryInEra $
      QueryInShelleyBasedEra (convert eon) queryInSbe
