{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Internal.Query.Expr
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

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Block
import Cardano.Api.Internal.Certificate
import Cardano.Api.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.GenesisParameters
import Cardano.Api.Internal.IPC
import Cardano.Api.Internal.IPC.Monad
import Cardano.Api.Internal.Keys.Shelley
import Cardano.Api.Internal.NetworkId
import Cardano.Api.Internal.Query
import Cardano.Api.Internal.ReexposeLedger qualified as Ledger
import Cardano.Api.Internal.Tx.UTxO

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.State.Query qualified as L
import Cardano.Ledger.State qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Hashes hiding (Hash)
import Cardano.Ledger.Keys qualified as L
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
queryCurrentEpochState sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryCurrentEpochState

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
queryEpoch sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryEpoch

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
queryDebugLedgerState sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryDebugLedgerState

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
queryLedgerPeerSnapshot sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryLedgerPeerSnapshot

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
queryGenesisParameters sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryGenesisParameters

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
queryPoolDistribution era mPoolIds = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryPoolDistribution mPoolIds

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
queryPoolState era mPoolIds = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryPoolState mPoolIds

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
queryProtocolParameters sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryProtocolParameters

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
queryConstitutionHash sbe =
  (fmap . fmap . fmap) (L.anchorDataHash . L.constitutionAnchor) $
    queryExpr $
      QueryInEra $
        QueryInShelleyBasedEra sbe QueryConstitution

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
queryProtocolState sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryProtocolState

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
queryStakeAddresses sbe stakeCredentials networkId =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryStakeAddresses stakeCredentials networkId

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
queryStakeDelegDeposits era stakeCreds
  | S.null stakeCreds = pure . pure $ pure mempty
  | otherwise = do
      let sbe = convert era
      queryExpr $ QueryInEra . QueryInShelleyBasedEra sbe $ QueryStakeDelegDeposits stakeCreds

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
queryStakeDistribution sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryStakeDistribution

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
queryStakePoolParameters sbe poolIds
  | S.null poolIds = pure . pure $ pure mempty
  | otherwise =
      queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryStakePoolParameters poolIds

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
queryStakePools sbe =
  queryExpr $ QueryInEra . QueryInShelleyBasedEra sbe $ QueryStakePools

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
queryStakeSnapshot era mPoolIds = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryStakeSnapshot mPoolIds

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
queryUtxo sbe utxoFilter =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryUTxO utxoFilter

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
queryConstitution era = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryConstitution

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
queryGovState era = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryGovState

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
queryRatifyState era = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryRatifyState

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
queryFuturePParams era = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryFuturePParams

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
queryDRepState era drepCreds = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryDRepState drepCreds

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
queryDRepStakeDistribution era dreps = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryDRepStakeDistr dreps

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
querySPOStakeDistribution era spos = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QuerySPOStakeDistr spos

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
queryCommitteeMembersState era coldCreds hotCreds statuses = do
  let sbe = convert era
  queryExpr $
    QueryInEra $
      QueryInShelleyBasedEra sbe (QueryCommitteeMembersState coldCreds hotCreds statuses)

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
queryStakeVoteDelegatees era stakeCredentials = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryStakeVoteDelegatees stakeCredentials

queryAccountState
  :: ConwayEraOnwards era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either UnsupportedNtcVersionError (Either EraMismatch L.AccountState))
queryAccountState cOnwards =
  queryExpr $
    QueryInEra . QueryInShelleyBasedEra (convert cOnwards) $
      QueryAccountState

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
queryProposals cOnwards govActionIds = do
  let sbe = convert cOnwards
  queryExpr $
    QueryInEra . QueryInShelleyBasedEra sbe $
      QueryProposals govActionIds

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
queryStakePoolDefaultVote cOnwards stakePools = do
  let sbe = convert cOnwards
  queryExpr $
    QueryInEra . QueryInShelleyBasedEra sbe $
      QueryStakePoolDefaultVote stakePools
