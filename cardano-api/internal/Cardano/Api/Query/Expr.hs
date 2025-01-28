{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Query.Expr
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
  , queryProtocolParametersUpdate
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
  , queryStakeVoteDelegatees
  , queryProposals
  )
where

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.Convert
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import qualified Cardano.Api.ReexposeLedger as Ledger

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Api.State.Query as L
import qualified Cardano.Ledger.CertState as L
import qualified Cardano.Ledger.Coin as L
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Keys as L
import           Cardano.Ledger.SafeHash
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Cardano.Slotting.Slot
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import           Ouroboros.Network.Block (Serialised)
import           Ouroboros.Network.PeerSelection.LedgerPeers (LedgerPeerSnapshot)

import           Data.Map (Map)
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as S

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
          (Either EraMismatch (SafeHash (EraCrypto (ShelleyLedgerEra era)) L.AnchorData))
      )
queryConstitutionHash sbe =
  (fmap . fmap . fmap) (L.anchorDataHash . L.constitutionAnchor) $
    queryExpr $
      QueryInEra $
        QueryInShelleyBasedEra sbe QueryConstitution

queryProtocolParametersUpdate
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
          (Either EraMismatch (Map (Hash GenesisKey) ProtocolParametersUpdate))
      )
queryProtocolParametersUpdate sbe =
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe QueryProtocolParametersUpdate

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

queryDRepState
  :: ConwayEraOnwards era
  -> Set (L.Credential L.DRepRole L.StandardCrypto)
  -- ^ An empty credentials set means that states for all DReps will be returned
  -> LocalStateQueryExpr
      block
      point
      QueryInMode
      r
      IO
      ( Either
          UnsupportedNtcVersionError
          (Either EraMismatch (Map (L.Credential L.DRepRole L.StandardCrypto) (L.DRepState L.StandardCrypto)))
      )
queryDRepState era drepCreds = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryDRepState drepCreds

queryDRepStakeDistribution
  :: ConwayEraOnwards era
  -> Set (L.DRep L.StandardCrypto)
  -- ^ An empty DRep set means that distributions for all DReps will be returned
  -> LocalStateQueryExpr
      block
      point
      QueryInMode
      r
      IO
      (Either UnsupportedNtcVersionError (Either EraMismatch (Map (L.DRep L.StandardCrypto) L.Coin)))
queryDRepStakeDistribution era dreps = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QueryDRepStakeDistr dreps

querySPOStakeDistribution
  :: ConwayEraOnwards era
  -> Set (L.KeyHash 'L.StakePool L.StandardCrypto)
  -- ^ An empty SPO key hash set means that distributions for all SPOs will be returned
  -> LocalStateQueryExpr
      block
      point
      QueryInMode
      r
      IO
      ( Either
          UnsupportedNtcVersionError
          (Either EraMismatch (Map (L.KeyHash 'L.StakePool L.StandardCrypto) L.Coin))
      )
querySPOStakeDistribution era spos = do
  let sbe = convert era
  queryExpr $ QueryInEra $ QueryInShelleyBasedEra sbe $ QuerySPOStakeDistr spos

-- | Returns info about committee members filtered by: cold credentials, hot credentials and statuses.
-- If empty sets are passed as filters, then no filtering is done.
queryCommitteeMembersState
  :: ConwayEraOnwards era
  -> Set (L.Credential L.ColdCommitteeRole L.StandardCrypto)
  -> Set (L.Credential L.HotCommitteeRole L.StandardCrypto)
  -> Set L.MemberStatus
  -> LocalStateQueryExpr
      block
      point
      QueryInMode
      r
      IO
      (Either UnsupportedNtcVersionError (Either EraMismatch (L.CommitteeMembersState L.StandardCrypto)))
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
          (Either EraMismatch (Map StakeCredential (L.DRep L.StandardCrypto)))
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
  -> Set (L.GovActionId L.StandardCrypto)
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
