{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.Api.Query.Expr
  ( queryChainBlockNo
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
  , determineEraExpr
  , L.MemberStatus (..)
  , L.CommitteeMembersState (..)
  , queryCommitteeMembersState
  , queryDRepStakeDistribution
  , queryDRepState
  , queryGovState
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.Value

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Api.State.Query as L
import qualified Cardano.Ledger.CertState as L
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Keys as L
import           Cardano.Ledger.SafeHash
import           Cardano.Slotting.Slot
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S

queryChainBlockNo :: ()
  => LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (WithOrigin BlockNo))
queryChainBlockNo =
  queryExpr QueryChainBlockNo

queryChainPoint :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError ChainPoint)
queryChainPoint =
  queryExpr $ QueryChainPoint CardanoMode

queryCurrentEra :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError AnyCardanoEra)
queryCurrentEra =
  queryExpr $ QueryCurrentEra CardanoModeIsMultiEra

queryCurrentEpochState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedCurrentEpochState era)))
queryCurrentEpochState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryCurrentEpochState

queryEpoch :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch EpochNo))
queryEpoch eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryEpoch

queryDebugLedgerState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedDebugLedgerState era)))
queryDebugLedgerState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryDebugLedgerState

queryEraHistory :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError (EraHistory CardanoMode))
queryEraHistory =
  queryExpr $ QueryEraHistory CardanoModeIsMultiEra

queryGenesisParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (GenesisParameters ShelleyEra)))
queryGenesisParameters eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryGenesisParameters

queryPoolDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolDistribution era)))
queryPoolDistribution eraInMode sbe mPoolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryPoolDistribution mPoolIds

queryPoolState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolState era)))
queryPoolState eraInMode sbe mPoolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryPoolState mPoolIds

queryProtocolParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Ledger.PParams (ShelleyLedgerEra era))))
queryProtocolParameters eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

queryConstitutionHash :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Maybe (SafeHash (EraCrypto (ShelleyLedgerEra era)) L.AnchorData))))
queryConstitutionHash eraInMode sbe =
  (fmap . fmap . fmap . fmap) (L.anchorDataHash .  L.constitutionAnchor)
    $ queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryConstitution

queryProtocolParametersUpdate :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (Hash GenesisKey) ProtocolParametersUpdate)))
queryProtocolParametersUpdate eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolParametersUpdate

queryProtocolState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (ProtocolState era)))
queryProtocolState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolState

queryStakeAddresses :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> NetworkId
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map StakeAddress Lovelace, Map StakeAddress PoolId)))
queryStakeAddresses eraInMode sbe stakeCredentials networkId =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryStakeAddresses stakeCredentials networkId

queryStakeDelegDeposits :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either Consensus.EraMismatch (Map StakeCredential Lovelace)))
queryStakeDelegDeposits eraInMode sbe stakeCreds
  | S.null stakeCreds = pure . pure $ pure mempty
  | otherwise         = queryExpr $ QueryInEra eraInMode . QueryInShelleyBasedEra sbe $ QueryStakeDelegDeposits stakeCreds

queryStakeDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (Hash StakePoolKey) Rational)))
queryStakeDistribution eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryStakeDistribution

queryStakePoolParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set PoolId
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map PoolId StakePoolParameters)))
queryStakePoolParameters eraInMode sbe poolIds
  | S.null poolIds  = pure . pure $ pure mempty
  | otherwise       = queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryStakePoolParameters poolIds

queryStakePools :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Set PoolId)))
queryStakePools eraInMode sbe =
  queryExpr $ QueryInEra eraInMode . QueryInShelleyBasedEra sbe $ QueryStakePools

queryStakeSnapshot :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedStakeSnapshots era)))
queryStakeSnapshot eraInMode sbe mPoolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryStakeSnapshot mPoolIds

querySystemStart :: ()
  => LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError SystemStart)
querySystemStart =
  queryExpr QuerySystemStart

queryUtxo :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> QueryUTxOFilter
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (UTxO era)))
queryUtxo eraInMode sbe utxoFilter =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryUTxO utxoFilter

-- | A monad expression that determines what era the node is in.
determineEraExpr :: ()
  => ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError AnyCardanoEra)
determineEraExpr cModeParams = runExceptT $
  case consensusModeOnly cModeParams of
    ByronMode -> pure $ AnyCardanoEra ByronEra
    ShelleyMode -> pure $ AnyCardanoEra ShelleyEra
    CardanoMode -> ExceptT queryCurrentEra

queryConstitution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Maybe (L.Constitution (ShelleyLedgerEra era)))))
queryConstitution eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryConstitution

queryGovState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (L.GovState (ShelleyLedgerEra era))))
queryGovState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryGovState

queryDRepState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set (L.Credential L.DRepRole L.StandardCrypto)
  -- ^ An empty credentials set means that states for all DReps will be returned
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (L.Credential L.DRepRole L.StandardCrypto) (L.DRepState L.StandardCrypto))))
queryDRepState eraInMode sbe drepCreds = queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryDRepState drepCreds

queryDRepStakeDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set (L.DRep L.StandardCrypto)
  -- ^ An empty DRep set means that distributions for all DReps will be returned
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (L.DRep L.StandardCrypto) Lovelace)))
queryDRepStakeDistribution eraInMode sbe dreps = queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryDRepStakeDistr dreps

-- | Returns info about committee members filtered by: cold credentials, hot credentials and statuses.
-- If empty sets are passed as filters, then no filtering is done.
queryCommitteeMembersState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set (L.Credential L.ColdCommitteeRole L.StandardCrypto)
  -> Set (L.Credential L.HotCommitteeRole L.StandardCrypto)
  -> Set L.MemberStatus
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Maybe (L.CommitteeMembersState L.StandardCrypto))))
queryCommitteeMembersState eraInMode sbe coldCreds hotCreds statuses =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe (QueryCommitteeMembersState coldCreds hotCreds statuses)
