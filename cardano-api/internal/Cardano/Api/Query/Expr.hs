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
  , queryPparams
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
  , queryCommitteeState
  , queryDRepStakeDistribution
  , queryDRepState
  , queryGovState
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.GenesisParameters
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Value

import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.CertState as L
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Keys as L
import           Cardano.Ledger.SafeHash
import qualified Cardano.Ledger.Shelley.Core as L
import           Cardano.Slotting.Slot
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Data.Map (Map)
import           Data.Set (Set)

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
  => EraInMode ShelleyEra mode
  -> ShelleyBasedEra ShelleyEra
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

queryPparams :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch ProtocolParameters))
queryPparams eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
{-# DEPRECATED queryPparams "Use queryProtocolParameters instead" #-}

queryProtocolParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch ProtocolParameters))
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
queryStakeDelegDeposits eraInMode sbe stakeCreds =
  queryExpr $ QueryInEra eraInMode . QueryInShelleyBasedEra sbe $ QueryStakeDelegDeposits stakeCreds

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
queryStakePoolParameters eraInMode sbe poolIds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryStakePoolParameters poolIds

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
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (L.Credential L.DRepRole L.StandardCrypto) (L.DRepState L.StandardCrypto))))
queryDRepState eraInMode sbe drepCreds =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryDRepState drepCreds

queryDRepStakeDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set (L.DRep L.StandardCrypto)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (L.DRep L.StandardCrypto) Lovelace)))
queryDRepStakeDistribution eraInMode sbe dreps =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe $ QueryDRepStakeDistr dreps

queryCommitteeState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (L.CommitteeState (ShelleyLedgerEra era))))
queryCommitteeState eraInMode sbe =
  queryExpr $ QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryCommitteeState
