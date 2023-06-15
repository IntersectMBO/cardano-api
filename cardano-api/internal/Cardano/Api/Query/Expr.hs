{-# LANGUAGE GADTs #-}

module Cardano.Api.Query.Expr
  ( queryChainBlockNo
  , queryChainPoint
  , queryCurrentEpochState
  , queryCurrentEra
  , queryDebugLedgerState
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
queryCurrentEpochState qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryCurrentEpochState

queryDebugLedgerState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedDebugLedgerState era)))
queryDebugLedgerState qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryDebugLedgerState

queryEraHistory :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError (EraHistory CardanoMode))
queryEraHistory =
  queryExpr $ QueryEraHistory CardanoModeIsMultiEra

queryGenesisParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch GenesisParameters))
queryGenesisParameters qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryGenesisParameters

queryPoolDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolDistribution era)))
queryPoolDistribution qeInMode qSbe mPoolIds =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryPoolDistribution mPoolIds

queryPoolState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedPoolState era)))
queryPoolState qeInMode qSbe mPoolIds =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryPoolState mPoolIds

queryPparams :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch ProtocolParameters))
queryPparams qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryProtocolParameters
{-# DEPRECATED queryPparams "Use queryProtocolParameters instead" #-}

queryProtocolParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch ProtocolParameters))
queryProtocolParameters qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryProtocolParameters

queryProtocolParametersUpdate :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (Hash GenesisKey) ProtocolParametersUpdate)))
queryProtocolParametersUpdate qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryProtocolParametersUpdate

queryProtocolState :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (ProtocolState era)))
queryProtocolState qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryProtocolState

queryStakeAddresses :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> NetworkId
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map StakeAddress Lovelace, Map StakeAddress PoolId)))
queryStakeAddresses qeInMode qSbe stakeCredentials networkId =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryStakeAddresses stakeCredentials networkId

queryStakeDelegDeposits :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either Consensus.EraMismatch (Map StakeCredential Lovelace)))
queryStakeDelegDeposits qeInMode qSbe stakeCreds =
  queryExpr $ QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakeDelegDeposits stakeCreds

queryStakeDistribution :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map (Hash StakePoolKey) Rational)))
queryStakeDistribution qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryStakeDistribution

queryStakePoolParameters :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set PoolId
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Map PoolId StakePoolParameters)))
queryStakePoolParameters qeInMode qSbe poolIds =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryStakePoolParameters poolIds

queryStakePools :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Set PoolId)))
queryStakePools qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools

queryStakeSnapshot :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Maybe (Set PoolId)
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (SerialisedStakeSnapshots era)))
queryStakeSnapshot qeInMode qSbe mPoolIds =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryStakeSnapshot mPoolIds

querySystemStart :: ()
  => LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError SystemStart)
querySystemStart =
  queryExpr QuerySystemStart

queryUtxo :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> QueryUTxOFilter
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (UTxO era)))
queryUtxo qeInMode qSbe utxoFilter =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryUTxO utxoFilter

-- | A monad expression that determines what era the node is in.
determineEraExpr :: ()
  => ConsensusModeParams mode
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError AnyCardanoEra)
determineEraExpr cModeParams = runExceptT $
  case consensusModeOnly cModeParams of
    ByronMode -> pure $ AnyCardanoEra ByronEra
    ShelleyMode -> pure $ AnyCardanoEra ShelleyEra
    CardanoMode -> ExceptT queryCurrentEra
