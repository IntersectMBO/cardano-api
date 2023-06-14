{-# LANGUAGE GADTs #-}

module Cardano.Api.Query.Expr
  ( queryChainBlockNo
  , queryChainPoint
  , queryCurrentEra
  , queryEraHistory
  , queryPparams
  , queryStakeDelegDeposits
  , queryStakePools
  , querySystemStart
  , queryUtxo
  , determineEraExpr
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.Modes
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

queryEraHistory :: ()
  => LocalStateQueryExpr block point (QueryInMode CardanoMode) r IO (Either UnsupportedNtcVersionError (EraHistory CardanoMode))
queryEraHistory =
  queryExpr $ QueryEraHistory CardanoModeIsMultiEra

queryPparams :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch ProtocolParameters))
queryPparams qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe QueryProtocolParameters

queryStakeDelegDeposits :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> Set StakeCredential
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either Consensus.EraMismatch (Map StakeCredential Lovelace)))
queryStakeDelegDeposits qeInMode qSbe stakeCreds =
  queryExpr $ QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakeDelegDeposits stakeCreds

queryStakePools :: ()
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> LocalStateQueryExpr block point (QueryInMode mode) r IO (Either UnsupportedNtcVersionError (Either EraMismatch (Set PoolId)))
queryStakePools qeInMode qSbe =
  queryExpr $ QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools

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
