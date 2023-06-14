{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Convenience query functions
--
module Cardano.Api.Convenience.Query (
    QueryConvenienceError(..),
    determineEra,
    -- * Simplest query related
    executeQueryCardanoMode,

    queryStateForBalancedTx,
    renderQueryConvenienceError,
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Eras
import           Cardano.Api.IO
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.TxBody
import           Cardano.Api.Utils
import           Cardano.Api.Value

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

import           Control.Monad.Trans (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (left, onLeft, onNothing)
import           Data.Function ((&))
import           Data.Map (Map)
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | EraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | QceUnsupportedNtcVersion !UnsupportedNtcVersionError

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (AcqFailure e) =
  "Acquiring failure: " <> textShow e
renderQueryConvenienceError (QueryEraMismatch (EraMismatch ledgerEraName' otherEraName')) =
  "The era of the node and the tx do not match. " <>
  "The node is running in the " <> ledgerEraName' <>
  " era, but the transaction is for the " <> otherEraName' <> " era."
renderQueryConvenienceError ByronEraNotSupported =
  "Byron era not supported"
renderQueryConvenienceError (EraConsensusModeMismatch cMode anyCEra) =
  "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
  " Era: " <> textShow anyCEra
renderQueryConvenienceError (QceUnsupportedNtcVersion (UnsupportedNtcVersionError minNtcVersion ntcVersion)) =
  "Unsupported feature for the node-to-client protocol version.\n" <>
  "This query requires at least " <> textShow minNtcVersion <> " but the node negotiated " <> textShow ntcVersion <> ".\n" <>
  "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: LocalNodeConnectInfo CardanoMode
  -> CardanoEra era
  -> [TxIn]
  -> [Certificate]
  -> IO (Either QueryConvenienceError ( UTxO era
                                      , ProtocolParameters
                                      , EraHistory CardanoMode
                                      , SystemStart
                                      , Set PoolId
                                      , Map StakeCredential Lovelace
                                      )
        )
queryStateForBalancedTx localNodeConnInfo era allTxIns certs = runExceptT $ do
  qSbe <- pure (getSbe $ cardanoEraStyle era)
    & onLeft left

  qeInMode <- pure (toEraInMode era CardanoMode)
    & onNothing (left (EraConsensusModeMismatch (AnyConsensusMode CardanoMode) (getIsCardanoEraConstraint era $ AnyCardanoEra era)))

  -- Queries
  let utxoQuery = QueryInEra qeInMode $ QueryInShelleyBasedEra qSbe
                    $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
      pparamsQuery = QueryInEra qeInMode
                        $ QueryInShelleyBasedEra qSbe QueryProtocolParameters
      eraHistoryQuery = QueryEraHistory CardanoModeIsMultiEra
      systemStartQuery = QuerySystemStart
      stakePoolsQuery = QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakePools
      stakeCreds = Set.fromList $ flip mapMaybe certs $ \case
        StakeAddressDeregistrationCertificate cred -> Just cred
        _ -> Nothing
      stakeDelegDepositsQuery =
        QueryInEra qeInMode . QueryInShelleyBasedEra qSbe $ QueryStakeDelegDeposits stakeCreds

  -- Query execution
  utxo <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryExpr utxoQuery))
    & onLeft (left . AcqFailure)
    & onLeft (left . QceUnsupportedNtcVersion)
    & onLeft (left . QueryEraMismatch)

  pparams <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryExpr pparamsQuery))
    & onLeft (left . AcqFailure)
    & onLeft (left . QceUnsupportedNtcVersion)
    & onLeft (left . QueryEraMismatch)

  eraHistory <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryExpr eraHistoryQuery))
    & onLeft (left . AcqFailure)
    & onLeft (left . QceUnsupportedNtcVersion)

  systemStart <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryExpr systemStartQuery))
    & onLeft (left . AcqFailure)
    & onLeft (left . QceUnsupportedNtcVersion)

  stakePools <- lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryExpr stakePoolsQuery))
    & onLeft (left . AcqFailure)
    & onLeft (left . QceUnsupportedNtcVersion)
    & onLeft (left . QueryEraMismatch)

  stakeDelegDeposits <-
    if null stakeCreds
      then pure mempty
      else do
        lift (executeLocalStateQueryExpr localNodeConnInfo Nothing (queryExpr stakeDelegDepositsQuery))
          & onLeft (left . AcqFailure)
          & onLeft (left . QceUnsupportedNtcVersion)
          & onLeft (left . QueryEraMismatch)

  return (utxo, pparams, eraHistory, systemStart, stakePools, stakeDelegDeposits)

-- | Query the node to determine which era it is in.
determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> IO (Either AcquiringFailure AnyCardanoEra)
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> return . Right $ AnyCardanoEra ByronEra
    ShelleyMode -> return . Right $ AnyCardanoEra ShelleyEra
    CardanoMode ->
      queryNodeLocalState localNodeConnInfo Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra

getSbe :: CardanoEraStyle era -> Either QueryConvenienceError (ShelleyBasedEra era)
getSbe LegacyByronEra = Left ByronEraNotSupported
getSbe (ShelleyBasedEra sbe) = return sbe

-- | Execute a query against the local node. The local
-- node must be in CardanoMode.
executeQueryCardanoMode
  :: SocketPath
  -> CardanoEra era
  -> NetworkId
  -> QueryInMode CardanoMode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryCardanoMode socketPath era nid q = runExceptT $ do
  let localNodeConnInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          , localNodeNetworkId = nid
          , localNodeSocketPath = socketPath
          }

  ExceptT $ executeQueryAnyMode era localNodeConnInfo q

-- | Execute a query against the local node in any mode.
executeQueryAnyMode
  :: forall result era mode. CardanoEra era
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryAnyMode era localNodeConnInfo q = runExceptT $ do
  let cMode = consensusModeOnly $ localConsensusModeParams localNodeConnInfo

  eraInMode <- pure (toEraInMode era cMode)
    & onNothing (left $ EraConsensusModeMismatch
        (AnyConsensusMode CardanoMode)
        (getIsCardanoEraConstraint era $ AnyCardanoEra era))

  case eraInMode of
    ByronEraInByronMode -> left ByronEraNotSupported
    _ ->
      lift (queryNodeLocalState localNodeConnInfo Nothing q)
        & onLeft (left . AcqFailure)
        & onLeft (left . QueryEraMismatch)
