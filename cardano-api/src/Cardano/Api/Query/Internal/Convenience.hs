{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Convenience query functions
module Cardano.Api.Query.Internal.Convenience
  ( QueryConvenienceError (..)
  , TxCurrentTreasuryValue (..)
  , determineEra

    -- * Simplest query related
  , executeQueryCardanoMode
  , executeQueryAnyMode
  , queryStateForBalancedTx
  , renderQueryConvenienceError
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal
import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.IO
import Cardano.Api.Monad.Error
import Cardano.Api.Network.IPC
import Cardano.Api.Network.Internal.NetworkId
import Cardano.Api.Pretty
import Cardano.Api.ProtocolParameters
import Cardano.Api.Query.Internal.Expr
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.Tx.Internal.Body
import Cardano.Api.UTxO (UTxO (..))

import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Conway.State (ChainAccountState (..), DRepState (..))
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.State (ChainAccountState (..), DRepState (..))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import Control.Exception.Safe (SomeException, displayException)
import Control.Monad
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Exts (IsList (..), IsString (..))

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | QceUnsupportedNtcVersion !UnsupportedNtcVersionError
  | QceUnexpectedException !SomeException
  deriving Show

instance Error QueryConvenienceError where
  prettyError = pshow . renderQueryConvenienceError

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (AcqFailure e) =
  "Acquiring failure: " <> textShow e
renderQueryConvenienceError (QueryEraMismatch (EraMismatch ledgerEraName' otherEraName')) =
  "The era of the node and the tx do not match. "
    <> "The node is running in the "
    <> ledgerEraName'
    <> " era, but the transaction is for the "
    <> otherEraName'
    <> " era."
renderQueryConvenienceError ByronEraNotSupported =
  "Byron era not supported"
renderQueryConvenienceError (QceUnsupportedNtcVersion (UnsupportedNtcVersionError ntcVersion supportedVersions)) =
  "Unsupported feature for the node-to-client protocol version.\n"
    <> "The negotiated version is "
    <> textShow ntcVersion
    <> " but this query is only supported in "
    <> textShow supportedVersions
    <> ".\n"
    <> "Probably either the client or the node is out-of-date.\n"
    <> "Later node versions support later node-to-client protocol versions (but development protocol versions are not enabled in the node by default)."
renderQueryConvenienceError (QceUnexpectedException e) =
  "Unexpected exception while processing query:\n" <> fromString (displayException e)

newtype TxCurrentTreasuryValue = TxCurrentTreasuryValue {unTxCurrentTreasuryValue :: L.Coin}
  deriving newtype Show

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Tx.Internal.Convenience.constructBalancedTx
queryStateForBalancedTx
  :: ()
  => CardanoEra era
  -> [TxIn]
  -> [Certificate era]
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       ( Either
           QueryConvenienceError
           ( UTxO era
           , LedgerProtocolParameters era
           , EraHistory
           , SystemStart
           , Set PoolId
           , Map StakeCredential L.Coin
           , Map (L.Credential L.DRepRole) L.Coin
           , Maybe (Featured ConwayEraOnwards era TxCurrentTreasuryValue)
           )
       )
queryStateForBalancedTx era allTxIns certs = runExceptT $ do
  sbe <-
    requireShelleyBasedEra era
      & onNothing (left ByronEraNotSupported)

  let stakeCreds = fromList $ mapMaybe filterUnRegCreds certs
      drepCreds = fromList $ mapMaybe filterUnRegDRepCreds certs

  -- Query execution
  utxo <-
    lift (queryUtxo sbe (QueryUTxOByTxIn (fromList allTxIns)))
      & onLeft (left . QceUnsupportedNtcVersion)
      & onLeft (left . QueryEraMismatch)

  pparams <-
    lift (queryProtocolParameters sbe)
      & onLeft (left . QceUnsupportedNtcVersion)
      & onLeft (left . QueryEraMismatch)

  eraHistory <-
    lift queryEraHistory
      & onLeft (left . QceUnsupportedNtcVersion)

  systemStart <-
    lift querySystemStart
      & onLeft (left . QceUnsupportedNtcVersion)

  stakePools <-
    lift (queryStakePools sbe)
      & onLeft (left . QceUnsupportedNtcVersion)
      & onLeft (left . QueryEraMismatch)

  stakeDelegDeposits <-
    monoidForEraInEonA era $ \beo ->
      lift (queryStakeDelegDeposits beo stakeCreds)
        & onLeft (left . QceUnsupportedNtcVersion)
        & onLeft (left . QueryEraMismatch)

  drepDelegDeposits <-
    monoidForEraInEonA era $ \con ->
      Map.map drepDeposit
        <$> ( lift (queryDRepState con drepCreds)
                & onLeft (left . QceUnsupportedNtcVersion)
                & onLeft (left . QueryEraMismatch)
            )

  featuredTxTreasuryValueM <-
    caseShelleyToBabbageOrConwayEraOnwards
      (const $ pure Nothing)
      ( \cOnwards -> do
          ChainAccountState{casTreasury} <-
            lift (queryAccountState cOnwards)
              & onLeft (left . QceUnsupportedNtcVersion)
              & onLeft (left . QueryEraMismatch)
          let txCurrentTreasuryValue = TxCurrentTreasuryValue casTreasury
          return $ Just $ Featured cOnwards txCurrentTreasuryValue
      )
      sbe

  pure
    ( utxo
    , LedgerProtocolParameters pparams
    , eraHistory
    , systemStart
    , stakePools
    , stakeDelegDeposits
    , drepDelegDeposits
    , featuredTxTreasuryValueM
    )

-- | Query the node to determine which era it is in.
determineEra
  :: ()
  => LocalNodeConnectInfo
  -> ExceptT AcquiringFailure IO AnyCardanoEra
determineEra localNodeConnInfo =
  queryNodeLocalState localNodeConnInfo VolatileTip QueryCurrentEra

-- | Execute a query against the local node. The local
-- node must be in CardanoMode.
executeQueryCardanoMode
  :: ()
  => SocketPath
  -> NetworkId
  -> QueryInMode (Either EraMismatch result)
  -> ExceptT QueryConvenienceError IO result
executeQueryCardanoMode socketPath nid q = do
  let localNodeConnInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          , localNodeNetworkId = nid
          , localNodeSocketPath = socketPath
          }

  executeQueryAnyMode localNodeConnInfo q

-- | Execute a query against the local node in any mode.
executeQueryAnyMode
  :: forall result
   . ()
  => LocalNodeConnectInfo
  -> QueryInMode (Either EraMismatch result)
  -> ExceptT QueryConvenienceError IO result
executeQueryAnyMode localNodeConnInfo q =
  liftEither
    <=< fmap (first QueryEraMismatch)
      . handleIOExceptionsWith QceUnexpectedException
      . modifyError AcqFailure
    $ queryNodeLocalState localNodeConnInfo VolatileTip q
