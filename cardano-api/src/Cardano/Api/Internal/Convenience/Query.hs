{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Convenience query functions
module Cardano.Api.Internal.Convenience.Query
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

import           Cardano.Api.Internal.Address
import           Cardano.Api.Internal.Certificate
import           Cardano.Api.Internal.Eon.ConwayEraOnwards
import           Cardano.Api.Internal.Eon.ShelleyBasedEra
import           Cardano.Api.Internal.Eras
import           Cardano.Api.Internal.Feature (Featured (..))
import           Cardano.Api.Internal.IO
import           Cardano.Api.Internal.IPC
import           Cardano.Api.Internal.IPC.Monad
import           Cardano.Api.Internal.Monad.Error
import           Cardano.Api.Internal.NetworkId
import           Cardano.Api.Internal.ProtocolParameters
import           Cardano.Api.Internal.Query
import           Cardano.Api.Internal.Query.Expr
import           Cardano.Api.Internal.Tx.Body
import           Cardano.Api.Internal.Utils

import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.CertState (DRepState (..))
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Keys as L
import qualified Cardano.Ledger.Shelley.LedgerState as L
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

import           Control.Exception.Safe (SomeException, displayException)
import           Control.Monad
import           Data.Bifunctor (first)
import           Data.Function ((&))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import           Data.Text (Text)
import           GHC.Exts (IsList (..), IsString (..))

data QueryConvenienceError
  = AcqFailure AcquiringFailure
  | QueryEraMismatch EraMismatch
  | ByronEraNotSupported
  | QceUnsupportedNtcVersion !UnsupportedNtcVersionError
  | QceUnexpectedException !SomeException
  deriving Show

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
renderQueryConvenienceError (QceUnsupportedNtcVersion (UnsupportedNtcVersionError minNtcVersion ntcVersion)) =
  "Unsupported feature for the node-to-client protocol version.\n"
    <> "This query requires at least "
    <> textShow minNtcVersion
    <> " but the node negotiated "
    <> textShow ntcVersion
    <> ".\n"
    <> "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."
renderQueryConvenienceError (QceUnexpectedException e) =
  "Unexpected exception while processing query:\n" <> fromString (displayException e)

newtype TxCurrentTreasuryValue = TxCurrentTreasuryValue {unTxCurrentTreasuryValue :: L.Coin}
  deriving newtype Show

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Internal.Convenience.Construction.constructBalancedTx
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
           , Map (L.Credential L.DRepRole L.StandardCrypto) L.Coin
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
          L.AccountState{L.asTreasury} <-
            lift (queryAccountState cOnwards)
              & onLeft (left . QceUnsupportedNtcVersion)
              & onLeft (left . QueryEraMismatch)
          let txCurrentTreasuryValue = TxCurrentTreasuryValue asTreasury
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
