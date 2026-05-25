{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Experimental.Tx.Internal.Validate
  ( queryValidateTx
  , QueryValidateTxError (..)
  )
where

import Cardano.Api.Block (ChainPoint (..))
import Cardano.Api.Era
import Cardano.Api.Experimental.Era (IsEra, LedgerEra, obtainCommonConstraints, useEra)
import Cardano.Api.Experimental.Tx.Internal.Type (SignedTx (..))
import Cardano.Api.Genesis.Internal (shelleyGenesisDefaults)
import Cardano.Api.Genesis.Internal.Parameters
import Cardano.Api.Network.IPC (LocalStateQueryExpr)
import Cardano.Api.Network.IPC.Internal.Version (UnsupportedNtcVersionError)
import Cardano.Api.Network.Internal.NetworkId (NetworkMagic (..), toNetworkMagic, toShelleyNetwork)
import Cardano.Api.Query.Internal.Expr
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.Tx.Internal.TxIn (fromShelleyTxIn)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.BaseTypes (boundRational)
import Cardano.Ledger.Coin (unCoin)
import Cardano.Ledger.Core (allInputsTxBodyF, bodyTxL)
import Cardano.Ledger.Shelley.API (ApplyTxError, EpochState (..), LedgerEnv (..), applyTx)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..), mkShelleyGlobals)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL)
import Cardano.Ledger.State (utxoL)
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Set qualified as Set
import Lens.Micro (set, (^.))

data QueryValidateTxError
  = QueryValidateTxUnsupportedNtcVersion UnsupportedNtcVersionError
  | QueryValidateTxEraMismatch EraMismatch
  | QueryValidateTxEpochStateDecodeError CBOR.DecoderError
  deriving Show

-- | Run applyTx against the node's current ledger state and return the raw result.
--
-- TODO: Replace this provisional implementation (which queries the full
-- EpochState from the node and runs applyTx client-side) with a dedicated
-- node-side consensus query that runs applyTx server-side without
-- transferring the ledger state. The replacement should have the same type
-- signature as a normal local state query.
queryValidateTx
  :: forall era block point r
   . IsEra era
  => SignedTx era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either QueryValidateTxError (Either (ApplyTxError (LedgerEra era)) ()))
queryValidateTx (SignedTx ledgerTx) = obtainCommonConstraints (useEra @era) $ do
  let sbe = convert (useEra @era)
  eGenParams <- queryGenesisParameters sbe
  eSerEpochState <- queryCurrentEpochState sbe
  eEraHistory <- queryEraHistory
  eChainPoint <- queryChainPoint
  -- QueryCurrentEpochState uses DebugEpochState (QFNoTables), so the
  -- returned epoch state has empty UTxO tables.  We query the relevant
  -- UTxOs separately and inject them before running applyTx.
  let relevantTxIns =
        Set.map
          fromShelleyTxIn
          (ledgerTx ^. bodyTxL . allInputsTxBodyF)
  eUtxo <- queryUtxo sbe (QueryUTxOByTxIn relevantTxIns)
  return $ do
    genParams <-
      first QueryValidateTxUnsupportedNtcVersion eGenParams
        >>= first QueryValidateTxEraMismatch
    serEpochState <-
      first QueryValidateTxUnsupportedNtcVersion eSerEpochState
        >>= first QueryValidateTxEraMismatch
    eraHistory <- first QueryValidateTxUnsupportedNtcVersion eEraHistory
    chainPoint <- first QueryValidateTxUnsupportedNtcVersion eChainPoint
    utxo <-
      first QueryValidateTxUnsupportedNtcVersion eUtxo
        >>= first QueryValidateTxEraMismatch
    epochState <-
      first QueryValidateTxEpochStateDecodeError $
        decodeCurrentEpochState sbe serEpochState
    let CurrentEpochState es = epochState
        LedgerEpochInfo epochInfo = toLedgerEpochInfo eraHistory
        globals = mkShelleyGlobals (toShelleyGenesis genParams) epochInfo
        slotNo = chainPointToSlotNo chainPoint
    Right $
      shelleyBasedEraConstraints sbe $
        let esWithUtxo = set utxoL (toLedgerUTxO sbe utxo) es
         in void $ applyTx globals (mkMempoolEnv @era esWithUtxo slotNo) (esLState esWithUtxo) ledgerTx

chainPointToSlotNo :: ChainPoint -> SlotNo
chainPointToSlotNo ChainPointAtGenesis = SlotNo 0
chainPointToSlotNo (ChainPoint slotNo _) = slotNo

mkMempoolEnv
  :: forall era. IsEra era => EpochState (LedgerEra era) -> SlotNo -> LedgerEnv (LedgerEra era)
mkMempoolEnv epochState slotNo =
  obtainCommonConstraints (useEra @era) $
    LedgerEnv
      { ledgerSlotNo = slotNo
      , ledgerEpochNo = Nothing
      , ledgerIx = minBound
      , ledgerPp = epochState ^. curPParamsEpochStateL
      , ledgerAccount = esChainAccountState epochState
      }

toShelleyGenesis :: GenesisParameters ShelleyEra -> ShelleyGenesis
toShelleyGenesis gp =
  shelleyGenesisDefaults
    { sgSystemStart = protocolParamSystemStart gp
    , sgNetworkMagic = let NetworkMagic m = toNetworkMagic (protocolParamNetworkId gp) in m
    , sgNetworkId = toShelleyNetwork (protocolParamNetworkId gp)
    , sgActiveSlotsCoeff = case boundRational (protocolParamActiveSlotsCoefficient gp) of
        Nothing -> error "toShelleyGenesis: invalid activeSlotsCoefficient"
        Just r -> r
    , sgSecurityParam = protocolParamSecurity gp
    , sgEpochLength = protocolParamEpochLength gp
    , sgSlotsPerKESPeriod = fromIntegral (protocolParamSlotsPerKESPeriod gp)
    , sgMaxKESEvolutions = fromIntegral (protocolParamMaxKESEvolutions gp)
    , sgUpdateQuorum = fromIntegral (protocolParamUpdateQuorum gp)
    , sgMaxLovelaceSupply = fromIntegral (unCoin (protocolParamMaxLovelaceSupply gp))
    , sgProtocolParams = protocolInitialUpdateableProtocolParameters gp
    }
