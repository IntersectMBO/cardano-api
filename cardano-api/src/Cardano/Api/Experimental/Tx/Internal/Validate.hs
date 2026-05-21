{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Experimental.Tx.Internal.Validate
  ( queryValidateTx
  , QueryValidateTxError (..)
  )
where

import Cardano.Api.Block (ChainPoint (..))
import Cardano.Api.Era
import Cardano.Api.Genesis.Internal (shelleyGenesisDefaults)
import Cardano.Api.Genesis.Internal.Parameters
import Cardano.Api.Network.IPC (LocalStateQueryExpr)
import Cardano.Api.Network.IPC.Internal.Version (UnsupportedNtcVersionError)
import Cardano.Api.Network.Internal.NetworkId (NetworkId (..), NetworkMagic (..), toShelleyNetwork)
import Cardano.Api.Query.Internal.Expr
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.Tx.Internal.Sign (Tx (..))

import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.BaseTypes (boundRational)
import Cardano.Ledger.Coin (unCoin)
import Cardano.Ledger.Shelley.API (ApplyTxError, EpochState (..), LedgerEnv (..), applyTx)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..), mkShelleyGlobals)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL)
import Cardano.Slotting.Slot (SlotNo (..))

import Control.Monad (void)
import Data.Bifunctor (first)
import Lens.Micro ((^.))

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
   . ShelleyBasedEra era
  -> Tx era
  -> LocalStateQueryExpr
       block
       point
       QueryInMode
       r
       IO
       (Either QueryValidateTxError (Either (ApplyTxError (ShelleyLedgerEra era)) ()))
queryValidateTx sbe (ShelleyTx _sbe ledgerTx) = do
  eGenParams <- queryGenesisParameters sbe
  eSerEpochState <- queryCurrentEpochState sbe
  eEraHistory <- queryEraHistory
  eChainPoint <- queryChainPoint
  return $ do
    genParams <-
      first QueryValidateTxUnsupportedNtcVersion eGenParams
        >>= first QueryValidateTxEraMismatch
    serEpochState <-
      first QueryValidateTxUnsupportedNtcVersion eSerEpochState
        >>= first QueryValidateTxEraMismatch
    eraHistory <- first QueryValidateTxUnsupportedNtcVersion eEraHistory
    chainPoint <- first QueryValidateTxUnsupportedNtcVersion eChainPoint
    epochState <- first QueryValidateTxEpochStateDecodeError $
      decodeCurrentEpochState sbe serEpochState
    let CurrentEpochState es = epochState
        LedgerEpochInfo epochInfo = toLedgerEpochInfo eraHistory
        globals = mkShelleyGlobals (toShelleyGenesis genParams) epochInfo
        slotNo = chainPointToSlotNo chainPoint
    Right $ shelleyBasedEraConstraints sbe $
      void $ applyTx globals (mkMempoolEnv sbe es slotNo) (esLState es) ledgerTx

chainPointToSlotNo :: ChainPoint -> SlotNo
chainPointToSlotNo ChainPointAtGenesis = SlotNo 0
chainPointToSlotNo (ChainPoint slotNo _) = slotNo

mkMempoolEnv
  :: ShelleyBasedEra era -> EpochState (ShelleyLedgerEra era) -> SlotNo -> LedgerEnv (ShelleyLedgerEra era)
mkMempoolEnv sbe epochState slotNo =
  shelleyBasedEraConstraints sbe $
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
    , sgNetworkMagic = case protocolParamNetworkId gp of
        Mainnet -> 764824073
        Testnet (NetworkMagic m) -> m
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
