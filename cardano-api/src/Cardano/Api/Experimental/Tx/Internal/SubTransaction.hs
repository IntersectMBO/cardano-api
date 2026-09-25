module Cardano.Api.Experimental.Tx.Internal.SubTransaction
  ( SubTx (..)
  )
where

import Cardano.Api.Experimental.Simple.Script
import Cardano.Api.Experimental.Tx.Internal.AnyWitness (AnyWitness (..))
import Cardano.Api.Experimental.Tx.Internal.TopTx.BodyContent
  ( TxCertificates (..)
  , TxInsReference (..)
  , TxMintValue (..)
  , TxOut (..)
  , TxProposalProcedures (..)
  , TxVotingProcedures (..)
  , TxWithdrawals (..)
  )
import Cardano.Api.Ledger.Internal.Reexport (StrictMaybe (..))
import Cardano.Api.Ledger.Internal.Reexport qualified as L
import Cardano.Api.Tx.Internal.TxIn (TxIn)
import Cardano.Api.Tx.Internal.TxMetadata (TxMetadata (..))

import Cardano.Ledger.Api qualified as L

import Data.Map.Strict (Map)
import Data.OSet.Strict (OSet)

-- | Content of a Dijkstra sub-transaction body.
--
-- Compared to the top-level 'TxBodyContent', a sub-transaction has no collateral inputs,
-- no total/return collateral, no fee, no required signer key hashes (replaced by guards)
-- and no script validity flag.
data SubTx era
  = SubTx
  { subTxIns :: [(TxIn, AnyWitness era)]
  , subTxInsReference :: TxInsReference era
  , subTxOuts :: [TxOut era]
  , subTxCertificates :: TxCertificates era
  , subTxWithdrawals :: TxWithdrawals era
  , subTxValidityLowerBound :: Maybe L.SlotNo
  , subTxValidityUpperBound :: Maybe L.SlotNo
  , subTxMintValue :: TxMintValue era
  , subTxProtocolParams :: Maybe (L.PParams era)
  , subTxMetadata :: TxMetadata
  , subTxAuxScripts :: [SimpleScript era]
  , subTxProposalProcedures :: Maybe (TxProposalProcedures era)
  , subTxVotingProcedures :: Maybe (TxVotingProcedures era)
  , subTxCurrentTreasuryValue :: Maybe L.Coin
  , subTxTreasuryDonation :: Maybe L.Coin
  , subTxSupplementalDatums :: Map L.DataHash (L.Data era)
  -- ^ ------------------------------------------------------------
  -- Fields below are new in the Dijkstra era.
  -- ------------------------------------------------------------
  , subTxGuards :: OSet (L.Credential L.Guard)
  , subTxRequiredTopLevelGuards :: Map (L.Credential L.Guard) (StrictMaybe (L.Data era))
  , subTxDirectDeposits :: L.DirectDeposits
  , subTxAccountBalanceIntervals :: L.AccountBalanceIntervals era
  }
