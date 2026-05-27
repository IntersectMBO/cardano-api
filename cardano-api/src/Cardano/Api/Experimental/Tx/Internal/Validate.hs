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

import Cardano.Api.Era.Internal.Eon.Convert (convert)
import Cardano.Api.Experimental.Era (IsEra, LedgerEra, obtainCommonConstraints, useEra)
import Cardano.Api.Experimental.Tx.Internal.Type (SignedTx (..))
import Cardano.Api.Network.IPC (LocalStateQueryExpr)
import Cardano.Api.Network.IPC.Internal.Version (UnsupportedNtcVersionError)
import Cardano.Api.Query.Internal.Expr
import Cardano.Api.Query.Internal.Type.QueryInMode
import Cardano.Api.Tx.Internal.Sign (Tx (..))

import Cardano.Ledger.Shelley.API (ApplyTxError)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

data QueryValidateTxError
  = QueryValidateTxUnsupportedNtcVersion UnsupportedNtcVersionError
  | QueryValidateTxEraMismatch EraMismatch
  deriving Show

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
  let tx = ShelleyTx (convert (useEra @era)) ledgerTx
  result <- querySbe (useEra @era) (QueryValidateTx tx)
  return $ case result of
    Left err -> Left (QueryValidateTxUnsupportedNtcVersion err)
    Right (Left mismatch) -> Left (QueryValidateTxEraMismatch mismatch)
    Right (Right r) -> Right r
