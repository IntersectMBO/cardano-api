{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Experimental.Simple.Script
  ( SimpleScript (..)
  , SimpleScriptOrReferenceInput (..)
  )
where

import Cardano.Api.Tx.Internal.TxIn (TxIn)

import Cardano.Ledger.Core qualified as Ledger

-- | A simple script in a particular era. We leverage ledger's Cardano.Api.Experimental.ErasraScript
-- type class methods to work with the script.
data SimpleScript era where
  SimpleScript :: Ledger.EraScript era => Ledger.NativeScript era -> SimpleScript era

deriving instance Show (SimpleScript era)

data SimpleScriptOrReferenceInput era
  = SScript (SimpleScript era)
  | SReferenceScript TxIn
  deriving Show
