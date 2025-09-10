{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Experimental.Plutus.Internal.Script
  ( PlutusScriptInEra (..)
  , PlutusScriptOrReferenceInput (..)
  )
where

import Cardano.Api.Tx.Internal.TxIn (TxIn)

import Cardano.Ledger.Plutus.Language (PlutusRunnable)
import Cardano.Ledger.Plutus.Language qualified as L

-- | A Plutus script in a particular era.
-- Why PlutusRunnable? Mainly for deserialization benefits.
-- The deserialization of this type looks at the
-- major protocol version and the script language to determine if
-- indeed the script is runnable. This is a dramatic improvement over the old api
-- which essentially read a 'ByteString' and hoped for the best.
-- Any failures due to malformed/invalid scripts were caught upon transaction
-- submission or running the script when attempting to predict the necessary execution units.
--
-- Where do we get the major protocol version from?
-- In order to access the major protocol version we pass in an 'era` type parameter which
-- can be translated to the major protocol version.
--
-- Where do we get the script language from?
-- The serialized version of 'PlutusRunnable' encodes the script language.
-- See `DecCBOR (PlutusRunnable l)` in cardano-ledger for more details.
data PlutusScriptInEra (lang :: L.Language) era where
  PlutusScriptInEra :: PlutusRunnable lang -> PlutusScriptInEra lang era

deriving instance Show (PlutusScriptInEra lang era)

deriving instance Eq (PlutusScriptInEra lang era)

-- | You can provide the plutus script directly in the transaction
-- or a reference input that points to the script in the UTxO.
-- Using a reference script saves space in your transaction.
data PlutusScriptOrReferenceInput lang era
  = PScript (PlutusScriptInEra lang era)
  | PReferenceScript TxIn
  deriving (Show, Eq)
