-- | Leios specific key types and their 'Key' class instances
--
-- This module now lives in the cardano-keys package; re-exported here for compatibility.
module Cardano.Api.Key.Internal.Leios
  ( module Cardano.Keys.Leios

    -- * Internal conversion functions
  , toLedgerBlsKey
  )
where

import Cardano.Crypto.DSIGN.BLS12381 qualified as Crypto
import Cardano.Crypto.DSIGN.Class qualified as Crypto
import Cardano.Keys.Leios
import Cardano.Ledger.State qualified as Ledger

-- | Derive the pair a pool has to register -- verification key plus proof of
-- possession -- from a BLS signing key, which is otherwise fiddly to assemble
-- from the pieces 'Cardano.Keys.Leios' exports.
--
-- Both halves are derived straight from the signing key rather than via
-- 'createBlsPossessionProof', because 'BlsPossessionProof' is exported from
-- 'Cardano.Keys.Leios' as an abstract type, so its payload cannot be unwrapped
-- here.
toLedgerBlsKey :: SigningKey BlsKey -> Ledger.BlsKey
toLedgerBlsKey (BlsSigningKey sk) =
  Ledger.BlsKey
    { Ledger.blsPubKey = Crypto.deriveVerKeyDSIGN sk
    , Ledger.blsPossessionProof = Crypto.createPossessionProofDSIGN Crypto.minSigPoPDST sk
    }
