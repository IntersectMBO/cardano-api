{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Experimental.Tx.Internal.Serialise
  ( writeTxFileTextEnvelope
  , writeTxFileTextEnvelopeCanonical
  )
where

import Cardano.Api.Error
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Tx.Internal.Type
import Cardano.Api.IO
import Cardano.Api.Serialise.Cbor.Canonical
import Cardano.Api.Serialise.TextEnvelope.Internal

writeTxFileTextEnvelope
  :: IsEra era
  => File content Out
  -> SignedTx era
  -> IO (Either (FileError ()) ())
writeTxFileTextEnvelope path =
  writeLazyByteStringFile path
    . serialiseTextEnvelope
    . serialiseTxToTextEnvelope

serialiseTxToTextEnvelope :: forall era. IsEra era => SignedTx era -> TextEnvelope
serialiseTxToTextEnvelope tx' =
  obtainCommonConstraints (useEra @era) $
    serialiseToTextEnvelope (Just "Ledger Cddl Format") tx'

-- | Write transaction in the text envelope format. The CBOR will be in canonical format according
-- to RFC 7049. It is also a requirement of CIP-21, which is not fully implemented.
--
-- 1. RFC 7049: https://datatracker.ietf.org/doc/html/rfc7049#section-3.9
-- 2. CIP-21: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0021/README.md#canonical-cbor-serialization-format
writeTxFileTextEnvelopeCanonical
  :: IsEra era
  => File content Out
  -> SignedTx era
  -> IO (Either (FileError ()) ())
writeTxFileTextEnvelopeCanonical path =
  writeLazyByteStringFile path
    . serialiseTextEnvelope
    . canonicaliseTextEnvelopeCbor
    . serialiseTxToTextEnvelope
 where
  canonicaliseTextEnvelopeCbor :: TextEnvelope -> TextEnvelope
  canonicaliseTextEnvelopeCbor te = do
    let canonicalisedTxBs =
          either
            ( \err ->
                error $
                  "writeTxFileTextEnvelopeCanonical: Impossible - deserialisation of just serialised bytes failed "
                    <> show err
            )
            id
            . canonicaliseCborBs
            $ teRawCBOR te
    te{teRawCBOR = canonicalisedTxBs}
