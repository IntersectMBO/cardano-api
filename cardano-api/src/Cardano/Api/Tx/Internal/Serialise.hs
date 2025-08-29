{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Tx.Internal.Serialise
  ( deserialiseByronTx
  , deserialiseWitnessLedger
  , serialiseByronTx
  , serialiseTxToTextEnvelope
  , serialiseWitnessLedger
  , writeByronTxFileTextEnvelope
  , writeTxFileTextEnvelope
  , writeTxFileTextEnvelopeCanonical
  , writeTxWitnessFileTextEnvelope
  )
where

import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.IO
import Cardano.Api.Serialise.Cbor.Canonical
import Cardano.Api.Serialise.TextEnvelope.Internal
import Cardano.Api.Tx.Internal.Sign

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Ledger.Binary qualified as CBOR

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS

-- Text envelope serialisation

deserialiseByronTx :: TextEnvelope -> Either TextEnvelopeError (Byron.ATxAux ByteString)
deserialiseByronTx tec =
  first TextEnvelopeDecodeError $
    CBOR.decodeFullAnnotatedBytes
      CBOR.byronProtVer
      "Byron Tx"
      CBOR.decCBOR
      (LBS.fromStrict $ teRawCBOR tec)

serialiseByronTx :: Byron.ATxAux ByteString -> TextEnvelope
serialiseByronTx tx =
  TextEnvelope
    { teType = "Tx ByronEra"
    , teDescription = "Ledger Cddl Format"
    , teRawCBOR = CBOR.recoverBytes tx
    }

writeByronTxFileTextEnvelope
  :: File content Out
  -> Byron.ATxAux ByteString
  -> IO (Either (FileError ()) ())
writeByronTxFileTextEnvelope path =
  writeLazyByteStringFile path
    . serialiseTextEnvelope
    . serialiseByronTx

serialiseTxToTextEnvelope :: ShelleyBasedEra era -> Tx era -> TextEnvelope
serialiseTxToTextEnvelope era' tx' =
  shelleyBasedEraConstraints era' $ do
    serialiseToTextEnvelope (Just "Ledger Cddl Format") tx'

deserialiseWitnessLedger
  :: forall era
   . ShelleyBasedEra era
  -> TextEnvelope
  -> Either TextEnvelopeError (KeyWitness era)
deserialiseWitnessLedger sbe te =
  shelleyBasedEraConstraints sbe $
    legacyDecoding te $
      deserialiseFromTextEnvelope te
 where
  -- This wrapper ensures that we can still decode the key witness
  -- that were serialized before we migrated to using 'serialiseToTextEnvelope'.
  -- Previously we were differentiating between the bootstrap key witness and the
  -- shelley key witness with CBOR tags 0 and 1. This is a deviation from the ledger's
  -- CDDL. See `legacyKeyWitnessEncode` for more details.
  legacyDecoding
    :: TextEnvelope
    -> Either TextEnvelopeError (KeyWitness era)
    -> Either TextEnvelopeError (KeyWitness era)
  legacyDecoding TextEnvelope{teDescription, teRawCBOR} (Left (TextEnvelopeDecodeError _)) =
    case teDescription of
      "Key BootstrapWitness ShelleyEra" -> do
        w <-
          first TextEnvelopeDecodeError $
            CBOR.decodeFullAnnotator
              (eraProtVerLow sbe)
              "Shelley Witness"
              CBOR.decCBOR
              (LBS.fromStrict teRawCBOR)
        Right $ ShelleyBootstrapWitness sbe w
      "Key Witness ShelleyEra" -> do
        w <-
          first TextEnvelopeDecodeError $
            CBOR.decodeFullAnnotator
              (eraProtVerLow sbe)
              "Shelley Witness"
              CBOR.decCBOR
              (LBS.fromStrict teRawCBOR)
        Right $ ShelleyKeyWitness sbe w
      desc -> Left $ TextEnvelopeUnknownKeyWitness desc
  legacyDecoding _ v = v

serialiseWitnessLedger :: forall era. ShelleyBasedEra era -> KeyWitness era -> TextEnvelope
serialiseWitnessLedger sbe kw =
  shelleyBasedEraConstraints sbe $
    serialiseToTextEnvelope (Just $ TextEnvelopeDescr desc) kw
 where
  desc :: String
  desc = shelleyBasedEraConstraints sbe $ case kw of
    ShelleyBootstrapWitness{} -> "Key BootstrapWitness ShelleyEra"
    ShelleyKeyWitness{} -> "Key Witness ShelleyEra"

writeTxFileTextEnvelope
  :: ShelleyBasedEra era
  -> File content Out
  -> Tx era
  -> IO (Either (FileError ()) ())
writeTxFileTextEnvelope sbe path =
  writeLazyByteStringFile path
    . serialiseTextEnvelope
    . serialiseTxToTextEnvelope sbe

-- | Write transaction in the text envelope format. The CBOR will be in canonical format according
-- to RFC 7049. It is also a requirement of CIP-21, which is not fully implemented.
--
-- 1. RFC 7049: https://datatracker.ietf.org/doc/html/rfc7049#section-3.9
-- 2. CIP-21: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0021/README.md#canonical-cbor-serialization-format
writeTxFileTextEnvelopeCanonical
  :: ShelleyBasedEra era
  -> File content Out
  -> Tx era
  -> IO (Either (FileError ()) ())
writeTxFileTextEnvelopeCanonical sbe path =
  writeLazyByteStringFile path
    . serialiseTextEnvelope
    . canonicaliseTextEnvelopeCbor
    . serialiseTxToTextEnvelope sbe
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

writeTxWitnessFileTextEnvelope
  :: ShelleyBasedEra era
  -> File () Out
  -> KeyWitness era
  -> IO (Either (FileError ()) ())
writeTxWitnessFileTextEnvelope sbe path =
  writeLazyByteStringFile path
    . serialiseTextEnvelope
    . serialiseWitnessLedger sbe
