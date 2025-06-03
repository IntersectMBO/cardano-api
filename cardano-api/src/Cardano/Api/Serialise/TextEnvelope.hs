module Cardano.Api.Serialise.TextEnvelope
  ( -- * TextEnvelope Serialisation
    HasTextEnvelope (..)
  , textEnvelopeTypeInEra
  , TextEnvelope (..)
  , TextEnvelopeType (..)
  , TextEnvelopeDescr (..)
  , textEnvelopeRawCBOR
  , TextEnvelopeError (..)
  , serialiseToTextEnvelope
  , deserialiseFromTextEnvelope
  , readFileTextEnvelope
  , writeFileTextEnvelope
  , readTextEnvelopeFromFile
  , readTextEnvelopeOfTypeFromFile
  , textEnvelopeToJSON
  , serialiseTextEnvelope
  , legacyComparison

    -- ** Reading one of several key types
  , FromSomeType (..)
  , deserialiseFromTextEnvelopeAnyOf
  , readFileTextEnvelopeAnyOf

    -- ** Data family instances
  , AsType (..)

    -- * CDDL Serialisation
  , TextEnvelopeCddlError (..)
  , FromSomeTypeCDDL (..)
  , cddlTypeToEra

    -- ** Reading one of several transaction or key witness types
  , readFileTextEnvelopeCddlAnyOf
  , deserialiseFromTextEnvelopeCddlAnyOf
  , writeTxFileTextEnvelopeCddl
  , writeTxFileTextEnvelopeCanonicalCddl
  , writeTxWitnessFileTextEnvelopeCddl
  -- Exported for testing
  , deserialiseByronTxCddl
  , serialiseWitnessLedgerCddl
  , deserialiseWitnessLedgerCddl

    -- ** Byron tx serialization
  , serializeByronTx
  , writeByronTxFileTextEnvelopeCddl
  )
where

import Cardano.Api.Internal.SerialiseLedgerCddl
import Cardano.Api.Internal.SerialiseTextEnvelope
