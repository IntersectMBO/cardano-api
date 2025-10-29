module Cardano.Api.Experimental.Serialise.TextEnvelope.Internal
  ( textEnvelopeToJSONEra
  )
where

import Cardano.Api.Experimental.Era
import Cardano.Api.Serialise.TextEnvelope.Internal
  ( HasTextEnvelope
  , TextEnvelopeDescr
  , textEnvelopeToJSON
  )

import Data.ByteString.Lazy qualified as LBS

textEnvelopeToJSONEra
  :: HasTextEnvelope (f (LedgerEra era))
  => Era era
  -> Maybe TextEnvelopeDescr
  -> f (LedgerEra era)
  -> LBS.ByteString
textEnvelopeToJSONEra era mDesc c =
  obtainCommonConstraints era $ textEnvelopeToJSON mDesc c
