-- | Functions and types for working with delegation certificates in Byron
module Cardano.Api.ByronDelegation
  ( ACertificate (..)
  , isValid
  , signCertificate
  )
where

import           Cardano.Chain.Delegation (ACertificate (..), isValid, signCertificate)
