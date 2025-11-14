{-# LANGUAGE GADTs #-}

module Cardano.Api.Experimental.Script
  ( AnyScript (..)
  )
where

import Cardano.Api.Experimental.Plutus.Internal.Script
import Cardano.Api.Experimental.Simple.Script

data AnyScript era where
  SimpleScript :: SimpleScriptOrReferenceInput era -> AnyScript era
  PlutusScript :: PlutusScriptOrReferenceInput lang era -> AnyScript era
