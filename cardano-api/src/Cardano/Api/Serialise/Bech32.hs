{-# OPTIONS_GHC -Wno-orphans #-}

-- | Bech32 Serialisation
--
-- The types and functions of this module now live in the cardano-keys package;
-- re-exported here for compatibility, together with the 'Error' instance this
-- API adds on top of them.
module Cardano.Api.Serialise.Bech32
  ( module Cardano.Keys.Serialise.Bech32
  )
where

import Cardano.Api.Error (Error (..))

import Cardano.Keys.Serialise.Bech32

instance Error Bech32DecodeError where
  prettyError = renderBech32DecodeError
