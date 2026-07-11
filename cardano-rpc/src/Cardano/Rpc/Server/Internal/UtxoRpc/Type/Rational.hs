module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Rational
  ( utxoRpcRationalNumberToRational
  )
where

import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c

import RIO

import Data.Ratio ((%))
import Network.GRPC.Spec

-- | Convert a protobuf 'U5c.RationalNumber' into a 'Rational'.
--
-- Fails with 'Nothing' when the denominator is zero, which is also the
-- default value of the protobuf field when it is omitted.
utxoRpcRationalNumberToRational :: Proto U5c.RationalNumber -> Maybe Rational
utxoRpcRationalNumberToRational r =
  case r ^. U5c.denominator of
    0 -> Nothing
    den -> Just $ r ^. U5c.numerator . to fromIntegral % fromIntegral den
