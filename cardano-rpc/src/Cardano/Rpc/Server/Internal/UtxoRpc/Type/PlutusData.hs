{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Type.PlutusData
  ( scriptDataToUtxoRpcPlutusData
  , utxoRpcPlutusDataToScriptData
  )
where

import Cardano.Api.Era
import Cardano.Api.Plutus
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.BigInt

import RIO

import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

scriptDataToUtxoRpcPlutusData :: ScriptData -> Proto UtxoRpc.PlutusData
scriptDataToUtxoRpcPlutusData = \case
  ScriptDataBytes bs ->
    defMessage & U5c.boundedBytes .~ bs
  ScriptDataNumber int -> defMessage & U5c.bigInt .~ inject int
  ScriptDataList sds ->
    defMessage & U5c.array . U5c.items .~ map scriptDataToUtxoRpcPlutusData sds
  ScriptDataMap elements -> do
    let pairs =
          elements <&> \(k, v) ->
            defMessage
              & U5c.key .~ scriptDataToUtxoRpcPlutusData k
              & U5c.value .~ scriptDataToUtxoRpcPlutusData v
    defMessage & U5c.map . U5c.pairs .~ pairs
  ScriptDataConstructor tag args -> do
    -- Details of plutus tag serialisation:
    -- https://github.com/IntersectMBO/plutus/blob/fc78c36b545ee287ae8796a0c1a7d04cf31f4cee/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72
    let constr =
          defMessage
            & ( if tag <= fromIntegral (maxBound @Word32)
                  then U5c.tag .~ fromIntegral tag
                  else (U5c.tag .~ 102) . (U5c.anyConstructor .~ fromIntegral @_ @Word64 tag)
              )
            & U5c.fields .~ map scriptDataToUtxoRpcPlutusData args
    defMessage & U5c.constr .~ constr

utxoRpcPlutusDataToScriptData
  :: HasCallStack
  => MonadThrow m
  => Proto UtxoRpc.PlutusData
  -> m ScriptData
utxoRpcPlutusDataToScriptData pd
  | Just bs <- pd ^. U5c.maybe'boundedBytes =
      pure $ ScriptDataBytes bs
  | Just bigInt <- pd ^. U5c.maybe'bigInt =
      ScriptDataNumber <$> utxoRpcBigIntToInteger bigInt
  | Just arr <- pd ^. U5c.maybe'array =
      ScriptDataList <$> traverse utxoRpcPlutusDataToScriptData (arr ^. U5c.items)
  | Just m <- pd ^. U5c.maybe'map = do
      let convertPair pair = do
            k <- utxoRpcPlutusDataToScriptData $ pair ^. U5c.key
            v <- utxoRpcPlutusDataToScriptData $ pair ^. U5c.value
            pure (k, v)
      ScriptDataMap <$> traverse convertPair (m ^. U5c.pairs)
  | Just constr <- pd ^. U5c.maybe'constr = do
      let tag = constr ^. U5c.tag
          anyC = constr ^. U5c.anyConstructor
          -- Inverse of scriptDataToUtxoRpcPlutusData: tag 102 signals that the
          -- real constructor index lives in anyConstructor (for values > maxBound @Word32).
          -- See: https://github.com/IntersectMBO/plutus/blob/fc78c36b545ee287ae8796a0c1a7d04cf31f4cee/plutus-core/plutus-core/src/PlutusCore/Data.hs#L72
          resolvedTag =
            if tag == 102 && anyC /= 0
              then fromIntegral anyC
              else fromIntegral tag
      fields <- traverse utxoRpcPlutusDataToScriptData $ constr ^. U5c.fields
      pure $ ScriptDataConstructor resolvedTag fields
  | otherwise =
      throwM . stringException $ "utxoRpcPlutusDataToScriptData: no PlutusData variant set"
