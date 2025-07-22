{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Server.Internal.Orphans () where

import Cardano.Api.Address
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import RIO hiding (toList)

import Data.ProtoLens (defMessage)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Text.Encoding qualified as T
import GHC.IsList
import Network.GRPC.Spec

---------------
-- Conversion
---------------

-- It's easier to use 'Proto a' wrappers for RPC types, because it makes lens automatically available.

-- TODO: write property tests for bijections

instance Inject (Proto UtxoRpc.RationalNumber) (Ratio Integer) where
  inject r = r ^. #numerator . to fromIntegral % r ^. #denominator . to fromIntegral

instance Inject (Ratio Integer) (Proto UtxoRpc.RationalNumber) where
  inject r =
    defMessage
      & #numerator .~ fromIntegral (numerator r)
      & #denominator .~ fromIntegral (denominator r)

instance Inject (Proto UtxoRpc.ExUnits) L.ExUnits where
  inject r =
    L.ExUnits
      { L.exUnitsMem = r ^. #memory . to fromIntegral
      , L.exUnitsSteps = r ^. #steps . to fromIntegral
      }

instance Inject L.ExUnits (Proto UtxoRpc.ExUnits) where
  inject L.ExUnits{L.exUnitsMem = mem, L.exUnitsSteps = steps} =
    defMessage
      & #memory .~ fromIntegral mem
      & #steps .~ fromIntegral steps

-- | Note that conversion is not total in the other direction
instance Inject TxIn (Proto UtxoRpc.TxoRef) where
  inject (TxIn txId' (TxIx txIx)) =
    defMessage
      & #hash .~ serialiseToRawBytes txId'
      & #index .~ fromIntegral txIx

instance Inject (ReferenceScript era) (Proto UtxoRpc.Script) where
  inject ReferenceScriptNone = defMessage
  inject (ReferenceScript _ (ScriptInAnyLang _ script)) =
    case script of
      SimpleScript _ ->
        defMessage & #native .~ serialiseToCBOR script
      PlutusScript PlutusScriptV1 ps ->
        defMessage & #plutusV1 .~ serialiseToRawBytes ps
      PlutusScript PlutusScriptV2 ps ->
        defMessage & #plutusV2 .~ serialiseToRawBytes ps
      PlutusScript PlutusScriptV3 ps ->
        defMessage & #plutusV3 .~ serialiseToRawBytes ps

instance IsCardanoEra era => Inject (UTxO era) [Proto UtxoRpc.AnyUtxoData] where
  inject utxo =
    toList utxo <&> \(txIn, TxOut addressInEra txOutValue datum script) -> do
      let multiAsset =
            fromList $
              toList (valueToPolicyAssets $ txOutValueToValue txOutValue) <&> \(pId, policyAssets) -> do
                let assets =
                      toList policyAssets <&> \(assetName, Quantity qty) -> do
                        defMessage
                          & #name .~ serialiseToRawBytes assetName
                          -- we don't have access to info it the coin was minted in the transaction,
                          -- maybe we should add it later
                          & #maybe'mintCoin .~ Nothing
                          & #outputCoin .~ fromIntegral qty
                defMessage
                  & #policyId .~ serialiseToRawBytes pId
                  & #assets .~ assets
          datumRpc = case datum of
            TxOutDatumNone ->
              defMessage
            TxOutDatumHash _ scriptDataHash ->
              defMessage
                & #hash .~ serialiseToRawBytes scriptDataHash
                & #originalCbor .~ mempty -- we don't have it
            TxOutDatumInline _ hashableScriptData ->
              defMessage
                & #hash .~ serialiseToRawBytes (hashScriptDataBytes hashableScriptData)
                & #originalCbor .~ getOriginalScriptDataBytes hashableScriptData

          protoTxOut =
            defMessage
              -- TODO we don't have serialiseToRawBytes for AddressInEra, so perhaps this is wrong, because 'address'
              -- has type bytes, but we're putting text there
              & #address .~ T.encodeUtf8 (cardanoEraConstraints (cardanoEra @era) $ serialiseAddress addressInEra)
              & #coin .~ fromIntegral (L.unCoin (txOutValueToLovelace txOutValue))
              & #assets .~ multiAsset
              & #datum .~ datumRpc
              & #script .~ inject script
      defMessage
        & #nativeBytes .~ "" -- TODO where to get that from? run cbor serialisation of utxos list?
        & #txoRef .~ inject txIn
        & #cardano .~ protoTxOut

-----------
-- Errors
-----------

-- TODO add RIO to cardano-api and move this instance there

instance Error StringException where
  prettyError = pshow
