{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Predicate
  ( matchesUtxoPredicate
  , exactAddressPredicate
  , extractAddressesFromPredicate
  , matchesAddressPattern
  , matchesAssetPattern
  , matchesTxOutputPattern
  , matchesAnyUtxoPattern
  , serialisePaymentCredential
  , serialiseStakeCredential
  )
where

import Cardano.Api.Address
import Cardano.Api.Era
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc

import RIO hiding (toList)

import Data.ByteString qualified as BS
import Data.ProtoLens (defMessage)
import Data.Set qualified as Set
import GHC.IsList
import Network.GRPC.Spec (Proto (..))

-- | Check if a UTxO entry matches a 'UtxoPredicate'.
-- All present fields are combined with AND logic.
matchesUtxoPredicate
  :: IsCardanoEra era
  => UtxoRpc.UtxoPredicate
  -> TxOut CtxUTxO era
  -> Bool
matchesUtxoPredicate p txOut =
  all (`matchesAnyUtxoPattern` txOut) (p ^. UtxoRpc.maybe'match)
    && not (any (`matchesUtxoPredicate` txOut) (p ^. UtxoRpc.not))
    && all (`matchesUtxoPredicate` txOut) (p ^. UtxoRpc.allOf)
    && (null (p ^. UtxoRpc.anyOf) || any (`matchesUtxoPredicate` txOut) (p ^. UtxoRpc.anyOf))

-- | Check if a UTxO entry matches an 'AnyUtxoPattern'.
-- Delegates to the Cardano-specific 'TxOutputPattern' if present.
matchesAnyUtxoPattern
  :: IsCardanoEra era
  => UtxoRpc.AnyUtxoPattern
  -> TxOut CtxUTxO era
  -> Bool
matchesAnyUtxoPattern pat txOut =
  all (`matchesTxOutputPattern` txOut) (pat ^. UtxoRpc.maybe'cardano)

-- | Check if a tx output matches a 'TxOutputPattern'.
-- Address and asset filters are combined with AND; absent fields are vacuously true.
matchesTxOutputPattern
  :: IsCardanoEra era
  => UtxoRpc.TxOutputPattern
  -> TxOut CtxUTxO era
  -> Bool
matchesTxOutputPattern pat (TxOut addrInEra txOutValue _datum _script) =
  all (`matchesAddressPattern` addrInEra) (pat ^. UtxoRpc.maybe'address)
    && all (`matchesAssetPattern` txOutValueToValue txOutValue) (pat ^. UtxoRpc.maybe'asset)

-- | Check if an address matches an 'AddressPattern'.
-- All present fields (exact, payment, delegation) must match (AND logic).
-- Byron addresses only support exact matching; payment\/delegation filters reject them.
matchesAddressPattern
  :: IsCardanoEra era
  => UtxoRpc.AddressPattern
  -> AddressInEra era
  -> Bool
matchesAddressPattern pat addr =
  exactMatch && paymentMatch && delegationMatch
 where
  -- proto3 optional bytes defaults to empty when absent
  matchesRawField field actual = BS.null field || field == actual

  exactMatch = matchesRawField (pat ^. UtxoRpc.exactAddress) $ serialiseToRawBytes addr
  paymentMatch = case addr of
    AddressInEra ShelleyAddressInEra{} (ShelleyAddress _ payCred _) ->
      matchesRawField (pat ^. UtxoRpc.paymentPart) . serialisePaymentCredential $
        fromShelleyPaymentCredential payCred
    _ -> BS.null $ pat ^. UtxoRpc.paymentPart
  delegationMatch = case addr of
    AddressInEra ShelleyAddressInEra{} (ShelleyAddress _ _ stakeRef) ->
      case fromShelleyStakeReference stakeRef of
        StakeAddressByValue cred ->
          matchesRawField (pat ^. UtxoRpc.delegationPart) $ serialiseStakeCredential cred
        _ -> BS.null $ pat ^. UtxoRpc.delegationPart
    _ -> BS.null $ pat ^. UtxoRpc.delegationPart

-- | A 'UtxoPredicate' matching UTxOs at the exact address.
exactAddressPredicate
  :: IsCardanoEra era
  => AddressInEra era
  -> Proto UtxoRpc.UtxoPredicate
exactAddressPredicate address =
  Proto $
    defMessage
      & UtxoRpc.match
        .~ ( defMessage
               & UtxoRpc.cardano
                 .~ (defMessage & UtxoRpc.address .~ (defMessage & UtxoRpc.exactAddress .~ serialiseToRawBytes address))
           )

-- | Serialise a 'PaymentCredential' to raw bytes (the key or script hash).
serialisePaymentCredential :: PaymentCredential -> ByteString
serialisePaymentCredential (PaymentCredentialByKey h) = serialiseToRawBytes h
serialisePaymentCredential (PaymentCredentialByScript h) = serialiseToRawBytes h

-- | Serialise a 'StakeCredential' to raw bytes (the key or script hash).
serialiseStakeCredential :: StakeCredential -> ByteString
serialiseStakeCredential (StakeCredentialByKey h) = serialiseToRawBytes h
serialiseStakeCredential (StakeCredentialByScript h) = serialiseToRawBytes h

-- | Check if a 'Value' contains a native asset matching an 'AssetPattern'.
-- Ada entries are always skipped; zero-quantity entries do not match.
matchesAssetPattern
  :: UtxoRpc.AssetPattern
  -> Value
  -> Bool
matchesAssetPattern pat value =
  any matchesEntry (toList value)
 where
  patternPolicy = pat ^. UtxoRpc.policyId
  patternTokenName = pat ^. UtxoRpc.assetName
  matchesEntry (AssetId policy tokenName, Quantity qty) =
    (BS.null patternPolicy || serialiseToRawBytes policy == patternPolicy)
      && (BS.null patternTokenName || serialiseToRawBytes tokenName == patternTokenName)
      && qty > 0
  matchesEntry (AdaAssetId, _) = False

-- | Try to extract a set of exact addresses from the predicate for use with 'QueryUTxOByAddress'.
-- Returns 'Just' if the optimization is applicable, 'Nothing' otherwise.
extractAddressesFromPredicate :: UtxoRpc.UtxoPredicate -> Maybe (Set AddressAny)
extractAddressesFromPredicate p =
  case (p ^. UtxoRpc.maybe'match, p ^. UtxoRpc.not, p ^. UtxoRpc.allOf, p ^. UtxoRpc.anyOf) of
    (Just pat, [], [], []) -> extractAddressFromPattern pat
    (Nothing, [], [], anyPreds@(_ : _)) ->
      Set.unions <$> traverse extractAddressesFromPredicate anyPreds
    _ -> Nothing
 where
  extractAddressFromPattern :: UtxoRpc.AnyUtxoPattern -> Maybe (Set AddressAny)
  extractAddressFromPattern pat = do
    txoPat <- pat ^. UtxoRpc.maybe'cardano
    addrPat <- txoPat ^. UtxoRpc.maybe'address
    let exact = addrPat ^. UtxoRpc.exactAddress
    guard $ not (BS.null exact)
    addrAny <- either (const Nothing) Just $ deserialiseFromRawBytes AsAddressAny exact
    pure $ Set.singleton addrAny
