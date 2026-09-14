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
  , matchesTxPredicate
  , matchesAnyChainTxPattern
  , matchesTxPattern
  , matchesAddressPatternBytes
  , matchesAssetPatternProto
  , matchesCertificatePattern
  , credentialBytes
  , CertificateCredentials (..)
  , certificateCredentials
  , certificateStakeCredentials
  , certificatePoolKeyHashes
  , certificateDRepBytes
  )
where

import Cardano.Api.Address
import Cardano.Api.Era
import Cardano.Api.Serialise.Raw
import Cardano.Api.Tx
import Cardano.Api.Value
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Proto.Api.UtxoRpc.Submit qualified as Submit
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.BigInt (utxoRpcBigIntToInteger)

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
  => Proto UtxoRpc.UtxoPredicate
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
  => Proto UtxoRpc.AnyUtxoPattern
  -> TxOut CtxUTxO era
  -> Bool
matchesAnyUtxoPattern pat txOut =
  all (`matchesTxOutputPattern` txOut) (pat ^. UtxoRpc.maybe'cardano)

-- | Check if a tx output matches a 'TxOutputPattern'.
-- Address and asset filters are combined with AND; absent fields are vacuously true.
matchesTxOutputPattern
  :: IsCardanoEra era
  => Proto UtxoRpc.TxOutputPattern
  -> TxOut CtxUTxO era
  -> Bool
matchesTxOutputPattern pat (TxOut addrInEra txOutValue _datum _script) =
  all (`matchesAddressPattern` addrInEra) (pat ^. UtxoRpc.maybe'address)
    && all (`matchesAssetPattern` txOutValueToValue txOutValue) (pat ^. UtxoRpc.maybe'asset)

-- | proto3 optional bytes default to empty when absent; treat that as "don't care".
matchesRawField :: ByteString -> ByteString -> Bool
matchesRawField field actual = BS.null field || field == actual

-- | Check if an address matches an 'AddressPattern'.
-- All present fields (exact, payment, delegation) must match (AND logic).
-- Byron addresses only support exact matching; payment\/delegation filters reject them.
matchesAddressPattern
  :: IsCardanoEra era
  => Proto UtxoRpc.AddressPattern
  -> AddressInEra era
  -> Bool
matchesAddressPattern pat addr =
  exactMatch && paymentMatch && delegationMatch
 where
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
  :: Proto UtxoRpc.AssetPattern
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
extractAddressesFromPredicate :: Proto UtxoRpc.UtxoPredicate -> Maybe (Set AddressAny)
extractAddressesFromPredicate p =
  case (p ^. UtxoRpc.maybe'match, p ^. UtxoRpc.not, p ^. UtxoRpc.allOf, p ^. UtxoRpc.anyOf) of
    (Just pat, [], [], []) -> extractAddressFromPattern pat
    (Nothing, [], [], anyPreds@(_ : _)) ->
      Set.unions <$> traverse extractAddressesFromPredicate anyPreds
    _ -> Nothing
 where
  extractAddressFromPattern :: Proto UtxoRpc.AnyUtxoPattern -> Maybe (Set AddressAny)
  extractAddressFromPattern pat = do
    txoPat <- pat ^. UtxoRpc.maybe'cardano
    addrPat <- txoPat ^. UtxoRpc.maybe'address
    let exact = addrPat ^. UtxoRpc.exactAddress
    guard $ not (BS.null exact)
    addrAny <- either (const Nothing) Just $ deserialiseFromRawBytes AsAddressAny exact
    pure $ Set.singleton addrAny

-- ---------------------------------------------------------------------------
-- TxPredicate: matching a mempool\/submitted tx (proto-native, no ledger types)
-- ---------------------------------------------------------------------------

-- | Check if a tx matches a 'TxPredicate'.
-- All present fields are combined with AND logic.
matchesTxPredicate
  :: Proto Submit.TxPredicate
  -> Proto UtxoRpc.Tx
  -> Bool
matchesTxPredicate p tx =
  all (`matchesAnyChainTxPattern` tx) (p ^. Submit.maybe'match)
    && not (any (`matchesTxPredicate` tx) (p ^. Submit.not))
    && all (`matchesTxPredicate` tx) (p ^. Submit.allOf)
    && (null (p ^. Submit.anyOf) || any (`matchesTxPredicate` tx) (p ^. Submit.anyOf))

-- | Check if a tx matches an 'AnyChainTxPattern'.
-- Delegates to the Cardano-specific 'TxPattern' if present.
matchesAnyChainTxPattern
  :: Proto Submit.AnyChainTxPattern
  -> Proto UtxoRpc.Tx
  -> Bool
matchesAnyChainTxPattern pat tx =
  all (`matchesTxPattern` tx) (pat ^. Submit.maybe'cardano)

-- | Check if a tx matches a 'TxPattern'. All present fields are combined with
-- AND logic; absent fields are vacuously true.
--
-- 'consumes' and the input side of 'has_address'\/'moves_asset' rely on
-- 'TxInput.as_output', which the mempool tx conversion never populates
-- (resolving it needs a UTxO lookup outside the pure conversion) - these
-- fields never fire on real mempool traffic today, only in tests that build
-- fixtures with 'as_output' set.
matchesTxPattern
  :: Proto UtxoRpc.TxPattern
  -> Proto UtxoRpc.Tx
  -> Bool
matchesTxPattern pat tx =
  all (matchesConsumesPattern tx) (pat ^. UtxoRpc.maybe'consumes)
    && all (matchesProducesPattern tx) (pat ^. UtxoRpc.maybe'produces)
    && all (matchesHasAddressPattern tx) (pat ^. UtxoRpc.maybe'hasAddress)
    && all (matchesMovesAssetPattern tx) (pat ^. UtxoRpc.maybe'movesAsset)
    && all (matchesMintsAssetPattern tx) (pat ^. UtxoRpc.maybe'mintsAsset)
    && all (matchesHasCertificatePattern tx) (pat ^. UtxoRpc.maybe'hasCertificate)

-- | Resolved outputs of the tx's inputs (see the 'as_output' caveat on 'matchesTxPattern').
resolvedInputOutputs :: Proto UtxoRpc.Tx -> [Proto UtxoRpc.TxOutput]
resolvedInputOutputs tx = mapMaybe (^. UtxoRpc.maybe'asOutput) (tx ^. UtxoRpc.inputs)

-- | 'consumes': match any input (by its resolved output) that exhibits the pattern.
matchesConsumesPattern :: Proto UtxoRpc.Tx -> Proto UtxoRpc.TxOutputPattern -> Bool
matchesConsumesPattern tx pat = any (matchesTxOutputPatternProto pat) (resolvedInputOutputs tx)

-- | 'produces': match any output that exhibits the pattern.
matchesProducesPattern :: Proto UtxoRpc.Tx -> Proto UtxoRpc.TxOutputPattern -> Bool
matchesProducesPattern tx pat = any (matchesTxOutputPatternProto pat) (tx ^. UtxoRpc.outputs)

-- | Check if a tx output (an input's resolved output, or an output proper) matches a 'TxOutputPattern'.
matchesTxOutputPatternProto :: Proto UtxoRpc.TxOutputPattern -> Proto UtxoRpc.TxOutput -> Bool
matchesTxOutputPatternProto pat output =
  all
    (\addressPat -> matchesAddressPatternBytes addressPat (output ^. UtxoRpc.address))
    (pat ^. UtxoRpc.maybe'address)
    && all
      (\assetPat -> matchesAssetPatternProto (> 0) assetPat (output ^. UtxoRpc.assets))
      (pat ^. UtxoRpc.maybe'asset)

-- | 'has_address': match any address appearing in the tx's outputs, resolved
-- inputs, resolved collateral inputs or collateral return.
matchesHasAddressPattern :: Proto UtxoRpc.Tx -> Proto UtxoRpc.AddressPattern -> Bool
matchesHasAddressPattern tx pat = any (matchesAddressPatternBytes pat) (txAddresses tx)

-- | Every address appearing anywhere in the tx; see 'matchesHasAddressPattern'.
txAddresses :: Proto UtxoRpc.Tx -> [ByteString]
txAddresses tx =
  map (^. UtxoRpc.address) $
    resolvedInputOutputs tx
      <> tx ^. UtxoRpc.outputs
      <> mapMaybe (^. UtxoRpc.maybe'asOutput) (tx ^. UtxoRpc.collateral . UtxoRpc.collateral)
      <> maybeToList (tx ^. UtxoRpc.collateral . UtxoRpc.maybe'collateralReturn)

-- | Check if raw address bytes match an 'AddressPattern'.
-- Mirrors 'matchesAddressPattern', but for proto-native tx data where addresses
-- are opaque bytes rather than parsed 'AddressInEra' values: the bytes are
-- parsed via 'deserialiseFromRawBytes' first. Unparseable bytes and Byron
-- addresses only support exact matching, same as 'matchesAddressPattern'.
matchesAddressPatternBytes :: Proto UtxoRpc.AddressPattern -> ByteString -> Bool
matchesAddressPatternBytes pat addressBytes =
  exactMatch && paymentMatch && delegationMatch
 where
  exactMatch = matchesRawField (pat ^. UtxoRpc.exactAddress) addressBytes
  parsedAddress = either (const Nothing) Just $ deserialiseFromRawBytes AsAddressAny addressBytes
  paymentMatch = case parsedAddress of
    Just (AddressShelley (ShelleyAddress _ payCred _)) ->
      matchesRawField (pat ^. UtxoRpc.paymentPart) . serialisePaymentCredential $
        fromShelleyPaymentCredential payCred
    _ -> BS.null $ pat ^. UtxoRpc.paymentPart
  delegationMatch = case parsedAddress of
    Just (AddressShelley (ShelleyAddress _ _ stakeRef)) ->
      case fromShelleyStakeReference stakeRef of
        StakeAddressByValue cred ->
          matchesRawField (pat ^. UtxoRpc.delegationPart) $ serialiseStakeCredential cred
        _ -> BS.null $ pat ^. UtxoRpc.delegationPart
    _ -> BS.null $ pat ^. UtxoRpc.delegationPart

-- | 'moves_asset': match any asset moved by the tx, i.e. present (with a
-- positive quantity) in a resolved input or an output.
matchesMovesAssetPattern :: Proto UtxoRpc.Tx -> Proto UtxoRpc.AssetPattern -> Bool
matchesMovesAssetPattern tx =
  let movedAssets = concatMap (^. UtxoRpc.assets) (resolvedInputOutputs tx <> tx ^. UtxoRpc.outputs)
   in \pat -> matchesAssetPatternProto (> 0) pat movedAssets

-- | 'mints_asset': match any asset minted or burned by the tx. Burns carry a
-- negative quantity, so (unlike 'moves_asset') zero is the only excluded value.
matchesMintsAssetPattern :: Proto UtxoRpc.Tx -> Proto UtxoRpc.AssetPattern -> Bool
matchesMintsAssetPattern tx pat = matchesAssetPatternProto (/= 0) pat (tx ^. UtxoRpc.mint)

-- | Check if a policy\/asset-name pattern matches any asset entry across a
-- list of 'Multiasset' bundles. @quantityMatches@ selects which quantities
-- count, since 'moves_asset' and 'mints_asset' apply different sign checks.
-- A 'BigInt' that fails to decode is treated as no match.
matchesAssetPatternProto
  :: (Integer -> Bool)
  -> Proto UtxoRpc.AssetPattern
  -> [Proto UtxoRpc.Multiasset]
  -> Bool
matchesAssetPatternProto quantityMatches pat multiassets =
  any
    matchesEntry
    [ (multiasset ^. UtxoRpc.policyId, asset)
    | multiasset <- multiassets
    , asset <- multiasset ^. UtxoRpc.assets
    ]
 where
  patternPolicy = pat ^. UtxoRpc.policyId
  patternTokenName = pat ^. UtxoRpc.assetName
  matchesEntry (policy, asset) =
    matchesRawField patternPolicy policy
      && matchesRawField patternTokenName (asset ^. UtxoRpc.name)
      && maybe False quantityMatches (utxoRpcBigIntToInteger (asset ^. UtxoRpc.quantity))

-- | 'has_certificate': match any certificate in the tx that exhibits the pattern.
matchesHasCertificatePattern :: Proto UtxoRpc.Tx -> Proto UtxoRpc.CertificatePattern -> Bool
matchesHasCertificatePattern tx pat = any (matchesCertificatePattern pat) (tx ^. UtxoRpc.certificates)

-- | Check if a certificate matches a 'CertificatePattern'.
-- The five discriminated branches (stake registration\/deregistration\/delegation,
-- pool registration\/retirement) only match the identically-shaped certificate -
-- the newer Conway certificate families (reg\/unreg\/vote-deleg, DRep, committee)
-- are only reachable through the three "any_*" wildcards below, which scan
-- every certificate variant that carries the relevant credential.
matchesCertificatePattern
  :: Proto UtxoRpc.CertificatePattern
  -> Proto UtxoRpc.Certificate
  -> Bool
matchesCertificatePattern pat cert =
  all
    (`matchesExactCredential` (cert ^. UtxoRpc.maybe'stakeRegistration))
    (pat ^. UtxoRpc.maybe'stakeRegistration)
    && all
      (`matchesExactCredential` (cert ^. UtxoRpc.maybe'stakeDeregistration))
      (pat ^. UtxoRpc.maybe'stakeDeregistration)
    && all (matchesStakeDelegationPattern cert) (pat ^. UtxoRpc.maybe'stakeDelegation)
    && all (matchesPoolRegistrationPattern cert) (pat ^. UtxoRpc.maybe'poolRegistration)
    && all (matchesPoolRetirementPattern cert) (pat ^. UtxoRpc.maybe'poolRetirement)
    && all
      (matchesAnyStakeCredential (certStakeCredentials credentials))
      (nonEmpty $ pat ^. UtxoRpc.anyStakeCredential)
    && all
      (matchesAnyPoolKeyHash (certPoolKeyHashes credentials))
      (nonEmpty $ pat ^. UtxoRpc.anyPoolKeyhash)
    && all (matchesAnyDRep (certDRepBytes credentials)) (nonEmpty $ pat ^. UtxoRpc.anyDrep)
 where
  -- classified once, then read for all three any_* checks below
  credentials = certificateCredentials cert

  -- these three fields are oneof branches: empty means "this branch isn't set"
  nonEmpty bytes = if BS.null bytes then Nothing else Just bytes

  matchesExactCredential expected = maybe False ((== credentialBytes expected) . credentialBytes)

  matchesStakeDelegationPattern c delegPat = case c ^. UtxoRpc.maybe'stakeDelegation of
    Nothing -> False
    Just delegCert ->
      all
        (`matchesExactCredential` Just (delegCert ^. UtxoRpc.stakeCredential))
        (delegPat ^. UtxoRpc.maybe'stakeCredential)
        && matchesRawField (delegPat ^. UtxoRpc.poolKeyhash) (delegCert ^. UtxoRpc.poolKeyhash)

  -- a 'PoolRegistrationCert' only carries one key hash ('operator'); the
  -- pattern's 'pool_keyhash' field is documented as derived from it
  matchesPoolRegistrationPattern c regPat = case c ^. UtxoRpc.maybe'poolRegistration of
    Nothing -> False
    Just regCert ->
      let operatorKeyHash = regCert ^. UtxoRpc.operator
       in matchesRawField (regPat ^. UtxoRpc.operator) operatorKeyHash
            && matchesRawField (regPat ^. UtxoRpc.poolKeyhash) operatorKeyHash

  matchesPoolRetirementPattern c retirePat = case c ^. UtxoRpc.maybe'poolRetirement of
    Nothing -> False
    Just retireCert ->
      matchesRawField (retirePat ^. UtxoRpc.poolKeyhash) (retireCert ^. UtxoRpc.poolKeyhash)
        && (retirePat ^. UtxoRpc.epoch == 0 || retirePat ^. UtxoRpc.epoch == retireCert ^. UtxoRpc.epoch)

-- | The bare key\/script hash bytes of a credential, regardless of which it is.
credentialBytes :: Proto UtxoRpc.StakeCredential -> ByteString
credentialBytes cred = fromMaybe (cred ^. UtxoRpc.scriptHash) (cred ^. UtxoRpc.maybe'addrKeyHash)

-- | The addr-key\/script hash bytes of a 'DRep' vote target, or 'Nothing' for
-- an abstain\/no-confidence vote (which carries no hash).
voteDrepBytes :: Proto UtxoRpc.DRep -> Maybe ByteString
voteDrepBytes drep = drep ^. UtxoRpc.maybe'addrKeyHash <|> drep ^. UtxoRpc.maybe'scriptHash

-- | Credentials a certificate is about, classified once per oneof variant.
-- Exhaustive match, no wildcard: a new certificate variant in the proto is a
-- compile error here, not a silently unmatched cert.
data CertificateCredentials = CertificateCredentials
  { certStakeCredentials :: [Proto UtxoRpc.StakeCredential]
  , certPoolKeyHashes :: [ByteString]
  , certDRepBytes :: [ByteString]
  }

certificateCredentials :: Proto UtxoRpc.Certificate -> CertificateCredentials
certificateCredentials cert =
  case cert ^. UtxoRpc.maybe'certificate of
    Nothing -> CertificateCredentials [] [] []
    Just (Proto variant) -> case variant of
      UtxoRpc.Certificate'StakeRegistration cred ->
        CertificateCredentials [Proto cred] [] []
      UtxoRpc.Certificate'StakeDeregistration cred ->
        CertificateCredentials [Proto cred] [] []
      UtxoRpc.Certificate'StakeDelegation delegCert ->
        CertificateCredentials
          [Proto delegCert ^. UtxoRpc.stakeCredential]
          [Proto delegCert ^. UtxoRpc.poolKeyhash]
          []
      -- a 'PoolRegistrationCert' only carries one key hash ('operator'); the
      -- owners are deliberately excluded, matching 'matchesPoolRegistrationPattern'
      UtxoRpc.Certificate'PoolRegistration regCert ->
        CertificateCredentials [] [Proto regCert ^. UtxoRpc.operator] []
      UtxoRpc.Certificate'PoolRetirement retireCert ->
        CertificateCredentials [] [Proto retireCert ^. UtxoRpc.poolKeyhash] []
      -- delegates genesis keys; carries no stake\/pool\/drep credential
      UtxoRpc.Certificate'GenesisKeyDelegation _ ->
        CertificateCredentials [] [] []
      -- targets are reward-adjustment recipients, not a subject credential
      UtxoRpc.Certificate'MirCert _ ->
        CertificateCredentials [] [] []
      UtxoRpc.Certificate'RegCert regCert ->
        CertificateCredentials [Proto regCert ^. UtxoRpc.stakeCredential] [] []
      UtxoRpc.Certificate'UnregCert unregCert ->
        CertificateCredentials [Proto unregCert ^. UtxoRpc.stakeCredential] [] []
      UtxoRpc.Certificate'VoteDelegCert voteDelegCert ->
        CertificateCredentials
          [Proto voteDelegCert ^. UtxoRpc.stakeCredential]
          []
          (maybeToList . voteDrepBytes $ Proto voteDelegCert ^. UtxoRpc.drep)
      UtxoRpc.Certificate'StakeVoteDelegCert stakeVoteDelegCert ->
        CertificateCredentials
          [Proto stakeVoteDelegCert ^. UtxoRpc.stakeCredential]
          [Proto stakeVoteDelegCert ^. UtxoRpc.poolKeyhash]
          (maybeToList . voteDrepBytes $ Proto stakeVoteDelegCert ^. UtxoRpc.drep)
      UtxoRpc.Certificate'StakeRegDelegCert stakeRegDelegCert ->
        CertificateCredentials
          [Proto stakeRegDelegCert ^. UtxoRpc.stakeCredential]
          [Proto stakeRegDelegCert ^. UtxoRpc.poolKeyhash]
          []
      UtxoRpc.Certificate'VoteRegDelegCert voteRegDelegCert ->
        CertificateCredentials
          [Proto voteRegDelegCert ^. UtxoRpc.stakeCredential]
          []
          (maybeToList . voteDrepBytes $ Proto voteRegDelegCert ^. UtxoRpc.drep)
      UtxoRpc.Certificate'StakeVoteRegDelegCert stakeVoteRegDelegCert ->
        CertificateCredentials
          [Proto stakeVoteRegDelegCert ^. UtxoRpc.stakeCredential]
          [Proto stakeVoteRegDelegCert ^. UtxoRpc.poolKeyhash]
          (maybeToList . voteDrepBytes $ Proto stakeVoteRegDelegCert ^. UtxoRpc.drep)
      UtxoRpc.Certificate'AuthCommitteeHotCert authCert ->
        CertificateCredentials
          [ Proto authCert ^. UtxoRpc.committeeColdCredential
          , Proto authCert ^. UtxoRpc.committeeHotCredential
          ]
          []
          []
      UtxoRpc.Certificate'ResignCommitteeColdCert resignCert ->
        CertificateCredentials [Proto resignCert ^. UtxoRpc.committeeColdCredential] [] []
      UtxoRpc.Certificate'RegDrepCert regDrepCert ->
        let drepCredential = Proto regDrepCert ^. UtxoRpc.drepCredential
         in CertificateCredentials [drepCredential] [] [credentialBytes drepCredential]
      UtxoRpc.Certificate'UnregDrepCert unregDrepCert ->
        let drepCredential = Proto unregDrepCert ^. UtxoRpc.drepCredential
         in CertificateCredentials [drepCredential] [] [credentialBytes drepCredential]
      UtxoRpc.Certificate'UpdateDrepCert updateDrepCert ->
        let drepCredential = Proto updateDrepCert ^. UtxoRpc.drepCredential
         in CertificateCredentials [drepCredential] [] [credentialBytes drepCredential]

-- | Every stake-shaped credential embedded in a certificate; see 'certificateCredentials'.
certificateStakeCredentials :: Proto UtxoRpc.Certificate -> [Proto UtxoRpc.StakeCredential]
certificateStakeCredentials = certStakeCredentials . certificateCredentials

matchesAnyStakeCredential :: [Proto UtxoRpc.StakeCredential] -> ByteString -> Bool
matchesAnyStakeCredential stakeCredentials bytes = any ((== bytes) . credentialBytes) stakeCredentials

-- | Every pool key hash embedded in a certificate; see 'certificateCredentials'.
certificatePoolKeyHashes :: Proto UtxoRpc.Certificate -> [ByteString]
certificatePoolKeyHashes = certPoolKeyHashes . certificateCredentials

matchesAnyPoolKeyHash :: [ByteString] -> ByteString -> Bool
matchesAnyPoolKeyHash poolKeyHashes bytes = bytes `elem` poolKeyHashes

-- | Every DRep, identified by key\/script hash, embedded in a certificate; see 'certificateCredentials'.
certificateDRepBytes :: Proto UtxoRpc.Certificate -> [ByteString]
certificateDRepBytes = certDRepBytes . certificateCredentials

matchesAnyDRep :: [ByteString] -> ByteString -> Bool
matchesAnyDRep drepHashes bytes = bytes `elem` drepHashes
