{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Rpc.Server.Internal.UtxoRpc.Certificate
  ( txCertToUtxoRpcCertificate
  , credentialToUtxoRpcStakeCredential
  , drepToUtxoRpcDRep
  , anchorToUtxoRpcAnchor
  )
where

import Cardano.Api.Address
import Cardano.Api.Era
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Serialise.Raw
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as UtxoRpc
import Cardano.Rpc.Server.Internal.Orphans ()

import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Binary qualified as L (ipv4ToBytes, ipv6ToBytes)
import Cardano.Ledger.Coin qualified as L (DeltaCoin (..))
import Cardano.Ledger.Dijkstra.TxCert qualified as L
  ( DijkstraTxCert (..)
  , dijkstraToConwayDelegCert
  )
import Cardano.Ledger.Hashes qualified as L (ScriptHash (..), VRFVerKeyHash (..))

import RIO

import Data.ByteString.Short qualified as SBS
import Data.Map.Strict qualified as M
import Data.MemPack.Buffer (byteArrayToShortByteString)
import Data.ProtoLens (defMessage)
import Network.GRPC.Spec

-- | Convert a ledger transaction certificate to the UTxO RPC 'UtxoRpc.Certificate' message.
-- The @redeemer@ field is left unset: it comes from the witness set, not the certificate.
--
-- The dispatch matches on concrete eras because each group of eras has its own
-- concrete certificate type: 'L.ShelleyTxCert' up to Babbage, 'L.ConwayTxCert' in Conway
-- and 'L.DijkstraTxCert' in Dijkstra. Their payloads are era-independent, so the
-- helpers below need no era constraints.
txCertToUtxoRpcCertificate
  :: ShelleyBasedEra era
  -> L.TxCert (ShelleyLedgerEra era)
  -> UtxoRpc.Certificate
txCertToUtxoRpcCertificate = \case
  ShelleyBasedEraShelley -> shelleyTxCertToUtxoRpcCertificate
  ShelleyBasedEraAllegra -> shelleyTxCertToUtxoRpcCertificate
  ShelleyBasedEraMary -> shelleyTxCertToUtxoRpcCertificate
  ShelleyBasedEraAlonzo -> shelleyTxCertToUtxoRpcCertificate
  ShelleyBasedEraBabbage -> shelleyTxCertToUtxoRpcCertificate
  ShelleyBasedEraConway -> conwayTxCertToUtxoRpcCertificate
  ShelleyBasedEraDijkstra -> dijkstraTxCertToUtxoRpcCertificate

shelleyTxCertToUtxoRpcCertificate :: L.ShelleyTxCert era -> UtxoRpc.Certificate
shelleyTxCertToUtxoRpcCertificate = \case
  L.ShelleyTxCertDelegCert delegCert ->
    shelleyDelegCertToUtxoRpcCertificate delegCert
  L.ShelleyTxCertPool poolCert ->
    poolCertToUtxoRpcCertificate poolCert
  L.ShelleyTxCertGenesisDeleg (L.GenesisDelegCert genesisKeyHash delegateKeyHash vrfKeyHash) ->
    defMessage
      & U5c.genesisKeyDelegation
        .~ ( defMessage
               & U5c.genesisHash .~ keyHashToBytes genesisKeyHash
               & U5c.genesisDelegateHash .~ keyHashToBytes delegateKeyHash
               & U5c.vrfKeyhash .~ L.hashToBytes (L.unVRFVerKeyHash vrfKeyHash)
           )
  L.ShelleyTxCertMir (L.MIRCert pot target) -> do
    let source :: UtxoRpc.MirSource =
          case pot of
            L.ReservesMIR -> UtxoRpc.MIR_SOURCE_RESERVES
            L.TreasuryMIR -> UtxoRpc.MIR_SOURCE_TREASURY
        mir :: UtxoRpc.MirCert =
          case target of
            L.StakeAddressesMIR rewards ->
              defMessage
                & U5c.from .~ source
                & U5c.to
                  .~ ( M.toList rewards <&> \(credential, L.DeltaCoin delta) ->
                         defMessage
                           & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
                           & U5c.deltaCoin .~ getProto (inject delta)
                     )
            L.SendToOppositePotMIR coin ->
              defMessage
                & U5c.from .~ source
                & U5c.otherPot .~ fromIntegral (L.unCoin coin)
    defMessage & U5c.mirCert .~ mir

shelleyDelegCertToUtxoRpcCertificate :: L.ShelleyDelegCert -> UtxoRpc.Certificate
shelleyDelegCertToUtxoRpcCertificate = \case
  L.ShelleyRegCert credential ->
    defMessage & U5c.stakeRegistration .~ credentialToUtxoRpcStakeCredential credential
  L.ShelleyUnRegCert credential ->
    defMessage & U5c.stakeDeregistration .~ credentialToUtxoRpcStakeCredential credential
  L.ShelleyDelegCert credential poolKeyHash ->
    defMessage
      & U5c.stakeDelegation
        .~ ( defMessage
               & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
               & U5c.poolKeyhash .~ keyHashToBytes poolKeyHash
           )

conwayTxCertToUtxoRpcCertificate :: L.ConwayTxCert era -> UtxoRpc.Certificate
conwayTxCertToUtxoRpcCertificate = \case
  L.ConwayTxCertDeleg delegCert -> conwayDelegCertToUtxoRpcCertificate delegCert
  L.ConwayTxCertPool poolCert -> poolCertToUtxoRpcCertificate poolCert
  L.ConwayTxCertGov govCert -> conwayGovCertToUtxoRpcCertificate govCert

-- | Dijkstra shares the pool and governance certificates with Conway. Its delegation
-- certificates differ only in that deposits are mandatory, so they are converted
-- through 'L.dijkstraToConwayDelegCert' (which is lossless).
dijkstraTxCertToUtxoRpcCertificate :: L.DijkstraTxCert era -> UtxoRpc.Certificate
dijkstraTxCertToUtxoRpcCertificate = \case
  L.DijkstraTxCertDeleg delegCert ->
    conwayDelegCertToUtxoRpcCertificate $ L.dijkstraToConwayDelegCert delegCert
  L.DijkstraTxCertPool poolCert -> poolCertToUtxoRpcCertificate poolCert
  L.DijkstraTxCertGov govCert -> conwayGovCertToUtxoRpcCertificate govCert

conwayDelegCertToUtxoRpcCertificate :: L.ConwayDelegCert -> UtxoRpc.Certificate
conwayDelegCertToUtxoRpcCertificate = \case
  -- a registration without an explicit deposit is the legacy (pre-Conway)
  -- stake registration certificate, so it maps to the same proto variant
  L.ConwayRegCert credential L.SNothing ->
    defMessage & U5c.stakeRegistration .~ credentialToUtxoRpcStakeCredential credential
  L.ConwayRegCert credential (L.SJust deposit) ->
    defMessage
      & U5c.regCert
        .~ ( defMessage
               & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
               & U5c.coin .~ getProto (inject deposit)
           )
  L.ConwayUnRegCert credential L.SNothing ->
    defMessage & U5c.stakeDeregistration .~ credentialToUtxoRpcStakeCredential credential
  L.ConwayUnRegCert credential (L.SJust refund) ->
    defMessage
      & U5c.unregCert
        .~ ( defMessage
               & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
               & U5c.coin .~ getProto (inject refund)
           )
  L.ConwayDelegCert credential delegatee ->
    case delegatee of
      L.DelegStake poolKeyHash ->
        defMessage
          & U5c.stakeDelegation
            .~ ( defMessage
                   & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
                   & U5c.poolKeyhash .~ keyHashToBytes poolKeyHash
               )
      L.DelegVote drep ->
        defMessage
          & U5c.voteDelegCert
            .~ ( defMessage
                   & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
                   & U5c.drep .~ drepToUtxoRpcDRep drep
               )
      L.DelegStakeVote poolKeyHash drep ->
        defMessage
          & U5c.stakeVoteDelegCert
            .~ ( defMessage
                   & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
                   & U5c.poolKeyhash .~ keyHashToBytes poolKeyHash
                   & U5c.drep .~ drepToUtxoRpcDRep drep
               )
  L.ConwayRegDelegCert credential delegatee deposit ->
    case delegatee of
      L.DelegStake poolKeyHash ->
        defMessage
          & U5c.stakeRegDelegCert
            .~ ( defMessage
                   & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
                   & U5c.poolKeyhash .~ keyHashToBytes poolKeyHash
                   & U5c.coin .~ getProto (inject deposit)
               )
      L.DelegVote drep ->
        defMessage
          & U5c.voteRegDelegCert
            .~ ( defMessage
                   & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
                   & U5c.drep .~ drepToUtxoRpcDRep drep
                   & U5c.coin .~ getProto (inject deposit)
               )
      L.DelegStakeVote poolKeyHash drep ->
        defMessage
          & U5c.stakeVoteRegDelegCert
            .~ ( defMessage
                   & U5c.stakeCredential .~ credentialToUtxoRpcStakeCredential credential
                   & U5c.poolKeyhash .~ keyHashToBytes poolKeyHash
                   & U5c.drep .~ drepToUtxoRpcDRep drep
                   & U5c.coin .~ getProto (inject deposit)
               )

conwayGovCertToUtxoRpcCertificate :: L.ConwayGovCert -> UtxoRpc.Certificate
conwayGovCertToUtxoRpcCertificate = \case
  L.ConwayRegDRep credential deposit anchor ->
    defMessage
      & U5c.regDrepCert
        .~ ( defMessage
               & U5c.drepCredential .~ credentialToUtxoRpcStakeCredential credential
               & U5c.coin .~ getProto (inject deposit)
               & U5c.maybe'anchor .~ fmap anchorToUtxoRpcAnchor (L.strictMaybeToMaybe anchor)
           )
  L.ConwayUnRegDRep credential refund ->
    defMessage
      & U5c.unregDrepCert
        .~ ( defMessage
               & U5c.drepCredential .~ credentialToUtxoRpcStakeCredential credential
               & U5c.coin .~ getProto (inject refund)
           )
  L.ConwayUpdateDRep credential anchor ->
    defMessage
      & U5c.updateDrepCert
        .~ ( defMessage
               & U5c.drepCredential .~ credentialToUtxoRpcStakeCredential credential
               & U5c.maybe'anchor .~ fmap anchorToUtxoRpcAnchor (L.strictMaybeToMaybe anchor)
           )
  L.ConwayAuthCommitteeHotKey coldCredential hotCredential ->
    defMessage
      & U5c.authCommitteeHotCert
        .~ ( defMessage
               & U5c.committeeColdCredential .~ credentialToUtxoRpcStakeCredential coldCredential
               & U5c.committeeHotCredential .~ credentialToUtxoRpcStakeCredential hotCredential
           )
  L.ConwayResignCommitteeColdKey coldCredential anchor ->
    defMessage
      & U5c.resignCommitteeColdCert
        .~ ( defMessage
               & U5c.committeeColdCredential .~ credentialToUtxoRpcStakeCredential coldCredential
               & U5c.maybe'anchor .~ fmap anchorToUtxoRpcAnchor (L.strictMaybeToMaybe anchor)
           )

poolCertToUtxoRpcCertificate :: L.PoolCert -> UtxoRpc.Certificate
poolCertToUtxoRpcCertificate = \case
  L.RegPool poolParams ->
    defMessage & U5c.poolRegistration .~ stakePoolParamsToUtxoRpcPoolRegistration poolParams
  L.RetirePool poolKeyHash epochNo ->
    defMessage
      & U5c.poolRetirement
        .~ ( defMessage
               & U5c.poolKeyhash .~ keyHashToBytes poolKeyHash
               & U5c.epoch .~ L.unEpochNo epochNo
           )

stakePoolParamsToUtxoRpcPoolRegistration :: L.StakePoolParams -> UtxoRpc.PoolRegistrationCert
stakePoolParamsToUtxoRpcPoolRegistration poolParams =
  defMessage
    & U5c.operator .~ keyHashToBytes (L.sppId poolParams)
    & U5c.vrfKeyhash .~ L.hashToBytes (L.unVRFVerKeyHash (L.sppVrf poolParams))
    & U5c.pledge .~ getProto (inject (L.sppPledge poolParams))
    & U5c.cost .~ getProto (inject (L.sppCost poolParams))
    & U5c.margin .~ getProto (inject (L.unboundRational (L.sppMargin poolParams)))
    & U5c.rewardAccount .~ serialiseToRawBytes (fromShelleyStakeAddr (L.sppAccountAddress poolParams))
    & U5c.poolOwners .~ map keyHashToBytes (toList (L.sppOwners poolParams))
    & U5c.relays .~ map stakePoolRelayToUtxoRpcRelay (toList (L.sppRelays poolParams))
    & U5c.maybe'poolMetadata
      .~ fmap poolMetadataToUtxoRpcPoolMetadata (L.strictMaybeToMaybe (L.sppMetadata poolParams))

stakePoolRelayToUtxoRpcRelay :: L.StakePoolRelay -> UtxoRpc.Relay
stakePoolRelayToUtxoRpcRelay = \case
  L.SingleHostAddr port ipv4 ipv6 ->
    defMessage
      & U5c.ipV4 .~ L.strictMaybe mempty L.ipv4ToBytes ipv4
      & U5c.ipV6 .~ L.strictMaybe mempty L.ipv6ToBytes ipv6
      & U5c.port .~ portToWord32 port
  L.SingleHostName port dnsName ->
    defMessage
      & U5c.dnsName .~ L.dnsToText dnsName
      & U5c.port .~ portToWord32 port
  L.MultiHostName dnsName ->
    defMessage & U5c.dnsName .~ L.dnsToText dnsName
 where
  -- proto3 uint32 defaults to 0, which encodes an absent port
  portToWord32 :: L.StrictMaybe L.Port -> Word32
  portToWord32 = L.strictMaybe 0 (fromIntegral . L.portToWord16)

poolMetadataToUtxoRpcPoolMetadata :: L.PoolMetadata -> UtxoRpc.PoolMetadata
poolMetadataToUtxoRpcPoolMetadata metadata =
  defMessage
    & U5c.url .~ L.urlToText (L.pmUrl metadata)
    & U5c.hash .~ SBS.fromShort (byteArrayToShortByteString (L.pmHash metadata))

-- | Convert a ledger credential of any key role to the UTxO RPC
-- 'UtxoRpc.StakeCredential' message, which carries the bare key or script hash.
credentialToUtxoRpcStakeCredential :: L.Credential kr -> UtxoRpc.StakeCredential
credentialToUtxoRpcStakeCredential = \case
  L.KeyHashObj keyHash ->
    defMessage & U5c.addrKeyHash .~ keyHashToBytes keyHash
  L.ScriptHashObj (L.ScriptHash scriptHash) ->
    defMessage & U5c.scriptHash .~ L.hashToBytes scriptHash

-- | Convert a ledger DRep to the UTxO RPC 'UtxoRpc.DRep' message.
drepToUtxoRpcDRep :: L.DRep -> UtxoRpc.DRep
drepToUtxoRpcDRep = \case
  L.DRepKeyHash keyHash ->
    defMessage & U5c.addrKeyHash .~ keyHashToBytes keyHash
  L.DRepScriptHash (L.ScriptHash scriptHash) ->
    defMessage & U5c.scriptHash .~ L.hashToBytes scriptHash
  L.DRepAlwaysAbstain ->
    defMessage & U5c.abstain .~ True
  L.DRepAlwaysNoConfidence ->
    defMessage & U5c.noConfidence .~ True

-- | Convert a ledger anchor to the UTxO RPC 'UtxoRpc.Anchor' message.
anchorToUtxoRpcAnchor :: L.Anchor -> UtxoRpc.Anchor
anchorToUtxoRpcAnchor anchor =
  defMessage
    & U5c.url .~ L.urlToText (L.anchorUrl anchor)
    & U5c.contentHash .~ L.hashToBytes (L.extractHash (L.anchorDataHash anchor))

keyHashToBytes :: L.KeyHash kr -> ByteString
keyHashToBytes = L.hashToBytes . L.unKeyHash
