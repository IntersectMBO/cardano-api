{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Conversion of the network's per-era genesis configuration to the UTxO RPC
-- 'U5c.Genesis' message.
--
-- The @Genesis@ message has one field per Byron, Shelley, Alonzo and Conway
-- genesis parameter. Each era is mapped by an updater over one shared
-- accumulator ('byronGenesisToProto', 'shelleyGenesisToProto',
-- 'alonzoGenesisToProto', 'conwayGenesisToProto'); 'genesisBundleToProto'
-- threads a single 'defMessage' through all four. The shared accumulator lets
-- Alonzo and Conway both contribute to the single @cost_models@ field
-- (PlutusV1 from Alonzo, PlutusV3 from Conway) without any message merge.
module Cardano.Rpc.Server.Internal.UtxoRpc.Type.Genesis
  ( genesisBundleToProto

    -- * Per-era mappers

    -- | Exported for differential fixture testing against real genesis JSON
    -- (see @Test.Cardano.Rpc.Genesis.Fixture@); not part of the public API.
  , byronGenesisToProto
  , shelleyGenesisToProto
  , alonzoGenesisToProto
  , conwayGenesisToProto
  )
where

import Cardano.Api.Era (Inject (..))
import Cardano.Api.Ledger qualified as L
import Cardano.Rpc.Proto.Api.UtxoRpc.Query qualified as U5c
import Cardano.Rpc.Server.Internal.Orphans ()
import Cardano.Rpc.Server.Internal.UtxoRpc.Type.Certificate
  ( constitutionToUtxoRpcConstitution
  , keyHashToBytes
  , scriptHashToBytes
  )
import Cardano.Rpc.Server.NodeKernelAccess.Type (GenesisBundle (..))

import Cardano.Chain.Common qualified as Byron
  ( KeyHash
  , LovelacePortion
  , TxFeePolicy (..)
  , TxSizeLinear (..)
  , addressF
  , lovelacePortionToRational
  , lovelaceToInteger
  , unBlockCount
  , unKeyHash
  )
import Cardano.Chain.Delegation qualified as Byron
  ( Certificate
  , delegateVK
  , epoch
  , issuerVK
  , signature
  )
import Cardano.Chain.Genesis qualified as Byron
  ( GenesisAvvmBalances (..)
  , GenesisData (..)
  , GenesisDelegation (..)
  , GenesisKeyHashes (..)
  , GenesisNonAvvmBalances (..)
  , configGenesisData
  )
import Cardano.Chain.Slotting qualified as Byron (getEpochNumber, unSlotNumber)
import Cardano.Chain.Update qualified as Byron (ProtocolParameters (..), SoftforkRule (..))
import Cardano.Crypto qualified as Byron
  ( fromCompactRedeemVerificationKey
  , fullSignatureHexF
  , fullVerificationKeyF
  , hashHexF
  , redeemVKB64UrlF
  , unProtocolMagicId
  )
import Cardano.Ledger.Address qualified as L
import Cardano.Ledger.Alonzo.Genesis qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Hashes qualified as L
import Cardano.Ledger.Shelley.Genesis qualified as L

import RIO

import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as Text (pack)
import Data.Text.Encoding qualified as Text (decodeUtf8)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Formatting (sformat)
import GHC.Exts qualified as Exts (toList)
import Lens.Micro ((?~))
import Network.GRPC.Spec

-- | Convert the network's genesis bundle to the UTxO RPC 'U5c.Genesis'
-- message, populating the Byron, Shelley, Alonzo and Conway fields.
genesisBundleToProto :: GenesisBundle -> Proto U5c.Genesis
genesisBundleToProto GenesisBundle{byronConfig, transitionConfig} =
  byronGenesisToProto byronGenesis
    . shelleyGenesisToProto shelleyGenesis
    . alonzoGenesisToProto alonzoGenesis
    . conwayGenesisToProto conwayGenesis
    $ defMessage
 where
  byronGenesis = Byron.configGenesisData byronConfig
  shelleyGenesis = transitionConfig ^. L.tcShelleyGenesisL
  -- LatestKnownEra is Dijkstra; its previous era is Conway, whose translation
  -- context is the Conway genesis.
  conwayGenesis = transitionConfig ^. L.tcPreviousEraConfigL . L.tcTranslationContextL
  -- Dijkstra -> Conway -> Babbage -> Alonzo config, whose translation context
  -- is the Alonzo genesis.
  alonzoGenesis =
    transitionConfig
      ^. L.tcPreviousEraConfigL
        . L.tcPreviousEraConfigL
        . L.tcPreviousEraConfigL
        . L.tcTranslationContextL

--------------------------------------------------------------------------------
-- Byron
--------------------------------------------------------------------------------

byronGenesisToProto :: Byron.GenesisData -> Proto U5c.Genesis -> Proto U5c.Genesis
byronGenesisToProto genesisData message =
  message
    & U5c.avvmDistr .~ avvmDistr
    & U5c.blockVersionData .~ blockVersionData (Byron.gdProtocolParameters genesisData)
    & U5c.protocolConsts .~ protocolConsts
    & U5c.startTime .~ startTime
    & U5c.bootStakeholders .~ bootStakeholders
    & U5c.heavyDelegation .~ heavyDelegation
    & U5c.nonAvvmBalances .~ nonAvvmBalances
 where
  -- Unix seconds, not milliseconds.
  startTime :: Word64
  startTime = round (utcTimeToPOSIXSeconds (Byron.gdStartTime genesisData))

  protocolConsts :: Proto U5c.ProtocolConsts
  protocolConsts =
    defMessage
      & U5c.k .~ fromIntegral (Byron.unBlockCount (Byron.gdK genesisData))
      & U5c.protocolMagic .~ Byron.unProtocolMagicId (Byron.gdProtocolMagicId genesisData)

  -- vssMaxTtl and vssMinTtl have no Byron ledger source and stay at their
  -- proto default of 0.

  -- Byron stores only a set of genesis key hashes; the genesis JSON synthesises
  -- weight 1 for each, which is matched here.
  bootStakeholders :: Map Text Word64
  bootStakeholders =
    Map.fromList
      [ (byronKeyHashHex keyHash, 1)
      | keyHash <- toList (Byron.unGenesisKeyHashes (Byron.gdGenesisKeyHashes genesisData))
      ]

  heavyDelegation :: Map Text (Proto U5c.HeavyDelegation)
  heavyDelegation =
    Map.fromList
      [ (byronKeyHashHex keyHash, heavyDelegationCert cert)
      | (keyHash, cert) <-
          Map.toList (Byron.unGenesisDelegation (Byron.gdHeavyDelegation genesisData))
      ]

  nonAvvmBalances :: Map Text Text
  nonAvvmBalances =
    Map.fromList
      [ (sformat Byron.addressF address, tshow (Byron.lovelaceToInteger lovelace))
      | (address, lovelace) <-
          Map.toList (Byron.unGenesisNonAvvmBalances (Byron.gdNonAvvmBalances genesisData))
      ]

  avvmDistr :: Map Text Text
  avvmDistr =
    Map.fromList
      [ ( sformat Byron.redeemVKB64UrlF (Byron.fromCompactRedeemVerificationKey redeemKey)
        , tshow (Byron.lovelaceToInteger lovelace)
        )
      | (redeemKey, lovelace) <-
          Map.toList (Byron.unGenesisAvvmBalances (Byron.gdAvvmDistr genesisData))
      ]

-- | Byron key hashes render as lowercase base16, matching the genesis JSON.
byronKeyHashHex :: Byron.KeyHash -> Text
byronKeyHashHex = sformat Byron.hashHexF . Byron.unKeyHash

heavyDelegationCert :: Byron.Certificate -> Proto U5c.HeavyDelegation
heavyDelegationCert cert =
  defMessage
    & U5c.cert .~ sformat Byron.fullSignatureHexF (Byron.signature cert)
    & U5c.delegatePk .~ sformat Byron.fullVerificationKeyF (Byron.delegateVK cert)
    & U5c.issuerPk .~ sformat Byron.fullVerificationKeyF (Byron.issuerVK cert)
    & U5c.omega .~ fromIntegral (Byron.getEpochNumber (Byron.epoch cert))

blockVersionData :: Byron.ProtocolParameters -> Proto U5c.BlockVersionData
blockVersionData pp =
  defMessage
    & U5c.scriptVersion .~ fromIntegral (Byron.ppScriptVersion pp)
    & U5c.slotDuration .~ tshow (Byron.ppSlotDuration pp)
    & U5c.maxBlockSize .~ tshow (Byron.ppMaxBlockSize pp)
    & U5c.maxHeaderSize .~ tshow (Byron.ppMaxHeaderSize pp)
    & U5c.maxTxSize .~ tshow (Byron.ppMaxTxSize pp)
    & U5c.maxProposalSize .~ tshow (Byron.ppMaxProposalSize pp)
    & U5c.mpcThd .~ tshow (lovelacePortionWord (Byron.ppMpcThd pp))
    & U5c.heavyDelThd .~ tshow (lovelacePortionWord (Byron.ppHeavyDelThd pp))
    & U5c.updateVoteThd .~ tshow (lovelacePortionWord (Byron.ppUpdateVoteThd pp))
    & U5c.updateProposalThd .~ tshow (lovelacePortionWord (Byron.ppUpdateProposalThd pp))
    & U5c.updateImplicit .~ tshow (Byron.unSlotNumber (Byron.ppUpdateProposalTTL pp))
    & U5c.unlockStakeEpoch .~ tshow (Byron.getEpochNumber (Byron.ppUnlockStakeEpoch pp))
    & U5c.softforkRule .~ softforkRule (Byron.ppSoftforkRule pp)
    & U5c.txFeePolicy .~ txFeePolicy (Byron.ppTxFeePolicy pp)

softforkRule :: Byron.SoftforkRule -> Proto U5c.SoftforkRule
softforkRule rule =
  defMessage
    & U5c.initThd .~ tshow (lovelacePortionWord (Byron.srInitThd rule))
    & U5c.minThd .~ tshow (lovelacePortionWord (Byron.srMinThd rule))
    & U5c.thdDecrement .~ tshow (lovelacePortionWord (Byron.srThdDecrement rule))

-- | The raw Word64 numerator the Byron genesis JSON emits for a
-- 'Byron.LovelacePortion' (over a fixed 1e15 denominator). The @unLovelacePortion@
-- accessor is not exported, so the value is recovered from
-- 'Byron.lovelacePortionToRational'.
lovelacePortionWord :: Byron.LovelacePortion -> Word64
lovelacePortionWord = round . (* 1_000_000_000_000_000) . Byron.lovelacePortionToRational

txFeePolicy :: Byron.TxFeePolicy -> Proto U5c.TxFeePolicy
txFeePolicy = \case
  Byron.TxFeePolicyTxSizeLinear (Byron.TxSizeLinear constant multiplier) ->
    defMessage
      -- The Byron genesis JSON scales both coefficients by 1e9.
      & U5c.summand .~ tshow (1_000_000_000 * Byron.lovelaceToInteger constant)
      & U5c.multiplier .~ tshow (floor (1_000_000_000 * multiplier) :: Integer)

--------------------------------------------------------------------------------
-- Shelley
--------------------------------------------------------------------------------

shelleyGenesisToProto :: L.ShelleyGenesis -> Proto U5c.Genesis -> Proto U5c.Genesis
shelleyGenesisToProto genesis message =
  message
    & U5c.activeSlotsCoeff .~ inject (L.unboundRational (L.sgActiveSlotsCoeff genesis))
    & U5c.epochLength .~ fromIntegral (L.unEpochSize (L.sgEpochLength genesis))
    & U5c.maxKesEvolutions .~ fromIntegral (L.sgMaxKESEvolutions genesis)
    & U5c.slotsPerKesPeriod .~ fromIntegral (L.sgSlotsPerKESPeriod genesis)
    & U5c.updateQuorum .~ fromIntegral (L.sgUpdateQuorum genesis)
    & U5c.securityParam .~ fromIntegral (L.unNonZero (L.sgSecurityParam genesis))
    & U5c.maxLovelaceSupply .~ inject (fromIntegral (L.sgMaxLovelaceSupply genesis) :: Integer)
    & U5c.networkMagic .~ L.sgNetworkMagic genesis
    & U5c.networkId .~ networkIdText (L.sgNetworkId genesis)
    -- Slot length in milliseconds; mainnet's 1 second yields 1000.
    & U5c.slotLength .~ round (1000 * L.fromNominalDiffTimeMicro (L.sgSlotLength genesis))
    & U5c.systemStart .~ Text.pack (iso8601Show (L.sgSystemStart genesis))
    & U5c.genDelegs .~ genDelegs
    & U5c.initialFunds .~ initialFunds
    & U5c.protocolParams .~ protocolParams
 where
  networkIdText :: L.Network -> Text
  networkIdText = \case
    L.Mainnet -> "Mainnet"
    L.Testnet -> "Testnet"

  genDelegs :: Map Text (Proto U5c.GenDelegs)
  genDelegs =
    Map.fromList
      [ ( hexText (keyHashToBytes keyHash)
        , defMessage
            & U5c.delegate .~ hexText (keyHashToBytes (L.genDelegKeyHash pair))
            & U5c.vrf .~ hexText (L.hashToBytes (L.unVRFVerKeyHash (L.genDelegVrfHash pair)))
        )
      | (keyHash, pair) <- Map.toList (L.sgGenDelegs genesis)
      ]

  initialFunds :: Map Text (Proto U5c.BigInt)
  initialFunds =
    Map.fromList
      [ (hexText (L.serialiseAddr address), inject coin)
      | (address, coin) <- Exts.toList (L.sgInitialFunds genesis)
      ]

  -- Built manually: 'protocolParamsToUtxoRpcPParams' requires 'ConwayEraPParams'.
  -- The Shelley genesis holds a 'PParams ShelleyEra'; the era-generic
  -- 'EraPParams' lenses read every field present in the proto message.
  -- sppExtraEntropy, sppD and sppMinUTxOValue have no proto counterpart and are
  -- dropped.
  protocolParams :: Proto U5c.PParams
  protocolParams =
    defMessage
      & U5c.minFeeCoefficient .~ inject (L.fromCompact (L.unCoinPerByte (pp ^. L.ppTxFeePerByteL)))
      & U5c.minFeeConstant .~ inject (pp ^. L.ppTxFeeFixedL)
      & U5c.maxBlockBodySize .~ fromIntegral (pp ^. L.ppMaxBBSizeL)
      & U5c.maxTxSize .~ fromIntegral (pp ^. L.ppMaxTxSizeL)
      & U5c.maxBlockHeaderSize .~ fromIntegral (pp ^. L.ppMaxBHSizeL)
      & U5c.stakeKeyDeposit .~ inject (pp ^. L.ppKeyDepositL)
      & U5c.poolDeposit .~ inject (pp ^. L.ppPoolDepositL)
      & U5c.poolRetirementEpochBound .~ fromIntegral (L.unEpochInterval (pp ^. L.ppEMaxL))
      & U5c.desiredNumberOfPools .~ fromIntegral (pp ^. L.ppNOptL)
      & U5c.poolInfluence .~ inject (L.unboundRational (pp ^. L.ppA0L))
      & U5c.monetaryExpansion .~ inject (L.unboundRational (pp ^. L.ppRhoL))
      & U5c.treasuryExpansion .~ inject (L.unboundRational (pp ^. L.ppTauL))
      & U5c.minPoolCost .~ inject (pp ^. L.ppMinPoolCostL)
      & U5c.protocolVersion .~ inject (pp ^. L.ppProtocolVersionL)
   where
    pp = L.sgProtocolParams genesis

--------------------------------------------------------------------------------
-- Alonzo
--------------------------------------------------------------------------------

alonzoGenesisToProto :: L.AlonzoGenesis -> Proto U5c.Genesis -> Proto U5c.Genesis
alonzoGenesisToProto genesis message =
  message
    & U5c.lovelacePerUtxoWord .~ inject (L.unCoinPerWord (L.agCoinsPerUTxOWord genesis))
    & U5c.executionPrices
      .~ ( defMessage
             & U5c.steps .~ inject (L.unboundRational (L.prSteps prices))
             & U5c.memory .~ inject (L.unboundRational (L.prMem prices))
         )
    & U5c.maxTxExUnits .~ inject (L.agMaxTxExUnits genesis)
    & U5c.maxBlockExUnits .~ inject (L.agMaxBlockExUnits genesis)
    & U5c.maxValueSize .~ L.agMaxValSize genesis
    & U5c.collateralPercentage .~ fromIntegral (L.agCollateralPercentage genesis)
    & U5c.maxCollateralInputs .~ fromIntegral (L.agMaxCollateralInputs genesis)
    -- Only PlutusV1 comes from the Alonzo genesis; PlutusV3 is set by Conway on
    -- the shared accumulator, PlutusV2 and PlutusV4 never appear in genesis.
    & U5c.costModels . U5c.maybe'plutusV1
      ?~ (defMessage & U5c.values .~ L.getCostModelParams (L.agPlutusV1CostModel genesis))
 where
  prices = L.agPrices genesis

--------------------------------------------------------------------------------
-- Conway
--------------------------------------------------------------------------------

conwayGenesisToProto :: L.ConwayGenesis -> Proto U5c.Genesis -> Proto U5c.Genesis
conwayGenesisToProto genesis message =
  message
    & U5c.committeeMinSize .~ fromIntegral (L.ucppCommitteeMinSize upgrade)
    & U5c.committeeMaxTermLength
      .~ fromIntegral (L.unEpochInterval (L.ucppCommitteeMaxTermLength upgrade))
    & U5c.govActionLifetime .~ fromIntegral (L.unEpochInterval (L.ucppGovActionLifetime upgrade))
    & U5c.drepActivity .~ fromIntegral (L.unEpochInterval (L.ucppDRepActivity upgrade))
    & U5c.govActionDeposit .~ inject (L.ucppGovActionDeposit upgrade)
    & U5c.drepDeposit .~ inject (L.ucppDRepDeposit upgrade)
    & U5c.minFeeRefScriptCostPerByte
      .~ inject (L.unboundRational (L.ucppMinFeeRefScriptCostPerByte upgrade))
    & U5c.poolVotingThresholds .~ poolVotingThresholds (L.ucppPoolVotingThresholds upgrade)
    & U5c.drepVotingThresholds .~ drepVotingThresholds (L.ucppDRepVotingThresholds upgrade)
    & U5c.constitution .~ constitutionToUtxoRpcConstitution (L.cgConstitution genesis)
    & U5c.committee .~ committee
    & U5c.costModels . U5c.maybe'plutusV3
      ?~ (defMessage & U5c.values .~ L.getCostModelParams (L.ucppPlutusV3CostModel upgrade))
 where
  upgrade = L.cgUpgradePParams genesis

  poolVotingThresholds :: L.PoolVotingThresholds -> Proto U5c.PoolVotingThresholds
  poolVotingThresholds thresholds =
    defMessage
      & U5c.motionNoConfidence .~ inject (L.unboundRational (thresholds ^. L.pvtMotionNoConfidenceL))
      & U5c.committeeNormal .~ inject (L.unboundRational (thresholds ^. L.pvtCommitteeNormalL))
      & U5c.committeeNoConfidence
        .~ inject (L.unboundRational (thresholds ^. L.pvtCommitteeNoConfidenceL))
      & U5c.hardForkInitiation .~ inject (L.unboundRational (thresholds ^. L.pvtHardForkInitiationL))
      & U5c.ppSecurityGroup .~ inject (L.unboundRational (thresholds ^. L.pvtPPSecurityGroupL))

  drepVotingThresholds :: L.DRepVotingThresholds -> Proto U5c.DRepVotingThresholds
  drepVotingThresholds thresholds =
    defMessage
      & U5c.motionNoConfidence .~ inject (L.unboundRational (thresholds ^. L.dvtMotionNoConfidenceL))
      & U5c.committeeNormal .~ inject (L.unboundRational (thresholds ^. L.dvtCommitteeNormalL))
      & U5c.committeeNoConfidence
        .~ inject (L.unboundRational (thresholds ^. L.dvtCommitteeNoConfidenceL))
      & U5c.updateToConstitution
        .~ inject (L.unboundRational (thresholds ^. L.dvtUpdateToConstitutionL))
      & U5c.hardForkInitiation .~ inject (L.unboundRational (thresholds ^. L.dvtHardForkInitiationL))
      & U5c.ppNetworkGroup .~ inject (L.unboundRational (thresholds ^. L.dvtPPNetworkGroupL))
      & U5c.ppEconomicGroup .~ inject (L.unboundRational (thresholds ^. L.dvtPPEconomicGroupL))
      & U5c.ppTechnicalGroup .~ inject (L.unboundRational (thresholds ^. L.dvtPPTechnicalGroupL))
      & U5c.ppGovGroup .~ inject (L.unboundRational (thresholds ^. L.dvtPPGovGroupL))
      & U5c.treasuryWithdrawal .~ inject (L.unboundRational (thresholds ^. L.dvtTreasuryWithdrawalL))

  committee :: Proto U5c.Committee
  committee =
    defMessage
      & U5c.threshold .~ inject (L.unboundRational (L.committeeThreshold c))
      & U5c.members
        .~ Map.fromList
          -- The proto committee member key does not distinguish a key hash from
          -- a script hash; both render as bare hex.
          [ (credentialHexText credential, fromIntegral (L.unEpochNo epochNo))
          | (credential, epochNo) <- Map.toList (L.committeeMembers c)
          ]
   where
    c = L.cgCommittee genesis

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Lowercase base16 rendering of raw bytes, matching genesis JSON hash keys.
hexText :: ByteString -> Text
hexText = Text.decodeUtf8 . Base16.encode

credentialHexText :: L.Credential kr -> Text
credentialHexText = \case
  L.KeyHashObj keyHash -> hexText (keyHashToBytes keyHash)
  L.ScriptHashObj scriptHash -> hexText (scriptHashToBytes scriptHash)
