-- | This module provides a library interface that is intended to be
-- the complete API for Shelley covering everything, including
-- exposing constructors for the lower level types.
module Cardano.Api.Shelley
  ( module Cardano.Api

    -- * Genesis
  , ShelleyGenesis (..)
  , shelleyGenesisDefaults
  , alonzoGenesisDefaults
  , decodeAlonzoGenesis
  , conwayGenesisDefaults

    -- * Cryptographic key interface
    -- $keys
  , Key (..)
  , VerificationKey (..)
  , SigningKey (..)

    -- * Hashes
  , Hash (..)

    -- * Type Proxies
  , AsType (..)

    -- * Payment addresses

    -- | Constructing and inspecting Shelley payment addresses
  , Address (ShelleyAddress)
  , toShelleyAddr
  , fromShelleyAddr
  , fromShelleyAddrIsSbe
  , fromShelleyAddrToAny
  , toShelleyStakeCredential
  , fromShelleyStakeCredential
  , NetworkId (Mainnet, Testnet)

    -- * Stake addresses
  , PaymentCredential (..)
  , StakeAddress (..)
  , StakeAddressReference (..)
  , StakeCredential (..)
  , toShelleyStakeAddr
  , fromShelleyStakeAddr
  , fromShelleyStakeReference
  , fromShelleyPaymentCredential

    -- * Building transactions

    -- | Constructing and inspecting transactions
  , TxBody (ShelleyTxBody)
  , TxId (TxId)
  , toShelleyTxId
  , fromShelleyTxId
  , getTxIdShelley
  , TxIn (TxIn)
  , toShelleyTxIn
  , fromShelleyTxIn
  , TxOut (TxOut)
  , toShelleyTxOut
  , fromShelleyTxOut
  , TxIx (TxIx)
  , toMaryValue
  , fromMaryValue
  , calcMinimumDeposit

    -- * Arbitrary signing
  , signArbitraryBytesKes

    -- * Signing transactions

    -- | Creating transaction witnesses one by one, or all in one go.
  , Tx (ShelleyTx)

    -- ** Incremental signing and separate witnesses
  , KeyWitness
    ( ShelleyBootstrapWitness
    , ShelleyKeyWitness
    )
  , ShelleyWitnessSigningKey
    ( WitnessPaymentKey
    , WitnessPaymentExtendedKey
    , WitnessStakeKey
    , WitnessStakeExtendedKey
    , WitnessStakePoolKey
    , WitnessGenesisKey
    , WitnessGenesisExtendedKey
    , WitnessGenesisDelegateKey
    , WitnessGenesisDelegateExtendedKey
    )
  , ShelleySigningKey (..)
  , getShelleyKeyWitnessVerificationKey
  , getTxBodyAndWitnesses
  , makeShelleySignature
  , toShelleySigningKey

    -- * Blocks
  , fromConsensusBlock
  , toConsensusBlock
  , fromConsensusTip
  , fromConsensusPointHF
  , toConsensusPointHF

    -- * Transaction metadata

    -- | Embedding additional structured data within transactions.
  , toShelleyMetadata
  , fromShelleyMetadata
  , toShelleyMetadatum
  , fromShelleyMetadatum

    -- * Protocol parameters
  , LedgerProtocolParameters (..)
  , EraBasedProtocolParametersUpdate (..)
  , CommonProtocolParametersUpdate (..)
  , AlonzoOnwardsPParams (..)
  , DeprecatedAfterBabbagePParams (..)
  , DeprecatedAfterMaryPParams (..)
  , ShelleyToAlonzoPParams (..)
  , IntroducedInBabbagePParams (..)
  , IntroducedInConwayPParams (..)
  , createEraBasedProtocolParamUpdate
  , convertToLedgerProtocolParameters
  , ProtocolParameters (..)
  , checkProtocolParameters
  , ProtocolParametersError (..)

    -- * Scripts
  , fromShelleyBasedScript
  , toShelleyScript
  , toShelleyMultiSig
  , fromShelleyMultiSig
  , toAllegraTimelock
  , fromAllegraTimelock
  , toShelleyScriptHash
  , fromShelleyScriptHash
  , PlutusScript (..)
  , PlutusScriptOrReferenceInput (..)
  , SimpleScriptOrReferenceInput (..)
  , toAlonzoLanguage
  , fromAlonzoLanguage
  , toPlutusData
  , fromPlutusData
  , toAlonzoData
  , fromAlonzoData
  , toAlonzoPrices
  , fromAlonzoPrices
  , toAlonzoExUnits
  , fromAlonzoExUnits
  , toScriptIndex
  , scriptDataFromJsonDetailedSchema
  , scriptDataToJsonDetailedSchema
  , calculateExecutionUnitsLovelace

    -- * Reference Scripts
  , ReferenceScript (..)
  , refScriptToShelleyScript

    -- * Certificates
  , Certificate (..)
  , toShelleyCertificate
  , fromShelleyCertificate
  , toShelleyPoolParams

    -- ** Operational certificates
  , OperationalCertificate (OperationalCertificate)
  , OperationalCertificateIssueCounter (..)
  , OperationalCertIssueError (..)

    -- * Stake Pool
  , StakePoolMetadata (StakePoolMetadata)
  , stakePoolName
  , stakePoolDescription
  , stakePoolTicker
  , stakePoolHomepage
  , StakePoolMetadataReference (StakePoolMetadataReference)
  , stakePoolMetadataURL
  , stakePoolMetadataHash
  , StakePoolParameters (StakePoolParameters)
  , stakePoolId
  , stakePoolVRF
  , stakePoolCost
  , stakePoolMargin
  , stakePoolRewardAccount
  , stakePoolPledge
  , stakePoolOwners
  , stakePoolRelays
  , stakePoolMetadata
  , StakePoolRelay
    ( StakePoolRelayIp
    , StakePoolRelayDnsARecord
    , StakePoolRelayDnsSrvRecord
    )
  , EpochNo (..)

    -- * Governance Actions
  , createAnchor
  , createPreviousGovernanceActionId
  , createGovernanceActionId

    -- * DRep
  , DRepMetadata (DRepMetadata)
  , DRepMetadataReference (DRepMetadataReference)

    -- ** Stake pool operator's keys
  , StakePoolKey
  , PoolId

    -- ** KES keys
  , KesKey
  , KESPeriod (..)

    -- ** VRF keys
  , VrfKey

    -- ** Low level protocol interaction with a Cardano node
  , LocalNodeConnectInfo (LocalNodeConnectInfo)
  , LocalNodeClientProtocols (LocalNodeClientProtocols)

    -- ** Shelley based eras
  , ShelleyLedgerEra

    -- *** Ledger Events
  , LedgerEvent (..)
  , AnyProposals (..)
  , AnyRatificationState (..)
  , MIRDistributionDetails (..)
  , PoolReapDetails (..)
  , toLedgerEvent

    -- ** Local State Query
  , DebugLedgerState (..)
  , decodeDebugLedgerState
  , ProtocolState (..)
  , decodeProtocolState
  , SerialisedDebugLedgerState (..)
  , CurrentEpochState (..)
  , SerialisedCurrentEpochState (..)
  , decodeCurrentEpochState
  , PoolState (..)
  , SerialisedPoolState (..)
  , decodePoolState
  , PoolDistribution (..)
  , SerialisedPoolDistribution (..)
  , decodePoolDistribution
  , StakeSnapshot (..)
  , SerialisedStakeSnapshots (..)
  , decodeStakeSnapshot
  , decodeBigLedgerPeerSnapshot
  , UTxO (..)
  , AcquiringFailure (..)
  , SystemStart (..)

    -- ** Governance
  , GovernanceAction (..)
  , GovernanceActionId (..)
  , Proposal (..)
  , VotingProcedure (..)
  , VotingProcedures (..)
  , GovernancePoll (..)
  , GovernancePollAnswer (..)
  , GovernancePollError (..)
  , Vote (..)
  , Voter (..)
  , createProposalProcedure
  , createVotingProcedure
  , renderGovernancePollError
  , fromProposalProcedure
  , hashGovernancePoll
  , verifyPollAnswer

    -- ** Various calculations
  , LeadershipError (..)
  , currentEpochEligibleLeadershipSlots
  , evaluateTransactionExecutionUnitsShelley
  , nextEpochEligibleLeadershipSlots

    -- ** Conversions
  , shelleyPayAddrToPlutusPubKHash
  , toConsensusGenTx
  , fromAlonzoCostModels
  -- TODO: arrange not to export these
  , toLedgerNonce
  , toShelleyNetwork
  , fromShelleyPoolParams
  , fromLedgerPParamsUpdate
  , emptyVotingProcedures
  , mergeVotingProcedures
  , singletonVotingProcedures
  , VotesMergingConflict (..)
  )
where

import           Cardano.Api
import           Cardano.Api.Internal.Address
import           Cardano.Api.Internal.Block
import           Cardano.Api.Internal.Certificate
import           Cardano.Api.Internal.DRepMetadata
import           Cardano.Api.Internal.Eon.ShelleyBasedEra
import           Cardano.Api.Internal.Fees
import           Cardano.Api.Internal.Genesis
import           Cardano.Api.Internal.Governance.Actions.ProposalProcedure
import           Cardano.Api.Internal.Governance.Actions.VotingProcedure
import           Cardano.Api.Internal.Governance.Poll
import           Cardano.Api.Internal.InMode
import           Cardano.Api.Internal.IPC
import           Cardano.Api.Internal.Keys.Praos
import           Cardano.Api.Internal.Keys.Shelley
import           Cardano.Api.Internal.LedgerEvents.ConvertLedgerEvent
import           Cardano.Api.Internal.LedgerEvents.LedgerEvent
import           Cardano.Api.Internal.LedgerState
import           Cardano.Api.Internal.NetworkId
import           Cardano.Api.Internal.OperationalCertificate
import           Cardano.Api.Internal.ProtocolParameters
import           Cardano.Api.Internal.Query
import           Cardano.Api.Internal.Script
import           Cardano.Api.Internal.ScriptData
import           Cardano.Api.Internal.StakePoolMetadata
import           Cardano.Api.Internal.Tx.Body
import           Cardano.Api.Internal.Tx.Sign
import           Cardano.Api.Internal.TxMetadata
import           Cardano.Api.Internal.Value
