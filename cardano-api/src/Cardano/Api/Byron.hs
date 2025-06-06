-- | This module provides a library interface that is intended to be
-- the complete API for Byron covering everything, including exposing
-- constructors for the lower level types.
module Cardano.Api.Byron
  ( module Api
  , AsType (..)

    -- * Cryptographic key interface
    -- $keys
  , VerificationKey (..)
  , SigningKey (..)
  , SomeByronSigningKey (..)
  , ByronKey
  , ByronKeyLegacy

    -- * Hashes
  , Hash (..)

    -- * Network identifier
  , NetworkId (Mainnet, Testnet)

    -- * Signing transactions

    -- ** Incremental signing and separate witnesses
  , KeyWitness (ByronKeyWitness)
  , WitnessNetworkIdOrByronAddress
    ( WitnessNetworkId
    , WitnessByronAddress
    )

    -- * Errors
  , Api.Error (..)
  , Api.FileError (..)

    -- ** Low level protocol interaction with a Cardano node
  , Api.LocalNodeConnectInfo (Api.LocalNodeConnectInfo)
  , Api.LocalNodeClientProtocols (Api.LocalNodeClientProtocols)

    -- *** Chain sync protocol
  , Api.ChainSyncClient (..)

    -- *** Local tx submission
  , Api.LocalTxSubmissionClient (Api.LocalTxSubmissionClient)

    -- *** Local state query
  , Api.LocalStateQueryClient (..)

    -- * Update Proposal
  , ByronUpdateProposal (..)
  , ByronProtocolParametersUpdate (..)
  , makeByronUpdateProposal
  , toByronLedgerUpdateProposal
  , makeProtocolParametersUpdate

    -- * Vote
  , ByronVote (..)
  , makeByronVote
  , toByronLedgertoByronVote

    -- * Hardcoded configuration parameters
  , applicationName
  , applicationVersion
  , softwareVersion

    -- * Byron ledger re-exports

    -- ** Address components
  , AddrAttributes (..)
  , Address
  , KeyHash
  , addressDetailedF
  , addressF
  , addressHash
  , checkVerKeyAddress
  , decodeAddressBase58
  , mkAttributes

    -- ** Lovelace handling
  , Lovelace
  , LovelacePortion
  , lovelacePortionToRational
  , mkKnownLovelace
  , rationalToLovelacePortion

    -- ** Genesis configuration and AVVM
  , Config (..)
  , FakeAvvmOptions (..)
  , GeneratedSecrets (..)
  , GenesisAvvmBalances (..)
  , GenesisData (..)
  , GenesisDataError (..)
  , GenesisDataGenerationError (..)
  , GenesisDelegation (..)
  , GenesisDelegationError
  , GenesisHash (..)
  , GenesisInitializer (..)
  , GenesisSpec (..)
  , NetworkMagic (..)
  , PoorSecret (..)
  , TestnetBalanceOptions (..)
  , TxFeePolicy (..)
  , TxSizeLinear (..)
  , generateGenesisData
  , mkGenesisDelegation
  , mkGenesisSpec
  , readGenesisData

    -- ** Updates
  , ApplicationName (..)
  , InstallerHash (..)
  , NumSoftwareVersion
  , Proposal
  , ProtocolParameters (..)
  , ProtocolVersion (..)
  , SoftforkRule (..)
  , SoftwareVersion (..)
  , SystemTag (..)
  , Vote
  , checkApplicationName
  , checkSystemTag

    -- ** Blocks, slots, and epochs
  , BlockCount (..)
  , EpochNumber (..)
  , SlotNumber (..)
  , decCBORABlockOrBoundary

    -- ** UTxO components
  , ATxAux (..)
  , CompactTxIn
  , CompactTxOut
  , Tx (..)
  , TxIn (..)
  , TxOut (..)
  , UTxO (..)
  , defaultUTxOConfiguration
  , fromCompactTxIn
  , fromCompactTxOut
  , genesisUtxo

    -- ** Delegation
  , ACertificate (..)
  , Certificate
  , isValid
  , signCertificate
  )
where

import Cardano.Api qualified as Api
import Cardano.Api.Byron.Internal.Key
import Cardano.Api.Byron.Internal.Proposal
import Cardano.Api.Network.Internal.NetworkId hiding (NetworkMagic)
import Cardano.Api.Serialise.TextEnvelope.Internal.Cddl
import Cardano.Api.Tx.Internal.Body hiding (TxIn, TxOut)
import Cardano.Api.Tx.Internal.Sign hiding (ATxAux (..), Tx (..))
import Cardano.Api.Value.Internal hiding (Lovelace)

import Cardano.Chain.Block (decCBORABlockOrBoundary)
import Cardano.Chain.Common
  ( AddrAttributes (..)
  , Address
  , BlockCount (..)
  , KeyHash
  , Lovelace
  , LovelacePortion
  , NetworkMagic (..)
  , TxFeePolicy (..)
  , TxSizeLinear (..)
  , addressDetailedF
  , addressF
  , addressHash
  , checkVerKeyAddress
  , decodeAddressBase58
  , lovelacePortionToRational
  , mkAttributes
  , mkKnownLovelace
  , rationalToLovelacePortion
  )
import Cardano.Chain.Delegation (ACertificate (..), Certificate, isValid, signCertificate)
import Cardano.Chain.Genesis
  ( Config (..)
  , FakeAvvmOptions (..)
  , GeneratedSecrets (..)
  , GenesisAvvmBalances (..)
  , GenesisData (..)
  , GenesisDataError (..)
  , GenesisDataGenerationError (..)
  , GenesisDelegation (..)
  , GenesisDelegationError
  , GenesisHash (..)
  , GenesisInitializer (..)
  , GenesisSpec (..)
  , PoorSecret (..)
  , TestnetBalanceOptions (..)
  , generateGenesisData
  , mkGenesisDelegation
  , mkGenesisSpec
  , readGenesisData
  )
import Cardano.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import Cardano.Chain.UTxO
  ( ATxAux (..)
  , CompactTxIn
  , CompactTxOut
  , Tx (..)
  , TxIn (..)
  , TxOut (..)
  , UTxO (..)
  , defaultUTxOConfiguration
  , fromCompactTxIn
  , fromCompactTxOut
  , genesisUtxo
  )
import Cardano.Chain.Update
  ( ApplicationName (..)
  , InstallerHash (..)
  , NumSoftwareVersion
  , Proposal
  , ProtocolParameters (..)
  , ProtocolVersion (..)
  , SoftforkRule (..)
  , SoftwareVersion (..)
  , SystemTag (..)
  , Vote
  , checkApplicationName
  , checkSystemTag
  )
