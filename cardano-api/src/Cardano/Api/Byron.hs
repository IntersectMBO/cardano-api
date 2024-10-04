-- | This module provides a library interface that is intended to be
-- the complete API for Byron covering everything, including exposing
-- constructors for the lower level types.
module Cardano.Api.Byron
  ( module Cardano.Api
  , AsType (..)

    -- * Cryptographic key interface
    -- $keys
  , VerificationKey (..)
  , SigningKey (..)
  , SomeByronSigningKey (..)

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
  , Error (..)
  , FileError (..)

    -- ** Low level protocol interaction with a Cardano node
  , LocalNodeConnectInfo (LocalNodeConnectInfo)
  , LocalNodeClientProtocols (LocalNodeClientProtocols)

    -- *** Chain sync protocol
  , ChainSyncClient (..)

    -- *** Local tx submission
  , LocalTxSubmissionClient (LocalTxSubmissionClient)

    -- *** Local state query
  , LocalStateQueryClient (..)

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

    -- ** Conversions
  , fromByronTxIn
  , toByronLovelace
  , toByronNetworkMagic
  , toByronProtocolMagicId
  , toByronRequiresNetworkMagic

    -- * Hardcoded configuration parameters
  , applicationName
  , applicationVersion
  , softwareVersion

    -- * Serialization
  , serializeByronTx
  , writeByronTxFileTextEnvelopeCddl

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

import           Cardano.Api hiding (Address, Certificate, Lovelace, NetworkMagic, TxIn, TxOut,
                   UTxO (..))
import           Cardano.Api.Keys.Byron
import           Cardano.Api.NetworkId hiding (NetworkMagic)
import           Cardano.Api.SerialiseLedgerCddl
import           Cardano.Api.SpecialByron
import           Cardano.Api.Tx.Body hiding (TxIn, TxOut)
import           Cardano.Api.Tx.Sign hiding (ATxAux (..))
import           Cardano.Api.Value hiding (Lovelace)

import           Cardano.Chain.Block (decCBORABlockOrBoundary)
import           Cardano.Chain.Common (AddrAttributes (..), Address, BlockCount (..), KeyHash,
                   Lovelace, LovelacePortion, NetworkMagic (..), TxFeePolicy (..),
                   TxSizeLinear (..), addressDetailedF, addressF, addressHash, checkVerKeyAddress,
                   decodeAddressBase58, lovelacePortionToRational, mkAttributes, mkKnownLovelace,
                   rationalToLovelacePortion)
import           Cardano.Chain.Delegation (ACertificate (..), Certificate, isValid, signCertificate)
import           Cardano.Chain.Genesis (Config (..), FakeAvvmOptions (..), GeneratedSecrets (..),
                   GenesisAvvmBalances (..), GenesisData (..), GenesisDataError (..),
                   GenesisDataGenerationError (..), GenesisDelegation (..), GenesisDelegationError,
                   GenesisHash (..), GenesisInitializer (..), GenesisSpec (..), PoorSecret (..),
                   TestnetBalanceOptions (..), generateGenesisData, mkGenesisDelegation,
                   mkGenesisSpec, readGenesisData)
import           Cardano.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import           Cardano.Chain.Update (ApplicationName (..), InstallerHash (..), NumSoftwareVersion,
                   Proposal, ProtocolParameters (..), ProtocolVersion (..), SoftforkRule (..),
                   SoftwareVersion (..), SystemTag (..), Vote, checkApplicationName, checkSystemTag)
import           Cardano.Chain.UTxO (ATxAux (..), CompactTxIn, CompactTxOut, TxIn (..), TxOut (..),
                   UTxO (..), defaultUTxOConfiguration, fromCompactTxIn, fromCompactTxOut,
                   genesisUtxo)
