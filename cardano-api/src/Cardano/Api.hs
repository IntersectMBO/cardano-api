{-# LANGUAGE PatternSynonyms #-}

-- | This module provides a library interface for interacting with Cardano as
-- a user of the system.
--
-- It is intended to be the complete API covering everything but without exposing
-- constructors that reveal any lower level types.
--
-- In the interest of simplicity it glosses over some details of the system.
-- Most simple tools should be able to work just using this interface,
-- however you can go deeper and expose the types from the underlying libraries
-- using "Cardano.Api.Byron" or "Cardano.Api.Shelley".
module Cardano.Api
  ( -- * Eras
    ByronEra
  , ShelleyEra
  , AllegraEra
  , MaryEra
  , AlonzoEra
  , BabbageEra
  , ConwayEra
  , CardanoEra (..)
  , IsCardanoEra (..)
  , AnyCardanoEra (..)
  , anyCardanoEra
  , InAnyCardanoEra (..)
  , inAnyCardanoEra
  , cardanoEraConstraints
  , ToCardanoEra (..)

    -- * Eon support
  , Eon (..)
  , EraInEon (..)
  , inEonForEraMaybe
  , forEraInEon
  , forEraInEonMaybe
  , forEraMaybeEon
  , maybeEon
  , monoidForEraInEon
  , monoidForEraInEonA
  , inEonForShelleyBasedEra
  , inEonForShelleyBasedEraMaybe
  , forShelleyBasedEraInEon
  , forShelleyBasedEraInEonMaybe
  , forShelleyBasedEraMaybeEon
  , Featured (..)
  , mkFeatured
  , unFeatured
  , asFeaturedInEra
  , asFeaturedInShelleyBasedEra
  , Convert (..)
  , Inject (..)

    -- * Eons

    -- ** From Byron
  , ByronToAlonzoEra (..)
  , byronToAlonzoEraConstraints

    -- ** From Shelley
  , ShelleyEraOnly (..)
  , shelleyEraOnlyConstraints
  , shelleyEraOnlyToShelleyBasedEra
  , ShelleyToAllegraEra (..)
  , shelleyToAllegraEraConstraints
  , shelleyToAllegraEraToShelleyBasedEra
  , ShelleyToMaryEra (..)
  , shelleyToMaryEraConstraints
  , shelleyToMaryEraToShelleyBasedEra
  , ShelleyToAlonzoEra (..)
  , shelleyToAlonzoEraConstraints
  , shelleyToAlonzoEraToShelleyBasedEra
  , ShelleyToBabbageEra (..)
  , shelleyToBabbageEraConstraints
  , shelleyToBabbageEraToShelleyBasedEra
  , ShelleyBasedEra (..)
  , IsShelleyBasedEra (..)
  , AnyShelleyBasedEra (..)
  , InAnyShelleyBasedEra (..)
  , inAnyShelleyBasedEra
  , shelleyBasedEraConstraints

    -- ** From Allegra
  , AllegraEraOnwards (..)
  , IsAllegraBasedEra (..)

    -- ** From Mary
  , MaryEraOnwards (..)
  , maryEraOnwardsConstraints
  , maryEraOnwardsToShelleyBasedEra
  , IsMaryBasedEra (..)

    -- ** From Alonzo
  , AlonzoEraOnwards (..)
  , alonzoEraOnwardsConstraints
  , alonzoEraOnwardsToShelleyBasedEra
  , IsAlonzoBasedEra (..)

    -- ** From Babbage
  , BabbageEraOnwards (..)
  , babbageEraOnwardsConstraints
  , babbageEraOnwardsToShelleyBasedEra
  , IsBabbageBasedEra (..)

    -- ** From Conway
  , ConwayEraOnwards (..)
  , conwayEraOnwardsConstraints
  , conwayEraOnwardsToShelleyBasedEra
  , conwayEraOnwardsToBabbageEraOnwards
  , IsConwayBasedEra (..)

    -- * Era case handling

    -- ** Case on CardanoEra
  , caseByronOrShelleyBasedEra
  , caseByronToAlonzoOrBabbageEraOnwards

    -- ** Case on ShelleyBasedEra
  , caseShelleyEraOnlyOrAllegraEraOnwards
  , caseShelleyToAllegraOrMaryEraOnwards
  , caseShelleyToMaryOrAlonzoEraOnwards
  , caseShelleyToAlonzoOrBabbageEraOnwards
  , caseShelleyToBabbageOrConwayEraOnwards

    -- ** Eon relaxation

    -- *** for AlonzoEraOnly
  , shelleyToAlonzoEraToShelleyToBabbageEra

    -- *** for AlonzoEraOnwards
  , alonzoEraOnwardsToMaryEraOnwards

    -- *** for BabbageEraOnwards
  , babbageEraOnwardsToMaryEraOnwards
  , babbageEraOnwardsToAlonzoEraOnwards

    -- *** Assertions on era
  , requireShelleyBasedEra

    -- ** IO
  , File (..)
  , FileDirection (..)
  , mapFile
  , onlyIn
  , onlyOut
  , intoFile
  , readByteStringFile
  , readLazyByteStringFile
  , readTextFile
  , writeByteStringFileWithOwnerPermissions
  , writeByteStringFile
  , writeByteStringOutput
  , writeLazyByteStringFileWithOwnerPermissions
  , writeLazyByteStringFile
  , writeLazyByteStringOutput
  , writeTextFileWithOwnerPermissions
  , writeTextFile
  , writeTextOutput

    -- * Type tags
  , HasTypeProxy (..)
  , AsType (..)
  , asType

    -- * Cryptographic key interface
    -- $keys
  , Key (..)
  , SigningKey (..)
  , VerificationKey (..)
  , castVerificationKey
  , castSigningKey
  , generateSigningKey
  , generateInsecureSigningKey

    -- ** Hashes

    -- | In Cardano most keys are identified by their hash, and hashes are
    -- used in many other places.
  , Hash
  , castHash
  , renderSafeHashAsHex

    -- * Mnemonics

    -- | Functions for working with mnemonics
    -- ** Mnemonics generation
  , MnemonicSize (..)
  , generateMnemonic

    -- ** Key derivation from mnemonics
  , MnemonicToSigningKeyError (..)
  , signingKeyFromMnemonic
  , signingKeyFromMnemonicWithPaymentKeyIndex

    -- ** Mnemonic word queries
  , findMnemonicWordsWithPrefix
  , autocompleteMnemonicPrefix

    -- * Payment addresses

    -- | Constructing and inspecting normal payment addresses
  , Address (..)
  , ByronAddr
  , ShelleyAddr
  , NetworkId (..)

    -- ** Byron addresses
  , makeByronAddress
  , ByronKey
  , ByronKeyLegacy

    -- ** Shelley addresses
  , makeShelleyAddress
  , PaymentCredential (..)
  , StakeAddressPointer (..)
  , StakeAddressReference (..)
  , PaymentKey
  , PaymentExtendedKey
  , parseHexHash

    -- ** Addresses in any era
  , AddressAny (..)
  , parseAddressAny

    -- ** Addresses in specific eras
  , AddressInEra (..)
  , isKeyAddress
  , AddressTypeInEra (..)
  , byronAddressInEra
  , shelleyAddressInEra
  , anyAddressInShelleyBasedEra
  , anyAddressInEra
  , toAddressAny
  , makeByronAddressInEra
  , makeShelleyAddressInEra

    -- * Stake addresses

    -- | Constructing and inspecting stake addresses
  , StakeAddress
  , StakeCredential
  , makeStakeAddress
  , stakeAddressCredential
  , StakeKey
  , StakeExtendedKey

    -- ** Multi-asset values
  , Quantity (..)
  , PolicyId (..)
  , scriptPolicyId
  , AssetName (..)
  , AssetId (..)
  , Value
  , parsePolicyId
  , parseAssetName
  , parseTxOutMultiAssetValue
  , parseMintingMultiAssetValue
  , parseUTxOValue
  , selectAsset
  , valueFromList
  , valueToList
  , filterValue
  , negateValue
  , ValueNestedRep (..)
  , ValueNestedBundle (..)
  , valueToNestedRep
  , valueFromNestedRep
  , renderValue
  , renderMultiAsset
  , renderValuePretty
  , renderMultiAssetPretty
  , toLedgerValue
  , fromLedgerValue
  , PolicyAssets (..)
  , policyAssetsToValue
  , valueToPolicyAssets
  , multiAssetToPolicyAssets

    -- ** Ada \/ Lovelace within multi-asset values
  , Lovelace
  , quantityToLovelace
  , lovelaceToQuantity
  , selectLovelace
  , lovelaceToValue
  , valueToLovelace

    -- * Blocks

    -- ** Blocks in the context of an era
  , Block (..)
  , pattern Block
  , BlockHeader (..)
  , getBlockHeader
  , getBlockTxs

    -- ** Points on the chain
  , ChainPoint (..)
  , EpochNo (..)

    -- ** Tip of the chain
  , ChainTip (..)
  , BlockNo (..)
  , chainTipToChainPoint

    -- * Building transactions

    -- * Building transactions

    -- | Constructing and inspecting transactions

    -- ** Transaction bodies
  , TxBody (..)
  , createTransactionBody
  , createAndValidateTransactionBody
  , makeByronTransactionBody
  , TxBodyContent (..)
  , getTxBodyContent

    -- ** Transaction body builders
  , defaultTxBodyContent
  , defaultTxFee
  , defaultTxValidityUpperBound
  , setTxIns
  , modTxIns
  , addTxIns
  , addTxIn
  , setTxInsCollateral
  , modTxInsCollateral
  , addTxInsCollateral
  , addTxInCollateral
  , setTxInsReference
  , modTxInsReference
  , addTxInsReference
  , addTxInReference
  , setTxOuts
  , modTxOuts
  , addTxOuts
  , addTxOut
  , setTxTotalCollateral
  , modTxTotalCollateral
  , setTxReturnCollateral
  , modTxReturnCollateral
  , setTxFee
  , modTxFee
  , setTxValidityLowerBound
  , modTxValidityLowerBound
  , setTxValidityUpperBound
  , modTxValidityUpperBound
  , setTxMetadata
  , modTxMetadata
  , setTxAuxScripts
  , modTxAuxScripts
  , setTxExtraKeyWits
  , modTxExtraKeyWits
  , addTxExtraKeyWits
  , setTxProtocolParams
  , setTxWithdrawals
  , modTxWithdrawals
  , setTxCertificates
  , modTxCertificates
  , setTxUpdateProposal
  , modTxUpdateProposal
  , setTxMintValue
  , modTxMintValue
  , addTxMintValue
  , subtractTxMintValue
  , setTxScriptValidity
  , modTxScriptValidity
  , setTxProposalProcedures
  , setTxVotingProcedures
  , setTxCurrentTreasuryValue
  , setTxTreasuryDonation
  , TxBodyError (..)
  , TxOutputError (..)
  , TxBodyScriptData (..)

    -- ** Transaction Ids
  , TxId (..)
  , parseTxId
  , getTxId
  , getTxIdByron

    -- ** Transaction inputs
  , TxIn (TxIn)
  , parseTxIn
  , TxIns
  , TxIx (TxIx)
  , parseTxIx
  , renderTxIn
  , getReferenceInputsSizeForTxIds

    -- ** Transaction outputs
  , CtxTx
  , CtxUTxO
  , TxOut (TxOut)
  , TxOutValue (..)
  , TxOutInAnyEra (..)
  , txOutInAnyEra
  , txOutValueToLovelace
  , txOutValueToValue
  , lovelaceToTxOutValue
  , TxOutDatum (..)

    -- ** Other transaction body types
  , TxInsCollateral (..)
  , TxInsReference (..)
  , TxTotalCollateral (..)
  , TxReturnCollateral (..)
  , TxFee (..)
  , TxValidityLowerBound (..)
  , TxValidityUpperBound (..)
  , SlotNo (..)
  , EpochSlots (..)
  , TxMetadataInEra (..)
  , TxAuxScripts (..)
  , TxExtraKeyWitnesses (..)
  , TxWithdrawals (..)
  , TxCertificates (..)
  , mkTxCertificates
  , TxUpdateProposal (..)
  , TxMintValue (..)
  , mkTxMintValue
  , txMintValueToValue
  , indexTxMintValue
  , TxVotingProcedures (..)
  , mkTxVotingProcedures
  , TxProposalProcedures (..)
  , mkTxProposalProcedures
  , convProposalProcedures

    -- ** Building vs viewing transactions
  , BuildTxWith (..)
  , BuildTx
  , ViewTx
  , buildTxWithToMaybe

    -- ** Fee calculation
  , LedgerEpochInfo (..)
  , toLedgerEpochInfo
  , evaluateTransactionFee
  , calculateMinTxFee
  , estimateTransactionKeyWitnessCount

    -- ** Minimum required UTxO calculation
  , calculateMinimumUTxO

    -- ** Script execution units
  , evaluateTransactionExecutionUnits
  , ScriptExecutionError (..)
  , TransactionValidityError (..)

    -- ** Transaction balance
  , evaluateTransactionBalance

    -- ** Building transactions with automated fees and balancing
  , estimateBalancedTxBody
  , estimateOrCalculateBalancedTxBody
  , makeTransactionBodyAutoBalance
  , AutoBalanceError (..)
  , BalancedTxBody (..)
  , FeeEstimationMode (..)
  , RequiredShelleyKeyWitnesses (..)
  , RequiredByronKeyWitnesses (..)
  , TotalReferenceScriptsSize (..)
  , TxFeeEstimationError (..)
  , TxBodyErrorAutoBalance (..)
  , TxScriptValidity (..)
  , ScriptValidity (..)
  , txScriptValidityToScriptValidity

    -- * Signing transactions

    -- | Creating transaction witnesses one by one, or all in one go.
  , Tx (Tx)
  , getTxBody
  , getTxWitnesses

    -- ** Signing in one go
  , signByronTransaction
  , signShelleyTransaction

    -- ** Incremental signing and separate witnesses
  , makeSignedByronTransaction
  , makeSignedTransaction
  , KeyWitness
  , makeByronKeyWitness
  , ShelleyWitnessSigningKey (..)
  , makeShelleyKeyWitness
  , makeShelleyKeyWitness'
  , makeShelleyBootstrapWitness
  , makeShelleyBasedBootstrapWitness

    -- * Transaction metadata

    -- | Embedding additional structured data within transactions.
  , TxMetadata (..)
  , AsTxMetadata (..)

    -- ** Constructing metadata
  , TxMetadataValue (..)
  , makeTransactionMetadata
  , mergeTransactionMetadata
  , metaTextChunks
  , metaBytesChunks

    -- ** Validating metadata
  , validateTxMetadata
  , TxMetadataRangeError (..)

    -- ** Conversion to\/from JSON
  , TxMetadataJsonSchema (..)
  , metadataFromJson
  , metadataToJson
  , metadataValueFromJsonNoSchema
  , metadataValueToJsonNoSchema
  , TxMetadataJsonError (..)
  , TxMetadataJsonSchemaError (..)

    -- * Governance action metadata
  , CIP108 (..)

    -- ** DRep Metadata
  , DRepMetadata
  , DRepMetadataReference
  , hashDRepMetadata
  , CIP119 (..)

    -- * Certificates
  , Certificate (..)

    -- ** Registering stake address and delegating

    -- | Certificates that are embedded in transactions for registering and
    -- unregistering stake address, and for setting the stake pool delegation
    -- choice for a stake address.
  , StakeAddressRequirements (..)
  , StakeDelegationRequirements (..)
  , makeStakeAddressDelegationCertificate
  , makeStakeAddressRegistrationCertificate
  , makeStakeAddressUnregistrationCertificate
  , makeStakeAddressAndDRepDelegationCertificate

    -- ** Registering stake pools

    -- | Certificates that are embedded in transactions for registering and
    -- retiring stake pools. This includes updating the stake pool parameters.
  , StakePoolRegistrationRequirements (..)
  , StakePoolRetirementRequirements (..)
  , makeStakePoolRegistrationCertificate
  , makeStakePoolRetirementCertificate
  , StakePoolParameters
  , StakePoolRelay
  , StakePoolMetadataReference

    -- ** Anchor data
  , AnchorDataFromCertificateError (..)
  , getAnchorDataFromCertificate
  , isDRepRegOrUpdateCert

    -- * Rewards
  , DelegationsAndRewards (..)
  , mergeDelegsAndRewards

    -- * Stake pool off-chain metadata
  , StakePoolMetadata
  , validateAndHashStakePoolMetadata
  , StakePoolMetadataValidationError (..)

    -- * Scripts

    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.

    -- ** Script languages
  , SimpleScript'
  , PlutusScriptV1
  , PlutusScriptV2
  , PlutusScriptV3
  , ScriptLanguage (..)
  , PlutusScriptVersion (..)
  , AnyScriptLanguage (..)
  , AnyPlutusScriptVersion (..)
  , IsPlutusScriptLanguage (..)
  , IsScriptLanguage (..)

    -- ** Scripts in a specific language
  , Script (..)
  , PlutusScriptInEra (..)

    -- ** Scripts in any language
  , ScriptInAnyLang (..)
  , toScriptInAnyLang

    -- ** Scripts in a specific era
  , ScriptInEra (..)
  , toScriptInEra
  , eraOfScriptInEra
  , HasScriptLanguageInEra (..)
  , ToAlonzoScript (..)
  , AlonzoEraOnwardsConstraints

    -- ** Use of a script in an era as a witness
  , WitCtxTxIn
  , WitCtxMint
  , WitCtxStake
  , WitCtx (..)
  , ScriptWitness (..)
  , getScriptWitnessScript
  , getScriptWitnessReferenceInput
  , getScriptWitnessReferenceInputOrScript
  , Witness (..)
  , KeyWitnessInCtx (..)
  , ScriptWitnessInCtx (..)
  , IsScriptWitnessInCtx (..)
  , ScriptDatum (..)
  , ScriptRedeemer

    -- ** Inspecting 'ScriptWitness'es
  , AnyScriptWitness (..)
  , ScriptWitnessIndex (..)
  , renderScriptWitnessIndex
  , collectTxBodyScriptWitnesses

    -- ** Languages supported in each era
  , ScriptLanguageInEra (..)
  , scriptLanguageSupportedInEra
  , sbeToSimpleScriptLanguageInEra
  , languageOfScriptLanguageInEra
  , eraOfScriptLanguageInEra

    -- ** Simple scripts

    -- | Making multi-signature and time-lock scripts.
  , SimpleScript (..)

    -- ** Plutus scripts
  , PlutusScript
  , examplePlutusScriptAlwaysSucceeds
  , examplePlutusScriptAlwaysFails

    -- ** Script data
  , collectPlutusScriptHashes
  , HashableScriptData
  , hashScriptDataBytes
  , getOriginalScriptDataBytes
  , getScriptData
  , unsafeHashableScriptData
  , ScriptData (..)
  , parseScriptDataHash

    -- ** Validation
  , ScriptDataRangeError (..)
  , validateScriptData

    -- ** Conversion to\/from JSON
  , ScriptDataJsonSchema (..)
  , scriptDataFromJson
  , scriptDataToJson
  , ScriptDataJsonError (..)
  , ScriptDataJsonSchemaError (..)
  , ScriptDataJsonBytesError (..)
  , scriptDataJsonToHashable

    -- ** Script execution units
  , ExecutionUnits (..)
  , ExecutionUnitPrices (..)
  , CostModel (..)
  , toAlonzoCostModel
  , fromAlonzoCostModel
  , toAlonzoCostModels

    -- ** Script addresses

    -- | Making addresses from scripts.
  , ScriptHash (..)
  , parseScriptHash
  , hashScript

    -- * Serialisation

    -- | Support for serialising data in JSON, CBOR and text files.
  , InputFormat (..)
  , InputDecodeError (..)
  , deserialiseInput
  , deserialiseInputAnyOf
  , renderInputDecodeError
  , SomeAddressVerificationKey (..)
  , deserialiseAnyVerificationKey
  , deserialiseAnyVerificationKeyBech32
  , deserialiseAnyVerificationKeyTextEnvelope
  , renderSomeAddressVerificationKey
  , mapSomeAddressVerificationKey

    -- ** CBOR
  , SerialiseAsCBOR
  , ToCBOR
  , FromCBOR
  , serialiseToCBOR
  , deserialiseFromCBOR

    -- ** JSON
  , ToJSON
  , FromJSON
  , serialiseToJSON
  , deserialiseFromJSON
  , JsonDecodeError (..)
  , readFileJSON
  , writeFileJSON
  , prettyPrintJSON

    -- ** Bech32
  , SerialiseAsBech32
  , serialiseToBech32
  , deserialiseFromBech32
  , deserialiseAnyOfFromBech32
  , Bech32DecodeError (..)
  , UsingBech32 (..)

    -- ** Bech32 CIP-129
  , Cip129 (..)
  , deserialiseFromBech32Cip129
  , serialiseToBech32Cip129
  , serialiseGovActionIdToBech32Cip129
  , deserialiseGovActionIdFromBech32Cip129

    -- ** Addresses

    -- | Address serialisation is (sadly) special
  , SerialiseAddress
  , serialiseAddress
  , deserialiseAddress

    -- ** Raw binary

    -- | Some types have a natural raw binary format.
  , SerialiseAsRawBytes
  , serialiseToRawBytes
  , deserialiseFromRawBytes
  , SerialiseAsRawBytesError (..)
  , serialiseToRawBytesHex
  , deserialiseFromRawBytesHex
  , serialiseToRawBytesHexText
  , parseRawBytesHex
  , RawBytesHexError (..)
  , UsingRawBytes (..)
  , UsingRawBytesHex (..)

    -- ** Text envelope

    -- | Support for a envelope file format with text headers and a hex-encoded
    -- binary payload.
  , HasTextEnvelope (..)
  , TextEnvelope (..)
  , TextEnvelopeType (..)
  , TextEnvelopeDescr
  , TextEnvelopeError (..)
  , textEnvelopeTypeInEra
  , textEnvelopeRawCBOR
  , textEnvelopeToJSON
  , serialiseToTextEnvelope
  , deserialiseFromTextEnvelope
  , readFileTextEnvelope
  , writeFileTextEnvelope
  , readTextEnvelopeFromFile
  , readTextEnvelopeOfTypeFromFile

    -- ** Text envelope CDDL

    -- | Support for serialising values in the ledger's CDDL format.
    -- Note, this will be deprecated in the future in favour of a
    -- single API.
  , FromSomeTypeCDDL (..)
  , readFileTextEnvelopeCddlAnyOf
  , deserialiseFromTextEnvelopeCddlAnyOf
  , writeTxFileTextEnvelopeCddl
  , writeTxFileTextEnvelopeCanonicalCddl
  , writeTxWitnessFileTextEnvelopeCddl
  , deserialiseByronTxCddl
  , serialiseWitnessLedgerCddl
  , deserialiseWitnessLedgerCddl
  , TextEnvelopeCddlError (..)

    -- *** Reading one of several key types
  , readKeyFile
  , readKeyFileTextEnvelope
  , readKeyFileAnyOf

    -- *** Read one of several types
  , FromSomeType (..)
  , deserialiseFromTextEnvelopeAnyOf
  , readFileTextEnvelopeAnyOf

    -- * Errors
  , Error (..)
  , throwErrorAsException
  , FileError (..)

    -- * Node interaction

    -- | Operations that involve talking to a local Cardano node.

    -- ** Node Config
  , NodeConfig (..)
  , NodeConfigFile
  , readNodeConfig

    -- ** Genesis Files
  , ByronGenesisFile
  , ShelleyGenesisFile
  , AlonzoGenesisFile
  , ConwayGenesisFile

    -- *** Genesis Config
  , GenesisConfig (..)
  , readCardanoGenesisConfig
  , mkProtocolInfoCardano

    -- **** Byron Genesis Config
  , readByronGenesisConfig
  , GenesisHashByron (..)

    -- **** Shelley Genesis Config
  , ShelleyConfig (..)
  , GenesisHashShelley (..)
  , readShelleyGenesisConfig
  , shelleyPraosNonce

    -- **** Alonzo Genesis Config
  , GenesisHashAlonzo (..)
  , readAlonzoGenesisConfig

    -- **** Conway Genesis Config
  , GenesisHashConway (..)
  , readConwayGenesisConfig

    -- *** Environment
  , Env (..)
  , genesisConfigToEnv

    -- ** Queries

    -- ** Submitting transactions

    -- ** High level protocol interaction with a Cardano node

    -- *** Initialization / Accumulation
  , envSecurityParam
  , LedgerState (..)
  , initialLedgerState
  , encodeLedgerState
  , decodeLedgerState
  , applyBlock
  , ValidationMode (..)

    -- *** Traversing the block chain
  , foldBlocks
  , FoldStatus (..)
  , chainSyncClientWithLedgerState
  , chainSyncClientPipelinedWithLedgerState

    -- *** Ledger state conditions
  , ConditionResult (..)
  , fromConditionResult
  , toConditionResult
  , AnyNewEpochState (..)
  , foldEpochState
  , getAnyNewEpochState
  , getLedgerTablesUTxOValues

    -- *** Errors
  , LedgerStateError (..)
  , FoldBlocksError (..)
  , GenesisConfigError (..)
  , InitialLedgerStateError (..)

    -- ** Low level protocol interaction with a Cardano node
  , connectToLocalNode
  , connectToLocalNodeWithVersion
  , LocalNodeConnectInfo (..)
  , ConsensusModeParams (..)
  , ConsensusProtocol
  , ChainDepStateProtocol
  , ConsensusBlockForEra
  , LocalNodeClientProtocols (..)
  , LocalNodeClientParams (..)
  , mkLocalNodeClientParams
  , LocalChainSyncClient (..)
  --  connectToRemoteNode,

    -- ** Protocol related types
  , BlockType (..)
  , SomeBlockType (..)
  , reflBlockType
  , Protocol (..)
  , ProtocolInfoArgs (..)

    -- *** Chain sync protocol

    -- | To construct a @ChainSyncClient@ see @Cardano.Api.Client@ or
    -- @Cardano.Api.ClientPipelined@.
  , ChainSyncClient (..)
  , ChainSyncClientPipelined (..)
  , BlockInMode (..)
  , LocalNodeClientProtocolsInMode

    -- *** Local tx submission
  , LocalTxSubmissionClient (..)
  , TxInMode (..)
  , TxValidationErrorInCardanoMode (..)
  , SubmitResult (..)
  , submitTxToNodeLocal

    -- *** Local state query
  , LocalStateQueryClient (..)
  , QueryInMode (..)
  , SystemStart (..)
  , QueryInEra (..)
  , QueryInShelleyBasedEra (..)
  , QueryUTxOFilter (..)
  , UTxO (..)
  , queryNodeLocalState
  , executeQueryCardanoMode
  , UnsupportedNtcVersionError (..)

    -- *** Local tx monitoring
  , LocalTxMonitorClient (..)
  , LocalTxMonitoringQuery (..)
  , LocalTxMonitoringResult (..)
  , MempoolSizeAndCapacity (..)
  , queryTxMonitoringLocal
  , TxIdInMode (..)
  , EraHistory (..)
  , getProgress
  , getSlotForRelativeTime

    -- *** Common queries
  , determineEra
  , getLocalChainTip

    -- * Node operation

    -- | Support for the steps needed to operate a node

    -- ** Operational certificates
  , OperationalCertificate
  , OperationalCertificateIssueCounter
  , OperationalCertIssueError
  , getHotKey
  , getKesPeriod
  , getOpCertCount
  , issueOperationalCertificate

    -- * Constitutional Committee keys
  , CommitteeColdKey
  , CommitteeColdExtendedKey
  , CommitteeHotKey
  , CommitteeHotExtendedKey

    -- * Genesis file

    -- | Types and functions needed to inspect or create a genesis file.
  , GenesisKey
  , GenesisExtendedKey
  , GenesisDelegateKey
  , GenesisDelegateExtendedKey
  , GenesisUTxOKey
  , genesisUTxOPseudoTxIn

    -- ** Genesis parameters
  , GenesisParameters (..)

    -- * Special transactions

    -- | There are various additional things that can be embedded in a
    -- transaction for special operations.
  , GenesisKeyDelegationRequirements (..)
  , MirCertificateRequirements (..)
  , makeMIRCertificate
  , makeGenesisKeyDelegationCertificate
  , MIRTarget (..)
  , MIRPot (..)
  , selectStakeCredentialWitness

    -- * Protocol parameter updates
  , UpdateProposal (..)
  , ProtocolParametersUpdate (..)
  , makeShelleyUpdateProposal
  , PraosNonce
  , makePraosNonce
  , NetworkMagic (..)

    -- * Protocol parameters
  , ProtocolParametersConversionError (..)

    -- ** Conversions
  , toCtxUTxOTxOut
  , fromCtxUTxOTxOut
  -- TODO: arrange not to export these
  , fromNetworkMagic
  , toNetworkMagic
  , fromLedgerTxOuts
  , toLedgerUTxO
  , fromLedgerUTxO
  , SlotsInEpoch (..)
  , SlotsToEpochEnd (..)
  , slotToEpoch

    -- * Node socket related
  , SocketPath
  , NodeToClientVersion (..)

    -- ** Queries
  , executeQueryAnyMode

    -- ** Monadic queries
  , LocalStateQueryExpr
  , executeLocalStateQueryExpr
  , queryExpr
  , chainPointToSlotNo
  , chainPointToHeaderHash
  , makeChainTip
  , writeSecrets

    -- * Convenience functions

    -- ** Transaction construction
  , constructBalancedTx

    -- ** Queries
  , QueryConvenienceError (..)
  , TxCurrentTreasuryValue (..)
  , queryStateForBalancedTx
  , renderQueryConvenienceError

    -- ** Misc
  , ScriptLockedTxInsError (..)
  , TxInsExistError (..)
  , renderNotScriptLockedTxInsError
  , renderTxInsExistError
  , txInsExistInUTxO
  , notScriptLockedTxIns
  , textShow

    -- ** Query expressions
  , queryAccountState
  , queryChainBlockNo
  , queryChainPoint
  , queryCurrentEpochState
  , queryCurrentEra
  , queryDebugLedgerState
  , queryLedgerPeerSnapshot
  , queryEpoch
  , queryConstitutionHash
  , queryEraHistory
  , queryGenesisParameters
  , queryPoolDistribution
  , queryPoolState
  , queryProtocolParameters
  , queryProtocolState
  , queryStakeAddresses
  , queryStakeDelegDeposits
  , queryStakeDistribution
  , queryStakePoolParameters
  , queryStakePools
  , queryStakeSnapshot
  , querySystemStart
  , queryUtxo
  , queryConstitution
  , queryGovState
  , queryRatifyState
  , queryFuturePParams
  , queryDRepState
  , queryDRepStakeDistribution
  , querySPOStakeDistribution
  , queryProposals
  , queryCommitteeMembersState
  , queryStakeVoteDelegatees
  , queryStakePoolDefaultVote
  , queryLedgerConfig

    -- ** Committee State Query
  , MemberStatus (..)
  , CommitteeMembersState (..)

    -- ** DReps
  , DRepKey
  , DRepExtendedKey

    -- ** Governance actions
  , getAnchorDataFromGovernanceAction
  , validateGovActionAnchorData

    -- ** Governance related certificates
  , AnchorDataHash (..)
  , AnchorUrl (..)
  , CommitteeColdkeyResignationRequirements (..)
  , CommitteeHotKeyAuthorizationRequirements (..)
  , DRepRegistrationRequirements (..)
  , DRepUnregistrationRequirements (..)
  , DRepUpdateRequirements (..)
  , makeCommitteeColdkeyResignationCertificate
  , makeCommitteeHotKeyAuthorizationCertificate
  , makeDrepRegistrationCertificate
  , makeDrepUnregistrationCertificate
  , makeDrepUpdateCertificate
  , ResolvablePointers (..)
  , unsafeBoundedRational

    -- ** Debug
  , DebugPlutusFailure (..)
  , renderDebugPlutusFailure

    -- ** Supporting modules
  , module Cardano.Api.Internal.Monad.Error
  , module Cardano.Api.Internal.Pretty
  )
where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Anchor
import Cardano.Api.Internal.Block
import Cardano.Api.Internal.CIP.Cip129
import Cardano.Api.Internal.Certificate
import Cardano.Api.Internal.Convenience.Construction
import Cardano.Api.Internal.Convenience.Query
import Cardano.Api.Internal.DRepMetadata
import Cardano.Api.Internal.DeserialiseAnyOf
import Cardano.Api.Internal.Eon.AllegraEraOnwards
import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Internal.Eon.ByronToAlonzoEra
import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Internal.Eon.MaryEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eon.ShelleyEraOnly
import Cardano.Api.Internal.Eon.ShelleyToAllegraEra
import Cardano.Api.Internal.Eon.ShelleyToAlonzoEra
import Cardano.Api.Internal.Eon.ShelleyToBabbageEra
import Cardano.Api.Internal.Eon.ShelleyToMaryEra
import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.Eras.Case
import Cardano.Api.Internal.Error
import Cardano.Api.Internal.Feature
import Cardano.Api.Internal.Fees
import Cardano.Api.Internal.Genesis
import Cardano.Api.Internal.GenesisParameters
import Cardano.Api.Internal.Governance.Actions.ProposalProcedure
import Cardano.Api.Internal.Governance.Metadata.DrepRegistration (CIP119 (..))
import Cardano.Api.Internal.Governance.Metadata.GovAction (CIP108 (..))
import Cardano.Api.Internal.Governance.Metadata.Validation
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Hash
import Cardano.Api.Internal.IO
import Cardano.Api.Internal.IPC
import Cardano.Api.Internal.IPC.Monad
import Cardano.Api.Internal.InMode
import Cardano.Api.Internal.Keys.Byron
import Cardano.Api.Internal.Keys.Class
import Cardano.Api.Internal.Keys.Mnemonics
import Cardano.Api.Internal.Keys.Read
import Cardano.Api.Internal.Keys.Shelley
import Cardano.Api.Internal.LedgerState
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.Monad.Error
import Cardano.Api.Internal.NetworkId
import Cardano.Api.Internal.OperationalCertificate
import Cardano.Api.Internal.Orphans ()
import Cardano.Api.Internal.Plutus
import Cardano.Api.Internal.Pretty
import Cardano.Api.Internal.Protocol
import Cardano.Api.Internal.ProtocolParameters
import Cardano.Api.Internal.Query hiding (LedgerState (..))
import Cardano.Api.Internal.Query.Expr
import Cardano.Api.Internal.Rewards
import Cardano.Api.Internal.Script
import Cardano.Api.Internal.ScriptData
import Cardano.Api.Internal.Serialise.Cbor
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseJSON
import Cardano.Api.Internal.SerialiseLedgerCddl
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.SerialiseTextEnvelope
import Cardano.Api.Internal.SerialiseUsing
import Cardano.Api.Internal.StakePoolMetadata
import Cardano.Api.Internal.Tx.Body
import Cardano.Api.Internal.Tx.Sign
import Cardano.Api.Internal.Tx.UTxO
import Cardano.Api.Internal.TxMetadata
import Cardano.Api.Internal.Utils
import Cardano.Api.Internal.Value
import Cardano.Api.Internal.ValueParser
