# Changelog for cardano-api

## 10.17.1.0

- Cardano.Api.Experimental: Fix missing key witnesses in certificates
  (compatible, bugfix)
  [PR 879](https://github.com/IntersectMBO/cardano-api/pull/879)

- Add `genChainPoint` and `genChainPointAt`.
  (feature, compatible)
  [PR 877](https://github.com/IntersectMBO/cardano-api/pull/877)

- Add `null` `size` functions to `Cardano.Api.UTxO`.
  (feature, compatible)
  [PR 876](https://github.com/IntersectMBO/cardano-api/pull/876)

## 10.17.0.0

- Introduce new certificate type
  (feature, compatible)
  [PR 858](https://github.com/IntersectMBO/cardano-api/pull/858)

- Breaking refactor: change module structure according to [ADR-009](https://github.com/input-output-hk/cardano-node-wiki/wiki/ADR-009-cardano-api-exports-convention)

  Add new exported modules:

    - Cardano.Api.Certificate
    - Cardano.Api.Era
    - Cardano.Api.Experimental.Plutus
    - Cardano.Api.Genesis
    - Cardano.Api.Governance
    - Cardano.Api.Key
    - Cardano.Api.IPC
    - Cardano.Api.Plutus
    - Cardano.Api.Query
    - Cardano.Api.Serialise.DeserialiseAnyOf
    - Cardano.Api.Serialise.TextEnvelope
    - Cardano.Api.Serialise.Tx
    - Cardano.Api.Value

  Rename modules:

    - Cardano.Api.Byron.Internal.Key -> Cardano.Api.Internal.Keys.Byron
    - Cardano.Api.Byron.Internal.Proposal -> Cardano.Api.Internal.SpecialByron
    - Cardano.Api.Certificate.Internal -> Cardano.Api.Internal.Certificate
    - Cardano.Api.Certificate.Internal.OperationalCertificate -> Cardano.Api.Internal.OperationalCertificate
    - Cardano.Api.Certificate.Internal.StakePoolMetadata -> Cardano.Api.Internal.StakePoolMetadata
    - Cardano.Api.Certificate.Internal.DRepMetadata -> Cardano.Api.Internal.DRepMetadata
    - Cardano.Api.Compatible.Tx -> Cardano.Api.Internal.Compatible.Tx
    - Cardano.Api.ProtocolParameters -> Cardano.Api.Internal.ProtocolParameters
    - Cardano.Api.Consensus.Internal.Reexport -> Cardano.Api.Internal.ReexposeConsensus
    - Cardano.Api.Consensus.Internal.Mode -> Cardano.Api.Internal.Modes
    - Cardano.Api.Consensus.Internal.InMode -> Cardano.Api.Internal.InMode
    - Cardano.Api.Consensus.Internal.Protocol -> Cardano.Api.Internal.Protocol
    - Cardano.Api.Era.Internal.Case -> Cardano.Api.Internal.Eras.Case
    - Cardano.Api.Era.Internal.Core -> Cardano.Api.Internal.Eras.Core
    - Cardano.Api.Era.Internal.Feature -> Cardano.Api.Internal.Feature
    - Cardano.Api.Era.Internal.Eon.AllegraEraOnwards -> Cardano.Api.Internal.Eon.AllegraEraOnwards
    - Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards -> Cardano.Api.Internal.Eon.AlonzoEraOnwards
    - Cardano.Api.Era.Internal.Eon.BabbageEraOnwards -> Cardano.Api.Internal.Eon.BabbageEraOnwards
    - Cardano.Api.Era.Internal.Eon.ByronToAlonzoEra -> Cardano.Api.Internal.Eon.ByronToAlonzoEra
    - Cardano.Api.Era.Internal.Eon.Convert -> Cardano.Api.Internal.Eon.Convert
    - Cardano.Api.Era.Internal.Eon.ConwayEraOnwards -> Cardano.Api.Internal.Eon.ConwayEraOnwards
    - Cardano.Api.Era.Internal.Eon.MaryEraOnwards -> Cardano.Api.Internal.Eon.MaryEraOnwards
    - Cardano.Api.Era.Internal.Eon.ShelleyBasedEra -> Cardano.Api.Internal.Eon.ShelleyBasedEra
    - Cardano.Api.Era.Internal.Eon.ShelleyEraOnly -> Cardano.Api.Internal.Eon.ShelleyEraOnly
    - Cardano.Api.Era.Internal.Eon.ShelleyToAllegraEra -> Cardano.Api.Internal.Eon.ShelleyToAllegraEra
    - Cardano.Api.Era.Internal.Eon.ShelleyToAlonzoEra -> Cardano.Api.Internal.Eon.ShelleyToAlonzoEra
    - Cardano.Api.Era.Internal.Eon.ShelleyToBabbageEra -> Cardano.Api.Internal.Eon.ShelleyToBabbageEra
    - Cardano.Api.Era.Internal.Eon.ShelleyToMaryEra -> Cardano.Api.Internal.Eon.ShelleyToMaryEra
    - Cardano.Api.Era -> Cardano.Api.Internal.Eras
    - Cardano.Api.Experimental.Era -> Cardano.Api.Internal.Experimental.Eras
    - Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness -> Cardano.Api.Internal.Experimental.Plutus.IndexedPlutusScriptWitness
    - Cardano.Api.Experimental.Plutus.Internal.ScriptWitness -> Cardano.Api.Internal.Experimental.Plutus.ScriptWitness
    - Cardano.Api.Experimental.Plutus.Internal.Script -> Cardano.Api.Internal.Experimental.Plutus.Script
    - Cardano.Api.Experimental.Plutus.Internal.Shim.LegacyScripts -> Cardano.Api.Internal.Experimental.Plutus.Shim.LegacyScripts
    - Cardano.Api.Experimental.Simple.Script -> Cardano.Api.Internal.Experimental.Simple.Script
    - Cardano.Api.Experimental.Tx -> Cardano.Api.Internal.Experimental.Tx
    - Cardano.Api.Experimental.Tx.Internal.AnyWitness -> Cardano.Api.Internal.Experimental.Witness.AnyWitness
    - Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements -> Cardano.Api.Internal.Experimental.Witness.TxScriptWitnessRequirements
    - Cardano.Api.Genesis.Internal.Parameters -> Cardano.Api.Internal.GenesisParameters
    - Cardano.Api.Genesis.Internal -> Cardano.Api.Internal.Genesis
    - Cardano.Api.Governance.Internal.Action.ProposalProcedure -> Cardano.Api.Internal.Governance.Actions.ProposalProcedure
    - Cardano.Api.Governance.Internal.Action.VotingProcedure -> Cardano.Api.Internal.Governance.Actions.VotingProcedure
    - Cardano.Api.Governance.Internal.Metadata.Anchor -> Cardano.Api.Internal.Anchor
    - Cardano.Api.Governance.Internal.Metadata.DrepRegistration -> Cardano.Api.Internal.Governance.Metadata.DrepRegistration
    - Cardano.Api.Governance.Internal.Metadata.GovAction -> Cardano.Api.Internal.Governance.Metadata.GovAction
    - Cardano.Api.Governance.Internal.Metadata.Validation -> Cardano.Api.Internal.Governance.Metadata.Validation
    - Cardano.Api.Governance.Internal.Poll -> Cardano.Api.Internal.Governance.Poll
    - Cardano.Api.IO.Internal.Compat.Posix -> Cardano.Api.Internal.IO.Compat.Posix
    - Cardano.Api.IO.Internal.Compat.Win32 -> Cardano.Api.Internal.IO.Compat.Win32
    - Cardano.Api.IO.Internal.Compat -> Cardano.Api.Internal.IO.Compat
    - Cardano.Api.IO.Internal.Base -> Cardano.Api.Internal.IO.Base
    - Cardano.Api.IO -> Cardano.Api.Internal.IO
    - Cardano.Api.Key.Internal -> Cardano.Api.Internal.Keys.Shelley
    - Cardano.Api.Key.Internal.Class -> Cardano.Api.Internal.Keys.Class
    - Cardano.Api.Key.Internal.Mnemonic -> Cardano.Api.Internal.Keys.Mnemonics
    - Cardano.Api.Key.Internal.Praos -> Cardano.Api.Internal.Keys.Praos
    - Cardano.Api.Key.Internal.SomeAddressVerificationKey -> Cardano.Api.Internal.Keys.SomeAddressVerificationKey
    - Cardano.Api.Ledger.Internal.Reexport -> Cardano.Api.Internal.ReexposeLedger
    - Cardano.Api.LedgerState -> Cardano.Api.Internal.LedgerState
    - Cardano.Api.LedgerState.Internal.ConvertLedgerEvent -> Cardano.Api.Internal.LedgerEvents.ConvertLedgerEvent
    - Cardano.Api.LedgerState.Internal.LedgerEvent -> Cardano.Api.Internal.LedgerEvents.LedgerEvent
    - Cardano.Api.LedgerState.Internal.Rule.BBODY.DELEGS -> Cardano.Api.Internal.LedgerEvents.Rule.BBODY.DELEGS
    - Cardano.Api.LedgerState.Internal.Rule.BBODY.LEDGER -> Cardano.Api.Internal.LedgerEvents.Rule.BBODY.LEDGER
    - Cardano.Api.LedgerState.Internal.Rule.BBODY.UTXOW -> Cardano.Api.Internal.LedgerEvents.Rule.BBODY.UTXOW
    - Cardano.Api.LedgerState.Internal.Rule.TICK.RUPD -> Cardano.Api.Internal.LedgerEvents.Rule.TICK.RUPD
    - Cardano.Api.LedgerState.Internal.Rule.TICK.NEWEPOCH -> Cardano.Api.Internal.LedgerEvents.Rule.TICK.NEWEPOCH
    - Cardano.Api.Network.Internal.Reexport -> Cardano.Api.Internal.ReexposeNetwork
    - Cardano.Api.Network.Internal.NetworkId -> Cardano.Api.Internal.NetworkId
    - Cardano.Api.Network.IPC.Internal.ChainSync.ClientPipelined -> Cardano.Api.ChainSync.ClientPipelined
    - Cardano.Api.Network.IPC.Internal.ChainSync.Client -> Cardano.Api.ChainSync.Client
    - Cardano.Api.Network.IPC.Internal.Monad -> Cardano.Api.Internal.IPC.Monad
    - Cardano.Api.Network.IPC.Internal.Version -> Cardano.Api.Internal.IPC.Version
    - Cardano.Api.Network.IPC.Internal -> Cardano.Api.Internal.IPC
    - Cardano.Api.Plutus.Internal -> Cardano.Api.Internal.Plutus
    - Cardano.Api.Plutus.Internal.ScriptData -> Cardano.Api.Internal.ScriptData
    - Cardano.Api.Plutus.Internal.Script -> Cardano.Api.Internal.Script
    - Cardano.Api.Serialise.Bech32 -> Cardano.Api.Internal.SerialiseBech32
    - Cardano.Api.Serialise.Cip129 -> Cardano.Api.Internal.CIP.Cip129
    - Cardano.Api.Serialise.Cbor.Canonical -> Cardano.Api.Internal.Serialise.Cbor.Canonical
    - Cardano.Api.Serialise.Cbor -> Cardano.Api.Internal.Serialise.Cbor
    - Cardano.Api.Serialise.DeserialiseAnyOf -> Cardano.Api.Internal.DeserialiseAnyOf
    - Cardano.Api.Serialise.Json -> Cardano.Api.Internal.SerialiseJSON
    - Cardano.Api.Serialise.Raw -> Cardano.Api.Internal.SerialiseRaw
    - Cardano.Api.Serialise.SerialiseUsing -> Cardano.Api.Internal.SerialiseUsing
    - Cardano.Api.Serialise.TextEnvelope.Internal -> Cardano.Api.Internal.SerialiseTextEnvelope
    - Cardano.Api.Serialise.TextEnvelope.Internal.Cddl -> Cardano.Api.Internal.SerialiseLedgerCddl
    - Cardano.Api.Query.Internal.Convenience -> Cardano.Api.Internal.Convenience.Query
    - Cardano.Api.Query.Internal.Expr -> Cardano.Api.Internal.Query.Expr
    - Cardano.Api.Query.Internal.Type.DebugLedgerState -> Cardano.Api.Internal.Query.Types
    - Cardano.Api.Query.Internal.Type.DelegationsAndRewards -> Cardano.Api.Internal.Rewards
    - Cardano.Api.Query.Internal.Type.QueryInMode -> Cardano.Api.Internal.Query
    - Cardano.Api.Tx.Internal.Body -> Cardano.Api.Internal.Tx.Body
    - Cardano.Api.Tx.Internal.Body.Lens -> Cardano.Api.Ledger.Lens
    - Cardano.Api.Tx.Internal.Convenience -> Cardano.Api.Internal.Convenience.Construction
    - Cardano.Api.Tx.Internal.Fee -> Cardano.Api.Internal.Fees
    - Cardano.Api.Tx.Internal.Sign -> Cardano.Api.Internal.Tx.Sign
    - Cardano.Api.Tx.Internal.BuildTxWith -> Cardano.Api.Internal.Tx.BuildTxWith
    - Cardano.Api.Tx.Internal.Output -> Cardano.Api.Internal.Tx.Output
    - Cardano.Api.Tx.Internal.TxIn -> Cardano.Api.Internal.TxIn
    - Cardano.Api.Tx.Internal.TxMetadata -> Cardano.Api.Internal.TxMetadata
    - Cardano.Api.Address -> Cardano.Api.Internal.Address
    - Cardano.Api.Block -> Cardano.Api.Internal.Block
    - Cardano.Api.Error -> Cardano.Api.Internal.Error
    - Cardano.Api.HasTypeProxy -> Cardano.Api.Internal.HasTypeProxy
    - Cardano.Api.Hash -> Cardano.Api.Internal.Hash
    - Cardano.Api.Monad.Error -> Cardano.Api.Internal.Monad.Error
    - Cardano.Api.Pretty -> Cardano.Api.Internal.Pretty
    - Cardano.Api.Pretty.Internal.ShowOf -> Cardano.Api.Internal.Via.ShowOf
    - Cardano.Api.UTxO -> Cardano.Api.Internal.Tx.UTxO
    - Cardano.Api.Value.Internal.Parser -> Cardano.Api.Internal.ValueParser
    - Cardano.Api.Value.Internal -> Cardano.Api.Internal.Value
  (breaking, refactoring)
  [PR 840](https://github.com/IntersectMBO/cardano-api/pull/840)

- Remove partial `IsString` instances. Use `deserialiseFromRawBytesHex` or specialised parser instead.
  Make `SerialiseAsBech32` class use `HumanReadablePart` instead of `Text` for Bech32 prefix in `bech32PrefixFor` and `bech32PrefixesPermitted` functions.
  (breaking, refactoring)
  [PR 842](https://github.com/IntersectMBO/cardano-api/pull/842)

- Add `genBlockHeader` and related functions to `Test.Gen.Cardano.Api.Typed`.
  (compatible)
  [PR 834](https://github.com/IntersectMBO/cardano-api/pull/834)

- Add more `Data.Map` style functions to `Cardano.Api.Tx.UTxO`: `insert`, `delete`, `adjust`, `union`, `unions`, `difference`, `intersection`, `map`. `mapWithKey`, `mapKeys`, `foldMap`, `fromMap`, `find`, `findWithKey`.
  (compatible)
  [PR 841](https://github.com/IntersectMBO/cardano-api/pull/841)

## 10.16.3.0

- Add `IsShelleyBasedEra` constraint to `EraCommonConstraints`
  (compatible)
  [PR 849](https://github.com/IntersectMBO/cardano-api/pull/849)

- Added an instance for `Convert ConwayEraOnwards Era`
  (feature)
  [PR 848](https://github.com/IntersectMBO/cardano-api/pull/848)

## 10.16.2.0

- Add `IsCardanoEra` constraint to `EraCommonConstraints`
  (compatible)
  [PR 846](https://github.com/IntersectMBO/cardano-api/pull/846)

- Add MonoTraversable, MonoFoldable, MonoFunctor to the UTxO type
  (compatible)
  [PR 845](https://github.com/IntersectMBO/cardano-api/pull/845)

## 10.16.1.0

- Bump network and consensus dependencies
  (compatible, maintenance)
  [PR 843](https://github.com/IntersectMBO/cardano-api/pull/843)

- Added `Pretty` instance to `Url`.
  (compatible)
  [PR 839](https://github.com/IntersectMBO/cardano-api/pull/839)

## 10.16.0.0

- Allow providing of actual datum for reference inputs in `TxInsReference`.
  (feature, breaking)
  [PR 814](https://github.com/IntersectMBO/cardano-api/pull/814)

- Add `toMap` synonym for `unUTxO`, `resolveTxIn` synonym for `lookup` functions to `Cardano.Api.Tx.UTxO`.
  (compatible)
  [PR 832](https://github.com/IntersectMBO/cardano-api/pull/832)

- Deprecate `makeShelleyTransactionBody`
  (breaking, refactoring)
  [PR 835](https://github.com/IntersectMBO/cardano-api/pull/835)

- Add constraints to EraCommonConstraints
  (compatible, refactoring)
  [PR 831](https://github.com/IntersectMBO/cardano-api/pull/831)

- Fix an autobalancing error needlessly thrown when there is no change in the transaction. Add property test for autobalancing.
  (bugfix, test)
  [PR 829](https://github.com/IntersectMBO/cardano-api/pull/829)

- - Deprecate `shelleyToAlonzoEraToShelleyToBabbageEra`, `alonzoEraOnwardsToMaryEraOnwards`, `babbageEraOnwardsToMaryEraOnwards`, `babbageEraOnwardsToAlonzoEraOnwards`.
  - Add `asType` for easier obtaining of `HasTypeProxy` proxies
  - Remove unneded `AsType a` functions' arguments
  - Remove some `ByronEra`-related dead code.
  (breaking, refactoring)
  [PR 825](https://github.com/IntersectMBO/cardano-api/pull/825)

- Implement Cip129 class. This type class captures the bech32 encoding modification that allows identification of various governance credentials and governance action ids.
  (feature)
  [PR 778](https://github.com/IntersectMBO/cardano-api/pull/778)

## 10.15.0.0

- Removed Babbage era from `Experimental` API together with `babbageEraOnwardsToEra` function.
  (breaking)
  [PR 828](https://github.com/IntersectMBO/cardano-api/pull/828)

- Improved autobalancing errors when change has no lovelace
  `checkMinUTxOValue` has its arguments flipped.
  (breaking, refactoring)
  [PR 816](https://github.com/IntersectMBO/cardano-api/pull/816)

- Added support for stake pool extended keys
  (feature)
  [PR 781](https://github.com/IntersectMBO/cardano-api/pull/781)

- Exposed `substituteExecutionUnits` & `handleExUnitsErrors` from `Cardano.Api.Internal.Fees` module
  (compatible)
  [PR 820](https://github.com/IntersectMBO/cardano-api/pull/820)

- Fixed CBOR codecs for Proposal
  (compatible, maintenance)
  [PR 823](https://github.com/IntersectMBO/cardano-api/pull/823)

## 10.14.1.0

- Update consensus to 0.26
  (feature, compatible)
  [PR 821](https://github.com/IntersectMBO/cardano-api/pull/821)

## 10.14.0.0

- Update to Plutus 1.45
  (feature, compatible)
  [PR 818](https://github.com/IntersectMBO/cardano-api/pull/818)

- Implement the changes required for UTxO-HD. See the documentation https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/Overview.
  - Augment `Cardano.Api.Internal.LedgerState.LedgerState` with Cardano `LedgerTables`.
  - Require `SingI` for `QueryFootprint`s in Consensus queries
  (feature, breaking)
  [PR 774](https://github.com/IntersectMBO/cardano-api/pull/774)

- Better reporting of negative balance in transaction balancing. Remove redundant `Either` from `evaluateTransactionExecutionUnits` and `evaluateTransactionExecutionUnitsShelley` signatures.
  (breaking, refactoring, bugfix)
  [PR 799](https://github.com/IntersectMBO/cardano-api/pull/799)

- Add `outputs`, `fromShelleyUTxO` and `toShelleyUTxO` functions to `Cardano.Api.Tx.UTxO`.
  (compatible)
  [PR 812](https://github.com/IntersectMBO/cardano-api/pull/812)

- New `ToJSON (Consensus.ChainDepState (ConsensusProtocol era))` constraints for shelley based eons.
  New constraints for `ShelleyBasedEra`:
  * FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  * IsCardanoEra era
  * IsShelleyBasedEra era
  (feature)
  [PR 806](https://github.com/IntersectMBO/cardano-api/pull/806)

## 10.13.1.0

- Fix `toAnyWitness` to not ignore simple scripts
  Update `getVersion` to retrieve the highest protocol version given an era
  (compatible, bugfix)
  [PR 805](https://github.com/IntersectMBO/cardano-api/pull/805)

## 10.13.0.0

- Sort transaction fields in CBOR representation
  (feature, compatible)
  [PR 785](https://github.com/IntersectMBO/cardano-api/pull/785)

- Fix bug in the construction of the redeemer pointer map.
  (breaking, bugfix)
  [PR 800](https://github.com/IntersectMBO/cardano-api/pull/800)

- Removed `createPreviousGovernanceActionId`. Use `GovPurposeId . createGovernanceActionId` instead.
  Export `GovPurposeId` through `Cardano.Api.Ledger`.
  (breaking, refactoring)
  [PR 797](https://github.com/IntersectMBO/cardano-api/pull/797)

- Remove unused ledger types' wrappers: `GovernanceActionId` and `Voter`. Use `GovActionId` and `Voter` from ledger instead.
  (breaking, refactoring)
  [PR 796](https://github.com/IntersectMBO/cardano-api/pull/796)

- Delegate decision on supported versions for queries to Consensus.
  (feature)
  [PR 790](https://github.com/IntersectMBO/cardano-api/pull/790)

- Adding missing `fromCtxUTxOTxOut` export
  (compatible)
  [PR 794](https://github.com/IntersectMBO/cardano-api/pull/794)

## 10.12.0.0

- Bumped ledger and dependencies for node 10.3 release.
  (breaking)
  [PR 758](https://github.com/IntersectMBO/cardano-api/pull/758)
    * Removed `queryProtocolParametersUpdate` and the use of parameterised crypto (`EraCrypto c`, this enables many other data types to become mono-morphic over `StandardCrypto`)
    * Added `queryStakePoolDefaultVote` and `queryLedgerConfig`

- New witness api
  (feature, compatible, refactoring)
  [PR 763](https://github.com/IntersectMBO/cardano-api/pull/763)

- Fix inputSet to be parameterized on the era
  (breaking, bugfix)
  [PR 788](https://github.com/IntersectMBO/cardano-api/pull/788)

- Remove the ProtocolParameters type, that has been deprecated for a while
  (breaking)
  [PR 729](https://github.com/IntersectMBO/cardano-api/pull/729)

## 10.11.1.0

- Add missing `CastVerificationKeyRole StakePoolExtendedKey StakePoolKey` instance
  (compatible)
  [PR 782](https://github.com/IntersectMBO/cardano-api/pull/782)

## 10.11.0.0

- Added support for generating mnemonics and for deriving payment and stake keys from mnemonics.
  (feature)
  [PR 678](https://github.com/IntersectMBO/cardano-api/pull/678)

- Add `fromCtxUTxOTxOut`, inverse of `toCtxUTxOTxOut`
  (compatible)
  [PR 770](https://github.com/IntersectMBO/cardano-api/pull/770)

- Make 1-1 relationship of witness and policy ID in TxMintValue instead of 1-*
  Remove exports: `parseValue`, `ParserValueRole`
  Add new type `PolicyAssets` representing minted assets within a single PolicyId
  Add `mkTxMintValue` helper function
  (breaking, refactoring)
  [PR 776](https://github.com/IntersectMBO/cardano-api/pull/776)

- Add Key instance for StakePoolExtendedKey
  (feature, compatible)
  [PR 777](https://github.com/IntersectMBO/cardano-api/pull/777)

- Add missing certificates in compatible transaction building in eras after Shelley and prior to Alonzo
  (bugfix)
  [PR 775](https://github.com/IntersectMBO/cardano-api/pull/775)

- Added `HasTextEnvelope` instance for `EraHistory`
  (feature, compatible)
  [PR 771](https://github.com/IntersectMBO/cardano-api/pull/771)

- Define `fromList` and `toList` in `Cardano.Api.UTxO` module.
  (compatible)
  [PR 767](https://github.com/IntersectMBO/cardano-api/pull/767)

## 10.10.0.0

- Include deserialisation of stake keys in deserialiseAnyVerificationKeyTextEnvelope
  (feature, compatible)
  [PR 757](https://github.com/IntersectMBO/cardano-api/pull/757)

- Fixed a bug that caused balancing algorithm to drop unwitnessed government actions
  (bugfix)
  [PR 765](https://github.com/IntersectMBO/cardano-api/pull/765)

- Modify `estimateTransactionKeyWitnessCount` to estimate simple scripts too
  (bugfix)
  [PR 755](https://github.com/IntersectMBO/cardano-api/pull/755)

- Split compatible transaction building into separate building and signing functions.
  Rename `Cardano.Api.Internal.Tx.Compatible` to `Cardano.Api.Internal.Compatible.Tx`.
  (breaking)
  [PR 750](https://github.com/IntersectMBO/cardano-api/pull/750)

- Fixed wrong type of space in cardano-api's change log
  (bugfix)
  [PR 754](https://github.com/IntersectMBO/cardano-api/pull/754)

## 10.9.0.0

- Re-export `DebugPlutusFailure` and `renderDebugPlutusFailure`.
  (compatible)
  [PR 715](https://github.com/IntersectMBO/cardano-api/pull/715)

- Add `Cardano.Api.Tx.UTxO` module for common UTxO operations.
  (feature, compatible)
  [PR 710](https://github.com/IntersectMBO/cardano-api/pull/710)

- Removed `serialiseTxLedgerCddl` and `deserialiseTxLedgerCddl`, and updated `writeTxFileTextEnvelopeCddl` to use new format.
  (breaking)
  [PR 746](https://github.com/IntersectMBO/cardano-api/pull/746)

- Upgrade ouroboros-consensus-diffusion to >=0.19 && <0.21
  (compatible)
  [PR 751](https://github.com/IntersectMBO/cardano-api/pull/751)

- Update TxProposalProcedures type to make invalid states irrepresentable.
  (breaking, refactoring)
  [PR 726](https://github.com/IntersectMBO/cardano-api/pull/726)

- Add function `collectPlutusScriptHashes` to collect script hashes needed to validate a given transaction
  (feature)
  [PR 735](https://github.com/IntersectMBO/cardano-api/pull/735)

- Fix transaction autobalancing when deregistering a credential
  (bugfix)
  [PR 718](https://github.com/IntersectMBO/cardano-api/pull/718)

## 10.8.0.0

- Add QueryFuturePParams
  (feature, compatible)
  [PR 739](https://github.com/IntersectMBO/cardano-api/pull/739)

- Re-export `getBlockTxs`
  (compatible, bugfix)
  [PR 738](https://github.com/IntersectMBO/cardano-api/pull/738)

- Change a representation of witnesses in transaction's certificates to an ordered map where a certificate is the key.
  (breaking, bugfix)
  [PR 734](https://github.com/IntersectMBO/cardano-api/pull/734)

- Add support for the ratify-state query
  (feature, compatible)
  [PR 737](https://github.com/IntersectMBO/cardano-api/pull/737)

## 10.7.0.0

- Deprecate patterns, to lower entry bar knowledge to this codebase
  (breaking, refactoring)
  [PR 733](https://github.com/IntersectMBO/cardano-api/pull/733)

- Introduce new type `PlutusScriptInEra` and fix the double cbor encoding plutus script bug
  Resolves: https://github.com/IntersectMBO/cardano-api/issues/685
  (bugfix)
  [PR 720](https://github.com/IntersectMBO/cardano-api/pull/720)

- Shelley: export {to,from}AlonzoLanguage (Plutus script language conversions)
  (compatible)
  [PR 731](https://github.com/IntersectMBO/cardano-api/pull/731)

- Expose GovActionState from Ledger
  (feature, compatible)
  [PR 730](https://github.com/IntersectMBO/cardano-api/pull/730)

- Deprecate some patterns, remove deprecated ones. See the deprecation stanzas for migration guidance.
  (compatible, refactoring)
  [PR 728](https://github.com/IntersectMBO/cardano-api/pull/728)

## 10.6.0.0

- Downgrade plutus version to 1.37
  (bugfix)
  [PR 727](https://github.com/IntersectMBO/cardano-api/pull/727)

- Added schema checking functionality for DRep registration, DRep update, and GovAction metadata, based on CIP-0100, CIP-0108, and CIP-0119. Also add functionality to check whether a certificate is for DRep registration or updating.
  (feature, compatible, test)
  [PR 713](https://github.com/IntersectMBO/cardano-api/pull/713)

- Added `GetBigLedgerPeerSnapshot` block query
  (breaking)
  [PR 521](https://github.com/IntersectMBO/cardano-api/pull/521)

- Add certificates support in `createCompatibleSignedTx`.
  (feature, breaking)
  [PR 691](https://github.com/IntersectMBO/cardano-api/pull/691)

- Integrate `typed-protocols`, `ouroboros-network`, `cardano-ledger` and `ouroboros-consensus`
  (breaking, feature)
  [PR 687](https://github.com/IntersectMBO/cardano-api/pull/687)

- Export genTxOutByron
  (compatible)
  [PR 703](https://github.com/IntersectMBO/cardano-api/pull/703)

## 10.5.0.0

- Don't export the ledger's `coerceKeyRole` function anymore, export RewardAccount
  (breaking)
  [PR 708](https://github.com/IntersectMBO/cardano-api/pull/708)

- Adds `modTxTotalCollateral`, `modTxReturnCollateral`, `modTxFee`, `modTxValidityLowerBound`, `modTxValidityUpperBound`, `modTxMetadata`, `modTxAuxScripts`, `modTxWithdrawals`, `modTxCertificates`, `modTxUpdateProposal`, `modTxScriptValidity`, `modTxMintValue` functions for modifying `TxBodyContent`. Adds `addTxMintValue` and `subtractTxMintValue`.
  (compatible)
  [PR 706](https://github.com/IntersectMBO/cardano-api/pull/706)

- Add TxBodyModifier functions: `addTxIns`, `modTxInsCollateral`, `addTxInsCollateral`, `addTxInCollateral`, `modTxInsReference`, `addTxInsReference`, `addTxInReference`, `addTxOuts`, `modTxExtraKeyWits`, `addTxExtraKeyWits`.
  (compatible)
  [PR 704](https://github.com/IntersectMBO/cardano-api/pull/704)

## 10.4.0.0

- Integrate queryProposals (GetProposals)
  (feature)
  [PR 684](https://github.com/IntersectMBO/cardano-api/pull/684)

- Export HasKeyRole's coerceKeyRole function
  (compatible)
  [PR 699](https://github.com/IntersectMBO/cardano-api/pull/699)

- Exposed `Language`, `Plutus`, `Script`, `getNativeScript`, `languageToText`, `plutusBinary`, `plutusScriptLanguage`, `serializeAsHexText`, `showTimelock` in `Cardano.Api.Ledger`, and `AlonzoEraOnwardsConstraints` in `Cardano.Api`
  (feature)
  [PR 689](https://github.com/IntersectMBO/cardano-api/pull/689)

- Export the Convert class
  (compatible)
  [PR 696](https://github.com/IntersectMBO/cardano-api/pull/696)

- fromProposalProcedure: return StakeCredential
  (breaking)
  [PR 692](https://github.com/IntersectMBO/cardano-api/pull/692)

- Export GovActionIx from the ledger
  (compatible)
  [PR 688](https://github.com/IntersectMBO/cardano-api/pull/688)

- We introduce the `Convert` type class as an alternative to cardano-ledger's `Inject` typeclass.
  While `Inject` is more general, `Convert` is specifically designed for transformations between era-indexed types,
  making the intent clearer at call sites where we're converting between eons.
  (feature)
  [PR 690](https://github.com/IntersectMBO/cardano-api/pull/690)

## 10.3.0.0

- Add `sbeToSimpleScriptLanguageInEra`, `getScriptWitnessScript`, `getScriptWitnessReferenceInput`, `getScriptWitnessReferenceInputOrScript` function
  Refactor `TxMintValue` to better represent minting state
  Propagate `IsPlutusLanguage` constraint to `ScriptLanguage lang`, `AnyPlutusScriptVersion`, `Script lang` and `ScriptWitness witctx era` types.
  Remove `Maybe ScriptHash` from `PReferenceScript` and `SReferenceScript`.
  (breaking, refactoring)
  [PR 663](https://github.com/IntersectMBO/cardano-api/pull/663)

- Improve plutus script failure error
  (feature)
  [PR 683](https://github.com/IntersectMBO/cardano-api/pull/683)

- Remove experimental code exposure in `Cardano.Api` non-experimental modules
  (breaking)
  [PR 681](https://github.com/IntersectMBO/cardano-api/pull/681)

- Deprecate eons conversion functions like `conwayEraOnwardsToBabbageEraOnwards`. Add [`Inject`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-BaseTypes.html#t:Inject) instances for eon conversions. See the PR description for migration aid.
  (compatible, refactoring)
  [PR 636](https://github.com/IntersectMBO/cardano-api/pull/636)

## 10.2.0.0

- ValueParser: rename publicly exposed function names to indicate they are parsers

  To adapt: prefix old function name by `parse` and everything should compile again.
  (breaking)
  [PR 674](https://github.com/IntersectMBO/cardano-api/pull/674)

- Parameterize Value parser on role of the Value being parsed: transaction output or minting policy
  (breaking)
  [PR 666](https://github.com/IntersectMBO/cardano-api/pull/666)

- Add function to extract anchor data from a certificate
  (feature)
  [PR 664](https://github.com/IntersectMBO/cardano-api/pull/664)

- Export the Committee record from the ledger, for use in the CLI
  (compatible)
  [PR 669](https://github.com/IntersectMBO/cardano-api/pull/669)

- Exposed functions and types from `ouroboros-*` required by `cardano-cli`.
  (compatible)
  [PR 667](https://github.com/IntersectMBO/cardano-api/pull/667)

- Rename `TxOutDatumInTx` datum to `TxOutSupplementalDatum`
  (breaking)
  [PR 662](https://github.com/IntersectMBO/cardano-api/pull/662)

- Exposed `GovAction` and `Constitution` types and constructors required by hash check in `transaction build` in `cardano-cli`
  (feature)
  [PR 661](https://github.com/IntersectMBO/cardano-api/pull/661)

## 10.1.0.0

- Use correct stake deregistration certs in Conway
  Require 400,000 deposit when registering keys in `defaultShelleyGenesis`.
  (bugfix)
  [PR 657](https://github.com/IntersectMBO/cardano-api/pull/657)

- Expose transitive dependencies from `cardano-ledger` needed by `cardano-cli`
  Breaking change: `Cardano.Api.Ledger` no longer exports Byron's `Tx` but it is now exported in `Cardano.Api.Byron`
  (breaking)
  [PR 656](https://github.com/IntersectMBO/cardano-api/pull/656)

- Fix datum conversion in 'fromLedgerTxOuts' when using 'fromAlonzoTxOut'
  (bugfix)
  [PR 620](https://github.com/IntersectMBO/cardano-api/pull/620)

## 10.0.0.0

- Bump ouroboros-consensus to 0.21, ouroboros-consensus-cardano to 0.20, ouroboros-consensus-protocol to 0.9.0.2, ouroboros-consensus-diffusion to 0.18, ouroboros-network-api to 0.10, plutus-ledger-api to 1.36, plutus-core to 1.36, and cardano-ledger-binary to 1.3.
    * Stop supplying protocol version to Globals
  (breaking)
  [PR 643](https://github.com/IntersectMBO/cardano-api/pull/643)

- Export an additional symbol
  (compatible)
  [PR 652](https://github.com/IntersectMBO/cardano-api/pull/652)

## 9.4.0.0

- Expose AnyVote type
  (feature, compatible)
  [PR 649](https://github.com/IntersectMBO/cardano-api/pull/649)

- Handle plutus related ledger events in Conway
  (feature, compatible)
  [PR 650](https://github.com/IntersectMBO/cardano-api/pull/650)

- Exposed functions and types from cardano-ledger-alonzo, and cardano-ledger-byron required by cardano-cli.
  Breaking changes:
    * `Address` type and its constructor `ByronAddress` are no longer exported by `Cardano.Api.Byron`, but they are both exported by `Cardano.Api` now, together with `ShelleyAddress` constructor.
    * The types `TxId`, `TxIn`, `TxOut`, and `TxIx`, with their constructors are no longer exported by `Cardano.Api.Byron`.
  (breaking, refactoring)
  [PR 647](https://github.com/IntersectMBO/cardano-api/pull/647)

- Add voting to simple tx interface
  (feature, breaking)
  [PR 648](https://github.com/IntersectMBO/cardano-api/pull/648)

- This module exposes `createCompatibleSignedTx` which is intended to be used in testing only. It allows the creation of simple unbalanced transactions that can submit protocol updates in any era.
  (feature)
  [PR 644](https://github.com/IntersectMBO/cardano-api/pull/644)

- Add new field 'inlineDatumRaw' to TxOut ToJSON instance

  It contains the raw CBOR for any inline datum.

  When building applications that need to spend from a script UTxO which has a datum attached,
  that off-chain code needs access to the raw Datum for evaluating the transaction and calculate fees.
  (feature, compatible)
  [PR 632](https://github.com/IntersectMBO/cardano-api/pull/632)

- Make `IsXXXBasedEra` a class hierarchy
  (compatible)
  [PR 641](https://github.com/IntersectMBO/cardano-api/pull/641)

- Allow next leadership-schedule at 4k/f
  (bugfix)
  [PR 639](https://github.com/IntersectMBO/cardano-api/pull/639)

- Introduce supplementary datums
  (feature)
  [PR 640](https://github.com/IntersectMBO/cardano-api/pull/640)

- Added new types to CDDL and added test for forward compatibility of `deserialiseTxLedgerCddl`
  (bugfix, test)
  [PR 634](https://github.com/IntersectMBO/cardano-api/pull/634)

- Fix collateral balancing when building transaction
  (compatible, bugfix)
  [PR 631](https://github.com/IntersectMBO/cardano-api/pull/631)

- Removes the dependency to `optparse-applicative-fork`, as it is unused internally and not useful for users.
  (breaking, refactoring)
  [PR 635](https://github.com/IntersectMBO/cardano-api/pull/635)

- Add export `fromLedgerUTxO` to `Cardano.Api.
  (feature, compatible)
  [PR 627](https://github.com/IntersectMBO/cardano-api/pull/627)

## 9.3.0.0

- Upgrade `cardano-ledger-*`, `ouroboros-consensus-cardano`, `ouroboros-network-api`, `plutus-core` and `plutus-ledger-api`.
  (feature, breaking)
  [PR 610](https://github.com/IntersectMBO/cardano-api/pull/610)

- Fix current treasury value in transaction building: default to `Nothing` instead of `0`.
  Experimental API: remove redundant type families and functions #625
  (breaking, bugfix)
  [PR 625](https://github.com/IntersectMBO/cardano-api/pull/625)

- Deprecate `valueFromList` and valueToList. Add `IsList Value` instance.
  Fix fee estimation when autobalancing assets minted in the transaction.
  (breaking, bugfix)
  [PR 622](https://github.com/IntersectMBO/cardano-api/pull/622)

- Include datums and redeemers in makeUnsignedTx
  (bugfix)
  [PR 623](https://github.com/IntersectMBO/cardano-api/pull/623)

- Deprecate createAndValidateTransactionBody. Use createTransactionBody instead.
  (breaking)
  [PR 597](https://github.com/IntersectMBO/cardano-api/pull/597)

- Introduce new `newtype UnsignedTx` type which aims to eventually replace `data TxBody`
  Introduce new `data Era` type which reduces the number of eras cardano-api exposes. It is only concerned with mainnet and the upcoming era
  Update experimental api and propagate
  `BalancedTxBody` now returns `UnsignedTx` which is a wrapped cardano-ledger `Tx`.
  (feature, breaking)
  [PR 604](https://github.com/IntersectMBO/cardano-api/pull/604)

- Fix datum conversion in 'fromShelleyTxOut' when using 'ShelleyBasedEraAlonzo' as input
  (bugfix)
  [PR 613](https://github.com/IntersectMBO/cardano-api/pull/613)

## 9.2.0.0

- Add `Lovelace` as a type synonym to `Coin`
  (compatible)
  [PR 614](https://github.com/IntersectMBO/cardano-api/pull/614)

- `TxInsReference` is no longer parameterised by `build`.
  New functions `mkFeatured`, `setTxProposalProcedures`, `setTxVotingProcedures`, `mkTxVotingProcedures`, `mkTxProposalProcedures`, `convProposalProcedures`, and `buildTxWithToMaybe`.
  New generator `genValidTxBody`.
  Fix missing script proposals in transaction building [#594](https://github.com/IntersectMBO/cardano-api/issues/594).
  (breaking, refactoring, bugfix, test)
  [PR 602](https://github.com/IntersectMBO/cardano-api/pull/602)

- Exposed `querySPOStakeDistribution` query
  (feature)
  [PR 598](https://github.com/IntersectMBO/cardano-api/pull/598)

- Modify TxCertificates allow multiple script witnesses for a single stake credential
  (breaking, bugfix)
  [PR 595](https://github.com/IntersectMBO/cardano-api/pull/595)

## 9.1.0.0

- Export metadataValueFromJsonNoSchema
  (feature, compatible)
  [PR 591](https://github.com/IntersectMBO/cardano-api/pull/591)

- Make ScriptDatum in spending scripts optional in accordance with CIP-0069
  (feature, compatible)
  [PR 575](https://github.com/IntersectMBO/cardano-api/pull/575)

- Update substituteExecutionUnits to include proposal and vote script witnesses
  (feature, breaking)
  [PR 587](https://github.com/IntersectMBO/cardano-api/pull/587)

- Fix reading of Plutus V2 cost models with different lengths in AlonzoGenesis in different eras
  (compatible, bugfix)
  [PR 564](https://github.com/IntersectMBO/cardano-api/pull/564)

- Export queryAccountState
  (compatible)
  [PR 588](https://github.com/IntersectMBO/cardano-api/pull/588)

- Add `IsAllegraBasedEra`, `IsAlonzoBasedEra`, `IsBabbageBasedEra`, `IsConwayBasedEra`, `IsMaryBasedEra` type classes.
  Add `ToAlonzoScript` and `HasScriptLanguageInEra` type classes.
  (feature, compatible)
  [PR 585](https://github.com/IntersectMBO/cardano-api/pull/585)

## 9.0.0.0

- - Remove redundant era conversion functions. Use `toCardanoEra` instead.
  - Add IO Exception handling to consensus query execution.
  - Refactor Cardano.Api.Convenience.Query to return `ExceptT e IO a` instead of `IO (Either e a)`
  (breaking, refactoring)
  [PR 566](https://github.com/IntersectMBO/cardano-api/pull/566)

## 8.49.0.0

- Make the query used by the CLI's `transaction build` return the current treasury value, so that command to do treasury donation doesn't require the user to pass it. Corresponding CLI PR: https://github.com/IntersectMBO/cardano-cli/pull/778
  (feature, breaking)
  [PR 557](https://github.com/IntersectMBO/cardano-api/pull/557)

- Small improvements to queries
  (breaking, refactoring)
  [PR 559](https://github.com/IntersectMBO/cardano-api/pull/559)

## 8.48.0.1

- Deserialize `ouroboros-consensus`'s `PoolDistr` instead of `cardano-ledger`'s `PoolDist` to maintain backwards compatibility with the `PoolDistr` query. This is needed because `cardano-ledger`'s `PoolDistr` datatype has changed.
  (compatible)
  [PR 562](https://github.com/IntersectMBO/cardano-api/pull/562)

## 8.48.0.0

- - Updated dependencies:
    - `cardano-ledger`
    - `ouroboros-consensus`
    - `ouroboros-network`
    - `plutus`
   - Replaced `Cardano.Api.Fees.TransactionValidityError.TransactionValidityTranslationError` with `Cardano.Api.Fees.ScriptExecutionError.ScriptErrorTranslationError`
   - Concretized `toShelleyMultiSig` and `fromShelleyMultiSig` to `ShelleyEra`.
  (feature, breaking)
  [PR 552](https://github.com/IntersectMBO/cardano-api/pull/552)

- evaluateTransactionExecutionUnitsShelley: return logs that are useful for debugging
  (breaking)
  [PR 555](https://github.com/IntersectMBO/cardano-api/pull/555)

- Export Target type from ouroboros-network, to be more convenient to users
  (compatible)
  [PR 448](https://github.com/IntersectMBO/cardano-api/pull/448)

## 8.47.0.0

- Add MuxError handling in `FoldBlocksError`. Rename `LedgerStateCondition` to `ConditionResult`.
  (breaking, refactoring)
  [PR 548](https://github.com/IntersectMBO/cardano-api/pull/548)

- Prepare deprecation of ProtocolParameters
  (compatible, documentation)
  [PR 547](https://github.com/IntersectMBO/cardano-api/pull/547)

- TxBodyContent: support treasury donations
  (breaking)
  [PR 543](https://github.com/IntersectMBO/cardano-api/pull/543)

- Added deprecation warning to function `serialiseTxLedgerCddl`
  (breaking)
  [PR 534](https://github.com/IntersectMBO/cardano-api/pull/534)

## 8.46.0.0

- - Updated `cardano-ledger`, `ouroboros-consensus` and `plutus` packages.
  - Added `FailT` dependency.
  - Updated `conwayGenesisDefaults` and `alonzoGenesisDefaults`.
  - Changed `CostModel` to use `Int64` instead of `Integer`.
  - Fixed `ProtocolParameters` golden test to account for the `Integer` -> `Int64` change in the `CostModel`.
  (breaking, test)
  [PR 523](https://github.com/IntersectMBO/cardano-api/pull/523)

## 8.45.2.0

- Update estimateBalancedTxBody to account for required deposits due to
  governance proposals and stake registration certificates
  (feature, compatible, improvement)
  [PR 527](https://github.com/IntersectMBO/cardano-api/pull/527)

## 8.45.1.0

- Adding `MinFeeRefScriptCostPerByte` to Conway PParams
  (feature)
  [PR 524](https://github.com/IntersectMBO/cardano-api/pull/524)

- Added a golden test for ProtocolParameters, added three property-based tests for comparing ProtocolParameters and PParams for the different eras, and fixed the implementation of `toAlonzoPParams` function.
  (improvement)
  [PR 457](https://github.com/IntersectMBO/cardano-api/pull/457)

## 8.45.0.0

- Sort metadata keys for no-schema json for canonical CBOR
  (breaking, bugfix)
  [PR 517](https://github.com/IntersectMBO/cardano-api/pull/517)

- Implement estimateBalancedTxBody and estimateOrCalculateBalancedTxBody
  (feature, compatible)
  [PR 511](https://github.com/IntersectMBO/cardano-api/pull/511)

- Add support for CC keys in the SomeAddressVerificationKey type
  (feature, breaking)
  [PR 514](https://github.com/IntersectMBO/cardano-api/pull/514)

## 8.44.0.0

- - Add `getReferenceInputsSizeForTxIds` function.
  - Bump `cardano-ledger` and `ouroboros-consensus` bounds. In particular, the Conway genesis parameter `ucppMinFeeRefScriptCostPerByte` needs to be set to the correct value after benchmarking.
  - Add an extra parameter to `evaluateTransactionFee`, the reference script size, which the Ledger requires to estimate the minimum fee of a transaction.
  (feature, breaking)
  [PR 496](https://github.com/IntersectMBO/cardano-api/pull/496)

- In `Cardano.API.LedgerState`:
  - Use type aliases and patterns from ouroboros-consensus instead of re-defining them.
  - Import entities from the right modules.
  (improvement)
  [PR 503](https://github.com/IntersectMBO/cardano-api/pull/503)

- Replace unsafeMergeVotingProcedures by mergeVotingProcedures, that handles incompatible votes and return an error
  (breaking, bugfix)
  [PR 498](https://github.com/IntersectMBO/cardano-api/pull/498)

## 8.43.0.0

- Undeprecate `evaluateTransactionFee`
  (compatible)
  [PR 493](https://github.com/IntersectMBO/cardano-api/pull/493)

- Re-implement `evaluateTransactionFee` using ledger's `calcMinFeeTx`
  The `evaluateTransactionFee` takes an additional `utxo` parameter and no longer takes `byronwitcount`.
  (breaking)
  [PR 490](https://github.com/IntersectMBO/cardano-api/pull/490)

## 8.42.0.0

- Add support for script-based CC members
  (feature, breaking)
  [PR 489](https://github.com/IntersectMBO/cardano-api/pull/489)

- Bump ouroboros-consensus-diffusion 0.12
  (compatible)
  [PR 488](https://github.com/IntersectMBO/cardano-api/pull/488)

## 8.39.3.0

- ouroboros-consensus-diffusion-0.12
  (compatible)
  [PR 487](https://github.com/IntersectMBO/cardano-api/pull/487)
  [CHaP PR](https://github.com/IntersectMBO/cardano-haskell-packages/pull/704)

## 8.41.0.0

- Add plutus script support when making hot key authorisation certificates
  (feature, breaking)
  [PR 476](https://github.com/IntersectMBO/cardano-api/pull/476)

- Exposed `UpgradeConwayPParams` constructors and type.
  (improvement)
  [PR 485](https://github.com/IntersectMBO/cardano-api/pull/485)

- Exported more realistic default genesis for both Conway and Alonzo, and export DRepState constructors.
  (improvement)
  [PR 482](https://github.com/IntersectMBO/cardano-api/pull/482)

## 8.40.0.0

- Use the ledger's Coin instead of our custom Lovelace type
  (breaking, improvement)
  [PR 475](https://github.com/IntersectMBO/cardano-api/pull/475)

- Remove error `"evaluateTransactionFee: TODO support Byron key witnesses"` in `estimateMinFeeTx`
  (compatible)
  [PR 478](https://github.com/IntersectMBO/cardano-api/pull/478)

- Revert #316 "Remove unused constraint"
  (improvement)
  [PR 472](https://github.com/IntersectMBO/cardano-api/pull/472)

- Expose Block constructors
  (improvement)
  [PR 468](https://github.com/IntersectMBO/cardano-api/pull/468)

- Add `SlotNo` and `BlockNo` parameters to `foldEpochState`'s callback function
  This gives access to the current `SlotNo` and `BlockNo` in a given requested block.
  (feature, breaking)
  [PR 470](https://github.com/IntersectMBO/cardano-api/pull/470)

- Default genesis parameters yielding positive treasury
  (compatible)
  [PR 425](https://github.com/IntersectMBO/cardano-api/pull/425)

- Simplify `EraInEon` to take fewer constraints
  (compatible)
  [PR 316](https://github.com/IntersectMBO/cardano-api/pull/316)

## 8.39.2.0

- Update ouroboros-consensus-0.16, ouroboros-consensus-cardano-0.14, ouroboros-consensus-diffusion-0.11, ouroboros-consensus-protocol-0.7, ouroboros-network-api-0.7
  (compatible)
  [PR 459](https://github.com/IntersectMBO/cardano-api/pull/459)

## 8.39.1.0

- Enable deposit return script addresses
  Enable constitutional scripts
  (bugfix, compatible, feature)
  [PR 456](https://github.com/IntersectMBO/cardano-api/pull/456)

## 8.39.0.0

- Allow `checkLedgerStateCondition` check to run in IO. Rename to `foldEpochState`.
  (feature, breaking)
  [PR 453](https://github.com/IntersectMBO/cardano-api/pull/453)

- Fix Conway script hash mismatch
  (bugfix)
  [PR 452](https://github.com/IntersectMBO/cardano-api/pull/452)

- add types for reexport
  (improvement)
  [PR 420](https://github.com/IntersectMBO/cardano-api/pull/420)

## 8.38.0.2

- Support constructing DRep update certificates
  (feature, compatible)
  [PR 421](https://github.com/IntersectMBO/cardano-api/pull/421)

## 8.38.0.1

- Make it build with ghc-9.8
  (maintenance, compatible)
  [PR 436](https://github.com/IntersectMBO/cardano-api/pull/436)

## 8.38.0.0

- Make committee keys able to sign transactions
  (feature, compatible)
  [PR 441](https://github.com/IntersectMBO/cardano-api/pull/441)

- bech32 prefixes for committee keys hashes were incorrect. This PR fixes that, in accordance with CIP-5: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0005/README.md?plain=1#L111
  (breaking)
  [PR 440](https://github.com/IntersectMBO/cardano-api/pull/440)

- Bump CHaP for cardano node 8.8 release
  Parameterize `TransactionValidityError` and `TxBodyErrorAutoBalance` on era
  Data constructor `TransactionValidityTranslationError` is now parameterized on `ContextError (ShelleyLedgerEra era)`
  `cppPoolRetireMaxEpoch`, `protocolParamPoolRetireMaxEpoch`,  `icCommitteeTermLength` and `protocolUpdatePoolRetireMaxEpoch` records are updated to `Ledger.EpochInterval`
  Records `cppMaxBlockBodySize`, `cppMaxTxSize`, `protocolUpdateMaxBlockBodySize`, `protocolUpdateMaxTxSize` have been updated to `StrictMaybe Word32`
  Records`cppMaxBlockHeaderSize`, `protocolUpdateMaxBlockHeaderSize` has been updated to `StrictMaybe Word16`
  Inhabitants `SuccessfulPlutusScript` and `FailedPlutusScript` of `LedgerEvent` are now parameterized on `NonEmpty PlutusWithContext`
  `queryNodeLocalState` are now `executeLocalStateQueryExpr` parameterized on `Target ChainPoint`
  `ResolvablePointers` data constructor has been updated with associated data types `PlutusPurpose AsIndex (ShelleyLedgerEra era)` and `PlutusPurpose AsItem (ShelleyLedgerEra era)`
  Replace `PrevGovActionId` with `GovPurposeId`
  Implement `TxProposalProcedures` and `TxVotingProcedures` which enables Plutus script witnessing of proposals and votes.
  Remove support for intermediate tx body format.
  (feature, breaking)
  [PR 407](https://github.com/IntersectMBO/cardano-api/pull/407)

- Expose getAnyNewEpochState. Simplify rendering LedgerState errors
  (breaking, improvement)
  [PR 439](https://github.com/IntersectMBO/cardano-api/pull/439)

- Add return value to `checkLedgerStateCondition`
  (breaking)
  [PR 433](https://github.com/IntersectMBO/cardano-api/pull/433)

- Fix that bech32 prefixes for CC keys were incorrect
  (breaking, bugfix)
  [PR 435](https://github.com/IntersectMBO/cardano-api/pull/435)

- Bump hedgehog-extras to 0.6.0.1 to benefit of this fix: https://github.com/input-output-hk/hedgehog-extras/pull/58
  (compatible, test, release)
  [PR 434](https://github.com/IntersectMBO/cardano-api/pull/434)

- Better `MonadError` handling to avoid nesting `ExceptT` in `modifyError`
  (compatible, improvement)
  [PR 431](https://github.com/IntersectMBO/cardano-api/pull/431)

## 8.37.1.0

- Implement checkLedgerStateConditions. This new function gives direct access to the `NewEpochState` which contains the ledger state. It also requires an `EpochNo` upon which it will terminate if the supplied condition is not met.
  (feature, compatible)
  [PR 424](https://github.com/IntersectMBO/cardano-api/pull/424)

- Use MonadError for 'LedgerState'. Add `Show LedgerState`, `IOException` handling in `foldBlocks`
  (compatible, improvement)
  [PR 422](https://github.com/IntersectMBO/cardano-api/pull/422)

- Support signing with drep extended key
  (compatible)
  [PR 419](https://github.com/IntersectMBO/cardano-api/pull/419)

## 8.37.0.0

- Expose CurrentEra and UpcomingEra pattern synonyms
  (feature, breaking, compatible)
  [PR 414](https://github.com/IntersectMBO/cardano-api/pull/414)

- Add renderSafeHashAsHex, which we want to remove from cardano-node and use in cardano-cli
  (feature, compatible)
  [PR 410](https://github.com/IntersectMBO/cardano-api/pull/410)

- Implement Era GADT and IsEra class as an alternative to the existing era handling code
  (feature, compatible)
  [PR 402](https://github.com/IntersectMBO/cardano-api/pull/402)

- Make it possible to merge again, by fixing dead links
  (improvement)
  [PR 411](https://github.com/IntersectMBO/cardano-api/pull/411)

- Expose all ledger events and convert the pattern synonyms to functions so GHC will warn us if we haven't handled a particular ledger event.
  (feature, breaking, improvement)
  [PR 400](https://github.com/IntersectMBO/cardano-api/pull/400)

- split cases in textEnvelopeType
  account for eras after conway using forEraInEon
  (breaking, bugfix)
  [PR 390](https://github.com/IntersectMBO/cardano-api/pull/390)

## 8.36.1.1

- Restore the inclusion of datum hashes in Alonzo era tx bodies
  (bugfix)
  [PR 398](https://github.com/IntersectMBO/cardano-api/pull/398)

## 8.36.1.0

- Expose AnyProposals and AnyRatificationState
  (feature, compatible)
  [PR 395](https://github.com/IntersectMBO/cardano-api/pull/395)

## 8.36.0.0

- New `ToJSON` instance for `TxValidationErrorInCardanoMode`
  (feature)
  [PR 393](https://github.com/IntersectMBO/cardano-api/pull/393)

- Remove `ByronTx` data constructor from `data Tx era`
  Delete Cardano.Api.Eon.ByronEraOnly module
  Remove `TxFeeImplicit` and `TxValidityNoUpperBound`
  (breaking)
  [PR 382](https://github.com/IntersectMBO/cardano-api/pull/382)

## 8.35.0.0

- Remove `renderEra` Use `docToText . pretty` instead. Rename `prettyTo*` functions to `docTo*`.
  (breaking, improvement)
  [PR 387](https://github.com/IntersectMBO/cardano-api/pull/387)

- Expose NewGovernanceProposals and EpochBoundaryRatificationState ledger events in Conway era
  (feature, compatible)
  [PR 388](https://github.com/IntersectMBO/cardano-api/pull/388)

- Guard queries with their respective eras
  (breaking, improvement)
  [PR 386](https://github.com/IntersectMBO/cardano-api/pull/386)

## 8.34.1.0

- Expose `stakeCredentialWitness` function, which returns only stake credentials for the certificates requiring witnessing: delegation and deregistration.
  (compatible, bugfix)
  [PR 383](https://github.com/IntersectMBO/cardano-api/pull/383)

## 8.34.0.0

- Parameterize `createAndValidateTransactionBody` on `ShelleyBasedEra era`
  (breaking)
  [PR 378](https://github.com/IntersectMBO/cardano-api/pull/378)

- Add `QueryStakeVoteDelegatees` to return the vote delegatee associated to a stake credential
  (feature, compatible)
  [PR 367](https://github.com/IntersectMBO/cardano-api/pull/367)

- `ToJSON` instance for `TxValidationError`
  (feature, compatible)
  [PR 379](https://github.com/IntersectMBO/cardano-api/pull/379)

## 8.33.0.0

- `Error` instance for `FileError` instead of `Pretty`
  Make `prettyToText` return strict `Text` and add `prettyToLazyText`
  Export `Doc`
  (feature, breaking)
  [PR 375](https://github.com/IntersectMBO/cardano-api/pull/375)

- Use `Pretty` for rendering errors instead of `Show`
  (breaking, improvement)
  [PR 370](https://github.com/IntersectMBO/cardano-api/pull/370)

- Integrate ledger relocation of Plutus modules.
  Eliminate `Ledger.Coin` wrapper in `MaryValue` uses.
  Update consensus dependencies.
  Use `Ledger.THKD` on `ConwayPParams`.
  (breaking)
  [PR 359](https://github.com/IntersectMBO/cardano-api/pull/359)

## 8.32.0.0

- Export `AllegraEraOnwards`
  (compatible)
  [PR 374](https://github.com/IntersectMBO/cardano-api/pull/374)

- Isolation of Byron era 1/n. Changes made to cardano-api where Byron era was never being called in the first place.
  (breaking)
  [PR 362](https://github.com/IntersectMBO/cardano-api/pull/362)

- Update and export `Test.Gen.Cardano.Api.ProtocolParameters`
  (bugfix, test)
  [PR 369](https://github.com/IntersectMBO/cardano-api/pull/369)

- Fix round trip for empty `Value`. Add tests.
  (bugfix)
  [PR 365](https://github.com/IntersectMBO/cardano-api/pull/365)

## 8.31.0.0

- Use ledger presentation of multi-asset values directly.  Lens to make this uniform over `ShelleyBasedEra`.

  Delete `ByronToAllegraEra`.

  New module `Cardano.Api.Ledger.Lens`.

  Modify `TxOutValue` to have `TxOutValueByron` and `TxOutValueShelleyBased` instead of `TxOutAdaOnly` and `TxOutValue` respectively.  `TxOutValueShelleyBased` now directly uses the ledger type instead of the `Value` type.

  These functions have changed to either `L.Value (ShelleyLedgerEra era)` instead of `Value` or eons or both:
  - `genValue`
  - `genValueDefault`
  - `genValueForMinting`
  (breaking)
  [PR 360](https://github.com/IntersectMBO/cardano-api/pull/360)

- Replace `IsCardanoEra` and `IsShelleyBasedEra` contraints in GADT constructors with `Typeable`
  (breaking, improvement)
  [PR 354](https://github.com/IntersectMBO/cardano-api/pull/354)

- Modify foldBlocks to recurse on ledger events instead of mapping over them. This allows finer grained control over when `foldBlocks` is stopped.
  (optimisation)
  [PR 353](https://github.com/IntersectMBO/cardano-api/pull/353)

- --protocol-(minor|major)-version cannot be changed via create-protocol-parameters-update command in conway
  (breaking)
  [PR 358](https://github.com/IntersectMBO/cardano-api/pull/358)

- Modify foldBlocks to allow the fold to terminate from the accumulator via the FoldStatus type.

  Modify foldBlocks to accumulate the chain tip rather than only immutable
  blocks (blocks that are k blocks away from the tip).

  Add debug mode to foldBlocks which forces it to error with information
  about ledger states, client and server tip, number of requests in flight
  and the current IORef state.
  (feature, breaking)
  [PR 350](https://github.com/IntersectMBO/cardano-api/pull/350)

- Delete types:
  - ByronAndAllegraEraOnwards
  - ByronToMaryEra
  - MaryEraOnly

  Delete functions:
  - allegraEraOnwardsToByronAndAllegraOnwardsEra
  - byronAndAllegraEraOnwardsConstraints
  - byronAndAllegraEraOnwardsToCardanoEra
  - byronToMaryEraConstraints
  - byronToMaryEraToCardanoEra
  - caseByronToMaryOrAlonzoEraOnwards
  - caseMaryEraOnlyOrAlonzoEraOnwards
  - maryEraOnlyConstraints
  - maryEraOnlyToCardanoEra
  - maryEraOnlyToShelleyBasedEra
  (breaking, improvement)
  [PR 355](https://github.com/IntersectMBO/cardano-api/pull/355)

## 8.30.0.0

- Remove uses of `coerceKeyRole`, use asWitness when key role conversion is required
  (feature, breaking)
  [PR 341](https://github.com/IntersectMBO/cardano-api/pull/341)

- Delete `ByronMode` and `ShelleyMode`
  Delete `anyEraInModeToAnyEra`
  Delete `AnyEraInMode`
  Modify to use `CardanoMode` only:
    - `executeQueryAnyMode`
    - `connectToLocalNode`
    - `connectToLocalNodeWithVersion
    - `mkLocalNodeClientParams`
    - `queryNodeLocalState`
    - `submitTxToNodeLocal`
    - `queryTxMonitoringLocal`
    - `getLocalChainTip`
    - `executeLocalStateQueryExpr`
  Deparameterise `LocalNodeClientProtocolsInMode`
  Modify `LocalNodeClientProtocols` to only work in `CardanoMode`.
  Rename `LocalNodeClientProtocolsInMode` to `LocalNodeClientProtocolsInIO`
  Rename `TxValidationErrorInMode` to `TxValidationErrorInCardanoMode`
  Deparameterise `TxValidationErrorInMode`
  Modify to use `CardanoMode` only
  - `determineEra`
  Deparameterise `LocalNodeConnectInfo`
  Deparameterise `LocalTxMonitoringResult`
  Deparameterise `LocalTxMonitoringQuery`
  Deparameterise `TxInMode`
  Modify to use `CardanoMode` only
  - `fromConsensusBlock`
  - `toConsensusBlock`
  - `queryExpr`
  - `getProgress`
  - `getSlotForRelativeTime`
  - `toLedgerEpochInfo`
  - `slotToEpoch`
  - `queryCurrentEpochState`
  - `queryEpoch`
  - `queryDebugLedgerState`
  - `queryGenesisParameters`
  - `queryPoolDistribution`
  - `queryPoolState`
  - `queryProtocolParameters`
  - `queryConstitutionHash`
  - `queryProtocolParametersUpdate`
  - `queryProtocolState`
  - `queryStakeAddresses`
  - `queryStakeDelegDeposits`
  - `queryStakeDistribution`
  - `queryStakePoolParameters`
  - `queryStakePools`
  - `queryStakeSnapshot`
  - `queryUtxo`
  - `queryConstitution`
  - `queryGovState`
  - `queryDRepState`
  - `queryDRepStakeDistribution`
  - `queryCommitteeMembersState`
  Deparameterise `BlockInMode`
  Deparameterise `TxIdInMode`
  Remove `EraInMode` argument in `QueryInEra` constructor
  Delete `ConsensusBlockForMode`
  Modify `TxInMode` to carry `CardanoEra` instead of `EraInMode`
  Deparameterise `BlockInMode`
  Deparameterise `TxIdInMode`
  Deparameterise `EraHistory`
  Modify to not take `EraInMode` argument
  - `queryCurrentEpochState`
  - `queryEpoch`
  - `queryDebugLedgerState`
  - `queryGenesisParameters`
  - `queryPoolDistribution`
  - `queryPoolState`
  - `queryProtocolParameters`
  - `queryConstitutionHash`
  - `queryProtocolParametersUpdate`
  - `queryProtocolState`
  - `queryStakeAddresses`
  - `queryStakeDelegDeposits`
  - `queryStakeDistribution`
  - `queryStakePoolParameters`
  - `queryStakePools`
  - `queryStakeSnapshot`
  - `queryUtxo`
  - `queryConstitution`
  - `queryGovState`
  - `queryDRepState`
  - `queryDRepStakeDistribution`
  - `queryCommitteeMembersState`
  Modify `TxInMode` constructors to remove `EraInMode` arguments from constructors
  Remove `EraInMode` from `TxValidationErrorInCardanoMode` constructors
  Delete `EraInMode`, `eraInModeToEra` and `toEraInMode`
  Remove `ConsensusMode` argument from:
  - `fromConsensusBlock`
  - `fromConsensusTip`
  - `determineEra`
  Delete `determineEraExpr`.  Use `queryCurrentEra` instead.
  Deparameterise `QueryInMode`
  Delete `AnyConsensusModeParams`
  Deparametrise `ConsensusModeParams`
  Remove `ConsensusMode` argument from `EraHistory` constructor
  Delete `EraConsensusModeMismatch` constructor
  Delete `localConsensusMode`, `AnyConsensusMode` and `renderMode`
  Delete `EraConsensusModeMismatch`
  Delete `CardanoMode` and `ConsensusMode`.
  (breaking)
  [PR 342](https://github.com/IntersectMBO/cardano-api/pull/342)

- Switch to use lens and eons for txbody construction
  (improvement)
  [PR 334](https://github.com/IntersectMBO/cardano-api/pull/334)

- Simplify `createTransactionBody`
  (improvement)
  [PR 333](https://github.com/IntersectMBO/cardano-api/pull/333)

- In `Tx`, `ByronTx` now carries additional `ByronEraOnly` witness
  In `TxBody`, `TxBodyByron` now carries additional `ByronEraOnly` witness
  Delete `CardanoEraStyle` because eons solve the same problem more generally
  Delete `cardanoEraStyle`
  (breaking)
  [PR 335](https://github.com/IntersectMBO/cardano-api/pull/335)

## 8.29.0.0

- Updated ledger, consensus and typed-protocol packages
  Adapted ProtocolParameters to the new ProtVer >=8 constraint in  in ppuProtocolVersionL
  Replaced  queryCommitteState with new queryCommitteeMembersState
  Improved  costModel generation for tests
  Adapt to new type HKDNoUpdate f ProtVer of cppProtocolVersion
  Export types needed for querying the committee state
  Adjust to type change of proposalProceduresTxBodyL
  Remove invalidBeforeL and invalidHereAfterL defined in ledger
  (breaking, maintenance)
  [PR 321](https://github.com/IntersectMBO/cardano-api/pull/321)

- Expose `PlutusScriptV3`
  (bugfix)
  [PR 343](https://github.com/IntersectMBO/cardano-api/pull/343)

- Add support for simple scripts in Conway
  (bugfix)
  [PR 344](https://github.com/IntersectMBO/cardano-api/pull/344)

- Delete `ProtocolUTXOCostPerWord` feature
  (breaking)
  [PR 339](https://github.com/IntersectMBO/cardano-api/pull/339)

- Delete `ProtocolUTxOCostPerByteFeature`
  (breaking)
  [PR 340](https://github.com/IntersectMBO/cardano-api/pull/340)

- Add Show instance for FoldBlocksError
  (feature, compatible)
  [PR 338](https://github.com/IntersectMBO/cardano-api/pull/338)

- Split the `txValidityRange` field into two fields.
  New function `allegraEraOnwardsToByronAndAllegraOnwardsEra`
  (breaking, improvement)
  [PR 332](https://github.com/IntersectMBO/cardano-api/pull/332)

- Add support for Plutus V1 in Conway
  (bugfix)
  [PR 323](https://github.com/IntersectMBO/cardano-api/pull/323)

- DRep extended key: add CastVerificationKeyRole
  (compatible)
  [PR 327](https://github.com/IntersectMBO/cardano-api/pull/327)

## 8.28.0.0

- Delete `AlonzoEraOnly`.
  Export `caseMaryEraOnlyOrAlonzoEraOnwards`
  (feature, breaking)
  [PR 330](https://github.com/IntersectMBO/cardano-api/pull/330)

- New `shelleyToAlonzoEraToShelleyToBabbageEra` function
  (feature, compatible)
  [PR 329](https://github.com/IntersectMBO/cardano-api/pull/329)

- Delete unnecessary calls to `shelleyBasedEraConstraints`
  (improvement)
  [PR 325](https://github.com/IntersectMBO/cardano-api/pull/325)

## 8.27.0.0

- Remove `IsShelleyBasedEra` and `IsCardanoEra` from all functions and types.
  Use `ShelleyBasedEra` and `CardanoEra` instead.
  (breaking)
  [PR 313](https://github.com/IntersectMBO/cardano-api/pull/313)

## 8.26.0.0

- Add support for DRep extended keys
  (feature, compatible)
  [PR 320](https://github.com/IntersectMBO/cardano-api/pull/320)

- Parameterize GovernanceAction on era
  (breaking)
  [PR 322](https://github.com/IntersectMBO/cardano-api/pull/322)

- Haddock-document the `case*` functions
  (compatible, improvement)
  [PR 314](https://github.com/IntersectMBO/cardano-api/pull/314)

- New `MaryEraOnly` eon
  New functions:
  - `caseMaryEraOnlyOrAlonzoEraOnwards`
  - `disjointByronEraOnlyAndShelleyBasedEra`
  - `disjointAlonzoEraOnlyAndBabbageEraOnwards`
  Deprecate:
  - `noByronEraInShelleyBasedEra`.  Use `disjointByronEraOnlyAndShelleyBasedEra` instead.
  (feature, breaking)
  [PR 312](https://github.com/IntersectMBO/cardano-api/pull/312)

- Delete `EraCast` type class
  (breaking)
  [PR 308](https://github.com/IntersectMBO/cardano-api/pull/308)

- Fewer constraints in functions
  (compatible)
  [PR 310](https://github.com/IntersectMBO/cardano-api/pull/310)

- Introduce anchor newtypes for drep registration certificate
  (feature, compatible)
  [PR 305](https://github.com/IntersectMBO/cardano-api/pull/305)

- Reduce constraint usage with eons
  (compatible, improvement)
  [PR 299](https://github.com/IntersectMBO/cardano-api/pull/299)

- Delete `EraCast` instance for `Certificate`
  (breaking)
  [PR 307](https://github.com/IntersectMBO/cardano-api/pull/307)

- Add certs to txbody of Conway transactions
  (feature, compatible)
  [PR 306](https://github.com/IntersectMBO/cardano-api/pull/306)

- Add `BabbageEraOnly`
  (feature, compatible)
  [PR 304](https://github.com/IntersectMBO/cardano-api/pull/304)

## 8.25.2.0

- Add support for committee hot key witnesses
  (feature, compatible)
  [PR 300](https://github.com/IntersectMBO/cardano-api/pull/300)

- Delete `withShelleyBasedEraConstraintsForLedger`. Use `shelleyBasedEraConstraints` instead.
  (breaking, improvement)
  [PR 297](https://github.com/IntersectMBO/cardano-api/pull/297)

- Simplify `DebugLedgerState` with eons
  (breaking, improvement)
  [PR 296](https://github.com/IntersectMBO/cardano-api/pull/296)

## 8.25.0.1

- Haddock bug fix
  (compatible bug fix)

## 8.25.0.0

- Rename `AnyEraInEon` to `EraInEon`
  (breaking, improvement)
  [PR 289](https://github.com/IntersectMBO/cardano-api/pull/289)

- Require conway onwards for voting.
  Functions with modified type signatures:
  - `createVotingProcedure`
  - `toVotingCredential`
  - `singletonVotingProcedures`
  (breaking, improvement)
  [PR 293](https://github.com/IntersectMBO/cardano-api/pull/293)

- Add witness committee cold key
  (feature, compatible)
  [PR 292](https://github.com/IntersectMBO/cardano-api/pull/292)

- More use of ReexposeLedger
  (improvement)
  [PR 290](https://github.com/IntersectMBO/cardano-api/pull/290)

## 8.24.0.0

- Expose Conway drep registration certificate anchor
  (breaking, improvement)
  [PR 284](https://github.com/IntersectMBO/cardano-api/pull/284)

- Parameterize `AnyEraInEon`. Add `AnyEon`
  (breaking)
  [PR 287](https://github.com/IntersectMBO/cardano-api/pull/287)

- Add support for conway era protocol parameters.
  Adapted `GenesisCardano` to upstream Ledger/Consensus changes.
  (feature, breaking)
  [PR 270](https://github.com/IntersectMBO/cardano-api/pull/270)

## 8.23.0.0

- New `caseAlonzoOnlyOrBabbageEraOnwards` and `alonzoEraOnlyToAlonzoEraOnwards` functions
  (feature, compatible)
  [PR 282](https://github.com/IntersectMBO/cardano-api/pull/282)

- Delete `AuxScriptsSupportedInEra`.  Use `AllegraEraOnwards` instead.
  (breaking, improvement)
  [PR 273](https://github.com/IntersectMBO/cardano-api/pull/273)

- - Rename `inShelleyBasedEraEonMaybe` to `forShelleyBasedEraInEonMaybe`
  - Rename `inShelleyBasedEraEon` to `forShelleyBasedEraInEon`
  - Rename `maybeEonInShelleyBasedEra` to `forShelleyBasedEraMaybeEon`
  - Rename `eonInShelleyBasedEra` to `inEonForShelleyBasedEra`
  - Rename `maybeEonInEra` to `forEraMaybeEon`
  - Rename `inEraEonMaybe` to `forEraInEonMaybe`
  - New `inEonForEraMaybe` function
  - New `inEonForShelleyBasedEraMaybe` function
  (feature, breaking)
  [PR 281](https://github.com/IntersectMBO/cardano-api/pull/281)

- Remove `CollateralSupportedInEra`.  Use `AlonzoEraOnwards` instead.
  Remove `collateralSupportedInEra`.  Use `inEonForEra` instead.
  (breaking)
  [PR 271](https://github.com/IntersectMBO/cardano-api/pull/271)

- Replace `TxScriptValiditySupportedInEra`.  Use AlonzoEraOnwards instead.
  Delete `txScriptValiditySupportedInCardanoEra`. Use `forEraInEra` instead.
  Delete `txScriptValiditySupportedInShelleyBasedEra`. Use `forEraInEra` instead.
  (breaking)
  [PR 261](https://github.com/IntersectMBO/cardano-api/pull/261)

- Delete `ValidityUpperBoundSupportedInEra`.  Use `ShelleyBasedEra` instead.
  Delete `ValidityNoUpperBoundSupportedInEra`. Use `ByronAndAllegraEraOnwards` instead.
  New `caseByronAndAllegraEraOnwardsOrShelleyEraOnly` function.
  New `ShelleyEraOnly` eon
  New `ByronAndAllegraEraOnwards` eon
  Delete `validityUpperBoundSupportedInEra`.  Use `inEonForEra` instead.
  Delete `validityNoUpperBoundSupportedInEra`.  Use `inEonForEra` instead.
  Delete `IsByronToAllegraEra`.
  Delete `IsByronToMaryEra`.
  Delete `IsByronToAlonzoEra`.
  (breaking, improvement)
  [PR 272](https://github.com/IntersectMBO/cardano-api/pull/272)

- New `AnyEraInEon`.
  Delete:
  * AnyByronEraOnly
  * AnyByronToAllegraEra
  * AnyByronToMaryEra
  * AnyByronToAlonzoEra
  * AnyShelleyToAllegraEra
  * AnyShelleyToMaryEra
  * AnyShelleyToAlonzoEra
  * AnyShelleyToBabbageEra
  * AnyShelleyBasedEra
  * AnyMaryEraOnwards
  * AnyAlonzoEraOnly
  * AnyAlonzoEraOnwards
  * AnyBabbageEraOnwards
  * AnyConwayEraOnwards
  Use `AnyEraInEon` instead.
  (feature, breaking)
  [PR 280](https://github.com/IntersectMBO/cardano-api/pull/280)

- Delete `ValidityLowerBoundSupportedInEra`.  Use `AllegraEraOnwards` instead
  Delete `validityLowerBoundSupportedInEra`.  Use `inEonForEra` or equivalent instead
  (breaking, improvement)
  [PR 279](https://github.com/IntersectMBO/cardano-api/pull/279)

- Delete `TxExtraKeyWitnessesSupportedInEra`.  Use `AlonzoEraOnwards` instead.
  Delete `extraKeyWitnessesSupportedInEra`.  Use `inEonForEra` or related instead.
  (breaking, improvement)
  [PR 278](https://github.com/IntersectMBO/cardano-api/pull/278)

- Delete `TxTotalAndReturnCollateralSupportedInEra`.  Use `BabbageEraOnwards` instead.
  Delete `totalAndReturnCollateralSupportedInEra`.  Use `inEonForEra` or related instead.
  (breaking, improvement)
  [PR 275](https://github.com/IntersectMBO/cardano-api/pull/275)

- Explicit pattern match on all ledger certificates constructors.
  Remove `getIsCardanoEraConstraint`. Use `cardanoEraConstraints` instead.
  (breaking, improvement)
  [PR 277](https://github.com/IntersectMBO/cardano-api/pull/277)

## 8.22.0.0

- A prior refactor accidentally defaulted to `isLeadingSlotsTPraos` regardless of the era
  (compatible, bugfix)
  [PR 274](https://github.com/IntersectMBO/cardano-api/pull/274)

- Fix missing redeemers in certificate deregistration
  (bugfix)
  [PR 268](https://github.com/IntersectMBO/cardano-api/pull/268)

- Delete `WithdrawalsSupportedInEra`.  Use `ShelleyBasedEra` instead.
  Delete `withdrawalsSupportedInEra`.  Use `inEonForEra` instead.
  (breaking, improvement)
  [PR 260](https://github.com/IntersectMBO/cardano-api/pull/260)

- Replace `UpdateproposalSupportedInEra` with `ShelleyToBabbageEra`
  (breaking)
  [PR 258](https://github.com/IntersectMBO/cardano-api/pull/258)

- Delete `TxMetadataSupportedInEra`. Use `ShelleyBasedEra` instead.
  Delete `txMetadataSupportedInEra`.  Use `inEonForEra` instead.
  (breaking, improvement)
  [PR 263](https://github.com/IntersectMBO/cardano-api/pull/263)

- New functions `alonzoEraOnwardsToMaryEraOnwards` and `shelleyToAllegraEraToByronToAllegraEra`
  (feature, compatible)
  [PR 266](https://github.com/IntersectMBO/cardano-api/pull/266)

- Export `getTxBodyContent`
  (feature, compatible)
  [PR 267](https://github.com/IntersectMBO/cardano-api/pull/267)

- Delete `CertificatesSupportedInEra`.  Use `ShelleyBasedEra` instead.
  Delete `certificatesSupportedInEra`.  Use `inEonForEra` instead.
  (breaking)
  [PR 259](https://github.com/IntersectMBO/cardano-api/pull/259)

## 8.21.0.0

- Make ProposeNewCommittee use the appropriate type of key
  (breaking, improvement)
  [PR 264](https://github.com/IntersectMBO/cardano-api/pull/264)

- - Organise eon re-exports from `Cardano.Api`.
  - Export `MaryEraOnwards`
  (feature, compatible)
  [PR 265](https://github.com/IntersectMBO/cardano-api/pull/265)

- - Delete `AdaSupportedInEra`.  Use `ByronToAllegraEra` instead.
  - Delete `MultiAssetSupportedInEra`.  Use `MaryEraOnwards` instead.
  - Delete function `multiAssetSupportedInEra`.  Use `caseByronToAllegraOrMaryEraOnwards` instead.
  - New `ByronToAllegraEra` eon.
  - New `MaryEraOnwards` eon.
  - New functions:
    - `caseByronToAllegraOrMaryEraOnwards`
    - `caseShelleyToAllegraOrMaryEraOnwards`
    - `caseShelleyToMaryOrAlonzoEraOnwards`
    - `shelleyToAllegraEraToByronToAllegraEra`
  (breaking, improvement)
  [PR 254](https://github.com/IntersectMBO/cardano-api/pull/254)

- Delete unused eon type classes:
  - `IsAlonzoEraOnly`
  - `IsAlonzoEraOnwards`
  - `IsBabbageEraOnwards`
  - `IsByronEraOnly`
  - `IsCardanoEra`
  - `IsConwayEraOnwards`
  - `IsShelleyToAllegraEra`
  - `IsShelleyToAlonzoEra`
  - `IsShelleyToBabbageEra`
  - `IsShelleyToMaryEra`
  (breaking)
  [PR 256](https://github.com/IntersectMBO/cardano-api/pull/256)

- Export `ByronEraOnly`
  (feature)
  [PR 255](https://github.com/IntersectMBO/cardano-api/pull/255)

- Update to the pre-commit script, so that it fails on hlint errors
  (improvement)
  [PR 253](https://github.com/IntersectMBO/cardano-api/pull/253)

- Delete `TxFeesExplicitInEra` and `TxFeesImplicitInEra`
  New `ByronEraOnly` feature
  Move `ShelleyBasedEra` into its own module as it is a legitimate feature
  (breaking, improvement)
  [PR 244](https://github.com/IntersectMBO/cardano-api/pull/244)

- - Rename `FeatureInEra` to `Eon`
  - Rename the following modules:
    - `Cardano.Api.Feature.AlonzoEraOnly -> Cardano.Api.Eon.AlonzoEraOnly`
    - `Cardano.Api.Feature.AlonzoEraOnwards -> Cardano.Api.Eon.AlonzoEraOnwards`
    - `Cardano.Api.Feature.BabbageEraOnwards -> Cardano.Api.Eon.BabbageEraOnwards`
    - `Cardano.Api.Feature.ConwayEraOnwards -> Cardano.Api.Eon.ConwayEraOnwards`
    - `Cardano.Api.Feature.ShelleyToAllegraEra -> Cardano.Api.Eon.ShelleyToAllegraEra`
    - `Cardano.Api.Feature.ShelleyToAlonzoEra -> Cardano.Api.Eon.ShelleyToAlonzoEra`
    - `Cardano.Api.Feature.ShelleyToBabbageEra -> Cardano.Api.Eon.ShelleyToBabbageEra`
    - `Cardano.Api.Feature.ShelleyToMaryEra -> Cardano.Api.Eon.ShelleyToMaryEra`
  - Rename the following functions:
    - `inEraFeature` to `inEonEra`
    - `inEraFeature` to `eraInEon`
  - Rename the following functions (conservatively replacing "feature" with "eon" until we have a better naming convention):
    - `inEraFeatureMaybe -> inEraEonMaybe`
    - `maybeFeatureInEra -> maybeEonInEra`
    - `featureInShelleyBasedEra -> eonInShelleyBasedEra`
    - `inShelleyBasedEraFeature -> inShelleyBasedEraEon`
    - `inShelleyBasedEraFeatureMaybe -> inShelleyBasedEraEonMaybe`
    - `maybeFeatureInShelleyBasedEra -> maybeEonInShelleyBasedEra`
  (breaking, improvement)
  [PR 247](https://github.com/IntersectMBO/cardano-api/pull/247)

## 8.20.2.0

- Add JSON instance for `Hash GenesisKey`
  (improvement)
  [PR 249](https://github.com/IntersectMBO/cardano-api/pull/249)

- Support more ledger constraints
  (feature, compatible)
  [PR 248](https://github.com/IntersectMBO/cardano-api/pull/248)

## 8.20.1.0

- Fix typos in some deserialization error messages
  (bugfix)
  [PR 243](https://github.com/IntersectMBO/cardano-api/pull/243)

- Fix DRep Stake and DRep Stake queries for empty lists
  (bugfix)
  [PR 245](https://github.com/IntersectMBO/cardano-api/pull/245)

- Fix querying for dreps in `transaction build` in eras before conway
  (bugfix)
  [PR 240](https://github.com/IntersectMBO/cardano-api/pull/240)

- Fix Query error in QueryStakeDelegDeposits when executing transaction build
  (bugfix)
  [PR 239](https://github.com/IntersectMBO/cardano-api/pull/239)

## 8.20.0.0

- Expose constraints from casing functions
  (feature, compatible)
  [PR 237](https://github.com/IntersectMBO/cardano-api/pull/237)

- New functions:
  * `caseByronOrShelleyBasedEra`
  * `caseShelleyToMaryOrAlonzoEraOnwards`
  * `caseShelleyToAlonzoOrBabbageEraOnwards`
  Renamed `caseShelleyToBabbageAndConwayEraOnwards` to `caseShelleyToBabbageOrConwayEraOnwards`
  (breaking)
  [PR 232](https://github.com/IntersectMBO/cardano-api/pull/232)

- Fix existing test constraints functions.  Type signatures changed on:
  * `shelleyBasedEraTestConstraints`
  * `shelleyToBabbageEraTestConstraints`
  * `conwayEraOnwardsTestConstraints`
  (breaking, bugfix)
  [PR 233](https://github.com/IntersectMBO/cardano-api/pull/233)

- Add the following features:
  * `AlonzoEraOnly`
  * `ShelleyToAllegraEra`
  * `BabbageEraOnwards`
  * `AlonzoEraOnwards`
  * `ShelleyToMaryEra`
  * `ShelleyToAlonzoEra`
  (feature)
  [PR 220](https://github.com/IntersectMBO/cardano-api/pull/220)

- New `caseShelleyToBabbageAndConwayEraOnwards` function
  (feature, compatible)
  [PR 231](https://github.com/IntersectMBO/cardano-api/pull/231)

- Delete deprecated functions and types:
  * `Allegra`
  * `AsAllegra`
  * `AsByron`
  * `AsMary`
  * `AsShelley`
  * `Byron`
  * `Mary`
  * `Shelley`
  * `eitherDeserialiseFromRawBytes`
  * `hashScriptData`
  * `makeStakeAddressPoolDelegationCertificate`
  * `makeTransactionBody`
  * `queryPparams`
  (breaking, improvement)
  [PR 230](https://github.com/IntersectMBO/cardano-api/pull/230)

- Updated to `cardano-ledger-conway-1.18`.
  [PR 227](https://github.com/IntersectMBO/cardano-api/pull/227)

## 8.19.0.0

- New `foldSomeAddressVerification` key function
  (feature, compatible)
  [PR 225](https://github.com/IntersectMBO/cardano-api/pull/225)

- `FeatureInEra` instances for `CardanoEra` and `ShelleyBasedEra`
  (feature, compatible)
  [PR 226](https://github.com/IntersectMBO/cardano-api/pull/226)

- Fix type signature of `queryGenesisParameters` so that it can be queried in any era
  (breaking, bugfix)
  [PR 224](https://github.com/IntersectMBO/cardano-api/pull/224)

## 8.18.0.0

- Fix exception when executing drep queries
  (bugfix)
  [PR 221](https://github.com/IntersectMBO/cardano-api/pull/221)

- Use `newtype` instead of `GADT` for `LedgerProtocolParameters`
  (breaking, improvement)
  [PR 218](https://github.com/IntersectMBO/cardano-api/pull/218)

- Changes:
  * Deleted `TxGovernanceActions`
  * New generators: `genProposals`, `genProposal`, `genVotingProcedures`
  * New test constraints functions: `shelleyBasedEraTestConstraints`, `shelleyToBabbageEraTestConstraints`, `conwayEraOnwardsTestConstraints`
  * New era functions: `inEraFeatureMaybe`, `inShelleyBasedEraFeatureMaybe`, `maybeFeatureInShelleyBasedEra`
  (breaking, improvement)
  [PR 217](https://github.com/IntersectMBO/cardano-api/pull/217)

- Replace ProtocolParameters usage with ledger's PParams
  (breaking)
  [PR 214](https://github.com/IntersectMBO/cardano-api/pull/214)

## 8.17.0.0

- Delete `TxVotes` and `VotingEntry` and use `VotingProcedures` instead
  (breaking)
  [PR 209](https://github.com/IntersectMBO/cardano-api/pull/209)

- New functions `emptyVotingProcedures`, `singletonVotingProcedures` and `mergeVotingProcedures`
  (feature, compatible)
  [PR 208](https://github.com/IntersectMBO/cardano-api/pull/208)

- Expose ledger lenses for governance types: `drepExpiryL`, `drepAnchorL`, `drepDepositL`, `csCommitteeCredsL`
  (feature, compatible)
  [PR 206](https://github.com/IntersectMBO/cardano-api/pull/206)

- New `makeStakeAddressAndDRepDelegationCertificate` function
  (feature, compatible)
  [PR 207](https://github.com/IntersectMBO/cardano-api/pull/207)

- Use injective type families to improve type inference
  (compatible)
  [PR 210](https://github.com/IntersectMBO/cardano-api/pull/210)

## 8.16.1.0

- Add `VotingProcedures` type.
  (feature, compatible)
  [PR 204](https://github.com/IntersectMBO/cardano-api/pull/204)

## 8.16.0.0

- Expose following queries from consensus:
    - GetGovState
    - GetDRepState
    - GetDRepStakeDistr
    - GetCommitteeState
    - GetConstitution
  (feature, compatible)
  [PR 196](https://github.com/IntersectMBO/cardano-api/pull/196)

- Update createAnchor to use hashAnchorData
  (feature, breaking)
  [PR 200](https://github.com/IntersectMBO/cardano-api/pull/200)

## 8.15.0.0

- Updating the ledger dependency to cardano-ledger-conway-1.7.0.0:
    Many superficial renamings
    TxVotes carries a map now
    ResolvablePointers now has a different representation than does the ledger
    ProposalNewCommitee requires the old committee's credentials
    The ProposalNewConstitution case of toGovernanceAction was hashing the argument'ByteString, but it was already a hash.
    See temporarilyOptOutOfPrevGovAction
    makeGovernanceActionId was reusing the transaction id as the governance action id, but the types no longer allow that.
    Semigroup oprhan was missing for ConwayPParams
    QueryConstitutionHash phantom type is now more specific
    Cardano.Ledger.Api no longer export EraCrypto
    Introduced (internal) pattern synonyms for scripts to coverup a change in the corresponding ledger types.
  (feature, breaking)
  [PR 179](https://github.com/IntersectMBO/cardano-api/pull/179)

- New `VotingEntry` type
  (compatible)
  [PR 194](https://github.com/IntersectMBO/cardano-api/pull/194)

- Fix parameterisation of `GovernanceActionId`
  (breaking)
  [PR 192](https://github.com/IntersectMBO/cardano-api/pull/192)

- Implement createPParams and begin propagating Ledger.PParams in cardano-api
  (feature)
  [PR 190](https://github.com/IntersectMBO/cardano-api/pull/190)

- Delete deprecated functions and types
  (improvement)
  [PR 173](https://github.com/IntersectMBO/cardano-api/pull/173)

## 8.14.0.0

- Fix parameterisation of `GovernanceActionId`
  (breaking)
  [PR 192](https://github.com/IntersectMBO/cardano-api/pull/192)

- Implement createPParams and begin propagating Ledger.PParams in cardano-api
  (feature)
  [PR 190](https://github.com/IntersectMBO/cardano-api/pull/190)

- Delete deprecated functions and types
  (improvement)
  [PR 173](https://github.com/IntersectMBO/cardano-api/pull/173)

## 8.13.1.0

- Implement `EraBasedProtocolParametersUpdate`
  (feature)
  [PR 180](https://github.com/IntersectMBO/cardano-api/pull/180)

## 8.13.0.0

- Modify `queryGenesisParameters` so that its type advertises it only returns genesis parameters for the Shelley era
  (breaking)
  [PR 181](https://github.com/IntersectMBO/cardano-api/pull/181)

## 8.12.0.0

- Fix `EraCast Certificate`
  (bugfix; no-api-changes)
  [PR 170](https://github.com/IntersectMBO/cardano-api/pull/170)

- Fix committee hot keys
  (feature; breaking)
  [PR 167](https://github.com/IntersectMBO/cardano-api/pull/167)

- New `inEraFeature` and `inShelleyBasedEraFeature` functions
  (feature; compatible)
  [PR 162](https://github.com/IntersectMBO/cardano-api/pull/162)

- Add `SerialiseAsBech32 (Hash CommitteeHotKey)` instance
  (feature; no-api-changes)
  [PR 160](https://github.com/IntersectMBO/cardano-api/pull/160)

## 8.11.1.0

- Fix typo: `Constitional` -> `Constitutional`
  (bugfix; no-api-changes)
  [PR 163](https://github.com/IntersectMBO/cardano-api/pull/163)

## 8.11.0.0

- Add parent feature constraints. New `IsShelleyToBabbageEra` and `ConwayEraOnwards` type classes.
  (feature; compatible)
  [PR 159](https://github.com/IntersectMBO/cardano-api/pull/159)

- Deprecate `TxVotesSupportedInEra`
  (feature; breaking)
  [PR 154](https://github.com/IntersectMBO/cardano-api/pull/154)

- Deprecate `shelleyCertificateConstraints` and `conwayCertificateConstraints`
  (feature; breaking)
  [PR 155](https://github.com/IntersectMBO/cardano-api/pull/155)

- More shelleyBasedEraConstraint constraints
  (feature; compatible)
  [PR 149](https://github.com/IntersectMBO/cardano-api/pull/149)

- Deprecate `TxGovernanceActionSupportedInEra`
  (feature; breaking)
  [PR 150](https://github.com/IntersectMBO/cardano-api/pull/150)

- Deprecate some constraint functions
  (feature; breaking)
  [PR 151](https://github.com/IntersectMBO/cardano-api/pull/151)

- Add SerialiseAsBech32 instances for committee cold and hot keys
  Remove CommitteeKey as it was redundant
  (feature; breaking)
  [PR 152](https://github.com/IntersectMBO/cardano-api/pull/152)

- Clean up constraints on Proposal and Vote instances, add their generators for tests
  (bugfix; compatible)
  [PR 118](https://github.com/IntersectMBO/cardano-api/pull/118)

## 8.10.2.0

- Expose shelleyCertificateConstraints and conwayCertificateConstraints
  (feature; compatible)
  [PR 147](https://github.com/IntersectMBO/cardano-api/pull/147)

- Provide additional constraints in `shelleyBasedEraConstraints`.  This will obsolete the following:
  - `obtainEraCryptoConstraints`
  - `obtainCryptoConstraints`
  - `obtainEraPParamsConstraint`
  - `obtainSafeToHashConstraint`
  Also provide additional constraints in `conwayEraOnwardsConstraints` and `shelleyToBabbageEraConstraints`
  (feature; compatible)
  [PR 143](https://github.com/IntersectMBO/cardano-api/pull/143)

- Expose `shelleyCertificateConstraints` and `conwayCertificateConstraints`
  (feature; compatible)
  [PR 147](https://github.com/IntersectMBO/cardano-api/pull/147)

## 8.10.1.0

- Fix permissions of file written using handleFileForWritingWithOwnerPermissionImpl
  (bugfix; no-api-changes)
  [PR 141](https://github.com/IntersectMBO/cardano-api/pull/141)

- Support more constraints for Conway witnesses
  - `conwayEraOnwardsConstraints`
  - `shelleyToBabbageEraConstraints`
  New types:
  - AnyConwayEraOnwards
  - AnyShelleyToBabbageEra
  (feature; compatible)
  [PR 137](https://github.com/IntersectMBO/cardano-api/pull/137)

- Expose functions for errors messages testing in golden files
  (feature; compatible)
  [PR 126](https://github.com/IntersectMBO/cardano-api/pull/126)

## 8.10.0.0

- Improved feature ergonomics
  New:
  - `Featured`
  - `asFeaturedInEra`
  - `asFeaturedInShelleyBasedEra`
  - `genFeaturedInEra`
  - `genMaybeFeaturedInEra`
  - `conwayEraOnwardsToCardanoEra`
  - `conwayEraOnwardsToCardanoEra`
  - `conwayEraOnwardsToShelleyBasedEra`
  - `shelleyToBabbageEraConstraints`
  - `shelleyToBabbageEraToCardanoEra`
  - `shelleyToBabbageEraToShelleyBasedEra`
  Deprecated:
  - `FeatureValue` Use `Maybe Featured` instead
  - `isFeatureValue`
  - `valueOrDefault`
  - `asFeatureValue`
  - `asFeatureValueInShelleyBasedEra`
  (feature; breaking)
  [PR 128](https://github.com/IntersectMBO/cardano-api/pull/128)

- Expose `toShelleyPoolParams` for Conway integration
  (feature; compatible)
  [PR 134](https://github.com/IntersectMBO/cardano-api/pull/134)

- `FeatureInEra` instance for `ShelleyBasedEra`
  (feature; compatible)
  [PR 131](https://github.com/IntersectMBO/cardano-api/pull/131)

## 8.9.0.0

- Expose more functionality from cardano-api
  (feature; compatible)
  [PR 130](https://github.com/IntersectMBO/cardano-api/pull/130)

- Rename `AtMostBabbageEra` to `ShelleyToBabbageEra` and add `FeatureInEra` instances to `ShelleyToBabbageEra` and `ConwayEraOnwards`.
  (feature; breaking)
  [PR 127](https://github.com/IntersectMBO/cardano-api/pull/127)

- Incorporate remaining conway certificate types into cardano-api
  (feature; breaking)
  [PR 119](https://github.com/IntersectMBO/cardano-api/pull/119)

- Wire up remaining Conway governance actions
  (feature; compatible)
  [PR 112](https://github.com/IntersectMBO/cardano-api/pull/112)

- Fix ghc version CPP
  (bugfix; compatible)
  [PR 123](https://github.com/IntersectMBO/cardano-api/pull/123)

## 8.8.1.1

- Add a HasTypeProxy constraint to getVerificationKey
  (feature; compatible)
  [PR 122](https://github.com/IntersectMBO/cardano-api/pull/122)

## 8.8.1.0

- Make it build with ghc-9.6
  (feature; no-api-changes)
  [PR 104](https://github.com/IntersectMBO/cardano-api/pull/104)

- Fix Show and Eq instances for Proposal type
  (bugfix; no-api-changes)
  [PR 115](https://github.com/IntersectMBO/cardano-api/pull/115)

- Export `withShelleyBasedEraConstraintsForLedger`
  Use the ^>= version range operator for ledger and consensus libraries to avoid breaking changes affecting builds.
  (feature; compatible)
  [PR 108](https://github.com/IntersectMBO/cardano-api/pull/108)

- -  Update to the latest ledger and consensus
  -  Introduce CommitteeKey
  (feature; breaking)
  [PR 99](https://github.com/IntersectMBO/cardano-api/pull/99)

## 8.8.0.0

- Add CastVerificationKeyRole StakePoolKey StakeKey instance
  (feature; compatible)
  [PR 101](https://github.com/IntersectMBO/cardano-api/pull/101)

- New `shelleyBasedEraConstraints` function
  (feature; compatible)
  [PR 103](https://github.com/IntersectMBO/cardano-api/pull/103)

- Propagate ledger's TxCert type family throughout the api
  (feature; breaking)
  [PR 97](https://github.com/IntersectMBO/cardano-api/pull/97)

- Parameterise `Certificate` with `era`
  (feature; breaking)
  [PR 84](https://github.com/IntersectMBO/cardano-api/pull/84)

## 8.7.0.0

- Update VotingCredential to be parameterized on cardano-api's era type
  (feature; breaking)
  [PR 85](https://github.com/IntersectMBO/cardano-api/pull/85)

## 8.6.0.0

- - Parameterise `Certificate` type with phantom `era` type argument
  - Additional `CardanoEra era` argument for:
    - `makeCommitteeDelegationCertificate`
    - `makeCommitteeHotKeyUnregistrationCertificate`
    - `makeMIRCertificate`
    - `makeStakeAddressDeregistrationCertificate`
    - `makeStakeAddressPoolDelegationCertificate`
    - `makeStakeAddressRegistrationCertificate`
    - `makeStakePoolRegistrationCertificate`
    - `makeStakePoolRetirementCertificate`
  - New functions:
    - `cardanoEraConstraints`
    - `textEnvelopeTypeInEra`
  - Delete `Certificate` constructor arguments from:
    - `PoolRegistration`
    - `PoolReRegistration`
  (feature; breaking)
  [PR 83](https://github.com/IntersectMBO/cardano-api/pull/83)

- - Conway related
  - Incorporate the ability to specify votes and governance actions in transactions
  - Introduce TxVotes and TxGovernanceActions types
  (feature; breaking)
  [PR 41](https://github.com/IntersectMBO/cardano-api/pull/41)

## 8.5.2.0

- New `requireShelleyBasedEra` function
  (feature; compatible)
  [PR 58](https://github.com/IntersectMBO/cardano-api/pull/58)

## 8.5.1.0

- New `queryEpoch` function
  (feature; compatible)
  [PR 56](https://github.com/IntersectMBO/cardano-api/pull/56)

## 8.5.0.0

- New queries:
  * `queryCurrentEpochState`
  * `queryDebugLedgerState`
  * `queryGenesisParameters`
  * `queryPoolDistribution`
  * `queryPoolState`
  * `queryProtocolParameters`
  * `queryProtocolParametersUpdate`
  * `queryProtocolState`
  * `queryStakeAddresses`
  * `queryStakeDistribution`
  * `queryStakePoolParameters`
  * `queryStakeSnapshot`
  Deprecate:
  * `queryPparams`
  (feature; breaking)
  [PR 53](https://github.com/IntersectMBO/cardano-api/pull/53)

## 8.4.0.0

- Simplify `queryStateForBalancedTx` such that:
  * It no longer invokes `executeQueryCardanoMode` `queryNodeLocalState, allowing it to run over a single connection
  * It no longer takes `socketPath` and `networkId` parameters
  (feature; breaking)
  [PR 47](https://github.com/IntersectMBO/cardano-api/pull/47)

## 8.3.0.0

- This module exports our query API as functions.  It is intended to replace our query API as the public API.

  Having functions as our public API rather than data allows us to export non-primitive functions as well as
  afford us the ability to refactor our query data types without breaking user code
  (feature; compatible)
  [PR 48](https://github.com/IntersectMBO/cardano-api/pull/48)

- Generate fields only if they make sense for the given era.
  Changes:
  - New `Cardano.Api.Feature` module
  - New`FeatureInEra` type class
  - New `FeatureValue` type
  - New functions:
    - `genFeatureValueInEra`
    - `featureInShelleyBasedEra`
    - `isFeatureValue`
    - `valueOrDefault`
    - `asFeatureValue`
    - `asFeatureValueInShelleyBasedEra`
  - `genProtocolParameters` and `genValidProtocolParameters` functions take additional `era` argument
  (feature; breaking)
  [PR 40](https://github.com/IntersectMBO/cardano-api/pull/40)

## 8.2.0.0

- Changes:
  - Updated plutus, ledger, and consensus dependency bounds
  - Added support for Plutus V3 in conway
  - Added a `query` field to `LocalNodeConnectInfo`
  - Field added to `LocalNodeConnectInfo`
  (feature; breaking)
  [PR 24](https://github.com/IntersectMBO/cardano-api/pull/24)

- Crypto pinning via `iohkNix` overlay
  (bugfix; no-api-changes)
  [PR 27](https://github.com/IntersectMBO/cardano-api/pull/27)

## 8.1.1.1

- Make `cardano-api:internal` component public
  (feature; compatible)
  [PR 26](https://github.com/IntersectMBO/cardano-api/pull/26)

## 8.1.1.0

- Expose `toAlonzoCostModels` function from `Cardano.Api`
  (feature; compatible)
  [PR 17](https://github.com/IntersectMBO/cardano-api/pull/17)

- Add `Eq` and `Data` instances to various error types
  (feature; compatible)
  [PR 9](https://github.com/IntersectMBO/cardano-api/pull/9)

- Remove support for reading `ApplicationName` and `ApplicationVersion` from node configuration and replace with hardcoded values.
  (feature; breaking)
  [PR 8](https://github.com/IntersectMBO/cardano-api/pull/8)

- Add new `checkVrfFilePermissions` function
  (feature; compatible)
  [PR 11](https://github.com/IntersectMBO/cardano-api/pull/11)

- Added `genValidProtocolParameters` generator producing `ProtocolParameters` which do pass
  validations in `Cardano.Api.ProtocolParameters` module.  Remove `Cardano.Api.Orphans` from public API.
  (feature, test; breaking)
  [PR 1](https://github.com/IntersectMBO/cardano-api/pull/1)

## 8.1.0

- Addition of `QueryStakeDelegDeposits` ledger state query.

- **Breaking change** - `evaluateTransactionBalance`, `makeTransactionBodyAutoBalance` and
  `constructBalancedTx` requires a mapping from `StakeCredential` to a deposit for every
  unregistration certificate present in the transaction

- **Breaking change** - `queryStateForBalancedTx` now requires a list of certificates and
  produces a Map with `StakeCredential` to a deposit for every deregistration certificate
  in the supplied list


### Features

- New functions: `intoFile`, `readByteStringFile`, `readLazyByteStringFile`, `readTextFile`.
  Modify functions in `Cardano.Api.IO` to use unspecified content type rather than `()`.
  ([PR 5194](https://github.com/input-output-hk/cardano-node/pull/5194))

- **Breaking change**

- Delete `Cardano.Api.Environment` module.  Merge two `SocketPath` type definitions to the one defined in `Cardano.Api.IO`
  Delete `EnvSocketError` and associated functions and types.
  ([PR 5215](https://github.com/input-output-hk/cardano-node/pull/5215))

- `NodeConfigFile` newtype replaced with `NodeConfigFile` type alias.
  `GenesisConfigFile` newtype replaced with `ByronGenesisFile`, `ShelleyGenesisFile`, `AlonzoGenesisFile`, `ConwayGenesisFile` type aliases.
  New `ByronGenesisConfig`, `ShelleyGenesisConfig`, `AlonzoGenesisConfig`, `ConwayGenesisConfig` type aliases.
  ([PR 5217](https://github.com/input-output-hk/cardano-node/pull/5217))

### Bugs

- Fix `toEraInMode` for Conway
  ([PR 5175](https://github.com/input-output-hk/cardano-node/pull/5175))

### 8.0.0 -- May 2023

- Add `getSlotForRelativeTime` function ([PR 5130](https://github.com/input-output-hk/cardano-node/pull/5130))

- New `ToJSON ScriptWitnessIndex` instance that produces machine readable output.
  Any `JSON` output uses this instance.
  [PR 5168](https://github.com/input-output-hk/cardano-node/pull/5168)

### Features

- Delete `readEnvSocketPath` function.
  ([PR 5207](https://github.com/input-output-hk/cardano-node/pull/5207))

- Expose node config reading functionality: `NodeConfig`, `NodeConfigFile` and `readNodeConfig`

- Expose genesis file reading functionality:
  - All eras: `GenesisConfig` and `readCardanoGenesisConfig`
  - Byron: `readByronGenesisConfig`
  - Shelley: `ShelleyConfig`, `GenesisHashShelley`, `readShelleyGenesisConfig` and `shelleyPraosNonce`
  - Alonzo: `GenesisHashAlonzo` and `readAlonzoGenesisConfig`
  - Conway: `GenesisHashConway` and `readConwayGenesisConfig`

- Expose envirnment construction functionality: `mkProtocolInfoCardano` and `genesisConfigToEnv`

- New error exports:
  - `TxOutInAnyEra(..)`
  - `txOutInAnyEra`
  - `StakePoolMetadataValidationError(..)`
  - `ScriptHash(..)`
  ([PR 5188](https://github.com/input-output-hk/cardano-node/pull/5188))

- Rename `TestEnableDevelopmentHardForkEras` to `ExperimentalHardForksEnabled` and
  `TestEnableDevelopmentNetworkProtocols` to `ExperimentalProtocolsEnabled`
  ([PR 4341](https://github.com/input-output-hk/cardano-node/pull/4341))

- Changed type of `protocolParamTxFeeFixed`, `protocolParamTxFeePerByte` from `Natural` to
  `Lovelace` and `protocolUpdateTxFeeFixed` and `protocolUpdateTxFeePerByte` from `Maybe Natural`
  to `Maybe Lovelace` ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- Append, not prepend change output when balancing a transaction ([PR 4343](https://github.com/input-output-hk/cardano-node/pull/4343))

- Expose convenience functions `executeQueryCardanoMode`, `determineEra`, `constructBalancedTx` and `queryStateForBalancedTx` ([PR 4446](https://github.com/input-output-hk/cardano-node/pull/4446))

- Expand `BalancedTxBody` to include `TxBodyContent` ([PR 4491](https://github.com/input-output-hk/cardano-node/pull/4491))

- Change `calculateMinimumUTxO` to return `Lovelace` instead of a `Value` ([PR 4482](https://github.com/input-output-hk/cardano-node/pull/4482))

- **Breaking change** - Reduce exposed modules in cardano-api ([PR4546](https://github.com/input-output-hk/cardano-node/pull/4546))

- **Breaking change** - `deserialiseFromRawBytes` method of the `SerialiseAsRawBytes` type class to return `Either` instead of `Maybe`.  Deprecate `eitherDeserialiseFromRawBytes`.  Use `deserialiseFromRawBytes` instead.
  ([PR 4876](https://github.com/input-output-hk/cardano-node/pull/4876))

- **Breaking change** - Preserve ScriptData bytes with HashableScriptData ([PR4886](https://github.com/input-output-hk/cardano-node/pull/4886))


- **Breaking change** - `determineEraExpr` to return `IO (Either UnsupportedNtcVersionError AnyCardanoEra)` instead of `IO AnyCardanoEra`.
  ([PR4788](https://github.com/input-output-hk/cardano-node/pull/4788))

- **Breaking change** - `queryExpr` to return `IO (Either UnsupportedNtcVersionError a)` instead of `IO a`.
  ([PR4788](https://github.com/input-output-hk/cardano-node/pull/4788))

- **Breaking change** - Remove distinction between multisig and timelock scripts([PR4763](https://github.com/input-output-hk/cardano-node/pull/4763))

- **Breaking change** Change return type of `queryNodeLocalState` to new `AcquiringFailure` type.

- **Breaking change** - For performance reasons, `evaluateTransactionFee` to take a
  `Ledger.PParams (ShelleyLedgerEra era)` argument instead of `ProtocolParameters`
  New type `BundledProtocolParameters` and new functions `bundleProtocolParams` and `unbundleProtocolParams`.
  ([PR4903](https://github.com/input-output-hk/cardano-node/pull/4903))

- Auto-balance multi asset transactions ([PR 4450](https://github.com/input-output-hk/cardano-node/pull/4450))

- **Breaking change** - Removed `fromShelleyPParams` in favor of
  `fromLedgerPParams ShelleyBasedEraShelley`
  ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - JSON fields have been changed: ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))
  - For `PState`:
    - Renamed `"pParams pState"` -> `"pParams"`
    - Renamed `"fPParams pState"` -> `"fPParams"`
    - Renamed `"retiring pState"` -> `"retiring"`
    - Added `"deposits"`
  - For `DState`:
    - Removed `"unifiedRewards"`
    - Added `"unified"`, which contains an object with both rewards and deposits.
  - For `InstantaneousRewards`:
    - Addition of `"deltaReserves"` and `"deltaTreasury"` fields
  - `CostModel` in `AlonzoGenesis` and `PParams` is formatted with a list of values to
    promote forward compatibility

- Fix a bug where only metadata from TxAuxData was hashed upon
  transaction body creation with `createTransactionBody` ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - Change representation of `CostModel`. It is no longer a mapping from
  param name to values, but instead a list with values, where order of value dictates the
  mapping to param names of a plutus cost model for a particular plutus version ([PR
  5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - ToJSON instance for CostModel and consequently for
  ProtocolParameters will now produce a list of values instead of a key value
  mapping. ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- **Breaking change** - `calculateMinimumUTxO` no longer fails, it is a total computation.
  ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- New generators in `gen` sublibrary: `genPositiveLovelace`, `genPositiveQuantity` and
  `genSignedNonZeroQuantity`. ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))
- New 'Governance.Poll' API implementing [CIP-0094](https://github.com/cardano-foundation/CIPs/pull/496) ([PR 5050](https://github.com/input-output-hk/cardano-node/pull/5050))

- Split serialisation from IO
  ([PR 5049](https://github.com/input-output-hk/cardano-node/pull/5049))

- Move parsers to reusable location
  ([PR 5046](https://github.com/input-output-hk/cardano-node/pull/5046))

- Remove unused error constructors
  ([PR 5041](https://github.com/input-output-hk/cardano-node/pull/5041))

- New `bounded` function
  ([PR 4919](https://github.com/input-output-hk/cardano-node/pull/4919))

- Modify `constructBalancedTx` to take `LedgerEpochInfo`
  ([PR 4858](https://github.com/input-output-hk/cardano-node/pull/4858))

- Add `ReaderT` of `NodeToClientVersion` to `LocalStateQueryExpr`
  ([PR 4809](https://github.com/input-output-hk/cardano-node/pull/4809))

- New `QueryStakeSnapshot` query
  ([PR 4754](https://github.com/input-output-hk/cardano-node/pull/4754))
  ([PR 4179](https://github.com/input-output-hk/cardano-node/pull/4179))

- Move signing key reading to cardano-api
  ([PR 4698](https://github.com/input-output-hk/cardano-node/pull/4698))

- Replace `Data.Map` with `Data.Map.Strict`
  ([PR 4675](https://github.com/input-output-hk/cardano-node/pull/4675))

- New `Cardano.Api.DeserialiseAnyOf` module
  ([PR 4639](https://github.com/input-output-hk/cardano-node/pull/4639))

- Replace `deserialiseFromRawBytes` function with `eitherDeserialiseFromRawBytes`
  ([PR 4626](https://github.com/input-output-hk/cardano-node/pull/4626))

- New `deserialiseFromTextEnvelopeCddlAnyOf` function
  ([PR 4625](https://github.com/input-output-hk/cardano-node/pull/4625))

- ** Breaking ** Replace `NotScriptLockedTxInsError` type with `ScriptLockedTxInsError`
  ([PR 4484](https://github.com/input-output-hk/cardano-node/pull/4484))

- Separate validation and creation of transaction bodies
  ([PR 4468](https://github.com/input-output-hk/cardano-node/pull/4468))

- New `QueryPoolDistribution` query
  ([PR 4250](https://github.com/input-output-hk/cardano-node/pull/4250))

- More efficient `ToJSON` instances that make use of `toEncoding` for streaming.
  ([PR 4205](https://github.com/input-output-hk/cardano-node/pull/4205))

- Expose `AcquireFailure` and `SystemStart` from `Cardano.Api.Shelley`
  ([PR 4199](https://github.com/input-output-hk/cardano-node/pull/4199))

- Update `makeTransactionBodyAutoBalance` function to automatically calculate the total and return collateral values
  ([PR 4198](https://github.com/input-output-hk/cardano-node/pull/4198))

- New `QueryPoolState` query
  ([PR 4170](https://github.com/input-output-hk/cardano-node/pull/4170))

- Add `utxoCostPerByte` protocol parameter
  ([PR 4141](https://github.com/input-output-hk/cardano-node/pull/4141))

- Expose `Key` interface in `Cardano.Api.Shelley`
  ([PR 4048](https://github.com/input-output-hk/cardano-node/pull/4048))

- New `generateInsecureSigningKey` function
  ([PR 4021](https://github.com/input-output-hk/cardano-node/pull/4021))

- SPO on-chain poll commands adjustments
  ([PR 5132](https://github.com/input-output-hk/cardano-node/pull/5132))

- UTC Time to slots conversion function
  ([PR 5130](https://github.com/input-output-hk/cardano-node/pull/5130))

- Add new interim governance commands: {create, answer, verify}-poll
  ([PR 5112](https://github.com/input-output-hk/cardano-node/pull/5112))

- CIP-1694 make space for DRep certificates
  ([PR 5108](https://github.com/input-output-hk/cardano-node/pull/5108))

- File type to track the content and direction of files
  ([PR 5105](https://github.com/input-output-hk/cardano-node/pull/5105))

- Expose UsingRawBytes et al types
  ([PR 5086](https://github.com/input-output-hk/cardano-node/pull/5086))

- Expose SerialiseAsRawBytesError in Cardano.Api
  ([PR 5085](https://github.com/input-output-hk/cardano-node/pull/5085))

- New genCardanoKeyWitness function
  ([PR 5071](https://github.com/input-output-hk/cardano-node/pull/5071))

- Replace roundtripCBOR with trippingCbor
  ([PR 5069](https://github.com/input-output-hk/cardano-node/pull/5069))

- Remove non-round-trippable value TxInsReferenceNone for babbage onwards in generator
  ([PR 5064](https://github.com/input-output-hk/cardano-node/pull/5064))

- Improve roundtrip functions to report annotations on callsite
  ([PR 5063](https://github.com/input-output-hk/cardano-node/pull/5063))

- Define Functor instance for FileError
  ([PR 5057](https://github.com/input-output-hk/cardano-node/pull/5057))

- Script data serialisation
  ([PR 5002](https://github.com/input-output-hk/cardano-node/pull/5002))

- Add LedgerStateBabbage and LedgerStateConway pattern synonyms
  ([PR 5001](https://github.com/input-output-hk/cardano-node/pull/5001))

- Conway hard forks on prot-ver 9
  ([PR 4988](https://github.com/input-output-hk/cardano-node/pull/4988))

- Guard against overflows in Shelley TxIns
  ([PR 4956](https://github.com/input-output-hk/cardano-node/pull/4956))

- Remove duplicate scripts when building transaction body for Mary, Alonzo and Babbage
  ([PR 4953](https://github.com/input-output-hk/cardano-node/pull/4953))

- Combinators for TxBodyContent and related types
  ([PR 4941](https://github.com/input-output-hk/cardano-node/pull/4941))

- Preserve ScriptData bytes fix
  ([PR 4926](https://github.com/input-output-hk/cardano-node/pull/4926))

- Detect invalid counter and certificate
  ([PR 4880](https://github.com/input-output-hk/cardano-node/pull/4880))

- Implement ADR-2: Restructure modules for generators
  ([PR 4833](https://github.com/input-output-hk/cardano-node/pull/4833))

- New NodeToClientVersionOf typeclass
  ([PR 4787](https://github.com/input-output-hk/cardano-node/pull/4787))

- Implement signArbitraryBytesKes for use in Mithril
  ([PR 4779](https://github.com/input-output-hk/cardano-node/pull/4779))

- Export SubmitResult from Cardano.Api
  ([PR 4753](https://github.com/input-output-hk/cardano-node/pull/4753))

- Add support for ghc-9.2 and partial support for CHaP
  ([PR 4701](https://github.com/input-output-hk/cardano-node/pull/4701))

- Append tx output in cli transaction build command
  ([PR 4696](https://github.com/input-output-hk/cardano-node/pull/4696))

- Add ToJSON/FromJSON instances for ChainPoint
  ([PR 4686](https://github.com/input-output-hk/cardano-node/pull/4686))

- Add an Ord ChainPoint instance
  ([PR 4685](https://github.com/input-output-hk/cardano-node/pull/4685))

- Derive Eq instance for AcquiringFailure
  ([PR 4683](https://github.com/input-output-hk/cardano-node/pull/4683))

- Export `fromShelleyBasedScript` from Cardano.Api
  ([PR 4682](https://github.com/input-output-hk/cardano-node/pull/4682))

- Expose TextEnvelopeCddl from Cardano.Api
  ([PR 4635](https://github.com/input-output-hk/cardano-node/pull/4635))

- Expose txScriptValidityToScriptValidity in Cardano.Api
  ([PR 4628](https://github.com/input-output-hk/cardano-node/pull/4628))

- Cardano Node 1.35.6 aka Single Relay P2P release
  ([PR 4612](https://github.com/input-output-hk/cardano-node/pull/4612))

- Update ouroboros-network and cardano-ledger dependencies
  ([PR 4608](https://github.com/input-output-hk/cardano-node/pull/4608))

- export RawBytesHexError
  ([PR 4599](https://github.com/input-output-hk/cardano-node/pull/4599))

- Module reshuffle
  ([PR 4593](https://github.com/input-output-hk/cardano-node/pull/4593))

- Add Ord instance for AddressInEra
  ([PR 4587](https://github.com/input-output-hk/cardano-node/pull/4587))

- Add ToJSON and FromJSON instances for Address
  ([PR 4568](https://github.com/input-output-hk/cardano-node/pull/4568))

- Export TxIns type alias
  ([PR 4565](https://github.com/input-output-hk/cardano-node/pull/4565))

- Export IsPlutusScriptLanguage
  ([PR 4554](https://github.com/input-output-hk/cardano-node/pull/4554))

- Export more generators
  ([PR 4534](https://github.com/input-output-hk/cardano-node/pull/4534))

- Condense Read and Validation modules in cardano-cli
  ([PR 4516](https://github.com/input-output-hk/cardano-node/pull/4516))

- Export TxTotalAndReturnCollateralSupportedInEra from Cardano.Api
  ([PR 4496](https://github.com/input-output-hk/cardano-node/pull/4496))

- Export `LocalTxSubmissionClient` data constructor
  ([PR 5096](https://github.com/input-output-hk/cardano-node/pull/5096))

### Bugs

- Fix: Add `AStakeExtendedVerificationKey` back into `deserialiseAnyVerificationKeyTextEnvelope`
  ([PR 4918](https://github.com/input-output-hk/cardano-node/pull/4918))

- Fix: Re-add `AGenesisExtendedVerificationKey` back into `deserialiseAnyVerificationKeyTextEnvelope`
  ([PR 4894](https://github.com/input-output-hk/cardano-node/pull/4894))

- Allow reading text envelopes from pipes ([PR 4384](https://github.com/input-output-hk/cardano-node/pull/4384))

- Fix 4493 bug - TxWitness text envelope format does not roundtrip in Shelley era
  ([PR 4501](https://github.com/input-output-hk/cardano-node/pull/4501))

- Fix minUTxO calculation in `calculateMinimumUTxO` function in `cardano-api`
  ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

## 1.35.3 -- August 2022

- Fix leadership schedule for current on babbage (#4106)
- Update build to allow all invalid scripts (again) (#4088)
- Fix building of Alonzo transaction in Babbage era. (#4166)
- Add `utxoCostPerByte` protocol parameter (#4141)

## 1.35.2 -- July 2022 (not released)

None

## 1.35.1 -- July 2022 (not released)

None

## 1.35.0 -- June 2022
- Add Vasil hardfork to cardano-api and cardano-cli (#3765)
- Reference script integration (#3953)
- Wire up remaining Plutusv2 reference script types (#4034)
- Add `IsString` (Hash BlockHeader) (#3619)
- Make `LedgerStateEvents` a type alias (#3692)
- Propagate protocol epoch state decode error (#3696)
- Expose the tx mempool monitoring mini protocol in cardano-api (#3706)
- Babbage functionality integration in cardano api Part 1 (#3803)
- Remove unused package (#3816)
- Add `IsCardanoEra` constraint to BlockInMode (#3665)
- Update cardano-api's TxOut with inline datum (#3773)
- Update cardano-api txout with reference scripts (#3779)
- Implement return and total collateral in cardano-api (#3787)
- Add reference transaction inputs to cardano-api (#3804)
- Fix datum in tx and ref scripts (#3882)
- Support the babbage era in the API function `cddlTypeToEra` (#3916)
- Fix typo for TxWitness BabbageEra (#3961)
- kes-period-info property test (#3718)
- Extend deserialiseFromRawBytesHex to produce error description (#3304)
- add genesis create-cardano command (#3832)
- Propagate protocol in block type (#3818)
- Create VRF signing key file with correct permissions (#1948)
- Update example-reference-script-usage.sh to also use inline datums (#4006)
- Restore deleted comment (#4044)
- Do not require decentralization parameter in protocol parameters (#4051)

## 1.34.0 -- February 2022

- Expose `lovelaceToTxOutValue`. (#3381)
- Implement two functions: `currentEpochEligibleLeadershipSlots` and
  `nextEpochEligibleLeadershipSlots` to get the leadership slots for the
  current/next epoch respectively. (#3464, #3494)
- Various small internal fixes. (#3466)
- Add a `capi` library to support using the cardano node as a C library in other
  software. (#3501)
- `fromShelleyAddr` now takes an explicit `ShelleyBasedEra` parameter to
  determine the era. The previous behaviour (with an implicit
  `IsShelleyBasedEra` constraint) can be obtained with `fromShelleyAddrIsSbe`.
  (#2253, #3606)

## 1.33.0 -- December 2021
## 1.32.1 -- November 2021

- Asset names are now rendered in a more consistent fashion in JSON output.
  Previously names which happened to be valid ASCII were rendered as such, and
  ones which were not resulted in unprintable characters. Now all names are
  rendered as hex. Future clients may choose to additionally render ASCII names
  if plausible to do so. (#3211)
- Various testing improvements. (#3361)
- Expose ledger events via the ledger state API. (#3374)

## 1.31.0 -- October 2021

- Various internal improvements and refactorings. (#3163, #3253, #3288)

## 1.30.0 -- September 2021

- Improvements to the ledger state API. (#3143)
- Make it easier to use monadic queries. (#3151)
- Implement 'getBlockHeader' for Alonzo. This was a stray function that got
  missed when implementing Alonzo in the API. (#3158)
- A few additional exports for API consumers. (#3156)
- Expose ledger events through the API. Ledger events provide a way for
  consumers to receive details about things that are happening inside the
  ledger, and will be used by tools such as db-sync. (#3085)
- Improve the error message reported when you try to spend a non-Plutus locked
  input using a Plutus script. (#3187)


## 1.29.0 -- August 2021

- Support for automated Tx building. (#2953)
- A few additional exports for API consumers. (#3001, #3055)
- Miscellaneous internal improvements. (#2948)
- Block folding interface now derives the network ID automatically from the
  ledger config. (#2955, #2975)
- Improve the error generated when a Tx output does not meet the minimum UTxO
  value. (#3027)
- Add support for querying the Alonzo ledger state. (#2974)
- Update the API documentation.

## 1.28.0 -- July 2021

- Support for the upcoming Alonzo era, including protocol parameters, Plutus
  scripts and collateral inputs. (#2784, #2798, #2808, #2810, #2815, #2818,
  #2823, #2828)
- Add a function 'getTransactionBodyContent'. This extracts a general view of
  the TxBody from the era-specific bodies. (#2663)
- Add API support for new node queries:
  - `QuerySystemStart` gets the system start time.
  - `QueryStakePools` and `QueryStakePoolParameters` can be used to get details
    on the currently known stake pools.
  - `QueryUTxOFilter` provides various ways to query a filtered subset of the
    UTxO.
  (#2843)
- Added functions to the API to assist in automated transaction building:
  - `evaluateTransactionBalance` computes the current balance of a (partial)
    transaction, which is helpful for determining what needs to be done to
    correctly balance it (such that value produced equals value consumed).
  - `evaluateTransactionExecutionUnits` computes how many ExUnits will be needed
    by all the scripts in a (partial) transaction.
  - `evaluateTransactionFee` computes the fee for a (partial) transaction,
    assuming a given number of VKey witnesses (corresponding to inputs).
  - `estimateTransactionKeyWitnessCount` attempts to estimate the number of VKey
    witnesses needed.
  - `makeTransactionBodyAutoBalance` attempts to create and automatically
    balance a transaction body, using the above tools.
  (#2906)
- Miscellaneous internal improvements. (#2836, #2840)

## 1.27.0 -- April 2021

- Add initial support for the ledger state and folding over blocks to the API.
  (#2633)
- Scripts are now stored within the TxBody in the API, rather than in the
  witnesses. (#2547)

## 1.26.1 -- March 2021

- The cardano-submit-api now takes transactions encoded as CBOR rather than
  JSON. This reverts a change to existing behaviour for backwards compatibility.
  (#2491, #2512)
- Remove a backwards-compatibility workaround related to the optional query
  point (#2241 below) when querying the NodeLocalState. This had resulted in
  spurious notifications of disconnection in the logs. Note that as a
  consequence of this, instances of the CLI and other tools using the 1.26.1 API
  will fail to query node state from older versions of the node. (#2540)

## 1.26.0 -- March 2020
- Added a demo for the use of cardano-client. This is an API to allow writing
  programs to interact with the cardano node. (#2295, #2303)
- Removed code pertaining to the old IPC API (#2319)
- Add the ability to calculate the minimum deposit needed for a transaction to
  the API, given a value. (#2325)
- When querying the NodeLocalState, make the query point optional, and use the
  chain tip when not specified. (#2241)
- Various internal improvements and refactoring (#2349, #2458)

## 1.25.0 -- January 2020
- New IPC modules for easier interaction with the node, including support for
  all existing local state queries (#2230, #2238, #2263, #2277, #2286)
- API support for Byron era update proposals and votes (#2209, #2271)
- Make Cardano.Api the primary public module for the API.
- API support for serialising multi-asset PolicyId and AssetName (#2270)
- API for pretty-printing JSON output (#2103)
- Improved tests for Byron era legacy key formats (#2259)
- More precise error cases for tx outputs that are out of range (#2217)
- Host up-to-date generated API documentation via github
  https://github.com/IntersectMBO/cardano-node (#2273, #2276, #2278)

## 1.24.2 -- December 2020

None

## 1.24.1 -- December 2020

- Fix the getTxId implementation for Byron-era transactions (#2169)

## 1.24.0 -- December 2020

- Full API support for the Allegra and Mary eras, including creating
  transactions for the new eras, and support for the special new features in
  the new eras: script extensions, tx validity intervals, auxiliary scripts,
  multi-asset tx outputs and asset minting (#2092, #2110, #2111, #2121, #2127,
  #2128, #2141, #2149)


## 1.23.0 -- November 2020

- Preliminary support for the Allegra script language extensions (#2069)
- Preliminary support for the Mary multi-asset extensions (#2083, #2085, #2093)
- Internal refactoring of the API code (#2040, #2055, #2094)

## 1.22.1 -- October 2020

None

## 1.22.0 -- October 2020

- Preliminary support for the upcoming Allegra and Mary eras (#1958, #2019)
- Additional test coverage (#1999)

## 1.21.2 -- September 2020

- Add a Ed25519-BIP32 instance of the new crypto classes (#1933, #1952)
- Adjust what is exposed via Cardano.Api.{Byron,Shelley} (#1932)

## 1.21.1 -- September 2020

None

## 1.21.0 -- September 2020
- Support for multi-signature scripts (#1788)
- Support for Byron witnesses for addresses that use attributes, which includes
  all addresses in legacy Daedalus Byron wallets (#1851, #1871)
- Introduce a Cardano.Api top level module exporting only the public parts
  and modules Cardano.Api.{Byron,Shelley} that expose the underlying library
  types for applications that need it (#1881)

## 1.20.0 -- September 2020

- Improved support for JSON to Tx metadata conversions, with two supported
  JSON schemas, suitable for different use cases (#1797)

## 1.19.1 -- September 2020

- Adjust the tx metadata JSON schema to be fully recursive (#1735)
- Audit compliance with CIP5 for common bech32 prefixes (#1781)
- Add functionality for validating tx metadata (#1432, #1677)

## 1.19.0 -- August 2020

- Support for scripts and specifically multi-sig scripts (#1623)
- Support for JSON syntax for multi-sig scripts (#1660)
- Support for converting tx metadata to/from JSON (#1606, #1682)
- Support for Bech32-encoded stake pool IDs (#1528, #1638)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

## 1.18.0 -- July 2020

None

## 1.17.0 -- July 2020

- Allow genesis keys as tx witnesses (#1483)
- Allow extended genesis delegate keys to sign operational certs (#1497)
- Add support for extended keys for stake, genesis and delegate keys (#1487)

## 1.16.0 -- July 2020

- Remove the old API (#1444, #1456)
- Added raw serialisation instances for all key types (#1455)
- Added bech32 serialisation following draft CIP 5 (#1455)

## 1.15.1 -- July 2020

- Include tx metadata in transactions in the new api (#1406)
- Add support for extended ed25519 keys for payment keys (#1411)
- Improve tx submission API in the new API (#1430)

## 1.15.0 -- July 2020

- Fix the ledger state dump query (#1333, #1334)
- Support for Byron witnesses in Shelley txs in the typed API (#1339)
- Support for Bech32 serialisation in the typed API (#1382)
- Support for other additional functionality in the typed API (#1337, #1375)
- More tests for the typed API (#1360, #1369, #1378)
- Moving code around to eliminate the cardano-config package (#1289, #1380)

## 1.14.2 -- June 2020

- Fix the query that dumps the ledger state as JSON (#1333)

## 1.14.1 -- June 2020

No changes in the cardano-api. There were changes in the cardano-node.

## 1.14.0 -- June 2020

- Improvements to the strongly-typed API (#1112, #1220, #1227, #1246)

  The API is not yet stable in this release.

## 1.13.0 -- June 2020

- Initial version of an improved strongly-typed API.
  Initially focusing on creating and serialising keys.

  The API is not yet stable in this release.

## 1.11.0 -- April 2020

- Initial version of the API package. The package provides client-side
  functionality for constructing and submitting transactions.

  The API is not yet stable in this release.

- Initial transaction API with Byron support and Shelley stubs (#787)
- Shelley address key pair generation (#799)
