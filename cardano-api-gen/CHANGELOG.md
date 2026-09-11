# Changelog for cardano-api-gen

## 10.2.0.0

- Upgrade ledger: cardano-protocol-tpraos-1.4.1.0, cardano-ledger-shelley-test-1.7.0.0, cardano-ledger-shelley-1.17.0.0, cardano-ledger-mary-1.9.0.0, cardano-ledger-dijkstra-0.1.0.0, cardano-ledger-core-1.18.0.0, cardano-ledger-conway-1.20.0.0, cardano-ledger-byron-1.2.0.0, cardano-ledger-binary-1.7.0.0, cardano-ledger-babbage-1.12.0.0, cardano-ledger-api-1.12.0.0, cardano-ledger-alonzo-1.14.0.0, cardano-data-1.2.4.1, cardano-crypto-wrapper-1.6.1.0
  Upgrade consensus: ouroboros-consensus-protocol-0.13.0.0, ouroboros-consensus-diffusion-0.24.0.0, ouroboros-consensus-0.28.0.0, ouroboros-consensus-cardano-0.26.0.0
  Upgrade network: ouroboros-network-protocols-0.12.0.0, ouroboros-network-framework-0.14.0.0, ouroboros-network-api-0.11.0.0, ouroboros-network-0.18.0.0
  Upgrade plutus-core-1.53, plutus-ledger-api-1.53
  (feature, breaking)
  [PR 954](https://github.com/IntersectMBO/cardano-api/pull/954)

- Remove `Arbitrary ByteString` instance
  (compatible)
  [PR 959](https://github.com/IntersectMBO/cardano-api/pull/959)

## 10.1.0.0

- Export genTxOutByron
  (compatible)
  [PR 703](https://github.com/IntersectMBO/cardano-api/pull/703)

## 9.0.0.0

- Deprecate `valueFromList` and valueToList. Add `IsList Value` instance.
  Fix fee estimation when autobalancing assets minted in the transaction.
  (breaking, bugfix)
  [PR 622](https://github.com/IntersectMBO/cardano-api/pull/622)

- Deprecate createAndValidateTransactionBody. Use createTransactionBody instead.
  (breaking)
  [PR 597](https://github.com/IntersectMBO/cardano-api/pull/597)

- Add `Lovelace` as a type synonym to `Coin`
  (compatible)
  [PR 614](https://github.com/IntersectMBO/cardano-api/pull/614)

- New generator `genValidTxBody`.
  Fix missing script proposals in transaction building [#594](https://github.com/IntersectMBO/cardano-api/issues/594).
  (breaking, refactoring, bugfix, test)
  [PR 602](https://github.com/IntersectMBO/cardano-api/pull/602)

- TxBodyContent: support treasury donations
  (breaking)
  [PR 543](https://github.com/IntersectMBO/cardano-api/pull/543)

- Adding `MinFeeRefScriptCostPerByte` to Conway PParams
  (feature)
  [PR 524](https://github.com/IntersectMBO/cardano-api/pull/524)

- Use the ledger's Coin instead of our custom Lovelace type
  (breaking, improvement)
  [PR 475](https://github.com/IntersectMBO/cardano-api/pull/475)

## 8.2.1.0

- Allow `checkLedgerStateCondition` check to run in IO. Rename to `foldEpochState`.
  (feature, breaking)
  [PR 453](https://github.com/IntersectMBO/cardano-api/pull/453)


## 8.2.0.0

- Deprecate `TxVotesSupportedInEra`
  (feature; breaking)
  [PR 154](https://github.com/IntersectMBO/cardano-api/pull/154)

- Expose functions for errors messages testing in golden files
  (feature; compatible)
  [PR 126](https://github.com/IntersectMBO/cardano-api/pull/126)

## 8.1.1.1

- Add a `HasTypeProxy` constraint to `getVerificationKey`
  (bugfix; compatible)
  [PR 122](https://github.com/IntersectMBO/cardano-api/pull/122)

## 8.1.0.2

First usable version.
