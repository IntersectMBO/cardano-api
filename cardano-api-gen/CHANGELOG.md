# Changelog for cardano-api-gen

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
