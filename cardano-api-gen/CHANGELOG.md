# Changelog for cardano-api-gen

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
