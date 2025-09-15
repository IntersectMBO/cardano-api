# Changelog for cardano-wasm

## 10.0.0.0 (unreleased)

- Added `newTx` and `newExperimentalEraTx` functions to `cardano-wasm` API
  (feature)
  [PR 953](https://github.com/IntersectMBO/cardano-api/pull/953)

- Added simple wallet example using `cardano-wasm`
  (feature)
  [PR 940](https://github.com/IntersectMBO/cardano-api/pull/940)

- Fix coin serialisation to use `bigint` instead of `Number`
  (bugfix)
  [PR 931](https://github.com/IntersectMBO/cardano-api/pull/931)

- cardano-wasm | Add `getUtxos` using GRPC-web to JS API
  (feature, compatible)
  [PR 927](https://github.com/IntersectMBO/cardano-api/pull/927)

- cardano-wasm | Add getProtocolParams query to cardano-wasm
  (feature)
  [PR 924](https://github.com/IntersectMBO/cardano-api/pull/924)

- Fix capitalisation error in typescript declaration (it used BigInt instead of bigint, which is a different thing)
  (bugfix)
  [PR 925](https://github.com/IntersectMBO/cardano-api/pull/925)

- Added support for submitting transactions through gRPC-web to the `cardano-wasm` API
  (feature)
  [PR 920](https://github.com/IntersectMBO/cardano-api/pull/920)

- Added support for handling payment addresses and keys to `cardano-wasm` API
  (feature)
  [PR 917](https://github.com/IntersectMBO/cardano-api/pull/917)

- Updated `proto-js-bundle` nix output to create a single `.js` bundle file with qualified names for each `.proto`
  (feature)
  [PR 914](https://github.com/IntersectMBO/cardano-api/pull/914)

- Added wasm API support for querying era from cardano-node (through GRPC-web)
  (feature)
  [PR 912](https://github.com/IntersectMBO/cardano-api/pull/912)

- Added typescript declaration for `cardano-wasm` API
  (feature, documentation)
  [PR 901](https://github.com/IntersectMBO/cardano-api/pull/901)

- Add `estimateMinFee` function to `UnsignedTx` in `cardano-wasm` API
  (feature)
  [PR 900](https://github.com/IntersectMBO/cardano-api/pull/900)

- Add `SerialiseAsRawBytes` instance to `UnsignedTx ConwayEra`
  (feature)
  [PR 880](https://github.com/IntersectMBO/cardano-api/pull/880)

- Added support for compiling `cardano-api` to `wasm`
  (feature)
  [PR 852](https://github.com/IntersectMBO/cardano-api/pull/852)

