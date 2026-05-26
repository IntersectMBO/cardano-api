# Changelog for cardano-rpc

## 10.3.0.0 -- 2026-05-26

- gRPC: Guard against fetching the entire UTxO set. ReadUtxos now returns an empty response when no keys are provided. SearchUtxos now rejects predicates that cannot be narrowed to specific addresses with INVALID_ARGUMENT, instead of falling back to QueryUTxOWhole.
  (breaking)
  [PR 1214](https://github.com/intersectmbo/cardano-api/pull/1214)

- Add evalTx gRPC method to the UTxO RPC submit service, evaluating a CBOR-serialised transaction against the current ledger state and returning per-redeemer execution units, computed minimum fee, script evaluation errors, and balance check results without submitting.
  (feature)
  [PR 1193](https://github.com/intersectmbo/cardano-api/pull/1193)

- Bump `proto-lens` lower bound to `>=0.7.1.7`.
  (compatible)
  [PR 1185](https://github.com/intersectmbo/cardano-api/pull/1185)

- Add lower bound to proto-lens >= 0.7.1.6
  (compatible)
  [PR 1149](https://github.com/intersectmbo/cardano-api/pull/1149)

- gRPC: add tip timestamp to ChainPoint response
  (bugfix)
  [PR 1134](https://github.com/intersectmbo/cardano-api/pull/1134)

- Add searchUtxos gRPC method to the UTxO RPC query service, implementing predicate-based UTxO filtering with address, asset, and boolean combinators, plus cursor-based pagination.
  (feature)
  [PR 1123](https://github.com/intersectmbo/cardano-api/pull/1123)

## 10.2.0.0

- Integrate new Ledger and Consensus packages for Node 10.7.
  (breaking)
  [PR 1050](https://github.com/IntersectMBO/cardano-api/pull/1050)

- Add lower bound to proto-lens >= 0.7.1.6
  (compatible)
  [PR 1149](https://github.com/IntersectMBO/cardano-api/pull/1149)

## 10.1.0.0

- Remove configuration reload action in cardano-rpc server startup
  (breaking, refactoring)
  [PR 1114](https://github.com/IntersectMBO/cardano-api/pull/1114)

- Upgrade proto definitions to utxorpc v1 beta https://github.com/utxorpc/spec/pull/183
  (feature, breaking)
  [PR 1080](https://github.com/IntersectMBO/cardano-api/pull/1080)

- Add `SerialiseAsRawBytes Word64` instance to `Cardano.Api.Serialise.Raw`
  Add `Text` reexport to `Cardano.Api.Pretty. Add `DecoderError` reexport to `Cardano.Api.Serialise.Cbor`
  gRPC: Add tracing datatypes
  (compatible)
  [PR 1079](https://github.com/IntersectMBO/cardano-api/pull/1079)

- gRPC: Add TxOut CBOR representation to `readUtxos` method, fix address serialisation in TxOutput.
  (bugfix)
  [PR 1021](https://github.com/IntersectMBO/cardano-api/pull/1021)

- Add decoded PlutusData and NativeScript in proto definition #947
  (feature, compatible)
  [PR 947](https://github.com/IntersectMBO/cardano-api/pull/947)

- Add getProtocolParamsJson gRPC endpoint
  (feature, compatible)
  [PR 919](https://github.com/IntersectMBO/cardano-api/pull/919)

- Add UTxO RPC: submitTx method
  (feature)
  [PR 905](https://github.com/IntersectMBO/cardano-api/pull/905)

- Update `proto-js-bundle` nix output to create bundles for all the `.proto` files
  (feature)
  [PR 913](https://github.com/IntersectMBO/cardano-api/pull/913)

- Add nix output that produces a bundle with a web-grpc client for `cardano-rpc`
  (feature)
  [PR 911](https://github.com/IntersectMBO/cardano-api/pull/911)

- Add `readUtxos` UTxO RPC query
  (feature)
  [PR 889](https://github.com/IntersectMBO/cardano-api/pull/889)

- add UTxO RPC protocol parameters query
  (feature)
  [PR 888](https://github.com/IntersectMBO/cardano-api/pull/888)
