# Changelog for cardano-rpc

## 10.1.0.0

- Upgrade proto definitions to utxorpc v1 beta https://github.com/utxorpc/spec/pull/183
  (feature, breaking)
  [PR 1080](https://github.com/IntersectMBO/cardano-api/pull/1080)

- Add `SerialiseAsRawBytes Word64` instance to `Cardano.Api.Serialise.Raw`
  Add `Text reexport to `Cardano.Api.Pretty. Add `DecoderError` reexport to `Cardano.Api.Serialise.Cbor`
  gRPC: Add tracing datatypes
  (compatible)
  [PR 1079](https://github.com/IntersectMBO/cardano-api/pull/1079)

- gRPC: Add TxOut CBOR representation to `readUtxos` method, fix address serialisation in TxOutput.
  (bugfix)
  [PR 1021](https://github.com/IntersectMBO/cardano-api/pull/1021)

- Add decoded PlutusData and NativeScript in proto definition #947
  (feature, compatible)
  [PR 947](https://github.com/IntersectMBO/cardano-api/pull/947)

- cardano-rpc | Add getProtocolParamsJson gRPC endpoint
  (feature, compatible)
  [PR 919](https://github.com/IntersectMBO/cardano-api/pull/919)

- cardano-rpc | Add UTxO RPC: submitTx method
  (feature)
  [PR 905](https://github.com/IntersectMBO/cardano-api/pull/905)

- Update `proto-js-bundle` nix output to create bundles for all the `.proto` files
  (feature)
  [PR 913](https://github.com/IntersectMBO/cardano-api/pull/913)

- Add nix output that produces a bundle with a web-grpc client for `cardano-rpc`
  (feature)
  [PR 911](https://github.com/IntersectMBO/cardano-api/pull/911)

- cardano-rpc | Add `readUtxos` UTxO RPC query
  (feature)
  [PR 889](https://github.com/IntersectMBO/cardano-api/pull/889)

- cardano-rpc: add UTxO RPC protocol parameters query
  (feature)
  [PR 888](https://github.com/IntersectMBO/cardano-api/pull/888)
