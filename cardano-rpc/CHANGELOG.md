# Changelog for cardano-rpc

## 11.3.0.0 -- 2026-09-04

- The cardano-rpc gRPC server can now listen on HTTP/2 (h2c) or HTTP/2 over TLS on a configured IP address and port instead of only a unix domain socket, configured via new cardano-node options such as `--grpc-listen-port` and `--grpc-tls-certificate`. `RpcConfigF`'s `rpcSocketPath` field was replaced by the new `RpcEndpoint` sum type. Error responses no longer include internal diagnostic detail such as call stacks. Script evaluation requests are rejected when the transaction exceeds the protocol maximum size or carries more than 100 redeemers. UTxO reads are limited to 20000 keys and block fetches to 500 references per request.
  (feature, breaking)
  [PR 1322](https://github.com/intersectmbo/cardano-api/pull/1322)

- Update the vendored UTxO RPC v1beta proto definitions to the latest upstream utxorpc/spec (v0.19.2 plus the unreleased EvalReport optional-field flags from [utxorpc/spec#203](https://github.com/utxorpc/spec/pull/203), a wire-compatible change), including resetting FetchBlock to the upstream repeated request/response shape (the single-item variant moved to the upcoming utxorpc v1). Expose all v1beta service methods: the unimplemented ones (ReadData, ReadTx, ReadEraSummary, ReadState, ReadMempool, WaitForTx, WatchMempool, DumpHistory) respond with the UNIMPLEMENTED gRPC status. Populate the new `Tx.votes` field (governance votes, Conway onwards) and the new `TxOutput.original_cbor` field (canonical re-encoding of the output; the ledger does not retain the original on-chain TxOut bytes).
  (feature, breaking)
  [PR 1303](https://github.com/intersectmbo/cardano-api/pull/1303)

## 11.2.0.0 -- 2026-08-25

- Fixed the UTxO RPC `ReadGenesis` response reporting no initial funds for networks created with `cardano-cli create-testnet-data`, and stopped the node retaining the parsed genesis in memory for its whole lifetime. The Shelley genesis is now read from disk when `ReadGenesis` is served, verified against the genesis hash computed at node startup, and kept for five minutes after the last request; a genesis file that changed since startup fails the request with `FAILED_PRECONDITION`. Breaking change: `mkNodeKernelAccess` no longer takes `ProtocolInfoArgs` and takes the Shelley genesis file path instead.
  (bugfix, breaking)
  [PR 1305](https://github.com/intersectmbo/cardano-api/pull/1305)

## 11.1.0.0 -- 2026-08-17

- Add the `QueryService.ReadGenesis` UTxO RPC method, returning the full per-era genesis configuration.
  (feature)
  [PR 1277](https://github.com/intersectmbo/cardano-api/pull/1277)

- Implement the FollowTip SyncService method: streams fully parsed blocks as the chain advances, starting from the first intersection point found on the chain (an empty-hash block ref denotes origin). An empty intersect list follows from the current tip; an unmatched intersect list fails with NOT_FOUND. Rollbacks are delivered as undo actions carrying the rolled-back blocks where they can be reconstructed from ChainDB, falling back to reset otherwise.
  (feature)
  [PR 1268](https://github.com/intersectmbo/cardano-api/pull/1268)

- Implement the ReadTip SyncService method: returns the current chain tip as slot, block hash, height and timestamp.
  (feature)
  [PR 1259](https://github.com/intersectmbo/cardano-api/pull/1259)

- Fix wire encoding of two TxOutput fields: address now carries raw ledger address bytes instead of bech32/base58 text, and Datum.hash now carries the 32-byte datum hash instead of the datum CBOR for inline datums, matching other UTxO RPC implementations.
  (bugfix)
  [PR 1258](https://github.com/intersectmbo/cardano-api/pull/1258)

- Approximate out-of-range rationals in RationalNumber conversions instead of wrapping. Preparatory refactoring for FetchBlock transaction bodies: split UtxoRpc.Type into Type.* modules, use Proto-wrapped messages in conversion functions, generalise txOutToUtxoRpcTxOutput over eras, expose request helpers, and return the parsed block from fetchBlock.
  (bugfix, refactoring)
  [PR 1253](https://github.com/intersectmbo/cardano-api/pull/1253)

- Fix server crash when a client-supplied RationalNumber has a zero denominator (the protobuf field default): the value is now rejected as invalid instead of throwing.
  (bugfix)
  [PR 1253](https://github.com/intersectmbo/cardano-api/pull/1253)

- FetchBlock now returns fully populated transactions for blocks in every era, including Byron. Previously the response contained only the block header and raw block bytes.
  (feature)
  [PR 1247](https://github.com/intersectmbo/cardano-api/pull/1247)

- FetchBlock: add Block.timestamp via slot-to-UTC conversion using EraHistory. Breaking: `mkNodeKernelAccess` now takes `TopLevelConfig`; `NodeKernelAccess` field `nkaChainDB` renamed to `chainDb`.
  (feature, breaking)
  [PR 1242](https://github.com/intersectmbo/cardano-api/pull/1242)

- FetchBlock: add Block.timestamp via slot-to-UTC conversion using EraHistory. Breaking: `mkNodeKernelAccess` now takes `TopLevelConfig`; `NodeKernelAccess` field `nkaChainDB` renamed to `chainDb`.
  (feature, breaking)
  [PR 1242](https://github.com/intersectmbo/cardano-api/pull/1242)

- `Cardano.Rpc.Server.Internal.UtxoRpc.Type.txInTxOutToAnyUtxoData` now serialises the UTxO RPC `nativeBytes` for tx outputs using the era's ledger CBOR (`L.serialize' (eraProtVerHigh era)`) instead of the unversioned `Cardano.Binary.serialize'`. Downstream gRPC consumers will see the era's canonical encoding rather than the previous era-agnostic bytes.
  (bugfix)
  [PR 1221](https://github.com/intersectmbo/cardano-api/pull/1221)

## 11.0.0.0 -- 2026-05-26

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
