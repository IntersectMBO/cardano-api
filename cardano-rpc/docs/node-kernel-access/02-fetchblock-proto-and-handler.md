# Piece 2: FetchBlock proto and handler

## Problem

cardano-rpc has no `FetchBlock` implementation and no direct ChainDB access path.
Before rewriting existing N2C-based RPCs, we need a clean end-to-end proof that direct node access works, using a new method with no legacy code to break.

## Why

This piece adds the SyncService proto definition and the `fetchBlockMethod` handler.
After this piece, the proto bindings exist, the handler compiles, but it cannot run end-to-end yet (needs piece 3 for node-side implementation).
Starting with a new RPC method de-risks the node kernel access architecture without touching any existing N2C code.

## User value

As a dApp developer, I want to fetch raw block bytes by slot and hash via gRPC so that I can decode blocks client-side without running a separate chain indexer.

## Acceptance criteria

1. **AC1: sync.proto and codegen** - A new proto file `cardano-rpc/proto/utxorpc/v1beta/sync/sync.proto` exists, containing the `SyncService` with only the `FetchBlock` RPC and its message types (`BlockRef`, `FetchBlockRequest`, `FetchBlockResponse`, `AnyChainBlock`).
   Running `buf generate proto` in the nix dev shell produces proto-lens bindings under `gen/` (`Proto.Utxorpc.V1beta.Sync.Sync` and `Proto.Utxorpc.V1beta.Sync.Sync_Fields`).
   Both generated modules are listed in `cardano-rpc.cabal` under `library gen`.
   - Test: unit - `cabal build cardano-rpc` compiles the generated modules

2. **AC2: Proto API wrapper for SyncService** - A new module `Cardano.Rpc.Proto.Api.UtxoRpc.Sync` exists, following the same pattern as `Query.hs` and `Submit.hs` (re-exports generated proto modules, declares `RequestMetadata`, `ResponseInitialMetadata`, `ResponseTrailingMetadata` type instances for `Protobuf SyncService`).
   The module is listed in `exposed-modules` in `cardano-rpc.cabal`.
   - Test: unit - compiles

3. **AC3: fetchBlockMethod implementation** - A new module `Cardano.Rpc.Server.Internal.UtxoRpc.Sync` exists with `fetchBlockMethod :: FetchBlockRequest -> RpcHandler FetchBlockResponse`.
   For each `BlockRef` in the request, the method extracts `slot` and `hash` fields.
   It calls `nkaFetchBlock slotNo hashBytes` via `withNodeKernelAccess`.
   If the result is `Just rawBytes`, the block is wrapped in an `AnyChainBlock` with `native_bytes` set to `rawBytes`.
   If the result is `Nothing`, the block is omitted from the response (not-found blocks are skipped silently) and a `TraceRpcFetchBlockNotFound` trace is emitted with the slot number.
   Only the `native_bytes` field is populated; the `cardano` oneof field is left empty.
   - Test: E2E - `hprop_rpc_fetch_block` verifies a produced block can be fetched and its raw bytes are non-empty

4. **AC4: Missing slot returns INVALID_ARGUMENT** - When a `BlockRef` has `slot == 0` (the proto default, meaning unset) but a non-empty `hash`, `fetchBlockMethod` returns a gRPC `INVALID_ARGUMENT` error with a message containing "slot is required".
   A `RealPoint` cannot be constructed without both slot and hash; the method validates this before calling `nkaFetchBlock`.
   - Test: unit - `H.propertyOnce`: construct a `FetchBlockRequest` with a `BlockRef` whose slot is 0 and hash is non-empty, call `fetchBlockMethod`, assert `GrpcException` with `GrpcInvalidArgument` is thrown

5. **AC5: Empty hash returns INVALID_ARGUMENT** - When a `BlockRef` has a non-zero `slot` but an empty `hash`, `fetchBlockMethod` returns a gRPC `INVALID_ARGUMENT` error with a message containing "hash is required".
   - Test: unit - `H.propertyOnce`: construct a `FetchBlockRequest` with a `BlockRef` whose slot is non-zero and hash is empty, call `fetchBlockMethod`, assert `GrpcException` with `GrpcInvalidArgument` is thrown

6. **AC6: Server.hs registers SyncService** - `Server.hs` registers `methodsSyncRpc` alongside `methodsNodeRpc`, `methodsUtxoRpc`, and `methodsUtxoRpcSubmit`.
   - Test: unit - compiles (verified by build)

## Out of scope

- Populating the `cardano` oneof field in `AnyChainBlock` (requires protobuf block type mapping, a separate piece of work).
- Streaming RPCs from the sync proto (`FollowTip`, `DumpHistory`).
- `mkNodeKernelAccess` in cardano-node (piece 3).
- Node startup wiring in `Run.hs` (piece 3).
- E2E test infrastructure (piece 3 provides the node-side implementation needed to run FetchBlock end-to-end).
- `field_mask` support on `FetchBlockRequest` (parse-into-proto would need the `cardano` field populated first).
- Handling of `BlockRef.height` and `BlockRef.timestamp` fields (not needed for `RealPoint` construction; reserved for future use).

## Definition of done

- [ ] All AC tests written (compile, fail on stubs)
- [ ] Implementation complete (all tests pass via `cabal test`)
- [ ] `cabal build cardano-rpc` succeeds from `/work` with no warnings
- [ ] Nix CI checks pass
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean (`scripts/devshell/prettify` run on changed files)
- [ ] No build warnings

## Notes

### Design decision: skip not-found blocks (not NOT_FOUND status)

The proto `FetchBlockResponse` has `repeated AnyChainBlock block` with no per-element status field.
A missing block can only be expressed by omission from the list or by failing the entire request with a gRPC NOT_FOUND status.
Failing the entire batch because one block is missing would be surprising and unhelpful for clients requesting multiple blocks.
Therefore, blocks not found in ChainDB are silently omitted from the response.
Clients can compare the count of returned blocks against the count of requested `BlockRef` entries to detect missing blocks.

### Files affected

| File | Change |
|---|---|
| `proto/utxorpc/v1beta/sync/sync.proto` | **New.** FetchBlock RPC and message types. |
| `gen/Proto/Utxorpc/V1beta/Sync/Sync.hs` | **Generated.** Proto-lens bindings (do not edit manually). |
| `gen/Proto/Utxorpc/V1beta/Sync/Sync_Fields.hs` | **Generated.** Proto-lens field accessors (do not edit manually). |
| `src/Cardano/Rpc/Proto/Api/UtxoRpc/Sync.hs` | **New.** Proto API wrapper for SyncService. |
| `src/Cardano/Rpc/Server/Internal/UtxoRpc/Sync.hs` | **New.** `fetchBlockMethod`. |
| `src/Cardano/Rpc/Server.hs` | Register `methodsSyncRpc`. |
| `cardano-rpc.cabal` | Add new modules to `exposed-modules` and `library gen`. |

### Gotchas for the implementer

- **Proto codegen requires nix dev shell.** `buf` is not available outside it.
  Run `nix develop --command bash -c "cd cardano-rpc && buf generate proto"`.

- **`AnyChainBlock` has both `native_bytes` and `cardano`.** For this piece, only populate `native_bytes`.
  The parsed `cardano` block field is a separate, complex piece of work.

- **`GetRawBlock` returns `Lazy.ByteString`.** The proto `native_bytes` field is strict `ByteString`.
  Convert via `Data.ByteString.Lazy.toStrict`.

- **Hash bytes conversion.** The proto hash is raw bytes (`ByteString`).
  `HeaderHash (CardanoBlock StandardCrypto)` is `OneEraHash` wrapping `ShortByteString`.
  Convert via `OneEraHash . SBS.toShort . BS.toStrict` (if the input is lazy) or `OneEraHash . SBS.toShort` (if strict).
  No CBOR wrapping is needed; it is raw hash bytes.

- **`RealPoint` requires both slot and hash.** If the proto `BlockRef` has `slot == 0` (proto default for unset) but a non-empty hash, or a non-zero slot but empty hash, we cannot construct a `RealPoint`.
  Validate both fields and return `INVALID_ARGUMENT` if either is missing.

### Dependencies

- **Upstream:** piece 1 (for `NodeKernelAccess`, `withNodeKernelAccess`, environment wiring, and tracing types).
- **Downstream:** piece 3 (provides the node-side `mkNodeKernelAccess` implementation needed for E2E).

### Testing approach

| AC | Type | What it tests |
|---|---|---|
| AC1 | unit | Proto codegen compiles |
| AC2 | unit | Proto API wrapper compiles |
| AC3 | E2E | `fetchBlockMethod` end-to-end happy path (requires piece 3) |
| AC4 | unit | Missing slot returns `INVALID_ARGUMENT` |
| AC5 | unit | Empty hash returns `INVALID_ARGUMENT` |
| AC6 | unit | Server registration compiles |

### Open questions

- Should `nkaFetchBlock` also support fetching by hash alone (without slot)?
  `ChainDB` has `getBlockComponent` which takes a `RealPoint` (requiring both), so hash-only lookup would need a different API (`iteratorNext` or similar).
  For now, both slot and hash are required.
  Hash-only lookup can be a follow-up story if needed.

## Reference docs

- [Architecture and current state](analysis-architecture.md) - cardano-rpc overview and spec coverage
- [Build and conventions](prereqs-build-and-conventions.md) - proto codegen instructions, nix build commands
