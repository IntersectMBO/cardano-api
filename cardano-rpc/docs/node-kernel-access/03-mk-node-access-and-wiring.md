# Piece 3: mkNodeKernelAccess and node wiring

## Problem

Pieces 1 and 2 define the `NodeKernelAccess` interface in cardano-rpc and add the FetchBlock handler, but no concrete implementation exists yet.
The `IORef (Maybe NodeKernelAccess)` is always `Nothing` at runtime, so every gRPC request returns `UNAVAILABLE`.
This piece provides the real implementation by constructing `NodeKernelAccess` from `NodeKernel` internals and wiring it into the node startup sequence.

## Why

Without this piece, the node kernel access migration is incomplete: cardano-rpc has a working interface with no backing implementation.
This is the single piece that makes the entire N2C-to-direct-access transition functional at runtime.
After this piece, FetchBlock works end-to-end.

## User value

As a cardano-node operator, I want the gRPC server to query ledger state directly through the node kernel so that queries are faster and do not require a separate N2C socket connection.

## Acceptance criteria

1. **AC1: New module exists** - A new module `Cardano.Node.Rpc.NodeKernelAccess` exists at `cardano-node/cardano-node/src/Cardano/Node/Rpc/NodeKernelAccess.hs` and exports `mkNodeKernelAccess`.
   - Test: unit - `cabal build cardano-node` compiles with the new module

2. **AC2: mkNodeKernelAccess signature** - `mkNodeKernelAccess` has the signature `NodeKernel IO RemoteAddress LocalConnectionId (CardanoBlock StandardCrypto) -> NodeKernelAccess`.
   - Test: unit - compiles with correct type; incorrect type would cause a build failure

3. **AC3: nkaFetchBlock implementation** - `nkaFetchBlock` converts the hash bytes to `HeaderHash (CardanoBlock StandardCrypto)` (via `OneEraHash` wrapping `ShortByteString`), constructs a `RealPoint`, calls `ChainDB.getBlockComponent GetRawBlock`, and returns the result as strict `ByteString` (converted from lazy via `BS.toStrict`).
   - Test: E2E - `hprop_rpc_fetch_block` verifies a produced block can be fetched

4. **AC4: Snapshot acquisition with bracket** - `nkaWithSnapshot` acquires a `ReadOnlyForker` via `getReadOnlyForkerAtPoint chainDB VolatileTip` inside `withRegistry`, and uses `bracket` to ensure `roforkerClose` is called even when the callback throws an exception.
   - Test: E2E - existing `hprop_rpc_query_pparams` exercises the snapshot path end-to-end

5. **AC5: Forker error handling** - When `getReadOnlyForkerAtPoint` returns `Left err`, `nkaWithSnapshot` throws a descriptive error containing the stringified `GetForkerError`.
   - Test: unit - compiles; this is a defensive guard whose `Left` path is not exercised by E2E tests (`VolatileTip` is always available in a running testnet)

6. **AC6: Query round-trip** - `LedgerSnapshot.runQuery` converts a `QueryInMode` to a consensus `Query` via `toConsensusQuery`, calls `answerQuery` with the forker and `ExtLedgerCfg`, then converts the result back via `fromConsensusQueryResult`.
   - Test: E2E - `hprop_rpc_query_pparams` verifies the full query round-trip returns valid protocol parameters

7. **AC7: Transaction submission** - `nkaSubmitTx` converts a `TxInMode` to a consensus `GenTx` via `toConsensusGenTx`, submits it through `addLocalTxs` using the `MkSolo` constructor (GHC 9.10), and maps `MempoolTxAdded` to `SubmitSuccess` and `MempoolTxRejected` to `SubmitFail` (converting the error via `fromConsensusApplyTxErr`).
   - Test: E2E - `hprop_rpc_transaction` submits a transaction and verifies success

8. **AC8: IORef wiring in Run.hs** - `Run.hs` creates `nodeKernelAccessRef <- newIORef Nothing` before the RPC server `withAsync`, passes it to `rpcServerLoop`, and in `rnNodeKernelHook` calls `writeIORef nodeKernelAccessRef (Just (mkNodeKernelAccess nodeKernel))`.
   A comment at the `writeIORef` site documents the single-writer/many-readers safety invariant.
   `rpcServerLoop` calls `runRpcServer` with the new signature, threading through the `IORef`.
   - Test: E2E - any successful gRPC request in the testnet suite proves the IORef was populated

9. **AC9: Tracing - LogFormatting and MetaTrace** - In `Cardano.Node.Tracing.Tracers.Rpc`, the `LogFormatting` instance handles `TraceRpcSync` constructors.
   The `MetaTrace TraceRpc` instance is updated: `namespaceFor` maps the new constructors, `severityFor` assigns appropriate severity, `documentFor` provides descriptions, and `allNamespaces` lists the new namespaces.
   Note: the initial `TraceRpcSync` constructors and the renamed submit constructors are already added in piece 1 (which must update this file in lockstep with `Tracing.hs`).
   This AC covers any additional trace handling needed for `mkNodeKernelAccess`-specific paths.
   - Test: unit - compiles; `-Wincomplete-patterns` (via `-Werror`) catches missing branches

10. **AC10: E2E test for FetchBlock** - A new E2E test `hprop_rpc_fetch_block` starts a testnet, produces at least one block, fetches it via the `FetchBlock` gRPC method using the block's slot and hash, and verifies the response contains an `AnyChainBlock` whose `native_bytes` is non-empty.
    The test is added to the testnet test runner alongside the existing RPC tests.
    - Test: E2E - `TASTY_PATTERN='/RPC FetchBlock/' cabal test cardano-testnet-test`

11. **AC11: Build clean and existing tests pass** - `cabal build cardano-rpc` and `cabal build cardano-node` both succeed with no errors or warnings.
    `cabal test cardano-rpc:test:cardano-rpc-test` passes with no failures.
    Existing E2E tests (`hprop_rpc_query_pparams`, `hprop_rpc_transaction`, `hprop_rpc_search_utxos`) pass without modification.
    - Test: E2E - full CI pass

12. **AC12: Cabal module listing** - `cardano-node.cabal` lists `Cardano.Node.Rpc.NodeKernelAccess` in `exposed-modules`.
    - Test: unit - `cabal build cardano-node` would fail if the module is missing from the listing

## Out of scope

- All cardano-rpc-side type and interface changes (pieces 1 and 2 define `NodeKernelAccess`, `LedgerSnapshot`, `withNodeKernelAccess`, `fetchBlockMethod`).
- Rewriting existing RPC method bodies (pieces 4-7).
- New integration test coverage beyond `hprop_rpc_fetch_block` or conformance tests (piece 8).
- Dynamic era handling in `getEraMethod` (remains hardcoded to Conway).
- Removal of `nodeSocketPath` from `RpcConfig` (kept for backward compatibility with testnet and POM configuration).
- Performance benchmarking of direct access versus N2C.
- Streaming or ChainSync support.

## Definition of done

- [ ] All AC tests written (compile, fail on stubs)
- [ ] Implementation complete (all tests pass)
- [ ] `cabal build cardano-node` succeeds with no warnings
- [ ] `cabal build cardano-rpc` succeeds with no warnings
- [ ] E2E tests pass: `cabal test cardano-testnet-test --test-option='-p /RPC/'`
- [ ] Nix CI checks pass
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean
- [ ] No build warnings

## Notes

### Key imports for mkNodeKernelAccess

- `Cardano.Api.Query.Internal.Type.QueryInMode (toConsensusQuery, fromConsensusQueryResult)`
- `Cardano.Api.Consensus.Internal.InMode (toConsensusGenTx, fromConsensusApplyTxErr)`
- `Ouroboros.Consensus.Ledger.Query (answerQuery)`
- `Ouroboros.Consensus.Storage.ChainDB (getReadOnlyForkerAtPoint)`
- `Control.ResourceRegistry (withRegistry)`
- `Ouroboros.Consensus.Mempool.API (addLocalTxs, MempoolAddTxResult (..))`

### Files affected

**cardano-node:**

| File | Change |
|---|---|
| `src/Cardano/Node/Rpc/NodeKernelAccess.hs` | **New.** `mkNodeKernelAccess` from `NodeKernel`. |
| `src/Cardano/Node/Run.hs` | IORef creation, kernel hook population, `rpcServerLoop` signature update. |
| `src/Cardano/Node/Tracing/Tracers/Rpc.hs` | `LogFormatting` and `MetaTrace` instances for `TraceRpcSync`. |
| `cardano-node.cabal` | Add `Cardano.Node.Rpc.NodeKernelAccess` to `exposed-modules`. |

**cardano-testnet:**

| File | Change |
|---|---|
| `test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/FetchBlock.hs` | **New.** `hprop_rpc_fetch_block` E2E test. |
| `test/cardano-testnet-test/cardano-testnet-test.hs` | Register new test. |

### Gotchas for the implementer

- **`ReadOnlyForker` must be closed even on exception;** use `bracket` with `roforkerClose` inside `withRegistry`.

- **`getReadOnlyForkerAtPoint` can return `Left GetForkerError`** if the point is not on chain or too old; throw a descriptive error.

- **`MkSolo` constructor.** On GHC 9.10, use `MkSolo` (not `Solo`) from `Data.Tuple` when calling `addLocalTxs`.

- **`toConsensusQuery` returns `Some (Query (CardanoBlock c))`;** pattern match with `Some`.

- **`fromConsensusQueryResult` for era-specific queries returns `Either EraMismatch result`;** the `runQuery` implementation must handle the `Left` case.

- **`RealPoint` requires both slot and hash.** The `nkaFetchBlock` implementation must construct `RealPoint` from the slot and hash provided by the handler.

- **`rpcServerLoop` currently calls `runRpcServer rpcTracer (config, networkMagic)`.** The signature change adds the `IORef` parameter and uncurries the tuple (pieces 1 and 2 handle the cardano-rpc side of this change).

- **The `rpcServerLoop` signature change touches a function that also handles SIGHUP reconfiguration.** Care needed not to break the config reload path.

### Risks

| Risk | Status | Mitigation |
|------|--------|------------|
| `answerQuery` API differs from expected signature | **Verified** (2026-05-22) | Signature matches: `ExtLedgerCfg blk -> ReadOnlyForker' m blk -> Query blk result -> m result` |
| Forker lifecycle - leaking forkers on exceptions | **Addressed** | `bracket` pattern in `nkaWithSnapshot`; `withRegistry` provides additional safety net |
| `addLocalTxs` API changed in recent consensus | **Verified** (2026-05-22) | Name and signature confirmed; use `MkSolo` constructor on GHC 9.10 |
| Concurrent reads during kernel initialisation race | Low risk | `IORef` write in `rnNodeKernelHook` happens before any RPC can succeed; `withNodeKernelAccess` checks atomically |

### Dependencies

- **Upstream:** pieces 1 and 2 (for `NodeKernelAccess` types, environment wiring, proto definitions, and handler).
- **Downstream:** pieces 4-8 (method rewrites and integration testing).

### Testing approach

| AC | Type | What it tests |
|---|---|---|
| AC1 | unit | New module compiles |
| AC2 | unit | `mkNodeKernelAccess` type signature |
| AC3 | E2E | `nkaFetchBlock` via `hprop_rpc_fetch_block` |
| AC4 | E2E | Snapshot path via `hprop_rpc_query_pparams` |
| AC5 | unit | Defensive forker error guard (compiles) |
| AC6 | E2E | Full query round-trip |
| AC7 | E2E | Transaction submission via `hprop_rpc_transaction` |
| AC8 | E2E | IORef wiring (proven by any successful gRPC request) |
| AC9 | unit | Tracing instances compile |
| AC10 | E2E | `hprop_rpc_fetch_block` dedicated integration test |
| AC11 | E2E | Full regression (all existing tests pass) |
| AC12 | unit | Cabal listing (compiles) |

## Reference docs

- [API signatures](prereqs-api-signatures.md) - `answerQuery`, `getReadOnlyForkerAtPoint`, `toConsensusQuery` signatures
- [UTxO-HD internals](analysis-utxohd-internals.md) - forker lifecycle, backing store mechanics
- [Build and conventions](prereqs-build-and-conventions.md) - nix build commands for cardano-node
