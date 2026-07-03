# Piece 1: NodeKernelAccess types and cardano-rpc plumbing

## Problem

cardano-rpc currently threads `LocalNodeConnectInfo` through its environment and `MonadRpc` constraint.
Every RPC method grabs the connection info and opens a fresh N2C socket connection per request.
To support direct ledger state access (ADR-019), we need a new abstraction that replaces this pattern with an `IORef (Maybe NodeKernelAccess)` passed in by cardano-node at startup.

## Why

This piece creates the `NodeKernelAccess` abstraction and wires it through the cardano-rpc infrastructure.
After this piece, cardano-rpc compiles with the new types threaded through, but no new RPC methods exist yet (piece 2) and no node-side implementation exists yet (piece 3).
Separating the plumbing from the method rewrites and node-side implementation keeps each piece small and reviewable.

## User value

As a cardano-rpc developer, I want the `NodeKernelAccess` abstraction and environment wiring in place so that I can rewrite individual RPC methods to use node kernel access in subsequent pieces.

## Acceptance criteria

1. **AC1: NodeKernelAccess module** - A new module `Cardano.Rpc.Server.Internal.NodeKernelAccess` exists at `src/Cardano/Rpc/Server/Internal/NodeKernelAccess.hs`, exporting `NodeKernelAccess(..)`, `LedgerSnapshot(..)`, and `withNodeKernelAccess`.
   `NodeKernelAccess` is a record with three fields: `nkaWithSnapshot :: forall a. (LedgerSnapshot -> IO a) -> IO a`, `nkaSubmitTx :: TxInMode -> IO (SubmitResult TxValidationErrorInCardanoMode)`, and `nkaFetchBlock :: SlotNo -> ByteString -> IO (Maybe ByteString)`.
   `LedgerSnapshot` is a newtype wrapping `runQuery :: forall result. QueryInMode result -> IO result`.
   The module is listed in `exposed-modules` in `cardano-rpc.cabal`.
   - Test: unit - compiles and is importable from the test suite

2. **AC2: withNodeKernelAccess unavailable behaviour** - `withNodeKernelAccess` reads the `IORef (Maybe NodeKernelAccess)`; when the value is `Nothing`, it throws a `GrpcException` with `grpcError = GrpcUnavailable` and message containing "not yet initialised".
   When the value is `Just na`, it passes `na` to the callback and returns the callback's result.
   - Test: unit - `H.propertyOnce`: create `IORef Nothing`, call `withNodeKernelAccess`, assert `GrpcException` with `GrpcUnavailable` is thrown; create `IORef (Just mockNodeKernelAccess)`, call `withNodeKernelAccess`, assert callback receives the value and its return value is propagated

3. **AC3: Server.hs signature change** - `runRpcServer` signature changes from `Tracer IO TraceRpc -> (RpcConfig, NetworkMagic) -> IO ()` to `Tracer IO TraceRpc -> RpcConfig -> NetworkMagic -> IORef (Maybe NodeKernelAccess) -> IO ()`.
   The module re-exports `NodeKernelAccess(..)` and `LedgerSnapshot(..)`.
   `RpcEnv` construction is updated to include both `rpcNodeKernelAccess` (from the new parameter) and `rpcLocalNodeConnectInfo` (preserved temporarily).
   Note: `methodsSyncRpc` is NOT registered in this piece - that happens in piece 2 when the SyncService proto and handler exist.
   - Test: unit - compiles (API change verified by build)

4. **AC4: Environment and MonadRpc wiring** - `RpcEnv` in `Env.hs` gains a new field `rpcNodeKernelAccess :: !(IORef (Maybe NodeKernelAccess))`.
   A `Has (IORef (Maybe NodeKernelAccess)) RpcEnv` instance is added to `Monad.hs`.
   `MonadRpc` constraint includes `Has (IORef (Maybe NodeKernelAccess)) e`.
   The old `rpcLocalNodeConnectInfo` field and `Has LocalNodeConnectInfo RpcEnv` instance are kept temporarily so that method files compile unchanged.
   - Test: unit - compiles; the new constraint is exercised by `withNodeKernelAccess` usage in the test from AC2

5. **AC5: Tracing for new trace types** - `Tracing.hs` gains a `TraceRpcSync` sum type with constructors: `TraceRpcFetchBlockSpan TraceSpanEvent` (span begin/end), `TraceRpcFetchBlockNotFound SlotNo` (block not on chain).
   `TraceRpc` gains a `TraceRpcSync TraceRpcSync` constructor.
   `Pretty` instances render the span events as "Started fetch block method" / "Finished fetch block method" and the not-found as "Block not found at slot <n>".
   An `Inject TraceRpcSync TraceRpc` instance is provided.
   `TraceRpcSubmitN2cConnectionError SomeException` is replaced by `TraceRpcNodeKernelAccessUnavailable` (no payload) and `TraceRpcForkerError String`.
   `Pretty TraceRpcSubmit` renders them as `"Ledger access unavailable (node kernel not yet initialised)"` and `"Forker error: <msg>"` respectively.
   The corresponding one-line update in `Submit.hs` (replacing `Left $ TraceRpcSubmitN2cConnectionError e` with `Left $ TraceRpcNodeKernelAccessUnavailable`) is included so that the build stays clean.
   - Test: unit - `H.propertyOnce` asserting the `Pretty` output of each new constructor contains the expected substrings

## Out of scope

- Populating the `cardano` oneof field in `AnyChainBlock` (requires protobuf block type mapping, a separate piece of work).
- Streaming RPCs from the sync proto (`FollowTip`, `DumpHistory`).
- Rewriting existing RPC methods (Query, Submit, Eval, Node) to use `NodeKernelAccess` (pieces 4-7).
- Removing `rpcLocalNodeConnectInfo` and `Has LocalNodeConnectInfo` from `RpcEnv` / `MonadRpc` (happens when the last N2C method is rewritten in pieces 4-7).
- Removing `mkLocalNodeConnectInfo` (removed alongside `rpcLocalNodeConnectInfo`).
- Removing `nodeSocketPath` from `RpcConfig` (still needed for `nodeSocketPathToRpcSocketPath`).
- Proto definitions and codegen (piece 2).
- FetchBlock handler (piece 2).
- `mkNodeKernelAccess` in cardano-node (piece 3).
- Node startup wiring (piece 3).
- Adding new E2E tests (no runtime behaviour changes in this piece).

## Definition of done

- [ ] All AC tests written (compile, fail on stubs)
- [ ] Implementation complete (all tests pass via `cabal test`)
- [ ] `cabal build cardano-rpc` succeeds from `/work` with no warnings
- [ ] Nix CI checks pass
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean (`scripts/devshell/prettify` run on changed files)
- [ ] No build warnings

## Notes

### Design decision: keep both fields temporarily

This piece adds `rpcNodeKernelAccess :: IORef (Maybe NodeKernelAccess)` to `RpcEnv` alongside the existing `rpcLocalNodeConnectInfo :: LocalNodeConnectInfo`.
Removing `rpcLocalNodeConnectInfo` would break every existing method file (`Node.hs`, `Query.hs`, `Submit.hs`, `Eval.hs`) because they all use `nodeConnInfo <- grab` to obtain a `LocalNodeConnectInfo`.
Rewriting those method bodies is the work of pieces 4-7.

Both fields coexist in `RpcEnv` and both `Has` instances exist in `MonadRpc`.
This means:
- Existing method files compile without any changes.
- Runtime behaviour of existing methods is unchanged (they still use N2C).
- Pieces 4-7 each rewrite one method's N2C usage; the last piece to land removes the old field and instance.

### Files affected

| File | Change |
|---|---|
| `src/Cardano/Rpc/Server/Internal/NodeKernelAccess.hs` | **New.** `NodeKernelAccess`, `LedgerSnapshot`, `withNodeKernelAccess`. |
| `src/Cardano/Rpc/Server/Internal/Env.hs` | Add `rpcNodeKernelAccess` field alongside existing `rpcLocalNodeConnectInfo`. |
| `src/Cardano/Rpc/Server/Internal/Monad.hs` | Add `Has (IORef (Maybe NodeKernelAccess)) RpcEnv` instance. Add constraint to `MonadRpc`. |
| `src/Cardano/Rpc/Server/Internal/Tracing.hs` | Add `TraceRpcSync` type and constructors. Replace `TraceRpcSubmitN2cConnectionError` with `TraceRpcNodeKernelAccessUnavailable` and `TraceRpcForkerError`. |
| `src/Cardano/Rpc/Server/Internal/UtxoRpc/Submit.hs` | One-line trace constructor update. |
| `src/Cardano/Rpc/Server.hs` | New signature, re-exports, updated `RpcEnv` construction. |
| `cardano-rpc.cabal` | Add `NodeKernelAccess` module to `exposed-modules`. |

**cardano-node** (must update in lockstep to keep `-Werror` clean):

| File | Change |
|---|---|
| `src/Cardano/Node/Tracing/Tracers/Rpc.hs` | Handle renamed `TraceRpcNodeKernelAccessUnavailable`/`TraceRpcForkerError` and new `TraceRpcSync` constructors in `forMachine`, `asMetrics`, `namespaceFor`, `severityFor`, `documentFor`, `allNamespaces`. |
| `src/Cardano/Node/Run.hs` | Create `nodeKernelAccessRef <- newIORef Nothing`, pass through `rpcServerLoop` to `runRpcServer`. Update `rpcServerLoop` signature. |

### Gotchas for the implementer

- **Import narrowing in `Monad.hs`**: when adding the new `Has` instance, ensure `Inject` (used by `putTrace`) is still available.
  Currently it comes from `import Cardano.Api`; if imports are narrowed, import it explicitly from `Cardano.Api.Era`.

- **`RankNTypes` extension.** Both `NodeKernelAccess` and `LedgerSnapshot` use higher-rank fields, requiring the `RankNTypes` extension in `NodeKernelAccess.hs`.

- **`runRpcServer` keeps `NetworkMagic`.** The old `rpcLocalNodeConnectInfo` is still used by existing methods, so `mkLocalNodeConnectInfo` still needs `NetworkMagic`.
  It is dropped only when `rpcLocalNodeConnectInfo` is finally removed in a later piece.

- **`Submit.hs` trace constructor.** `Submit.hs` currently references `TraceRpcSubmitN2cConnectionError` in its `submitTx` helper.
  The trace constructor rename requires a corresponding one-line update in `Submit.hs`: replace `Left $ TraceRpcSubmitN2cConnectionError e` with `Left $ TraceRpcNodeKernelAccessUnavailable` (dropping the exception payload, since the new constructor carries no payload).

- **`SomeException` import**: `Control.Exception` is still needed in `Tracing.hs` because `TraceRpcError` and `TraceRpcFatalError` use `SomeException`.

- **`GrpcException` import**: `withNodeKernelAccess` throws `GrpcException` from `Network.GRPC.Spec`.
  `grpc-spec` is already a dependency of `cardano-rpc`.

- **`RpcConfig.nodeSocketPath` stays**: ADR-019 explicitly notes this.
  The config field remains for deriving `rpcSocketPath` via `nodeSocketPathToRpcSocketPath`.

### Dependencies

- **Upstream:** none (this is the first piece).
- **Downstream:** all pieces 2-8 depend on this (for the `NodeKernelAccess` record, environment wiring, and tracing).

### Testing approach

This piece is primarily a wiring/structural change.
Two ACs have genuine Hedgehog property tests:
- AC2 (`withNodeKernelAccess` behaviour): `H.propertyOnce` covering the `Nothing` and `Just` branches.
- AC5 (tracing pretty-print): `H.propertyOnce` asserting rendered output of the new constructors.

AC1, AC3, AC4 are verified by successful compilation.

## Reference docs

- [Consensus protocol and snapshots](analysis-consensus-protocol.md) - snapshot consistency rationale
- [API signatures](prereqs-api-signatures.md) - `NodeKernelAccess` type design context
- [Implementation details](prereqs-implementation-details.md) - subtle gotchas for the interface
