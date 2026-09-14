# Implementation Details and Verification

Subtle implementation gotchas, per-method query inventory, and verification checklist.

---

## 4. Subtle Implementation Details

### Snapshot Consistency (IMPORTANT)

The current N2C code runs all queries within one `executeLocalStateQueryExpr` call, acquiring a single ledger snapshot.
Protocol parameters, UTxO results, chain tip, and block number all come from the same ledger state.

The `NodeKernelAccess` design must preserve this: use `nkaWithSnapshot` to group all queries for a single RPC handler under one `ReadOnlyForker`.
Do NOT call `nkaWithSnapshot` multiple times within a single handler - that would break consistency.

This is especially critical for `EvalTx`, which queries 7 different things that must be mutually consistent:
1. `queryProtocolParameters` - needed for tx evaluation
2. `queryUtxo` (by TxIn set) - inputs being spent
3. `querySystemStart` - for slot-to-time conversion
4. `queryEraHistory` - for slot-to-time conversion
5. `queryStakeDelegDeposits` - for deposit tracking
6. `queryDRepState` - for governance-related evaluation
7. `queryStakePoolParameters` - for delegation-related evaluation

If these came from different ledger states, tx evaluation could produce wrong results or spurious failures.

### 4.1 The `eon` existential must escape the snapshot callback

The `eon` value (from `forEraInEon @Era era ...`) is needed after the `nkaWithSnapshot`
callback for protobuf conversion.
The callback must return it as part of its result tuple:

```haskell
-- CORRECT: eon escapes the callback
(pparams, chainPoint, blockNo, eon) <- liftIO $ withNodeKernelAccess laRef $ \la -> do
  nkaWithSnapshot la $ \snapshot -> do
    AnyCardanoEra era <- runQuery snapshot QueryCurrentEra
    eon <- forEraInEon @Era era (error "Minimum Conway era required") pure
    ...
    pure (pparams, chainPoint, blockNo, eon)

-- WRONG: eon would be scoped inside the callback only
liftIO $ withNodeKernelAccess laRef $ \la -> do
  nkaWithSnapshot la $ \snapshot -> do
    ...
  -- can't use eon out here for protobuf conversion
```

This works because `eon` is an existential that's pattern-matched later.
The tuple captures the existential witness.

### 4.2 `RankNTypes` in `NodeKernelAccess` and `LedgerSnapshot`

Both `NodeKernelAccess` and `LedgerSnapshot` use higher-rank quantification:
```haskell
nkaWithSnapshot :: forall a. (LedgerSnapshot -> IO a) -> IO a
runQuery       :: forall result. QueryInMode result -> IO result
```

This requires `{-# LANGUAGE RankNTypes #-}` on `NodeKernelAccess.hs`.
The `forall a.` on `nkaWithSnapshot` ensures the snapshot cannot escape the callback scope.
The `forall result.` on `runQuery` lets callers pass any `QueryInMode` variant and get the corresponding result type back.

### 4.3 `throwEither` / `throwExceptT` still needed in Submit.hs

After the rewrite, `Query.hs` and `Node.hs` no longer need `throwExceptT` or the double `throwEither` pattern (because `NodeKernelAccess` callbacks throw directly on error).
But `Submit.hs` still uses `throwEither` via `putTraceThrowEither` for tx validation errors.
Don't remove the `Error` module import from `Submit.hs`.

### 4.4 `SomeException` import in Tracing.hs

After removing `TraceRpcSubmitN2cConnectionError SomeException`, the `Control.Exception` import for `SomeException` is **still needed** because `TraceRpc` (defined in the same module) uses `SomeException` in `TraceRpcError` and `TraceRpcFatalError`.

### 4.5 The `SearchUtxos` tracing gap (verified: no bug)

`TraceRpcQuerySearchUtxosSpan` is already fully wired in `forMachine`, `asMetrics`, and the `MetaTrace` instance in `cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs`.
The originally reported pre-existing bug does not exist in the current codebase.
Step 11 of the implementation plan only needs to swap the N2C trace constructors.

### 4.6 `Net.Tx.SubmitFail` / `Net.Tx.SubmitSuccess` still needed

After the rewrite, `Submit.hs` still pattern-matches on `SubmitFail` / `SubmitSuccess` from `Cardano.Api.Network.IPC`.
Don't remove that qualified import.

### 4.7 Config.hs backward compatibility

The plan **keeps** `nodeSocketPath` in `RpcConfig`.
The earlier design iteration wanted to remove it, but analysis showed it would break:
- `cardano-node/cardano-testnet/src/Testnet/Types.hs` (line 155: `nodeRpcSocketPath`)
- `cardano-node/cardano-node/src/Cardano/Node/Configuration/POM.hs`
- `makeRpcConfig` uses it to derive the default `rpcSocketPath`

The field just isn't used at runtime for N2C connections anymore.

### 4.8 Submit.hs uses both `nkaWithSnapshot` and `nkaSubmitTx`

The submit method needs two separate interactions with `NodeKernelAccess`:
1. First, `nkaWithSnapshot` to query the current era (via `runQuery QueryCurrentEra`) for tx deserialisation.
2. Then `nkaSubmitTx` (after the tx is deserialised and validated) to submit the transaction.

Era determination now happens inside the snapshot callback via `runQuery` rather than a dedicated `laDetermineEra` field.
Tx deserialisation is pure/monadic and happens between the two calls.
Do not fold both into a single `nkaWithSnapshot` - `nkaSubmitTx` is a separate operation that goes to the mempool, not the ledger snapshot.

### 4.9 Era mismatch in `runQuery` result unwrapping

`fromConsensusQueryResult` for era-specific queries (pparams, utxo) returns `Either EraMismatch result`.
The `runQuery` implementation inside `mkNodeKernelAccess` must unwrap this:

```haskell
-- Inside the runQuery implementation for era-specific queries:
raw <- answerQuery cfg forker consensusQuery
case fromConsensusQueryResult qim consensusQuery raw of
  Left eraMismatch -> throwIO (userError $ "Era mismatch: " <> show eraMismatch)
  Right result     -> pure result
```

With the snapshot-based design, all queries within one `nkaWithSnapshot` call share the same `ReadOnlyForker`, so era mismatches between queries inside a single snapshot are impossible.
The only cross-snapshot era race is in `Submit.hs` (see 4.8): era detection uses one `nkaWithSnapshot` call, then `nkaSubmitTx` is a separate call - an era transition between them is theoretically possible at hard fork boundaries but extremely rare.
Throwing is acceptable since the client can retry.

### 4.10 `QueryCurrentEra` routes through `toConsensusQuery` (verified)

`toConsensusQuery` handles `QueryCurrentEra` by wrapping it as `BlockQuery (QueryHardFork GetCurrentEra)` internally.
No special handling is needed - just call `runQuery snapshot QueryCurrentEra` inside the `nkaWithSnapshot` callback.

### 4.11 `runQuery` helper must use `bracket` for forker lifecycle (fixed in plan)

The implementation plan's `runQuery` snippet now uses `bracket` for exception-safe forker cleanup.
`withRegistry` also provides a safety net (auto-closes registered resources when its scope exits), but explicit `bracket` around `roforkerClose` is still preferred for deterministic cleanup.

### 4.12 IORef thread safety during startup

The `IORef (Maybe NodeKernelAccess)` has one writer (`rnNodeKernelHook`) and multiple readers (gRPC handler threads).
This is safe because:
- GHC's `IORef` guarantees atomicity for single-word writes
- The write is `Nothing -> Just la` (a single pointer write)
- Readers see either `Nothing` (return UNAVAILABLE) or `Just la` (proceed)
- No read-modify-write cycle exists

There is NO race condition here.
But if someone later adds a second write site, this assumption breaks.
Document it in a comment in `Run.hs`.

### 4.13 `Solo` constructor name on GHC 9.10 (verified)

GHC 9.10 uses `MkSolo` as the constructor.
Import from `Data.Tuple` (not `GHC.Tuple`):
```haskell
import Data.Tuple (Solo (..))

MkSolo addTxRes <- addLocalTxs mempool (MkSolo genTx)
```

This matches the pattern used in ouroboros-consensus's own `LocalTxSubmission/Server.hs`.

### 4.14 `getReadOnlyForkerAtPoint` takes `ResourceRegistry` from where? (verified)

Each call to `getReadOnlyForkerAtPoint` needs a `ResourceRegistry`.
Use `withRegistry` from `Control.ResourceRegistry` to create a fresh one per query.
This matches the pattern used by the existing N2C LocalStateQuery server in `ouroboros-consensus-diffusion/Network/NodeToClient.hs`.

Registry cleanup does automatically close registered forkers when the registry scope exits.
Using `bracket` with `roforkerClose` inside `withRegistry` is still good practice for explicit cleanup, but the registry provides a safety net if an exception bypasses it.

### 4.15 The `asType` in `deserialiseTx` comes from where?

In `Submit.hs`, the existing code uses `asType` (line 52):
```haskell
deserialiseTx sbe = shelleyBasedEraConstraints sbe $ deserialiseFromCBOR asType
```

After the rewrite, this code is unchanged but `asType` comes from `Cardano.Api` (re-exported by the blanket `import Cardano.Api`).
If you change the `Cardano.Api` import to be more specific, make sure `AsType` / `asType` is still in scope.
It's from `Cardano.Api.Serialise.SerialiseUsing` or `Cardano.Api.Serialise.Cbor`.

### 4.16 `NoFieldSelectors` on `Env.hs`

`Env.hs` has `{-# LANGUAGE NoFieldSelectors #-}`.
This means `RpcEnv` fields like `rpcNodeKernelAccess` are NOT available as accessor functions.
The `Has` instance in `Monad.hs` must use `NamedFieldPuns`:

```haskell
-- This works (NamedFieldPuns):
instance Has (IORef (Maybe NodeKernelAccess)) RpcEnv where
  obtain RpcEnv{rpcNodeKernelAccess} = rpcNodeKernelAccess

-- This does NOT work (NoFieldSelectors blocks it):
instance Has (IORef (Maybe NodeKernelAccess)) RpcEnv where
  obtain = rpcNodeKernelAccess  -- ERROR: not a function
```

The existing `Has LocalNodeConnectInfo RpcEnv` instance already uses `NamedFieldPuns`, so just follow the same pattern.

### 4.17 Removing `import Cardano.Api` is high-risk

Three files (Env.hs, Monad.hs, Server.hs) currently have blanket `import Cardano.Api` which re-exports hundreds of names.
The plan replaces these with specific imports.
This is the most likely source of compilation errors because it's easy to miss a name that was silently in scope.

**Strategy:** For each file where `import Cardano.Api` is removed:
1. Remove the import
2. Try to build
3. Add specific imports for each "not in scope" error
4. Repeat until clean

Files that **keep** `import Cardano.Api` (because they use many names from it):
- `Query.hs` - uses `AnyCardanoEra`, `forEraInEon`, `Era`, `convert`, `ShelleyBasedEra`, `UTxO`, `TxIn`, `TxIx`, `TxOut`, `CtxUTxO`, `QueryUTxOFilter`, `ChainPoint`, `BlockNo`, `WithOrigin`, `serialiseToRawBytesHexText`, `serialiseToRawBytes`, `deserialiseFromRawBytes`, `AsTxId`, `IsEra`, `obtainCommonConstraints`, `fromList`, etc.
- `Submit.hs` - uses `AnyCardanoEra`, `forEraInEon`, `ShelleyBasedEra`, `Tx`, `TxId`, `TxInMode`, `shelleyBasedEraConstraints`, `deserialiseFromCBOR`, `getTxId`, `getTxBody`, `serialiseToRawBytes`, etc.
- `Node.hs` - uses `AnyCardanoEra`, `forEraInEon`, `Era`, `convert`, `obtainCommonConstraints`, etc.

For these files, **keep `import Cardano.Api`** and just add the `NodeKernelAccess` import alongside it.

### 4.18 `SubmitResult` and `TxValidationErrorInCardanoMode` imports

`SubmitResult` and `TxValidationErrorInCardanoMode` need explicit imports - they are NOT re-exported from the top-level `Cardano.Api` module.
Import both from `Cardano.Api.Network.IPC` (or alternatively, `SubmitResult` is available via `Cardano.Api.Network` and originates from `Ouroboros.Network.Protocol.LocalTxSubmission.Type`).

This matters for `NodeKernelAccess.hs` which uses both in its type signature.

### 4.19 No new dependencies required

`cardano-ledger-core` and `grpc-spec` are already listed in the `cardano-rpc.cabal` dependencies.
The `NodeKernelAccess` interface uses only cardano-api types, so no consensus dependencies are needed in cardano-rpc.
The `mkNodeKernelAccess` implementation in cardano-node already has all necessary consensus dependencies.

### 4.20 `withNodeKernelAccess` throws gRPC `UNAVAILABLE` during startup

The chosen variant is `withNodeAccessOrUnavailable`, which reads the `IORef (Maybe NodeKernelAccess)` and throws a gRPC `UNAVAILABLE` status code if the value is `Nothing`.
This cleanly handles the startup window before `rnNodeKernelHook` writes the `Just` value.
The gRPC client can retry on `UNAVAILABLE`, which is the standard practice for transient unavailability.

---

## 5. RPC Method Query Inventory

This section lists the exact queries each RPC method executes within a single `executeLocalStateQueryExpr` call (i.e. a single ledger snapshot).
All queries within one call are consistent - they see the same ledger state.

### ReadParams (in `Query.hs`)

1. `queryProtocolParameters` - protocol parameters for the current era
2. `queryChainPoint` - chain tip
3. `queryChainBlockNo` - block number at tip
4. `querySystemStart` - for slot-to-timestamp conversion (via `slotToTimestamp`)
5. `queryEraHistory` - for slot-to-timestamp conversion (via `slotToTimestamp`)

### ReadUtxos (in `Query.hs`)

1. `queryUtxo` (by TxIn set or whole) - the requested UTxOs
2. `queryChainPoint` - chain tip
3. `queryChainBlockNo` - block number at tip
4. `querySystemStart` - for slot-to-timestamp conversion (via `slotToTimestamp`)
5. `queryEraHistory` - for slot-to-timestamp conversion (via `slotToTimestamp`)

### SearchUtxos (in `Query.hs`)

1. `queryUtxo` (by address set or whole, then client-side predicate filtering + pagination)
2. `queryChainPoint` - chain tip
3. `queryChainBlockNo` - block number at tip
4. `querySystemStart` - for slot-to-timestamp conversion (via `slotToTimestamp`)
5. `queryEraHistory` - for slot-to-timestamp conversion (via `slotToTimestamp`)

### EvalTx (in `Eval.hs`)

This is the most query-intensive method, requiring 7 queries within one snapshot:

1. `queryProtocolParameters` - needed for tx evaluation
2. `queryUtxo` (by TxIn set) - inputs being spent
3. `querySystemStart` - for slot-to-time conversion
4. `queryEraHistory` - for slot-to-time conversion
5. `queryStakeDelegDeposits` - for deposit tracking
6. `queryDRepState` - for governance-related evaluation
7. `queryStakePoolParameters` - for delegation-related evaluation

### GetProtocolParamsJson (in `Node.hs`)

1. `queryProtocolParameters` - protocol parameters as JSON

### SubmitTx (in `Submit.hs`)

Uses `submitTxToNodeLocal` (LocalTxSubmission mini-protocol), not LocalStateQuery.
In the direct access design, this maps to `nkaSubmitTx` (mempool `addLocalTxs`).

### Slot-to-timestamp conversion note

`ReadParams`, `ReadUtxos`, and `SearchUtxos` all query `querySystemStart` and `queryEraHistory` for slot-to-timestamp conversion via the `slotToTimestamp` helper.
`EvalTx` also queries both for slot-to-time conversion.
The original plan did not account for these - they must be included in the `LedgerSnapshot` queries.
Both are handled by `answerQuery`: `GetSystemStart` is extracted from config, and `queryEraHistory` maps to `BlockQuery (QueryHardFork GetInterpreter)`.

---

## 7. Verification Checklist

After implementing all steps:

- [ ] `nix build 'path:/work/cardano-api#cardano-rpc:lib:cardano-rpc' --allow-import-from-derivation --accept-flake-config`
- [ ] `nix build 'path:/work/cardano-api#cardano-rpc:test:cardano-rpc-test' --allow-import-from-derivation --accept-flake-config`
- [ ] `nix build 'path:/work/cardano-node#cardano-node:lib:cardano-node' --allow-import-from-derivation --accept-flake-config` (adjust path for worktree)
- [ ] No `-Wunused-packages` warnings
- [ ] No `-Wredundant-constraints` warnings
- [ ] hlint passes (backtick-infix, no mixed styles)
- [ ] Integration tests pass (testnet gRPC tests exercise the full path)
- [ ] Startup window test: gRPC query before kernel init returns `UNAVAILABLE`

---

## 8. Files in this documentation set

| File | Purpose |
|------|---------|
| `ADR-019-node-kernel-access-for-cardano-rpc.md` | Architecture Decision Record |
| `implementation-plan.md` | Step-by-step implementation plan with before/after code |
| `analysis-architecture.md` | Architecture and current state |
| `analysis-utxohd-internals.md` | UTxO-HD internals |
| `analysis-consensus-protocol.md` | Consensus protocol and snapshot consistency |
| `prereqs-api-signatures.md` | API signatures and type references |
| `prereqs-build-and-conventions.md` | Build instructions and conventions |
| `prereqs-implementation-details.md` | **This file** - implementation details, query inventory, and verification checklist |
