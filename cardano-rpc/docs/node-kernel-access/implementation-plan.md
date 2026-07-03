# Node Kernel Access for cardano-rpc: Implementation Plan

Related ADR: [ADR-019](./ADR-019-node-kernel-access-for-cardano-rpc.md)

---

## Overview

Changes span two submodules: **cardano-api** (contains cardano-rpc) and **cardano-node**.
Work the cardano-rpc side first, then the cardano-node side.

Detailed step-by-step implementation is in the numbered story files:

- [01-node-access-types.md](01-node-access-types.md) - `NodeKernelAccess` types, `Env`, `Monad`, `Tracing`, `Server`, and cabal wiring
- [02-fetchblock-proto-and-handler.md](02-fetchblock-proto-and-handler.md) - SyncService proto, codegen, `fetchBlockMethod`, server registration
- [03-mk-node-access-and-wiring.md](03-mk-node-access-and-wiring.md) - `mkNodeKernelAccess` in cardano-node, `Run.hs` wiring, E2E test, tracing
- [04-rewrite-query-methods.md](04-rewrite-query-methods.md) - Rewrite `Query.hs` to use snapshot pattern
- [05-rewrite-submit-method.md](05-rewrite-submit-method.md) - Rewrite `Submit.hs` to use `NodeKernelAccess`
- [06-rewrite-eval-method.md](06-rewrite-eval-method.md) - Rewrite `Eval.hs` to use snapshot pattern
- [07-rewrite-node-methods.md](07-rewrite-node-methods.md) - Rewrite `Node.hs` to use snapshot pattern
- [08-integration-testing.md](08-integration-testing.md) - Integration testing and validation

---

## Current state analysis

### Key code paths being replaced

All query/submit methods currently follow the same N2C pattern:

```haskell
-- 1. Grab the LocalNodeConnectInfo from the reader env
nodeConnInfo <- grab

-- 2. Determine the current era via N2C
AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo

-- 3. Execute a local state query expression over N2C
(result, ...) <- liftIO . (throwEither =<<) $
  executeLocalStateQueryExpr nodeConnInfo VolatileTip $ do
    result <- throwEither =<< throwEither =<< queryXxx sbe ...
    chainPoint <- throwEither =<< queryChainPoint
    blockNo <- throwEither =<< queryChainBlockNo
    pure (result, chainPoint, blockNo)
```

Every query method also runs `querySystemStart` and `queryEraHistory` inside the same `executeLocalStateQueryExpr` call for slot-to-timestamp conversion.

This pattern appears in:
- `Query.hs`: `readParamsMethod`, `readUtxosMethod`, `searchUtxosMethod`
- `Eval.hs`: `evalTxMethod` (queries 7 things: protocol parameters, UTxO by TxIn set, system start, era history, stake deleg deposits, DRep state, stake pool parameters)
- `Submit.hs`: `submitTxMethod` (era detection + `submitTxToNodeLocal`)
- `Node.hs`: `getProtocolParamsJsonMethod` (era detection + query)

### Current environment wiring

```
RpcEnv (Env.hs)
  |- config :: RpcConfig
  |- tracer :: Tracer m TraceRpc
  +- rpcLocalNodeConnectInfo :: LocalNodeConnectInfo  <- REPLACE with IORef

Monad.hs:
  instance Has LocalNodeConnectInfo RpcEnv              <- REPLACE
  type MonadRpc: Has LocalNodeConnectInfo e             <- REPLACE

Server.hs:
  runRpcServer :: Tracer IO TraceRpc -> (RpcConfig, NetworkMagic) -> IO ()
    calls mkLocalNodeConnectInfo to build the env       <- REPLACE
```

### Current tracing (Tracing.hs + Rpc.hs)

```
TraceRpcSubmit
  |- TraceRpcSubmitN2cConnectionError SomeException    <- REMOVE
  |- TraceRpcSubmitTxDecodingError DecoderError        (keep)
  |- TraceRpcSubmitTxValidationError ...               (keep)
  +- TraceRpcSubmitSpan TraceSpanEvent                 (keep)
```

The N2C error is wrapped in `Submit.hs` via `tryAny` + `first TraceRpcSubmitN2cConnectionError`.

---

## File summary

### New files (2)

| File | Purpose |
|------|---------|
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/NodeKernelAccess.hs` | `NodeKernelAccess` record + `withNodeKernelAccess` |
| `cardano-node/cardano-node/src/Cardano/Node/Rpc/NodeKernelAccess.hs` | `mkNodeKernelAccess` from `NodeKernel` |

### Modified files (12)

| File | Change | Story |
|------|--------|-------|
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server.hs` | New signature, re-export `NodeKernelAccess` + `LedgerSnapshot`, IORef wiring, register `methodsSyncRpc` | [01](01-node-access-types.md), [02](02-fetchblock-proto-and-handler.md) |
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/Env.hs` | `RpcEnv` holds `IORef`, drop `LocalNodeConnectInfo` | [01](01-node-access-types.md) |
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/Monad.hs` | `Has` instance + `MonadRpc` update | [01](01-node-access-types.md) |
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/Tracing.hs` | Replace N2C trace with ledger-access traces, add `TraceRpcSync` | [01](01-node-access-types.md) |
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Query.hs` | Rewrite 3 query methods to use snapshot pattern | [04](04-rewrite-query-methods.md) |
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Eval.hs` | Rewrite `evalTxMethod` to use snapshot pattern (7 queries) | [06](06-rewrite-eval-method.md) |
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Submit.hs` | Rewrite submit with snapshot for era detection | [05](05-rewrite-submit-method.md) |
| `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/Node.hs` | Rewrite `getProtocolParamsJsonMethod` to use snapshot pattern | [07](07-rewrite-node-methods.md) |
| `cardano-api/cardano-rpc/cardano-rpc.cabal` | Add `NodeKernelAccess` module, proto gen modules | [01](01-node-access-types.md), [02](02-fetchblock-proto-and-handler.md) |
| `cardano-node/cardano-node/src/Cardano/Node/Run.hs` | IORef + writeIORef in kernel hook | [03](03-mk-node-access-and-wiring.md) |
| `cardano-node/cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs` | Update trace instances | [03](03-mk-node-access-and-wiring.md) |
| `cardano-node/cardano-node/cardano-node.cabal` | Add new module | [03](03-mk-node-access-and-wiring.md) |

### Unchanged

| File | Why |
|------|-----|
| `cardano-rpc/src/Cardano/Rpc/Server/Config.hs` | `nodeSocketPath` still needed for socket path derivation |
| `cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Type.hs` | Conversion code untouched |
| `cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Predicate.hs` | Filtering untouched |
| Testnet test files | gRPC client API unchanged |

---

## Verification

1. `nix build .#cardano-rpc` in cardano-api submodule
2. `nix build .#cardano-rpc:test:cardano-rpc-test` - unit tests
3. `nix build .#cardano-node` in cardano-node submodule
4. Integration tests via testnet (if available)

---

## Risks and mitigations

| Risk | Status | Mitigation |
|------|--------|------------|
| `answerQuery` API differs from expected signature | **Verified** (2026-05-22) | Signature matches: `ExtLedgerCfg blk -> ReadOnlyForker' m blk -> Query blk result -> m result` |
| Forker lifecycle - leaking forkers on exceptions | **Addressed** | `bracket` pattern in `nkaWithSnapshot`; `withRegistry` provides additional safety net |
| `QueryCurrentEra` not available via `toConsensusQuery` | **Verified: no risk** | `toConsensusQuery` handles `QueryCurrentEra` by wrapping as `BlockQuery (QueryHardFork GetCurrentEra)` |
| `addLocalTxs` API changed in recent consensus | **Verified** (2026-05-22) | Name and signature confirmed; use `MkSolo` constructor on GHC 9.10 |
| Concurrent reads during kernel initialisation race | Low risk | `IORef` write in `rnNodeKernelHook` happens before any RPC can succeed; `withNodeKernelAccess` checks atomically |
| `Target` / `withRegistry` import paths wrong | **Fixed** | `Target` from `Ouroboros.Network.Protocol.LocalStateQuery.Type`; `withRegistry` from `Control.ResourceRegistry` |
