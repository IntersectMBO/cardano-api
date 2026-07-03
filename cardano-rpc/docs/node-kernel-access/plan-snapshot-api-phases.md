# Snapshot API implementation phases

Builds the ledger-snapshot and mempool-submit capability on `NodeKernelAccess` that pieces 4-7 assume but piece 1 deferred.
Piece 1 shipped a deliberately thin record (`chainDb`, `systemStart`, `readEraHistory`, `securityParam`, `genesisConfig`) because direct `chainDb` access was enough for FetchBlock, ReadTip, FollowTip and ReadGenesis.
The remaining N2C methods - ReadParams, ReadUtxos, SearchUtxos (piece 4), SubmitTx (piece 5), EvalTx (piece 6) and GetProtocolParamsJson (piece 7) - all need ledger state queries or mempool submission, which nothing in the current record provides.
This plan is the gate between piece 3 (done) and pieces 4-7: once it lands, those rewrites become mechanical and can proceed in parallel.

Each phase compiles independently and can be reviewed as a separate commit.

## Related documents

- [01-node-access-types.md](01-node-access-types.md) - the original snapshot design; its "Status: as built" section records the deferral this plan now pays off.
- [04-rewrite-query-methods.md](04-rewrite-query-methods.md), [05-rewrite-submit-method.md](05-rewrite-submit-method.md), [06-rewrite-eval-method.md](06-rewrite-eval-method.md), [07-rewrite-node-methods.md](07-rewrite-node-methods.md) - the consumers this plan unblocks.
- [prereqs-api-signatures.md](prereqs-api-signatures.md) - verified consensus signatures (`answerQuery`, `getReadOnlyForkerAtPoint`, `addLocalTxs`, `withRegistry`), dated 2026-05-22.
- [analysis-consensus-protocol.md](analysis-consensus-protocol.md) - snapshot consistency rationale and `answerQuery` footprint dispatch.
- [analysis-utxohd-internals.md](analysis-utxohd-internals.md) - forker mechanics and the three read tiers.

## Naming: plan docs vs as-built conventions

Pieces 4-7 were written against piece 1's original names: `nkaWithSnapshot`, `nkaSubmitTx`, `LedgerSnapshot`, `withNodeKernelAccess`.
The as-built code took a different shape: the record lives in `Cardano.Rpc.Server.NodeKernelAccess.Type` with `NoFieldSelectors` and unprefixed punned fields, and handlers reach it via `grabNodeKernelAccess` (there is no `withNodeKernelAccess`).
This plan follows the as-built conventions:

| Plan-doc name (pieces 4-7) | As-built name |
|---|---|
| `nkaWithSnapshot` | `withSnapshot` field on `NodeKernelAccess` |
| `nkaSubmitTx` | `submitTx` field on `NodeKernelAccess` |
| `withNodeKernelAccess laRef $ \na -> ...` | `na <- grabNodeKernelAccess` |
| `LedgerSnapshot` / `runQuery` | unchanged |

## Phase 0: re-verify consensus signatures

Doc-only phase, no code.
The signatures in [prereqs-api-signatures.md](prereqs-api-signatures.md) were verified on 2026-05-22 against the checked-out source, and nothing in cardano-rpc calls them yet, so drift would be invisible until now.
Re-verify against the pinned versions (haskdogs cache, `~/.cache/haskdogs/<pkg>-<version>/`, not the sibling checkouts) before writing code:

- `answerQuery` - constraint list and the `ExtLedgerCfg blk` / `ReadOnlyForker' m blk` parameters.
- The ChainDB API-level accessor for acquiring a `ReadOnlyForker`.
  prereqs documents the Impl-level `getReadOnlyForkerAtPoint :: ChainDbEnv m blk -> ...`; the code must use whatever the `ChainDB m blk` record (or `Ouroboros.Consensus.Storage.ChainDB.API`) exposes, since `NodeKernelAccess` holds the abstract `ChainDB`, not `ChainDbEnv`.
- `Target` (from `Ouroboros.Network.Protocol.LocalStateQuery.Type`) and `GetForkerError` constructors.
- `addLocalTxs`, `MempoolAddTxResult`, and the `MkSolo` gotcha on GHC 9.10.
- `toConsensusQuery` / `fromConsensusQueryResult` and `toConsensusGenTx` / `fromConsensusApplyTxErr` (cardano-api side, so drift is unlikely, but they are load-bearing here).

Output: prereqs-api-signatures.md updated if anything moved, or its "verified as of" date refreshed.

## Phase 1: tracing groundwork

Atomic: trace constructor changes must build in lockstep with cardano-node's hand-maintained clause lists.

- Re-add `TraceRpcForkerError` to `Tracing.hs` (it existed once and was removed as unused in commit 3dcd5b4d8; reinstate it where that commit removed it from, with a `Pretty` instance rendering "Forker error: <msg>").
- Handle it in cardano-node's `Cardano/Node/Tracing/Tracers/Rpc.hs`: `forMachine`, `namespaceFor`, `severityFor` (Error), `documentFor`, `allNamespaces`.
  It is not a metric, so `asMetrics`/`metricsDocFor` keep their catch-all defaults.

No behaviour change: the constructor has no producer until phase 2.

Build: `cabal build cardano-rpc && cabal build cardano-node`

## Phase 2: `LedgerSnapshot` type and `withLedgerSnapshot` function

Introduces the snapshot machinery as a standalone function, not yet reachable by handlers.
The whole forker lifecycle is one reviewable diff in one module.

- `NodeKernelAccess/Type.hs`: add `newtype LedgerSnapshot = LedgerSnapshot { runQuery :: forall result. QueryInMode result -> IO result }` (needs `RankNTypes`).
- `NodeKernelAccess.hs`: add `withLedgerSnapshot` taking the tracer, the `ChainDB` and the `TopLevelConfig`, plus the callback.
  It acquires a `ReadOnlyForker` at `VolatileTip` inside `withRegistry`, brackets `roforkerClose`, and passes a `LedgerSnapshot` built from the forker to the callback.
  `runQuery q` = `toConsensusQuery q` (unwrap the `Some` existential), `answerQuery (ExtLedgerCfg topLevelConfig) forker`, then `fromConsensusQueryResult q`.
  The function is monomorphic in `CardanoBlock StandardCrypto`, matching `readGenesisBundle`; the caller (phase 3) has that equality in scope from its `CardanoBlockType` match.
- A `Left GetForkerError` from acquisition at `VolatileTip` should be impossible (`PointNotOnChain`/`PointTooOld` require a specific point); trace `TraceRpcForkerError` and throw `GrpcException` with `GrpcInternal`.

Build: `cabal build cardano-rpc`

## Phase 3: `withSnapshot` field on `NodeKernelAccess`

Two-line wiring change, trivially reviewable.

- `NodeKernelAccess/Type.hs`: add field `withSnapshot :: forall a. (LedgerSnapshot -> IO a) -> IO a`.
  Keeping the higher-rank continuation as a record field means unit tests can install a mock snapshot without a ChainDB.
- `NodeKernelAccess.hs`: populate it in `mkNodeKernelAccess` as `withLedgerSnapshot tracer chainDb topLevelConfig` (all three already in scope inside the `CardanoBlockType` match).
- Export `LedgerSnapshot (..)` wherever handlers will need it (follow how the existing fields are re-exported via `Cardano.Rpc.Server.NodeKernelAccess`).

Build: `cabal build cardano-rpc && cabal build cardano-node`

## Phase 4: first consumer - port GetProtocolParamsJson (piece 7)

Deliver [07-rewrite-node-methods.md](07-rewrite-node-methods.md) as the next commit so the snapshot API does not land unused and untested.
It is the smallest rewrite, exercises the full path (`QueryCurrentEra` + `QueryProtocolParameters` in one snapshot, the `eon` existential returned out of the callback), and `hprop_rpc_query_pparams` gives it E2E coverage.

Build: `cabal build cardano-rpc && TASTY_PATTERN='/pparams/' cabal test cardano-testnet-test`

## Phase 5: `submitTx` capability

Independent of phases 2-4 (depends only on phase 0's verification); one commit covering function and field, since there is no bracket lifecycle to review separately.

- `NodeKernelAccess/Type.hs`: add field `submitTx :: TxInMode -> IO (SubmitResult TxValidationErrorInCardanoMode)`.
- `NodeKernelAccess.hs`: implement via `toConsensusGenTx`, `addLocalTxs (Consensus.getMempool nodeKernel) (MkSolo genTx)`, then map `MempoolTxAdded -> SubmitSuccess` and `MempoolTxRejected _ err -> SubmitFail (fromConsensusApplyTxErr err)`.

No consumer until phase 6, so no behaviour change.

Build: `cabal build cardano-rpc`

## Phase 6: port SubmitTx (piece 5)

Deliver [05-rewrite-submit-method.md](05-rewrite-submit-method.md): two deliberately separate access calls (era detection via `withSnapshot`, submission via `submitTx`), so no forker is held open during mempool insertion.

Build: `cabal build cardano-rpc && TASTY_PATTERN='/transaction/' cabal test cardano-testnet-test`

## Downstream: pieces 4 and 6, then IPC removal

With phase 4 as the template, piece 4 ([Query.hs](04-rewrite-query-methods.md)) and piece 6 ([Eval.hs](06-rewrite-eval-method.md)) proceed per their own docs, in parallel with phases 5-6 if desired - all touch disjoint files.
Keep the same commit discipline there:

- Piece 4: one commit per method (`readParamsMethod`, `readUtxosMethod`, `searchUtxosMethod`), each independently green; an optional leading commit extracts a shared `withQuerySnapshot` helper if the duplication warrants it.
- Piece 6: one method but the highest-risk rewrite (seven queries that must share one snapshot, and no automated E2E test today); a single commit, reviewed against piece 6's AC list.

The last rewrite to land also removes the dead IPC plumbing, as its own commit:

- `rpcLocalNodeConnectInfo` field and `mkLocalNodeConnectInfo` in `Env.hs`.
- The `Has LocalNodeConnectInfo RpcEnv` instance and the corresponding `MonadRpc` constraint in `Monad.hs`.
- The `mkLocalNodeConnectInfo` call and `NetworkMagic` threading in `runRpcServer` (`Server.hs`).
- `RpcConfig.nodeSocketPath` **stays**: it is still needed to derive the RPC socket path via `nodeSocketPathToRpcSocketPath` (ADR-019 notes this explicitly).

## Phase dependency graph

```
Phase 0 (re-verify signatures)
  |
  +--------------------+
  v                    v
Phase 1 (tracing)    Phase 5 (submitTx capability)
  |                    |
  v                    v
Phase 2 (withLedgerSnapshot)   Phase 6 (piece 5 port)
  |
  v
Phase 3 (withSnapshot field)
  |
  v
Phase 4 (piece 7 port) ──► piece 4, piece 6 ──► IPC removal ──► piece 8
```

Phases 5-6 only need phase 0, but phase 6's rewrite also uses `withSnapshot` for era detection, so in practice it lands after phase 3.

## Design decisions

- **`runQuery` stays `QueryInMode`-typed.**
  Handlers keep the query vocabulary they already use, the cardano-api conversion pair (`toConsensusQuery`/`fromConsensusQueryResult`) is reused as-is, and pieces 4-7 were written against this shape.
  The alternative - exposing raw consensus `Query blk` - would save one conversion layer but rewrite every handler's query code and leak consensus types into method files.
- **Forker target is always `VolatileTip`.**
  Every current method queries the node's tip; point-pinned snapshots can be added later if a method needs them.
- **SearchUtxos keeps read-all-then-slice.**
  `answerQuery` handles the `SQFTraverseTables` footprint internally with batched range reads, so the port inherits today's semantics.
  Pushing pagination down to `roforkerRangeReadTables` (see [analysis-utxohd-internals.md](analysis-utxohd-internals.md)) is a separate optimisation, out of scope here.
- **Queries needing state consistency go through the snapshot, not the record fields.**
  `NodeKernelAccess` already has `systemStart` and `readEraHistory` fields (used by the Sync methods), but pieces 4-7 run `QuerySystemStart`/`QueryEraHistory` inside the snapshot callback so the values are consistent with the queried chain point (piece 4's AC5).
  The record fields remain for callers that do not hold a snapshot.

## Delivery slicing

Each PR needs its own new changelog fragment (herald counts only fragments added since the fork point).
Phases map one-to-one onto commits; PRs group them:

1. PR: phases 0-4 (snapshot API, one commit per phase, plus the piece 7 port as proof of use).
2. PR: piece 4 (Query.hs, one commit per method).
3. PR: phases 5-6 (submitTx capability + Submit.hs port).
4. PR: piece 6 (Eval.hs), plus the IPC-removal commit if it lands last.
5. Piece 8 (integration testing) closes: E2E for EvalTx (none exists today) and un-hardcoding the `timestamp === 0` assertion in `hprop_rpc_query_pparams`.
