# Direct Node Access for cardano-rpc

ADR: [ADR-019](../../../../cardano-node-wiki/docs/ADR-019-node-kernel-access-for-cardano-rpc.md)

## Overview

Replace N2C IPC (Unix socket queries to cardano-node) with in-process access to `NodeKernel`.
This covers ledger state queries (via `answerQuery`), block retrieval (via `ChainDB`), and transaction submission (via `Mempool`).
See ADR-019 for the full rationale.

## Reference documents

Technical analysis (split by topic):
- [Architecture and current state](analysis-architecture.md) - cardano-rpc overview, spec coverage, data path
- [UTxO-HD internals](analysis-utxohd-internals.md) - backing store, LedgerTables, forker mechanics
- [Consensus protocol and snapshots](analysis-consensus-protocol.md) - LocalStateQuery, in-memory state, snapshot consistency

Prerequisites (split by topic):
- [API signatures](prereqs-api-signatures.md) - verified consensus and cardano-api type signatures
- [Build and conventions](prereqs-build-and-conventions.md) - project layout, nix build, codebase gotchas
- [Implementation details](prereqs-implementation-details.md) - subtle gotchas, query inventory, verification checklist

Cross-cutting:
- [Implementation plan](implementation-plan.md) - file summary, verification, risks

## Deliverables

Each piece is independently deliverable and testable.
Piece 1 creates the `NodeKernelAccess` abstraction and wires it through cardano-rpc.
Piece 2 adds the FetchBlock proto and handler.
Piece 3 implements `mkNodeKernelAccess` in cardano-node and wires it into the startup sequence, making FetchBlock work end-to-end.
Pieces 4-7 rewrite existing N2C methods to use `NodeKernelAccess`.
Piece 9 adds the FollowTip server-streaming endpoint.
Piece 8 is the final validation.

```
1 (NodeKernelAccess types) ─► 2 (FetchBlock proto) ─► 3 (mkNodeKernelAccess + wiring) ─► 4 (query methods)  ─┐
                                                                             ├► 5 (submit method)  │
                                                                             ├► 6 (eval method)    ├► 8 (integration testing)
                                                                             ├► 7 (node methods)   │
                                                                             └► 9 (FollowTip)     ─┘
```

| # | Deliverable | Scope | Repo |
|---|-------------|-------|------|
| 1 | [NodeKernelAccess types and plumbing](01-node-access-types.md) | `NodeKernelAccess` record, env/monad/tracing/server wiring | cardano-rpc |
| 2 | [FetchBlock proto and handler](02-fetchblock-proto-and-handler.md) | SyncService proto, codegen, `fetchBlockMethod`, server registration | cardano-rpc |
| 3 | [mkNodeKernelAccess and node wiring](03-mk-node-access-and-wiring.md) | `mkNodeKernelAccess` from `NodeKernel`, `Run.hs` IORef wiring, E2E test | cardano-node |
| 4 | [Rewrite query methods](04-rewrite-query-methods.md) | ReadParams, ReadUtxos, SearchUtxos switch to snapshot pattern | cardano-rpc |
| 5 | [Rewrite submit method](05-rewrite-submit-method.md) | SubmitTx switches to nkaSubmitTx | cardano-rpc |
| 6 | [Rewrite eval method](06-rewrite-eval-method.md) | EvalTx (7 queries) switches to snapshot | cardano-rpc |
| 7 | [Rewrite node methods](07-rewrite-node-methods.md) | GetProtocolParamsJson switches to snapshot | cardano-rpc |
| 8 | [Integration testing](08-integration-testing.md) | Validate all tests pass, document coverage gaps | both |
| 9 | [FollowTip streaming](09-follow-tip.md) | Server-streaming chain follower via gRPC | both |

## NodeKernel coverage

`NodeKernel` provides access to three subsystems:

| Subsystem | Access | RPCs covered |
|-----------|--------|-------------|
| **ChainDB** | `getBlockComponent`, `getReadOnlyForkerAtPoint`, `newFollower` | FetchBlock, DumpHistory, FollowTip, ReadTip, all ledger queries |
| **Mempool** | `addLocalTxs`, `getSnapshot` | SubmitTx, ReadMempool |
| **TopLevelConfig** | `getTopLevelConfig` | ReadGenesis, `ExtLedgerCfg` for `answerQuery` |

Two spec RPCs (ReadTx, ReadData) require indexes the node does not build - these need an external indexer.
