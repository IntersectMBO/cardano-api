# Architecture and Current State

This document covers cardano-rpc's current architecture, spec coverage, and data flow.

---

## cardano-rpc Implementation Overview

**Three gRPC services** defined in proto files:

| Service | Methods | Proto |
|---------|---------|-------|
| **Node** (custom) | `GetEra`, `GetProtocolParamsJson` | `cardano/rpc/node.proto` |
| **QueryService** (UTxO RPC) | `ReadParams`, `ReadUtxos`, `SearchUtxos` | `utxorpc/v1beta/query/query.proto` |
| **SubmitService** (UTxO RPC) | `SubmitTx`, `EvalTx` | `utxorpc/v1beta/submit/submit.proto` |

**Architecture:**
- `RpcEnv` holds config, tracer, and node connection info - injected via `Has` typeclass + `MonadRpc` constraint.
- Server runs on a **Unix domain socket** (insecure/local IPC), default `rpc.sock` next to the node socket.
- Queries the node via standard Node-to-Client local state queries.
- Approximately 2000 lines across 16 modules plus generated proto-lens code in `gen/`.

**Key modules:**
- `Server.hs` - entry point, registers method groups, top-level exception handler.
- `Type.hs` (~600 lines) - bidirectional conversions between cardano-api/ledger types and protobuf.
- `Predicate.hs` - UTxO pattern matching (address, asset, composite predicates) with address extraction optimisation.
- `Query.hs` - pagination (token = `TxId#OutputIndex`, default 100 items), deterministic sort by TxIn.
- `Submit.hs` - CBOR deserialisation -> era validation -> node submission.

**Node integration** (in the worktree):
- `--grpc-enable` / `--grpc-socket-path` CLI flags.
- `EnableRpc` / `RpcSocketPath` config keys.
- Runs concurrently via `withAsync` in `Cardano.Node.Run`.
- Structured tracing with metrics (`rpc.request.QueryService.*`, etc.).

**Integration tests** in `cardano-testnet/test/`:
- `hprop_rpc_query_pparams` - validates all 44 protocol params match ledger.
- `hprop_rpc_transaction` - full round-trip: fetch UTxOs -> build tx -> submit via RPC -> confirm on-chain.

---

### ADR-018

**Primary document:** `ADR-018-cardano-rpc-grpc-server.md`

**Status:** Proposed (2026-03-11)

**Key decisions:**
- Implements the **UTxO RPC** standard (`utxorpc.org`) plus custom Cardano extensions.
- **Opt-in and experimental** - must be explicitly enabled.
- Unix socket keeps the same local-access-only security model as the existing node socket.
- Remote access via **Envoy reverse proxy** with TLS, rate limiting, and mTLS auth.

**Current limitations acknowledged:**
- Serialisation overhead (CBOR encode/decode round-trips).
- Connection-per-request cost (Ouroboros handshake).
- Sequential mini-protocol bottleneck.

**Planned improvements:**
- Direct ledger state access (in-memory TVar/STM reads, bypassing IPC).
- Connection pooling over persistent IPC connections.

**Dependencies:** `grapesy`, `grpc-spec`, `proto-lens`.

---

## UTxO RPC Spec Coverage

Current cardano-rpc implements a subset of the UTxO RPC v1beta specification.
The full spec defines four services; cardano-rpc currently covers two partially.

**QueryService** (8 spec RPCs):

| RPC | Implemented | Notes |
|-----|-------------|-------|
| ReadParams | Yes | Protocol parameters |
| ReadUtxos | Yes | UTxO lookup by TxIn |
| SearchUtxos | Yes | UTxO search by predicate |
| ReadData | No | Datum by hash - needs PlutusData lookup |
| ReadTx | No | Transaction by hash - needs ChainDB index |
| ReadGenesis | No | Genesis config - available from TopLevelConfig |
| ReadEraSummary | No | Era history - available via QueryEraHistory |
| ReadState | No | Generic state query (e.g. GetStakePoolDistribution) |

**SubmitService** (5 spec RPCs):

| RPC | Implemented | Notes |
|-----|-------------|-------|
| SubmitTx | Yes | Transaction submission |
| EvalTx | Yes | Transaction evaluation (7 N2C queries) |
| WaitForTx | No | Streaming - watch for tx confirmation |
| ReadMempool | No | Mempool snapshot - Mempool.getSnapshot |
| WatchMempool | No | Streaming - mempool changes |

**SyncService** (4 spec RPCs) and **WatchService** (1 spec RPC) are not yet in cardano-rpc's proto files.

The node kernel access design must support all currently implemented RPCs.
Future RPCs (ReadMempool, ReadGenesis, ReadEraSummary, ReadState) will also benefit from direct access.

---

## Data Path: gRPC to Ledger State

cardano-rpc does **not** read ledger state directly.
It acts as a **Node-to-Client (N2C) IPC client** that connects to the same node it's running inside, via a Unix domain socket.

### The connection chain

```
gRPC client
  -> cardano-rpc server (Unix socket: rpc.sock)
    -> cardano-api IPC layer (Unix socket: node.sock)
      -> cardano-node's Ouroboros mini-protocol server
        -> in-memory ledger state
```

### Step by step

**1. Fresh connection per request** (`Env.hs:16-29`)

`RpcEnv` holds a `LocalNodeConnectInfo` (socket path + network magic + consensus mode params).
The TODO on line 19 confirms: there's currently **one connection per RPC request** - no connection pooling yet.

**2. Era determination** - every handler starts with:
```haskell
AnyCardanoEra era <- liftIO . throwExceptT $ determineEra nodeConnInfo
```
This opens a N2C connection and runs the `QueryCurrentEra` mini-protocol query.

**3. Local State Query mini-protocol** (`Query.hs:52`, `Node.hs:47`)

The core mechanism is `executeLocalStateQueryExpr` from `cardano-api`.
This function (`IPC/Internal/Monad.hs:71-93`):
1. Opens a **new** N2C connection via `connectToLocalNodeWithVersion` (Ouroboros network layer -> `Net.connectTo` on the local Unix socket).
2. Negotiates the Node-to-Client protocol version.
3. Runs the **LocalStateQuery** mini-protocol:
   - Sends `MsgAcquire` targeting `VolatileTip` (latest ledger state).
   - The node acquires a **read snapshot** of ledger state at the tip.
   - Sends `MsgQuery` for each query (protocol params, UTxOs, chain point, block number).
   - Receives results.
   - Sends `MsgRelease` then `MsgDone`.

**4. Specific queries used:**

| RPC Method | Ouroboros Queries |
|---|---|
| `ReadParams` | `queryProtocolParameters` + `querySystemStart` + `queryEraHistory` + `queryChainPoint` + `queryChainBlockNo` |
| `ReadUtxos` | `queryUtxo` (by TxIn set) + `querySystemStart` + `queryEraHistory` + chain point + block no |
| `SearchUtxos` | `queryUtxo` (by address set or whole) + `querySystemStart` + `queryEraHistory` + chain point + block no, then client-side predicate filtering + pagination |
| `GetProtocolParamsJson` | `queryProtocolParameters` |
| `SubmitTx` | `submitTxToNodeLocal` (LocalTxSubmission mini-protocol) |
| `EvalTx` | `queryProtocolParameters` + `queryUtxo` (by TxIn) + `querySystemStart` + `queryEraHistory` + `queryStakeDelegDeposits` + `queryDRepState` + `queryStakePoolParameters` |

**5. Query optimisation in SearchUtxos** (`Query.hs:104-106`)

Before querying, `extractAddressesFromPredicate` inspects the predicate tree.
If addresses can be extracted, it uses `QueryUTxOByAddress` (indexed lookup in the ledger).
Otherwise it falls back to `QueryUTxOWhole` (fetches entire UTxO set - expensive).

### Key implications

- **Every RPC request opens a new Ouroboros connection** - full handshake + protocol negotiation each time.
- **Queries are sequential** within a connection (the LocalStateQuery protocol is inherently sequential).
- **All data passes through CBOR serialisation** - the node serialises ledger state into CBOR over the socket, cardano-api deserialises it, then cardano-rpc re-serialises to protobuf.
- **The node acquires a consistent snapshot** at `VolatileTip` - so protocol params, UTxOs, and chain point within one `executeLocalStateQueryExpr` call are all from the same ledger state.

This is the architecture the ADR acknowledges as having overhead, with the planned improvement being **direct ledger state access** via TVar/STM reads (bypassing IPC entirely).
