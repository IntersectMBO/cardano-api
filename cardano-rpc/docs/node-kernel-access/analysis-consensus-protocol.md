# Consensus Protocol and Snapshot Consistency

This document covers the consensus query protocol, in-memory ledger state architecture, and snapshot consistency requirements.

---

## LocalStateQuery Server-Side Protocol

### Protocol Setup

The server is initialised in `NodeToClient.hs`:
```haskell
hStateQueryServer =
    localStateQueryServer (ExtLedgerCfg cfg)
      . ChainDB.getReadOnlyForkerAtPoint getChainDB
```

### Protocol States

```
StIdle --> Acquire --> StAcquiring --> Acquired --> StAcquired
StAcquired --> Query --> StQuerying --> Result --> StAcquired
StAcquired --> Release --> StIdle
StAcquired --> ReAcquire --> StAcquiring
```

### Query Answering

```haskell
answerQuery config forker query = case query of
  BlockQuery blockQuery ->
    case sing :: Sing footprint of
      SQFNoTables ->
        answerPureBlockQuery config blockQuery
          <$> atomically (roforkerGetLedgerState forker)
      SQFLookupTables ->
        answerBlockQueryLookup config blockQuery forker
      SQFTraverseTables ->
        answerBlockQueryTraverse config blockQuery forker
  GetSystemStart ->
    pure $ getSystemStart config
  GetChainBlockNo ->
    headerStateBlockNo . headerState
      <$> atomically (roforkerGetLedgerState forker)
  GetChainPoint ->
    headerStatePoint . headerState
      <$> atomically (roforkerGetLedgerState forker)
```

### Acquiring: ReadOnlyForker

When a client sends "Acquire" with a target point:
```haskell
getReadOnlyForkerAtPoint ::
    ResourceRegistry m ->
    Target (Point blk) ->
    m (Either GetForkerError (ReadOnlyForker' m blk))
```

Returns a read-only forker providing:
- Consistent view of ledger state at the requested point.
- Access to ledger tables for queries that need them.
- Automatic resource cleanup.

### What Node Kernel Access Must Replicate

1. Accessing the ChainDB (from NodeKernel).
2. Reading current ledger state atomically: `atomically (ChainDB.getCurrentLedger chainDB)`.
3. For specific point queries: `ChainDB.getReadOnlyForkerAtPoint chainDB reg target`.
4. Reading ledger tables if needed.
5. Converting query results to appropriate response types.

---

## Ledger State In-Memory Architecture

### Node Kernel Structure

```haskell
data NodeKernel m addrNTN addrNTC blk = NodeKernel
  { getChainDB :: ChainDB m blk
  , getMempool :: Mempool m blk
  , getTopLevelConfig :: TopLevelConfig blk
  , ... (peer management, tracers, block forging state, etc.)
  }
```

Wrapped in `NodeKernelData`:
```haskell
newtype NodeKernelData blk =
  NodeKernelData
  { unNodeKernelData :: IORef (StrictMaybe (NodeKernel IO RemoteAddress LocalConnectionId blk))
  }
```

### Access Points to Ledger State

**Via ChainDB API:**
```haskell
getCurrentLedger :: STM m (ExtLedgerState blk EmptyMK)
getImmutableLedger :: STM m (ExtLedgerState blk EmptyMK)
getPastLedger :: Point blk -> STM m (Maybe (ExtLedgerState blk EmptyMK))
getCurrentChain :: STM m (AnchoredFragment (Header blk))
getReadOnlyForkerAtPoint :: ResourceRegistry m -> Target (Point blk) -> m (Either GetForkerError (ReadOnlyForker' m blk))
```

**Via ReadOnlyForker:**
```haskell
data ReadOnlyForker m l = ReadOnlyForker
  { roforkerGetLedgerState :: STM m (l EmptyMK)
  , roforkerReadTables :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
  , roforkerRangeReadTables :: RangeQueryPrevious l -> m (LedgerTables l ValuesMK, Maybe (TxIn l))
  }
```

### Concrete Ledger State Types (from Shelley)

```haskell
data NewEpochState era = NewEpochState
  { nesEL :: EpochNo, nesEs :: EpochState era, nesRu :: PulsingRewUpdate era
  , nesPd :: PoolDistr, nesBprev :: BlocksMade, nesBcur :: BlocksMade }

data UTxOState era = UTxOState
  { _utxosUtxo :: UTxO era, _utxosDeposited :: Coin, _utxosFees :: Coin
  , _utxosGovState :: GovState era, _utxosDonation :: Coin }
```

### Existing In-Process Access Patterns

**LedgerMetrics Tracer** - traces metrics every N slots using `mapNodeKernelDataIO` and `nkQueryLedger`.

**Forging Loop** - gets ledger state for a specific point using `getReadOnlyForkerAtPoint`.

**Peer Selection** - reads immutable ledger via `getImmutableLedger`.

---

## cardano-rpc Implementation Details

### Protocol Definitions

1. **`cardano/rpc/node.proto`** - `GetEra()`, `GetProtocolParamsJson()`, Era enum.
2. **`utxorpc/v1beta/query/query.proto`** - `ReadParams()`, `ReadUtxos()`, `SearchUtxos()` with pagination.
3. **`utxorpc/v1beta/submit/submit.proto`** - `SubmitTx()`, `EvalTx()` with CBOR-serialised transactions.
4. **`utxorpc/v1beta/cardano/cardano.proto`** - Complete Cardano data structures (Tx, TxOutput, PParams with 44 fields, certificates, scripts PlutusV1-V4, governance).

### Key Architectural Patterns

- **Dependency Injection**: `RpcEnv` + `Has` typeclass + `MonadRpc` constraint + `RIO` monad.
- **Error Handling**: `RpcException` GADT, `throwEither`/`throwExceptT` helpers, top-level handler.
- **Tracing**: Span-based with random IDs, metrics emission, integration with cardano-node tracers.
- **Query Optimisation**: Address extraction from predicates for indexed lookup (`QueryUTxOByAddress`).
- **Pagination**: Token = `TxId#OutputIndex`, deterministic sort by TxIn, default 100 items.
- **Number Representation**: BigInt follows RFC 8949 CBOR encoding (int64, big_u_int, big_n_int).

### Integration Tests

1. `hprop_rpc_query_pparams` - validates all 44 protocol params match ledger.
2. `hprop_rpc_transaction` - full round-trip: fetch UTxOs -> build tx -> submit -> confirm on-chain.

---

## Snapshot Consistency

The current N2C code runs all queries within one `executeLocalStateQueryExpr` call, which acquires a single ledger snapshot at `VolatileTip`.
Protocol parameters, UTxO results, chain tip, and block number all come from the same ledger state.

This is critical for correctness:
- `EvalTx` queries 7 different things (pparams, UTxOs, system start, era history, stake delegations, DRep state, stake pool params) that must be mutually consistent for transaction evaluation to be valid.
- Query methods use chain tip alongside query results; a mismatch would return UTxOs from one block with a chain tip from another.

A naive `NodeKernelAccess` design with individual callbacks per query type would break this guarantee.
Each callback would acquire its own `ReadOnlyForker` against a potentially different chain tip.

The solution is a snapshot-based design:

```haskell
data NodeKernelAccess = NodeKernelAccess
  { nkaWithSnapshot :: forall a. (LedgerSnapshot -> IO a) -> IO a
  , nkaSubmitTx     :: TxInMode -> IO (SubmitResult TxValidationErrorInCardanoMode)
  }

newtype LedgerSnapshot = LedgerSnapshot
  { runQuery :: forall result. QueryInMode result -> IO result }
```

`nkaWithSnapshot` acquires a single `ReadOnlyForker` and wraps it.
All queries within one `nkaWithSnapshot` call share the same forker and therefore the same ledger state.
