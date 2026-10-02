# API Signatures and Type References

Verified consensus and cardano-api type signatures needed for the node kernel access implementation.

---

## 1. Consensus API Signatures (Verified)

All signatures below were verified against the checked-out source as of 2026-05-22.

### 1.1 `answerQuery`

**Module:** `Ouroboros.Consensus.Ledger.Query`
**File:** `ouroboros-consensus/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Query.hs`

```haskell
answerQuery
  :: forall blk m result.
     (BlockSupportsLedgerQuery blk, ConfigSupportsNode blk, HasAnnTip blk, MonadSTM m)
  => ExtLedgerCfg blk
  -> ReadOnlyForker' m blk
  -> Query blk result
  -> m result
```

- Takes `ExtLedgerCfg blk` (construct via `ExtLedgerCfg (getTopLevelConfig nk)`)
- Takes `ReadOnlyForker' m blk` (the primed alias: `ReadOnlyForker m (ExtLedgerState blk) blk`)
- `Query blk result` is a GADT with constructors: `BlockQuery`, `GetSystemStart`, `GetChainBlockNo`, `GetChainPoint`, `DebugLedgerConfig`
- Handles all three footprints internally via `sing :: Sing footprint` dispatch: `SQFNoTables` -> `answerPureBlockQuery`, `SQFLookupTables` -> `answerBlockQueryLookup`, `SQFTraverseTables` -> `answerBlockQueryTraverse`

**Footprint dispatch in detail:**
- `SQFNoTables` - pure ledger state queries (pparams, epoch, etc.) - calls `roforkerGetLedgerState` via STM.
  No table access, no IO beyond the STM read.
- `SQFLookupTables` - point lookups (UTxO by TxIn) - calls `roforkerReadTables`.
  O(m log n) for m keys in a store of n entries.
- `SQFTraverseTables` - table traversals (UTxO by address, whole UTxO) - calls `roforkerRangeReadTables`.
  O(n) full scan with batched range reads (default batch size 100k entries).

`answerQuery` returns plain Haskell values - no serialisation.

The `GetSystemStart` and `GetChainBlockNo`/`GetChainPoint` queries are handled directly at
the top level (not via `BlockQuery`).
`GetSystemStart` extracts the value from `ExtLedgerCfg` (the config, not the state).
`GetChainBlockNo` and `GetChainPoint` read from `headerState` via STM.

### 1.2 `getReadOnlyForkerAtPoint`

**Module:** `Ouroboros.Consensus.Storage.ChainDB.API` (API), `Ouroboros.Consensus.Storage.ChainDB.Impl.Query` (implementation)

```haskell
getReadOnlyForkerAtPoint
  :: IOLike m
  => ChainDbEnv m blk
  -> ResourceRegistry m
  -> Target (Point blk)
  -> m (Either GetForkerError (ReadOnlyForker' m blk))
```

- **`Target`** is from `Ouroboros.Network.Protocol.LocalStateQuery.Type` (NOT `Ouroboros.Consensus.Block`)
  ```haskell
  data Target point = VolatileTip | SpecificPoint point | ImmutableTip
  ```
- **`GetForkerError`** is from `Ouroboros.Consensus.Storage.LedgerDB.Forker`:
  ```haskell
  data GetForkerError = PointNotOnChain | PointTooOld !(Maybe ExceededRollback)
  ```
- Returns `ReadOnlyForker' m blk` (the primed alias)
- **`roforkerClose`** is a record field of `ReadOnlyForker`, type `!(m ())`

### 1.3 `toConsensusQuery` / `fromConsensusQueryResult`

**Module:** `Cardano.Api.Query.Internal.Type.QueryInMode` (re-exported via `Cardano.Api.Query`)

```haskell
toConsensusQuery
  :: (HasCallStack, Consensus.CardanoBlock StandardCrypto ~ block)
  => QueryInMode result
  -> Some (Consensus.Query block)

fromConsensusQueryResult
  :: (HasCallStack, Consensus.CardanoBlock StandardCrypto ~ block)
  => QueryInMode result
  -> Consensus.Query block result'
  -> result'
  -> result
```

- Both exported.
- `toConsensusQuery` returns `Some (Consensus.Query block)` (existential wrapper).
- `fromConsensusQueryResult` needs the original `QueryInMode` for type-level dispatch.
- **`QueryCurrentEra` routes through `toConsensusQuery` correctly.**
  It wraps internally as `BlockQuery (QueryHardFork GetCurrentEra)`.
  No special handling needed.

### 1.4 `toConsensusGenTx` / `fromConsensusApplyTxErr`

**Module:** `Cardano.Api.Consensus.Internal.InMode` (re-exported via `Cardano.Api.Consensus`)

```haskell
toConsensusGenTx
  :: Consensus.CardanoBlock StandardCrypto ~ block
  => TxInMode -> Consensus.GenTx block

fromConsensusApplyTxErr
  :: Consensus.CardanoBlock StandardCrypto ~ block
  => Consensus.ApplyTxErr block -> TxValidationErrorInCardanoMode
```

- Both exported from `Cardano.Api.Consensus`.
- `SubmitResult` is from `Ouroboros.Network.Protocol.LocalTxSubmission.Type`, re-exported via `Cardano.Api.Network`.

### 1.5 `addLocalTxs` (Mempool API)

**Module:** `Ouroboros.Consensus.Mempool.API`

```haskell
addLocalTxs
  :: forall m blk t. (MonadSTM m, Traversable t)
  => Mempool m blk -> t (GenTx blk) -> m (t (MempoolAddTxResult blk))
```

- Name is `addLocalTxs` (wrapper around lower-level `addTx` field with `AddTxForLocalClient`).
- On GHC 9.10, use **`MkSolo`** (not `Solo`) as the constructor:
  ```haskell
  import Data.Tuple (Solo (..))
  MkSolo addTxRes <- addLocalTxs mempool (MkSolo genTx)
  ```
- Result type:
  ```haskell
  data MempoolAddTxResult blk
    = MempoolTxAdded !(Validated (GenTx blk))
    | MempoolTxRejected !(GenTx blk) !(ApplyTxErr blk)
  ```
- Error type in rejection is `ApplyTxErr blk` (a type family; for Shelley blocks: `SL.ApplyTxError era`).

### 1.6 `withRegistry`

**Module:** `Control.ResourceRegistry` (NOT `Ouroboros.Consensus.Util.ResourceRegistry`)

```haskell
withRegistry :: (ResourceRegistry m -> m a) -> m a
```

- Registry cleanup automatically closes registered forkers when the registry scope exits.
- This means `bracket` around `roforkerClose` is still good practice for explicit cleanup, but `withRegistry` provides a safety net if an exception bypasses the explicit close.

### 1.7 `NodeKernel` type parameters

**Module:** `Ouroboros.Consensus.NodeKernel` (in ouroboros-consensus-diffusion)

```haskell
data NodeKernel m addrNTN addrNTC blk = NodeKernel
  { getChainDB              :: ChainDB m blk
  , getMempool              :: Mempool m blk
  , getTopLevelConfig       :: TopLevelConfig blk
  , getFetchClientRegistry  :: FetchClientRegistry (ConnectionId addrNTN) (HeaderWithTime blk) blk m
  , ...
  }
```

- In cardano-node (`Run.hs` line 404): `NodeKernel IO RemoteAddress LocalConnectionId blk`
- `getChainDB`, `getMempool`, `getTopLevelConfig` are all **record fields** (not standalone functions).
- `rnNodeKernelHook` receives the kernel in the callback: `\registry nodeKernel -> do ...`
