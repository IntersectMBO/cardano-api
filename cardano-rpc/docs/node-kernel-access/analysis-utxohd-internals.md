# UTxO-HD Internals

This document covers the UTxO-HD backing store architecture and its implications for node kernel access.

---

## UTxO-HD Architecture Impact

UTxO-HD fundamentally changes what "direct access" means for UTxO data.

The key insight is that **the UTxO set is no longer in the in-memory ledger state**.
When you call:

```haskell
atomically (ChainDB.getCurrentLedger chainDB)
  :: STM IO (ExtLedgerState blk EmptyMK)
```

That `EmptyMK` is literal - the `shelleyLedgerTables` field is `EmptyMK`, meaning **no UTxO data**.
The UTxO lives in a backing store (either an in-memory `TVar` map or an on-disk LSM tree), accessed only through `ReadOnlyForker`.

### The three tiers of data access

**Tier 1: Pure ledger state (EmptyMK) - free, STM**
- Protocol parameters (`getPParams`), epoch number, stake distribution, chain point, block number.
- These live in `NewEpochState` which is fully in-memory regardless of UTxO-HD.
- Access: `atomically (getCurrentLedger chainDB)` -> pure extraction.
- No forker needed, no IO, no disk.

**Tier 2: Point lookups (KeysMK -> ValuesMK) - cheap, needs forker**
- `GetUTxOByTxIn`: look up specific TxIns.
- Access: `roforkerReadTables forker (LedgerTables (KeysMK txinSet))`.
- O(m log n) - goes to backing store for the specific keys.
- On V2/LSM: disk reads for specific keys.
- On V1/InMemory: map lookups in TVar.

**Tier 3: Table traversals (RangeRead) - expensive, needs forker + iteration**
- `GetUTxOByAddress`, `GetUTxOWhole`: full or filtered scan.
- Access: loop calling `roforkerRangeReadTables` with `QueryBatchSize` (default 100k entries per batch).
- O(n) - must scan entire backing store, applying a predicate filter per batch.
- On V2/LSM: sequential range reads through LSM tree levels, deserialising `TxOutBytes` back to `Core.TxOut era`.
- The existing consensus code in `answerShelleyTraversingQueries` does exactly this loop.

### What this means for the direct access design

**For Tier 1 (ReadParams pparams):** The win is massive and simple.
Replace the entire N2C round-trip with a single STM read.
No forker, no table access, no disk.
This is the easy win.

**For Tier 2 (ReadUtxos by TxIn):** The win is real but more nuanced.
You skip the N2C + CBOR overhead, but you still need:
1. Acquire a `ReadOnlyForker` via `ChainDB.getReadOnlyForkerAtPoint` (allocates resources).
2. Call `roforkerReadTables` (may hit disk on LSM backend).
3. Close the forker.

The backing store IO is the same cost either way - you just eliminate the IPC + serialisation wrapper.
For small lookups this is a big relative improvement.

**For Tier 3 (SearchUtxos by address / whole UTxO):** The most interesting case.
Currently:
- N2C server does the full range-read loop internally, accumulates the entire result, CBOR-serialises the whole `UTxO era`, sends it over the socket.
- cardano-api deserialises the whole thing.
- cardano-rpc converts to protobuf.

With direct access:
- cardano-rpc can call `roforkerRangeReadTables` directly in a loop.
- Can apply predicate filtering **per batch** (already happens on the consensus side, now happens in-process).
- Can implement **true streaming pagination** - instead of reading the entire filtered result and then slicing, stop after collecting enough items for one page.
- Converts ledger types -> protobuf directly per batch, no intermediate CBOR or cardano-api types.

This is where the design gets the most interesting improvement: **pagination can be pushed down to the backing store level**.
Currently `SearchUtxos` fetches the entire filtered UTxO set, sorts it, and slices.
With direct range reads, you can stop early after filling a page.

**For tx submission (Mempool.addTx):** The tx needs to be a `GenTx blk` (consensus type), not the cardano-api `TxInMode` wrapper.
The current path is: CBOR bytes -> `deserialiseFromCBOR` -> cardano-api `Tx era` -> `toConsensusGenTx` -> N2C LocalTxSubmission -> mempool.
Direct path: CBOR bytes -> `deserialiseFromCBOR` -> `GenTx blk` -> `Mempool.addTx`.
Skips the entire protocol layer.

### The HFC complication

`ChainDB` and `NodeKernel` are parameterised by `CardanoBlock StandardCrypto`, which is a `HardForkBlock` across all eras.
The `LedgerTables` use `CanonicalTxIn` and `HardForkTxOut` - canonical representations that abstract across eras.
To get era-specific `Core.TxOut era`, you need `ejectLedgerTables` from the HFC layer.

The existing `answerShelleyLookupQueries` and `answerShelleyTraversingQueries` already handle this via injection/ejection functions passed as parameters.
cardano-rpc's direct access layer would need to replicate this pattern - or better, reuse the existing `answerQuery` function from consensus with a directly-obtained forker rather than one obtained through the protocol.

### Depth of integration

There's a spectrum of how deep cardano-rpc reaches into consensus:

1. **Shallow**: Reuse `answerQuery` from consensus - get a forker, call `answerQuery cfg forker query`, get back the same types the N2C server would produce.
   Eliminates IPC + CBOR serialisation but still uses the consensus query dispatch.
   Simplest, lowest risk.

2. **Medium**: Call `roforkerReadTables` / `roforkerRangeReadTables` directly, handle HFC ejection, convert ledger types -> protobuf.
   More code but enables streaming pagination optimisation.

3. **Deep**: Bypass forkers for Tier 1 queries entirely (just STM reads), use forkers only for Tier 2/3.
   Mix-and-match per query type.

Option 3 is what was outlined in the earlier design.
The risk is coupling to consensus internals.
But Tier 1 queries via STM are so simple and stable that the coupling is minimal, and the existing `nkQueryLedger` helper in `Queries.hs` already does exactly this for metrics.

---

## LedgerTables Type Family System

### Core Type Definition

**File:** `ouroboros-consensus/Ouroboros/Consensus/Ledger/Tables/Basics.hs`

```haskell
type LedgerTables :: LedgerStateKind -> MapKind -> Type
newtype LedgerTables l mk = LedgerTables
  { getLedgerTables :: mk (TxIn l) (TxOut l)
  }
```

Where `MapKind` is `Type -> Type -> Type` (key and value parameters).

### MapKind Definitions

**File:** `ouroboros-consensus/Ouroboros/Consensus/Ledger/Tables/MapKind.hs`

- **EmptyMK**: `data EmptyMK k v = EmptyMK` - no UTxO data present.
- **KeysMK**: `newtype KeysMK k v = KeysMK (Set k)` - only key set.
- **ValuesMK**: `newtype ValuesMK k v = ValuesMK {getValuesMK :: Map k v}` - full key-value map.
- **DiffMK**: `newtype DiffMK k v = DiffMK {getDiffMK :: Diff k v}` - changes (Insert v | Delete).
- **TrackingMK**: `data TrackingMK k v = TrackingMK !(Map k v) !(Diff k v)` - values + accumulated diffs.
- **SeqDiffMK**: finger tree of diffs.

### What EmptyMK Means Concretely

When you have `LedgerState era EmptyMK`:
- The `EmptyMK` represents that **the UTxO map data is literally not present** in this ledger state.
- The Consensus layer stores actual UTxO data separately on disk (in the backing store).
- The UTxO can be "stowed" (extracted from the ledger) or "unstowed" (injected back into the ledger).

### Shelley-Specific Instance

```haskell
type instance TxIn (LedgerState (ShelleyBlock proto era)) = BigEndianTxIn
type instance TxOut (LedgerState (ShelleyBlock proto era)) = Core.TxOut era
```

`BigEndianTxIn` is a wrapper that ensures proper byte ordering for serialisation.

### Cardano HFC Handling

```haskell
type instance TxIn (LedgerState (HardForkBlock xs)) = CanonicalTxIn xs
type instance TxOut (LedgerState (HardForkBlock xs)) = HardForkTxOut xs
```

Key functions: `injectLedgerTables` and `ejectLedgerTables` transform between era-specific and canonical representations via `bimapLedgerTables`.

### How cardano-api Gets UTxO from Queries

The flow:
1. cardano-api calls `queryUtxo` with a filter.
2. Converts to `toConsensusQuery` -> `GetUTxOWhole` / `GetUTxOByAddress` / `GetUTxOByTxIn`.
3. Consensus queries in-memory LedgerState (with `ValuesMK`).
4. Result comes as `Shelley.UTxO (ShelleyLedgerEra era)`.
5. Converts via `fromLedgerUTxO` to cardano-api `UTxO era`.

---

## UTxO-HD Architecture Report

### LedgerDB and Backing Store

The LedgerDB is responsible for:
- Maintaining in-memory ledger state at the tip.
- Maintaining past k in-memory ledger states (supports rollback).
- Providing LedgerTables at any of the last k states.
- Storing snapshots on disk.
- Flushing LedgerTable differences to backing store.

### Backing Store Abstraction

```haskell
data BackingStore m keys key values diff = BackingStore
  { bsClose :: m ()
  , bsCopy :: SerializeTablesHint values -> FS.FsPath -> m ()
  , bsValueHandle :: m (BackingStoreValueHandle m keys key values)
  , bsWrite :: SlotNo -> WriteHint diff -> diff -> m ()
  , bsSnapshotBackend :: SnapshotBackend
  }
```

### Two Backend Implementations

**V1 Backend - InMemory:**
- Uses a `TVar` holding full map.
- All data stays in memory.
- Used for testing or small deployments.

**V2 Backend - LSM (Log-Structured Merge):**
- Uses LSM trees from the `lsm-tree` library.
- Keys and values serialised to bytes.
- Uses indexed packing (`IndexedMemPack`) for efficient serialisation.

### The Forker

```haskell
data Forker m l = Forker
  { forkerReadTables :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
  , forkerRangeReadTables :: RangeQueryPrevious l -> m (LedgerTables l ValuesMK, Maybe (TxIn l))
  , forkerGetLedgerState :: STM m (l EmptyMK)    -- NO UTxO
  , forkerReadStatistics :: m Statistics
  , forkerPush :: l DiffMK -> m ()
  , forkerCommit :: STM m ()
  }
```

**Critical insight:** `forkerGetLedgerState` returns `l EmptyMK` - **the UTxO data is NOT included**.
You must explicitly call `forkerReadTables`.

### Range Queries and BatchSize

`QueryBatchSize` defaults to 100,000 entries.
`roforkerRangeReadTables` returns up to that many entries plus one additional key for pagination.
Client iterates until `Nothing` (end of table).

### ReadOnlyForker vs getCurrentLedger

| Aspect | getCurrentLedger | getReadOnlyForker |
|--------|------------------|------------------|
| Returns | `l EmptyMK` (STM) | `Forker m l` (IO) |
| UTxO access | NO | YES via `roforkerReadTables` |
| Speed | Fast (STM, in-memory) | Slower (allocates, I/O) |
| Use case | Ledger state queries, consensus | Query answering requiring UTxO |
| Resource management | None | Must close forker |

### Three Footprint Categories for Shelley Queries

**QFNoTables:** `GetLedgerTip`, `GetEpochNo`, `GetCurrentPParams`, etc. - uses `answerPureBlockQuery`, just in-memory ledger state.

**QFLookupTables:** `GetUTxOByTxIn` - uses `answerShelleyLookupQueries`, O(m * log n) for m inputs.

**QFTraverseTables:** `GetUTxOByAddress`, `GetUTxOWhole` - uses `answerShelleyTraversingQueries`, O(n) full scan with batched range reads.

### UTxO-HD Pipeline Diagram

```
Client Query (e.g., GetUTxOByAddress)
  |
  v
answerShelleyTraversingQueries
  |
  v
getReadOnlyForker(cfg, pt) -> Forker
  |
  v
roforkerRangeReadTables (QueryBatchSize = 100k)
  |
  v
BackingStore.bsvhRangeRead
  |- V1: reads from TVar (InMemory)
  |- V2: reads from LSM tree
  |
  v
Apply DbChangelog diffs (forward apply) [V1 only]
  |
  v
Filter by predicate (e.g., address match)
  |
  v
Accumulate into result (loop until end)
  |
  v
Return UTxO era to client
```

### Why MapKind Matters

The MapKind parameter elegantly separates concerns:
- **`EmptyMK`** = "I have the ledger rules state but no UTxO".
- **`ValuesMK`** = "I have loaded UTxO from disk".
- **`KeysMK`** = "Here are the keys I want to read".
- **`DiffMK`** = "Here are the changes since last checkpoint".

This allows the type system to enforce that you cannot accidentally use UTxO when you have not loaded it.
