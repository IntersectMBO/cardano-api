# FetchBlock timestamp implementation phases

Adds `Block.timestamp` to the FetchBlock response.
Each phase compiles independently and can be reviewed as a separate commit.

## Background

Cardano blocks are slot-indexed, not timestamped.
Converting slot to wall-clock time requires `SystemStart` + `EraHistory`.
`SystemStart` is a genesis parameter, constant for the lifetime of the chain.
`EraHistory` depends on ledger state (hard fork transitions).

The current ledger state is available via `ChainDB.getCurrentLedger :: STM m (ExtLedgerState blk EmptyMK)` - a cheap STM read, no forker needed.
`hardForkSummary cfg ledger` computes the era summary from the in-memory ledger state.

## Phase 1: Add `nkaSystemStart` and `nkaEraHistory` to `NodeKernelAccessF` ✅

Provides the ingredients for time calculations without prescribing the recipe.

- Add two fields to `NodeKernelAccessF`:
  - `nkaSystemStart :: SystemStart` - pure field, extracted from `TopLevelConfig` at construction time
  - `nkaEraHistory :: m EraHistory` - callback, reads current ledger state on each call via `getCurrentLedger` + `hardForkSummary` + `mkInterpreter`
- `SystemStart` passed to `mkNodeKernelAccess` from `Run.hs` (where consensus imports are available)
- `mkNodeKernelAccess` stays pure - the `m` effect happens when `nkaEraHistory` is called
- Re-exports added to `Cardano.Api.Consensus`: `getCurrentLedger`, `hardForkSummary`, `mkInterpreter`, `TopLevelConfig`, `configBlock`, `configLedger`, `ledgerState`, `HasHardForkHistory(..)`, `ConfigSupportsNode`
- Unsupported block types throw `GrpcInternal` with the block type name in the message
- Stubbed callbacks (`nkaWithSnapshot`, `nkaSubmitTx`) throw `GrpcUnimplemented`

Build: `cabal build cardano-rpc && cabal build cardano-node`

## Phase 2: Compute timestamp in `fetchBlockMethod`

Callers compose `nkaSystemStart` + `nkaEraHistory` with `slotToUTCTime` (cardano-api).

- In `fetchBlockMethod` (Sync.hs), after fetching the block:
  1. Read `nkaSystemStart nodeKernelAccess` (pure)
  2. Call `nkaEraHistory nodeKernelAccess` (reads ledger state)
  3. Compute: `slotToUTCTime systemStart eraHistory slot`
  4. On `Right utcTime`: convert to milliseconds: `round . (* 1000) . utcTimeToPOSIXSeconds`
  5. On `Left PastHorizonException`: throw `GrpcInternal` - a fetched block's slot is in-horizon by construction, so this is a bug
  6. Set `U5c.cardano . U5c.timestamp .~ timestampMs` on the response
- Remove the `-- TODO: timestamp` comment

Build: `cabal build cardano-rpc`

## Phase 3: Update E2E test

- Update `FetchBlock.hs` test:
  - Remove `block ^. U5c.cardano . U5c.timestamp H.=== 0`
  - Compute expected timestamp from `SystemStart` + `EraHistory` via N2C IPC (same as Query.hs test does)
  - Assert actual timestamp is within 1000ms tolerance of expected: `H.assertWithinTolerance`
- Verify all RPC tests still pass (no regressions)

Build: `TASTY_PATTERN='/RPC/' cabal test cardano-testnet-test`

## Phase dependency graph

```
Phase 1 (nkaSystemStart + nkaEraHistory) ✅
  |
  v
Phase 2 (timestamp in handler)
  |
  v
Phase 3 (E2E test)
```

## Design decisions

- **Provide ingredients, not the recipe.**
  `nkaSystemStart` and `nkaEraHistory` are general-purpose.
  Callers compose with `slotToUTCTime`, `getProgress`, `slotToEpoch`, or any other era-history function.
  Not limited to timestamp computation.

- **No forker needed.**
  `getCurrentLedger` (STM) gives the current ledger state directly.
  The forker API is reserved for the full query migration where consistent multi-query snapshots matter.

- **Live recompute.**
  `hardForkSummary` is cheap (in-memory traversal, O(number_of_eras)).
  Caching (`RunWithCachedSummary`) is available as a future optimisation if profiling warrants it.

- **Same `slotToUTCTime` as Query.hs.**
  One conversion path across handler and test.

- **`PastHorizonException` on a fetched block is a bug.**
  A block in ChainDB was produced in a known era.
  The current tip's summary covers all slots up to the tip.

## Scope

This plan adds timestamp to FetchBlock and provides `SystemStart` + `EraHistory` on `NodeKernelAccessF`.
These fields also enable future use by ReadGenesis, ReadEraSummary, and any method needing time or era information.
