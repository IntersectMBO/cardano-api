# Piece 4: Rewrite query methods to use NodeKernelAccess

## Problem

The three query methods in `Query.hs` (`readParamsMethod`, `readUtxosMethod`, `searchUtxosMethod`) use the Node-to-Client IPC pattern (`executeLocalStateQueryExpr` via `LocalNodeConnectInfo`).
This creates a new socket connection per request and double-serialises data through CBOR.
These methods need to be rewritten to use the snapshot-based `NodeKernelAccess` interface for direct ledger state access.

## Why

Eliminating the N2C round-trip reduces latency and resource overhead for every query request.
Using a single `nkaWithSnapshot` call per request preserves the consistency guarantee (all queries see the same ledger state) while removing the socket and serialisation costs.

## User value

As a dApp developer querying protocol parameters or UTxOs via gRPC, I want responses served directly from the ledger state so that queries are faster and I do not pay the overhead of a Node-to-Client IPC round-trip.

## Acceptance criteria

1. **AC1: readParamsMethod uses NodeKernelAccess** - `readParamsMethod` acquires a `LedgerSnapshot` via `withNodeKernelAccess` and `nkaWithSnapshot`, then runs all queries (`QueryCurrentEra`, `QueryProtocolParameters`, `QuerySystemStart`, `QueryEraHistory`, `QueryChainPoint`, `QueryChainBlockNo`) against that snapshot.
   No references to `executeLocalStateQueryExpr`, `determineEra`, or `VolatileTip` remain in this method.
   - Test: E2E - `hprop_rpc_query_pparams` passes, validating all 44 protocol parameter fields match the ledger and the ledger tip (slot, hash, height) is correct.

2. **AC2: readUtxosMethod uses NodeKernelAccess** - `readUtxosMethod` follows the same snapshot pattern as AC1, querying UTxOs via `QueryUTxO` inside `nkaWithSnapshot`.
   The `txoRefToTxIn` helper and protobuf conversion logic remain unchanged.
   - Test: E2E - `hprop_rpc_query_pparams` readUtxos assertion passes (UTxO set matches epoch state view).

3. **AC3: searchUtxosMethod uses NodeKernelAccess** - `searchUtxosMethod` follows the same snapshot pattern, with post-query filtering and pagination logic unchanged.
   - Test: E2E - `hprop_rpc_search_utxos` passes (exact address, payment credential, empty, and whole-set predicates all return correct results).

4. **AC4: Snapshot consistency** - Each query method opens exactly one `nkaWithSnapshot` call, and all queries within that call (era detection, domain query, systemStart, eraHistory, chainPoint, blockNo) execute against the same `LedgerSnapshot`.
   This preserves the consistency guarantee of the previous `executeLocalStateQueryExpr` pattern.
   - Test: unit - code review; verified structurally by the single `nkaWithSnapshot` callback per method. A Haddock note on each method documents this invariant.

5. **AC5: systemStart and eraHistory inside snapshot** - Every query method queries `QuerySystemStart` and `QueryEraHistory` inside the snapshot callback (not outside it), so that `slotToTimestamp` receives values consistent with the chain point.
   - Test: unit - compile-time verification; if either query is moved outside the snapshot callback, the types enforce that the values are not available. Code review confirms placement.

6. **AC6: Unused N2C imports removed** - The following symbols are no longer imported in `Query.hs`: `throwExceptT`, `executeLocalStateQueryExpr`, `queryProtocolParameters`, `queryChainPoint`, `queryChainBlockNo`, `queryUtxo`, `VolatileTip`.
   `Cardano.Rpc.Server.Internal.NodeKernelAccess` is added as an import.
   - Test: unit - `-Wall` clean build with no unused-import warnings.

7. **AC7: Pagination unit tests unaffected** - The `paginateByTxIn` function and its six unit tests in `Test.Cardano.Rpc.Pagination` remain unchanged and pass.
   - Test: unit - `cabal test cardano-rpc-test` passes with all pagination properties green.

8. **AC8: Build clean** - `cabal build cardano-rpc` compiles with no errors or warnings.
   - Test: unit - successful build under `-Wall -Werror` (or project-level warning settings).

## Out of scope

- Rewriting `evalTxMethod` in `Eval.hs` (separate piece).
- Rewriting `submitTxMethod` in `Submit.hs` (separate piece).
- Rewriting `getProtocolParamsJsonMethod` in `Node.hs` (separate piece).
- Changes to `Env.hs`, `Monad.hs`, or `Server.hs` (covered by piece 1: NodeKernelAccess types).
- Changes to `Tracing.hs` trace constructors (covered by piece 1).
- Updating the `hprop_rpc_query_pparams` timestamp assertion (currently hardcoded to `=== 0` with a TODO comment; this piece preserves existing behaviour).
- Creating `mkNodeKernelAccess` in cardano-node (piece 3).
- Integration test infrastructure changes (piece 8).

## Definition of done

- [ ] All AC tests written (compile, fail on stubs)
- [ ] Implementation complete (all tests pass via `cabal test`)
- [ ] Nix CI checks pass (`nix build 'path:/work#checks.x86_64-linux.test'` and `e2e`)
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean
- [ ] No build warnings

## Notes

- **Piece 1 prerequisite assumption:** this story assumes piece 1 delivers the `Has (IORef (Maybe NodeKernelAccess)) RpcEnv` instance in `Monad.hs`, the `MonadRpc` constraint update, and the `RpcEnv` field change in `Env.hs`.
  If piece 1 is scoped more narrowly (only `NodeKernelAccess.hs` itself), those wiring changes must be pulled into this piece or an intermediate one.
- **Integration tests require piece 3:** the E2E tests (`hprop_rpc_query_pparams`, `hprop_rpc_search_utxos`, `hprop_rpc_transaction`) spin up a real testnet node, which needs `mkNodeKernelAccess` wired in `Run.hs` (piece 3).
  This piece can be validated at the build and unit-test level independently; full E2E validation happens after piece 3 lands.
- **Timestamp field:** `hprop_rpc_query_pparams` asserts `timestamp === 0` (line 102) with the comment "not possible to implement at this moment".
  After this rewrite, `systemStart` and `eraHistory` are available inside the snapshot, so `slotToTimestamp` should produce real values.
  However, updating the test assertion is out of scope for this piece; it should be addressed in a follow-up once the full pipeline is wired end-to-end.
- **Mechanical transformation:** all three methods follow the identical old-to-new pattern.
  The only difference is the domain query (`QueryProtocolParameters` vs `QueryUTxO`).
  Consider extracting a shared `withQuerySnapshot` helper if the duplication becomes unwieldy, but this is an implementation decision, not an AC.
- **File changed:** `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Query.hs` (single file).

## Reference docs

- [Consensus protocol and snapshots](analysis-consensus-protocol.md) - snapshot consistency and `answerQuery` dispatch
- [Implementation details](prereqs-implementation-details.md) - query inventory for ReadParams/ReadUtxos/SearchUtxos
- [UTxO-HD internals](analysis-utxohd-internals.md) - forker mechanics for UTxO queries
