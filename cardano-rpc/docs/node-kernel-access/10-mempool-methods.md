# Piece 10: Mempool methods (ReadMempool, WaitForTx, WatchMempool)

## Problem

`methodsUtxoRpcSubmit` registers `UnsupportedMethod` for `readMempool`, `waitForTx` and `watchMempool`, so the server answers all three with `UNIMPLEMENTED`.
These methods cannot be served over N2C: the LocalTxMonitor protocol is acquire-snapshot-then-query, so a socket-based watcher can only poll by re-acquiring.
Event-driven mempool access needs the in-process kernel path.

## Why

Unlike pieces 4-7, these methods are not migrations - no N2C implementation exists or is possible.
`NodeKernelAccess` unlocks them: `getSnapshot` on the consensus `Mempool` is a plain STM read, so watchers block on ordinary STM retry instead of polling, and `snapshotTxsAfter` keyed by ticket number gives incremental delivery for a watch stream.

## User value

As a dApp or tooling developer, I want to observe my transaction's progress through the mempool to confirmation over gRPC, so that I do not need to poll the node or run an external indexer.

## Acceptance criteria

1. **AC1: Mempool capability on `NodeKernelAccess`** - `NodeKernelAccess/Type.hs` gains a field `mempool :: Consensus.Mempool IO (Consensus.CardanoBlock Consensus.StandardCrypto)`, mirroring the `chainDb` field; `mkNodeKernelAccess` populates it from `Consensus.getMempool nodeKernel`.
   This is a cardano-rpc-only change: `mkNodeKernelAccess` already receives the whole `NodeKernel`, so no cardano-node wiring changes.
   - Test: unit - `cabal build cardano-rpc` from the metarepo root.
2. **AC2: `readMempoolMethod`** - one-shot handler: reads `atomically (getSnapshot mempool)`, converts each snapshot entry via `txForgetValidated` then `fromConsensusGenTx` to a `TxInMode`, and answers with one `TxInMempool` per transaction: `ref` is the raw `TxId` bytes (from `getTxId` on the tx body), `native_bytes` is `serialiseToCBOR` of the tx (a canonical re-encoding, see Notes), `cardano` is the parsed tx reusing the existing tx-to-proto conversion the Sync handlers use for block bodies, and `stage` is `STAGE_MEMPOOL`.
   Registered as `Method (mkNonStreaming $ wrapInSpan TraceRpcReadMempoolSpan . readMempoolMethod)` in the `readMempool` slot of `methodsUtxoRpcSubmit` (position 2; `ServiceMethods` alphabetical order must be preserved).
   `ReadMempoolRequest` is an empty message (no field mask), so `parsed_state` is always populated.
   - Test: unit - snapshot-to-proto conversion on fixture transactions.
   - Test: E2E - response is well-formed after a submit; the E2E must tolerate an empty result (the tx may already be confirmed - see Notes on single-node testnet timing).
3. **AC3: `TxPredicate` evaluator** - a pure function `matchesTxPredicate :: Proto UtxoRpc.TxPredicate -> Proto Cardano.Tx -> Bool` in `Predicate.hs`, mirroring `matchesUtxoPredicate`'s `not`/`allOf`/`anyOf` recursion, covering the `TxPattern` fields: `consumes`, `produces`, `has_address`, `moves_asset`, `mints_asset`, `has_certificate`.
   It evaluates against the already-converted proto `Tx`, so the conversion from AC2 is done once per tx and shared between filtering and the response payload.
   - Test: unit - Hedgehog properties per pattern field plus the boolean combinators.
4. **AC4: `watchMempoolMethod`** - server-streaming handler built on a dependency-injected `watchMempoolStream` (same testability shape as `followTipStream`: mempool read actions and the `send` callback are plain arguments).
   The blocking wait is `atomically $ do { snapshot <- getSnapshot mempool; check (changed snapshot); pure snapshot }` where change detection compares the full `TicketNo` list plus `snapshotSlotNo` between snapshots - the LocalTxMonitor server's `isSameSnapshot` pattern; a max-ticket comparison would miss removal-only changes.
   Emits additions only: new entries from `snapshotTxsAfter lastSeenTicket`, filtered by `matchesTxPredicate`, pruned by the request's field mask, each with `stage = STAGE_MEMPOOL`.
   - Test: unit - drive `watchMempoolStream` with injected snapshots; assert delivery, filtering and no duplicates across snapshot transitions.
   - Test: E2E - open WatchMempool, submit a matching tx, receive it.
5. **AC5: `waitForTxMethod`** - server-streaming handler tracking a per-ref state machine.
   Refs are parsed with `deserialiseFromRawBytes AsTxId`; matching is done on cardano-api `TxId` values obtained via `fromConsensusGenTx`/`getTxId` (the cardano-api `TxId` is not era-tagged, so raw-byte comparison is well-defined).
   Emits `STAGE_MEMPOOL` for a ref when it appears in the mempool snapshot, and `STAGE_CONFIRMED` when the tx appears in a `RollForward` block delivered by a follower (`withFollower`) opened at stream start.
   The stream closes once every requested ref has reached `STAGE_CONFIRMED`.
   The mempool watcher and the follower run as two concurrent producers feeding one bounded queue that the handler drains to `send` - the handler may block indefinitely between events, which is safe (see Notes).
   - Test: unit - state machine over injected mempool/follower event streams, including the eviction and duplicate-delivery cases.
   - Test: E2E - submit a tx, assert the stream delivers `STAGE_CONFIRMED`; do not assert `STAGE_MEMPOOL` (racy on a single-node testnet, see Notes).
6. **AC6: Tracing** - `Tracing.hs` gains `TraceRpcReadMempoolSpan`, `TraceRpcWaitForTxSpan` and `TraceRpcWatchMempoolSpan`; all three handlers are wrapped with `wrapInSpan` like every existing method.
   - Test: manual - constructors present and used in `Server.hs` registration.
7. **AC7: Streaming error assertions bypass `serverStreaming`** - any test asserting a non-OK terminal status on these streams drives the call directly (`withRPC` + `sendFinalInput` + `recvOutput`), because grapesy's `serverStreaming` recv maps error-terminated streams to a clean end of stream (established pattern: `followTipExpectingError`).
   - Test: manual - test code inspection.

## Out of scope

- `STAGE_ACKNOWLEDGED` and `STAGE_NETWORK` are never emitted: the first locally observable stage is `STAGE_MEMPOOL`, and network propagation is not observable from the local node.
  The utxorpc spec is silent on both (its generated docs only restate the field comments).
- Removal/eviction events in WatchMempool: the proto has no stage representing removal, and eviction *reasons* (revalidation drop vs manual removal vs inclusion) are only visible via `TraceEventMempool`, a write-only tracer installed at mempool construction - push-based eviction events would need tracer fan-out wired at `NodeKernelArgs` construction time in cardano-node (a future piece, if ever needed).
- Rollback semantics for confirmed txs: a confirmed-then-rolled-back tx is not re-announced by WaitForTx; FollowTip's `undo` stream is the client-side tool for that.
- The SubmitTx migration itself (piece 5): [plan-snapshot-api-phases.md](plan-snapshot-api-phases.md) phase 5's `submitTx` capability is now implemented as a helper over this piece's `mempool` field.
- Closing the WaitForTx submit-wait race window by starting the follower from an earlier intersect (see Notes for the documented client contract instead); revisit if conformance tests require it.

## Definition of done

- [ ] All ACs verified
- [ ] `cabal build cardano-rpc` clean from the metarepo root, no new warnings (repo CI builds with `-Werror`)
- [ ] Unit tests pass (`cabal test cardano-rpc-test`)
- [ ] E2E tests pass (`cardano-testnet-test` RPC suite)
- [ ] `scripts/devshell/prettify` run on changed files
- [ ] Reviewer pass
- [ ] CI green

## Notes

- **Capability shape:** the `mempool` field mirrors `chainDb` - a raw consensus handle on the record, with ergonomic helpers (`fetchBlock`, `withFollower` precedent) as functions in `NodeKernelAccess.hs`.
  Narrow per-use fields were rejected: read, watch and submit all need the same handle, and STM composability (`retry`/`check`) requires exposing the STM `getSnapshot`, not an `IO`-wrapped copy.
- **WaitForTx race contract:** a tx that reaches the chain between submission and the WaitForTx call is invisible - it is no longer in the mempool and does not appear in any block delivered after stream start, so the stream would wait forever on that ref.
  Clients must open WaitForTx before or concurrently with submitting the tx.
  This contract is documented on the handler; the alternative (follower intersect a few blocks back to replay the window) is deferred.
- **Eviction is non-terminal:** a ref that leaves the mempool without on-chain inclusion produces no message and the stream keeps waiting.
  The proto has no rejection stage, and chain churn can legitimately re-add a tx to the mempool later; clients apply their own deadline.
- **Byte fidelity:** the ledger `Tx` inside a mempool `GenTx` is not MemoBytes-backed, so `native_bytes` is a canonical re-encoding, not the submitter's original bytes - same caveat and documentation pattern as `txOutToUtxoRpcTxOutput`.
- **Blocking is safe in handlers:** grapesy runs http2 with the timeout manager disabled, so a handler blocked on STM between stream elements is not reaped.
- **Single-node testnet timing:** txs confirm within a slot or two, so E2E assertions on the transient `STAGE_MEMPOOL` are racy; E2Es assert `STAGE_CONFIRMED` and the stage-transition logic is covered by unit tests on the injected stream functions.
- **Hard compile-time dependency on AC1 only:** phases for this piece (see [plan-mempool-phases.md](plan-mempool-phases.md)) let the three handlers land independently after the capability field exists.
- Current file: `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Submit.hs` (registration: `cardano-api/cardano-rpc/src/Cardano/Rpc/Server.hs`)
- E2E test files: `cardano-node/cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/`

## Reference docs

- [plan-mempool-phases.md](plan-mempool-phases.md) - phased build order for this piece
- [plan-snapshot-api-phases.md](plan-snapshot-api-phases.md) - the `submitTx` capability (phase 5), now built on this piece's `mempool` field
- [analysis-consensus-protocol.md](analysis-consensus-protocol.md) - snapshot consistency background
- [prereqs-implementation-details.md](prereqs-implementation-details.md) - codebase gotchas
