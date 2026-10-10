# Phased plan: mempool methods (piece 10)

Build order for [10-mempool-methods.md](10-mempool-methods.md).
Each phase is one reviewable commit; phases 3-6 are independent of each other once phases 1-2 are in.

## Verified signatures (pinned ouroboros-consensus 3.0.1.0, checked 2026-09-07)

- `getMempool :: NodeKernel m addrNTN addrNTC blk -> Mempool m blk` (record field, `Ouroboros.Consensus.NodeKernel`)
- `getSnapshot :: STM m (MempoolSnapshot blk)` (record field of `Mempool`, `Ouroboros.Consensus.Mempool.API`; a pure STM query, does not touch ledger state)
- `MempoolSnapshot` fields used: `snapshotTxs :: [(Validated (GenTx blk), TicketNo, TxMeasure blk)]`, `snapshotTxsAfter :: TicketNo -> [...]`, `snapshotHasTx :: GenTxId blk -> Bool`, `snapshotSlotNo :: SlotNo`
- `txForgetValidated :: Validated (GenTx blk) -> GenTx blk` (class `LedgerSupportsMempool`, `Ouroboros.Consensus.Ledger.SupportsMempool`)
- `fromConsensusGenTx :: Consensus.CardanoBlock StandardCrypto ~ block => Consensus.GenTx block -> TxInMode` (re-exported by `Cardano.Api`)
- `addLocalTxs :: (MonadSTM m, Traversable t) => Mempool m blk -> t (GenTx blk) -> m (t (MempoolAddTxResult blk))` (free function over `addTx` with `AddTxForLocalClient`; blocks until the tx fits the mempool)
- Change detection reference: `Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server.recvMsgAwaitAcquire` compares `(tno <$> snapshotTxs a) == (tno <$> snapshotTxs b) && snapshotSlotNo a == snapshotSlotNo b` inside `atomically`/`check`.

## Phase 1: `mempool` capability field

- `NodeKernelAccess/Type.hs`: add `mempool :: Consensus.Mempool IO (Consensus.CardanoBlock Consensus.StandardCrypto)`, mirroring `chainDb`.
- `NodeKernelAccess.hs`: populate from `Consensus.getMempool nodeKernel` in `mkNodeKernelAccess`.

No consumer yet, no behaviour change, no cardano-node change (`mkNodeKernelAccess` already receives the whole `NodeKernel`).

Build: `cabal build cardano-rpc`

## Phase 2: tracing constructors

- `Tracing.hs`: add `TraceRpcReadMempoolSpan`, `TraceRpcWaitForTxSpan`, `TraceRpcWatchMempoolSpan`.

Build: `cabal build cardano-rpc`

## Phase 3: ReadMempool (AC2)

- `Submit.hs`: `readMempoolMethod` - snapshot read, `txForgetValidated`/`fromConsensusGenTx` conversion, `TxInMempool` assembly reusing the Sync handlers' tx-to-proto conversion.
- `Server.hs`: replace the `readMempool` `UnsupportedMethod` slot (keep `ServiceMethods` order: evalTx, readMempool, submitTx, waitForTx, watchMempool).
- Unit tests for the conversion.

Build: `cabal build cardano-rpc && cabal test cardano-rpc-test`

## Phase 4: TxPredicate evaluator (AC3)

- `Predicate.hs`: `matchesTxPredicate`, mirroring `matchesUtxoPredicate`; Hedgehog unit properties.

Build: `cabal build cardano-rpc && cabal test cardano-rpc-test`

## Phase 5: WatchMempool (AC4)

- `Submit.hs` (or a new `Mempool.hs` if `Submit.hs` grows unwieldy): dependency-injected `watchMempoolStream` + `watchMempoolMethod`; STM change detection per the LocalTxMonitor pattern; additions via `snapshotTxsAfter`; predicate filter and field mask.
- `Server.hs`: replace the `watchMempool` slot with `Method (mkServerStreaming ...)`.
- Unit tests on the injected stream function.

Build: `cabal build cardano-rpc && cabal test cardano-rpc-test`

## Phase 6: WaitForTx (AC5)

- Dependency-injected `waitForTxStream` (per-ref state machine over merged mempool/follower events) + `waitForTxMethod` using `withFollower`.
- `Server.hs`: replace the `waitForTx` slot.
- Unit tests for the state machine, including eviction (no message, keeps waiting) and the race contract.

Build: `cabal build cardano-rpc && cabal test cardano-rpc-test`

## Phase 7: E2E tests (piece 10 slice of AC2/AC4/AC5)

- `cardano-testnet-test`: WatchMempool receives a submitted tx; WaitForTx delivers `STAGE_CONFIRMED`; ReadMempool answers well-formed.
- Error-status assertions drive calls via `withRPC`/`recvOutput` (AC7).

Build: `cabal build cardano-rpc && TASTY_PATTERN='/rpc/' cabal test cardano-testnet-test`

## Design decisions

- **Expose the raw `Mempool` handle, not per-use wrappers.**
  Mirrors `chainDb`; read, watch and submit share one handle, and STM composability (`check`/`retry`) requires the STM `getSnapshot` itself.
  [plan-snapshot-api-phases.md](plan-snapshot-api-phases.md) phase 5's `submitTx` becomes a helper over this field.
- **Change detection compares the full ticket list plus `snapshotSlotNo`.**
  The in-tree LocalTxMonitor server does exactly this; a max-ticket comparison misses removal-only changes.
- **WaitForTx eviction is non-terminal and the submit-wait race is a documented client contract.**
  No rejection stage exists in the proto, and chain churn can re-add txs; clients apply deadlines and open the stream before submitting.
- **Stages ACKNOWLEDGED and NETWORK are never emitted.**
  Neither is locally observable; the spec's generated docs impose no semantics for them.
