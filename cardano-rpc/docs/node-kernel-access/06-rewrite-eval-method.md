# Piece 6: Rewrite EvalTx to use NodeKernelAccess

## Problem

`evalTxMethod` in `Eval.hs` queries seven different ledger state values via Node-to-Client IPC using `executeLocalStateQueryExpr`.
This incurs a connection-per-request cost and CBOR serialisation round-trips that are unnecessary when the RPC server runs in-process with the node.

## Why

EvalTx is the most query-intensive RPC method.
Replacing N2C with direct `NodeKernelAccess` removes IPC overhead and double serialisation for all seven queries in a single call.

## User value

As a DApp developer calling `EvalTx`, I want transaction evaluation to use node kernel access so that execution unit estimates return faster and with lower resource cost.

## Acceptance criteria

1. **AC1: Single snapshot for all queries** - All seven queries (protocol parameters, UTxO by TxIn, system start, era history, stake delegation deposits, DRep state, stake pool parameters) execute inside a single `nkaWithSnapshot` callback, ensuring they share one `ReadOnlyForker` and see the same ledger state.
   - Test: manual - Code review confirms `evalTxMethod` calls `nkaWithSnapshot` exactly once and all `runQuery` calls are inside that callback.

2. **AC2: IORef NodeKernelAccess from environment** - `evalTxMethod` obtains `IORef (Maybe NodeKernelAccess)` via `grab` instead of `LocalNodeConnectInfo`.
   - Test: unit - The module compiles against the updated `MonadRpc` constraint that provides `Has (IORef (Maybe NodeKernelAccess)) e` instead of `Has LocalNodeConnectInfo e`.

3. **AC3: Era detection inside snapshot** - The current era is determined via `runQuery snapshot QueryCurrentEra` inside the snapshot callback, replacing the separate `determineEra` call over N2C.
   - Test: unit - Compilation succeeds; `determineEra` and `throwExceptT` are no longer called.

4. **AC4: Eon escapes the snapshot** - The `eon` existential (from `forEraInEon @Era`) is returned as part of the snapshot callback's result tuple, making it available for post-snapshot protobuf conversion and `evaluateTransaction`.
   - Test: unit - Compilation succeeds; the `obtainCommonConstraints eon $ do ...` block after the snapshot uses the `eon` value returned from the callback.

5. **AC5: No N2C imports remain** - `executeLocalStateQueryExpr`, `queryProtocolParameters`, `queryUtxo`, `querySystemStart`, `queryEraHistory`, `queryStakeDelegDeposits`, `queryDRepState`, `queryStakePoolParameters`, `VolatileTip`, and `determineEra` are no longer imported or called in `Eval.hs`.
   - Test: manual - Code review and `grep` confirm none of these names appear in `Eval.hs`.

6. **AC6: NodeKernelAccess import added** - `Eval.hs` imports `Cardano.Rpc.Server.Internal.NodeKernelAccess` (for `withNodeKernelAccess`, `LedgerSnapshot`, and `runQuery`).
   - Test: unit - The module compiles with the new import; no unused-import warning.

7. **AC7: Post-snapshot logic unchanged** - The `evaluateTransaction` call, redeemer data assembly, balance check, and protobuf response construction remain unchanged from the current implementation.
   - Test: manual - Code review confirms the `obtainCommonConstraints eon $ do ...` block is identical to the pre-rewrite version.

8. **AC8: Compiles without warnings** - `cabal build cardano-rpc` completes with no errors and no warnings (including `-Wunused-packages` and `-Wredundant-constraints`).
   - Test: unit - Build succeeds cleanly.

9. **AC9: Full test suite passes** - All tests in `cardano-rpc-test` pass after the rewrite, including the existing protobuf conversion tests (`hprop_mkProtoTxEval_success`, `hprop_mkProtoTxEval_with_errors`, `hprop_scriptWitnessIndex_to_redeemerPurpose`, `hprop_mkProtoRedeemer`, `hprop_scriptExecutionError_to_evalReport`).
   - Test: unit - `cabal test cardano-rpc-test` exits successfully with no failures.

10. **AC10: ExBudget results match baseline** - Submitting a transaction evaluation request to the rewritten method produces execution unit estimates consistent with known mainnet baselines or prior N2C results.
    - Test: manual - Run a known transaction evaluation against a local testnet and verify the returned `ExBudget` values, fee, and balance check results are reasonable and match expectations.

## Out of scope

- Changes to `extractBalanceCheckCreds` (pure helper, unaffected by the data source change).
- Changes to `mkProtoTxEval`, `mkProtoRedeemer`, `scriptExecutionErrorToEvalReport`, or any protobuf conversion code in `Type.hs`.
- Changes to the post-snapshot evaluation block (`obtainCommonConstraints eon $ do ...` containing `evaluateTransaction`, redeemer assembly, balance checking, and response construction).
- Adding an automated integration test for `EvalTx` (this is a known gap; piece 8 validates the full method end-to-end).
- Changes to other RPC methods (`Query.hs`, `Submit.hs`, `Node.hs`); those are covered by separate pieces.
- Removing `import Cardano.Api` from `Eval.hs`; the module uses many `Cardano.Api` names for era handling and serialisation, so the blanket import stays.
- Removing `import Cardano.Rpc.Server.Internal.Error` from `Eval.hs`; `throwEither` is still used by `putTraceThrowEither`.

## Definition of done

- [ ] All AC tests written (compile, fail on stubs)
- [ ] Implementation complete (all tests pass via `cabal test`)
- [ ] Nix CI checks pass (`nix build 'path:.#cardano-rpc:lib:cardano-rpc'` and `nix build 'path:.#cardano-rpc:test:cardano-rpc-test'`)
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean
- [ ] No build warnings

## Notes

### Dependency

This piece requires piece 1 (NodeKernelAccess types) to be complete.
Without `Cardano.Rpc.Server.Internal.NodeKernelAccess` and the updated `MonadRpc` constraint (from piece 1), this code cannot compile.

### Snapshot consistency invariant

Snapshot consistency is the most critical invariant in this rewrite.
If protocol parameters come from block N but UTxOs come from block N+1, transaction evaluation will produce invalid results (wrong fee calculations, incorrect script execution budgets, or spurious failures).
The `nkaWithSnapshot` pattern guarantees all seven queries share one `ReadOnlyForker`, preserving the same consistency guarantee as the current `executeLocalStateQueryExpr` block.

### Eon existential escape

The `eon` value determined inside the snapshot callback must be returned as part of the result tuple.
It is needed by the post-snapshot `obtainCommonConstraints eon $ do ...` block for `evaluateTransaction` and protobuf conversion.
See the prerequisites document (section 4.1) for details on why this works with existential types.

### Integration test gap

No existing automated test exercises `evalTxMethod` end-to-end.
The unit tests in `Test.Cardano.Rpc.Eval` cover protobuf conversion helpers (`mkProtoTxEval`, `mkProtoRedeemer`, `scriptExecutionErrorToEvalReport`) but not the method itself.
Piece 8 (integration testing) is expected to close this gap.
AC10 covers a manual verification in the interim.

### Query arguments unchanged

The seven query arguments come from `extractBalanceCheckCreds` and `allInputs` exactly as in the current implementation.
The rewrite changes only the call surface (`runQuery snapshot (QueryInMode ...)` instead of `throwEither =<< throwEither =<< queryXxx sbe ...`), not the arguments themselves.
Specifically: `QueryUTxOByTxIn allInputs`, `apiStakeCreds` (for stake delegation deposits), `unregDRepCreds` (for DRep state), and `apiPoolIds` (for stake pool parameters).

### Import changes summary

- Add: `Cardano.Rpc.Server.Internal.NodeKernelAccess (withNodeKernelAccess, LedgerSnapshot (..))`
- Remove: `Cardano.Rpc.Server.Internal.Error` is kept (still used by `putTraceThrowEither`)
- The duplicate qualified import (`U5c` and `UtxoRpc` both pointing to `Cardano.Rpc.Proto.Api.UtxoRpc.Submit`) is pre-existing and not addressed by this piece.

## Reference docs

- [Consensus protocol and snapshots](analysis-consensus-protocol.md) - snapshot consistency (critical for 7-query EvalTx)
- [Implementation details](prereqs-implementation-details.md) - full EvalTx query inventory
- [API signatures](prereqs-api-signatures.md) - query type signatures
