# Piece 8: Integration testing and validation

## Problem

All seven preceding pieces replace the N2C IPC path with node kernel access, but there is no explicit validation that the existing integration tests still pass, that no N2C artefacts remain in the codebase, and that new tracing constructors appear in logs.
Without this validation gate, the ADR-019 migration could regress end-to-end behaviour silently.

## Why

This is the final piece in the ADR-019 node kernel access migration.
It gates the merge of the whole effort by confirming that the observable behaviour of cardano-rpc is unchanged from the perspective of gRPC clients, while verifying that the internal wiring has shifted entirely to in-process ledger access.

## User value

As a cardano-rpc maintainer, I want to confirm that the node kernel access migration preserves all existing integration test behaviour and identify any new test coverage gaps, so that the ADR-019 work can be merged with confidence.

## Acceptance criteria

1. **AC1: `hprop_rpc_query_pparams` passes** - The existing E2E test that validates all 44 protocol parameters match between the gRPC response and the ledger state passes without modification.
   The test connects via `Rpc.withConnection` to the gRPC socket, which is unchanged by this migration.
   - Test: E2E - `TASTY_PATTERN='/RPC Query Protocol Params/' cabal test cardano-testnet-test`

2. **AC2: `hprop_rpc_transaction` passes** - The existing E2E test that performs a full round-trip (fetch UTxOs via SearchUtxos, build transaction, submit via SubmitTx, confirm on-chain via SearchUtxos) passes without modification.
   - Test: E2E - `TASTY_PATTERN='/RPC Transaction Submit/' cabal test cardano-testnet-test`

3. **AC3: `hprop_rpc_search_utxos` passes** - The existing E2E test that exercises SearchUtxos with exact-address predicates, payment-credential predicates, non-matching predicates, and predicate-less queries passes without modification.
   - Test: E2E - `TASTY_PATTERN='/RPC SearchUtxos/' cabal test cardano-testnet-test`

4. **AC4: No RPC test file changes** - No files under `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/` are modified by the ADR-019 branch relative to the base branch.
   `Testnet/Types.hs` is also unchanged; `nodeRpcSocketPath` still delegates to `nodeSocketPathToRpcSocketPath`.
   - Test: manual - `git diff main -- cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/ cardano-testnet/src/Testnet/Types.hs` shows zero changes

5. **AC5: No N2C trace constructors remain** - `TraceRpcSubmitN2cConnectionError` does not appear anywhere in the cardano-rpc or cardano-node source trees.
   The constructor was removed in piece 1 and replaced by `TraceRpcNodeKernelAccessUnavailable` and `TraceRpcForkerError`.
   - Test: unit - `grep -r 'TraceRpcSubmitN2cConnectionError' cardano-api/cardano-rpc/src/ cardano-node/cardano-node/src/` returns no matches

6. **AC6: No N2C connection imports in RPC methods** - The modules `Query.hs`, `Submit.hs`, `Eval.hs`, and `Node.hs` no longer import `executeLocalStateQueryExpr`, `submitTxToNodeLocal`, `determineEra`, or `VolatileTip` from `Cardano.Api`.
   - Test: unit - `grep -l 'executeLocalStateQueryExpr\|submitTxToNodeLocal\|determineEra\|VolatileTip' cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/*.hs cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/Node.hs` returns no matches

7. **AC7: No runtime N2C connection from cardano-rpc** - The absence of `executeLocalStateQueryExpr`, `submitTxToNodeLocal`, and `determineEra` imports (AC6) is the static proof that cardano-rpc no longer opens N2C connections at runtime.
   As additional confirmation, a manual testnet run should show no `connectToLocalNode` traces originating from cardano-rpc in the node logs.
   - Test: manual - inspect structured trace output from a testnet run and confirm no N2C connection traces are attributed to cardano-rpc

8. **AC8: New trace constructors appear in trace configuration** - `NodeKernelAccessUnavailable` and `ForkerError` appear in the `allNamespaces` list of `MetaTrace TraceRpc` in `cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs`.
   - Test: unit - `grep -c 'NodeKernelAccessUnavailable\|ForkerError' cardano-node/cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs` returns at least 2

9. **AC9: Coverage gaps documented** - A note is added to this story (or a follow-up ticket is filed) listing the known integration test coverage gaps:
   (a) No E2E test for EvalTx (the `evalTxMethod` is exercised only by unit tests on the protobuf conversion layer, not end-to-end).
   (b) No E2E test for the UNAVAILABLE gRPC response during startup before `NodeKernel` is ready.
   (c) No E2E test for forker acquisition failure.
   (d) No E2E test for snapshot consistency under concurrent ledger state changes.
   - Test: manual - reviewer confirms the gaps are documented

## Out of scope

- Writing new E2E tests for EvalTx, UNAVAILABLE startup response, forker errors, or snapshot consistency (documented as gaps in AC9; to be addressed in separate stories).
- Performance benchmarking of node kernel access vs N2C (separate initiative).
- Changes to `Testnet/Types.hs`, `Testnet/Start/Types.hs`, or any RPC test file.
- Modifications to the gRPC client library (`Cardano.Rpc.Client`).
- Changes to protobuf definitions or generated code.

## Definition of done

- [ ] All three E2E tests pass: `hprop_rpc_query_pparams`, `hprop_rpc_transaction`, `hprop_rpc_search_utxos`
- [ ] N2C artefact checks pass (AC5, AC6)
- [ ] New trace constructors verified (AC8)
- [ ] No RPC test files modified (AC4)
- [ ] Coverage gaps documented (AC9)
- [ ] Nix CI checks pass (`nix build 'path:/work/cardano-api#checks.x86_64-linux.test'` and `e2e`)
- [ ] haskell-reviewer agent finds no critical or style issues across all ADR-019 changes
- [ ] fourmolu clean
- [ ] No build warnings

## Notes

- **Dependency:** all pieces 1-7 must be complete before this piece can be validated.
  This is the merge gate for the entire ADR-019 node kernel access effort.
- **Three E2E tests, not two:** The implementation plan's Step 13 mentions two tests (`hprop_rpc_query_pparams` and `hprop_rpc_transaction`), but `hprop_rpc_search_utxos` also exists in `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/SearchUtxos.hs` and exercises the same gRPC client path.
  All three must pass.
- **Testnet wiring is automatic:** The testnet creates a real `cardano-node` with `runtimeEnableRpc = RpcEnabled` (or `cardanoEnableRpc = RpcEnabled` in `SearchUtxos`).
  The `Run.hs` wiring in piece 3 populates the `IORef (Maybe NodeKernelAccess)` via `rnNodeKernelHook`, so the RPC server transitions from UNAVAILABLE to serving automatically.
- **gRPC client path is unchanged:** Tests connect via `Rpc.withConnection def (Rpc.ServerUnix rpcSocket)`, which talks to the gRPC Unix socket.
  The internal switch from N2C to node kernel access is invisible at this layer.
- **Unit tests in cardano-rpc-test:** The unit tests in `cardano-rpc/test/` (protocol parameter conversion, predicate matching, eval protobuf construction, pagination, type conversion) test the conversion layer which is unaffected by this migration.
  They should continue to pass without changes.
- **File locations:**
  - E2E tests: `cardano-node/cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/{Query,Transaction,SearchUtxos}.hs`
  - Test runner: `cardano-node/cardano-testnet/test/cardano-testnet-test/cardano-testnet-test.hs`
  - Testnet types: `cardano-node/cardano-testnet/src/Testnet/Types.hs` (exports `nodeRpcSocketPath`)
  - Node wiring: `cardano-node/cardano-node/src/Cardano/Node/Run.hs`
  - Trace instances: `cardano-node/cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs`

## Reference docs

- [Implementation details](prereqs-implementation-details.md) - verification checklist
- [Build and conventions](prereqs-build-and-conventions.md) - test commands
- [Architecture and current state](analysis-architecture.md) - spec coverage gaps
