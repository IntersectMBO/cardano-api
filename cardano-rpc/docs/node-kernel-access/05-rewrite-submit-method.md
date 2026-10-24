# Piece 5: Rewrite SubmitTx to use NodeKernelAccess

## Problem

`submitTxMethod` in `Submit.hs` uses Node-to-Client IPC for both era detection (`determineEra`) and transaction submission (`submitTxToNodeLocal`).
Each call opens a separate N2C connection, adding latency and requiring `tryAny` wrapping to handle connection-level exceptions.
This must be replaced with the `NodeKernelAccess` interface introduced in piece 1.

## Why

Eliminating N2C from the submit path removes a connection per request, avoids double serialisation, and brings `submitTxMethod` in line with the node kernel access architecture (ADR-019).

## User value

As a cardano-rpc operator, I want transaction submission to use in-process ledger access so that submit latency is lower and N2C connection failures are no longer a failure mode.

## Acceptance criteria

1. **AC1: Era detection via snapshot** - `submitTxMethod` determines the current era by calling `withNodeKernelAccess laRef` then `nkaWithSnapshot` and `runQuery snapshot QueryCurrentEra`, replacing the previous `determineEra nodeConnInfo` N2C call.
   - Test: manual - verify by code inspection that `determineEra` is no longer called in `Submit.hs`

2. **AC2: Submission via nkaSubmitTx** - Transaction submission calls `withNodeKernelAccess laRef $ \la -> nkaSubmitTx la (TxInMode sbe tx)`, a separate `withNodeKernelAccess` invocation from the era detection in AC1.
   Era detection requires a ledger state snapshot; submission goes through the mempool and does not.
   These are intentionally two separate `withNodeKernelAccess` calls to avoid holding a forker open during mempool insertion.
   - Test: manual - verify by code inspection that `submitTxToNodeLocal` is no longer called and that era detection and submission use separate `withNodeKernelAccess` calls

3. **AC3: N2C error wrapping removed** - The `tryAny` wrapping and `first TraceRpcSubmitN2cConnectionError` mapping are removed from the `submitTx` helper.
   N2C connection errors are no longer possible via the `NodeKernelAccess` path, so this error category does not apply.
   - Test: manual - `grep -En 'TraceRpcSubmitN2cConnectionError|tryAny' Submit.hs` returns no results

4. **AC4: Submit result pattern matching preserved** - The `SubmitFail`/`SubmitSuccess` (or equivalent `TxSubmitFail`/`TxSubmitSuccess`) pattern matching on the submission result is preserved.
   Validation errors are still wrapped as `TraceRpcSubmitTxValidationError` and the transaction ID is still extracted from the ledger transaction on success.
   - Test: E2E - `hprop_rpc_transaction` verifies the full submit-then-query round trip (existing test, no changes needed)

5. **AC5: Import housekeeping** - The following import changes are made in `Submit.hs`:
   - Added: `Cardano.Rpc.Server.Internal.NodeKernelAccess` (for `withNodeKernelAccess`, `nkaWithSnapshot`, `runQuery`, `nkaSubmitTx`)
   - Removed: `submitTxToNodeLocal` usage (from `Cardano.Api`)
   - Removed: `determineEra` usage (from `Cardano.Api`)
   - Removed: `throwExceptT` usage (from `Cardano.Rpc.Server.Internal.Error`)
   - Test: unit - `cabal build cardano-rpc` compiles with no errors and no new warnings

6. **AC6: Existing E2E test passes** - `hprop_rpc_transaction` in `cardano-testnet-test` passes without modification, confirming that observable behaviour is unchanged.
   - Test: E2E - run `hprop_rpc_transaction` via the testnet test suite

## Out of scope

- Removal of the `TraceRpcSubmitN2cConnectionError` constructor from `Tracing.hs` (covered by piece 1)
- Updates to `cardano-node` tracer references in `Rpc.hs` (piece 3)
- Changes to any other RPC method (`Query.hs`, `Eval.hs`, `Node.hs`)
- New tracing constructors for ledger-access errors (piece 1)
- Changes to `Monad.hs`, `Env.hs`, or `Server.hs` (piece 1: NodeKernelAccess types)
- The `NodeKernelAccess` module itself (piece 1)

## Definition of done

- [ ] All AC verifications completed (code inspection, compilation, E2E)
- [ ] `cabal build cardano-rpc` compiles with no errors or warnings
- [ ] `hprop_rpc_transaction` E2E test passes
- [ ] Nix CI checks pass (`nix build 'path:.#checks.x86_64-linux.test'` and `e2e`)
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean
- [ ] No build warnings

## Notes

- **Hard compile-time dependency on piece 1:** This piece imports `Cardano.Rpc.Server.Internal.NodeKernelAccess` and calls `withNodeKernelAccess`, `nkaWithSnapshot`, `runQuery`, and `nkaSubmitTx`.
  None of these exist until piece 1 is landed.
  The `Has (IORef (Maybe NodeKernelAccess)) RpcEnv` instance and the updated `MonadRpc` constraint are also delivered by piece 1.
- **TDD fit:** This is a behaviour-preserving refactor.
  The existing E2E test (`hprop_rpc_transaction`) is the primary verification.
  No new unit tests are introduced because testing `submitTxMethod` in isolation would require either a real node or a `NodeKernelAccess` mock, neither of which is established in this codebase yet.
  Compile success and the existing E2E are the practical gates.
- **Two withNodeKernelAccess calls by design:** Era detection opens a forker (via `nkaWithSnapshot`) to query the ledger state.
  Submission goes through the mempool (via `nkaSubmitTx`) and does not need a forker.
  Bundling them in a single `nkaWithSnapshot` callback would hold the forker open during mempool insertion, which is unnecessary and blocks other forker consumers.
- **Current file:** `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Submit.hs`
- **E2E test file:** `cardano-node/cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/Transaction.hs`

## Reference docs

- [Architecture and current state](analysis-architecture.md) - current N2C submit path
- [API signatures](prereqs-api-signatures.md) - `addLocalTxs` signature
