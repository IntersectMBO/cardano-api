# Piece 7: Rewrite Node methods to use NodeKernelAccess

## Problem

`getProtocolParamsJsonMethod` in `Node.hs` uses the N2C pattern (`determineEra` + `executeLocalStateQueryExpr` + `queryProtocolParameters`) which opens a new socket connection per request and double-serialises data.
This is the last remaining method in `Node.hs` that needs migrating to the snapshot-based `NodeKernelAccess` interface.

## Why

Replacing the N2C call path with node kernel access removes per-request socket overhead and double serialisation for this method, bringing `Node.hs` in line with the new architecture defined in ADR-019.

## User value

As a cardano-rpc maintainer, I want `getProtocolParamsJsonMethod` to use the snapshot-based `NodeKernelAccess` interface so that the Node service no longer depends on N2C for protocol parameter queries.

## Acceptance criteria

1. **AC1: `getEraMethod` unchanged** - The body of `getEraMethod` remains byte-identical to its current form (hardcoded Conway return).
   Making it dynamic is out of scope until new eras are added.
   - Test: manual - `git diff` shows no changes to `getEraMethod`

2. **AC2: Snapshot-based protocol parameter query** - `getProtocolParamsJsonMethod` obtains a `NodeKernelAccess` via `grab @(IORef (Maybe NodeKernelAccess))` and queries protocol parameters inside a single `nkaWithSnapshot` callback using `runQuery`.
   Both `pparams` and `eon` are returned from the snapshot block so that `obtainCommonConstraints eon $ A.encode pparams` continues to work outside it.
   - Test: manual - code review confirms the snapshot pattern is used correctly

3. **AC3: Era detection inside snapshot** - The current era is determined via `runQuery snapshot QueryCurrentEra` inside the snapshot callback, replacing the N2C `determineEra` call.
   The `forEraInEon @Era` guard and `convert eon` pattern are preserved.
   - Test: E2E - `hprop_rpc_query_pparams` validates the returned era-specific protocol parameters match the ledger

4. **AC4: Import cleanup and clean compilation** - `Cardano.Rpc.Server.Internal.Error` is removed from imports (no more `throwExceptT`, `throwEither`).
   `Cardano.Rpc.Server.Internal.NodeKernelAccess` is added.
   `cabal build cardano-rpc` succeeds with no warnings related to `Node.hs`.
   - Test: unit - `cabal build cardano-rpc` exits with code 0 and no warnings

5. **AC5: E2E behavioural equivalence** - `hprop_rpc_query_pparams` passes, confirming that the rewritten method returns identical protocol parameters, ledger tip slot, block hash, and block number to the N2C implementation.
   - Test: E2E - `hprop_rpc_query_pparams` in `cardano-testnet-test`

## Out of scope

- Making `getEraMethod` dynamic (future work when new eras arrive).
- Changes to `Env.hs`, `Monad.hs`, or `Server.hs` (covered by piece 1).
- Creating `NodeKernelAccess.hs` itself (piece 1 prerequisite).
- Rewriting methods in `Query.hs`, `Eval.hs`, or `Submit.hs` (pieces 4-6).
- Tracing changes in `Tracing.hs` or `Rpc.hs` (piece 1 and piece 3).
- Changes to `cardano-rpc.cabal` (piece 1 adds the new module).

## Definition of done

- [ ] All AC verifications mapped to existing tests or confirmed via code review
- [ ] Implementation complete (all existing tests still pass via `cabal test`)
- [ ] Nix CI checks pass
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean
- [ ] No build warnings

## Notes

- **Dependency:** piece 1 (NodeKernelAccess types) must be merged first.
  The environment and monad wiring that puts `IORef (Maybe NodeKernelAccess)` into `RpcEnv` and `MonadRpc` is delivered by piece 1 and must be in place before this piece compiles.
- **`eon` escapes the snapshot block** because it is needed at line 50 of the current code: `obtainCommonConstraints eon $ A.encode pparams`.
  The implementer must return `(pparams, eon)` from the `nkaWithSnapshot` callback, not just `pparams`.
- **Error handling change:** The current code uses `throwExceptT` and nested `throwEither` chains for N2C error conversion.
  The new code relies on `withNodeKernelAccess` throwing `GrpcException` with `GrpcUnavailable` if the kernel is not yet initialised, and on `runQuery` propagating exceptions directly.
  This is simpler but changes the exception type from `RpcException` to `GrpcException` for the "not initialised" case.
- **Smallest piece in the plan:** This rewrites one method in one file.
  It is a good candidate for a quick early win and a template for the larger Query/Eval/Submit rewrites.
- **File:** `cardano-api/cardano-rpc/src/Cardano/Rpc/Server/Internal/Node.hs`

## Reference docs

- [Architecture and current state](analysis-architecture.md) - current N2C query path
- [API signatures](prereqs-api-signatures.md) - query type signatures
