# Piece 9: FollowTip streaming endpoint

## Problem

Chain-following clients (Kupo, Scrolls, Oura, custom indexers) currently need a direct N2C ChainSync connection to stream blocks from the node.
This requires local socket access, a Haskell-compatible Ouroboros implementation, and version negotiation.
There is no gRPC equivalent.

## Why

`FollowTip` is the gRPC equivalent of the Ouroboros ChainSync mini-protocol - the most architecturally significant method in the UTxO RPC spec.
It is the first server-streaming RPC in cardano-rpc and the first to use `ChainDB` follower capabilities.
After this piece, remote clients can follow the chain over TCP using standard gRPC libraries in any language.

## User value

As a chain indexer developer, I want to follow the chain tip over gRPC so that I can stream blocks without implementing the Ouroboros ChainSync protocol or having local socket access to the node.

## Acceptance criteria

1. **AC1: NodeKernelAccess follower capability** - `NodeKernelAccess` gains a new field `nkaWithFollower :: forall a. ([ChainPoint] -> ChainFollower -> IO a) -> IO a`.
   The callback receives an intersection function and a `ChainFollower` handle.
   `nkaWithFollower` uses bracket semantics: the follower is closed when the callback returns or throws.
   - Test: unit - compiles

2. **AC2: ChainFollower type** - A new type `ChainFollower` is defined in the `NodeKernelAccess` module with:
   - `cfNextChange :: IO ChainChange` - blocks until the next chain event is available
   - `cfFindIntersect :: [ChainPoint] -> IO (Maybe ChainPoint)` - finds the most recent point from the list that is on the current chain
   `ChainChange` is a sum type with `ChainApply ChainPoint ByteString` (slot/hash + raw block CBOR) and `ChainUndo ChainPoint ByteString` (rollback with full block data).
   - Test: unit - compiles

3. **AC3: mkNodeKernelAccess implements nkaWithFollower** - `mkNodeKernelAccess` implements `nkaWithFollower` by calling `ChainDB.newFollower` inside `withRegistry`.
   The follower is created with `GetRawBlock` as the block component.
   `cfNextChange` wraps `followerInstruction` (blocking variant), mapping `AddBlock` to `ChainApply` and `RollBack` to `ChainUndo`.
   `cfFindIntersect` wraps the follower's intersection-finding capability.
   The follower is closed via `followerClose` when the bracket exits.
   - Test: E2E - `hprop_rpc_follow_tip`

4. **AC4: sync.proto FollowTip RPC** - The existing `sync.proto` (from piece 2) is extended with `rpc FollowTip(FollowTipRequest) returns (stream FollowTipResponse)`.
   `FollowTipRequest` has `repeated BlockRef intersect`.
   `FollowTipResponse` has `oneof action { AnyChainBlock apply = 1; AnyChainBlock undo = 2; BlockRef reset = 3; }` and a `BlockRef tip` field.
   Proto-lens bindings are regenerated.
   - Test: unit - compiles

5. **AC5: followTipMethod handler** - A new function `followTipMethod` in `Cardano.Rpc.Server.Internal.UtxoRpc.Sync` implements the server-streaming handler.
   On stream open:
   - Calls `withNodeKernelAccess` then `nkaWithFollower`.
   - Converts the client's `repeated BlockRef intersect` to `[ChainPoint]`.
   - Calls `cfFindIntersect`. If no intersection found, sends a `reset` action to genesis.
   On each iteration:
   - Calls `cfNextChange` (blocks until next event).
   - Maps `ChainApply` to an `apply` action and `ChainUndo` to an `undo` action.
   - Populates `native_bytes` in the `AnyChainBlock`.
   - Queries the current tip via `nkaWithSnapshot` and populates the `tip` field.
   - Sends the `FollowTipResponse` on the stream.
   On stream close:
   - The bracket in `nkaWithFollower` ensures the follower is cleaned up.
   - Test: E2E - `hprop_rpc_follow_tip`

6. **AC6: Server.hs registers FollowTip** - `Server.hs` registers `followTipMethod` in the `SyncService` alongside `fetchBlockMethod`.
   - Test: unit - compiles

7. **AC7: Tracing** - New trace constructors `TraceRpcFollowTipStarted`, `TraceRpcFollowTipEnded`, `TraceRpcFollowTipApply`, `TraceRpcFollowTipUndo`, `TraceRpcFollowTipReset` are added.
   `LogFormatting` and `MetaTrace` instances are updated in `Cardano.Node.Tracing.Tracers.Rpc`.
   - Test: unit - compiles (`-Wincomplete-patterns` catches missing branches)

8. **AC8: E2E test** - A new test `hprop_rpc_follow_tip` starts a testnet, opens a `FollowTip` stream with an empty intersect list, submits a transaction, reads events from the stream until a block containing the transaction arrives as an `apply` action, and verifies the `native_bytes` is non-empty and the `tip` field has a valid slot.
   - Test: E2E - `TASTY_PATTERN='/RPC FollowTip/' cabal test cardano-testnet-test`

9. **AC9: Build clean** - `cabal build cardano-rpc`, `cabal build cardano-node`, and all existing tests pass with no errors or warnings.
   - Test: E2E - full CI pass

## Out of scope

- Parsed `cardano.Block` population in `AnyChainBlock` (native_bytes only).
- `FieldMask` support.
- Concurrent follower limits or idle timeouts (follow-up).
- `DumpHistory` (separate piece).
- ReadTip (separate issue, can use existing N2C path).
- Backpressure tuning beyond gRPC's built-in HTTP/2 flow control.

## Definition of done

- [ ] All AC tests written (compile, fail on stubs)
- [ ] Implementation complete (all tests pass)
- [ ] `cabal build cardano-rpc` succeeds with no warnings
- [ ] `cabal build cardano-node` succeeds with no warnings
- [ ] E2E tests pass: `cabal test cardano-testnet-test --test-option='-p /RPC/'`
- [ ] Nix CI checks pass
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean (`scripts/devshell/prettify` run on changed files)
- [ ] No build warnings

## Notes

### Key differences from Ouroboros ChainSync

| Aspect | ChainSync | FollowTip |
|--------|-----------|-----------|
| Transport | Unix socket (N2C) | TCP/gRPC |
| Rollback data | Only the rollback point | Full block included in `undo` |
| No-intersect | `MsgIntersectNotFound` | `reset` action |
| Pipelining | Client-driven (`MsgRequestNextPipelined`) | Server-driven (gRPC stream backpressure) |
| Block format | CBOR | Proto (`AnyChainBlock` with `native_bytes`) |

### Why N2C ChainSync is insufficient

Each gRPC `FollowTip` client would need its own N2C socket connection with a dedicated ChainSync follower.
The node creates a fresh follower per N2C connection with no way to multiplex.
In-process access via `ChainDB.newFollower` lets cardano-rpc create followers directly without socket overhead.

### Tip reporting

Each `FollowTipResponse` includes the current chain tip via `nkaWithSnapshot` querying `VolatileTip`.
This lets clients measure sync lag (difference between their position and the tip).

### Follower lifecycle

The `nkaWithFollower` bracket ensures cleanup on all exit paths:
- Normal stream completion
- Client disconnect (gRPC cancellation)
- Server-side exception

Each active follower holds a pointer into the `ChainDB`.
The node already supports multiple concurrent followers (one per N2C ChainSync connection).

### Files affected

**cardano-rpc:**

| File | Change |
|---|---|
| `src/Cardano/Rpc/NodeKernelAccess.hs` | Add `nkaWithFollower`, `ChainFollower`, `ChainChange` types. |
| `proto/utxorpc/v1beta/sync/sync.proto` | Add `FollowTip` RPC and response types. |
| `gen/Proto/Utxorpc/V1beta/Sync/Sync.hs` | **Regenerated.** |
| `gen/Proto/Utxorpc/V1beta/Sync/Sync_Fields.hs` | **Regenerated.** |
| `src/Cardano/Rpc/Server/Internal/UtxoRpc/Sync.hs` | Add `followTipMethod`. |
| `src/Cardano/Rpc/Server.hs` | Register `followTipMethod` in SyncService. |
| `cardano-rpc.cabal` | Update module listings if needed. |

**cardano-node:**

| File | Change |
|---|---|
| `src/Cardano/Node/Rpc/NodeKernelAccess.hs` | Implement `nkaWithFollower` via `ChainDB.newFollower`. |
| `src/Cardano/Node/Tracing/Tracers/Rpc.hs` | Add `FollowTip` trace constructors. |

**cardano-testnet:**

| File | Change |
|---|---|
| `test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/FollowTip.hs` | **New.** `hprop_rpc_follow_tip`. |
| `test/cardano-testnet-test/cardano-testnet-test.hs` | Register new test. |

### Gotchas for the implementer

- **grapesy server-streaming API.** This is the first streaming handler in cardano-rpc. Study how grapesy exposes server-streaming methods - the handler signature differs from unary RPCs.

- **`followerInstruction` vs `followerInstructionBlocking`.** Use the blocking variant to avoid busy-waiting. It returns when the next chain event is available.

- **Rollback includes full block data.** The UTxO RPC spec includes the full block in `undo` actions, unlike ChainSync which only sends the rollback point. The follower must be created with `GetRawBlock` to have block data available for both directions.

- **`ChainDB.newFollower` requires `ResourceRegistry`.** Use `withRegistry` from `Control.ResourceRegistry` to manage the follower's lifecycle.

- **Proto codegen after modifying sync.proto.** Run `nix develop --command bash -c "cd cardano-rpc && buf generate proto"` to regenerate bindings.

### Risks

| Risk | Mitigation |
|------|------------|
| grapesy server-streaming API unfamiliar | Study grapesy examples and docs before implementation |
| Follower leak on unclean client disconnect | Bracket semantics in `nkaWithFollower`; gRPC cancellation triggers cleanup |
| High memory per follower with many clients | Out of scope for this piece; concurrent limits can be added later |
| `followerInstructionBlocking` semantics may differ across consensus versions | Verify the blocking variant exists and behaves as expected |

### Dependencies

- **Upstream:** pieces 1-3 (NodeKernelAccess types, sync.proto, mkNodeKernelAccess, node wiring).
- **Downstream:** piece 8 (integration testing).

### Testing approach

| AC | Type | What it tests |
|---|---|---|
| AC1 | unit | NodeKernelAccess follower field compiles |
| AC2 | unit | ChainFollower types compile |
| AC3 | E2E | Follower creation via mkNodeKernelAccess |
| AC4 | unit | Proto codegen compiles |
| AC5 | E2E | Full streaming handler end-to-end |
| AC6 | unit | Server registration compiles |
| AC7 | unit | Tracing instances compile |
| AC8 | E2E | Dedicated integration test |
| AC9 | E2E | Full regression |

## Reference docs

- [Architecture and current state](analysis-architecture.md) - cardano-rpc overview and spec coverage
- [Build and conventions](prereqs-build-and-conventions.md) - proto codegen instructions
- GitHub issue: #1219
