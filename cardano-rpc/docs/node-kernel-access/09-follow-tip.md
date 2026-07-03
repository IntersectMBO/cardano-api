# Piece 9: FollowTip streaming endpoint

## Problem

Chain-following clients (Kupo, Scrolls, Oura, custom indexers) currently need a direct N2C ChainSync connection to stream blocks from the node.
This requires local socket access, a Haskell-compatible Ouroboros implementation, and version negotiation.
There is no gRPC equivalent.

## Why

`FollowTip` is the gRPC equivalent of the Ouroboros ChainSync mini-protocol and the last method required by #1229 (replacing Ogmios as Kupo's chain data source).
It is the first server-streaming RPC in cardano-rpc and the first to use `ChainDB` follower capabilities.
After this piece, remote clients can follow the chain over TCP using standard gRPC libraries in any language.

## User value

As a chain indexer developer, I want to follow the chain tip over gRPC so that I can stream fully parsed blocks without implementing the Ouroboros ChainSync protocol or having local socket access to the node.

## Already in place

Do not re-plan these; they exist on the FetchBlock/ReadTip PR stack (#1247, #1258, #1259) and this piece builds on them.

- `sync.proto` already declares `rpc FollowTip(FollowTipRequest) returns (stream FollowTipResponse)` with `repeated BlockRef intersect` in the request and the `apply`/`undo`/`reset` oneof plus `BlockRef tip` in the response; proto-lens bindings are generated.
- `Server.hs` already registers a server-streaming stub for `followTip` (`mkServerStreaming $ \_ _ -> unimplemented`).
- `NodeKernelAccess` lives in cardano-rpc (`Cardano.Rpc.Server.NodeKernelAccess` and `.Type`) with fields `chainDb`, `systemStart` and `readEraHistory` (NoFieldSelectors); cardano-node only constructs it in `Cardano.Node.Run` and hands it over through an `IORef`.
- Full block-to-proto conversion exists: `fetchBlockMethod` assembles header, parsed transactions (`txToUtxoRpcTx`, all eras including Byron via `byronBlockTxs`) and slot timestamp.
- Tip projection exists: `readTipMethod` builds a fully populated `BlockRef` (slot, hash, height, timestamp) from a ChainDB header.
- Tracing pattern: `TraceRpcSync` span constructors wrapped via `wrapInSpan`, with seven exhaustive sites in cardano-node's `Cardano.Node.Tracing.Tracers.Rpc` (`forMachine`, `asMetrics`, `namespaceFor`, `severityFor`, `documentFor`, `metricsDocFor`, `allNamespaces`).

## Revised scope decision: parsed blocks are in scope

The original plan streamed `native_bytes` only.
That contradicts #1229: Kupo needs full transaction data (inputs, outputs, datums, scripts, metadata) in the stream, and re-parsing CBOR client-side defeats the purpose of the gRPC interface.
Since FetchBlock already builds the fully parsed `cardano` block, `FollowTip` reuses that assembly and streams both `native_bytes` and the parsed block.

## Phases

Each phase is one commit, builds with zero warnings from `/work`, and passes the full cardano-rpc test suite.
Phases 1-4 are cardano-api repo commits; phase 5 is the cardano-node repo counterpart.

### Phase 1: cardano-api re-exports for ChainDB followers

Extend `Cardano.Api.Consensus` (via the `ChainDB.` qualified exports and `Internal/Reexport.hs`) with the follower vocabulary:

- `ChainDB.newFollower`, the `Follower` type and its field accessors (`followerInstructionBlocking`, `followerForward`, `followerClose`), and `ChainDB.ChainType (..)`.
- `withRegistry` / `ResourceRegistry` (from the resource-registry package) so cardano-rpc does not need a direct ouroboros-consensus dependency.
- `ChainUpdate (..)` (`AddBlock` / `RollBack`) used by follower instructions.

Changelog fragment: cardano-api, `compatible`.

- Test: unit - compiles; no behaviour change.
- Review: export-list-only diff, mirrors the `getTipHeader` re-export commit in #1259.

### Phase 2: follower capability in NodeKernelAccess

In `Cardano.Rpc.Server.NodeKernelAccess`:

- `data ChainChange = ChainApply RawBlock | ChainRollBack ChainPoint` where `RawBlock` carries the raw CBOR and the parsed `BlockInMode` (same pair `fetchBlock` returns).
  Note: consensus `RollBack` carries only the rollback point, never the rolled-back blocks, which constrains what `undo` can contain (see the scope note in phase 4).
- `data ChainFollower = ChainFollower { nextChange :: IO ChainChange, findIntersect :: [ChainPoint] -> IO (Maybe ChainPoint) }`.
- `withFollower :: NodeKernelAccess -> (ChainFollower -> IO a) -> IO a` with bracket semantics: `withRegistry`, `ChainDB.newFollower` with a block component fetching raw bytes and the block (as in `fetchBlock`), `followerClose` on all exit paths.
- `nextChange` wraps `followerInstructionBlocking`, mapping `AddBlock` to `ChainApply` and `RollBack` to `ChainRollBack`; `findIntersect` wraps `followerForward`.

No server wiring yet; the stub stays.

- Test: unit - compiles; the E2E test in phase 5 exercises it.
- Review: self-contained module addition, no call sites change.

### Phase 3: extract shared block and tip assembly (pure refactor)

- Extract the response-body assembly from `fetchBlockMethod` into a shared function (raw bytes + `BlockInMode` + timestamp to `AnyChainBlock` with `native_bytes`, `cardano.header`, `cardano.body.tx`, `cardano.timestamp`).
- Extract the `BlockRef` tip projection from `readTipMethod` (header + timestamp to `BlockRef`).
- `fetchBlockMethod` and `readTipMethod` call the extracted functions; behaviour is unchanged.

- Test: existing cardano-rpc suite passes unchanged; E2E FetchBlock/ReadTip assertions in cardano-node still pass.
- Review: pure code motion, diff should show no logic edits.

### Phase 4: followTipMethod handler, wiring, tracing, README

- `followTipMethod` in `Cardano.Rpc.Server.Internal.UtxoRpc.Sync`, the grapesy server-streaming handler (request plus a send callback):
  1. `grabNodeKernelAccess`, then `withFollower`.
  2. Convert `repeated BlockRef intersect` to `[ChainPoint]`; reject malformed hashes with `INVALID_ARGUMENT` (as FetchBlock does).
  3. Non-empty intersect: `findIntersect`; on `Nothing` send a `reset` action carrying the origin `BlockRef` and continue from origin.
     Empty intersect: skip intersection and stream from origin (a fresh follower starts there); this is the initial-sync path for indexers.
     After a successful `followerForward`, the follower's first instruction is a `RollBack` to the intersection point - it comes out as the initial `reset`, telling the client where streaming resumes.
  4. Loop: `nextChange`, assemble the response with the phase 3 helpers, attach the current tip via `getTipHeader` and the tip projection, send, repeat.
     `ChainApply` becomes an `apply` action with the full parsed block; `ChainRollBack` becomes a `reset` action with the rollback point's `BlockRef`.
     Scope note: the UTxO RPC spec also allows `undo` actions carrying the rolled-back blocks, but consensus followers do not provide them (`RollBack` is point-only); serving `undo` needs a per-stream buffer of recently applied blocks, which is deferred (see out of scope).
     Clients handle `reset` exactly like ChainSync's `RollBackward`, which is what Kupo's sync loop already does.
  5. Cleanup on client disconnect, stream close or exception is guaranteed by the `withFollower` bracket (grapesy cancels the handler thread on disconnect).
- New `TraceRpcSync` constructors: `TraceRpcFollowTipSpan TraceSpanEvent` and `TraceRpcFollowTipReset` (intersection not found).
  No per-block apply/undo traces: they would fire for every block on the chain and the prometheus request counter plus consensus block traces already cover it.
  Note: `wrapInSpan` wraps unary handlers; the streaming handler needs a streaming-shaped span wrapper (begin on stream open, end on stream close) - add `wrapInSpanStreaming` next to it.
- `Server.hs`: replace the followTip stub.
- README: FollowTip row to supported.
- Changelog fragment: cardano-rpc, `feature`.

- Test: unit - compiles; handler logic is exercised end-to-end in phase 5.
- Review: the only phase with real logic; everything below it is already reviewed.

### Phase 5: cardano-node tracing and E2E test (cardano-node repo)

- `Cardano.Node.Tracing.Tracers.Rpc`: extend all seven exhaustive sites for the two new constructors; `rpc.request.SyncService.FollowTip` counter on span begin; severity Debug for the span, Info for reset.
- New `Cardano.Testnet.Test.Rpc.FollowTip` with `hprop_rpc_follow_tip`:
  1. Start a testnet with RPC enabled (as in the FetchBlock test).
  2. Open a `FollowTip` stream with an empty intersect list; read the first events and assert they are `apply` actions with non-empty `native_bytes`, populated `cardano.header`, and a `tip` with a 32-byte hash.
  3. Submit a transaction over gRPC, keep reading until an `apply` block contains its hash in `cardano.body.tx`, and assert the parsed transaction fields against the submitted values.
  4. Re-open a stream with the intersect set to an already-seen block ref and assert streaming resumes after that point (no replay from origin).
  5. Assert an unknown intersect ref yields a `reset` as the first message.
- Register the test in `cardano-testnet-test.hs`.

- Test: `TASTY_PATTERN='/RPC FollowTip/' cabal test cardano-testnet-test`.
- Review: tracer arms are mechanical; the test is the substance.

## Out of scope

- `FieldMask` support (also ignored by the other query methods).
- Concurrent follower limits and idle timeouts (follow-up; the node already supports one follower per N2C ChainSync client, so the mechanism scales the same way).
- `undo` actions with full rolled-back blocks: consensus `RollBack` is point-only, so rollbacks are streamed as `reset` (ChainSync-equivalent semantics, sufficient for Kupo).
  Serving `undo` needs a per-stream ring buffer of applied blocks (or a VolatileDB fetch racing against garbage collection) - follow-up if a client needs it.
- Rollback E2E coverage: a single-node testnet cannot produce a rollback; the rollback mapping is covered by the type-level totality of the `ChainUpdate` translation and left to conformance testing against other implementations.
- `DumpHistory` (separate piece).
- Backpressure tuning beyond gRPC's built-in HTTP/2 flow control.

## Definition of done

- [ ] Phases land as separate commits, each building with zero warnings and passing tests.
- [ ] E2E test passes: `TASTY_PATTERN='/RPC FollowTip/' cabal test cardano-testnet-test`.
- [ ] Nix CI checks pass.
- [ ] fourmolu clean (`scripts/devshell/prettify` on changed files; cardano-node has no prettify script).
- [ ] Changelog fragments present with the real PR numbers (herald counts only fragments added since the fork point).
- [ ] README coverage table updated.

## Notes

### Key differences from Ouroboros ChainSync

| Aspect | ChainSync | FollowTip |
|--------|-----------|-----------|
| Transport | Unix socket (N2C) | TCP/gRPC |
| Rollback | `MsgRollBackward` with the point | `reset` action with the point's `BlockRef` (`undo` with full blocks deferred) |
| No-intersect | `MsgIntersectNotFound` | `reset` action to origin |
| Pipelining | Client-driven (`MsgRequestNextPipelined`) | Server-driven (gRPC stream backpressure) |
| Block format | CBOR | Proto (`AnyChainBlock` with `native_bytes` and parsed block) |

### Why N2C ChainSync is insufficient

Each gRPC `FollowTip` client would need its own N2C socket connection with a dedicated ChainSync follower.
The node creates a fresh follower per N2C connection with no way to multiplex.
In-process access via `ChainDB.newFollower` lets cardano-rpc create followers directly without socket overhead.

### Tip reporting

Each `FollowTipResponse` includes the current tip, read via `getTipHeader` and projected with the same function `readTipMethod` uses.
This lets clients measure sync lag (difference between their position and the tip).

### Files affected

**cardano-api repo:**

| File | Phase | Change |
|---|---|---|
| `cardano-api/src/Cardano/Api/Consensus.hs` and `Consensus/Internal/Reexport.hs` | 1 | Follower re-exports. |
| `cardano-rpc/src/Cardano/Rpc/Server/NodeKernelAccess.hs` (and `.Type`) | 2 | `withFollower`, `ChainFollower`, `ChainChange`. |
| `cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Sync.hs` | 3, 4 | Extracted assembly helpers; `followTipMethod`. |
| `cardano-rpc/src/Cardano/Rpc/Server/Internal/Tracing.hs` | 4 | `TraceRpcFollowTipSpan`, `TraceRpcFollowTipReset`, `wrapInSpanStreaming`. |
| `cardano-rpc/src/Cardano/Rpc/Server.hs` | 4 | Register `followTipMethod`. |
| `cardano-rpc/README.md`, `.changes/` | 1, 4 | Coverage row, fragments. |

**cardano-node repo:**

| File | Phase | Change |
|---|---|---|
| `cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs` | 5 | Two new constructors across all seven case sites plus counter. |
| `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/FollowTip.hs` | 5 | New E2E test. |
| `cardano-testnet/test/cardano-testnet-test/cardano-testnet-test.hs` | 5 | Register the test. |

### Gotchas for the implementer

- **grapesy server-streaming.** First streaming handler in the codebase; the `mkServerStreaming` handler receives the request and a send callback instead of returning a response.
  `wrapInSpan` does not fit its shape - add a streaming variant.
- **`followerInstructionBlocking`, not `followerInstruction`.** The non-blocking variant would busy-wait.
- **Rollbacks are point-only.** `ChainUpdate`'s `RollBack` never carries the rolled-back blocks; do not plan for `undo`-with-blocks without a per-stream buffer.
  After `followerForward` succeeds, the first instruction is a `RollBack` to the intersection - expected, not an error.
- **`ChainDB.newFollower` needs a `ResourceRegistry`.** Use `withRegistry`; tie follower cleanup to the bracket, not to garbage collection.
- **GHC 9.6/9.10 MonoLocalBinds.** Local bindings whose type comes out of a constraint continuation (`anyEraTxConstraints`) and mid-do GADT dispatches need explicit type signatures; 9.12 infers them, so these break only in CI (see AGENTS.md GHC gotchas).
- **Proto is already generated.** Do not touch `gen/`; no codegen step is needed for this piece.
- **New trace constructors break cardano-node compilation** until all seven case sites in `Tracers/Rpc.hs` are extended; coordinate the cardano-api and cardano-node PRs like FetchBlock/ReadTip did.

### Risks

| Risk | Mitigation |
|------|------------|
| Follower leak on unclean client disconnect | Bracket in `withFollower`; grapesy cancels the handler thread on disconnect, which unwinds the bracket |
| High memory per follower with many clients | Out of scope; concurrent limits can be added later |
| Parsed-block assembly cost per streamed block | Same conversion FetchBlock already does once per request; if profiling shows pressure, a `FieldMask` fast path can skip tx parsing later |
| `followerForward` intersection semantics differ from expectation | Covered by E2E steps 4 and 5 (resume after known point, reset on unknown point) |

### Dependencies

- **Upstream:** the FetchBlock/ReadTip PR stack (#1247, #1258, #1259) must merge first; phase 1 also depends on the pinned consensus version exposing the follower API.
- **Downstream:** Kupo's gRPC `ChainProducer` backend (out of scope here, tracked by #1229).

## Reference docs

- [Architecture and current state](analysis-architecture.md) - cardano-rpc overview and spec coverage
- [Build and conventions](prereqs-build-and-conventions.md) - build instructions
- GitHub issues: #1219 (FollowTip), #1229 (Kupo/Ogmios replacement)
