# FetchBlock implementation phases

Covers pieces 1-3 from the delivery plan.
Each phase compiles independently and can be reviewed as a separate commit.

## Phase 1: NodeKernelAccess types module ✅

New file only, no existing code touched.

- Created `cardano-rpc/src/Cardano/Rpc/Server/NodeKernelAccess.hs`
  - `NodeKernelAccessF` record (parametric in monad `m`): `nkaWithSnapshot`, `nkaSubmitTx`, `nkaFetchBlock`
  - `LedgerSnapshot` newtype: `runQuery :: forall result. QueryInMode result -> m result`
  - `hoistNodeKernelAccess` for mapping both covariant and contravariant monad occurrences
- Added module to `cardano-rpc.cabal`

## Phase 2: Wire NodeKernelAccess into Env/Monad/Server ✅

Plumbing change, no behaviour change.

- `Env.hs`: added `type NodeKernelAccess = NodeKernelAccessF (RIO RpcEnv)` alias and `rpcNodeKernelAccess :: !(IORef (Maybe NodeKernelAccess))` field to `RpcEnv`
- `Monad.hs`: added `instance (m ~ RIO RpcEnv) => Has (IORef (Maybe (NodeKernelAccessF m))) RpcEnv` using type equality trick
- `Server.hs`: `runRpcServer` accepts `IORef (Maybe NodeKernelAccess)`, constructs `RpcEnv` with it
- `Run.hs` (cardano-node): `nodeKernelAccessRef <- newIORef Nothing` before `withAsync`, passed through `rpcServerLoop` to `runRpcServer`

## Phase 3: Trace constructors ✅

Updated both packages atomically.

- Added `TraceRpcSync` sum type with `TraceRpcFetchBlockSpan`, `TraceRpcFetchBlockNotFound`, `TraceRpcNodeKernelAccessUnavailable`, `TraceRpcForkerError`
- Added `TraceRpcSync` constructor to `TraceRpc`
- Kept `TraceRpcSubmitN2cConnectionError` (N2C tracers remain for incremental migration)
- Updated cardano-node `Tracers/Rpc.hs` with all new trace constructors

## Phase 4: sync.proto and codegen ✅

- Copied `sync.proto` from UTxO RPC spec (`@utxorpc-spec`)
- Ran codegen via `buf generate proto`
- Created `src/Cardano/Rpc/Proto/Api/UtxoRpc/Sync.hs` wrapper (re-exports Sync and Cardano proto types/fields)
- Added generated modules and wrapper to `cardano-rpc.cabal`

## Phase 5: fetchBlockMethod handler and server registration ✅

- Created `src/Cardano/Rpc/Server/Internal/UtxoRpc/Sync.hs` with `fetchBlockMethod`
  - Validates hash is non-empty (`INVALID_ARGUMENT`)
  - Calls `withNodeKernelAccess` then `nkaFetchBlock`
  - `Just (rawBytes, blockNo)` -> populates `AnyChainBlock` with `nativeBytes` and `cardano.header` (slot, hash, height)
  - `Nothing` -> throws `NOT_FOUND` with slot in message
- `Server.hs`: added `methodsSyncRpc` with stubs for unimplemented methods (dumpHistory, followTip, readTip), registered alongside existing method tables

## Phase 6: mkNodeKernelAccess in cardano-rpc ✅

Lives in cardano-rpc (not cardano-node) - only depends on consensus types re-exported via `Cardano.Api.Consensus`.

- Module structure per ADR-009:
  - `Cardano.Rpc.Server.NodeKernelAccess` - datatypes only (`NodeKernelAccessF`, `LedgerSnapshot`)
  - `Cardano.Rpc.Server.NodeKernelAccess.Internal` - functions (`mkNodeKernelAccess`, `withNodeKernelAccess`, `fetchBlock`)
- `mkNodeKernelAccess` takes `BlockType blk` + `NodeKernel IO addrNTN addrNTC blk`, polymorphic in block type
- `fetchBlock` uses GADT pattern match on `BlockType` via `withBlockTypeConstraints` to bring `HeaderHash blk ~ OneEraHash xs` into scope
- Returns `Maybe (ByteString, BlockNo)` - raw CBOR from `GetRawBlock` and block number from `GetBlock`
- `nkaWithSnapshot` and `nkaSubmitTx` are stubbed (NKA query/submit migration pending)
- `Run.hs`: `case blockType of CardanoBlockType -> writeIORef ref (Just (mkNodeKernelAccess blockType nodeKernel))`
- Re-exported consensus types in `Cardano.Api.Consensus`: `NodeKernel`, `ChainDB`, `BlockComponent`, `RealPoint`, `OneEraHash`, `HeaderHash`, `HasHeader`, `blockNo`, `StandardCrypto`, `CardanoBlock`

## Phase 7: E2E test ✅

- Created `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/FetchBlock.hs`
  - `hprop_rpc_fetch_block`: starts testnet with `RpcEnabled`, queries tip via CLI, calls FetchBlock via gRPC
  - Asserts: one block returned, non-empty `nativeBytes`, `cardano.header.slot` matches tip, `cardano.header.hash` matches tip, `cardano.header.height` matches tip block number
- Registered in `cardano-testnet-test.hs` test runner

## Remaining work

### Response fields not yet populated

| Field | Status | Blocker |
|-------|--------|---------|
| `AnyChainBlock.nativeBytes` | ✅ Set | - |
| `Block.header.slot` | ✅ Set | - |
| `Block.header.hash` | ✅ Set | - |
| `Block.header.height` | ✅ Set | - |
| `Block.timestamp` | ⬜ Not set | Needs `EraHistory` from ledger state (snapshot/query migration) |
| `Block.body.tx` | ⬜ Not set | Requires full block deserialisation + UTxO RPC tx mapping |

### Stubbed callbacks

| Callback | Blocker |
|----------|---------|
| `nkaWithSnapshot` | Separate story: query/snapshot migration |
| `nkaSubmitTx` | Separate story: submit migration |
