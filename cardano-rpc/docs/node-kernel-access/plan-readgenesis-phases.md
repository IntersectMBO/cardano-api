# ReadGenesis implementation phases

Implements the ReadGenesis QueryService method.
Each phase compiles independently and can be reviewed as a separate commit.

Genesis data is static - it never changes after node startup.
All data is extracted once from `TopLevelConfig` in `mkNodeKernelAccess` and stored as a pure field on `NodeKernelAccessF`.

## Phase 1: Proto service method + stub handler + registration + tracing

Atomic: proto change requires matching handler registration.

- Add `rpc ReadGenesis(ReadGenesisRequest) returns (ReadGenesisResponse)` to the `QueryService` block in `proto/utxorpc/v1beta/query/query.proto`
- Regenerate: `nix develop --command bash -c "cd cardano-rpc && buf generate proto"`
- Add `TraceRpcQueryReadGenesisSpan TraceSpanEvent` constructor to `TraceRpcQuery` in `Tracing.hs`
- Add stub `readGenesisMethod` in `Query.hs` returning `GrpcUnimplemented`
- Register in `methodsUtxoRpc` in `Server.hs` - position must match `ServiceMethods` order (verify against generated code)
- Update cardano-node `Tracers/Rpc.hs` with new trace constructor

Build: `cabal build cardano-rpc && cabal build cardano-node`

## Phase 2: NKA genesis bundle + envelope fields

Adds the static genesis data to `NodeKernelAccessF` and populates the response envelope.

- Add `GenesisBundle` record to `NodeKernelAccess.hs`:
  - `gbGenesisHashBytes :: !ByteString` (Byron genesis hash - the CIP-34 chain identifier)
  - `gbNetworkMagic :: !Word32`
  - `gbShelleyGenesis :: !ShelleyGenesis`
  - `gbByronGenesisData :: !GenesisData` (Byron)
  - `gbAlonzoGenesis :: !AlonzoGenesis`
  - `gbConwayGenesis :: !ConwayGenesis`
- Add pure field `nkaGenesisConfig :: GenesisBundle` to `NodeKernelAccessF`
- In `mkNodeKernelAccess`, extract from `getTopLevelConfig nodeKernel`:
  - Byron hash: `configBlock` -> `byronGenesisHash` -> `abstractHashToBytes`
  - Byron data: `configBlock` -> `byronGenesisConfig` -> `configGenesisData`
  - Shelley: `configLedger` -> Shelley partial config -> `shelleyLedgerGenesis`
  - Alonzo: `configLedger` -> Alonzo partial config -> `shelleyLedgerTranslationContext`
  - Conway: `configLedger` -> Conway partial config -> `shelleyLedgerTranslationContext`
  - Network magic: `shelleyLedgerGenesis` -> `sgNetworkMagic`
- Replace stub handler: populate `genesis` (hash bytes) and `caip2` (CIP-34 ID)
- `cardano` oneof remains empty (valid proto default)
- Add consensus re-exports to `Cardano.Api.Consensus` as needed (`TopLevelConfig`, `configBlock`, `configLedger`, etc.)

Build: `cabal build cardano-rpc && cabal build cardano-node`

### Blockers (must resolve before implementing Phase 2)

- **Which genesis hash?** Proto says "genesis hash for the chain" (singular).
  Byron genesis hash is the CIP-34 chain identifier and is available from `TopLevelConfig`.
  Shelley/Alonzo/Conway hashes are NOT recoverable from `TopLevelConfig` (CompactGenesis is lossy) - they live in `NodeConfiguration` which `mkNodeKernelAccess` does not receive.
  If the spec requires the Shelley hash, `mkNodeKernelAccess` needs an additional parameter, changing its signature.
  **Action:** run `/utxorpc-compare` to check what Dolos/other implementations return.
- **CAIP-2 format**: exact derivation per CIP-34 (genesis hash prefix + network magic?).
  **Action:** read CIP-34 and cross-reference with Dolos output.

## Phase 3: Shelley genesis fields mapping (proto fields 10-23)

Independent of phases 4-6.

- Add `shelleyGenesisToProto :: ShelleyGenesis -> U5c.Genesis` (or lens-based builder) in a new mapping module or `Type.hs`
- Map: `sgActiveSlotsCoeff`, `sgEpochLength`, `sgGenDelegs`, `sgMaxKESEvolutions`, `sgMaxLovelaceSupply`, `sgNetworkId`, `sgNetworkMagic`, `sgProtocolParams`, `sgSecurityParam`, `sgSlotLength`, `sgSlotsPerKESPeriod`, `sgSystemStart`, `sgUpdateQuorum`
- Note: `sgInitialFunds` will be empty (`CompactGenesis` erases it - document with comment)
- Wire into `readGenesisMethod`

Build: `cabal build cardano-rpc`

## Phase 4: Byron genesis fields mapping (proto fields 1-9)

Independent of phases 3, 5, 6.

- Add `byronGenesisToProto :: GenesisData -> U5c.Genesis` mapping
- Map: `gdAvvmDistr`, `gdProtocolParameters` (-> `protocolConsts` + `blockVersionData`), `gdStartTime`, `gdBootStakeholders`, `gdHeavyDelegation`, `gdNonAvvmBalances`, `gdVssCerts`, `gdFtsSeed`
- Wire into `readGenesisMethod`

Build: `cabal build cardano-rpc`

## Phase 5: Alonzo genesis fields mapping (proto fields 24-31)

Independent of phases 3, 4, 6.

- Add `alonzoGenesisToProto :: AlonzoGenesis -> U5c.Genesis` mapping
- Map: `agCoinsPerUTxOWord`, `agPrices`, `agMaxTxExUnits`, `agMaxBlockExUnits`, `agMaxValSize`, `agCollateralPercentage`, `agMaxCollateralInputs`, cost models (PlutusV1 from `agPlutusV1CostModel`, V2/V3 from extra config if available)

Build: `cabal build cardano-rpc`

## Phase 6 (optional): Conway genesis fields mapping (proto fields 32-42)

Independent of phases 3-5. May be deferred depending on priority.

- Add `conwayGenesisToProto :: ConwayGenesis -> U5c.Genesis` mapping
- Map: `cgUpgradePParams` (pool/DRep voting thresholds, committee min size, committee max term length, gov action lifetime/deposit, DRep deposit/activity, min fee ref script cost per byte), `cgConstitution`, `cgCommittee`

Build: `cabal build cardano-rpc`

## Phase 7: E2E test

- Create `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/Genesis.hs`
  - `hprop_rpc_read_genesis`: start testnet with `RpcEnabled`, call ReadGenesis via gRPC
  - Assert: `genesis` is 32 bytes, `caip2` is non-empty, `cardano` is set
  - Assert Shelley fields: `epochLength > 0`, `networkMagic` matches testnet magic, `systemStart` non-empty
  - Assert Byron fields: `protocolConsts` present, `startTime > 0`
  - Assert Alonzo fields: `executionPrices` present, `maxTxExUnits` non-zero
- Register in `cardano-testnet-test.hs` test runner

Build: `TASTY_PATTERN='/RPC ReadGenesis/' cabal test cardano-testnet-test`

## Phase dependency graph

```
Phase 1 (proto + stub)
  |
  v
Phase 2 (NKA bundle + envelope)
  |
  +---> Phase 3 (Shelley) --+
  |                          |
  +---> Phase 4 (Byron)   --+--> Phase 7 (E2E test)
  |                          |
  +---> Phase 5 (Alonzo)  --+
  |                          |
  +---> Phase 6 (Conway)  --+  (optional)
```

Phases 3-6 are independent of each other.
Phase 7 depends on at least phases 3-5 for meaningful assertions.

## Design notes

- **No cardano-api boundary violation.**
  All genesis types (`ShelleyGenesis`, `AlonzoGenesis`, `ConwayGenesis`, `ByronGenesisConfig`) are already re-exported from `Cardano.Api.Genesis`.
  `GenesisBundle` can live in `NodeKernelAccess.hs` without importing consensus directly.
- **Navigating `HardForkLedgerConfig`** to extract per-era configs requires `blk ~ CardanoBlock StandardCrypto`.
  The existing `withBlockTypeConstraints`/`CardanoBlockType` GADT pattern (from FetchBlock) already solves this.

## Known limitations

- `sgInitialFunds` and `sgStaking` are erased by `CompactGenesis` at runtime.
  The corresponding proto fields will be empty.
- Genesis hashes for Shelley/Alonzo/Conway cannot be recomputed from `CompactGenesis` (lossy compaction).
  Only the Byron genesis hash is available from `TopLevelConfig`.
