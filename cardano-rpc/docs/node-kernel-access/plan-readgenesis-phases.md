# ReadGenesis implementation phases

Implements the ReadGenesis QueryService method.
Each phase compiles independently and can be reviewed as a separate commit.

Genesis data is static - it never changes after node startup.
All data is extracted once in `mkNodeKernelAccess` and stored as a pure field on `NodeKernelAccess`.
It all comes from one source, and it is not a disk read:

- **`ProtocolInfoArgs`** (threaded from the boot-time `SomeConsensusProtocol`): the full, uncompacted `TransitionConfig LatestKnownEra` - Shelley, Alonzo, Conway and Dijkstra genesis with `sgInitialFunds` and `sgStaking` intact - plus the Shelley genesis hash computed at boot, plus the Byron `Cardano.Chain.Genesis.Config` (genesis data and hash), read directly off `CardanoProtocolParams`'s `byronProtocolParams` field with no hard-fork navigation needed.

## Related documents

- [story-read-genesis.md](../story-read-genesis.md) - the user story: acceptance criteria, out-of-scope list, open questions.
- [README.md](README.md) - node-kernel-access overview; capability table mapping `TopLevelConfig` to ReadGenesis.
- [analysis-architecture.md](analysis-architecture.md) - why ReadGenesis benefits from direct node access rather than N2C IPC.
- [03-mk-node-access-and-wiring.md](03-mk-node-access-and-wiring.md) - how `NodeKernelAccess` is populated from the node kernel hook.
  Partially stale: the implemented `mkNodeKernelAccess` lives in cardano-rpc's `NodeKernelAccess.hs`; it derives the `TopLevelConfig` from the passed-in `NodeKernel` via `Consensus.getTopLevelConfig` rather than taking it as a separate parameter.
- ADR-019 "Node kernel access for cardano-rpc" (cardano-node-wiki, `docs/ADR-019-node-kernel-access-for-cardano-rpc.md`) - the architecture decision this work builds on; its capability table backs ReadGenesis with `TopLevelConfig`.
- [IntersectMBO/cardano-api#1217](https://github.com/IntersectMBO/cardano-api/issues/1217) - the GitHub issue tracking this method.
  The issue's proposed data source (`queryLedgerConfig` over N2C IPC) predates node kernel access; this plan supersedes it.
- [UTxO RPC query spec](https://utxorpc.org/query/spec/) - `ReadGenesisRequest`/`ReadGenesisResponse` shape.
- [CIP-34](https://cips.cardano.org/cip/CIP-34) - Cardano's CAIP-2 chain identifier.

## Phase 1: Proto service method + stub handler + registration + tracing

Atomic: proto change requires matching handler registration.

- Add `rpc ReadGenesis(ReadGenesisRequest) returns (ReadGenesisResponse)` to the `QueryService` block in `proto/utxorpc/v1beta/query/query.proto`
- Regenerate: `nix develop --command bash -c "cd cardano-rpc && buf generate proto"`
- Add `TraceRpcQueryReadGenesisSpan TraceSpanEvent` constructor to `TraceRpcQuery` in `Tracing.hs`
- Add stub `readGenesisMethod` in `Query.hs` returning `GrpcUnimplemented`
- Register in `methodsUtxoRpc` in `Server.hs` - position must match `ServiceMethods` order (verify against generated code)
- Update cardano-node `Tracers/Rpc.hs` with new trace constructor

Build: `cabal build cardano-rpc && cabal build cardano-node`

## Phase 2: Genesis threading + NKA genesis bundle + envelope fields

Adds the static genesis data to `NodeKernelAccessF` and populates the response envelope.

Node-side wiring (cardano-node):

- Genesis file paths still live in `NodeConfiguration` (`ncProtocolConfig` holds `npcByronGenesisFile`, `npcShelleyGenesisFile`, `npcAlonzoGenesisFile`, `npcConwayGenesisFile`, `Cardano.Node.Types`, `GenesisFile` newtypes over `FilePath`), but cardano-rpc no longer sees any of them directly.
  The Shelley genesis is still read once, at boot, by `mkSomeConsensusProtocolCardano` (`Cardano.Node.Protocol.Cardano`) via `Shelley.readGenesis npcShelleyGenesisFile npcShelleyGenesisFileHash` - that call was always there for consensus's own needs, and cardano-rpc now reuses its result instead of reading the file again.
- `mkConsensusProtocol` `coerce`s the hash it already has (node's `GenesisHash` and cardano-api's `GenesisHashShelley` are both newtypes over the same Blake2b-256 hash) into a `GenesisHashShelley`, and returns it alongside `SomeConsensusProtocol` as a tuple, rather than as a field on `SomeConsensusProtocol` itself.
  The exported `SomeConsensusProtocol` constructor keeps its upstream (`ouroboros-consensus`) shape unchanged; it carries no extra genesis-hash field.
- `nc :: NodeConfiguration` is in scope at both wiring sites in `handleSimpleNode` (`cardano-node/src/Cardano/Node/Run.hs`): the `rpcServerLoop` launch and the `rnNodeKernelHook` lambda that calls `mkNodeKernelAccess`.
- Pass the `GenesisHashShelley` returned alongside `SomeConsensusProtocol` and the `ProtocolInfoArgs` already in scope into `mkNodeKernelAccess`, instead of a `FilePath`.
  `ProtocolInfoArgs`'s Cardano-block instance (`ProtocolInfoArgsCardano`, `Cardano.Api.Consensus.Internal.Protocol`) wraps `CardanoProtocolParams`, whose `cardanoLedgerTransitionConfig :: TransitionConfig LatestKnownEra` field is the already-parsed Shelley-onwards transition config; `cardanoLedgerTransitionConfig` is re-exported from `Cardano.Api.Consensus`.
- The node kernel access warnings are RPC traces, not startup traces.
  `StartupTrace` (`Cardano.Node.Startup`) therefore loses its `RpcUnsupportedBlockType` constructor, along with every clause for it in `Cardano.Node.Tracing.Tracers.Startup` (`forMachine`, `namespaceFor`, `severityFor`, `allNamespaces`, `ppStartupInfoTrace`).
- Handle the new `TraceRpcNodeKernelAccess` constructor in `Cardano.Node.Tracing.Tracers.Rpc` instead, in the hand-maintained clause lists that need it: `forMachine`, `namespaceFor`, `severityFor`, `documentFor` and `allNamespaces`.
  The only namespace is `RPC.NodeKernelAccess.UnsupportedBlockType`, at severity `Warning`; there is no `ShelleyGenesisUnavailable` namespace, because there is no disk read left to fail.
  It is not a metric, so `asMetrics` and `metricsDocFor` keep their catch-all defaults.
- Run.hs then passes `rpcTracer tracers` straight into `mkNodeKernelAccess` - it is already a `Tracer IO TraceRpc`, so no contramap is needed.

cardano-rpc side:

- Add `GenesisBundle` record to `NodeKernelAccess/Type.hs` (the module has `NoFieldSelectors`, so fields are unprefixed and read by punning).
  It reuses established types rather than exploding them into primitives:
  - `byronConfig :: !Byron.Config` - `Cardano.Chain.Genesis.Config` already bundles the genesis data with the hash the Byron ledger computed when it parsed the file, so it is kept whole.
  - `shelleyGenesisHash :: !GenesisHashShelley` - the boot-time hash returned alongside `SomeConsensusProtocol` from `mkConsensusProtocol`.
    `GenesisBundle` is only ever constructed for the Cardano protocol, so the hash is always available here; there is no missing-hash case to represent.
  - `transitionConfig :: !(TransitionConfig LatestKnownEra)` - the Shelley-onwards genesis configuration, the same representation `Cardano.Api.LedgerState.GenesisConfig` uses.
- Add pure field `genesisConfig :: GenesisBundle` to `NodeKernelAccess`
- `mkNodeKernelAccess` takes the plain `GenesisHashShelley` and the `ProtocolInfoArgs blk` instead of a `FilePath`, and passes them straight to `readGenesisBundle` (which needs nothing else - not even the `TopLevelConfig`, which `mkNodeKernelAccess` derives from the passed-in `NodeKernel` via `Consensus.getTopLevelConfig` for system start, era history and the security parameter, rather than taking it as its own parameter).
- `readGenesisBundle` is a pure function - no disk read, no `IO`, no `ExceptT`, and no hard-fork navigation.
  Pattern-matching `ProtocolInfoArgsCardano` on its parameter unwraps `CardanoProtocolParams` directly; its `byronProtocolParams` field is a `ProtocolParamsByron` carrying the Byron `Cardano.Chain.Genesis.Config` (`byronGenesis`) as-is, so there is no `HardForkLedgerConfig` walk needed to reach it.
  The Byron `LedgerConfig` is the same `Cardano.Chain.Genesis.Config` that `configBlock` carries, so the block config needs no separate walk.
- The `transitionConfig` field is no longer assembled from per-era pieces.
  It comes straight from `Consensus.cardanoLedgerTransitionConfig` applied to the `CardanoProtocolParams` unwrapped out of the passed-in `ProtocolInfoArgsCardano` - the same value cardano-node parsed and built at boot to construct `TopLevelConfig` in the first place.
  This removes the `shelleyLedgerTranslationContext` extraction for Alonzo/Conway/Dijkstra and the `Ledger.mkLatestTransitionConfig` reconstruction that this plan originally called for; Phases 3, 5 and 6 still project the per-era genesis back out the same way (`tcShelleyGenesisL` reaches the Shelley genesis from any era, Alonzo and Conway need `tcPreviousEraConfigL` chained to that era followed by `tcTranslationContextL`).
- The disk read is gone entirely: no `readShelleyGenesis` call, no `ShelleyConfig`/`ShelleyGenesisError` handling, no compacted-genesis fallback, and no `TraceRpcShelleyGenesisUnavailable` trace.
  The hash is always available at boot for the Cardano protocol (it is computed once, by consensus's own `Shelley.readGenesis` call, and threaded through rather than recomputed), so there is nothing left that can fail here.
- `Cardano.Api.LedgerState`'s export list does not need `readShelleyGenesis`, `ShelleyGenesisError (..)` or `renderShelleyGenesisError` added.
  `ShelleyConfig (..)` and `GenesisHashShelley (..)` were already exported, and only the latter is still needed.
- The tracer argument of `mkNodeKernelAccess` is `Tracer m TraceRpc` instead of `Tracer m Text`, and the one remaining warning lives in a `TraceRpcNodeKernelAccess` sub-sum in `Cardano.Rpc.Server.Internal.Tracing` alongside `TraceRpcQuery`/`TraceRpcSubmit`/`TraceRpcSync`, re-exported from `Cardano.Rpc.Server`.
- Replace stub handler: populate `genesis` (the Shelley genesis hash bytes) and `caip2` (see "Resolved" below).
  The network magic comes from `transitionConfig ^. tcShelleyGenesisL` at the call site, so `GenesisBundle` does not carry it separately.
- `cardano` oneof remains empty (valid proto default)
- Add consensus re-exports to `Cardano.Api.Consensus` as needed.
  `TopLevelConfig`, `configBlock`, `configLedger`, `ProtocolInfoArgs (..)` and `cardanoLedgerTransitionConfig` are already there; reaching the Byron genesis needs only `byronProtocolParams` and `byronGenesis`, both minimal accessor re-exports - `Cardano.Api.Consensus.Internal.Protocol` already has them unqualified in scope via its existing `Ouroboros.Consensus.Cardano`/`Ouroboros.Consensus.Cardano.Node` imports, so this is an export-list addition only, no new import.
  No sop-core dependency is needed at all: with no hard-fork walk, cardano-rpc has no use for `NP`, so there is no `strict-sop-core` dependency, no `Data.SOP.Strict (NP (..))` import, and no `hiding ((:*))` needed on the `Network.GRPC.Spec` import (nothing left to collide with).

Build: `cabal build cardano-rpc && cabal build cardano-node`

Alternative considered, and now adopted: capture the Shelley genesis hash and transition config during protocol setup and thread the values instead of a path.
This was originally rejected as changing protocol-setup plumbing shared by many call sites, while path threading looked local to the RPC wiring; the final design threads them anyway, alongside `SomeConsensusProtocol` and via `ProtocolInfoArgs`, because it removes an entire read-and-fallback surface from cardano-rpc rather than merely relocating a file path.

### Resolved

Both open questions were settled by comparing against Dolos, the reference UTxO RPC implementation.

- **Which genesis hash?** The Shelley one: the Blake2b-256 hash of the raw Shelley genesis JSON file bytes, computed once by `Cardano.Node.Protocol.Shelley.readGenesis` at node startup and threaded into cardano-rpc as a plain `GenesisHashShelley` returned alongside `SomeConsensusProtocol` from `mkConsensusProtocol`, rather than recomputed from a second read.
  Not the Byron hash, even though Byron's is the CIP-34 chain identifier.
- **CAIP-2 format.** Dolos does not derive it from CIP-34; it keys the identifier on the Shelley network magic.
  764824073 gives `cardano:mainnet`, 1 gives `cardano:preprod`, 2 gives `cardano:preview`, and any other magic gives `cardano:<magic in decimal>`.
  cardano-rpc follows the same scheme so that clients see identical chain identifiers from either server.

## Phases 3-6: per-era `cardano` field mapping (as built)

Phases 3-6 are implemented together in one new module,
`Cardano.Rpc.Server.Internal.UtxoRpc.Type.Genesis`, re-exported from the
`Cardano.Rpc.Server.Internal.UtxoRpc.Type` umbrella and wired into
`readGenesisMethod` via `& U5c.cardano .~ genesisBundleToProto genesisBundle`
(setting the `cardano` lens auto-wraps into the oneof).

### Composition design

The proto `Genesis` message has a single `cost_models` field fed by two eras
(PlutusV1 from Alonzo, PlutusV3 from Conway), and proto-lens has no field-merge.
Each era is therefore a pure updater over one shared accumulator -
`byronGenesisToProto`, `shelleyGenesisToProto`, `alonzoGenesisToProto`,
`conwayGenesisToProto`, each `X -> Proto U5c.Genesis -> Proto U5c.Genesis`.
`genesisBundleToProto` threads a single `defMessage` through all four, so Alonzo
sets `costModels . maybe'plutusV1` and Conway sets `costModels . maybe'plutusV3`
on the same message and both survive.
The per-era genesis values are projected out of the `GenesisBundle` with no
hard-fork navigation: Byron via `configGenesisData byronConfig`, Shelley via
`transitionConfig ^. tcShelleyGenesisL`, Conway via
`tcPreviousEraConfigL . tcTranslationContextL`, and Alonzo via three
`tcPreviousEraConfigL` hops (Dijkstra to Conway to Babbage to Alonzo) followed by
`tcTranslationContextL`.

### Phase 3: Shelley genesis (proto fields 10-23)

`shelleyGenesisToProto` maps `sgActiveSlotsCoeff`, `sgEpochLength`, `sgGenDelegs`,
`sgInitialFunds`, `sgMaxKESEvolutions`, `sgMaxLovelaceSupply`, `sgNetworkId`,
`sgNetworkMagic`, `sgProtocolParams`, `sgSecurityParam`, `sgSlotLength`,
`sgSlotsPerKESPeriod`, `sgSystemStart` and `sgUpdateQuorum`.
`sgNetworkId` renders as the text `Mainnet`/`Testnet`; `sgSystemStart` as
ISO-8601 via `iso8601Show`; `sgSlotLength` as milliseconds
(`round (1000 * fromNominalDiffTimeMicro sgSlotLength)`, so mainnet's 1 second is
1000); `sgGenDelegs` and `sgInitialFunds` keys as lowercase base16 of the key
hash and of `serialiseAddr` respectively.
`sgInitialFunds` (field 13) is populated because the Shelley genesis is projected
from the uncompacted boot-time transition config - the compacted in-memory copy
would yield an empty map; on mainnet and the published testnets it is genuinely
empty, custom networks (e.g. cardano-testnet) populate it.
The proto `PParams` sub-message is built by hand from the era-generic
`EraPParams` lenses on `PParams ShelleyEra` (`ppTxFeePerByteL`, `ppTxFeeFixedL`,
`ppMaxBBSizeL`, `ppMaxTxSizeL`, `ppMaxBHSizeL`, `ppKeyDepositL`, `ppPoolDepositL`,
`ppEMaxL`, `ppNOptL`, `ppA0L`, `ppRhoL`, `ppTauL`, `ppMinPoolCostL`,
`ppProtocolVersionL`), because `protocolParamsToUtxoRpcPParams` requires
`ConwayEraPParams`.
`sgStaking` has no proto counterpart, so it is not mapped.

### Phase 4: Byron genesis (proto fields 1-9)

`byronGenesisToProto` maps `gdAvvmDistr`, `gdProtocolParameters` (to
`protocolConsts` and `blockVersionData`), `gdStartTime`, `gdGenesisKeyHashes`
(proto `bootStakeholders`), `gdHeavyDelegation` and `gdNonAvvmBalances`.
`gdStartTime` is Unix seconds (`round . utcTimeToPOSIXSeconds`).
All hashes and keys reuse the Byron canonical-JSON formatters so the wire values
match the on-disk genesis: key hashes via `sformat hashHexF . unKeyHash`,
delegation certificate signatures via `fullSignatureHexF`, issuer/delegate
verification keys via `fullVerificationKeyF` (standard base64), non-AVVM
addresses via `addressF` (base58), and AVVM redeem keys via
`redeemVKB64UrlF . fromCompactRedeemVerificationKey` (base64url).
Byron stores only a set of genesis key hashes, so each `bootStakeholders` weight
is synthesised as 1, matching the genesis JSON.
`blockVersionData`'s size and threshold fields are stringified numbers;
LovelacePortion values render as their raw Word64 numerator (recovered via
`lovelacePortionToRational` because `unLovelacePortion` is not exported), and the
`txFeePolicy` summand/multiplier reproduce the Byron JSON's 1e9 scaling.

### Phase 5: Alonzo genesis (proto fields 24-31)

`alonzoGenesisToProto` maps `agCoinsPerUTxOWord`, `agPrices` (proto
`executionPrices`), `agMaxTxExUnits`, `agMaxBlockExUnits`, `agMaxValSize`,
`agCollateralPercentage`, `agMaxCollateralInputs` and the PlutusV1 cost model from
`agPlutusV1CostModel` (set on `costModels . maybe'plutusV1`).
`agExtraConfig` is not read; PlutusV2 and PlutusV4 are never in genesis.

### Phase 6: Conway genesis (proto fields 32-42)

`conwayGenesisToProto` maps `cgUpgradePParams` (committee min size and max term
length, gov action lifetime and deposit, DRep deposit and activity, min fee ref
script cost per byte, and the pool/DRep voting thresholds mapped to their named
proto fields), `cgConstitution` (anchor plus guardrails script hash) and
`cgCommittee` (threshold plus members), and the PlutusV3 cost model from
`ucppPlutusV3CostModel` (set on `costModels . maybe'plutusV3`, composing with
Alonzo's PlutusV1 on the shared accumulator).

Build: `cabal build cardano-rpc`

## Phase 7: E2E test

- Create `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/Genesis.hs`
  - `hprop_rpc_read_genesis`: start testnet with `RpcEnabled`, call ReadGenesis via gRPC
  - Assert: `genesis` is 32 bytes, `caip2` is non-empty, `cardano` is set
  - Assert Shelley fields: `epochLength > 0`, `networkMagic` matches testnet magic, `systemStart` non-empty
  - Assert `initialFunds` is non-empty: cardano-testnet funds its wallets there, so this proves end to end that the uncompacted boot-time genesis reached the RPC response (the compacted in-memory genesis would yield an empty map)
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
  All genesis types are reachable from cardano-api or from ledger packages cardano-rpc already depends on, so `GenesisBundle` needs no consensus import of its own.
  In practice the fields are typed with the ledger types directly (`Cardano.Ledger.Shelley.Genesis`, `Cardano.Ledger.Alonzo.Genesis`, `Cardano.Ledger.Conway.Genesis`, `Cardano.Chain.Genesis`), matching how the rest of cardano-rpc imports ledger types; `Cardano.Api.Genesis` names Alonzo and Conway genesis only through the `AlonzoGenesisConfig`/`ConwayGenesisConfig` aliases.
- **Reaching the Byron genesis needs no hard-fork navigation**, but the call to `readGenesisBundle` still requires `blk ~ CardanoBlock StandardCrypto`, because its signature is monomorphic in `Consensus.ProtocolInfoArgs (CardanoBlock StandardCrypto)`.
  Matching the `CardanoBlockType` constructor of cardano-api's `BlockType` GADT (as `mkNodeKernelAccess` already does) brings that equality into scope; there is no `withBlockTypeConstraints` helper, and none is needed.
- **Why the disk read is no longer needed.**
  `TopLevelConfig` only retains a compacted per-era `ShelleyLedgerConfig` (`Ouroboros.Consensus.Shelley.Ledger.Config`'s `CompactGenesis`): `compactGenesis` sets `sgInitialFunds = mempty` and `sgStaking = emptyGenesisStaking`, which is why this plan originally called for a second file read to recover them.
  `ProtocolInfoArgs`, threaded in from `mkNodeKernelAccess`'s caller, is the value cardano-node built before that compaction happens: `cardanoLedgerTransitionConfig` on the `CardanoProtocolParams` it carries is the full, uncompacted `TransitionConfig LatestKnownEra`, with `sgInitialFunds` and `sgStaking` intact.
  Reading the transition config off `ProtocolInfoArgs` instead of `TopLevelConfig` therefore sidesteps the compaction entirely, so the second disk read (and its fallback) can be deleted rather than merely deferred.
- **Mainnet data shape.**
  `mainnet-shelley-genesis.json` has `"initialFunds": {}` and no `"staking"` key at all; the testnet template is the same.
  An empty `initial_funds` in a mainnet response is therefore correct data, not a bug.

## Known limitations

- `sgStaking` is available via the transition config threaded from `ProtocolInfoArgs`, but the proto `Genesis` message has no staking field to carry it (spec gap; raise upstream if a client needs it).
- The proto `Genesis` message has no Dijkstra-era fields (it stops at Conway, field 42); `npcDijkstraGenesisFile` is not read.
- Alonzo/Conway genesis hashes are still not derivable without also reading those files; not needed while the `genesis` field carries the Shelley hash.
- The Byron `ftsSeed` (field 3) and `vssCerts` (field 9) proto fields stay at their default: `GenesisData` at the pinned cardano-ledger-byron carries no such data.
- The `protocolConsts` `vssMaxTtl`/`vssMinTtl` stay at 0: there is no Byron ledger source for them.
- Byron `bootStakeholders` weights are all 1: the ledger stores only a set of genesis key hashes, so the per-stakeholder weight is synthesised.
- No PlutusV2 (or PlutusV4) cost model is emitted: neither appears in any era's genesis.
- The Shelley `sgProtocolParams` fields `extraEntropy`, `d` (decentralisation) and `minUTxOValue` are dropped: the proto `PParams` message has no counterpart for them.
- A proto committee member key does not distinguish a key hash from a script hash: both `KeyHashObj` and `ScriptHashObj` credentials render as bare hex.
- The Byron AVVM key encoding reuses the ledger's own `redeemVKB64UrlF` formatter (padded base64url) so it matches the on-disk genesis and Dolos; this was verified against the ledger's canonical-JSON `ToObjectKey` instance rather than by round-tripping a live genesis.
