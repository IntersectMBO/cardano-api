# Implement ReadGenesis UTxO RPC method

## Problem

The UTxO RPC spec defines a `ReadGenesis` method on `QueryService` that returns genesis configuration data (genesis hash, CAIP-2 network identifier, and structured genesis parameters across all eras).
cardano-rpc's local `query.proto` already has the `ReadGenesisRequest` and `ReadGenesisResponse` message types, but the `rpc ReadGenesis` line is missing from the `QueryService` block and no handler exists.

## Why

Genesis configuration is static, well-defined data that tooling and wallets need to discover network parameters at startup.
Without `ReadGenesis`, clients must parse genesis JSON files out-of-band, duplicating logic that the node already has.

## User value

As a UTxO RPC client developer, I want to call `ReadGenesis` on the query service so that I can programmatically obtain the network's genesis configuration (hash, CAIP-2 ID, and era-specific parameters) without parsing raw genesis files.

## Acceptance criteria

1. **AC1: Proto service registration** - The `rpc ReadGenesis(ReadGenesisRequest) returns (ReadGenesisResponse)` line is added to the `QueryService` block in `proto/utxorpc/v1beta/query/query.proto`.
   Generated code is regenerated via `buf generate` in the nix devshell.
   The handler is registered in `methodsUtxoRpc` in `Server.hs` in the correct positional slot matching `ServiceMethods` order.
   - Test: unit - compiles; the new method slot is exercised by the gRPC method table construction

2. **AC2: NodeKernelAccessF genesis field** - `NodeKernelAccessF` gains a new pure field (e.g. `nkaGenesisConfig :: GenesisBundle`) that holds the genesis configuration data as Haskell-native types (not proto types).
   Being a pure (non-`m`) field reflects the fact that genesis data is static and never changes after node startup.
   `GenesisBundle` carries the Byron genesis hash, network magic, and per-era genesis configs (Shelley, Alonzo, Conway) needed to populate the response.
   `mkNodeKernelAccess` extracts this data from `TopLevelConfig` via `getTopLevelConfig`.
   - Test: unit - compiles; the new field is present on the record

3. **AC3: Genesis-to-proto conversion** - A pure conversion function (e.g. `genesisToUtxoRpcGenesis`) in the `Type` module converts the Haskell genesis config values into the proto-lens `Genesis` message.
   The function maps fields from all four eras: Byron (avvm_distr, block_version_data, start_time, protocol_consts, etc.), Shelley (active_slots_coeff, epoch_length, network_magic, protocol_params, system_start, etc.), Alonzo (lovelace_per_utxo_word, execution_prices, max_tx_ex_units, etc.), and Conway (committee, constitution, drep fields, voting thresholds, etc.).
   - Test: unit - `H.propertyOnce` golden test: feed known testnet genesis fixture values, assert specific proto fields are populated correctly (e.g. network_magic, system_start, epoch_length, active_slots_coeff).
   This is the primary unit-testable surface for the feature.

4. **AC4: Handler assembly** - A `readGenesisMethod` handler in `Cardano.Rpc.Server.Internal.UtxoRpc.Query` (or a new module if warranted) grabs the `NodeKernelAccessF` via `grabNodeKernelAccess`, calls the genesis callback, runs the conversion function, and assembles the `ReadGenesisResponse`.
   The response populates all three top-level fields: `genesis` (hash bytes), `caip2` (CIP-34 network identifier), and `cardano` (structured `Genesis` message).
   - Test: E2E - calling `ReadGenesis` on a running node returns a response with all three top-level fields populated and non-empty

5. **AC5: Tracing** - `TraceRpcQuery` gains a `TraceRpcQueryGenesisSpan TraceSpanEvent` constructor.
   `Pretty` renders it as "Started ReadGenesis method" / "Finished ReadGenesis method".
   The handler is wrapped in `wrapInSpan TraceRpcQueryGenesisSpan`.
   - Test: unit - `H.propertyOnce` asserting the `Pretty` output of the new constructor contains the expected substrings

6. **AC6: Node kernel unavailable** - When the node kernel is not yet initialised (`IORef` contains `Nothing`), calling `ReadGenesis` returns gRPC `UNAVAILABLE` with a message containing "not yet initialised".
   This is consistent with the existing `FetchBlock` behaviour via `grabNodeKernelAccess`.
   - Test: unit - `H.propertyOnce`: create `IORef Nothing`, call through the handler path, assert `GrpcException` with `GrpcUnavailable` is thrown (same pattern as the `NodeKernelAccess` unavailability unit test in piece 1)

## Out of scope

- **Field mask support**: `field_mask` in `ReadGenesisRequest` is ignored, consistent with `ReadParams`, `SearchUtxos`, and all other existing handlers.
  Field mask normalisation and filtering is a cross-cutting concern for a separate story.
- **Byron genesis file fields requiring file I/O**: some Byron genesis fields (e.g. `vss_certs`, `heavy_delegation` maps with complex nested structures) may be deferred to a follow-up if the ledger types do not expose them directly through `TopLevelConfig`.
- **Inverse proto-to-genesis conversion**: unlike protocol parameters, `ReadGenesis` is read-only with no need for round-tripping.
  No `utxoRpcGenesisToGenesis` function is needed.
- **ReadEraSummary**: this is a separate `QueryService` method defined in the upstream spec; it warrants its own story.
- **Minimum era guard**: unlike `ReadParams` which requires Conway, `ReadGenesis` returns static configuration from all eras and does not need an era check.

## Definition of done

- [ ] All AC tests written (compile, fail on stubs)
- [ ] Implementation complete (all tests pass via `cabal test`)
- [ ] Nix CI checks pass (`nix build 'path:.#checks.x86_64-linux.test'` and `e2e`)
- [ ] haskell-reviewer agent finds no critical or style issues
- [ ] fourmolu clean
- [ ] No build warnings

## Notes

### Open questions

- **Which genesis hash for the `genesis` bytes field?**
  Cardano has four genesis files (Byron, Shelley, Alonzo, Conway), each with its own hash.
  The proto spec says "genesis hash for the chain" (singular).
  Need to check what Dolos and other UTxO RPC implementations return here.
  Most likely the Shelley genesis hash, as it is the one that identifies the network, but this must be confirmed against the spec or reference implementations before implementation.
  The `utxorpc-compare` skill can verify what other servers return.

- **CAIP-2 format and derivation.**
  CIP-34 defines Cardano's CAIP-2 identifier.
  The exact format and how to derive it from genesis data (genesis hash prefix? network magic?) needs confirmation from CIP-34 and cross-referencing with Dolos output.

### Implementation risks

- **Extracting per-era genesis from `TopLevelConfig`/`HardForkBlock`.**
  The `mkNodeKernelAccess` function currently only uses `getChainDB` from the `NodeKernel`.
  Accessing genesis configs likely requires `getTopLevelConfig` and then navigating the `HardForkLedgerConfig` to extract per-era genesis.
  This may need consensus-specific accessors or new cardano-api surface.
  Worth spiking early.

- **Proto field coverage.**
  The `Genesis` proto message has ~42 fields across four eras.
  Some map directly to ledger types (epoch_length, network_magic); others (avvm_distr, vss_certs) may require non-trivial extraction from Byron genesis.
  The golden test in AC3 should cover the high-value fields first, with explicit TODOs for any deferred fields.

### Design decisions

- **Genesis data is static.**
  Unlike protocol parameters or UTxO state, genesis configs never change after node startup.
  This means the handler can cache the converted proto message on first call, avoiding repeated conversion.
  However, caching is an optimisation and not required for correctness.

- **Follows FetchBlock pattern, with a key difference.**
  FetchBlock uses an `m`-wrapped callback because it queries ChainDB per request.
  ReadGenesis uses a pure field because genesis data is static.
  The conversion layer (AC3) maps to proto, handler (AC4) assembles the response.
  This keeps the node-kernel boundary clean and the conversion testable in isolation.

### E2E harness

- The E2E test in AC4 depends on having a running cardano-node with the gRPC server enabled.
  The exact E2E harness location needs confirming (likely `cardano-node-tests` or nix-level integration tests).

### Dependencies

- Upstream proto: the `ReadGenesis` service line must be added to the local `query.proto` and code regenerated (AC1).
- cardano-node wiring: `mkNodeKernelAccess` in `NodeKernelAccess.Internal` must be updated to implement the new callback.
  The node-side call site in `cardano-node/src/Cardano/Node/Run.hs` (line ~590) may need adjustment if the callback requires data beyond what `NodeKernel` provides directly.
