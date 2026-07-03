# FetchBlock Byron transaction mapping

Populates `Block.body.tx` for Byron-era blocks in the FetchBlock response.
Today the handler silently returns an empty transaction list for Byron blocks, which covers mainnet epochs 0 to 207 (roughly 4.5 years of history).
Each phase compiles independently and can be reviewed as a separate commit.

## Background

cardano-api's `Tx era` GADT has no Byron constructor and `getBlockTxs` returns `[]` for `ByronBlock`, so the existing `txToUtxoRpcTx` path can never see Byron transactions.
The conversion must therefore work directly on the Byron ledger types, bypassing `Tx era` entirely.

The data path is:
`Block ByronEra` (cardano-api GADT, `ByronBlock` constructor) → `Consensus.ByronBlock` → `byronBlockRaw :: ABlockOrBoundary ByteString` → `ABOBBlock` → `blockTxPayload` → `aUnTxPayload :: [ATxAux ByteString]`.
`ABOBBoundary` (epoch boundary blocks) carry no transactions and map to an empty list.

## Field mapping

| Proto `Tx` field | Source | Notes |
|---|---|---|
| `hash` | `hashDecoded (aTaTx txAux)` | Hashes the original on-chain annotated bytes; 32 raw bytes, same convention as Shelley. Never re-serialise. |
| `inputs` | `txInputs` (`NonEmpty TxIn`) | `TxInUtxo txId ix`: `tx_hash` = 32 raw bytes, `output_index` = `Word16` widened to `uint32`. No `as_output`, no redeemer. |
| `outputs` | `txOutputs` (`NonEmpty TxOut`) | `address` = raw Byron address CBOR via `serialiseToRawBytes (ByronAddress addr)` (matches the pallas/gouroboros raw-bytes convention); `coin` via `lovelaceToInteger`. No assets, datum or script in Byron. |
| `witnesses.bootstrapWitnesses` | `VKWitness vk sig` entries | `unpackByronVKey` splits the XPub into a 32-byte vkey and the chain code; signature = raw 64 bytes via `unXSignature`; `attributes` = empty (Byron attributes live in addresses, not witnesses). |
| `witnesses.vkeywitness` | `RedeemWitness rvk rsig` entries | Plain Ed25519: 32-byte redeem key, 64-byte signature. |
| `fee` | unset (proto default 0) | Byron fees are implicit (inputs minus outputs) and not recoverable without UTxO lookups the handler does not have. Documented in both the conversion and handler haddocks. |
| `successful` | `True` | Byron has no phase-2 validation. |
| everything else | absent/empty | Validity, mint, certificates, withdrawals, collateral, reference inputs, auxiliary data and proposals do not exist in Byron transactions. |

## Phase 1: consensus re-export

- `Cardano.Api.Consensus` re-exports the `ByronBlock` type but not `byronBlockRaw`.
- Add `byronBlockRaw` (and `ABlockOrBoundary (..)` if not already reachable) to `Cardano.Api.Consensus.Internal.Reexport` and the `Cardano.Api.Consensus` export list, following the existing re-export structure.
- Ledger-level Byron types (`ATxAux`, `Tx`, `TxIn`, `TxOut`, witness types) come from direct `cardano-ledger-byron` imports in cardano-rpc, mirroring how `Certificate.hs` imports concrete ledger modules.

Build: `cabal build cardano-api`

## Phase 2: conversion module

- New module `Cardano.Rpc.Server.Internal.UtxoRpc.Byron` exporting
  `byronTxToUtxoRpcTx :: ATxAux ByteString -> UtxoRpc.Tx` (plain proto message, house style).
- Roughly 80-100 lines including haddock.
- cardano-rpc.cabal: add `cardano-ledger-byron`, `cardano-crypto-wrapper` and (if `unXSignature` needs it) `cardano-crypto` to the library build-depends.
  All are already in the build plan transitively, so this is build-depends lines only.

Build: `cabal build cardano-rpc`

## Phase 3: handler wiring

- In `fetchBlockMethod` (Sync.hs), the block is already destructured as `BlockInMode era block`.
- Case on `block`: `ByronBlock byronBlk` → `byronBlockRaw` → `ABOBBlock` maps `byronTxToUtxoRpcTx` over the payload, `ABOBBoundary` → `[]`; Shelley-based eras keep the existing `forEraInEon` path.
- About 10-15 lines.
- Caveat: the `Block era` constructors carry a deprecation pragma ("Use getBlockHeader instead"), and there is no non-deprecated accessor for the underlying consensus block.
  The match will raise a deprecation warning; options are a scoped `-Wno-deprecations` on the import, or accepting the warning.
  Decision needed at review time; the test suite builds with `-Wall` but not `-Werror`.

Build: `cabal build cardano-rpc`

## Phase 4: tests

- New module `Test.Cardano.Rpc.ByronTx` in cardano-rpc-test; cabal gains `cardano-ledger-byron:testlib` (and `cardano-crypto-wrapper:testlib` if needed) - both already in the build plan.
- Property: generate `Test.Cardano.Chain.UTxO.Gen.genTxAux` (produces `ATxAux ()`), re-annotate by serialising at `byronProtVer` and decoding with the ledger annotator API to obtain `ATxAux ByteString` (copy the round-trip pattern used by cardano-ledger-byron's own tests), then assert projections:
  hash equals `hashDecoded` bytes, input and output counts and contents, output address bytes equal the serialised Byron address, witness counts and arm routing (VKWitness → bootstrap, RedeemWitness → vkey), fee 0, all other fields empty.
- Optional hardening: a golden test decoding a real mainnet Byron block CBOR fixture, guarding the hash and address conventions against pallas.
  Requires sourcing a fixture; deferred unless requested.

Build: `cabal test cardano-rpc-test`

## Phase dependency graph

```
Phase 1 (byronBlockRaw re-export)
  |
  v
Phase 2 (Byron.hs conversion module)
  |
  v
Phase 3 (handler wiring)
  |
  v
Phase 4 (property tests)
```

## Design decisions

- **Bypass `Tx era` entirely.**
  cardano-api's transaction type cannot represent Byron transactions; the conversion consumes the Byron ledger types directly.

- **Hash from annotated bytes.**
  `hashDecoded` over `aTaTx` hashes the exact on-chain bytes; re-serialising risks a different encoding and therefore a wrong transaction id.

- **Fee stays unset.**
  Byron transaction bodies do not carry a fee; computing it requires resolving inputs against the UTxO set, which FetchBlock deliberately does not do.
  `native_bytes` remains the fidelity escape hatch.

- **Witness arm convention.**
  `VKWitness` carries an extended public key (XPub), which is what the proto `BootstrapWitness` shape (vkey + chain code) models; `RedeemWitness` is a plain Ed25519 pair and maps to `VKeyWitness`.
  This should be cross-checked against pallas once, at review time.

- **No E2E coverage is possible.**
  cardano-testnet chains start in a Shelley-based era (`creationEra :: AnyShelleyBasedEra`), so coverage is generator-based unit properties plus the optional golden fixture.

## Estimate

Roughly 200-280 lines total (conversion 80-100, handler 15, cabal 10, tests 100-150); about 1 to 1.5 days including review.
No new packages enter the build plan.

## Open questions for review

1. Deprecation warning on the `Block era` constructor match in the handler: accept the warning, or scoped `-Wno-deprecations`?
2. Golden fixture: source a mainnet Byron block now, or defer?
3. Should `RedeemWitness` instead map to `bootstrapWitnesses` with an empty chain code, if pallas does that? (Cross-check pending.)
