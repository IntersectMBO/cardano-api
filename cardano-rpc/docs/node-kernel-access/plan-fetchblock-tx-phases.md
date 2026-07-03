# FetchBlock tx deserialisation phases

Populates `Block.body.tx` in the FetchBlock response.
Each phase compiles independently and can be reviewed as a separate commit.

## Background

FetchBlock currently returns `nativeBytes` (raw CBOR), `header` (slot, hash, height), and `timestamp`.
The `body.tx` field is empty.
The proto `Tx` message has 14 fields - implementing them all at once is too large to review.
This plan breaks the work into incremental phases, each adding a subset of fields.

The parsed block is already available via `Consensus.GetBlock` in the same `getBlockComponent` call.
`fromConsensusBlock` + `getBlockTxs` gives `[Tx era]` - no new consensus APIs needed.

## Existing building blocks in Type.hs

- `txOutToUtxoRpcTxOutput` - TxOutput (address, coin, assets, datum, script)
- `scriptDataToUtxoRpcPlutusData` - PlutusData
- `simpleScriptToUtxoRpcNativeScript` - NativeScript
- `referenceScriptToUtxoRpcScript` - Script (reference scripts)
- `utxoRpcBigIntToInteger` - BigInt (inverse)
- `scriptWitnessIndexToRedeemerPurpose` - redeemer purpose mapping
- `mkProtoRedeemer` - Redeemer construction

Missing: TxInput, Withdrawal, Certificate, WitnessSet, AuxData, Collateral, Multiasset (for mint), GovernanceActionProposal, and the top-level `Tx era -> Proto Tx` composition.

## Phase 1: Scaffold - extract txs from block, populate hash + fee

Minimal end-to-end wiring with the simplest fields.

- Change `fetchBlock` to also return the parsed `BlockInMode` (via `fromConsensusBlock` on `GetBlock`)
- In `fetchBlockMethod`, call `getBlockTxs` to get `[Tx era]`
- For each tx, populate:
  - `hash` - `getTxId` serialised to raw bytes
  - `fee` - from `txFee` on the tx body
  - `successful` - `True` (all blocks in ChainDB have validated txs; phase 1 does not distinguish collateral-return scripts)
- Set `Block.body.tx` on the response
- Add an E2E test asserting tx count, hash, and fee for a submitted tx

Build: `cabal build cardano-rpc`

## Phase 2: Inputs, outputs, reference inputs

The core spending data. Reuses existing `txOutToUtxoRpcTxOutput`.

- Add `txInputToProto :: TxIn -> Proto TxInput` (tx_hash + output_index, without as_output or redeemer - those come later)
- Map `txIns` -> `Tx.inputs`
- Map `txOuts` -> `Tx.outputs` (reuse `txOutToUtxoRpcTxOutput`)
- Map `txInsReference` -> `Tx.reference_inputs`
- Extend E2E test to assert input/output fields

Build: `cabal build cardano-rpc`

## Phase 3: Validity, mint, withdrawals

Simple scalar/list fields.

- Map `txValidityLowerBound` + `txValidityUpperBound` -> `Tx.validity` (TxValidity: start, ttl)
- Map `txMintValue` -> `Tx.mint` (repeated Multiasset: policy_id + assets)
- Map `txWithdrawals` -> `Tx.withdrawals` (repeated Withdrawal: reward_account + coin, no redeemer yet)
- Extend E2E test with a minting tx

Build: `cabal build cardano-rpc`

## Phase 4: Collateral

- Map `txInsCollateral` -> `Collateral.collateral`
- Map `txReturnCollateral` -> `Collateral.collateral_return`
- Map `txTotalCollateral` -> `Collateral.total_collateral`
- Set `Tx.collateral`
- `Tx.successful` - refine: check `IsValid` flag on Alonzo+ txs

Build: `cabal build cardano-rpc`

## Phase 5: Witnesses

- Map vkey witnesses -> `WitnessSet.vkeywitness`
- Map scripts (Plutus + native) -> `WitnessSet.script`
- Map plutus datums -> `WitnessSet.plutus_datums` (reuse `scriptDataToUtxoRpcPlutusData`)
- Map redeemers -> `WitnessSet.redeemers` (reuse `mkProtoRedeemer`)
- Map bootstrap witnesses -> `WitnessSet.bootstrapWitnesses`
- Wire redeemers to `TxInput.redeemer` and `Withdrawal.redeemer`

Build: `cabal build cardano-rpc`

## Phase 6: Certificates

The largest proto message (19 oneof variants).

- Map each `TxCert era` variant to the corresponding `Certificate` oneof
- Pre-Conway certs: stake registration/deregistration, delegation, pool registration/retirement, genesis delegation, MIR
- Conway+ certs: reg, unreg, vote deleg, stake vote deleg, committee hot/cold, DRep reg/unreg/update
- Wire certificate redeemers

Build: `cabal build cardano-rpc`

## Phase 7: Auxiliary data + governance proposals

- Map tx metadata -> `AuxData.metadata`
- Map auxiliary scripts -> `AuxData.scripts`
- Map governance action proposals -> `Tx.proposals` (Conway+)
- This completes all 14 Tx fields

Build: `cabal build cardano-rpc`

## Phase dependency graph

```
Phase 1 (scaffold: hash, fee, successful)
  |
  v
Phase 2 (inputs, outputs, reference inputs)
  |
  v
Phase 3 (validity, mint, withdrawals)
  |
  v
Phase 4 (collateral, successful refinement)
  |
  v
Phase 5 (witnesses + redeemer wiring)
  |
  v
Phase 6 (certificates)
  |
  v
Phase 7 (auxiliary data, governance proposals)
```

## Design decisions

- **Incremental field population.**
  Each phase adds fields to the same `Tx` proto message.
  Unset proto fields default to empty/zero, so partial responses are valid.

- **No new consensus APIs.**
  `fromConsensusBlock` + `getBlockTxs` already exist in cardano-api.
  The work is entirely in the cardano-rpc mapping layer.

- **Reuse existing conversions.**
  `txOutToUtxoRpcTxOutput`, `scriptDataToUtxoRpcPlutusData`, `mkProtoRedeemer` etc. are already battle-tested in the Query and Submit handlers.

- **Witnesses and redeemers split from inputs/withdrawals.**
  Phase 2-3 populate inputs and withdrawals without redeemers.
  Phase 5 adds the witness set and wires redeemers back to their inputs/withdrawals.
  This avoids a large cross-cutting change.

- **Certificates are a separate phase.**
  The Certificate oneof has 19 variants spanning pre-Conway and Conway eras.
  Isolating it keeps reviews focused.

- **Dijkstra safety.**
  `conwayEraOnwardsConstraints` crashes at runtime for Dijkstra.
  Phases 6-7 (certificates, governance proposals) must use
  `caseShelleyToBabbageOrConwayOrDijkstra` and pattern match on concrete
  `ConwayEraOnwards` constructors in the right arm.
  The cardano-api lenses `proposalProceduresTxBodyL` and `votingProceduresTxBodyL`
  use `conwayEraOnwardsConstraints` internally - avoid them, go through the ledger
  tx body directly with explicit era constraints.

- **Work through the ledger tx directly.**
  `ShelleyTxBody` is deprecated.
  Access the ledger `Tx` via the `ShelleyTx` constructor and use ledger lenses
  (`bodyTxL`, `witsTxL`, `auxDataTxL`, `isValidTxL`) rather than cardano-api wrappers.
