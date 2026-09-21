# Plan: replace fine-grained eons with ledger-class constraints

## Goal

Reviewer guidance: cardano-rpc should only use the experimental `Era era` or `ShelleyBasedEra era`.
Any other eon (`AllegraEraOnwards`, `MaryEraOnwards`, `AlonzoEraOnwards`, `BabbageEraOnwards`, `ConwayEraOnwards`, `ShelleyToBabbageEra`) indicates a cardano-api function that needs converting.
This plan removes those eons from cardano-rpc: experimental `Era` where the code is Conway-onwards only, ledger-class constraints (`AnyEra*` from cardano-ledger-api) where the code must serve every Shelley-based era.

## Verified foundations

- The pinned `cardano-ledger-api 1.13.0.0` already ships the `AnyEra*` class family (verified in the installed interface files: `AnyEraTxBody`, `vldtTxBodyG`, `mintTxBodyG` present); no ledger bump is required.
- The ledger class chain is Dijkstra-total: `MaryEraTxBody`, `AlonzoEraTxBody`/`AlonzoEraTx`/`AlonzoEraTxWits`, `BabbageEraTxBody` and `ConwayEraTxBody` all have Dijkstra instances; only `ShelleyEraTxCert` excludes Dijkstra, by design (`AtMostEra "Conway"` superclass).
- `instance AnyEraTxBody DijkstraEra` and `instance AnyEraTxCert DijkstraEra` are empty instance bodies: era-gated getters degrade to `Nothing` instead of crashing, unlike cardano-api's eon bundles with their `error "TODO Dijkstra"` stubs.
- The experimental `Era` is a deliberate Conway+Dijkstra sliding window and cannot serve the historical arm; `sbeToEra` is the partial bridge.
- The current eon usage in cardano-rpc is 12 dispatch sites and 5 pure constraint-obtainment sites across Tx.hs, Certificate.hs, ProtocolParameters.hs and two test modules.

## Phase 0 - spikes (no production changes)

1. Check `AnyEraTxCert` variant coverage at the 1.13 pin against the 19 certificate oneof arms `Certificate.hs` maps; the phase 4 shape depends on this.
2. Pin down the `TopTx`/`TxLevel` axis: the collateral getters and `isValidTxL` are typed over `TxBody TopTx era`/`Tx TopTx era`; determine how cardano-api's `ShelleyTx` payload lines up with that parameter at the current pin.
3. Name-by-name check that every getter the nine Tx.hs gates need exists at 1.13 (mint, reference inputs, collateral inputs, collateral return, total collateral, is-valid, datums, redeemers, proposals); the vendored 1.14 checkout is not proof for the pin.

Gate: written findings; go/no-go for the phase 3 and phase 4 shapes.
Fallback if 1.13 lacks needed getters: bump the CHaP index for a newer cardano-ledger-api (separate decision, `/bump-indices`).

## Phase 1 - upstream cardano-api: extend `EraCommonConstraints`

- Add `L.ConwayEraPParams (LedgerEra era)` to `EraCommonConstraints` in `Cardano.Api.Experimental.Era`.
- Drop the `conwayEraOnwardsConstraints (convert era)` detour in `utxoRpcPParamsToProtocolParams` (`Type/ProtocolParameters.hs`); `obtainCommonConstraints era` alone then suffices.
- Changelog fragment for cardano-api (compatible: the bundle provides more, callers unaffected).

Gate: `cabal build cardano-rpc && cabal test cardano-rpc-test` from `/work`, all 71 tests, zero warnings.
Independent of every other phase; can go first as its own PR.

## Phase 2 - upstream cardano-api: convert `toScriptIndex` - REJECTED (attempted 2026-07-15, reverted)

- The attempt worked inside this repo (build and all 71 tests green) but broke cardano-node: `Cardano.Node.Tracing.Era.Shelley` calls `Api.toScriptIndex alonzoOnwards` at two sites, and other out-of-repo consumers likely exist.
- Converting `toScriptIndex` therefore needs a coordinated cross-repo migration, which is out of scope for this PR; the signature stays `AlonzoEraOnwards era -> ...`.
- Consequence for phase 3: the redeemer-indexing site in `Type/Tx.hs` keeps one local eon derivation for the `toScriptIndex` argument (`Eval.hs` already derives it totally from the experimental `Era` via `convert`, which satisfies the reviewer's rule).
- If upstream ever coordinates the change, the working shape is recorded here: `ShelleyBasedEra era` parameter with an `L.AlonzoEraScript (ShelleyLedgerEra era)` constraint, purposes projected through the `L.AlonzoEraScript`/`L.ConwayEraScript` class methods (`toSpendingPurpose` etc.), Conway-onwards purposes matched at the concrete constructors; downstream eon-holding callers pass `convert <witness>`.

## Phase 3 - Tx.hs: replace the nine eon gates with `AnyEra*` getters

- Add one local constraint boundary in cardano-rpc: a function matching the seven `ShelleyBasedEra` constructors (the allowed GADT) and providing the `AnyEraTx`/`AnyEraTxBody`/`AnyEraTxWits` instances for `ShelleyLedgerEra era`; at concrete constructors all instances resolve with no eon machinery, and the Dijkstra arm compiles because the ledger instances exist.
- This removes the `shelleyBasedEraConstraints` crash from the transaction path: Dijkstra blocks convert instead of erroring.
- Replace each `forShelleyBasedEraInEon` gate with the corresponding getter, mapping `Nothing` to the current proto default (`[]`, `mempty`, `defMessage`, `IsValid True`): mint, reference inputs, collateral inputs, collateral return + total collateral, is-valid, datums, redeemers, proposals, validity interval.
- The Shelley `ttlTxBodyL` special case stays as a concrete `ShelleyBasedEraShelley` arm if `vldtTxBodyG`'s total getter does not subsume it (phase 0 answers this).
- Delete the proposals tripwire comment: with a `ConwayEraTxBody`-backed getter the Dijkstra arm is live code, not a dead arm.
- Keep the `ShelleyBasedEra era` parameter: the output and address sub-conversions (`fromShelleyTxOut`, `serialiseAddress`) still need it, and it feeds the phase 2 `toScriptIndex`.
- Tests: the six per-era totality properties and the Conway projection property are the oracle and must stay green unchanged; keep the four `forShelleyBasedEraMaybeEon` feature oracles in `FetchBlockTx.hs` (asking whether an era supports a feature is legitimate regardless of how production answers it).

Gate: build + all 71 tests, zero warnings; wire behaviour byte-identical for Shelley..Conway.
Note for the changelog: Dijkstra behaviour changes from runtime crash to working conversion.

## Phase 4 - Certificate.hs - DONE (getter approach rejected, concrete dispatch implemented)

- The phase 0.1 coverage claim did not survive a source check: `AnyEraTxCert` cannot read pre-Conway stake delegations (`anyEraToDelegTxCert = const Nothing` for Shelley..Babbage and no legacy `DelegStakeTxCert` getter exists), and a getter-based encoder needs an error fallback for unmatched certificates, reintroducing hidden partiality.
- Implemented instead as an exploded seven-constructor `ShelleyBasedEra` match onto the three constructor-exhaustive family matchers (`shelleyTxCertToUtxoRpcCertificate`, `conwayTxCertToUtxoRpcCertificate`, `dijkstraTxCertToUtxoRpcCertificate`); the compiler proves totality per era and `caseShelleyToBabbageOrConwayEraOnwards` is no longer used.
- The Dijkstra certificate conversion is live again (delegation deposits bridge losslessly through `dijkstraToConwayDelegCert`; `cardano-ledger-dijkstra` is a direct dependency once more), so the whole Dijkstra transaction path now converts.

Gate passed: build + all 71 tests, zero warnings; the certificate-arm routing assertions in the Conway projection property unchanged.

## Phase 5 - upstream cardano-api conversions - PARTIALLY DONE

- DONE: `toScriptIndexAlonzo`, `toScriptIndexConway` and `toScriptIndexDijkstra` are exported from `Cardano.Api.Tx.Internal.Body` (additive, signatures generalised over the ledger era); cardano-rpc's local duplicates are deleted and the redeemer dispatch uses the exports.
- DEFERRED: relaxing `queryDRepState` (and siblings) and `queryStakeDelegDeposits` to `ShelleyBasedEra era` breaks out-of-repo callers (cardano-cli `EraBased/Query/Run.hs`, cardano-testnet `Components/Query.hs` pass eon witnesses), the same cross-repo coordination class as the rejected phase 2. `Eval.hs` already derives these eons totally from the experimental `Era` via `convert`, which satisfies the reviewer's rule.
- VERIFIED for the future relaxation: the consensus version-gating table (`Ouroboros.Consensus.Shelley.Ledger.Query`) marks `GetStakeDelegDeposits` as `const True` - valid at every protocol version - so cardano-api's `BabbageEraOnwards` restriction is API decoration only; `GetDRepState` and friends are `(>= v8)` version-guarded at runtime.
- DEFERRED: converting `toLedgerValue` away from `MaryEraOnwards` breaks cardano-cli callers; `Predicate.hs`'s test-only witness already derives totally via `convert` from the experimental `Era`.

## Phase 6 - cleanup - DONE

- AGENTS.md documents the exploded `ShelleyBasedEra` dispatch as the accepted pattern, the `AnyEra*` getters, `anyEraTxConstraints`, and the `AnyEraTxCert` legacy-delegation gap.
- No unused eon imports remain (all builds pass with zero warnings and `-Wunused-imports`).
- Final verification passed: `cabal build cardano-rpc && cabal test cardano-rpc-test && cabal build cardano-testnet-test` from `/work`, zero warnings, all 71 tests green.

## Risks and open questions

- The `TopTx` axis (phase 0.2) is the main mechanical unknown; four getters depend on it.
- The 1.13 pin may lack getters the vendored 1.14 has (phase 0.3); the fallback is a CHaP bump.
- Dijkstra tx conversion changes from crash to graceful output; confirm that is wanted now rather than after upstream Dijkstra wiring completes.
- If phase 4 lands on the fallback, `caseShelleyToBabbageOrConwayEraOnwards` remains in exactly one place; confirm with the reviewer that the certificate dispatch is an accepted exception to the eon rule.
