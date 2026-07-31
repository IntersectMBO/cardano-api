# Era ranges with `vary`: a design to replace eon boilerplate

Status: draft proposal.
Branch: `mgalazyn/support-era-ranges-with-vary`.

## Problem

The current eon machinery costs ~2,500 lines across `Cardano/Api/Era/Internal/` and scales badly:

- 12 hand-written era-range GADTs (`ShelleyBasedEra`, `AlonzoEraOnwards`, `ShelleyToBabbageEra`, ...), each a ~110-150 line module of identical shape: GADT, `Eon`/`ToCardanoEra`/`Convert` instances, a `<Name>Constraints` synonym that re-lists a ~33-item constraint preamble, a CPS reifier, and an `Is*BasedEra` class.
- Adding a new era touches every eon module, all six `Case.hs` dispatchers, the existentials, and the `Is*` ladder.
- Adding a new range costs a whole new module.
- The scheme fails open at runtime: every constraint bundle except `alonzoEraOnwardsConstraints` is an `error "TODO Dijkstra"` stub, all six `caseXOrY` dispatchers error on Dijkstra, and `AnyCardanoEra` cannot even represent Dijkstra.

The experimental API (ADR-004) sidesteps eons by fixing a two-era window (`Era` = Conway | Dijkstra) with one constraint bundle, but it cannot express historical eras, and ADR-016 notes that any back-port "would need to be revisited".

## Design principles

- **Simplicity first: the API must be easy to grok by newcomers.**
  The public vocabulary is deliberately tiny: a range alias (`Onwards ConwayEra`, `ShelleyEra :-: BabbageEra`), a witness (`EraIn range era`), one way to get constraints (`withRange`), one way to dispatch (`split`, `Vary.on`).
  A newcomer should be able to read a signature like `f :: EraIn (Onwards AlonzoEra) era -> Tx era -> X` and understand it without knowing any of the machinery.
- **Focus on reducing boilerplate.**
  Every layer below exists to delete hand-maintained code: no per-range modules, no per-range instances, no hand-copied constraint tables, no per-boundary dispatchers.
  If a proposed feature requires per-range or per-era code, it fails this principle.
- **Proper abstractions hiding the gory details.**
  Type families (`From`/`UpTo`/`All`), structural induction (`Here`/`There`), overlapping instances and any positional `unsafeCoerce` tricks live in an `Internal` module and never leak into user-facing signatures or error messages.
  User-facing failures must read as domain errors ("`ShelleyEraTxCert` has no `DijkstraEra` instance", "era not in range `Onwards AlonzoEra`" via custom `TypeError`s), not as exposed type-level plumbing.

## Core idea

An era range is not a datatype, it is a **type-level list of era tags**, sliced from a single master timeline.
Membership, subset and widening then come from `vary`'s vocabulary (`(:|)`, `Subset`, `morph`), constraints come from a generic `All` family selected through a structural membership witness, and existential dispatch is a `Vary` of era singletons.

Everything below is written once, generically.
No per-range modules, no per-range constraint tables, no per-range instances.

### Layer 0: one master timeline, grounded in the ledger

The ledger already encodes the era chain.
`Cardano.Ledger.Internal.Definition.Era` (cardano-ledger-core) defines every era as an empty data type, and the `Era` class carries an injective associated family encoding the predecessor relation:

```haskell
type PreviousEra era = (r :: Type) | r -> era
-- PreviousEra DijkstraEra = ConwayEra, ..., PreviousEra ShelleyEra = ByronEra
type LatestKnownEra = DijkstraEra
```

All of it imports cleanly from `Cardano.Ledger.Api.Era` (cardano-api already depends on cardano-ledger-api); never import the warning-tagged `Cardano.Ledger.Internal.Era`.
The chain is walkable with a closed family - injectivity is not needed for the backward walk, only concrete reduction:

```haskell
type family ErasDownFrom (era :: Type) :: [Type] where
  ErasDownFrom L.ShelleyEra = '[L.ShelleyEra]                        -- stop before Byron/VoidEra
  ErasDownFrom era          = era ': ErasDownFrom (L.PreviousEra era)
```

(The base case must be an exported era: `PreviousEra ByronEra` is the deliberately unexported `VoidEra`.)

Two ways to define the cardano-api timeline:

- **Derive it**: `type LedgerEras = Reverse (ErasDownFrom L.LatestKnownEra)`, over ledger era types.
  This matches ADR-016's direction ("index the type by the ledger era, not the cardano-api era"), and a new ledger era extends the timeline without touching this module.
- **Verify it**: keep a hand-written list over cardano-api tags, with a compile-time canary asserting it maps (through the injective `ShelleyLedgerEra` family) onto the derived ledger chain.
  When the ledger bumps `LatestKnownEra`, the canary breaks at compile time, exactly where era support must be added.

```haskell
type ShelleyBasedEras =
  '[ShelleyEra, AllegraEra, MaryEra, AlonzoEra, BabbageEra, ConwayEra, DijkstraEra]
```

Recommendation: start with the verified hand-written list over cardano-api tags, because layers 2-4 speak `CardanoEra` singletons; revisit deriving over ledger eras once the ledger-indexed experimental `TxBodyContent` becomes the main consumer.
Byron stays outside, exactly as `ShelleyBasedEra` excludes it today; the ledger's usable upgrade chain also starts at Shelley (the `ByronEra` instances of `TranslateEra`/`EraApi` are error stubs).
(`ByronToAlonzoEra` is nearly dead: `caseByronOrShelleyBasedEra` is already slated for deletion.)

### Layer 1: ranges are computed slices

```haskell
type family From (e :: Type) (es :: [Type]) :: [Type] where
  From e (e ': rest) = e ': rest
  From e (_ ': rest) = From e rest

type family UpTo (e :: Type) (es :: [Type]) :: [Type] where
  UpTo e (e ': _)    = '[e]
  UpTo e (x ': rest) = x ': UpTo e rest

type Onwards e  = From e ShelleyBasedEras
type e1 :-: e2  = UpTo e2 (From e1 ShelleyBasedEras)

-- Every existing eon becomes a one-line alias:
type AlonzoOnwards    = Onwards AlonzoEra          -- '[AlonzoEra, BabbageEra, ConwayEra, DijkstraEra]
type ShelleyToBabbage = ShelleyEra :-: BabbageEra
type SupportedEras = Onwards ConwayEra          -- ADR-004's window is just another range
```

A new era appended to `ShelleyBasedEras` automatically joins every `Onwards`-defined range.
A new range is one line.

### Aside: the ledger's own ordering machinery, and when to use it instead

The ledger encodes era order a second way: Nat-valued protocol-version bounds per era (`ProtVerLow`/`ProtVerHigh`: Shelley 2, ..., Conway 9-11, Dijkstra 12), with constraint aliases built on them in `Cardano.Ledger.Core.Era` (re-exported by `Cardano.Ledger.Api.Era`):

```haskell
type AtLeastEra (n :: Symbol) era = ProtVerAtLeast era (ProtVerLow (EraFromName n))
type AtMostEra  (n :: Symbol) era = ProtVerAtMost  era (ProtVerHigh (EraFromName n))
type ExactEra inEra era           = ProtVerInBounds era (ProtVerLow inEra) (ProtVerHigh inEra)
```

For a signature that merely constrains a single ledger era to a bound, with no dispatch and no witness, these already suffice at zero cost - e.g. `hkdProtocolVersionL` requires `AtMostEra "Babbage" era` today.
The list-based ranges complement rather than replace them: lists add what bounds alone cannot - the `EraIn` runtime witness, `All`-derived constraint reification, total `split`, and enumeration (`SomeEraIn`, generic `Bounded`/`Enum`).
Where both work, prefer the ledger alias in ledger-era-indexed signatures and the range machinery where cardano-api needs witnesses or dispatch.

Adjacent and worth knowing: `TranslateEra` and `EraApi`'s `upgradeTx`/`upgradeTxOut`/... are keyed on `PreviousEra` (`X (PreviousEra era) -> X era`), a ready-made per-step upgrade to build range-crossing conversions on later.

### Layer 2: one witness type replaces twelve GADTs

```haskell
-- Structural proof that era is in the list es, carrying the singleton.
data EraIn (es :: [Type]) (era :: Type) where
  Here  :: CardanoEra era -> EraIn (era ': es) era
  There :: EraIn es era -> EraIn (e ': es) era

eraInToCardanoEra :: EraIn es era -> CardanoEra era
```

`EraIn AlonzoOnwards era` is isomorphic to today's `AlonzoEraOnwards era`, but the type is generic in the range.
Conjuring a witness from a `CardanoEra` singleton is one inductive class, which simultaneously provides the `Eon` instance for **every** range at once:

```haskell
class KnownEras (es :: [Type]) where
  checkMember :: CardanoEra era -> Maybe (EraIn es era)

instance KnownEras '[] where
  checkMember _ = Nothing

instance (IsCardanoEra e, KnownEras es) => KnownEras (e ': es) where
  checkMember era = case testEquality era (cardanoEra @e) of
    Just Refl -> Just $ Here era
    Nothing   -> There <$> checkMember era

instance KnownEras es => Eon (EraIn es) where
  inEonForEra no yes era = maybe no yes $ checkMember era
```

That single `Eon` instance means the entire existing combinator zoo (`forEraInEon`, `forEraMaybeEon`, `maybeEon`, `monoidForEraInEon`, `Featured`, `EraInEon`) works unchanged for every range: `Featured (EraIn ConwayOnwards) era a` type-checks today because `Featured` abstracts over `eon :: Type -> Type`.

The seven-class `Is*BasedEra` ladder collapses into constraints of the form `HasEraIn es era` (one overlapping-instance class that produces `EraIn es era` the way `Is*BasedEra` produces eon witnesses), or simply `IsCardanoEra era` plus a runtime `checkMember`.

### Honest overlap: what is and is not reimplemented from vary

`EraIn`'s `Here`/`There` induction replicates the *technique* vary uses internally for its own instances, not vary's *function*.
The distinction:

- `Vary (Sings range)` is existential: "some era in the range", with the era's type-level identity erased.
- `EraIn range era` is indexed: a proof about the specific type variable `era` that other values in scope (`Tx era`, `TxBody era`) share.
  Vary's public API has no indexed witness, and eons are exactly indexed witnesses, so this half cannot be delegated to vary.
- Constraint dispatch (`All`/`withRange`) does not exist in vary at all; vary's own `Eq`/`Show`/`Hashable` instances hand-roll the same head/tail induction privately, and `Vary.Core` is an unexposed other-module.

So the parts that look like vary internals are precisely the parts vary does not export; the parts vary does export (the existential value, O(1) `morph`, `(:|)`, `Subset`) are used rather than rebuilt, in `Range.Some`.

If less duplication is wanted there are two routes:

- Represent membership positionally with vary's `(:|)` (type-level index + `KnownNat`) and select dictionaries from a table at that index with `unsafeCoerce` - exactly `Vary.Core`'s discipline.
  O(1) instead of O(range length), but it imports unsafe coercion into cardano-api and loses the GADT refinement that makes `withRange` and `splitRange` total by construction.
  With ranges capped at 7 eras, safety wins over the constant factor.
- Upstream: propose an indexed membership witness (a `Vary.Elem`) and an `All`-style constraint fold to vary itself; if accepted, `EraIn` and `withRange` shrink to thin wrappers.

### Layer 3: constraints for a range, derived instead of hand-listed

```haskell
type family All (c :: Type -> Constraint) (es :: [Type]) :: Constraint where
  All _ '[]       = ()
  All c (e ': es) = (c e, All c es)

-- The one generic reifier replacing all twelve *Constraints CPS functions:
withRange :: forall c es era r. All c es => EraIn es era -> (c era => r) -> r
withRange (Here _)  k = k
withRange (There m) k = withRange @c m k

-- First-class evidence when it needs to be stored (in a Vary arm, a record, a Map):
rangeDict :: forall c es era. All c es => EraIn es era -> Dict (c era)
rangeDict (Here _)  = Dict
rangeDict (There m) = rangeDict @c m
```

The decisive property: `All c es` for a concrete range is discharged by GHC **from the real ledger instances, era by era**.
Nobody writes the 33-constraint tables any more.
And Dijkstra failure mode inverts: where today `conwayEraOnwardsConstraints` compiles and then explodes at runtime, `withRange @c` on a range containing `DijkstraEra` simply fails to compile with "no instance `c DijkstraEra`" until the ledger support lands.
That is precisely the compile-time tripwire AGENTS.md already recommends for concrete-era dispatch.

Constraints are requested through named bundles, and the agreed idiom keeps each constraint list written exactly once: a plain constraint alias, with the constraints in their natural applied form, is the single source of truth (used applied in signatures); a two-line class+instance eta-expansion references the alias and makes it passable to `withRange` unapplied (a synonym alone cannot serve there - synonyms cannot be passed unsaturated, and no GHC 9.6-compatible extension lifts that):

```haskell
type ConwayCertConstraints era =
  ( IsShelleyBasedEra era
  , L.ConwayEraTxCert (ShelleyLedgerEra era)
  )

class ConwayCertConstraints era => ConwayCertsC era

instance ConwayCertConstraints era => ConwayCertsC era

-- signatures use the alias, applied; boundaries use the class, unapplied:
f :: ConwayCertConstraints era => Tx era -> X
g w = withRange @ConwayCertsC w $ f ...
```

Named bundle classes also dodge GHC's constraint-tuple arity cap and keep error messages readable.
Note that ledger classes already carry `Show`/`Eq`/CBOR superclasses on their associated types (e.g. `Show (TxCert era)` is a superclass of `EraTxCert`), so bundles stay small - and listing such implied constraints in an alias used in signatures would trip `-Wredundant-constraints`.
The fixed cost of each eta-expansion site is the pragma trio `FlexibleInstances`, `UndecidableInstances`, `UndecidableSuperClasses` (the superclass chain reaches ledger's `ProtVerLow <= ProtVerHigh` family constraint, and the universal instance fails the Paterson condition by design).

This also scales to the eon-sized bundles: the existing `type XConstraints era = (...)` synonyms stay as-is and gain the two-line eta-expansion, with the 30-odd constraints never repeated.
`(:&:)` remains for combining two bundle classes at a boundary without naming the combination.
A one-line canary `_ok :: Dict (All AlonzoOnwardsC AlonzoOnwards); _ok = Dict` verifies a bundle for the whole range at library-compile time.

### Layer 4: `Vary` for the existential side

```haskell
type family Sings (es :: [Type]) :: [Type] where
  Sings '[]       = '[]
  Sings (e ': es) = CardanoEra e ': Sings es

-- "some era in the range" - replaces AnyCardanoEra, AnyShelleyBasedEra, EraInEon, Some Era
type SomeEraIn es = Vary (Sings es)

someEra     :: EraIn es era -> SomeEraIn es
withSomeEra :: KnownEras es => SomeEraIn es -> (forall era. EraIn es era -> r) -> r
```

What `vary` buys here:

- `Vary.morph` widens `SomeEraIn AlonzoOnwards` to `SomeEraIn ShelleyBasedEras` in O(1) (a retag), replacing the hand-written `Convert`/`AnyX -> AnyY` conversion swarm.
- `Vary.on @(CardanoEra ConwayEra) ... $ Vary.exhaustiveCase` gives total per-era dispatch where adding an era to the master list breaks every dispatch site at compile time - the tripwire again, now enforced by types instead of by convention.
- `Show`/`Eq`/`Ord`/`ToJSON` for `SomeEraIn es` come free from vary's own inductive instances over `CardanoEra e`'s instances.
- vary's `(:|)` and `Subset` provide the membership/subset constraint vocabulary for signatures (`era :| AlonzoOnwards =>`), with custom `TypeError`s already built in.

`vary` has no constraint-dispatch facility of its own; that is exactly what Layers 2-3 add, using the same head/tail induction vary uses internally for its instances.

### Case.hs replacement

`ShelleyBasedEras = ShelleyToBabbage ++ ConwayOnwards` holds by construction, so the six boundary dispatchers become one generic total function:

```haskell
split :: KnownLength xs => EraIn (xs ++ ys) era -> Either (EraIn xs era) (EraIn ys era)
```

`caseShelleyToBabbageOrConwayEraOnwards f g sbe` becomes `either f g (split w)`, with constraints recovered inside `f`/`g` via `withRange`.
No Dijkstra error arms are possible: `split` is total for whatever the master list contains.
Splits also nest: the piece of one split is a range like any other, so n-way dispatch is nested two-way splits - something the old dispatchers could not do (nesting `forShelleyBasedEraInEon` is explicitly forbidden today).

## Module layout and compatibility with `Cardano.Api.Experimental.Era`

The new code lives under `Cardano.Api.Experimental.Era.*`, next to the API it must interoperate with:

```
Cardano.Api.Experimental.Era.Range        -- From/UpTo/Onwards/(:-:), EraIn, KnownEras, All, withRange, rangeDict, split
Cardano.Api.Experimental.Era.Range.Some   -- Sings, SomeEraIn, someEra, withSomeEra (the vary dependency is isolated here)
```

Compatibility with the existing experimental `Era` GADT is total and mechanical, because the experimental window is itself a range:

```haskell
type SupportedEras = Onwards ConwayEra   -- '[ConwayEra, DijkstraEra], tracks the Era GADT by construction

instance Convert Era (EraIn SupportedEras) where
  convert = \case
    ConwayEra   -> Here (cardanoEra @ConwayEra)
    DijkstraEra -> There (Here (cardanoEra @DijkstraEra))

instance Convert (EraIn SupportedEras) Era where
  convert = \case
    Here _         -> ConwayEra
    There (Here _) -> DijkstraEra
```

- `obtainCommonConstraints` interop: wrap `EraCommonConstraints` in a class synonym `CommonC`, then `withRange @CommonC w k` on an `EraIn SupportedEras era` is exactly `obtainCommonConstraints (convert w) k`.
- `IsEra era` gives `HasEraIn SupportedEras era` (witness conjuring), so experimental call sites need no new constraints.
- `sbeToEra`'s narrowing role is subsumed by `checkMember`/`split`: `split @(ShelleyEra :-: BabbageEra)` on the full timeline returns the deprecated eras on the `Left` and `EraIn SupportedEras era` on the `Right`, replacing the `DeprecatedEra` throw with a total `Either`.

This placement follows ADR-009 (experimental modules are the sandbox for not-yet-stabilised APIs) and keeps the old `Cardano.Api.Era.Internal.Eon.*` tree untouched until migration starts.

## Showcase tests

A new test module `Test.Cardano.Api.Experimental.EraRange` demonstrates the three range shapes with real ledger constraints.
The cert classes make ideal examples because certificate support genuinely fragments across eras: `L.ConwayEraTxCert` holds from Conway onwards (Dijkstra keeps the class even though its concrete `TxCert` type is the separate `DijkstraTxCert`), while `L.ShelleyEraTxCert` stops before Dijkstra.

```haskell
-- 1. Lower bound (Onwards range): ConwayEraTxCert holds for every era in Onwards ConwayEra.
--    Compiles because GHC discharges All ConwayCertsC '[ConwayEra, DijkstraEra]
--    from the real ledger instances, including Dijkstra's.
prop_lower_bound :: Property
prop_lower_bound = ... $ \(w :: EraIn (Onwards ConwayEra) era) ->
  withRange @ConwayCertsC w $
    -- e.g. build a delegation certificate via mkDelegTxCert and inspect it
    ...

-- 2. Upper bound (:-: range): ShelleyEraTxCert has NO Dijkstra instance,
--    so it is only derivable for ranges bounded above by Conway.
prop_upper_bound :: Property
prop_upper_bound = ... $ \(w :: EraIn (ShelleyEra :-: ConwayEra) era) ->
  withRange @LegacyCertsC w $
    -- e.g. build a legacy RegTxCert and inspect it
    ...

-- 3. Subset: start from the full timeline, split at the Babbage/Conway boundary,
--    and gain ConwayEraTxCert only on the narrowed side.
prop_subset :: Property
prop_subset = ... $ \(w :: EraIn ShelleyBasedEras era) ->
  case split @(ShelleyEra :-: BabbageEra) w of
    Left _preConway ->
      -- no Conway cert machinery here, and requesting it would not compile
      ...
    Right (conwayOnwards :: EraIn (Onwards ConwayEra) era) ->
      withRange @ConwayCertsC conwayOnwards $ ...
```

The negative space is as important as the tests: `withRange @LegacyCertsC` over `Onwards ConwayEra` must NOT compile (Dijkstra lacks the `ShelleyEraTxCert` instance), and neither must a `TxCert era ~ L.ConwayTxCert era` equality outside the singleton range `'[ConwayEra]` - the exact equality whose unsatisfiability makes today's `conwayEraOnwardsConstraints` crash at runtime for Dijkstra.
These are documented as commented-out must-not-compile examples next to the tests (a deliberate-compile-failure test target can follow later).

## What dies, what replaces it

| Today | LOC | Replacement | LOC |
|---|---|---|---|
| 12 eon GADT modules | ~1,770 | `EraIn` + `KnownEras` + range aliases | ~60 + 1 line/range |
| 12 `*Constraints` synonyms + reifiers | (incl. above) | `All` + `withRange`/`rangeDict` | ~20 |
| 13 `Eon` instances | (incl. above) | 1 `Eon (EraIn es)` instance | ~5 |
| ~30 `Convert` instances | (incl. above) | `checkMember`/`morph`/one `widen` | ~15 |
| `Case.hs` (6 dispatchers, all Dijkstra bombs) | 134 | generic `split` | ~25 |
| `Is*BasedEra` 7-class ladder | (spread) | `HasEraIn` class | ~10 |
| `AnyCardanoEra`/`AnyShelleyBasedEra`/`EraInEon`/`Some Era` | ~250 | `SomeEraIn es` + generic Bounded/Enum | ~40 |
| Per-new-era cost: every module above | | extend master list + tag/singleton/`IsCardanoEra`; ranges update automatically | |

Rough total: ~2,500 lines of manually-maintained machinery becomes a ~250-line generic core plus one alias per range.
Runtime `error "TODO Dijkstra"` stubs in the era machinery become compile errors at exactly the call sites that need missing instances.

## Migration story

- `EraIn es` is an `Eon`, so all generic combinators and `Featured` keep working immediately.
- Bidirectional pattern synonyms on `EraIn` named after the old constructors (`pattern AlonzoEraOnwardsAlonzo :: () => era ~ AlonzoEra => EraIn AlonzoOnwards era`) make migration mostly mechanical: change the type name, keep the pattern matches.
- Shims keep the 328 `shelleyBasedEraConstraints` call sites compiling during transition: `shelleyBasedEraConstraints w k = withRange @ShelleyBasedC (fromOldEon w) k`.
- The experimental API slots in rather than competes: `Era era ≅ EraIn SupportedEras era` and `EraCommonConstraints era ≅` a class synonym used with `All`.
  ADR-016's "back-porting would need to be revisited" becomes cheap: back-porting is choosing a wider range alias.

## Dependencies

- `vary` (0.1.1.3): on Hackage inside the pinned index-state, GHC 8.10+, core deps only `base` + `deepseq`.
  Needs adding to `cardano-api.cabal` `build-depends` (not yet in the cardano-api build plan).
- `constraints` (0.14.4): already `pre-existing` in the build-plan closure (via ouroboros-consensus, servant, sop-extras), so a direct dependency costs nothing.
  Only needed for the first-class `Dict` form (`rangeDict`); the CPS `withRange` is dependency-free.
- `cardano-ledger-api`: already a direct dependency; `Cardano.Ledger.Api.Era` is the sanctioned import for `PreviousEra`, `LatestKnownEra`, the era types, and the `AtLeastEra`/`AtMostEra` machinery (never the warning-tagged `Cardano.Ledger.Internal.Era`).

## Risks and gotchas

- **`TestEquality CardanoEra` has no Dijkstra arm** (`Era/Internal/Core.hs:299-307` falls through to `Nothing`).
  `checkMember` routes through it, so Dijkstra would silently read as "not a member".
  This is a pre-existing latent bug and must be fixed first (Boy Scout rule).
- GHC 9.6/9.10/9.12 must all be happy without CPP.
  Everything used (closed type families, GADT recursion over lists, class synonyms, overlapping instances for `HasEraIn`) is old, stable tech; vary itself supports 8.10+.
  The known MonoLocalBinds tuple-binding gotcha (AGENTS.md) applies to dict-heavy call sites: keep explicit signatures.
- Overlap/incoherence: vary's `Subset` uses `INCOHERENT` instances internally; our own core needs at most standard `OVERLAPPING` for `HasEraIn`, kept out of the public API surface.
- Error-message quality: `All c es` failures name the exact missing per-era instance, which is better than today; membership failures via vary's `(:|)` produce its custom `TypeError`.
  Custom `TypeError`s on `From`/`UpTo` misuse (era not in timeline, inverted bounds) are worth adding.
- Compile-time cost: `All` expands to at most 7 constraints per request, versus today's 33-item bundles re-listed at every reifier; expect neutral or better.
- `EraIn` selection is O(range length) at runtime, bounded by 7, called where today a `\case` sits; irrelevant.
  The `Vary` existential is a single `Word` tag; `morph` is O(1).

## Suggested next steps

1. Fix the `TestEquality CardanoEra` Dijkstra arm.
2. Prototype the core module `Cardano.Api.Experimental.Era.Range` (gory details in `Cardano.Api.Experimental.Era.Range.Internal`): `From`/`UpTo`/aliases, `EraIn`, `KnownEras`, `Eon` instance, `All`, `withRange`, `rangeDict`, `split`, plus `Cardano.Api.Experimental.Era.Range.Some` for the vary existential (`SomeEraIn`, `someEra`, `withSomeEra`).
3. Add compile-time canaries: the ranges that must hold today (`All ShelleyBasedC ShelleyBasedEras` minus the constraints Dijkstra genuinely lacks, documenting the residue as the true "TODO Dijkstra" list), and the timeline-vs-ledger check against `ErasDownFrom L.LatestKnownEra`.
4. Add the showcase tests (`Test.Cardano.Api.Experimental.EraRange`): lower bound, upper bound, and subset, per the section above.
5. Migrate one real consumer end-to-end as proof (candidate: a `caseShelleyToBabbageOrConwayEraOnwards` call site in cardano-api, since those are pure boilerplate today).
6. Verify with `cabal build cardano-api` from the metarepo root.
