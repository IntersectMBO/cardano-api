# Migrating from legacy cardano-api to the experimental API

This guide explains how to migrate code from the legacy `Cardano.Api` transaction construction API to the experimental API (`Cardano.Api.Experimental`).

## Why migrate?

The legacy API wraps ledger types in cardano-api-specific wrappers (`TxFee era`, `TxInsCollateral era`, `TxMetadataInEra era`, etc.) that add indirection without value.
The experimental API uses ledger types directly, supports only current and upcoming eras (Conway, Dijkstra), and is the intended replacement per ADR-004 and ADR-009.

Symbols deprecated in favour of the experimental API include `createTransactionBody`, `signShelleyTransaction`, `getTxBody`, `TxBody`, `ShelleyTxBody`, `defaultTxBodyContent` (old), and `BalancedTxBody`.

## Import convention

```haskell
import Cardano.Api                    -- stable API (types, eras, addresses, etc.)
import Cardano.Api.Experimental       -- era types, makeUnsignedTx, signTx, etc.
import qualified Cardano.Api.Experimental.Tx as Exp  -- TxBodyContent, setters, TxOut, fee functions
```

The old and new APIs export identically-named symbols (`defaultTxBodyContent`, `setTxIns`, `setTxOuts`, etc.).
When both are in scope, qualify the experimental ones via `Exp.`.

## Era types

| Legacy | Experimental | Notes |
|--------|-------------|-------|
| `ShelleyBasedEra era` | `Era era` | GADT with only `ConwayEra` and `DijkstraEra` constructors |
| `CardanoEra era` | (none) | Not needed; experimental API only covers supported eras |
| `IsShelleyBasedEra era` | `IsEra era` | `IsEra` only provides `useEra :: Era era` |
| `shelleyBasedEraConstraints sbe` | `obtainCommonConstraints era` | Brings `EraCommonConstraints era` into scope |

### Bridging eras

Convert `ShelleyBasedEra era` to `Era era` using `sbeToEra`:

```haskell
sbeToEra :: MonadError (DeprecatedEra era) m => ShelleyBasedEra era -> m (Era era)
```

Pre-Conway eras return `Left (DeprecatedEra sbe)` when `m ~ Either (DeprecatedEra era)`.
Pattern-match on the result and handle the error for unsupported eras.

## TxBodyContent

### Old (deprecated)

```haskell
data TxBodyContent build era = TxBodyContent
  { txIns            :: [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]
  , txInsCollateral  :: TxInsCollateral era
  , txOuts           :: [TxOut CtxTx era]
  , txFee            :: TxFee era
  , txValidityUpperBound :: TxValidityUpperBound era
  , txMetadata       :: TxMetadataInEra era
  , txProtocolParams :: BuildTxWith build (Maybe (LedgerProtocolParameters era))
  , ...
  }

defaultTxBodyContent :: ShelleyBasedEra era -> TxBodyContent BuildTx era
```

### New (experimental)

```haskell
data TxBodyContent era = TxBodyContent
  { txIns            :: [(TxIn, AnyWitness era)]
  , txInsCollateral  :: [TxIn]
  , txOuts           :: [TxOut era]         -- wraps L.TxOut era
  , txFee            :: L.Coin
  , txValidityUpperBound :: Maybe L.SlotNo
  , txMetadata       :: TxMetadata
  , txProtocolParams :: Maybe (L.PParams era)
  , ...
  }

defaultTxBodyContent :: TxBodyContent era    -- no arguments needed
```

### Field-by-field mapping

| Old type | New type | Conversion |
|----------|----------|------------|
| `(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))` | `(TxIn, AnyWitness era)` | Key witnesses: use `AnyKeyWitnessPlaceholder`; script witnesses: see witness section |
| `TxInsCollateral era` | `[TxIn]` | Extract the list of `TxIn`; `TxInsCollateralNone` becomes `[]` |
| `TxOut CtxTx era` | `Exp.TxOut era` | `Exp.TxOut (toShelleyTxOutAny sbe oldTxOut)` |
| `TxFee era` | `L.Coin` | `TxFeeExplicit _ coin` -> `coin` |
| `TxValidityUpperBound era` | `Maybe L.SlotNo` | `TxValidityUpperBound _ mSlot` -> `mSlot`; `Nothing` = no upper bound |
| `TxMetadataInEra era` | `TxMetadata` | `TxMetadataNone` -> `mempty`; `TxMetadataInEra _ m` -> `m` |
| `BuildTxWith BuildTx (Maybe (LedgerProtocolParameters era))` | `Maybe (L.PParams era)` | `unLedgerProtocolParameters` to unwrap; `setTxProtocolParams` wraps with `Just` |
| `TxInsCollateral era` | `[TxIn]` | Drop the eon witness |

### Using setters

```haskell
-- Old
defaultTxBodyContent sbe
  & setTxIns [(txIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
  & setTxOuts [txOut]
  & setTxFee (TxFeeExplicit sbe coin)
  & setTxInsCollateral (TxInsCollateral eon [collateralTxIn])

-- New
Exp.defaultTxBodyContent
  & Exp.setTxIns [(txIn, AnyKeyWitnessPlaceholder)]
  & Exp.setTxOuts [Exp.TxOut $ toShelleyTxOutAny sbe txOut]
  & Exp.setTxFee coin
  & Exp.setTxInsCollateral [collateralTxIn]
```

## Transaction construction

### Old flow (deprecated)

```haskell
txBody <- createTransactionBody sbe txBodyContent
let tx = signShelleyTransaction sbe txBody [WitnessPaymentKey signingKey]
```

Or manual construction:
```haskell
let ledgerTxBody = mkCommonTxBody sbe txInputs txOuts fee ...
    rawBody = ledgerTxBody ^. txBodyL
    unsignedLedgerTx = Ledger.mkBasicTx rawBody
    txHash = Ledger.extractHash $ Ledger.hashAnnotated rawBody
    witVKey = WitVKey (getShelleyKeyWitnessVerificationKey sk) (makeShelleySignature txHash sk)
    signedLedgerTx = unsignedLedgerTx & Ledger.witsTxL .~ ...
    tx = ShelleyTx sbe signedLedgerTx
```

### New flow (experimental)

```haskell
case sbeToEra sbe of
  Left deprecated -> Left $ "unsupported era: " ++ show deprecated
  Right era -> obtainCommonConstraints era $ do
    let txBodyContent = Exp.defaultTxBodyContent
          & Exp.setTxIns [(txIn, AnyKeyWitnessPlaceholder)]
          & Exp.setTxOuts [Exp.TxOut $ toShelleyTxOutAny sbe txOut]
          & Exp.setTxFee fee
    unsignedTx <- first show $ makeUnsignedTx era txBodyContent
    let witVKey = makeKeyWitness era unsignedTx (WitnessPaymentKey signingKey)
    case signTx era [] [witVKey] unsignedTx of
      SignedTx signedLedgerTx -> Right $ ShelleyTx sbe signedLedgerTx
```

Use monadic `Either` (via `first` from `Data.Bifunctor`) to flatten the `makeUnsignedTx` error case instead of nesting `case` expressions.
`signTx` has a single constructor `SignedTx` so it always succeeds - a simple `case` suffices.

### Key API functions

```haskell
makeUnsignedTx :: Era era -> TxBodyContent (LedgerEra era) -> Either MakeUnsignedTxError (UnsignedTx (LedgerEra era))
makeKeyWitness :: HasCallStack => Era era -> UnsignedTx (LedgerEra era) -> ShelleyWitnessSigningKey -> L.WitVKey L.Witness
signTx         :: Era era -> [L.BootstrapWitness] -> [L.WitVKey L.Witness] -> UnsignedTx (LedgerEra era) -> SignedTx era
```

## Converting back to `Tx era`

The rest of the pipeline may still consume `Tx era` (the old type).
Convert from `SignedTx era` back to `Tx era` by unwrapping:

```haskell
case signTx era [] witVKeys unsignedTx of
  SignedTx signedLedgerTx -> ShelleyTx sbe signedLedgerTx
```

Use `case` rather than a let-binding pattern for `SignedTx` to help GHC unify `LedgerEra era` with `ShelleyLedgerEra era` under `obtainCommonConstraints`.

## Witness types

| Old | New | Notes |
|-----|-----|-------|
| `KeyWitness KeyWitnessForSpending` | `AnyKeyWitnessPlaceholder` | Placeholder during body construction; actual key witnesses added at signing |
| `ScriptWitness ScriptWitnessForSpending (PlutusScriptWitness ...)` | `AnyPlutusScriptWitness (...)` | See ADR-010 |
| `Witness WitCtxTxIn era` | `AnyWitness era` | Unified witness type |

For incremental migration, use `legacyWitnessConversion` (exported from `Cardano.Api.Experimental`) to convert old-style witnesses.

## Plutus script witnesses

The experimental API replaces `ScriptInAnyLang` with `AnyPlutusScript era`, which wraps `PlutusScriptInEra lang era` existentially.
`PlutusScriptInEra` uses `PlutusRunnable` internally - scripts are validated at deserialisation time against the era's protocol version, not deferred to submission.

### Decoding a Plutus script from `ScriptInAnyLang`

When migrating code that receives a `ScriptInAnyLang` (e.g. from file deserialisation), decode it into `AnyPlutusScript` using `decodePlutusRunnable` from `Cardano.Ledger.Plutus.Language`:

```haskell
import qualified Cardano.Ledger.Plutus.Language as L
import Cardano.Api.Experimental (eraProtVerHigh, toPlutusSLanguage, obtainCommonConstraints)
import Cardano.Api.Experimental.Plutus (AnyPlutusScript (..), plutusScriptInEraSLanguage)
import qualified Cardano.Api.Experimental as Exp (PlutusScriptInEra (..))

anyPlutusScript <- obtainCommonConstraints era $
  case script of
    ScriptInAnyLang _lang (PlutusScript version (PlutusScriptSerialised sbs)) -> do
      let slang = toPlutusSLanguage version
          decode :: forall l. (L.PlutusLanguage l, Typeable l)
                 => L.SLanguage l -> IO (AnyPlutusScript (ShelleyLedgerEra ConwayEra))
          decode _ = case L.decodePlutusRunnable @l (eraProtVerHigh era) (L.Plutus (L.PlutusBinary sbs)) of
            Left err -> throwIO $ userError $ "script decode failed: " ++ show err
            Right runnable -> pure $ AnyPlutusScript (Exp.PlutusScriptInEra runnable)
      obtainLangConstraints slang $ decode slang
    _ -> throwIO $ userError "expected a Plutus script"
```

Key points:
- `toPlutusSLanguage` converts `PlutusScriptVersion` to `L.SLanguage lang`.
- `obtainLangConstraints` brings `PlutusLanguage lang` and `Typeable lang` into scope from `SLanguage lang`.
  Exported from `Cardano.Api.Experimental`.
- The local `decode` helper with `forall l` + `@l` type application is needed to connect the existential language type variable through `decodePlutusRunnable` to the `AnyPlutusScript` constructor.
  Without it, GHC cannot resolve the ambiguous `lang` type variable.

### Building a Plutus script witness

The old API used `PlutusScriptWitness` with `ScriptDatumForTxIn`:

```haskell
-- Old
ScriptWitness ScriptWitnessForSpending
  $ PlutusScriptWitness scriptLang version (PScript script')
      (ScriptDatumForTxIn $ Just datum) redeemer budget
```

The new API uses `Exp.PlutusScriptWitness` with `PlutusScriptDatum`, then wraps it in `AnyPlutusScriptWitness`:

```haskell
-- New
let slang = plutusScriptInEraSLanguage ps
    datum = SpendingScriptDatum dummyDatum   -- V1/V2: bare datum; V3/V4: wrap with Just
    witness = Exp.PlutusScriptWitness slang (Exp.PScript ps) datum redeemer budget
in AnyPlutusScriptWitness
     (AnyPlutusSpendingScriptWitness (createPlutusSpendingScriptWitness slang witness))
```

### `PlutusScriptDatum` and CIP-69

The `PlutusScriptDatumF` type family changes the datum type depending on the Plutus version:
- V1/V2: `SpendingScriptDatum :: HashableScriptData -> PlutusScriptDatum lang SpendingScript`
- V3/V4 (CIP-69): `SpendingScriptDatum :: Maybe HashableScriptData -> PlutusScriptDatum lang SpendingScript`

Construct `SpendingScriptDatum` directly, adjusting for the Plutus version:

```haskell
-- V1/V2
SpendingScriptDatum datum

-- V3/V4 (CIP-69)
SpendingScriptDatum (Just datum)
```

The `PlutusScriptDatumF` type family resolves `SpendingScriptDatum` differently per version, so GHC enforces the correct wrapping at compile time.

## Protocol parameters

### Unwrapping `LedgerProtocolParameters`

The experimental API uses `L.PParams era` directly, not wrapped in `BuildTxWith`:

```haskell
-- Old
& setTxProtocolParams (BuildTxWith (Just ledgerParameters))

-- New
& Exp.setTxProtocolParams (unLedgerProtocolParameters ledgerParameters)
```

Or destructure in the function pattern:

```haskell
genTx era (LedgerProtocolParameters pparams) ... =
  ...
  & Exp.setTxProtocolParams pparams
```

### When protocol parameters are needed

Protocol parameters are required when the transaction includes Plutus scripts (for the script integrity hash).
For key-only transactions, `setTxProtocolParams` is functionally inert but should still be wired in for correctness and future-proofing.

## Fee estimation

| Old | New |
|-----|-----|
| `evaluateTransactionFee sbe pp txBody keyWitCount byronWitCount refScriptSize` | `Exp.evaluateTransactionFee pp unsignedTx keyWitCount byronWitCount refScriptSize` |

The experimental version takes `UnsignedTx` directly (not `TxBody`), so `getTxBody` is no longer needed.

For full auto-balancing, use `makeTransactionBodyAutoBalance` from `Cardano.Api.Experimental.Tx`.

## Bridge functions for incremental migration

| Function | Purpose |
|----------|---------|
| `sbeToEra :: MonadError (DeprecatedEra era) m => ShelleyBasedEra era -> m (Era era)` | Convert era witness |
| `convertTxBodyToUnsignedTx :: ShelleyBasedEra era -> TxBody era -> UnsignedTx (LedgerEra era)` | Convert old TxBody to UnsignedTx |
| `legacyWitnessConversion` | Convert old-style witnesses to new `AnyWitness` |
| `toShelleyTxOutAny :: ShelleyBasedEra era -> TxOut ctx era -> L.TxOut (ShelleyLedgerEra era)` | Convert old TxOut to ledger TxOut (then wrap in `Exp.TxOut`) |

## Deprecated symbols reference

| Symbol | Replacement |
|--------|-------------|
| `createTransactionBody` | `makeUnsignedTx` from `Cardano.Api.Experimental` |
| `signShelleyTransaction` | `makeKeyWitness` + `signTx` |
| `getTxBody` | Use `UnsignedTx` directly |
| `TxBody` / `ShelleyTxBody` | `UnsignedTx` from `Cardano.Api.Experimental` |
| `defaultTxBodyContent sbe` (old) | `Exp.defaultTxBodyContent` (no args) |
| `BalancedTxBody` | `makeTransactionBodyAutoBalance` returns `(UnsignedTx, TxBodyContent)` |
| `getTxBodyContent` | Access `TxBodyContent` fields directly |
| `ProtocolParameters` (Cardano.Api) | `L.PParams era` from ledger |

## Error handling

### `makeUnsignedTx` errors

Use `Either` to handle `makeUnsignedTx` failures instead of `fromRight (error ...)`.
Always include the error in the message:

```haskell
-- Bad: loses the error
unsignedTx = fromRight (error "failed to create tx") $ makeUnsignedTx era txBodyContent

-- Good: preserves the error
unsignedTx = either (\err -> error $ "failed to create tx: " ++ show err) id $ makeUnsignedTx era txBodyContent

-- Better: in IO, use throwIO
unsignedTx <- either (\err -> throwIO $ userError $ "failed: " ++ show err) pure
                $ makeUnsignedTx era txBodyContent
```

### `evaluate` vs `catch` pattern

The old API used `evaluate` to force a pure expression and catch exceptions:

```haskell
-- Old
evaluate $ summary { projectedTxSize = Just $ txSizeInBytes dummyTx }
  `catch` \(SomeException e) -> ...
```

The new API is monadic (because `makeUnsignedTx` returns `Either`), so use a `do` block wrapped in `catch`:

```haskell
-- New
obtainCommonConstraints era (do
  unsignedTx <- either (\err -> throwIO ...) pure $ makeUnsignedTx era txBodyContent
  ...
  pure summary { projectedTxSize = ... }
  ) `catch` \(SomeException e) -> ...
```

## Gotchas

- **`makeUnsignedTx DijkstraEra` is `error "TODO Dijkstra"`.**
  Only ConwayEra is implemented in the experimental API today.
  Your code will compile for Dijkstra but crash at runtime until cardano-api fills in the implementation.

- **`obtainCommonConstraints` is needed for `LedgerEra era ~ ShelleyLedgerEra era`.**
  Without it, GHC cannot unify the type families, and `ShelleyTx sbe signedLedgerTx` will not type-check.

- **Use `case` not let-bindings for `SignedTx` pattern matching.**
  A let-binding `let SignedTx x = signTx ...` creates a rigid type variable that GHC cannot unify.
  Use `case signTx era [] wits unsignedTx of SignedTx ledgerTx -> ...` instead.

- **`MakeUnsignedTxMissingProtocolParams` error.**
  If your transaction includes Plutus scripts, you must set `Exp.setTxProtocolParams pparams`.
  Without protocol parameters, `makeUnsignedTx` cannot compute the script integrity hash.

- **Always use `useEra @era` with a type application, not bare `useEra`.**
  Without `@era`, GHC 9.6 infers an ambiguous type variable `era0` disconnected from the `era` in the type signature.
  GHC 9.12 is more lenient and may accept bare `useEra`, but CI runs GHC 9.6.
  The function must have `ScopedTypeVariables` and a `forall era.` binding in the type signature.

- **Ambiguous type variables with existential `AnyPlutusScript` decoding.**
  `AnyPlutusScript` hides the `lang` type variable.
  When calling `decodePlutusRunnable @l`, the type must be connected via a local helper with explicit `forall l` and `@l` type application.
  See the decoding example above.

- **Name collisions between old and new APIs.**
  Both export `defaultTxBodyContent`, `setTxIns`, `setTxOuts`, `setTxFee`, `TxOut`, etc.
  Import the experimental module qualified (`as Exp`) and hide conflicting names from `Cardano.Api` if needed.

- **`eraProtVerHigh` name collision.**
  Both `Cardano.Api` and `Cardano.Api.Experimental` export `eraProtVerHigh`.
  Hide it from the old API: `import Cardano.Api hiding (eraProtVerHigh)`.

- **`TxMetadata` vs `TxMetadataInEra`.**
  The new API uses `TxMetadata` directly.
  Convert: `TxMetadataNone` -> `mempty`, `TxMetadataInEra _ m` -> `m`.

- **`Exp.TxOut` wraps a ledger `L.TxOut era`, not the old `TxOut CtxTx era`.**
  Use `toShelleyTxOutAny sbe` to convert from old to ledger, then wrap in `Exp.TxOut`.

- **Use `Exp.TxOut (LedgerEra era)` in function signatures, not `Exp.TxOut era`.**
  `makeUnsignedTx` expects `TxBodyContent (LedgerEra era)`, so `Exp.setTxOuts` must receive `[Exp.TxOut (LedgerEra era)]`.
  Passing `[Exp.TxOut era]` forces `TxBodyContent era` which does not unify with `TxBodyContent (LedgerEra era)` - GHC cannot deduce `era ~ LedgerEra era` even inside `obtainCommonConstraints`.
  The constraint `ShelleyLedgerEra era ~ LedgerEra era` (from `EraCommonConstraints`) makes `toShelleyTxOutAny` output unify correctly with `LedgerEra era`.

- **`toShelleyTxOutAny` needs `IsShelleyBasedEra` - scope it inside `obtainCommonConstraints`.**
  `IsEra` does not imply `IsShelleyBasedEra`, so `shelleyBasedEra` and `toShelleyTxOutAny` are not available at the function signature level.
  Place the conversion inline in the body (inside `obtainCommonConstraints`), not in a `where` clause outside it.

- **Old `PlutusScript lang` to `PlutusScriptInEra lang era` conversion.**
  Use `deserialisePlutusScriptInEra sLang (serialiseToCBOR oldScript)`.
  This round-trips through CBOR but is the only stable conversion path when `PlutusRunnable` is not directly accessible from the old `PlutusScript`.

- **`cardano-ledger-core` dependency may be avoidable.**
  `obtainLangConstraints` and `decodeAnyPlutusScript` are now exported from `Cardano.Api.Experimental`.
  `decodeAnyPlutusScript :: L.Era era => ByteString -> AnyPlutusScriptLanguage -> Either CBOR.DecoderError (AnyPlutusScript era)` provides a simpler alternative to the manual `decodePlutusRunnable` approach shown above.
  Direct use of `Cardano.Ledger.Plutus.Language` is only needed for lower-level control.
