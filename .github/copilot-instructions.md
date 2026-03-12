# GitHub Copilot Instructions

This file provides hints and context for GitHub Copilot to better understand intentional
patterns in this codebase. Do not flag the items listed here as issues during code review.

## Known Intentional Patterns

### Dual-qualified imports of the same module

In test modules and some source files, `Cardano.Api.Ledger` (and similar modules) may be
imported under two different qualifiers, e.g.:

```haskell
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Ledger qualified as Ledger
```

This is intentional: `L` is used for brevity in complex expressions while `Ledger` is used
where the longer name aids readability. Do not suggest consolidating these into a single
qualifier.
