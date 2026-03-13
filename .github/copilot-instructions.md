# GitHub Copilot Custom Instructions

## Haskell-Specific Guidance

### Qualified Import Aliases

In Haskell, it is valid and intentional to have multiple modules imported with the same qualified alias. For example:

```haskell
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
```

This is valid GHC Haskell — both modules can be referenced via the `Exp.` prefix. Name resolution happens at the use site; ambiguity only arises if both modules export the same identifier and it is used without disambiguation. Do **not** flag this pattern as a compile error or suggest renaming one of the aliases unless there is an actual ambiguity at a specific use site.
