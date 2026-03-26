# GitHub Copilot Custom Instructions

## Qualified Import Aliases

Multiple Haskell modules may share the same qualified alias:

```haskell
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
```

This is valid GHC Haskell — both modules are accessible via the `Exp.` prefix. Name resolution happens at the use site; ambiguity only arises if both modules export the same identifier and it is used without disambiguation. Do **not** flag this pattern as a compile error or suggest renaming an alias unless there is an actual ambiguity at a specific use site.

## Cabal File Formatting

In `.cabal` files, all top-level package metadata fields (`name:`, `version:`, `synopsis:`, `description:`, `copyright:`, `author:`, `maintainer:`, `license:`, `build-type:`, etc.) are written at column 0 with no leading indentation. This is the standard Cabal format. Do **not** flag these fields as having inconsistent indentation.

When reviewing diffs of `.cabal` files, do **not** confuse diff context annotations (e.g., line numbers or `|` separators) with actual file content. Only comment on the content of the file itself, not on artefacts of the diff format.

## `cabal.project` sha256 Comments

`source-repository-package` stanzas in `cabal.project` may contain `--sha256:` comment lines:

```cabal
source-repository-package
  type: git
  location: https://github.com/some/package
  tag: abc123...
  --sha256: sha256-AAAA...=
```

These lines are **comments** (ignored by cabal) but are consumed by Nix tooling to fetch the package source. The hash is computed using a different fetch method than the `sha256` in `flake.nix` (which uses `fetchgit`), so the two values will **intentionally differ**. Do **not** flag a mismatch between a `--sha256:` comment in `cabal.project` and the corresponding `sha256` in `flake.nix` as an error.
