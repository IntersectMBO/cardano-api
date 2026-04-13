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

## Breaking Change Detection (CRITICAL)

When reviewing PRs, you MUST rigorously check for breaking changes and verify they are correctly classified in the changelog fragment (`.changes/*.yml`). This is the single most important aspect of PR review for this project. cardano-api is a foundational library -- downstream consumers (cardano-cli, cardano-node, dApps, tooling) break silently when we get this wrong.

### What constitutes a breaking change

Any of the following in an **exposed module** is a breaking change. Exposed modules are those listed under `exposed-modules:` in the `library` stanza of a `.cabal` file. Changes to `other-modules:` (internal modules) are NOT breaking unless they affect the behaviour of exposed API.

#### Type-level breaks
- Removing or renaming an exported type, data constructor, type class, or type family
- Adding a field to a record that uses non-record syntax (positional constructors)
- Adding a required field to a record constructor (even with named fields)
- Removing a field from any data constructor
- Changing the type of an existing field or function argument
- Changing a type synonym or type family equation
- Narrowing a type signature (making it less polymorphic) on an exported function
- Adding a new constraint to an exported function's type signature
- Changing the kind of a type variable
- Removing a type class instance that was previously exported

#### Value-level breaks
- Removing or renaming an exported function or value
- Changing the number or order of arguments to an exported function
- Changing default values or smart constructor behaviour in ways that alter semantics
- Changing the semantics of an exported function (different return value for same input) -- this is a **behavioural** break even if the type signature is unchanged

#### Module-level breaks
- Removing an exposed module entirely
- Removing re-exports from an exposed module (e.g. a module that re-exported `Foo(..)` no longer does)
- Moving a definition from an exposed module to an internal module without re-exporting it

#### Dependency-level breaks
- Bumping a major version of a dependency that is transitively exposed in the public API (e.g. types from `cardano-ledger-api` appear in cardano-api's signatures)
- Removing a dependency whose types appear in the public API

#### Things that are NOT breaking
- Adding new exported functions, types, or modules (this is `feature` or `compatible`)
- Adding new type class instances (unless orphans that could cause overlap)
- Adding optional fields with defaults to records using `HasField` / lenses
- Changes to internal modules (`Cardano.Api.Internal.*`, `other-modules:`) that do not change exposed behaviour
- Documentation, test, or refactoring changes with no API surface impact
- Performance improvements with identical semantics

### Verifying the changelog fragment

Every PR must include a `.changes/*.yml` file. When reviewing:

1. **Check the `kind:` field** -- if the PR contains ANY breaking change from the list above, the kind MUST include `breaking`. A PR marked as `feature`, `compatible`, `bugfix`, or anything else that introduces a breaking change is **incorrectly classified** and must be flagged.

2. **Check the `project:` field** -- the project must match whichever package's exposed API changed. The projects are: `cardano-api`, `cardano-api-gen`, `cardano-rpc`, `cardano-wasm`.

3. **Check the `description:`** -- for breaking changes, the description MUST clearly state what broke and what downstream consumers need to do to adapt. Vague descriptions like "update API" are not acceptable for breaking changes.

4. **If no changelog fragment exists**, flag the PR as incomplete. Every PR requires one.

5. **If multiple packages are affected**, there should be separate changelog fragments for each, or a single fragment listing multiple projects.

### How to review for breaking changes

When reviewing a diff:

1. First, identify which files are in exposed modules by cross-referencing with the `exposed-modules:` list in the relevant `.cabal` file.
2. For each changed exposed module, check every removed or modified line against the breaking change categories above.
3. Pay special attention to:
   - Removed exports in module headers (`module Foo (thing1, thing2, ...)`)
   - Changed type signatures
   - Modified data type definitions
   - Removed or renamed pattern synonyms
   - Changes to re-export modules like `Cardano.Api` (the main entry point re-exports heavily)
4. If a change touches `Cardano.Api` or `Cardano.Api.Experimental` (the main re-export modules), scrutinize especially carefully -- these are the most widely imported modules.

### Err on the side of caution

When uncertain whether something is breaking:
- **Flag it as potentially breaking** and ask the author to confirm
- It is far worse to miss a breaking change than to over-flag one
- A false positive costs a conversation; a false negative costs every downstream consumer
