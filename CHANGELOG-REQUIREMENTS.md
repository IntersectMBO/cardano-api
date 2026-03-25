# Changelog tooling requirements

## Context

This document captures requirements for replacing the in-house release scripts
(`download-prs.sh`, `generate-pr-changelogs.sh`) with a file-based changelog tool.

See: https://github.com/input-output-hk/cardano-dev/issues/17

## Current process (status quo)

- Contributors embed a YAML block in the PR description with `description`, `type`, and `projects`
- At release time, a maintainer downloads all PRs from GitHub and generates markdown
- Version bumps in `.cabal` files are manual
- A separate `tag.sh` script handles tagging with safety checks

### Pain points

- Downloading all PRs is slow and requires network access
- Depends on contributors correctly formatting YAML in PR descriptions (easy to get wrong)
- Requires `jq`, `yq`, `gh` tooling
- Custom scripts to maintain

## Requirements

### R1: File-based changelog fragments

Each PR must include a changelog fragment file committed to the repo.
This avoids parsing PR descriptions and eliminates merge conflicts on `CHANGELOG.md`.

### R2: Multiple packages (mono-repo)

The repo contains multiple packages, each with its own `CHANGELOG.md` and `.cabal` file:
- `cardano-api`
- `cardano-api-gen`
- `cardano-rpc`
- `cardano-wasm`

A fragment must be associated with one or more packages.
Release (batching + changelog assembly) is done per-package independently.

### R3: PVP versioning with auto-bumping (four-part: A.B.C.D)

Versions follow [Haskell PVP](https://pvp.haskell.org/), not SemVer.
The tool must accept and produce 4-part version strings like `10.25.0.0` or `0.0.0.1`.

Auto-bumping must be supported, based on the most significant kind across all unreleased
fragments for a package. The mapping from kinds to PVP digits is:

| Kind | PVP bump | Example (from `8.4.1.2`) |
|------|----------|--------------------------|
| `breaking` | Bump 2nd digit (A.**B**.0.0) | `8.5.0.0` |
| `feature`, `compatible` | Bump 3rd digit (A.B.**C**.0) | `8.4.2.0` |
| `bugfix`, `optimisation` | Bump 4th digit (A.B.C.**D**) | `8.4.1.3` |
| non-notable only | Bump 4th digit (A.B.C.**D**) | `8.4.1.3` |

The highest-significance kind wins. For example, if fragments contain both `breaking` and
`bugfix`, the 2nd digit is bumped.

The release workflow should support both:
- **Auto mode** — compute the next version from the current version + fragment kinds
- **Explicit mode** — maintainer specifies the exact version (e.g., `9.0.0.0` for a major Cardano release)

The current version is determined from the latest git tag matching `<package>-*` (e.g., `cardano-api-8.4.1.2`).

### R4: Multiple kinds per entry (multi-select)

Each changelog entry can have multiple kinds (comma-separated). Available kinds:

| Kind | Appears in changelog |
|------|---------------------|
| `breaking` | yes |
| `feature` | yes |
| `compatible` | yes |
| `bugfix` | yes |
| `optimisation` | yes |
| `refactoring` | no |
| `test` | no |
| `maintenance` | no |
| `release` | no |
| `documentation` | no |

### R5: Non-notable kinds are hidden in changelog

Entries where all kinds are non-notable (refactoring, test, maintenance, release, documentation)
must not appear in the rendered changelog. If at least one notable kind is present, the entry is visible.

### R6: PR number is recorded and validated

Each fragment records the PR number. CI validates that the PR number in the fragment matches
the actual PR number.

### R7: Changelog fragments are mandatory

Every PR must include a fragment. There is no opt-out mechanism (no `no-changelog` label).

### R8: Automated release PR via GitHub Actions

A workflow_dispatch action takes a package name and an optional PVP version string, then:
1. Determines the next version — either from the explicit input or auto-computed from
   the current git tag + the most significant kind in unreleased fragments (see R3)
2. Collects unreleased fragments for that package
3. Generates the version's changelog section
4. Updates the `version:` field in the package's `.cabal` file
5. Opens a PR with the changes

### R9: `.cabal` version replacement on release

When batching a release, the `version:` field in the relevant `.cabal` file is automatically
updated to the new version.

### R10: Changelog output format

The rendered changelog should match the existing format:

```markdown
## 10.25.0.0 — 2026-03-25

- Description of the change
  (bugfix, compatible)
  [PR 1234](https://github.com/IntersectMBO/cardano-api/pull/1234)

- Another change
  (feature)
  [PR 1235](https://github.com/IntersectMBO/cardano-api/pull/1235)
```

## Constraints

### C1: Changie PVP limitation

Changie (`changie batch`) only accepts SemVer (3-part) versions. It rejects 4-part PVP versions
like `0.0.0.1` with: `part string is not a supported version or version increment`.

This is a blocking issue for direct use of `changie batch`.

### C2: Changie kind selection is single-select

Changie's built-in `kinds` only allows selecting one kind per fragment.
Multi-select requires using a custom string field instead of the built-in kind mechanism.

## Open questions

1. Should we work around changie's PVP limitation (wrapper script, patching output), or
   consider a different tool / custom solution that natively supports PVP?
2. Is the existing `tag.sh` script kept as-is, or should tagging also be incorporated?
