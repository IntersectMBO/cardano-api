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

## Tool survey

No existing tool satisfies all requirements out of the box. The core problem is that every
tool with auto version bumping is hardwired to SemVer (3-part versions).

### Evaluated tools

| Tool | 4-part PVP | Fragments | Mono-repo | Multi-kind | Hidden kinds | Custom format | Auto bump | nixpkgs |
|---|---|---|---|---|---|---|---|---|
| **Towncrier** | yes (pass-through) | yes | yes | multi-file | yes (`showcontent`) | yes (Jinja2) | no | yes |
| **Scriv** | yes (pass-through) | yes | no | yes (in-file) | partial | yes (Jinja2) | no | no |
| **Reno** | yes (pass-through) | yes | no | yes (YAML) | no | limited (RST) | no | yes |
| **Changie** | no (rejects) | yes | yes | no | workaround | yes (Go tmpl) | SemVer only | yes |
| **git-cliff** | no (bump SemVer) | no | yes | n/a | n/a | yes (Tera) | SemVer only | yes |
| **Knope** | no | yes | yes | yes | ? | limited | SemVer only | no |
| **Changesets** | no | yes | yes | yes | no | yes | SemVer only | no |
| **release-please** | custom code needed | no | yes | n/a | n/a | limited | SemVer only | no |

### Towncrier — best candidate

Meets R1, R2, R5, R7, R10 natively:
- File-based fragments (core design)
- Mono-repo with per-package changelogs (via `[[tool.towncrier.section]]`)
- Hidden kinds via `showcontent = false` per fragment type
- Any version string (version-agnostic — never parses versions)
- Fully customizable output via Jinja2 templates
- Available in nixpkgs (`python3Packages.towncrier`)

**Gaps:**
- **R3 (auto PVP bumping):** Towncrier does not bump versions at all. A custom script is needed
  to read fragment kinds, determine the highest-significance kind, and compute the next PVP
  version from the latest git tag.
- **R4 (multi-kind per entry):** Towncrier encodes the kind in the fragment filename
  (`123.bugfix.md`). A PR with multiple kinds creates multiple fragment files
  (e.g., `123.bugfix.md` + `123.refactoring.md`). This works but is not single-file multi-select.
- **R9 (`.cabal` replacement):** Towncrier does not modify `.cabal` files. The release script
  must handle this.

### Changie — partial fit, blocking limitations

Meets R1, R2, R10 natively. Has mono-repo projects and `.cabal` replacements built in.

**Blockers:**
- **R3:** Rejects 4-part PVP versions (`changie batch 0.0.0.1` fails with
  `part string is not a supported version or version increment`).
- **R4:** Built-in `kinds` is single-select only. Workaround: use a custom string field,
  losing the interactive kind picker.
- **R3 (auto bump):** Only supports SemVer `major`/`minor`/`patch`/`auto`.

### Other tools — ruled out

- **git-cliff:** No file-based fragments (commit-based). SemVer-only bumping.
- **Knope, Changesets:** SemVer-only. Not in nixpkgs.
- **release-please:** No fragments. SemVer-only. Requires custom TypeScript for PVP.
- **Scriv:** No mono-repo support. Not in nixpkgs.
- **Reno:** No mono-repo support. RST-centric output.

## Conclusion

Every approach requires a custom PVP version-bumping script — no tool provides this.
The choice is which tool handles the fragment-to-changelog pipeline:

| Approach | Pros | Cons |
|----------|------|------|
| **Towncrier + PVP script** | Battle-tested, Jinja2 templates, native hidden kinds, in nixpkgs | Multi-kind = multi-file, no `.cabal` replacement |
| **Changie + wrapper** | Mono-repo projects, `.cabal` replacement, Go templates | Must work around SemVer-only `batch`, must work around single-select kinds |
| **Custom script only** | Full control, PVP-native, no tool limitations | More code to write and maintain |

## Open questions

1. Which approach to take: towncrier-based, changie-with-workarounds, or fully custom?
2. Is the existing `tag.sh` script kept as-is, or should tagging also be incorporated?
