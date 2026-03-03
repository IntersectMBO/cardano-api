# Versioning and release process (changie-based)

This document describes the release process using [changie](https://changie.dev/) for
changelog management. It replaces the `cardano-dev`-script-based process documented in
`RELEASING.md`.

## Overview of changes vs the old process

| Aspect | Old process | New process |
|--------|------------|-------------|
| Changelog source | PR description YAML block | Per-PR fragment YAML file in `.changes/unreleased/` |
| Changelog assembly | `generate-pr-changelogs.sh` (downloads all PRs from GitHub) | `changie batch` (reads local files, no network) |
| Version bump | Manual edit of all cabal files | `changie batch` applies regex replacements automatically |
| CI check | Parses YAML from PR body | Checks for presence and validity of fragment file |
| Release PR | Manual branch + manual edits | `create-release-pr` workflow dispatch |

---

## Developer workflow (PR authors)

Every PR that changes user-visible behaviour must include a changie fragment file.

### 1. Install changie

```bash
# macOS
brew install changie

# Linux / other — download binary from GitHub releases
# https://github.com/miniscruff/changie/releases
```

Or use the Nix devshell if it provides changie.

### 2. Create a fragment

From the repo root, run:

```bash
changie new --project <package>
```

Changie will prompt for:
- **Project** — the affected package (`cardano-api`, `cardano-api-gen`, `cardano-rpc`, or `cardano-wasm`)
- **Kind** — one of the kinds listed below
- **Body** — a short description of the change (what and why, not how)
- **PR Number** — the GitHub PR number (you can add this after opening the PR)

The command creates a file such as:
```
.changes/unreleased/cardano-api-20240303-153012.yaml
```

### 3. Commit the fragment

```bash
git add .changes/unreleased/
git commit -m "chore: add changelog fragment"
```

### Changelog kinds reference

| Kind | When to use |
|------|-------------|
| `breaking` | Removed or changed exported API in a backwards-incompatible way |
| `feature` | New exported function, type, or behaviour |
| `compatible` | Changed API in a backwards-compatible way (e.g., added a field) |
| `bugfix` | Fixed incorrect behaviour |
| `optimisation` | Measurable performance improvement with no API change |

Use the `no-changelog` label on a PR to skip the fragment requirement (suitable for
documentation-only changes, CI/tooling maintenance, or test-only PRs).

---

## Version bumping

`cardano-api` uses [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/)
with a four-part version `A.B.C.D`.

To decide the next version, find the current released version (CHaP is the most
reliable source: https://chap.intersectmbo.org/index.html), then apply the
[PVP decision tree](https://pvp.haskell.org/#decision-tree):

| Change type | Digit to bump | Example (from `10.24.1.0`) |
|-------------|--------------|--------------------------|
| Breaking API change | B | `10.25.0.0` |
| New backwards-compatible API | C | `10.24.2.0` |
| Backwards-compatible bug fix | D | `10.24.1.1` |
| Major Cardano release | A | `11.0.0.0` |

> **Note:** `changie batch auto` is not applicable here because changie's auto-bumping
> uses SemVer semantics. Always specify the version explicitly as a PVP string.

---

## Release workflow

The release PR is created automatically by a GitHub Actions workflow dispatch.

### Trigger the workflow

1. Go to **Actions → Create Release PR** in the GitHub UI.
2. Click **Run workflow**.
3. Fill in:
   - **Package** — the package to release (e.g., `cardano-api`)
   - **Version** — the PVP version string (e.g., `10.25.0.0`)
4. Click **Run workflow**.

The workflow will:
1. Run `changie batch <version> --project <package>` — aggregates all unreleased
   fragments for that package into a versioned YAML file and updates the `version:`
   field in the corresponding `.cabal` file.
2. Run `changie merge --project <package>` — prepends the new version section to the
   package's `CHANGELOG.md`.
3. Open a PR on branch `release/<package>-<version>` with the updated CHANGELOG and
   cabal file.

### Review the release PR

The release PR should normally contain only:
- An updated `<package>/CHANGELOG.md` with the new version section
- An updated `<package>/<package>.cabal` with `version: <new-version>`

If the PR contains unexpected code changes, investigate before merging.

> **Tip:** Avoid rebasing the release PR unnecessarily. Merge via merge queue with an
> explicit merge commit.

> **Tip:** Hold off on tagging and merging until the CHaP PR is merged (see below).

---

## After the release PR is merged

These steps are identical to the old process.

### 1. Publish to `cardano-haskell-packages` (CHaP)

**After verifying the release PR diff**, upload to CHaP:

1. Clone `cardano-haskell-packages` (first time only):
   ```bash
   git clone https://github.com/IntersectMBO/cardano-haskell-packages
   cd cardano-haskell-packages
   ```

2. Run the add-from-github script, replacing `<commit-hash>` with the merge commit:
   ```bash
   ./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-api <commit-hash> cardano-api
   ```
   List all packages being released after the commit hash if releasing multiple at once.

3. Push to a new branch and open a PR in CHaP.

4. Merge the CHaP PR (no additional approvals needed if you have the right access).

### 2. Tag the release

After successful CHaP CI, tag the release commit:

1. Ensure your `HEAD` is on the exact commit released to CHaP.
2. Create and push the tag:
   ```bash
   git tag cardano-api-10.25.0.0 HEAD
   git push origin cardano-api-10.25.0.0
   ```
   The tag must match the version in the CHANGELOG (CI may enforce this).

### 3. GitHub release (if pipeline configured)

If the repo has a release pipeline, it triggers on tag push. If the
_Wait for Hydra check-runs_ step fails, see [Troubleshooting](#troubleshooting) below.

After CI completes, undraft the GitHub release via the UI (not the API — see the
warning in `RELEASING.md`).

---

## Backporting

To release a patch for an older version:

1. Identify the affected release tags.

2. Create a branch from the target tag:
   ```bash
   git checkout -b release/cardano-api-10.14.2.0 cardano-api-10.14.1.0
   ```

3. Cherry-pick the fix commit(s) onto the branch.

4. Add a changie fragment manually (edit a `.changes/unreleased/*.yaml` file directly,
   or run `changie new` on the branch):
   ```yaml
   kind: bugfix
   body: "Description of the backported fix"
   custom:
     PR: 1234
   ```

5. Run changie batch and merge on the backport branch:
   ```bash
   changie batch 10.14.2.0 --project cardano-api
   changie merge --project cardano-api
   ```

6. Open a PR from this branch targeting the appropriate release branch (not `main`).

7. Follow the normal CHaP and tagging steps after the PR merges.

---

## Troubleshooting

### Cabal cache invalidation

If you update a `.cabal` file (e.g., after a version bump), CI caches keyed on cabal
files may need to be invalidated. Rerunning the workflow usually suffices; if not,
clear the cache manually in the GitHub Actions cache UI.

### Hydra race condition

If the release pipeline fails at _Wait for Hydra check-runs_, Hydra may not have run
on the tagged commit (this happens if the tag was not the remote `HEAD` at the time of
the PR, or if the tag was changed after the fact).

To force Hydra to pick up the tagged commit:
```bash
git checkout <tagged-commit>
git checkout -b ci/<whatever>
git push origin ci/<whatever>
```
Then retrigger the release workflow:
```bash
gh workflow run "Release Upload" -r <current-branch> -f target_tag=cardano-api-10.25.0.0
```
