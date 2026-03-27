# Versioning and release process

## Prerequisites

Install `herald` from the `cardano-dev` repository:

```bash
nix run github:input-output-hk/cardano-dev#herald -- --help
```

Alternatively, enter the `cardano-api` nix-shell where herald is available:

```bash
nix develop
```

## Day-to-day: adding changelog fragments

Every PR must include a changelog fragment in `.changes/`. Create one with:

```bash
herald new
```

This launches interactive mode, prompting for project, change kind, description, and PR number. You can also pass all fields directly:

```bash
herald new -p cardano-api -k breaking -d "Remove deprecated API" --pr 1234
```

CI validates fragments automatically via `herald validate --diff --pr <N>`, which checks that:

- every modified project has at least one fragment
- fragments are well-formed YAML matching `.herald.yml`
- the PR number in each new fragment matches the PR

## Release process

### 1. Preview the next version

Before starting a release, check what version herald would compute:

```bash
herald next cardano-api
```

This reads the current `.cabal` version, applies the highest bump from unreleased fragments, and prints the result.

### 2. Version bumping

`cardano-api` follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/). Herald computes the version bump automatically based on fragment kinds:

- `breaking` -- bump 2nd component (e.g. 8.4.1.2 -> 8.5.0.0)
- `feature`, `compatible` -- bump 3rd component (e.g. 8.4.1.2 -> 8.4.2.0)
- `bugfix`, `optimisation`, and all others -- bump 4th component (e.g. 8.4.1.2 -> 8.4.1.3)

The highest bump across all fragments wins. You can override the computed version with `--version`.

For the current version, check:
- the `.cabal` file
- the latest version on [CHaP](https://chap.intersectmbo.org/index.html)
- the latest git tag

### 3. Batch: generate changelog and bump version

Create a release branch:

```bash
git checkout -b release/cardano-api-8.5.0.0
```

> A separate branch is needed for every package you are releasing.
> For example, to release both `cardano-api` and `cardano-api-gen`, create two branches.

Run herald to collect fragments, update the changelog, and bump the `.cabal` version:

```bash
herald batch cardano-api
```

This will:
1. Read all `.changes/` fragments for `cardano-api`
2. Compute the next version (or use `--version A.B.C.D` to override)
3. Prepend a new section to `cardano-api/CHANGELOG.md`
4. Update `version:` in `cardano-api/cardano-api.cabal`
5. Remove the consumed fragment files

To also create a git commit and tag in one step:

```bash
herald batch cardano-api --commit-tag
```

Or just a commit without the tag:

```bash
herald batch cardano-api --commit
```

Review the generated changelog and create a PR from the release branch back to `master`.

Make sure the release PR contains:
- updated changelogs
- bumped version fields in `.cabal` files
- removed fragment files

> Usually the release PR should only contain a changelog update and a version bump.
> If you are making a release which aims to contain everything from `master`, there should be no additional code changes.

> Hold off on tagging and merging of the release PR until the CHaP PR gets merged. See step 5.

> Avoid unnecessary rebasing of the release PR to prevent accidental inclusion of unwanted changes.
> The release PR should be merged using merge queue with an explicit merge commit.

### 4. Releasing to `cardano-haskell-packages`

**After verifying the release PR diff** that it contains the correct contents, upload to [CHaP](https://github.com/intersectmbo/cardano-haskell-packages).

Detailed description is in the [CHaP repository README](https://github.com/intersectmbo/cardano-haskell-packages#how-to-add-a-new-package-version). Briefly:

1. Clone `cardano-haskell-packages` (first time only):
    ```bash
    git clone git@github.com:IntersectMBO/cardano-haskell-packages.git
    cd cardano-haskell-packages
    ```

2. Run the following script, replacing `<commit-hash>` with the tagged commit hash:
    ```bash
    ./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-api <commit-hash> cardano-api cardano-api-gen
    ```
    List all package names after the commit hash.

3. Push to a new branch, create a PR in CHaP, and merge it.

    After the package is released, check the version at https://chap.intersectmbo.org/all-package-versions/ and update dependant packages. Don't forget to bump the CHaP index in `cabal.project` and `flake.lock` too. See [CONTRIBUTING.md](https://github.com/IntersectMBO/cardano-cli/blob/master/CONTRIBUTING.md#updating-dependencies).

> CHaP CI can fail due to various reasons (e.g. invalid haddock syntax). Tagging after CHaP merge lets you accommodate such issues.

### 5. Tagging the release version

After successful CI build in CHaP, tag and enqueue the release PR to merge.

If you used `herald batch --commit-tag`, the tag is already created locally. Push it:

```bash
git push origin cardano-api-8.5.0.0
```

Otherwise, create the tag manually:

```bash
git tag cardano-api-8.5.0.0
git push origin cardano-api-8.5.0.0
```

Make sure that:
- Your `HEAD` is on the commit you are tagging -- **this must be the same commit released to CHaP**
- Your `HEAD` is in `release/packagename-version.x` branch history on the `origin` remote

#### GitHub release pipeline

If the repo has a release pipeline configured, it is triggered on tag push.

1. If the release pipeline fails during _Get specific check run status_ / _Wait for Hydra check-runs_, Hydra did not run on the tagged commit. Create a branch starting with `ci/` from the tagged commit and push it. Then retrigger:
   ```bash
   gh workflow run "Release Upload" -r $(git branch --show-current) -f target_tag=cardano-api-8.5.0.0
   ```

2. If a GitHub release is automatically created, undraft it via the UI: click the pen, untick _Set as a pre-release_, and _Update release_.

   > **GitHub bug**: The `PATCH` API endpoint messes up release metadata (author, commit). Use the UI instead.

## Backporting

If a bug affecting a release is discovered after subsequent releases, create patch versions:

1. Identify the commit that introduced the issue and determine affected releases.

2. Compute new patch versions by incrementing the fourth digit of the latest release in each affected series.

3. For each affected version, create a branch from its tag:
    ```bash
    git checkout -b release/cardano-api-10.14.2.0 cardano-api-10.14.1.0
    ```

4. Cherry-pick fixes via PRs into each release branch. Each fix PR should include a `herald new` fragment as usual.

5. From the release branch, batch the changelog:
    ```bash
    herald batch cardano-api --commit-tag
    ```

6. Publish to CHaP as in the normal release process.

7. Push the tag:
    ```bash
    git push origin cardano-api-10.14.2.0
    ```

8. Unlike normal releases, **do not merge** backport release PRs into `master` (the fix is already there). Close the backport PRs and open one PR from `master` that updates the changelog for all closed backport releases.

## Troubleshooting

### Build fails due to `installed package instance does not exist`
If your build fails with an error like:
```
Configuring library for cardano-ledger-conway-1.8.0.0..
Error: cabal: The following package dependencies were requested
--dependency='cardano-ledger-alonzo=...'
however the given installed package instance does not exist.
```
increase the cabal cache version number in [.github/workflows/haskell.yml](.github/workflows/haskell.yml):
```yaml
CABAL_CACHE_VERSION: "2023-08-22"
```
Setting this date to the current date is usually enough. If it is already the current date, add a suffix (e.g. `2023-08-22-1`). This happens due to cache collisions in [`cabal-cache`](https://github.com/haskell-works/cabal-cache).

## References
1. https://github.com/IntersectMBO/cardano-ledger/blob/master/RELEASING.md
1. https://chap.intersectmbo.org/index.html
1. https://input-output-hk.github.io/cardano-engineering-handbook/policy/haskell/packaging/versioning.html

<!-- vim: set spell textwidth=0: -->
