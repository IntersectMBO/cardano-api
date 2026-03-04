# Changelog

Add a changelog fragment by running the following command and committing the generated file:

```bash
changie new --project <package>
```

Available packages: `cardano-api`, `cardano-api-gen`, `cardano-rpc`, `cardano-wasm`

Available kinds:

| Kind | When to use |
|------|-------------|
| `breaking` | Removed or changed exported API in a backwards-incompatible way |
| `feature` | New exported function, type, or behaviour |
| `compatible` | Changed API in a backwards-compatible way |
| `bugfix` | Fixed incorrect behaviour |
| `optimisation` | Measurable performance improvement with no API change |

> If this PR does not need a changelog entry (e.g. documentation, CI, tests only),
> add the **`no-changelog`** label to skip this requirement.

# Context

Additional context for the PR goes here. If the PR fixes a particular issue please provide a [link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=) to the issue.

# How to trust this PR

Highlight important bits of the PR that will make the review faster. If there are commands the reviewer can run to observe the new behavior, describe them.

# Checklist

- [ ] Commit sequence broadly makes sense and commits have useful messages
- [ ] New tests are added if needed and existing tests are updated. See [Running tests](https://github.com/input-output-hk/cardano-node-wiki/wiki/Running-tests) for more details
- [ ] Self-reviewed the diff

<!--
### Note on CI ###
If your PR is from a fork, the necessary CI jobs won't trigger automatically for security reasons.
You will need to get someone with write privileges. Please contact IOG node developers to do this
for you.
-->
