#!/usr/bin/env bash
# fix-haddock-links.sh
#
# Usage: ./scripts/fix-haddock-links.sh <website-directory>
#
# ─── Why this script exists ──────────────────────────────────────────
#
# `cabal haddock-project` emits cross-package hrefs as relative paths
# (e.g. href="../cardano-ledger-api-1.2.3-hash/Foo.html") that don't
# resolve on the published docs site — we only host cardano-api's own
# output, not its dependencies. Every cross-package reference is a 404
# by default.
#
# This script replaces each such href with one of:
#
#   • an absolute URL on the upstream doc site, when the package is
#     on CHaP and we find a valid hosted module page for it; or
#   • a tooltip-annotated, unclickable <span>, when (a) the package
#     is not on CHaP — we deliberately don't link bootlibs like base,
#     bytestring, time, etc. because Haddock's URL shapes don't match
#     Hackage's and readers rarely follow those links anyway; or (b)
#     the package is on CHaP but the probe finds no doc site for it;
#     or (c) the upstream doc site is valid but the specific module
#     page 404s (upstream only publishes umbrella modules, our
#     Haddock asks for the defining sub-module).
#
# The published site thus has zero clickable 404s.
#
# ─── Pipeline ────────────────────────────────────────────────────────
#
#   Phase 1  Scan filesystem, symlink versioned dirs, fetch the CHaP
#            index, grep HTML for cross-package link targets.
#   Phase 2  For each discovered target, probe candidate doc-site URLs
#            and rewrite links (or mark unclickable if unresolvable).
#   Phase 2b Rewrite local re-export pages to point at the defining
#            upstream package, using Haddock's "Source" cabal-store
#            link as ground truth for which package the type lives in.
#   Phase 3  HEAD-validate rewritten URLs; rescue dead ones by probing
#            doc-site subdirs (api/, protocols/, framework/) and parent
#            modules with #t: fragment reconstruction. What can't be
#            rescued becomes an annotated <span>.
#
# ─── Doc-site resolution (Phase 2) ───────────────────────────────────
#
# For each CHaP package, try two things in order, first hit wins:
#
#   1. Name-suffix heuristic under *.cardano.intersectmbo.org — strip
#      trailing "-token" segments of the package name and HEAD-probe
#      each candidate's doc-index.html. Covers cardano-ledger-*,
#      plutus-*, ouroboros-*, etc.
#   2. Fixed fallback against IOG_DOC_BASES below — covers packages
#      whose subdomain isn't a suffix of the package name (e.g.
#      cardano-base lives at base.cardano.intersectmbo.org).
#
# Misses fall through to "Unmapped CHaP" — see the CI policy below.
#
# ─── Why non-CHaP packages are NOT linked ────────────────────────────
#
# Bootlibs (base, bytestring, time, the transformers stack, etc.) are
# on Hackage. Rewriting their hrefs to Hackage URLs almost always
# produced 404s — Haddock's per-module URL structure doesn't line up
# cleanly with Hackage's (src/ source views, -inplace suffixes from
# local rebuilds), and readers of cardano-api docs rarely click into
# bootlib internals anyway. We skip them entirely: rendered as
# unclickable <span>s, no outbound link, no validation, no noise.
#
# ─── Dead-link CI policy ─────────────────────────────────────────────
#
# Every dead link falls into one of two buckets:
#
#   Actionable (FAILS CI)
#     A CHaP package the probe couldn't resolve to any doc site. This
#     is usually a gap in IOG_DOC_BASES — add the package's upstream
#     doc base URL, or add the package to KNOWN_UNDOCUMENTED if it
#     genuinely has no published Haddocks anywhere.
#
#   Unfixable (does NOT fail CI, logged for visibility)
#     Three sub-causes, all outside this repo:
#       a. Module-level 404s on an otherwise-valid upstream doc site.
#          Empirically the cause is upstream publishing only their
#          umbrella "exposed-modules" (e.g. Cardano-Binary.html) while
#          Haddock generates hrefs to the defining sub-module (e.g.
#          Cardano-Binary-FromCBOR.html). We can't make upstream teams
#          publish their internal modules. (It's plausible that our
#          --internal flag in cabal haddock-project contributes; we
#          have not verified. Dropping --internal would cost us our
#          own internal-module pages on the published site, so we
#          leave it on and accept the span noise.)
#       b. Packages in KNOWN_UNDOCUMENTED (no published Haddocks
#          anywhere, only source on GitHub — e.g. kes-agent).
#       c. Haddock-emitted absolute Hackage URLs that lack a package
#          version (Hackage's routing requires one). A handful, out
#          of our control, treated as noise.
#       d. Master-vs-release drift between us and our dependencies.
#          Our haddocks are built from cardano-api master, which pins
#          specific releases of upstream packages. The linked upstream
#          doc sites publish from their master, which may have moved
#          or removed symbols since the release we pin. Our hrefs are
#          correct against the pinned release but 404 against current
#          master haddocks.
#
# Escape hatch: FIX_HADDOCK_LINKS_ALLOW_DEAD=1 exits 0 even if there
# are actionable entries, e.g. to deploy while investigating. Under
# GitHub Actions, actionable entries emit ::warning:: annotations so
# they surface in the job UI; unfixable ones are plain log lines.

set -euo pipefail
shopt -s nullglob   # empty-glob expands to nothing, not the literal pattern

WEBSITE_DIR="${1:?Usage: $0 <website-directory>}"
[ -d "$WEBSITE_DIR" ] || { echo "Error: $WEBSITE_DIR is not a directory" >&2; exit 1; }

TMPFILES=()
trap 'rm -f "${TMPFILES[@]}"' EXIT

mktemp_tracked() {
  local t; t=$(mktemp)
  TMPFILES+=("$t")
  printf '%s\n' "$t"
}

# ============================================================================
# Configuration and helpers
# ============================================================================
#
# For each CHaP package, probe candidate doc-site URLs in order until one
# returns 200 for doc-index.html (a standard Haddock asset every package
# site serves; wrong-site probes reliably 404). Candidate order is:
#   1. Name-suffix subdomain under *.cardano.intersectmbo.org, dropping
#      trailing "-<token>" segments of the package name — fast path for
#      families where the subdomain is a name-suffix (cardano-ledger-*,
#      ouroboros-*, plutus-*).
#   2. IOG_DOC_BASES — known doc-site roots for irregular-subdomain
#      families (cardano-base at base.cardano…, network-mux at
#      ouroboros-network.cardano…, io-classes at input-output-hk.github.io/
#      io-sim, etc.).
#
# Adding a new IOG doc site = one line in IOG_DOC_BASES. Packages on
# existing sites need no edit.

IOG_DOC_BASES=(
  "https://cardano-ledger.cardano.intersectmbo.org"
  "https://base.cardano.intersectmbo.org"
  "https://plutus.cardano.intersectmbo.org/haddock/latest"
  "https://ouroboros-consensus.cardano.intersectmbo.org/haddocks"
  "https://ouroboros-network.cardano.intersectmbo.org"
  "https://input-output-hk.github.io/io-sim"
  "https://input-output-hk.github.io/typed-protocols"
)

# CHaP packages we've confirmed have no public Haddocks anywhere (no
# gh-pages branch, no CloudFront site). Listed here so the script can
# classify them as "known unfixable" rather than "actionable gap in config"
# — they still appear as annotated dead-link spans in the output, but do
# NOT fail CI. Prefer adding a doc site to IOG_DOC_BASES where possible;
# this list is a last resort.
KNOWN_UNDOCUMENTED=(
  "kes-agent"
  "kes-agent-crypto"
)

# Some doc sites organise module pages into subdirectories beneath the
# package root (e.g. ouroboros-network publishes Ouroboros-Network-Magic.html
# under ouroboros-network/api/, not ouroboros-network/ directly). When a
# module page URL 404s at the flat path, the rescue logic retries under
# each of these subdirectories.
DOC_SUBDIRS=(api protocols framework)

derive_name_candidates() {
  local name="$1"
  while true; do
    printf '%s\n' "https://${name}.cardano.intersectmbo.org"
    [[ "$name" == *-* ]] || break
    name="${name%-*}"
    [[ "$name" == *-* ]] || break
  done
}

probe_site() {
  local code
  code=$(curl -sI -o /dev/null -w "%{http_code}" \
    --connect-timeout 5 --max-time 10 \
    --retry 3 --retry-delay 2 --retry-all-errors \
    "${1}/${2}/doc-index.html" 2>/dev/null || echo "000")
  [[ "$code" == "200" || "$code" == "301" || "$code" == "302" || "$code" == "307" || "$code" == "308" ]]
}

resolve_url() {
  local pkg="$1" is_chap="$2" base
  if [[ "$is_chap" != "yes" ]]; then echo "HACKAGE"; return; fi
  while IFS= read -r base; do
    probe_site "$base" "$pkg" && { echo "$base"; return; }
  done < <(derive_name_candidates "$pkg")
  for base in "${IOG_DOC_BASES[@]}"; do
    probe_site "$base" "$pkg" && { echo "$base"; return; }
  done
  echo "NONE"
}

# Append a sed substitution that turns <a href="PATTERN...">X</a> into a
# non-clickable <span class="dead-link" title="TITLE">X</span>, to the
# named script file. We use file-based sed (-f) rather than -e on the
# command line because full-repo builds can produce thousands of
# substitutions whose combined argv hits xargs's limit. The title is
# escaped against sed-replacement specials (& and \) and the | delimiter
# so future callers can pass arbitrary text without breaking the script.
add_unclickable_cmd() {
  local file="$1" href_pattern="$2" title="$3" escaped_title
  escaped_title=$(printf '%s' "$title" | sed 's/[&\\|]/\\&/g')
  printf 's|<a href="%s[^"]*"[^>]*>\\([^<]*\\)</a>|<span class="dead-link" title="%s">\\1</span>|g\n' \
    "$href_pattern" "$escaped_title" >> "$file"
}

# ============================================================================
# Phase 1: Prepare
# ============================================================================

echo "Phase 1: Preparing..."

# Pre-symlink directory set (used to match versioned-hash targets below)
declare -A SHORT_DIRS
for dir in "$WEBSITE_DIR"/*/; do
  SHORT_DIRS["$(basename "$dir")"]=1
done

# Create short-name symlinks for local cabal packages
# (cardano-api-10.26.0.0-inplace -> cardano-api)
symlink_count=0
for dir in "$WEBSITE_DIR"/*/; do
  name="$(basename "$dir")"
  [[ "$name" =~ -inplace-.+ ]] && continue
  if [[ "$name" =~ ^(.+)-[0-9]+\.[0-9]+.*-inplace$ ]]; then
    short="${BASH_REMATCH[1]}"
    if [[ ! -e "$WEBSITE_DIR/$short" ]]; then
      ln -s "$name" "$WEBSITE_DIR/$short"
      symlink_count=$((symlink_count + 1))
    fi
  fi
done

# Post-symlink local-package set (used to skip re-rewriting our own docs)
declare -A LOCAL_SET
for dir in "$WEBSITE_DIR"/*/; do
  [ -d "$dir" ] && LOCAL_SET["$(basename "$dir")"]=1
done

# Fetch CHaP index. --fail makes curl exit non-zero on HTTP errors so an
# outage doesn't silently produce an empty index that masquerades as
# "everything is non-CHaP".
CHAP_PKGS_FILE=$(mktemp_tracked)
if ! curl -sL --fail --retry 3 --retry-delay 2 --retry-all-errors \
       https://chap.intersectmbo.org/01-index.tar.gz \
       | tar -tz | grep -oP '^[^/]+' | sort -u > "$CHAP_PKGS_FILE"; then
  echo "Error: failed to fetch CHaP package index from https://chap.intersectmbo.org/01-index.tar.gz" >&2
  exit 1
fi
if [[ ! -s "$CHAP_PKGS_FILE" ]]; then
  echo "Error: CHaP package index is empty (fetch succeeded but produced no entries)" >&2
  exit 1
fi
declare -A CHAP_SET
while IFS= read -r pkg; do CHAP_SET["$pkg"]=1; done < "$CHAP_PKGS_FILE"

# Single HTML scan for all cross-package link targets
DISCOVERED_PKGS=$(grep -rohP --include='*.html' 'href="\.\./(\.\./)?\K[a-zA-Z][a-zA-Z0-9_.-]*(?=/)' \
                    "$WEBSITE_DIR" 2>/dev/null | sort -u)
discovered_total=$(echo "$DISCOVERED_PKGS" | grep -c . || true)

# Of those targets, symlink versioned-hash names whose stripped base
# matches an existing short directory.
# (href="../base-4.20.2.0-f074/..." + ./base/ exists -> base-4.20.2.0-f074 symlink)
while IFS= read -r target; do
  [ -z "$target" ] && continue
  [[ -e "$WEBSITE_DIR/$target" ]] && continue
  if [[ "$target" =~ ^(.+)-[0-9]+\.[0-9] ]]; then
    short="${BASH_REMATCH[1]}"
    if [[ -v "SHORT_DIRS[$short]" ]]; then
      ln -s "$short" "$WEBSITE_DIR/$target"
      symlink_count=$((symlink_count + 1))
    fi
  fi
done <<< "$DISCOVERED_PKGS"

echo "  Local directories:          ${#LOCAL_SET[@]}"
echo "  CHaP packages:              $(wc -l < "$CHAP_PKGS_FILE")"
echo "  Cross-package link targets: $discovered_total"
echo "  Symlinks created:           $symlink_count"

# ============================================================================
# Phase 2: Resolve doc sites and rewrite links
# ============================================================================

echo "Phase 2: Resolving doc sites and rewriting links..."

REWRITE_SCRIPT=$(mktemp_tracked)
UNMAPPED_CHAP=()   # CHaP packages we couldn't resolve to a doc site — gap in config
NON_CHAP=()        # non-CHaP packages (bootlibs etc.) we don't bother linking to
declare -A PKG_URL=()   # cached doc-site base URL per package (reused in Phase 2b)
skipped=0 rewrote_docsite=0

while IFS= read -r pkg; do
  [ -z "$pkg" ] && continue
  if [[ -v "LOCAL_SET[$pkg]" ]]; then skipped=$((skipped + 1)); continue; fi

  # Haddock emits hrefs to locally-built deps as "<name>-<version>-inplace"
  # even when their docs weren't generated into the output tree (so they're
  # not in LOCAL_SET). Strip the suffix so classification sees the real
  # package name, for cleaner tooltips. Keep the original `pkg` for the sed
  # href pattern, which is what appears in the HTML.
  # NB: ordering matters — evaluate the negative match first so it doesn't
  # clobber BASH_REMATCH[1] set by the capturing regex.
  lookup="$pkg"
  if [[ ! "$pkg" =~ -inplace-.+ ]] && [[ "$pkg" =~ ^(.+)-[0-9]+\.[0-9]+.*-inplace$ ]]; then
    lookup="${BASH_REMATCH[1]}"
  fi
  if [[ "$lookup" != "$pkg" && -v "LOCAL_SET[$lookup]" ]]; then
    skipped=$((skipped + 1)); continue
  fi

  is_chap="no"; [[ -v "CHAP_SET[$lookup]" ]] && is_chap="yes"
  url=$(resolve_url "$lookup" "$is_chap")
  PKG_URL[$lookup]="$url"

  if [[ "$url" == "NONE" ]]; then
    UNMAPPED_CHAP+=("$lookup")
    add_unclickable_cmd "$REWRITE_SCRIPT" "\\.\\./${pkg}/" "No hosted documentation available for ${lookup}"
    add_unclickable_cmd "$REWRITE_SCRIPT" "\\.\\./\\.\\./${pkg}/" "No hosted documentation available for ${lookup}"
    continue
  fi

  if [[ "$url" == "HACKAGE" ]]; then
    # Non-CHaP packages (bootlibs etc.) — don't link to Hackage. The links
    # to base/bytestring/time/... internals are rarely useful to readers of
    # cardano-api docs, and Haddock's per-module URL structure doesn't line
    # up cleanly with Hackage's rendering, so we'd mostly generate 404s.
    # Leave the symbol visible but unclickable.
    NON_CHAP+=("$lookup")
    add_unclickable_cmd "$REWRITE_SCRIPT" "\\.\\./${pkg}/" "${lookup} is not a CHaP package; docs not linked"
    add_unclickable_cmd "$REWRITE_SCRIPT" "\\.\\./\\.\\./${pkg}/" "${lookup} is not a CHaP package; docs not linked"
    continue
  fi

  target="${url}/${lookup}/"
  rewrote_docsite=$((rewrote_docsite + 1))
  printf 's|href="\\.\\./%s/|href="%s|g\n' "$pkg" "$target" >> "$REWRITE_SCRIPT"
  printf 's|href="\\.\\./\\.\\./%s/|href="%s|g\n' "$pkg" "$target" >> "$REWRITE_SCRIPT"
done <<< "$DISCOVERED_PKGS"

echo "  Local (skipped):        $skipped"
echo "  Rewritten (doc site):   $rewrote_docsite"
echo "  Non-CHaP (unclickable): ${#NON_CHAP[@]}"
echo "  Unmapped CHaP:          ${#UNMAPPED_CHAP[@]}"
if [[ ${#UNMAPPED_CHAP[@]} -gt 0 ]]; then
  printf '    - %s\n' "${UNMAPPED_CHAP[@]}"
fi

if [[ -s "$REWRITE_SCRIPT" ]]; then
  find "$WEBSITE_DIR" -name '*.html' -print0 | xargs -0 -P "$(nproc)" sed -i -f "$REWRITE_SCRIPT"
fi

# ============================================================================
# Phase 2b: Rewrite re-exported type links to the original defining package
# ============================================================================
#
# Haddock resolves re-exports to the LOCAL re-export page (e.g. a reference
# to cardano-ledger-byron's `Address` via `Cardano.Api.Byron` becomes
# href="Cardano-Api-Byron.html#t:Address", not an external link). The
# Haddock-emitted "Source" link, however, points to the cabal store for the
# original defining package (file:///.../.cabal/store/PKG-VERSION/.../src/
# Module.Name.html). We use that as the source of truth to rewrite these
# local re-export hrefs to point at the upstream doc site.

echo "Phase 2b: Rewriting re-exported type links..."

REEXPORTS_FILE=$(mktemp_tracked)
python3 - "$WEBSITE_DIR" <<'PYEOF' > "$REEXPORTS_FILE"
import os, re, sys
website = sys.argv[1]
pattern = re.compile(
    r'id="(t|v):([^"]+)" class="def">[^<]*</a>\s*<a href="file:///[^"]*?/([a-zA-Z][\w-]*)-\d+\.[^/]*/share/doc/html/src/([^"#]+)\.html'
)
for d in os.listdir(website):
    if not d.endswith('-inplace') or not os.path.isdir(os.path.join(website, d)):
        continue
    dir_path = os.path.join(website, d)
    for f in os.listdir(dir_path):
        if not f.endswith('.html'):
            continue
        html = open(os.path.join(dir_path, f)).read()
        for m in pattern.finditer(html):
            atype, name, pkg, source_module = m.groups()
            module_page = source_module.replace('.', '-') + '.html'
            # Tab-separated: local_page, anchor, name, package, module_page
            print('\t'.join([f, atype, name, pkg, module_page]))
PYEOF

reexport_total=$(wc -l < "$REEXPORTS_FILE" | tr -d ' ')
echo "  Re-exports found: $reexport_total"

reexport_rewritten=0
reexport_unresolvable=0
if [[ $reexport_total -gt 0 ]]; then
  # Build candidate URLs: direct, then subdirs, then parent-stripped (with subdirs).
  # Format: local_page \t anchor \t name \t candidate_url
  REEXPORT_CANDIDATES=$(mktemp_tracked)
  while IFS=$'\t' read -r local_page anchor name pkg module_page; do
    [ -z "$local_page" ] && continue

    # Resolve package base URL via the Phase 2 cache (fall back to resolve_url
    # for packages not seen as cross-package targets in the HTML).
    if [[ -v "PKG_URL[$pkg]" ]]; then
      base="${PKG_URL[$pkg]}"
    else
      is_chap="no"; [[ -v "CHAP_SET[$pkg]" ]] && is_chap="yes"
      base=$(resolve_url "$pkg" "$is_chap")
      PKG_URL[$pkg]="$base"
    fi

    # Skip unresolvable (NONE) and non-CHaP (HACKAGE — we don't link there).
    [[ "$base" == "NONE" || "$base" == "HACKAGE" ]] && continue

    # Direct URL
    echo "${local_page}	${anchor}	${name}	${base}/${pkg}/${module_page}" >> "$REEXPORT_CANDIDATES"

    # Subdirectory variants (ouroboros-network layout)
    for subdir in "${DOC_SUBDIRS[@]}"; do
      echo "${local_page}	${anchor}	${name}	${base}/${pkg}/${subdir}/${module_page}" >> "$REEXPORT_CANDIDATES"
    done

    # Progressively stripped parent modules (+ subdir variants)
    module_base="${module_page%.html}"
    while [[ "$module_base" == *-* ]]; do
      module_base="${module_base%-*}"
      echo "${local_page}	${anchor}	${name}	${base}/${pkg}/${module_base}.html" >> "$REEXPORT_CANDIDATES"
      for subdir in "${DOC_SUBDIRS[@]}"; do
        echo "${local_page}	${anchor}	${name}	${base}/${pkg}/${subdir}/${module_base}.html" >> "$REEXPORT_CANDIDATES"
      done
    done
  done < "$REEXPORTS_FILE"

  # Probe all unique candidate URLs in parallel
  REEXPORT_VALID_FILE=$(mktemp_tracked)
  if [[ -s "$REEXPORT_CANDIDATES" ]]; then
    # shellcheck disable=SC2016
    awk -F'\t' '{print $4}' "$REEXPORT_CANDIDATES" | sort -u | \
      xargs -P 16 -I{} sh -c \
        'code=$(curl -sI -o /dev/null -w "%{http_code}" --connect-timeout 5 --max-time 10 --retry 3 --retry-delay 2 --retry-all-errors "$1"); if [ "$code" = "200" ] || [ "$code" = "301" ] || [ "$code" = "302" ] || [ "$code" = "307" ] || [ "$code" = "308" ]; then echo "$1"; fi' _ {} \
        > "$REEXPORT_VALID_FILE" 2>/dev/null
  fi

  declare -A REEXPORT_VALID_SET=()
  while IFS= read -r url; do
    [ -z "$url" ] && continue
    REEXPORT_VALID_SET["$url"]=1
  done < "$REEXPORT_VALID_FILE"

  # For each re-export, walk its candidates in order and pick the first
  # that validated. First-hit-wins prefers the direct URL over subdir /
  # parent fallbacks.
  declare -A REEXPORT_RESOLVED=()   # "local_page|anchor|name" -> resolved_url
  while IFS=$'\t' read -r local_page anchor name candidate; do
    [ -z "$local_page" ] && continue
    key="${local_page}|${anchor}|${name}"
    [[ -v "REEXPORT_RESOLVED[$key]" ]] && continue
    if [[ -v "REEXPORT_VALID_SET[$candidate]" ]]; then
      REEXPORT_RESOLVED["$key"]="$candidate"
    fi
  done < "$REEXPORT_CANDIDATES"

  # Build sed rewrite rules. We only rewrite the exact
  # href="LOCAL_PAGE#ANCHOR:NAME" that we've proven is a re-export of a
  # type documented at a reachable upstream URL; the LOCAL_PAGE#ANCHOR:NAME
  # for local-only types (e.g. cardano-api's own Address GADT) is left
  # untouched.
  REEXPORT_SCRIPT=$(mktemp_tracked)
  for key in "${!REEXPORT_RESOLVED[@]}"; do
    IFS='|' read -r local_page anchor name <<< "$key"
    resolved="${REEXPORT_RESOLVED[$key]}"
    escaped_local=$(printf '%s' "$local_page" | sed 's|[&/\]|\\&|g; s|\.|\\.|g')
    escaped_resolved=$(printf '%s' "$resolved" | sed 's|[&/\]|\\&|g')
    printf 's|href="%s#%s:%s"|href="%s#%s:%s"|g\n' \
      "$escaped_local" "$anchor" "$name" "$escaped_resolved" "$anchor" "$name" \
      >> "$REEXPORT_SCRIPT"
    reexport_rewritten=$((reexport_rewritten + 1))
  done
  # Clamp to 0 in case Python emits duplicate (local_page, anchor, name)
  # rows for a single key — reexport_total counts rows, reexport_rewritten
  # counts unique resolved keys.
  reexport_unresolvable=$((reexport_total - reexport_rewritten))
  [[ $reexport_unresolvable -lt 0 ]] && reexport_unresolvable=0

  if [[ -s "$REEXPORT_SCRIPT" ]]; then
    find "$WEBSITE_DIR" -name '*.html' -print0 | xargs -0 -P "$(nproc)" sed -i -f "$REEXPORT_SCRIPT"
  fi
fi

echo "  Rewritten:    $reexport_rewritten"
echo "  Unresolvable: $reexport_unresolvable"

# ============================================================================
# Phase 3: Validate external URLs and annotate dead ones
# ============================================================================

echo "Phase 3: Validating external URLs..."

URLS_FILE=$(mktemp_tracked)
DEAD_URLS_FILE=$(mktemp_tracked)
# `|| true` because grep exits 1 on no-match, which under set -e + pipefail
# would abort the script on a degenerate corpus with no absolute URLs.
grep -rohP 'href="\Khttps://[^"#]+\.html' "$WEBSITE_DIR" 2>/dev/null | sort -u > "$URLS_FILE" || true
url_count=$(wc -l < "$URLS_FILE")

if [[ $url_count -gt 0 ]]; then
  # shellcheck disable=SC2016 # single quotes intentional: expansions evaluated by inner sh -c
  xargs -P 16 -I{} sh -c \
    'code=$(curl -sI -o /dev/null -w "%{http_code}" --connect-timeout 5 --max-time 10 --retry 3 --retry-delay 2 --retry-all-errors "$1"); if [ "$code" != "200" ] && [ "$code" != "301" ] && [ "$code" != "302" ] && [ "$code" != "307" ] && [ "$code" != "308" ]; then echo "$1"; fi' _ {} \
    < "$URLS_FILE" > "$DEAD_URLS_FILE" 2>/dev/null
fi
dead_count=$(wc -l < "$DEAD_URLS_FILE" | tr -d ' ')

# Phase 3a: rescue dead URLs by probing doc-site subdirectories (e.g.
# ouroboros-network/api/) and parent modules. Haddock frequently links to
# internal defining modules that don't publish as standalone pages — the
# type is actually documented on a parent module (stripping the last
# dash-separated segment). When we find a valid parent, we rewrite the
# link to <parent>#t:<LastComponent> so the browser still scrolls to the
# type definition instead of showing a greyed-out span.
declare -A DEAD_TO_RESCUE=() DEAD_TO_FRAGMENT=()
rescued_count=0
if [[ $dead_count -gt 0 ]]; then
  RESCUE_CANDIDATES=$(mktemp_tracked)
  while IFS= read -r dead_url; do
    [ -z "$dead_url" ] && continue
    base_path="${dead_url%/*}"
    module_file=$(basename "$dead_url" .html)

    # Subdirectory variants at the same level. Use the last dash-component
    # of the module name as the fragment suffix — harmless if the browser
    # doesn't find it, useful for links emitted without a #fragment.
    stripped_first="${module_file##*-}"
    for subdir in "${DOC_SUBDIRS[@]}"; do
      echo "${dead_url}	${base_path}/${subdir}/${module_file}.html	${stripped_first}" >> "$RESCUE_CANDIDATES"
    done

    # Progressively stripped parent modules (+ subdir variants).
    remaining="$module_file"
    parent_stripped_first=""
    while [[ "$remaining" == *-* ]]; do
      last="${remaining##*-}"
      remaining="${remaining%-*}"
      [[ -z "$parent_stripped_first" ]] && parent_stripped_first="$last"
      echo "${dead_url}	${base_path}/${remaining}.html	${parent_stripped_first}" >> "$RESCUE_CANDIDATES"
      for subdir in "${DOC_SUBDIRS[@]}"; do
        echo "${dead_url}	${base_path}/${subdir}/${remaining}.html	${parent_stripped_first}" >> "$RESCUE_CANDIDATES"
      done
    done
  done < "$DEAD_URLS_FILE"

  RESCUE_VALID_FILE=$(mktemp_tracked)
  if [[ -s "$RESCUE_CANDIDATES" ]]; then
    # shellcheck disable=SC2016
    awk -F'\t' '{print $2}' "$RESCUE_CANDIDATES" | sort -u | \
      xargs -P 16 -I{} sh -c \
        'code=$(curl -sI -o /dev/null -w "%{http_code}" --connect-timeout 5 --max-time 10 --retry 3 --retry-delay 2 --retry-all-errors "$1"); if [ "$code" = "200" ] || [ "$code" = "301" ] || [ "$code" = "302" ] || [ "$code" = "307" ] || [ "$code" = "308" ]; then echo "$1"; fi' _ {} \
        > "$RESCUE_VALID_FILE" 2>/dev/null
  fi

  declare -A RESCUE_VALID_SET=()
  while IFS= read -r url; do
    [ -z "$url" ] && continue
    RESCUE_VALID_SET["$url"]=1
  done < "$RESCUE_VALID_FILE"

  # First-match-wins: direct subdir hit preferred over deeper parents.
  while IFS=$'\t' read -r dead cand stripped; do
    [ -z "$dead" ] && continue
    [[ -v "DEAD_TO_RESCUE[$dead]" ]] && continue
    if [[ -v "RESCUE_VALID_SET[$cand]" ]]; then
      DEAD_TO_RESCUE["$dead"]="$cand"
      DEAD_TO_FRAGMENT["$dead"]="$stripped"
    fi
  done < "$RESCUE_CANDIDATES"

  rescued_count=${#DEAD_TO_RESCUE[@]}

  if [[ $rescued_count -gt 0 ]]; then
    RESCUE_SCRIPT=$(mktemp_tracked)
    for dead in "${!DEAD_TO_RESCUE[@]}"; do
      rescue="${DEAD_TO_RESCUE[$dead]}"
      stripped="${DEAD_TO_FRAGMENT[$dead]}"
      escaped_dead=$(printf '%s' "$dead" | sed 's|[&/\]|\\&|g; s|\.|\\.|g')
      escaped_rescue=$(printf '%s' "$rescue" | sed 's|[&/\]|\\&|g')
      # href="DEAD"      -> href="RESCUE#t:<stripped>"  (bare URL, add fragment)
      # href="DEAD#frag" -> href="RESCUE#frag"           (preserve fragment)
      printf 's|href="%s"|href="%s#t:%s"|g\n' \
        "$escaped_dead" "$escaped_rescue" "$stripped" >> "$RESCUE_SCRIPT"
      printf 's|href="%s#|href="%s#|g\n' \
        "$escaped_dead" "$escaped_rescue" >> "$RESCUE_SCRIPT"
    done
    find "$WEBSITE_DIR" -name '*.html' -print0 | xargs -0 -P "$(nproc)" sed -i -f "$RESCUE_SCRIPT"
  fi
fi

dead_remaining=$((dead_count - rescued_count))

echo "  URLs checked: $url_count"
echo "  Valid:        $((url_count - dead_count))"
echo "  Dead:         $dead_count"
echo "  Rescued:      $rescued_count"
echo "  Unrescuable:  $dead_remaining"

# Per-URL context tables, populated BEFORE the <a>→<span> replacement so
# the original anchor text and reference sites are still in the HTML.
declare -A DEAD_PKG DEAD_MODULE DEAD_SYMBOLS DEAD_REFS

DEAD_SCRIPT=$(mktemp_tracked)
if [[ $dead_remaining -gt 0 ]]; then
  while IFS= read -r dead_url; do
    [ -z "$dead_url" ] && continue
    # Rescued URLs have already been rewritten; no unclickable annotation.
    [[ -v "DEAD_TO_RESCUE[$dead_url]" ]] && continue

    # Package + Haskell module name (Haddock encodes dots as dashes).
    # For non-Hackage URLs, strip any DOC_SUBDIRS segment first so a path
    # like .../<pkg>/api/Module.html still extracts <pkg>, not "api".
    if [[ "$dead_url" == *"hackage.haskell.org"* ]]; then
      pkg=$(echo "$dead_url" | grep -oP 'package/\K[^/]+')
    else
      url_for_pkg="$dead_url"
      for sub in "${DOC_SUBDIRS[@]}"; do
        url_for_pkg="${url_for_pkg//\/${sub}\//\/}"
      done
      pkg=$(echo "$url_for_pkg" | grep -oP '[^/]+(?=/[^/]+\.html$)')
    fi
    module_file=$(basename "$dead_url" .html)
    DEAD_PKG[$dead_url]="$pkg"
    DEAD_MODULE[$dead_url]="${module_file//-/.}"

    # Symbols the dead URL is trying to link to (text of each <a> tag)
    regex_url=$(printf '%s' "$dead_url" | sed 's|[][\\.^$*+?{}|()]|\\&|g')
    DEAD_SYMBOLS[$dead_url]=$(grep -rohP \
      "<a[^>]*href=\"${regex_url}[^\"]*\"[^>]*>\\K[^<]+" "$WEBSITE_DIR" 2>/dev/null \
      | sort -u | head -8 | paste -sd ', ' || echo "")

    # Our own docs pages that reference it (the re-export sites we ship)
    DEAD_REFS[$dead_url]=$(grep -rlF "$dead_url" "$WEBSITE_DIR" 2>/dev/null \
      | sed "s|^${WEBSITE_DIR}/||" | sort -u | head -6 || echo "")

    echo "    - $dead_url ($pkg)"
    escaped=$(printf '%s' "$dead_url" | sed 's|[&/\]|\\&|g; s|\.|\\.|g')
    add_unclickable_cmd "$DEAD_SCRIPT" "$escaped" \
      "From ${pkg} — documentation not available at the expected URL"
  done < "$DEAD_URLS_FILE"
  if [[ -s "$DEAD_SCRIPT" ]]; then
    find "$WEBSITE_DIR" -name '*.html' -print0 | xargs -0 -P "$(nproc)" sed -i -f "$DEAD_SCRIPT"
  fi
fi

# Inject dead-link CSS whenever any <span class="dead-link"> was produced.
# The data-dead-link sentinel attribute makes the substitution idempotent:
# the sed address /data-dead-link/! skips lines that already contain a
# previous injection, so re-running the script doesn't accumulate duplicate
# <style> blocks.
if [[ ${#UNMAPPED_CHAP[@]} -gt 0 || ${#NON_CHAP[@]} -gt 0 || $dead_count -gt 0 ]]; then
  find "$WEBSITE_DIR" -name '*.html' -print0 | xargs -0 -P "$(nproc)" sed -i \
    '/data-dead-link/!s|</head>|<style data-dead-link="1">.dead-link { border-bottom: 1px dotted \#888; color: \#888; cursor: help; }</style></head>|'
fi

# ============================================================================
# Summary + strict-mode exit
# ============================================================================

echo ""
echo "=== fix-haddock-links summary ==="
echo "  Discovered:             $discovered_total"
echo "  Local (skipped):        $skipped"
echo "  Rewritten (doc site):   $rewrote_docsite"
echo "  Re-exports rewritten:   $reexport_rewritten"
echo "  Re-exports unresolved:  $reexport_unresolvable"
echo "  Non-CHaP (unclickable): ${#NON_CHAP[@]}"
echo "  Unmapped CHaP:          ${#UNMAPPED_CHAP[@]}"
echo "  URLs validated:         $url_count"
echo "  Dead links rescued:     $rescued_count"
echo "  Dead links annotated:   $dead_remaining"
echo "================================="

# Partition Unmapped CHaP into actionable (gap in IOG_DOC_BASES — we can
# probably fix this) vs known-unfixable (package has no published docs
# anywhere, listed in KNOWN_UNDOCUMENTED). Module-level 404s are always
# classified as unfixable: they're driven by upstream sites not publishing
# internal modules (our --internal flag generates references to them) or
# by version skew, neither of which is actionable inside this repo.
ACTIONABLE_UNMAPPED=()
UNFIXABLE_UNMAPPED=()
for pkg in "${UNMAPPED_CHAP[@]}"; do
  is_known_undocumented="no"
  for u in "${KNOWN_UNDOCUMENTED[@]}"; do
    [[ "$pkg" == "$u" ]] && { is_known_undocumented="yes"; break; }
  done
  if [[ "$is_known_undocumented" == "yes" ]]; then
    UNFIXABLE_UNMAPPED+=("$pkg")
  else
    ACTIONABLE_UNMAPPED+=("$pkg")
  fi
done

actionable_count=${#ACTIONABLE_UNMAPPED[@]}
unfixable_count=$(( ${#UNFIXABLE_UNMAPPED[@]} + dead_remaining ))

if [[ $actionable_count -eq 0 && $unfixable_count -eq 0 ]]; then
  exit 0
fi

# Actionable section — emit GH Actions ::warning:: annotations for UI visibility.
if [[ $actionable_count -gt 0 ]]; then
  echo ""
  echo "=== Actionable — fix these (${actionable_count}) ==="
  echo ""
  echo "CHaP packages the probe could not resolve to any known doc site:"
  for pkg in "${ACTIONABLE_UNMAPPED[@]}"; do
    echo "  - $pkg"
    [[ -n "${GITHUB_ACTIONS:-}" ]] && \
      echo "::warning title=Unmapped CHaP package::${pkg} has no known doc site. Add its base URL to IOG_DOC_BASES in scripts/fix-haddock-links.sh, or add ${pkg} to KNOWN_UNDOCUMENTED if it has no published Haddocks."
  done
  echo ""
  echo "  To fix each:"
  echo "    1. Check the package's source repo for a gh-pages or CloudFront"
  echo "       deployment of its Haddocks."
  echo "    2. If published: append the base URL to IOG_DOC_BASES in"
  echo "       scripts/fix-haddock-links.sh and re-run."
  echo "    3. If genuinely unpublished: add the package name to"
  echo "       KNOWN_UNDOCUMENTED in scripts/fix-haddock-links.sh so future"
  echo "       runs classify it as known-unfixable instead of failing CI."
fi

# Unfixable section — plain log lines, no ::warning:: flood in the UI.
if [[ $unfixable_count -gt 0 ]]; then
  echo ""
  echo "=== Known unfixable — not blocking (${unfixable_count}) ==="
  echo ""
  echo "These are annotated as greyed-out spans in the published docs and"
  echo "do not fail CI. They stem from causes outside this repo:"
  echo "  - Haddock generates cross-package hrefs to internal modules via"
  echo "    the --internal flag, but upstream doc sites only publish"
  echo "    exposed modules."
  echo "  - Some upstream packages have no published Haddocks at all."
  echo "  - Haddock occasionally emits absolute Hackage URLs without a"
  echo "    package version, which Hackage's routing doesn't accept."
  echo ""
  if [[ ${#UNFIXABLE_UNMAPPED[@]} -gt 0 ]]; then
    echo "CHaP packages with no published docs (${#UNFIXABLE_UNMAPPED[@]}, from KNOWN_UNDOCUMENTED):"
    for pkg in "${UNFIXABLE_UNMAPPED[@]}"; do
      echo "  - $pkg"
    done
    echo ""
  fi
  if [[ $dead_remaining -gt 0 ]]; then
    echo "Module-level 404s (${dead_remaining}):"
    while IFS= read -r dead_url; do
      [ -z "$dead_url" ] && continue
      [[ -v "DEAD_TO_RESCUE[$dead_url]" ]] && continue
      echo ""
      echo "  $dead_url"
      echo "    Upstream: ${DEAD_MODULE[$dead_url]} (in package ${DEAD_PKG[$dead_url]})"
      [[ -n "${DEAD_SYMBOLS[$dead_url]:-}" ]] && \
        echo "    Symbols we link to: ${DEAD_SYMBOLS[$dead_url]}"
      if [[ -n "${DEAD_REFS[$dead_url]:-}" ]]; then
        echo "    Linked from our docs:"
        while IFS= read -r ref; do
          [ -z "$ref" ] && continue
          echo "      - $ref"
        done <<< "${DEAD_REFS[$dead_url]}"
      fi
    done < "$DEAD_URLS_FILE"
  fi
fi

echo ""
if [[ $actionable_count -eq 0 ]]; then
  echo "No actionable dead links; exiting 0."
  exit 0
fi
if [[ "${FIX_HADDOCK_LINKS_ALLOW_DEAD:-0}" == "1" ]]; then
  echo "FIX_HADDOCK_LINKS_ALLOW_DEAD=1 — accepting actionable dead links, exiting 0."
  exit 0
fi
echo "Actionable dead links found — failing the build. Set FIX_HADDOCK_LINKS_ALLOW_DEAD=1 to accept them."
exit 1
