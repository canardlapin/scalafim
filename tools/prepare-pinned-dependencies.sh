#!/bin/sh
# Validate the immutable provider graph before installing Multivar's artifacts.
set -eu

case "${1:-}" in
  '') check_only=false ;;
  --check-only) check_only=true ;;
  *) echo 'Usage: prepare-pinned-dependencies.sh [--check-only]' >&2; exit 2 ;;
esac
[ "$#" -le 1 ] || exit 2
repo_root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
provider_tmp=$(mktemp -d "${TMPDIR:-/tmp}/scalafim-providers.XXXXXX")
trap 'rm -rf "$provider_tmp"' EXIT HUP INT TERM
mkdir "$provider_tmp/pins"

read_pin() {
  sed -n -e "s/^lazy val ${2}Revision = \"\\([0-9a-f]*\\)\".*/\\1/p" \
    -e "/^lazy val ${2}Revision =$/{n;s/^ *\"\\([0-9a-f]*\\)\".*/\\1/p;}" "$1"
}
require_sha() {
  case "$2" in
    ''|*[!0-9a-f]*) echo "Invalid $1 revision: $2" >&2; exit 1 ;;
  esac
  [ "${#2}" -eq 40 ] || { echo "Invalid $1 revision: $2" >&2; exit 1; }
}

# These are consumers of shared provider values, not another pin authority.
# Read every expected revision from Root's build and every actual edge from
# that consumer's exact Git tree. Keep the source trees for artifact publishing
# private to this invocation; no sibling paths or local artifact cache is used
# to decide whether the default graph agrees.
for provider in multivar alder graph4s image4s mvpa4s reframe4s mesh4s neuroim4s neuroatlas4s neurosurf4s; do
  revision=$(read_pin "$repo_root/build.sbt" "$provider")
  require_sha "$provider" "$revision"
  git clone --quiet --no-checkout --filter=blob:none \
    "https://github.com/canardlapin/$provider.git" "$provider_tmp/$provider"
  git -C "$provider_tmp/$provider" show "$revision:build.sbt" > "$provider_tmp/pins/$provider.sbt"
  if [ "$provider" = multivar ]; then
    git -C "$provider_tmp/multivar" checkout --quiet --detach "$revision"
    [ "$(git -C "$provider_tmp/multivar" rev-parse HEAD)" = "$revision" ]
  fi
done

# Root deliberately has no second spatial4s pin. Its image provider is the
# anchor; every other coordinate consumer must agree with that full revision.
spatial_revision=$(read_pin "$provider_tmp/pins/image4s.sbt" spatial4s)
require_sha spatial4s "$spatial_revision"

while read -r consumer dependency; do
  [ -n "$consumer" ] || continue
  if [ "$dependency" = spatial4s ]; then
    expected=$spatial_revision
  else
    expected=$(read_pin "$repo_root/build.sbt" "$dependency")
  fi
  require_sha "$dependency" "$expected"
  actual=$(read_pin "$provider_tmp/pins/$consumer.sbt" "$dependency")
  case "$dependency" in
    gale) label=Gale ;;
    intaglio) label=Intaglio ;;
    *) label=$dependency ;;
  esac
  if [ "$actual" != "$expected" ]; then
    echo "$label revision mismatch: ScalaFIM=$expected $consumer=$actual" >&2
    exit 1
  fi
done <<'EDGES'
multivar gale
alder gale
alder resample4s
graph4s gale
image4s gale
image4s ravel
image4s locus4s
image4s intaglio
reframe4s gale
reframe4s ravel
reframe4s image4s
mvpa4s gale
mvpa4s resample4s
mvpa4s locus4s
mvpa4s multivar
mvpa4s alder
mvpa4s neuroatlas4s
mvpa4s neurosurf4s
mesh4s graph4s
mesh4s locus4s
neuroim4s graph4s
neuroim4s gale
neuroim4s image4s
neuroim4s reframe4s
neuroim4s intaglio
neuroatlas4s neuroim4s
neuroatlas4s neurosurf4s
neuroatlas4s locus4s
neurosurf4s mesh4s
neurosurf4s neuroim4s
neurosurf4s image4s
neurosurf4s reframe4s
neurosurf4s locus4s
neurosurf4s graph4s
neurosurf4s zarr4s
neurosurf4s intaglio
reframe4s spatial4s
mesh4s spatial4s
neuroim4s spatial4s
neurosurf4s spatial4s
EDGES

echo 'Immutable provider graph: 40 shared edges agree (spatial4s anchored in image4s).'
if [ "$check_only" = true ]; then exit 0; fi
GALE_CACHE="$provider_tmp/gale" \
  "$provider_tmp/multivar/tools/publish-gale-local.sh"
RESAMPLE4S_CACHE="$provider_tmp/resample4s" \
  "$provider_tmp/multivar/tools/publish-resample4s-local.sh"
