#!/bin/sh
# Install Multivar's exact provider artifacts before loading the composite build.
set -eu

repo_root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
multivar_revision=$(sed -n 's/^lazy val multivarRevision = "\([0-9a-f]*\)".*/\1/p' "$repo_root/build.sbt")
case "$multivar_revision" in
  ''|*[!0-9a-f]*) echo 'Invalid multivarRevision in build.sbt' >&2; exit 1 ;;
esac
[ "${#multivar_revision}" -eq 40 ] || exit 1

provider_tmp=$(mktemp -d "${TMPDIR:-/tmp}/scalafim-providers.XXXXXX")
trap 'rm -rf "$provider_tmp"' EXIT HUP INT TERM
git clone --quiet --no-checkout --filter=blob:none \
  https://github.com/canardlapin/multivar.git "$provider_tmp/multivar"
git -C "$provider_tmp/multivar" checkout --quiet --detach "$multivar_revision"
[ "$(git -C "$provider_tmp/multivar" rev-parse HEAD)" = "$multivar_revision" ]

# Multivar compiles against Gale's artifact while ScalaFIM also loads Gale from
# source. Different revisions can compile successfully but fail at JS linking
# (or JVM invocation) when an API gained a defaulted parameter. Reject that
# mixed graph before publishing dependencies or starting the expensive gates.
gale_revision=$(sed -n 's/^lazy val galeRevision = "\([0-9a-f]*\)".*/\1/p' "$repo_root/build.sbt")
multivar_gale_revision=$(sed -n 's/^lazy val galeRevision = "\([0-9a-f]*\)".*/\1/p' "$provider_tmp/multivar/build.sbt")
if [ "${#gale_revision}" -ne 40 ] || [ "$gale_revision" != "$multivar_gale_revision" ]; then
  echo "Gale revision mismatch: ScalaFIM=$gale_revision Multivar=$multivar_gale_revision" >&2
  exit 1
fi
# Source providers that load their own Gale build carry the same hazard: each
# must pin ScalaFIM's Gale so one Gale resolves throughout the graph.
for provider in alder graph4s image4s mvpa4s reframe4s; do
  revision=$(sed -n "s/^lazy val ${provider}Revision = \"\\([0-9a-f]*\\)\".*/\\1/p" "$repo_root/build.sbt")
  [ "${#revision}" -eq 40 ] || { echo "Invalid ${provider}Revision in build.sbt" >&2; exit 1; }
  git clone --quiet --no-checkout --filter=blob:none \
    "https://github.com/canardlapin/$provider.git" "$provider_tmp/$provider"
  provider_gale=$(git -C "$provider_tmp/$provider" show "$revision:build.sbt" |
    sed -n -e 's/^lazy val galeRevision = "\([0-9a-f]*\)".*/\1/p' \
      -e '/^lazy val galeRevision =$/{n;s/^ *"\([0-9a-f]*\)".*/\1/p;}')
  if [ "$provider_gale" != "$gale_revision" ]; then
    echo "Gale revision mismatch: ScalaFIM=$gale_revision $provider=$provider_gale" >&2
    exit 1
  fi
done
# Mesh topology and anatomical routing exchange graph4s values. Local checkout
# overrides can hide stale transitive pins, so also check their default graph.
graph_revision=$(sed -n 's/^lazy val graph4sRevision = "\([0-9a-f]*\)".*/\1/p' "$repo_root/build.sbt")
for provider in mesh4s neuroim4s; do
  revision=$(sed -n "s/^lazy val ${provider}Revision = \"\\([0-9a-f]*\\)\".*/\\1/p" "$repo_root/build.sbt")
  [ "${#revision}" -eq 40 ] || { echo "Invalid ${provider}Revision in build.sbt" >&2; exit 1; }
  git clone --quiet --no-checkout --filter=blob:none \
    "https://github.com/canardlapin/$provider.git" "$provider_tmp/$provider"
  provider_graph=$(git -C "$provider_tmp/$provider" show "$revision:build.sbt" |
    sed -n -e 's/^lazy val graph4sRevision = "\([0-9a-f]*\)".*/\1/p' \
      -e '/^lazy val graph4sRevision =$/{n;s/^ *"\([0-9a-f]*\)".*/\1/p;}')
  if [ "${#graph_revision}" -ne 40 ] || [ "$provider_graph" != "$graph_revision" ]; then
    echo "graph4s revision mismatch: ScalaFIM=$graph_revision $provider=$provider_graph" >&2
    exit 1
  fi
done
# Atlas volume policy exchanges neuroim4s values with ScalaFIM's mixed consumers.
# Check its immutable default as well; sibling overrides must not hide drift.
neuroim_revision=$(sed -n 's/^lazy val neuroim4sRevision = "\([0-9a-f]*\)".*/\1/p' "$repo_root/build.sbt")
atlas_revision=$(sed -n 's/^lazy val neuroatlas4sRevision = "\([0-9a-f]*\)".*/\1/p' "$repo_root/build.sbt")
[ "${#atlas_revision}" -eq 40 ] || { echo 'Invalid neuroatlas4sRevision in build.sbt' >&2; exit 1; }
git clone --quiet --no-checkout --filter=blob:none \
  https://github.com/canardlapin/neuroatlas4s.git "$provider_tmp/neuroatlas4s"
atlas_neuroim=$(git -C "$provider_tmp/neuroatlas4s" show "$atlas_revision:build.sbt" |
  sed -n 's/^lazy val neuroim4sRevision = "\([0-9a-f]*\)".*/\1/p')
if [ "${#neuroim_revision}" -ne 40 ] || [ "$atlas_neuroim" != "$neuroim_revision" ]; then
  echo "neuroim4s revision mismatch: ScalaFIM=$neuroim_revision neuroatlas4s=$atlas_neuroim" >&2
  exit 1
fi
GALE_CACHE="$provider_tmp/gale" \
  "$provider_tmp/multivar/tools/publish-gale-local.sh"
RESAMPLE4S_CACHE="$provider_tmp/resample4s" \
  "$provider_tmp/multivar/tools/publish-resample4s-local.sh"
