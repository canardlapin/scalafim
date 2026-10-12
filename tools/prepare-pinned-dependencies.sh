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
GALE_CACHE="$provider_tmp/gale" \
  "$provider_tmp/multivar/tools/publish-gale-local.sh"
RESAMPLE4S_CACHE="$provider_tmp/resample4s" \
  "$provider_tmp/multivar/tools/publish-resample4s-local.sh"
