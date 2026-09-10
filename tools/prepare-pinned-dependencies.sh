#!/bin/sh
# Install Multivar's exact Gale artifact before loading the composite build.
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
GALE_CACHE="$provider_tmp/gale" "$provider_tmp/multivar/tools/publish-gale-local.sh"
