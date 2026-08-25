#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

browser_dir="$repo_root/modules/surface-view-three/browser"
sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-surface-browser-sbt}
npm_cache=${SCALAFIM_NPM_CACHE:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-surface-browser-npm}
mkdir -p "$sbt_cache_root" "$npm_cache"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}
export npm_config_cache=${npm_config_cache:-$npm_cache}

npm --prefix "$browser_dir" ci
if [[ ${1:-} == "--install-browser-deps" ]]; then
  npm --prefix "$browser_dir" exec -- playwright install --with-deps chromium
else
  npm --prefix "$browser_dir" exec -- playwright install chromium
fi

sbt \
  "-Dsbt.boot.directory=$sbt_cache_root/boot" \
  "-Dsbt.global.base=$sbt_cache_root/global" \
  "-Dsbt.ivy.home=$sbt_cache_root/ivy" \
  -Dsbt.supershell=false \
  'set surfaceViewThreeJS / scalaJSLinkerConfig ~= (_.withModuleKind(org.scalajs.linker.interface.ModuleKind.ESModule))' \
  surfaceViewThreeJS/fastLinkJS

npm --prefix "$browser_dir" run test:smoke
