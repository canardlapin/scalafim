#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-first-level-coverage-sbt}
mkdir -p "$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

sbt_args=(
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)
modules=(arJVM hrfJVM designJVM modelJVM fitJVM)

echo "[first-level-coverage] enforcing per-module scientific coverage floors"
for module in "${modules[@]}"; do
  echo "[first-level-coverage] $module"
  sbt "${sbt_args[@]}" \
    "set $module / coverageEnabled := true" \
    "$module/test" \
    "$module/coverageReport"
done
echo "[first-level-coverage] all module floors passed"
