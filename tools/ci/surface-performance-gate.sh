#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

output_dir=${SCALAFIM_SURFACE_PERFORMANCE_OUT:-$repo_root/target/surface-performance}
mkdir -p "$output_dir"
output_dir=$(cd "$output_dir" && pwd)

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-surface-performance-sbt}
mkdir -p "$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

sbt_args=(
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)

sbt "${sbt_args[@]}" \
  "surfaceViewJVM/Test/runMain scalafim.surface.view.SurfaceCompilerAllocationProbe" \
  2>&1 | tee "$output_dir/compiler.log"

sbt "${sbt_args[@]}" \
  "surfaceViewJVM/Test/runMain scalafim.surface.view.SurfaceMorphAllocationProbe" \
  2>&1 | tee "$output_dir/morph.log"

SCALAFIM_SURFACE_TOPOLOGY_CASE=fsaverage5 sbt "${sbt_args[@]}" \
  'set surfaceJS / Test / scalaJSUseTestModuleInitializer := false' \
  'set surfaceJS / Test / scalaJSUseMainModuleInitializer := true' \
  'set surfaceJS / Test / mainClass := Some("scalafim.surface.SurfaceTopologyJsProbe")' \
  'surfaceJS / Test / run' \
  2>&1 | tee "$output_dir/js-fsaverage5.log"

SCALAFIM_SURFACE_TOPOLOGY_CASE=cortical sbt "${sbt_args[@]}" \
  'set surfaceJS / Test / scalaJSUseTestModuleInitializer := false' \
  'set surfaceJS / Test / scalaJSUseMainModuleInitializer := true' \
  'set surfaceJS / Test / mainClass := Some("scalafim.surface.SurfaceTopologyJsProbe")' \
  'surfaceJS / Test / run' \
  2>&1 | tee "$output_dir/js-cortical.log"

python -S tools/ci/finalize_surface_performance.py \
  --input "$output_dir/compiler.log" \
  --input "$output_dir/morph.log" \
  --input "$output_dir/js-fsaverage5.log" \
  --input "$output_dir/js-cortical.log" \
  --output "$output_dir/receipt.json"

echo "[surface-performance] admission receipt: $output_dir/receipt.json"
