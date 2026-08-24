#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

output_dir=${SCALAFIM_BENCHMARK_OUT:-$repo_root/target/first-level-benchmarks}
mkdir -p "$output_dir"
output_dir=$(cd "$output_dir" && pwd)

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-first-level-benchmark-sbt}
mkdir -p "$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

sbt_args=(
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)
jmh_common="-wi 2 -i 4 -f 1 -t 1 -w 500ms -r 500ms -prof gc -rf json"

echo "[first-level-benchmark] compiling strict JMH projects"
sbt "${sbt_args[@]}" \
  hrfBenchJVM/Jmh/compile \
  fitBenchJVM/Jmh/compile \
  "hrfBenchJVM/Jmh/run $jmh_common -rff $output_dir/hrf-basis.json .*BasisResponseBenchmark.*" \
  "hrfBenchJVM/Jmh/run $jmh_common -p nScans=1200 -p nbasis=3 -p precision=0.1 -rff $output_dir/hrf-convolution.json .*RegressorConvolutionBenchmark.*" \
  "hrfBenchJVM/Jmh/run $jmh_common -p width=12.0 -p precision=0.05 -rff $output_dir/hrf-integration.json .*EpochIntegrationBenchmark.*" \
  "fitBenchJVM/Jmh/run $jmh_common -p timepoints=360 -p responses=128 -p order=4 -rff $output_dir/ar-estimation.json .*ArEstimationBenchmark.*" \
  "fitBenchJVM/Jmh/run $jmh_common -p timepoints=360 -p predictors=32 -p responses=128 -rff $output_dir/fit.json .*FirstLevelFitBenchmark.*"

python -S tools/benchmark/finalize_first_level_receipt.py \
  --raw "$output_dir/hrf-basis.json" \
  --raw "$output_dir/hrf-convolution.json" \
  --raw "$output_dir/hrf-integration.json" \
  --raw "$output_dir/ar-estimation.json" \
  --raw "$output_dir/fit.json" \
  --output "$output_dir/receipt.json"

echo "[first-level-benchmark] admission receipt: $output_dir/receipt.json"
