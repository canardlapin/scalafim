#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

if [[ -z "${JAVA_HOME:-}" ]]; then
  java_home=$(java -XshowSettings:properties -version 2>&1 | awk -F ' = ' '/^[[:space:]]*java.home = / { print $2; exit }')
  if [[ -z "$java_home" ]]; then
    echo "[first-level-release] could not resolve java.home from the selected java executable" >&2
    exit 1
  fi
  export JAVA_HOME="$java_home"
fi
if [[ ! -x "$JAVA_HOME/bin/java" ]]; then
  echo "[first-level-release] JAVA_HOME does not contain an executable bin/java: $JAVA_HOME" >&2
  exit 1
fi
export PATH="$JAVA_HOME/bin:$PATH"

output_dir=${SCALAFIM_RELEASE_OUT:-$repo_root/target/first-level-release}
mkdir -p "$output_dir/gates" "$output_dir/benchmark"
output_dir=$(cd "$output_dir" && pwd)
gate_dir="$output_dir/gates"
run_manifest="$output_dir/run-manifest.json"

rm -f "$gate_dir"/*.json "$gate_dir"/*.log
python3 -S tools/ci/finalize_first_level_release.py --initialize-run "$run_manifest"

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-first-level-release-sbt}
mkdir -p "$sbt_cache_root"
export SCALAFIM_SBT_CACHE_ROOT="$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

sbt_args=(
  "-Djava.awt.headless=true"
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)

gate_names=(
  documentation
  focused_first_level
  scientific_coverage
  compile_all
  test_all
  performance
)

gate_command() {
  case "$1" in
    documentation) printf '%s\n' 'python3 -S tools/docs/check_first_level_docs.py --check' ;;
    focused_first_level) printf '%s\n' 'bash tools/ci/first-level-gate.sh' ;;
    scientific_coverage) printf '%s\n' 'bash tools/ci/first-level-coverage.sh' ;;
    compile_all) printf '%s\n' 'bash tools/prepare-pinned-dependencies.sh && sbt scalafimCompileAll' ;;
    test_all) printf '%s\n' 'bash tools/ci/full-repository-tests.sh' ;;
    performance) printf '%s\n' 'bash tools/ci/first-level-benchmark.sh' ;;
    *) echo "unknown release gate: $1" >&2; return 2 ;;
  esac
}

write_skipped_gates() {
  local name
  for name in "${gate_names[@]}"; do
    local timestamp
    timestamp=$(date -u '+%Y-%m-%dT%H:%M:%SZ')
    printf 'skipped because the release court requires a clean source checkout\n' > "$gate_dir/$name.log"
    python3 -S tools/ci/finalize_first_level_release.py \
      --record-gate "$name" \
      --run-manifest "$run_manifest" \
      --status-dir "$gate_dir" \
      --gate-command "$(gate_command "$name")" \
      --gate-exit-code 125 \
      --gate-started-at "$timestamp" \
      --gate-ended-at "$timestamp" \
      --gate-log "$gate_dir/$name.log"
  done
}

run_gate() {
  local name=$1
  local logical_command=$2
  shift 2
  echo "[first-level-release] running $name"
  local started_at
  started_at=$(date -u '+%Y-%m-%dT%H:%M:%SZ')
  set +e
  "$@" > "$gate_dir/$name.log" 2>&1
  local exit_code=$?
  set -e
  local ended_at
  ended_at=$(date -u '+%Y-%m-%dT%H:%M:%SZ')
  python3 -S tools/ci/finalize_first_level_release.py \
    --record-gate "$name" \
    --run-manifest "$run_manifest" \
    --status-dir "$gate_dir" \
    --gate-command "$logical_command" \
    --gate-exit-code "$exit_code" \
    --gate-started-at "$started_at" \
    --gate-ended-at "$ended_at" \
    --gate-log "$gate_dir/$name.log"
  if (( exit_code != 0 )); then
    echo "[first-level-release] $name failed with exit code $exit_code" >&2
    tail -n 40 "$gate_dir/$name.log" >&2
  else
    echo "[first-level-release] $name passed"
  fi
}

if [[ -n "$(git status --porcelain --untracked-files=all)" ]]; then
  if [[ "${SCALAFIM_RELEASE_ALLOW_DIRTY_DIAGNOSTICS:-0}" != "1" ]]; then
    echo "[first-level-release] source checkout is dirty; recording a blocked report" >&2
    write_skipped_gates
    python3 -S tools/ci/finalize_first_level_release.py \
      --status-dir "$gate_dir" \
      --run-manifest "$run_manifest" \
      --benchmark docs/benchmarks/receipts/first-level-current.json \
      --output "$output_dir/report.json" || true
    echo "[first-level-release] blocked report: $output_dir/report.json" >&2
    exit 1
  fi
  echo "[first-level-release] dirty diagnostic mode; release eligibility remains impossible" >&2
fi

run_gate documentation \
  "$(gate_command documentation)" \
  python3 -S tools/docs/check_first_level_docs.py --check
run_gate focused_first_level \
  "$(gate_command focused_first_level)" \
  bash tools/ci/first-level-gate.sh
run_gate scientific_coverage \
  "$(gate_command scientific_coverage)" \
  bash tools/ci/first-level-coverage.sh

run_compile_all() {
  local provider_sbt_opts
  provider_sbt_opts="-Dsbt.boot.directory=$sbt_cache_root/boot -Dsbt.global.base=$sbt_cache_root/global -Dsbt.ivy.home=$sbt_cache_root/ivy -Dsbt.supershell=false"
  env \
    COURSIER_CACHE="$COURSIER_CACHE" \
    SBT_OPTS="${SBT_OPTS:+$SBT_OPTS }$provider_sbt_opts" \
    bash tools/prepare-pinned-dependencies.sh || return $?
  sbt "${sbt_args[@]}" scalafimCompileAll
}

run_gate compile_all \
  "$(gate_command compile_all)" \
  run_compile_all
run_gate test_all \
  "$(gate_command test_all)" \
  bash tools/ci/full-repository-tests.sh
benchmark_receipt=${SCALAFIM_RELEASE_BENCHMARK_RECEIPT:-}
if [[ -n "$benchmark_receipt" ]]; then
  run_gate performance \
    "python3 -S tools/benchmark/finalize_first_level_receipt.py --check $benchmark_receipt --require-release" \
    python3 -S tools/benchmark/finalize_first_level_receipt.py \
    --check "$benchmark_receipt" \
    --require-release
else
  run_gate performance \
    "$(gate_command performance)" \
    env SCALAFIM_BENCHMARK_OUT="$output_dir/benchmark" \
    bash tools/ci/first-level-benchmark.sh
  benchmark_receipt="$output_dir/benchmark/receipt.json"
fi

python3 -S tools/ci/finalize_first_level_release.py \
  --status-dir "$gate_dir" \
  --run-manifest "$run_manifest" \
  --benchmark "$benchmark_receipt" \
  --output "$output_dir/report.json"

echo "[first-level-release] eligible report: $output_dir/report.json"
