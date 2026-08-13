#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

echo "[first-level-ci] repository: $repo_root"

scala_version=$(sed -n 's/.*scalaVersion := "\([^"]*\)".*/\1/p' build.sbt | head -n 1)
sbt_version=$(sed -n 's/^sbt.version=//p' project/build.properties | head -n 1)
if [[ -z "$scala_version" || -z "$sbt_version" ]]; then
  echo "[first-level-ci] could not read the declared Scala/sbt versions" >&2
  exit 1
fi
echo "[first-level-ci] declared Scala $scala_version"
echo "[first-level-ci] declared sbt $sbt_version"

node_version=$(node --version)
node_major=${node_version#v}
node_major=${node_major%%.*}
if (( node_major < 20 )); then
  echo "[first-level-ci] Node.js 20 or newer is required; found $node_version" >&2
  exit 1
fi
echo "[first-level-ci] Node.js $node_version"

java_version=$(java -version 2>&1 | awk -F '"' '/version/ {print $2; exit}')
java_major=${java_version%%.*}
if [[ "$java_major" == "1" ]]; then
  java_major=${java_version#1.}
  java_major=${java_major%%.*}
fi
if [[ -z "$java_major" ]] || (( java_major < 17 )); then
  echo "[first-level-ci] JDK 17 or newer is required; found ${java_version:-unknown}" >&2
  exit 1
fi
echo "[first-level-ci] JDK $java_version"

echo "[first-level-ci] validating scenario manifest with Python stdlib only"
python -S tools/scenarios/validate_manifest.py docs/scenarios/manifest.json

echo "[first-level-ci] checking checked-in fixture coherence without external packages"
python -S -m unittest tools/r-parity/test_receipt_tools.py
python -S tools/r-parity/check_receipts.py

echo "[first-level-ci] checking executable documentation and benchmark receipt coherence"
python -S tools/docs/check_first_level_docs.py --check
python -S tools/benchmark/finalize_first_level_receipt.py \
  --check docs/benchmarks/receipts/first-level-current.json
python -S tools/ci/finalize_first_level_release.py \
  --check docs/release-report.json

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-first-level-sbt}
mkdir -p "$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

sbt_args=(
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)
gates=(
  firstLevelLawsJVM/Test/scalafmtCheck
  scenarioTestkitJVM/test
  scenarioTestkitJS/test
  arJVM/test
  arJS/test
  hrfJVM/test
  hrfJS/test
  hrfLawsJVM/test
  hrfLawsJS/test
  designJVM/test
  designJS/test
  modelJVM/test
  modelJS/test
  fitJVM/test
  fitJS/test
  firstLevelLawsJVM/test
  firstLevelLawsJS/test
)

echo "[first-level-ci] invoking focused gates:"
for gate in "${gates[@]}"; do
  echo "  - $gate"
done
sbt "${sbt_args[@]}" "${gates[@]}"
echo "[first-level-ci] focused first-level gates passed"
