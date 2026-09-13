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
if (( node_major < 24 )); then
  echo "[first-level-ci] Node.js 24 or newer is required; found $node_version" >&2
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
python3 -S tools/ci/check_test_inventory.py --check
python3 -S tools/scenarios/validate_manifest.py docs/scenarios/manifest.json

echo "[first-level-ci] checking checked-in fixture coherence without external packages"
python3 -S -m unittest tools/r-parity/test_receipt_tools.py
python3 -S tools/r-parity/check_receipts.py

echo "[first-level-ci] checking executable documentation and benchmark receipt coherence"
python3 -S tools/docs/check_first_level_docs.py --check

# Checked-in benchmark and release reports are historical transparent
# snapshots. Exact-candidate reports are necessarily generated after checkout
# and are validated by first-level-release.sh from its evidence directory.

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-first-level-sbt}
mkdir -p "$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

sbt_args=(
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)
run_batch() {
  local name=$1
  shift
  echo "[first-level-ci] running $name"
  sbt "${sbt_args[@]}" "$@"
}

# Each group gets a fresh sbt JVM so Scala.js linking and Node runners cannot
# accumulate across the whole first-level court.
run_batch formatting firstLevelLawsJVM/Test/scalafmtCheck
run_batch first-level-jvm \
  scenarioTestkitJVM/test arJVM/test hrfJVM/test hrfLawsJVM/test \
  designJVM/test modelJVM/test fitJVM/test firstLevelLawsJVM/test
run_batch first-level-js-a \
  scenarioTestkitJS/test arJS/test hrfJS/test hrfLawsJS/test
run_batch first-level-js-b \
  designJS/test modelJS/test fitJS/test firstLevelLawsJS/test
echo "[first-level-ci] focused first-level gates passed"
