#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-mvpa-sbt}
mkdir -p "$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

echo "[mvpa-ci] checking checked-in oracle freshness without invoking R"
python3 -m unittest tools/r-parity/test_mvpa_oracle_manifest.py
python3 tools/r-parity/mvpa_oracle_manifest.py

echo "[mvpa-ci] checking the external-conformance study without invoking Python toolkits or Octave"
python3 tools/reference/mvpa-conformance/validate_study.py
python3 tools/reference/mvpa-conformance/generate_scala_fixture.py \
  --check modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/scenarios/MvpaExternalReferenceFixture.scala
python3 tools/reference/mvpa-conformance/measure_ergonomics.py \
  --check docs/audits/mvpa-conformance/ergonomics.csv

sbt_args=(
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)
gates=(
  mvpaJVM/Compile/scalafmtCheck
  mvpaJVM/Test/scalafmtCheck
  mvpaJS/Compile/scalafmtCheck
  mvpaJS/Test/scalafmtCheck
  mvpaJVM/compile
  mvpaJS/compile
  mvpaBenchJVM/Jmh/compile
  mvpaJVM/test
  mvpaJS/test
  mvpaJS/fullLinkJS
  workflowExamplesJVM/test
)

echo "[mvpa-ci] invoking the unified MVPA gates:"
for gate in "${gates[@]}"; do
  echo "  - $gate"
done
sbt "${sbt_args[@]}" "${gates[@]}"
echo "[mvpa-ci] oracle and conformance freshness, strict JVM, Scala.js, JMH compile, formatting, optimized-link, and consumer gates passed"
