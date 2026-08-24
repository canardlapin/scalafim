#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"

if [[ -z "${JAVA_HOME:-}" ]]; then
  java_home=$(java -XshowSettings:properties -version 2>&1 | awk -F ' = ' '/^[[:space:]]*java.home = / { print $2; exit }')
  if [[ -z "$java_home" ]]; then
    echo "[full-repository-tests] could not resolve java.home from the selected java executable" >&2
    exit 1
  fi
  export JAVA_HOME="$java_home"
fi
if [[ ! -x "$JAVA_HOME/bin/java" ]]; then
  echo "[full-repository-tests] JAVA_HOME does not contain an executable bin/java: $JAVA_HOME" >&2
  exit 1
fi
export PATH="$JAVA_HOME/bin:$PATH"

sbt_cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-${TMPDIR:-/tmp}}/scalafim-full-repository-sbt}
mkdir -p "$sbt_cache_root"
export COURSIER_CACHE=${COURSIER_CACHE:-$sbt_cache_root/coursier}

sbt_args=(
  "-Djava.awt.headless=true"
  "-Dsbt.boot.directory=$sbt_cache_root/boot"
  "-Dsbt.global.base=$sbt_cache_root/global"
  "-Dsbt.ivy.home=$sbt_cache_root/ivy"
  "-Dsbt.supershell=false"
)

run_batch() {
  local name=$1
  shift
  echo "[full-repository-tests] running $name"
  sbt "${sbt_args[@]}" "$@"
}

# Start a fresh sbt JVM for each bounded group. Scala.js linking and Node test
# runners otherwise accumulate enough heap to exhaust ordinary CI runners.
run_batch core-jvm \
  locusDataJVM/test pipelineJVM/test responseJVM/test responseLawsJVM/test \
  latentJVM/test arJVM/test hrfJVM/test hrfLawsJVM/test \
  scenarioTestkitJVM/test designJVM/test
run_batch core-js \
  locusDataJS/test pipelineJS/test responseJS/test responseLawsJS/test \
  latentJS/test arJS/test hrfJS/test hrfLawsJS/test \
  scenarioTestkitJS/test designJS/test

run_batch image-surface-jvm \
  imageJVM/test imageViewJVM/test imageViewJava2dJVM/test thresholdJVM/test \
  motionJVM/test surfaceJVM/test surfaceViewJVM/test surfaceViewRasterJVM/test
run_batch image-surface-js \
  imageJS/test imageViewJS/test imageViewCanvasJS/test thresholdJS/test \
  motionJS/test surfaceJS/test surfaceViewJS/test surfaceViewRasterJS/test \
  surfaceViewThreeJS/test

run_batch data-jvm \
  surfaceViewConnectivityJVM/test surfaceViewExamplesJVM/test spatialJVM/test \
  atlasJVM/test archiveJVM/test archiveLnaJVM/test \
  archivedResponseInteropJVM/test datasetJVM/test
run_batch data-js \
  surfaceViewConnectivityJS/test surfaceViewExamplesJS/test spatialJS/test \
  atlasJS/test archiveJS/test archiveLnaJS/test \
  archivedResponseInteropJS/test datasetJS/test

run_batch analysis-jvm \
  modelJVM/test fitJVM/test firstLevelLawsJVM/test mvpaJVM/test mvpaFitJVM/test
run_batch analysis-js \
  modelJS/test fitJS/test firstLevelLawsJS/test mvpaJS/test mvpaFitJS/test

run_batch downstream-jvm \
  connectivityJVM/test mvpaDatasetJVM/test mvpaSpatialJVM/test groupJVM/test \
  fmriWorkflowJVM/test archiveZarrJVM/test datasetZarrJVM/test
run_batch downstream-js \
  connectivityJS/test mvpaDatasetJS/test mvpaSpatialJS/test groupJS/test \
  fmriWorkflowJS/test archiveZarrJS/test datasetZarrJS/test

# The JavaFX host tests require a display and are intentionally absent. Their
# source still compiles in scalafimCompileAll; portable examples run here.
run_batch examples \
  surfaceExamplesJVM/test atlasExamplesJVM/test workflowExamplesJVM/test
