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

run_batch ecosystem-boundaries ecosystemBoundaries

# Run comparative compiler and platform controls first for prompt portability feedback.
# Keep the comparative court separate: its real synthetic trial arm is expensive.
run_batch phrf-comparison-jvm phrfComparisonJVM/test
run_batch phrf-comparison-js phrfComparisonJS/test

# Start a fresh sbt JVM for each bounded group. Scala.js linking and Node test
# runners otherwise accumulate enough heap to exhaust ordinary CI runners.
run_batch core-jvm \
  pipelineJVM/test responseJVM/test responseLawsJVM/test \
  latentJVM/test arJVM/test hrfJVM/test \
  scenarioTestkitJVM/test designJVM/test providerSpikeJVM/test
run_batch core-js \
  pipelineJS/test responseJS/test responseLawsJS/test \
  latentJS/test arJS/test hrfJS/test \
  scenarioTestkitJS/test designJS/test providerSpikeJS/test

run_batch neuroim neuroimTestAll
run_batch neuroim-transforms neuroimTransformTestAll
run_batch neuroim-routing neuroimRoutingTestAll
run_batch neuroim-view-jvm neuroimViewJvmTestAll
run_batch neuroim-view-js neuroimViewJsTestAll
run_batch neuroatlas-core neuroatlasCoreTestAll
run_batch neuroatlas-volume neuroatlasVolumeTestAll
run_batch neuroatlas-surface neuroatlasSurfaceTestAll
run_batch neuroatlas-routing neuroatlasRoutingTestAll
run_batch mvpa4s-core mvpa4sCoreTestAll
run_batch mvpa4s-predictive mvpa4sPredictiveTestAll
run_batch mvpa4s-locus mvpa4sLocusTestAll
run_batch mvpa4s-imaging scalafimMvpaImagingTestAll

run_batch neurosurf-Core neurosurfCoreTestAll
run_batch neurosurf-Spatial neurosurfSpatialTestAll
run_batch neurosurf-Fixture neurosurfFixtureTestAll
run_batch neurosurf-ViewJvm neurosurfViewJvmTestAll
run_batch neurosurf-ViewJs neurosurfViewJsTestAll
run_batch neurosurf-Browser neurosurfBrowserTestAll
run_batch neurosurf-ExamplesJvm neurosurfExamplesJvmTestAll

run_batch threshold-motion-jvm thresholdJVM/test motionJVM/test
run_batch threshold-motion-js thresholdJS/test motionJS/test

run_batch data-jvm \
  surfaceViewConnectivityJVM/test \
  spatialJVM/test \
  archiveJVM/test archiveLnaJVM/test \
  archivedResponseInteropJVM/test datasetJVM/test
run_batch data-js \
  surfaceViewConnectivityJS/test \
  spatialJS/test \
  archiveJS/test archiveLnaJS/test \
  archivedResponseInteropJS/test datasetJS/test

run_batch estimates-jvm \
  estimatesJVM/test estimatesIoJVM/test
run_batch estimates-js \
  estimatesJS/test estimatesIoJS/test

run_batch analysis-jvm \
  modelJVM/test fitJVM/test firstLevelLawsJVM/test mvpaPipelineJVM/test mvpaFitJVM/test
run_batch analysis-js \
  modelJS/test fitJS/test firstLevelLawsJS/test mvpaPipelineJS/test mvpaFitJS/test

run_batch downstream-jvm \
  connectivityJVM/test mvpaDatasetJVM/test mvpaArtifactsJVM/test mvpaSpatialJVM/test \
  groupJVM/test mvpaGroupJVM/test \
  fmriWorkflowJVM/test archiveZarrJVM/test datasetZarrJVM/test
run_batch downstream-js \
  connectivityJS/test mvpaDatasetJS/test mvpaArtifactsJS/test mvpaSpatialJS/test \
  groupJS/test mvpaGroupJS/test \
  fmriWorkflowJS/test archiveZarrJS/test datasetZarrJS/test

# The surface JavaFX host tests require a display and are intentionally absent.
# Volume JavaFX contract tests run in neuroim-view-jvm without the toolkit.
run_batch examples \
  atlasExamplesJVM/test workflowExamplesJVM/test
run_batch examples-js \
  workflowExamplesJS/test
