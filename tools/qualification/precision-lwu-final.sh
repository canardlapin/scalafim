#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$repo_root"
platform=${1:?expected JVM or JS}
case "$platform" in JVM|JS) ;; *) echo "unsupported platform: $platform" >&2; exit 2 ;; esac
qualification_dir=${SCALAFIM_QUALIFICATION_DIR:-$repo_root/diagnostics/$platform}
mkdir -p "$qualification_dir"
export SCALAFIM_QUALIFICATION_DIR="$qualification_dir"
# A final gate must resolve the committed public providers. Clear inherited
# JVM property injection; task arguments below contain no provider override.
unset JAVA_TOOL_OPTIONS JDK_JAVA_OPTIONS SBT_OPTS
export SBT_OPTS='-Xmx4g -XX:ActiveProcessorCount=2 -Dsbt.supershell=false -Dsbt.color=false'
python3 -S tools/qualification/verify-final-inputs.py
uname -a > "$qualification_dir/host.txt"
cat /etc/os-release >> "$qualification_dir/host.txt"
java -version 2>> "$qualification_dir/host.txt"
node --version >> "$qualification_dir/host.txt"
node -p 'JSON.stringify(process.versions)' >> "$qualification_dir/host.txt"
ldd --version >> "$qualification_dir/host.txt"
git rev-parse HEAD > "$qualification_dir/source-commit.txt"

cache_root=${SCALAFIM_SBT_CACHE_ROOT:-${RUNNER_TEMP:-/tmp}/scalafim-final-$platform}
mkdir -p "$cache_root"
export COURSIER_CACHE="$cache_root/coursier"
sbt_args=("-Dsbt.boot.directory=$cache_root/boot" "-Dsbt.global.base=$cache_root/global"
  "-Dsbt.ivy.home=$cache_root/ivy" "-Dsbt.supershell=false" "-J-Xmx4g" "-J-XX:ActiveProcessorCount=2")
failed=0
run_stage() {
  local label=$1
  shift
  local result
  echo "[precision-lwu-final] $platform: $label"
  set +e
  sbt "${sbt_args[@]}" "$@" > "$qualification_dir/$label.log" 2>&1
  result=$?
  set -e
  echo "$result" > "$qualification_dir/$label-exit-code.txt"
  if [[ "$result" -ne 0 ]]; then failed=1; fi
}
# Each stage is a separate sbt process. No broad first-level/throughput spike
# is invoked, and the original 60-minute LWU suite timeout remains untouched.
run_stage budget-kernel "design$platform/testOnly scalafim.fmri.design.hrf.KernelBasisBudgetSuite scalafim.fmri.design.HrfKernelBasisSuite"
run_stage compact-cohort "firstLevelLaws$platform/testOnly scalafim.fmri.laws.profile.CompactConditionRuntimeSuite scalafim.fmri.laws.profile.ConditionStationarityRoundoffSuite"
run_stage lwu-accuracy "set firstLevelLaws$platform / Test / unmanagedSources += file(\"tools/qualification/LwuAccuracyOnlySuite.scala\")"   "firstLevelLaws$platform/testOnly scalafim.fmri.laws.profile.LwuAccuracyOnlySuite"
run_stage full-fit "fit$platform/test"
python3 -S tools/qualification/summarize-final-gates.py "$qualification_dir" "$platform"
exit "$failed"
