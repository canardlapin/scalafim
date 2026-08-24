#!/usr/bin/env python3
"""Run a small, deterministic mutation court for AR and missing-data policy."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile


REPO = Path(__file__).resolve().parents[2]
DEFAULT_OUT = REPO / "target" / "mutation-pilot"


@dataclass(frozen=True)
class Mutation:
  name: str
  relative_path: str
  before: str
  after: str
  test_task: str


MUTATIONS = (
  Mutation(
    name="ar-autocovariance-pair-count",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/ArEstimation.scala",
    before="pairCounts(lag) += 1L",
    after="pairCounts(lag) += 0L",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite",
  ),
  Mutation(
    name="ar-run-local-centering",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/ArEstimation.scala",
    before="runSums(segment.runIndex * residuals.cols + col) / runRows(segment.runIndex).toDouble",
    after="runSums(segment.runIndex * residuals.cols + col) / residuals.rows.toDouble",
    test_task="firstLevelLawsJVM/testOnly scalafim.fmri.laws.ArGeneratedLawsSuite",
  ),
  Mutation(
    name="ar-surviving-row-pooling",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/ArEstimation.scala",
    before="estimate.observations.toDouble / observations.toDouble",
    after="estimate.observations.toDouble / estimates.length.toDouble",
    test_task="firstLevelLawsJVM/testOnly scalafim.fmri.laws.ArGeneratedLawsSuite",
  ),
  Mutation(
    name="ar-censor-exclusion",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/TimeSegment.scala",
    before="if excludedRows.contains(row) then",
    after="if false then",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite",
  ),
  Mutation(
    name="ar-whitening-segment-reset",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/WhiteningPlan.scala",
    before="""while lag < coefficients.phi.length do
          val laggedRow = row - lag - 1
          if laggedRow >= segment.start then""",
    after="""while lag < coefficients.phi.length do
          val laggedRow = row - lag - 1
          if laggedRow >= 0 then""",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite scalafim.fmri.ar.WhiteningPlanSuite",
  ),
  Mutation(
    name="ar-pacf-forward-recursion-sign",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/Pacf.scala",
    before="previous(j) - km * previous((m - 2) - j)",
    after="previous(j) + km * previous((m - 2) - j)",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite",
  ),
  Mutation(
    name="ar-pacf-inverse-recursion-denominator",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/Pacf.scala",
    before="math.max(1.0 - km * km, eps)",
    after="math.max(1.0 + km * km, eps)",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite",
  ),
  Mutation(
    name="ar-yule-walker-recursion-sign",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/ArEstimation.scala",
    before="acc -= previous(j - 1) * gamma.at(ArLag.unsafe(m - j))",
    after="acc += previous(j - 1) * gamma.at(ArLag.unsafe(m - j))",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite",
  ),
  Mutation(
    name="ar-acf-normalization",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/AutocorrelationDiagnostics.scala",
    before="num / denom",
    after="num / (denom + 1.0)",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite",
  ),
  Mutation(
    name="ar-censor-reset-boundary",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/TimeSegment.scala",
    before=".map(_ + 1)",
    after=".map(value => value)",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite scalafim.fmri.ar.WhiteningPlanSuite",
  ),
  Mutation(
    name="ar-whitening-ar-sign",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/WhiteningPlan.scala",
    before="value -= coefficients.phi(lag) * input(laggedRow, col)",
    after="value += coefficients.phi(lag) * input(laggedRow, col)",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite scalafim.fmri.ar.WhiteningPlanSuite",
  ),
  Mutation(
    name="ar-whitening-ma-sign",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/WhiteningPlan.scala",
    before="value -= coefficients.theta(lag) * out(laggedRow, col)",
    after="value += coefficients.theta(lag) * out(laggedRow, col)",
    test_task="arJVM/testOnly scalafim.fmri.ar.FmriArParitySuite scalafim.fmri.ar.WhiteningPlanSuite",
  ),
  Mutation(
    name="ar-exact-first-scale-direction",
    relative_path="modules/ar/shared/src/main/scala/scalafim/fmri/ar/WhiteningPlan.scala",
    before="value *= firstScale",
    after="value /= firstScale",
    test_task="arJVM/testOnly scalafim.fmri.ar.WhiteningPlanSuite",
  ),
  Mutation(
    name="missing-response-retention",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MatrixAdapters.scala",
    before="if finite then retainedPositions += voxel",
    after="if true then retainedPositions += voxel",
    test_task="firstLevelLawsJVM/testOnly scalafim.fmri.laws.MissingResponseGeneratedLawsSuite",
  ),
  Mutation(
    name="missing-pattern-grouping-key",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
    before="positionsByRows.update(rows, positionsByRows.getOrElse(rows, Vector.empty) :+ voxelPosition)",
    after="positionsByRows.update(series.timepoints.indices.toVector, positionsByRows.getOrElse(series.timepoints.indices.toVector, Vector.empty) :+ voxelPosition)",
    test_task="firstLevelLawsJVM/testOnly scalafim.fmri.laws.MissingResponseGeneratedLawsSuite",
  ),
  Mutation(
    name="missing-source-timepoint-identity",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
    before="val timepoints = rowPositions.map(series.timepoints)",
    after="val timepoints = rowPositions.indices.toVector",
    test_task="fitJVM/testOnly scalafim.fmri.fit.FitPlanExecutorSuite",
  ),
  Mutation(
    name="missing-chunk-selection-timepoints",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
    before="time = IndexSelection.indices(pattern.sourceTimepoints*),",
    after="time = IndexSelection.indices(pattern.rows*),",
    test_task="firstLevelLawsJVM/testOnly scalafim.fmri.laws.MissingResponseGeneratedLawsSuite",
  ),
  Mutation(
    name="missing-selected-weight-alignment",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
    before="VolumeWeighting.Fixed(pattern.rows.map(weights), FixedWeightAlignment.SelectedRows)",
    after="VolumeWeighting.Fixed(weights.take(pattern.rows.length), FixedWeightAlignment.SelectedRows)",
    test_task="fitJVM/testOnly scalafim.fmri.fit.FitPlanExecutorSuite",
  ),
  Mutation(
    name="missing-insufficient-df-status",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
    before="Some(VoxelFitStatus.InsufficientResidualDegreesOfFreedom)",
    after="Some(VoxelFitStatus.RankDeficientObservedDesign)",
    test_task="fitJVM/testOnly scalafim.fmri.fit.FitPlanExecutorSuite",
  ),
  Mutation(
    name="missing-no-observations-status",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
    before="VoxelFitStatus.NoObservedResponses",
    after="VoxelFitStatus.Constant",
    test_task="fitJVM/testOnly scalafim.fmri.fit.FitPlanExecutorSuite",
  ),
  Mutation(
    name="missing-exclusion-source-order",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
    before="masked.sourceVoxels.flatMap(byVoxel.get)",
    after="masked.sourceVoxels.flatMap(byVoxel.get).reverse",
    test_task="fitJVM/testOnly scalafim.fmri.fit.FitPlanExecutorSuite",
  ),
  Mutation(
    name="fixed-effects-zero-variance",
    relative_path="modules/fit/shared/src/main/scala/scalafim/fmri/fit/FixedEffects.scala",
    before="else if variance <= 0.0 then VoxelFitStatus.ZeroResidualVariance",
    after="else if variance < 0.0 then VoxelFitStatus.ZeroResidualVariance",
    test_task="firstLevelLawsJVM/testOnly scalafim.fmri.fit.FixedEffectsStatusGeneratedLawsSuite",
  ),
)


def arguments() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--list", action="store_true", help="list mutant names and exit")
  parser.add_argument("--mutant", action="append", default=[], help="run only the named mutant; repeatable")
  parser.add_argument("--skip-baseline", action="store_true", help="skip the baseline suite (diagnostic use only)")
  parser.add_argument(
    "--output",
    type=Path,
    default=Path(os.environ.get("SCALAFIM_MUTATION_OUT", DEFAULT_OUT)),
    help="directory for logs and summary.json",
  )
  return parser.parse_args()


def ignored(_directory: str, names: list[str]) -> set[str]:
  excluded = {
    ".bsp",
    ".coursier-cache",
    ".git",
    ".ivy2",
    ".metals",
    ".mote",
    ".ruff_cache",
    ".sbt-boot",
    ".sbt-global",
    ".scala-build",
    "graphify-out",
    "node_modules",
    "target",
    "vendor",
  }
  return {name for name in names if name in excluded}


def sbt_command(cache_root: Path, task: str) -> list[str]:
  return [
    "sbt",
    f"-Dsbt.boot.directory={cache_root / 'boot'}",
    f"-Dsbt.global.base={cache_root / 'global'}",
    f"-Dsbt.ivy.home={cache_root / 'ivy'}",
    "-Dsbt.supershell=false",
    task,
  ]


def run_task(worktree: Path, cache_root: Path, task: str, log_path: Path) -> subprocess.CompletedProcess[str]:
  environment = os.environ.copy()
  environment.setdefault("COURSIER_CACHE", str(cache_root / "coursier"))
  completed = subprocess.run(
    sbt_command(cache_root, task),
    cwd=worktree,
    env=environment,
    check=False,
    capture_output=True,
    text=True,
  )
  log_path.write_text(completed.stdout + completed.stderr)
  return completed


def apply_mutation(path: Path, mutation: Mutation) -> str:
  original = path.read_text()
  occurrences = original.count(mutation.before)
  if occurrences != 1:
    raise RuntimeError(
      f"{mutation.name}: expected one source match in {mutation.relative_path}, found {occurrences}"
    )
  path.write_text(original.replace(mutation.before, mutation.after, 1))
  return original


def main() -> int:
  args = arguments()
  if args.list:
    for mutation in MUTATIONS:
      print(mutation.name)
    return 0

  requested = set(args.mutant)
  unknown = requested - {mutation.name for mutation in MUTATIONS}
  if unknown:
    raise SystemExit("unknown mutants: " + ", ".join(sorted(unknown)))
  selected = [mutation for mutation in MUTATIONS if not requested or mutation.name in requested]
  output = args.output.resolve()
  output.mkdir(parents=True, exist_ok=True)
  cache_root = Path(
    os.environ.get(
      "SCALAFIM_SBT_CACHE_ROOT",
      str(Path(tempfile.gettempdir()) / "scalafim-mutation-pilot-sbt"),
    )
  ).resolve()
  cache_root.mkdir(parents=True, exist_ok=True)

  with tempfile.TemporaryDirectory(prefix="scalafim-mutation-pilot-") as temporary:
    worktree = Path(temporary) / "scalafim"
    shutil.copytree(REPO, worktree, ignore=ignored)

    baseline_tasks = sorted({mutation.test_task for mutation in selected})
    if not args.skip_baseline:
      baseline = run_task(
        worktree,
        cache_root,
        ";" + ";".join(baseline_tasks),
        output / "baseline.log",
      )
      if baseline.returncode != 0:
        print(f"baseline failed; inspect {output / 'baseline.log'}", flush=True)
        return 2
      print(f"baseline passed ({len(baseline_tasks)} focused tasks)", flush=True)

    results: list[dict[str, object]] = []
    failed = False
    for mutation in selected:
      source = worktree / mutation.relative_path
      original = apply_mutation(source, mutation)
      try:
        completed = run_task(
          worktree,
          cache_root,
          mutation.test_task,
          output / f"{mutation.name}.log",
        )
      finally:
        source.write_text(original)

      log = (output / f"{mutation.name}.log").read_text()
      compile_failure = "Compilation failed" in log
      if completed.returncode == 0:
        status = "survived"
        failed = True
      elif compile_failure:
        status = "invalid"
        failed = True
      else:
        status = "killed"
      print(f"{mutation.name}: {status}", flush=True)
      results.append(
        {
          "log": f"{mutation.name}.log",
          "mutant": mutation.name,
          "source": mutation.relative_path,
          "status": status,
          "test_task": mutation.test_task,
        }
      )

    summary = {
      "killed": sum(result["status"] == "killed" for result in results),
      "mutants": results,
      "schema": "scalafim-ar-na-mutation-pilot/v1",
      "status": "pass" if not failed else "fail",
      "total": len(results),
    }
    (output / "summary.json").write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
    print(f"summary: {output / 'summary.json'}", flush=True)
    return 1 if failed else 0


if __name__ == "__main__":
  raise SystemExit(main())
