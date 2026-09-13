#!/usr/bin/env python3
"""Build or validate the first-level JMH admission receipt."""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
from pathlib import Path
import re
import subprocess
import sys
from typing import Any


REPO = Path(__file__).resolve().parents[2]
BUDGETS = REPO / "tools" / "benchmark" / "first-level-budgets.json"
DEFAULT_RECEIPT = REPO / "docs" / "benchmarks" / "receipts" / "first-level-current.json"
SCHEMA = "scalafim-first-level-benchmark-receipt/v2"
SOURCE_PATHS = (
  "build.sbt",
  "benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/BasisResponseBenchmark.scala",
  "benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/DenseDriveBenchmark.scala",
  "benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/RegressorConvolutionBenchmark.scala",
  "benchmarks/fit-jvm/src/main/scala/scalafim/fmri/ar/ArEstimationBenchmark.scala",
  "benchmarks/fit-jvm/src/main/scala/scalafim/fmri/fit/FirstLevelFitBenchmark.scala",
  "modules/design/shared/src/main/scala/scalafim/fmri/design/DesignSchema.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/FitBlock.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/Gls.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/MatrixAdapters.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/Ols.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/ResponsePreparation.scala",
  "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/Basis.scala",
  "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/HrfFunctions.scala",
  "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/Primitive.scala",
  "modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/regressor/Regressor.scala",
  "modules/model/shared/src/main/scala/scalafim/fmri/model/DesignBlock.scala",
  "tools/benchmark/first-level-budgets.json",
  "tools/benchmark/finalize_first_level_receipt.py",
  "tools/ci/first-level-benchmark.sh",
)


def load_object(path: Path) -> dict[str, Any]:
  value = json.loads(path.read_text())
  if not isinstance(value, dict):
    raise SystemExit(f"expected a JSON object in {path}")
  return value


def sha256(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def canonical_sha256(value: Any) -> str:
  encoded = json.dumps(
    value, ensure_ascii=False, allow_nan=False, sort_keys=True, separators=(",", ":")
  ).encode("utf-8")
  return hashlib.sha256(encoded).hexdigest()


def git(*args: str) -> str:
  completed = subprocess.run(
    ["git", *args], cwd=REPO, check=False, capture_output=True, text=True
  )
  if completed.returncode != 0:
    raise SystemExit(completed.stderr.strip() or "git command failed")
  return completed.stdout.strip()


def source_hashes() -> dict[str, str]:
  hashes: dict[str, str] = {}
  for relative in SOURCE_PATHS:
    path = REPO / relative
    if not path.is_file():
      raise SystemExit(f"missing benchmark source: {relative}")
    hashes[relative] = sha256(path)
  return hashes


def expected_params(name: str, budgets: dict[str, Any]) -> dict[str, str]:
  profile = budgets.get("profile")
  if not isinstance(profile, dict):
    raise SystemExit("benchmark budgets must contain a profile object")
  if ".RegressorConvolutionBenchmark." in name:
    key = "hrf_convolution"
  elif ".EpochIntegrationBenchmark." in name:
    key = "hrf_integration"
  elif ".ArEstimationBenchmark." in name:
    key = "ar_estimation"
  elif ".FirstLevelFitBenchmark." in name:
    key = "fit"
  else:
    return {}
  params = profile.get(key)
  if not isinstance(params, dict) or any(not isinstance(k, str) or not isinstance(v, str) for k, v in params.items()):
    raise SystemExit(f"benchmark profile {key} is malformed")
  return dict(sorted(params.items()))


def provider_revisions() -> dict[str, str]:
  build = (REPO / "build.sbt").read_text()
  revisions = {
    name: revision
    for name, revision in re.findall(
      r'^\s*lazy\s+val\s+(\w+Revision)\s*=\s*"([0-9a-f]{40})"',
      build,
      flags=re.MULTILINE,
    )
  }
  if not revisions:
    raise SystemExit("cannot resolve benchmark provider revisions")
  scala_match = re.search(r'ThisBuild\s*/\s*scalaVersion\s*:=\s*"([^"]+)"', build)
  sbt_match = re.search(
    r"^sbt\.version=(.+)$",
    (REPO / "project/build.properties").read_text(),
    flags=re.MULTILINE,
  )
  if scala_match is None or sbt_match is None:
    raise SystemExit("cannot resolve benchmark Scala/sbt versions")
  revisions["scalaVersion"] = scala_match.group(1)
  revisions["sbtVersion"] = sbt_match.group(1).strip()
  return dict(sorted(revisions.items()))


def cpu_model() -> str:
  value = platform.processor().strip()
  if value:
    return value
  if sys.platform == "darwin":
    completed = subprocess.run(
      ["sysctl", "-n", "machdep.cpu.brand_string"],
      check=False,
      capture_output=True,
      text=True,
    )
    if completed.returncode == 0 and completed.stdout.strip():
      return completed.stdout.strip()
  cpuinfo = Path("/proc/cpuinfo")
  if cpuinfo.is_file():
    for line in cpuinfo.read_text().splitlines():
      if line.lower().startswith("model name") and ":" in line:
        return line.split(":", 1)[1].strip()
  return "unknown"


def workload(results: list[dict[str, Any]], budgets: dict[str, Any]) -> dict[str, Any]:
  return {
    "budget_profile": budgets.get("profile"),
    "budget_schema_version": budgets.get("schema_version"),
    "results": [
      {
        "benchmark": row.get("benchmark"),
        "jmh": row.get("jmh"),
        "params": row.get("params"),
      }
      for row in results
    ],
  }


def parse_jmh(paths: list[Path], budgets: dict[str, Any]) -> list[dict[str, Any]]:
  rows: list[dict[str, Any]] = []
  seen: set[str] = set()
  expected = budgets.get("benchmarks")
  if not isinstance(expected, dict):
    raise SystemExit("benchmark budgets must contain a benchmarks object")

  for path in paths:
    payload = json.loads(path.read_text())
    if not isinstance(payload, list):
      raise SystemExit(f"expected a JMH result array in {path}")
    for raw in payload:
      if not isinstance(raw, dict) or not isinstance(raw.get("benchmark"), str):
        raise SystemExit(f"malformed JMH entry in {path}")
      name = raw["benchmark"]
      if name in seen:
        raise SystemExit(f"duplicate JMH benchmark: {name}")
      seen.add(name)
      budget = expected.get(name)
      if not isinstance(budget, dict):
        raise SystemExit(f"no admission budget for {name}")
      primary = raw.get("primaryMetric")
      secondary = raw.get("secondaryMetrics")
      if not isinstance(primary, dict) or not isinstance(secondary, dict):
        raise SystemExit(f"missing JMH metrics for {name}")
      allocation_metric = secondary.get("gc.alloc.rate.norm")
      if not isinstance(allocation_metric, dict):
        raise SystemExit(f"JMH gc profiler did not report allocation for {name}")
      score = float(primary["score"])
      unit = str(primary["scoreUnit"])
      allocation = float(allocation_metric["score"])
      max_score = float(budget["max_score"])
      max_allocation = float(budget["max_allocation_bytes_per_op"])
      if unit != budget.get("score_unit"):
        raise SystemExit(
          f"{name} reports {unit}, expected {budget.get('score_unit')}"
        )
      params = dict(sorted((raw.get("params") or {}).items()))
      if params != expected_params(name, budgets):
        raise SystemExit(
          f"{name} reports params {params}, expected {expected_params(name, budgets)}"
        )
      rows.append(
        {
          "allocation_bytes_per_op": allocation,
          "benchmark": name,
          "budget": {
            "max_allocation_bytes_per_op": max_allocation,
            "max_score": max_score,
            "score_unit": unit,
          },
          "jmh": {
            "forks": int(raw["forks"]),
            "measurement_iterations": int(raw["measurementIterations"]),
            "measurement_time": str(raw["measurementTime"]),
            "mode": str(raw["mode"]),
            "threads": int(raw["threads"]),
            "warmup_iterations": int(raw["warmupIterations"]),
            "warmup_time": str(raw["warmupTime"]),
          },
          "params": params,
          "passes_allocation_budget": allocation <= max_allocation,
          "passes_time_budget": score <= max_score,
          "score": score,
          "score_unit": unit,
        }
      )

  missing = sorted(set(expected) - seen)
  if missing:
    raise SystemExit("missing required JMH benchmarks: " + ", ".join(missing))
  return sorted(rows, key=lambda row: row["benchmark"])


def comparison(
    rows: dict[str, dict[str, Any]],
    left: str,
    right: str,
    metric: str,
    enforce_less: bool,
) -> dict[str, Any]:
  left_value = float(rows[left][metric])
  right_value = float(rows[right][metric])
  return {
    "enforced": enforce_less,
    "left": left,
    "metric": metric,
    "ratio_left_over_right": left_value / right_value,
    "right": right,
    "status": "pass" if not enforce_less or left_value < right_value else "fail",
  }


def comparisons(results: list[dict[str, Any]]) -> list[dict[str, Any]]:
  rows = {str(row["benchmark"]): row for row in results}
  hrf = "scalafim.fmri.hrf."
  ar = "scalafim.fmri.ar.ArEstimationBenchmark."
  fit = "scalafim.fmri.fit.FirstLevelFitBenchmark."
  return [
    comparison(rows, hrf + "RegressorConvolutionBenchmark.conv", hrf + "RegressorConvolutionBenchmark.fft", "score", True),
    comparison(rows, hrf + "RegressorConvolutionBenchmark.conv", hrf + "RegressorConvolutionBenchmark.loop", "score", True),
    comparison(rows, hrf + "EpochIntegrationBenchmark.exact", hrf + "EpochIntegrationBenchmark.trapezoid", "score", True),
    comparison(rows, hrf + "BasisResponseBenchmark.typedReconstruction", hrf + "BasisResponseBenchmark.directContraction", "score", False),
    comparison(rows, ar + "automaticGlobal", ar + "fixedOrderGlobal", "score", False),
    comparison(rows, ar + "automaticGlobal", ar + "fixedOrderGlobal", "allocation_bytes_per_op", False),
    comparison(rows, ar + "fixedOrderRun", ar + "fixedOrderGlobal", "score", False),
    comparison(rows, fit + "olsPreparedMultiresponse", fit + "olsPlanAndFit", "score", True),
    comparison(rows, fit + "olsPreparedMultiresponse", fit + "olsPlanAndFit", "allocation_bytes_per_op", True),
    comparison(rows, fit + "weightedPreparedFit", fit + "weightedPlanAndFit", "score", True),
    comparison(rows, fit + "weightedPreparedFit", fit + "weightedPlanAndFit", "allocation_bytes_per_op", True),
    comparison(rows, fit + "fixedGlsPreparedFit", fit + "fixedGlsPlanAndFit", "score", True),
    comparison(rows, fit + "fixedGlsPreparedFit", fit + "fixedGlsPlanAndFit", "allocation_bytes_per_op", True),
    comparison(rows, fit + "olsPreparedChunked", fit + "olsPreparedMultiresponse", "score", False),
    comparison(rows, fit + "compiledDesignPlanning", fit + "olsPreparedSingleResponse", "score", False),
  ]


def build_receipt(raw_paths: list[Path]) -> dict[str, Any]:
  budgets = load_object(BUDGETS)
  results = parse_jmh(raw_paths, budgets)
  relations = comparisons(results)
  failed = [
    row["benchmark"]
    for row in results
    if not row["passes_allocation_budget"] or not row["passes_time_budget"]
  ]
  failed_relations = [row for row in relations if row["status"] == "fail"]
  dirty_paths = git("status", "--porcelain").splitlines()
  blocking_caveats = []
  if dirty_paths:
    blocking_caveats.append(
      {
        "detail": "receipt was captured from a dirty worktree; rerun the release court after committing the accepted slice",
        "kind": "dirty_worktree",
      }
    )
  first = json.loads(raw_paths[0].read_text())[0]
  source_files = source_hashes()
  workload_value = workload(results, budgets)
  receipt = {
    "benchmark_status": "pass" if not failed and not failed_relations else "fail",
    "blocking_caveats": blocking_caveats,
    "budget_profile": budgets["profile"],
    "budget_schema_version": budgets["schema_version"],
    "comparisons": relations,
    "fixture_hashes": {},
    "raw_reports": [
      {"name": path.name, "sha256": sha256(path)} for path in raw_paths
    ],
    "release_eligible": not failed and not failed_relations and not blocking_caveats,
    "results": results,
    "schema_version": SCHEMA,
    "source": {
      "commit": git("rev-parse", "HEAD"),
      "dirty": bool(dirty_paths),
      "files": source_files,
      "files_sha256": canonical_sha256(source_files),
      "provider_revisions": provider_revisions(),
    },
    "toolchain": {
      "architecture": platform.machine(),
      "cpu": cpu_model(),
      "jmh": str(first["jmhVersion"]),
      "jvm": str(first["jdkVersion"]),
      "jvm_name": str(first["vmName"]),
      "os": platform.platform(),
      "python": platform.python_version(),
      "sbt": "1.11.7",
      "scala": "3.7.4",
    },
    "workload": workload_value,
    "workload_sha256": canonical_sha256(workload_value),
  }
  return receipt


def validate_receipt(path: Path, *, require_release: bool = False) -> None:
  receipt = load_object(path)
  errors: list[str] = []
  if receipt.get("schema_version") != SCHEMA:
    errors.append("schema_version")
  if receipt.get("benchmark_status") != "pass":
    errors.append("benchmark_status")
  source = receipt.get("source")
  current_files = source_hashes()
  if not isinstance(source, dict) or source.get("files") != current_files:
    errors.append("source.files")
  elif source.get("files_sha256") != canonical_sha256(current_files):
    errors.append("source.files_sha256")
  if not isinstance(source, dict) or source.get("provider_revisions") != provider_revisions():
    errors.append("source.provider_revisions")
  results = receipt.get("results")
  budget_object = load_object(BUDGETS)
  budgets = budget_object.get("benchmarks")
  if not isinstance(results, list) or not isinstance(budgets, dict):
    errors.append("results")
  else:
    names = {row.get("benchmark") for row in results if isinstance(row, dict)}
    if names != set(budgets):
      errors.append("results.required_benchmarks")
    if any(
      not isinstance(row, dict)
      or not row.get("passes_allocation_budget")
      or not row.get("passes_time_budget")
      for row in results
    ):
      errors.append("results.budgets")
    if any(
      not isinstance(row, dict)
      or row.get("params") != expected_params(str(row.get("benchmark")), budget_object)
      for row in results
    ):
      errors.append("results.params")
  expected_workload = workload(results, budget_object) if isinstance(results, list) else None
  if receipt.get("workload") != expected_workload:
    errors.append("workload")
  elif receipt.get("workload_sha256") != canonical_sha256(expected_workload):
    errors.append("workload_sha256")
  if receipt.get("fixture_hashes") != {}:
    errors.append("fixture_hashes")
  comparisons_value = receipt.get("comparisons")
  expected_comparisons = comparisons(results) if isinstance(results, list) and {row.get("benchmark") for row in results if isinstance(row, dict)} == set(budgets or {}) else None
  if not isinstance(comparisons_value, list) or comparisons_value != expected_comparisons or any(
    isinstance(row, dict) and row.get("enforced") and row.get("status") != "pass"
    for row in comparisons_value
  ):
    errors.append("comparisons")
  toolchain = receipt.get("toolchain")
  if not isinstance(toolchain, dict) or any(
      not isinstance(toolchain.get(key), str) or not toolchain[key]
      for key in ("architecture", "cpu", "jmh", "jvm", "jvm_name", "os", "python", "sbt", "scala")
  ):
    errors.append("toolchain")
  if require_release:
    if not isinstance(source, dict) or source.get("commit") != git("rev-parse", "HEAD"):
      errors.append("source.commit")
    if not isinstance(source, dict) or source.get("dirty") is not False:
      errors.append("source.dirty")
    if receipt.get("release_eligible") is not True:
      errors.append("release_eligible")
    raw_reports = receipt.get("raw_reports")
    if not isinstance(raw_reports, list) or not raw_reports:
      errors.append("raw_reports")
    else:
      for raw in raw_reports:
        name = raw.get("name") if isinstance(raw, dict) else None
        expected = raw.get("sha256") if isinstance(raw, dict) else None
        raw_path = path.parent / str(name)
        if not isinstance(name, str) or not isinstance(expected, str) or not raw_path.is_file() or sha256(raw_path) != expected:
          errors.append("raw_reports")
          break
  if errors:
    raise SystemExit(f"invalid first-level benchmark receipt {path}: {', '.join(errors)}")
  print(f"validated first-level benchmark receipt: {path}")


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--raw", action="append", type=Path, default=[])
  parser.add_argument("--output", type=Path, default=DEFAULT_RECEIPT)
  parser.add_argument("--check", type=Path)
  parser.add_argument("--require-release", action="store_true")
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  if args.check is not None:
    validate_receipt(args.check, require_release=args.require_release)
    return 0
  if not args.raw:
    raise SystemExit("at least one --raw JMH report is required")
  raw_paths = [path.resolve() for path in args.raw]
  for path in raw_paths:
    if not path.is_file():
      raise SystemExit(f"missing JMH report: {path}")
  receipt = build_receipt(raw_paths)
  args.output.parent.mkdir(parents=True, exist_ok=True)
  args.output.write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
  print(f"wrote {args.output}")
  if receipt["benchmark_status"] != "pass":
    return 1
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
