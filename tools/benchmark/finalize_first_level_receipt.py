#!/usr/bin/env python3
"""Build or validate the first-level JMH admission receipt."""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
from pathlib import Path
import subprocess
import sys
from typing import Any


REPO = Path(__file__).resolve().parents[2]
BUDGETS = REPO / "tools" / "benchmark" / "first-level-budgets.json"
DEFAULT_RECEIPT = REPO / "docs" / "benchmarks" / "receipts" / "first-level-current.json"
SCHEMA = "scalafim-first-level-benchmark-receipt/v1"
SOURCE_PATHS = (
  "build.sbt",
  "benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/BasisResponseBenchmark.scala",
  "benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/DenseDriveBenchmark.scala",
  "benchmarks/hrf-jvm/src/main/scala/scalafim/fmri/hrf/RegressorConvolutionBenchmark.scala",
  "benchmarks/fit-jvm/src/main/scala/scalafim/fmri/ar/ArEstimationBenchmark.scala",
  "benchmarks/fit-jvm/src/main/scala/scalafim/fmri/fit/FirstLevelFitBenchmark.scala",
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
          "params": dict(sorted((raw.get("params") or {}).items())),
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
  receipt = {
    "benchmark_status": "pass" if not failed and not failed_relations else "fail",
    "blocking_caveats": blocking_caveats,
    "budget_profile": budgets["profile"],
    "budget_schema_version": budgets["schema_version"],
    "comparisons": relations,
    "raw_reports": [
      {"name": path.name, "sha256": sha256(path)} for path in raw_paths
    ],
    "release_eligible": not failed and not failed_relations and not blocking_caveats,
    "results": results,
    "schema_version": SCHEMA,
    "source": {
      "commit": git("rev-parse", "HEAD"),
      "dirty": bool(dirty_paths),
      "files": source_hashes(),
    },
    "toolchain": {
      "architecture": platform.machine(),
      "jmh": str(first["jmhVersion"]),
      "jvm": str(first["jdkVersion"]),
      "jvm_name": str(first["vmName"]),
      "os": platform.platform(),
      "python": platform.python_version(),
      "sbt": "1.11.7",
      "scala": "3.7.4",
    },
  }
  return receipt


def validate_receipt(path: Path) -> None:
  receipt = load_object(path)
  errors: list[str] = []
  if receipt.get("schema_version") != SCHEMA:
    errors.append("schema_version")
  if receipt.get("benchmark_status") != "pass":
    errors.append("benchmark_status")
  source = receipt.get("source")
  if not isinstance(source, dict) or source.get("files") != source_hashes():
    errors.append("source.files")
  results = receipt.get("results")
  budgets = load_object(BUDGETS).get("benchmarks")
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
  comparisons_value = receipt.get("comparisons")
  if not isinstance(comparisons_value, list) or any(
    isinstance(row, dict) and row.get("enforced") and row.get("status") != "pass"
    for row in comparisons_value
  ):
    errors.append("comparisons")
  if errors:
    raise SystemExit(f"invalid first-level benchmark receipt {path}: {', '.join(errors)}")
  print(f"validated first-level benchmark receipt: {path}")


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--raw", action="append", type=Path, default=[])
  parser.add_argument("--output", type=Path, default=DEFAULT_RECEIPT)
  parser.add_argument("--check", type=Path)
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  if args.check is not None:
    validate_receipt(args.check)
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
