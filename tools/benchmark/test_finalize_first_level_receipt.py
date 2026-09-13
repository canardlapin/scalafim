from __future__ import annotations

import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch


sys.path.insert(0, str(Path(__file__).resolve().parent))
import finalize_first_level_receipt as receipt  # noqa: E402


CANDIDATE = "a" * 40


class ReceiptSuite(unittest.TestCase):
  def setUp(self) -> None:
    self.temporary = tempfile.TemporaryDirectory()
    self.root = Path(self.temporary.name)
    self.budgets = receipt.load_object(receipt.BUDGETS)

  def tearDown(self) -> None:
    self.temporary.cleanup()

  def results(self) -> list[dict[str, object]]:
    rows = []
    for name, budget in self.budgets["benchmarks"].items():
      rows.append({
        "allocation_bytes_per_op": 1.0,
        "benchmark": name,
        "budget": {
          "max_allocation_bytes_per_op": float(budget["max_allocation_bytes_per_op"]),
          "max_score": float(budget["max_score"]),
          "score_unit": budget["score_unit"],
        },
        "jmh": {
          "forks": 1,
          "measurement_iterations": 4,
          "measurement_time": "500 ms",
          "mode": "avgt",
          "threads": 1,
          "warmup_iterations": 2,
          "warmup_time": "500 ms",
        },
        "params": receipt.expected_params(name, self.budgets),
        "passes_allocation_budget": True,
        "passes_time_budget": True,
        "score": 0.01,
        "score_unit": budget["score_unit"],
      })
    by_name = {row["benchmark"]: row for row in rows}
    hrf = "scalafim.fmri.hrf."
    fit = "scalafim.fmri.fit.FirstLevelFitBenchmark."
    for name in (
        hrf + "RegressorConvolutionBenchmark.fft",
        hrf + "RegressorConvolutionBenchmark.loop",
        hrf + "EpochIntegrationBenchmark.trapezoid",
        fit + "olsPlanAndFit",
        fit + "weightedPlanAndFit",
        fit + "fixedGlsPlanAndFit",
    ):
      by_name[name]["score"] = 0.02
      by_name[name]["allocation_bytes_per_op"] = 2.0
    return sorted(rows, key=lambda row: str(row["benchmark"]))

  def write_receipt(self) -> Path:
    rows = self.results()
    raw = self.root / "fit.json"
    raw.write_text("[]\n")
    source_files = receipt.source_hashes()
    workload = receipt.workload(rows, self.budgets)
    value = {
      "benchmark_status": "pass",
      "blocking_caveats": [],
      "budget_profile": self.budgets["profile"],
      "budget_schema_version": self.budgets["schema_version"],
      "comparisons": receipt.comparisons(rows),
      "fixture_hashes": {},
      "raw_reports": [{"name": raw.name, "sha256": receipt.sha256(raw)}],
      "release_eligible": True,
      "results": rows,
      "schema_version": receipt.SCHEMA,
      "source": {
        "commit": CANDIDATE,
        "dirty": False,
        "files": source_files,
        "files_sha256": receipt.canonical_sha256(source_files),
        "provider_revisions": receipt.provider_revisions(),
      },
      "toolchain": {
        key: "recorded" for key in
        ("architecture", "cpu", "jmh", "jvm", "jvm_name", "os", "python", "sbt", "scala")
      },
      "workload": workload,
      "workload_sha256": receipt.canonical_sha256(workload),
    }
    path = self.root / "receipt.json"
    path.write_text(json.dumps(value, indent=2))
    return path

  def test_release_validation_binds_candidate_sources_workload_and_raw_report(self) -> None:
    path = self.write_receipt()
    with patch.object(receipt, "git", return_value=CANDIDATE):
      receipt.validate_receipt(path, require_release=True)

  def test_tampered_workload_source_inventory_and_raw_report_fail(self) -> None:
    cases = ("workload", "source", "raw")
    for case in cases:
      with self.subTest(case=case):
        path = self.write_receipt()
        value = json.loads(path.read_text())
        if case == "workload":
          value["workload_sha256"] = "0" * 64
        elif case == "source":
          value["source"]["provider_revisions"] = {}
        else:
          (self.root / "fit.json").write_text("tampered\n")
        path.write_text(json.dumps(value))
        with patch.object(receipt, "git", return_value=CANDIDATE):
          with self.assertRaises(SystemExit):
            receipt.validate_receipt(path, require_release=True)

  def test_parser_rejects_wrong_exact_parameters(self) -> None:
    name = "scalafim.fmri.fit.FirstLevelFitBenchmark.compiledDesignPlanning"
    raw = self.root / "wrong-params.json"
    raw.write_text(json.dumps([{
      "benchmark": name,
      "forks": 1,
      "measurementIterations": 4,
      "measurementTime": "500 ms",
      "mode": "avgt",
      "threads": 1,
      "warmupIterations": 2,
      "warmupTime": "500 ms",
      "params": {"timepoints": "999"},
      "primaryMetric": {"score": 1.0, "scoreUnit": "ms/op"},
      "secondaryMetrics": {"gc.alloc.rate.norm": {"score": 1.0}},
    }]))
    with self.assertRaisesRegex(SystemExit, "reports params"):
      receipt.parse_jmh([raw], self.budgets)


if __name__ == "__main__":
  unittest.main()
