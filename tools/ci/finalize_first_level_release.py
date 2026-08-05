#!/usr/bin/env python3
"""Build or validate a fail-closed first-level release report."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
from typing import Any, Optional


REPO = Path(__file__).resolve().parents[2]
SCHEMA = "scalafim-first-level-release-report/v1"
DEFAULT_BENCHMARK = REPO / "docs" / "benchmarks" / "receipts" / "first-level-current.json"
DEFAULT_OUTPUT = REPO / "docs" / "release-report.json"
GATES = {
  "documentation": "python -S tools/docs/check_first_level_docs.py --check",
  "focused_first_level": "bash tools/ci/first-level-gate.sh",
  "scientific_coverage": "bash tools/ci/first-level-coverage.sh",
  "compile_all": "sbt compileAll",
  "test_all": "sbt testAll",
  "performance": "bash tools/ci/first-level-benchmark.sh",
}
SOURCE_PATHS = (
  ".github/workflows/first-level.yml",
  ".github/workflows/first-level-performance.yml",
  ".github/workflows/scenario-receipts.yml",
  "build.sbt",
  "docs/benchmarks/first-level.md",
  "docs/first-level-analysis.md",
  "docs/release-assurance.md",
  "modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/DelayedMatchToSampleDslScenarioSuite.scala",
  "project/plugins.sbt",
  "tools/benchmark/finalize_first_level_receipt.py",
  "tools/benchmark/first-level-budgets.json",
  "tools/ci/finalize_first_level_release.py",
  "tools/ci/first-level-benchmark.sh",
  "tools/ci/first-level-coverage.sh",
  "tools/ci/first-level-gate.sh",
  "tools/ci/first-level-release.sh",
  "tools/docs/check_first_level_docs.py",
)
EXTERNAL_EVIDENCE = {
  "branch_protection": "SCALAFIM_RELEASE_BRANCH_PROTECTION_URL",
  "remote_first_level_ci": "SCALAFIM_RELEASE_CI_URL",
  "reference_regeneration": "SCALAFIM_RELEASE_RECEIPTS_URL",
}


def git(*args: str) -> str:
  completed = subprocess.run(
    ["git", *args], cwd=REPO, check=False, capture_output=True, text=True
  )
  if completed.returncode != 0:
    raise SystemExit(completed.stderr.strip() or "git command failed")
  return completed.stdout.strip()


def sha256(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def source_hashes() -> dict[str, str]:
  hashes: dict[str, str] = {}
  for relative in SOURCE_PATHS:
    path = REPO / relative
    if not path.is_file():
      raise SystemExit(f"missing release-court source: {relative}")
    hashes[relative] = sha256(path)
  return hashes


def command_output(*command: str) -> str:
  completed = subprocess.run(command, check=False, capture_output=True, text=True)
  output = completed.stdout.strip() or completed.stderr.strip()
  return output.splitlines()[0] if output else "unavailable"


def java_home() -> str:
  completed = subprocess.run(
    ["java", "-XshowSettings:properties", "-version"],
    check=False,
    capture_output=True,
    text=True,
  )
  for line in (completed.stdout + completed.stderr).splitlines():
    key, separator, value = line.strip().partition(" = ")
    if separator and key == "java.home":
      return value
  return "unavailable"


def load_object(path: Path) -> dict[str, Any]:
  value = json.loads(path.read_text())
  if not isinstance(value, dict):
    raise SystemExit(f"expected a JSON object in {path}")
  return value


def relative_or_absolute(path: Path) -> str:
  resolved = path.resolve()
  try:
    return str(resolved.relative_to(REPO))
  except ValueError:
    return str(resolved)


def gate_results(status_dir: Optional[Path]) -> dict[str, dict[str, Any]]:
  results: dict[str, dict[str, Any]] = {}
  for name, command in GATES.items():
    status_path = status_dir / f"{name}.exit" if status_dir is not None else None
    log_path = status_dir / f"{name}.log" if status_dir is not None else None
    command_path = status_dir / f"{name}.command" if status_dir is not None else None
    if status_path is None or not status_path.is_file():
      exit_code = None
      status = "not_run"
    else:
      exit_code = int(status_path.read_text().strip())
      status = "pass" if exit_code == 0 else "skipped" if exit_code == 125 else "fail"
    results[name] = {
      "command": command_path.read_text().strip() if command_path is not None and command_path.is_file() else command,
      "exit_code": exit_code,
      "log": relative_or_absolute(log_path) if log_path is not None and log_path.is_file() else None,
      "status": status,
    }
  return results


def external_evidence() -> dict[str, dict[str, Any]]:
  evidence: dict[str, dict[str, Any]] = {}
  for name, variable in EXTERNAL_EVIDENCE.items():
    url = os.environ.get(variable, "").strip()
    evidence[name] = {
      "environment_variable": variable,
      "status": "verified" if url else "unverified",
      "url": url or None,
    }
  return evidence


def build_report(status_dir: Optional[Path], benchmark_path: Path) -> dict[str, Any]:
  benchmark = load_object(benchmark_path)
  gates = gate_results(status_dir)
  external = external_evidence()
  dirty_lines = git("status", "--porcelain", "--untracked-files=all").splitlines()
  blockers: list[dict[str, str]] = []
  if dirty_lines:
    blockers.append(
      {
        "detail": "the source checkout contains tracked or untracked changes",
        "kind": "dirty_worktree",
      }
    )
  for name, result in gates.items():
    if result["status"] != "pass":
      blockers.append(
        {
          "detail": f"required local gate {name} has status {result['status']}",
          "kind": "local_gate",
        }
      )
  if benchmark.get("benchmark_status") != "pass" or not benchmark.get("release_eligible"):
    blockers.append(
      {
        "detail": "the selected benchmark receipt is not release eligible",
        "kind": "benchmark_receipt",
      }
    )
  for name, evidence in external.items():
    if evidence["status"] != "verified":
      blockers.append(
        {
          "detail": f"external evidence {name} has not been supplied",
          "kind": "external_evidence",
        }
      )

  commit = git("rev-parse", "HEAD")
  return {
    "blocking_caveats": blockers,
    "evidence": {
      "benchmark_receipt": {
        "path": relative_or_absolute(benchmark_path),
        "schema_version": benchmark.get("schema_version"),
        "sha256": sha256(benchmark_path),
        "status": benchmark.get("benchmark_status"),
      },
      "commit": f"https://github.com/canardlapin/scalafim/commit/{commit}",
      "reference_lock": "tools/r-parity/reference-lock.json",
      "scenario_manifest": "docs/scenarios/manifest.json",
    },
    "external_evidence": external,
    "generated_at": datetime.now(timezone.utc).isoformat(),
    "local_gates": gates,
    "release_eligible": not blockers,
    "schema_version": SCHEMA,
    "source": {
      "commit": commit,
      "dirty": bool(dirty_lines),
      "dirty_path_count": len(dirty_lines),
      "files": source_hashes(),
    },
    "status": "pass" if not blockers else "blocked",
    "toolchain": {
      "architecture": platform.machine(),
      "java": command_output("java", "-version"),
      "java_home": java_home(),
      "node": command_output("node", "--version"),
      "os": platform.platform(),
      "python": platform.python_version(),
      "sbt": "1.11.7",
      "scala": "3.7.4",
    },
  }


def validate_report(path: Path) -> None:
  report = load_object(path)
  errors: list[str] = []
  if report.get("schema_version") != SCHEMA:
    errors.append("schema_version")
  source = report.get("source")
  if not isinstance(source, dict) or source.get("files") != source_hashes():
    errors.append("source.files")
  evidence = report.get("evidence")
  benchmark = evidence.get("benchmark_receipt") if isinstance(evidence, dict) else None
  if not isinstance(benchmark, dict):
    errors.append("evidence.benchmark_receipt")
  else:
    benchmark_path = Path(str(benchmark.get("path", "")))
    if not benchmark_path.is_absolute():
      benchmark_path = REPO / benchmark_path
    if not benchmark_path.is_file() or benchmark.get("sha256") != sha256(benchmark_path):
      errors.append("evidence.benchmark_receipt.sha256")
  gates = report.get("local_gates")
  if not isinstance(gates, dict) or set(gates) != set(GATES):
    errors.append("local_gates")
  if report.get("release_eligible"):
    if report.get("status") != "pass" or report.get("blocking_caveats"):
      errors.append("release_eligible")
    if not isinstance(gates, dict) or any(
      not isinstance(value, dict) or value.get("status") != "pass"
      for value in gates.values()
    ):
      errors.append("local_gates.release")
  if errors:
    raise SystemExit(f"invalid first-level release report {path}: {', '.join(errors)}")
  print(f"validated first-level release report: {path}")


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--status-dir", type=Path)
  parser.add_argument("--benchmark", type=Path, default=DEFAULT_BENCHMARK)
  parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
  parser.add_argument("--check", type=Path)
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  if args.check is not None:
    validate_report(args.check)
    return 0
  benchmark_path = args.benchmark.resolve()
  if not benchmark_path.is_file():
    raise SystemExit(f"missing benchmark receipt: {benchmark_path}")
  status_dir = args.status_dir.resolve() if args.status_dir is not None else None
  report = build_report(status_dir, benchmark_path)
  args.output.parent.mkdir(parents=True, exist_ok=True)
  args.output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
  print(f"wrote {args.output}: {report['status']}")
  return 0 if report["release_eligible"] else 1


if __name__ == "__main__":
  raise SystemExit(main())
