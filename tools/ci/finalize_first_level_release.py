#!/usr/bin/env python3
"""Build or validate a fail-closed first-level release report."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import fnmatch
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import subprocess
from typing import Any, Optional
from urllib.parse import urlparse
import uuid


REPO = Path(__file__).resolve().parents[2]
SCHEMA = "scalafim-first-level-release-report/v2"
RUN_SCHEMA = "scalafim-first-level-release-run/v1"
GATE_SCHEMA = "scalafim-first-level-gate-receipt/v1"
EVIDENCE_SCHEMA = "scalafim-first-level-external-evidence/v1"
CANONICAL_REPOSITORY = "canardlapin/scalafim"
EVIDENCE_BUNDLE_ENV = "SCALAFIM_RELEASE_EVIDENCE_BUNDLE"
DEFAULT_BENCHMARK = REPO / "docs" / "benchmarks" / "receipts" / "first-level-current.json"
DEFAULT_OUTPUT = REPO / "docs" / "release-report.json"
GATES = {
  "documentation": "python3 -S tools/docs/check_first_level_docs.py --check",
  "focused_first_level": "bash tools/ci/first-level-gate.sh",
  "scientific_coverage": "bash tools/ci/first-level-coverage.sh",
  "compile_all": "sbt scalafimCompileAll",
  "test_all": "bash tools/ci/full-repository-tests.sh",
  "performance": "bash tools/ci/first-level-benchmark.sh",
}
SOURCE_PATHS = (
  ".github/workflows/first-level-laws-calibration.yml",
  ".github/workflows/first-level-mutation-pilot.yml",
  ".github/workflows/first-level.yml",
  ".github/workflows/first-level-performance.yml",
  ".github/workflows/scenario-receipts.yml",
  "benchmarks/fit-jvm/src/main/scala/scalafim/fmri/ar/ArEstimationBenchmark.scala",
  "build.sbt",
  "docs/benchmarks/first-level.md",
  "docs/first-level-analysis.md",
  "docs/release-assurance.md",
  "docs/scenarios/fixtures/ar.fmriar-parity.v1.r.json",
  "docs/scenarios/fixtures/fit.fmriar-estimated-gls.v1.r.json",
  "modules/ar/shared/src/test/scala/scalafim/fmri/ar/FmriArParitySuite.scala",
  "modules/ar/shared/src/test/scala/scalafim/fmri/ar/fixtures/FmriArRFixture.scala",
  "modules/ar/tools/generate_fmriar_parity.R",
  "modules/first-level-laws/shared/src/test/scala/scalafim/fmri/fit/FixedEffectsStatusGeneratedLawsSuite.scala",
  "modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/ArGeneratedLawsSuite.scala",
  "modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/ArLawGenerators.scala",
  "modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/LawRunProfile.scala",
  "modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/MissingResponseGeneratedLawsSuite.scala",
  "modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/MissingResponseGenerators.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/Contrasts.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/FixedEffects.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/MaskedResponseExecutor.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/ObservationPattern.scala",
  "modules/fit/shared/src/main/scala/scalafim/fmri/fit/VoxelFitStatus.scala",
  "modules/fit/shared/src/test/scala/scalafim/fmri/fit/ChunkedFitExecutorSuite.scala",
  "modules/fit/shared/src/test/scala/scalafim/fmri/fit/FitPlanExecutorSuite.scala",
  "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/FmriArEstimatedGlsFixture.scala",
  "modules/fit/shared/src/test/scala/scalafim/fmri/fit/scenarios/DelayedMatchToSampleDslScenarioSuite.scala",
  "modules/fit/tools/generate_fmriar_estimated_gls_parity.R",
  "modules/model/shared/src/main/scala/scalafim/fmri/model/FitConfig.scala",
  "project/plugins.sbt",
  "tools/benchmark/finalize_first_level_receipt.py",
  "tools/benchmark/first-level-budgets.json",
  "tools/ci/finalize_first_level_release.py",
  "tools/ci/first-level-benchmark.sh",
  "tools/ci/first-level-coverage.sh",
  "tools/ci/first-level-gate.sh",
  "tools/ci/first-level-release.sh",
  "tools/ci/full-repository-tests.sh",
  "tools/docs/check_first_level_docs.py",
  "tools/mutation/ar_na_pilot.py",
  "tools/r-parity/auxiliary-manifest.json",
  "tools/r-parity/check_receipts.py",
  "tools/r-parity/finalize_fmriar_estimated_gls_receipt.py",
  "tools/r-parity/finalize_fmriar_parity_receipt.py",
  "tools/r-parity/reference-lock.json",
)
LEGACY_EXTERNAL_EVIDENCE = {
  "branch_protection": "SCALAFIM_RELEASE_BRANCH_PROTECTION_URL",
  "remote_first_level_ci": "SCALAFIM_RELEASE_CI_URL",
  "reference_regeneration": "SCALAFIM_RELEASE_RECEIPTS_URL",
}
REQUIRED_EVIDENCE_KINDS = frozenset(LEGACY_EXTERNAL_EVIDENCE)
REQUIRED_FIRST_LEVEL_JOB_IDS = frozenset({
  "full-repository",
  "focused-first-level",
  "scientific-coverage",
})
REQUIRED_REGENERATION_JOB_IDS = frozenset({"r-receipts", "python-receipts"})
FIRST_LEVEL_WORKFLOW = ".github/workflows/first-level.yml"
REGENERATION_WORKFLOW = ".github/workflows/scenario-receipts.yml"


def git(*args: str) -> str:
  completed = subprocess.run(
    ["git", *args], cwd=REPO, check=False, capture_output=True, text=True
  )
  if completed.returncode != 0:
    raise SystemExit(completed.stderr.strip() or "git command failed")
  return completed.stdout.strip()


def sha256(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def canonical_sha256(value: Any) -> str:
  encoded = json.dumps(
    value, ensure_ascii=False, allow_nan=False, sort_keys=True, separators=(",", ":")
  ).encode("utf-8")
  return hashlib.sha256(encoded).hexdigest()


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


def valid_gate_command(name: str, command: Any) -> bool:
  if not isinstance(command, str) or not command.strip():
    return False
  if name == "performance":
    return command == GATES[name] or command.startswith(
      "python3 -S tools/benchmark/finalize_first_level_receipt.py --check "
    )
  return command == GATES[name]


def current_source_state(repo_root: Path = REPO) -> tuple[str, bool, list[str]]:
  def run(*args: str) -> str:
    completed = subprocess.run(
      ["git", *args], cwd=repo_root, check=False, capture_output=True, text=True
    )
    if completed.returncode != 0:
      raise SystemExit(completed.stderr.strip() or "git command failed")
    return completed.stdout.strip()

  commit = run("rev-parse", "HEAD")
  dirty_lines = run("status", "--porcelain", "--untracked-files=all").splitlines()
  return commit, bool(dirty_lines), dirty_lines


def provider_pins(repo_root: Path = REPO) -> dict[str, str]:
  build = (repo_root / "build.sbt").read_text()
  pins = {
    name: revision
    for name, revision in re.findall(
      r'^\s*lazy\s+val\s+(\w+Revision)\s*=\s*"([0-9a-f]{40})"',
      build,
      flags=re.MULTILINE,
    )
  }
  scala_match = re.search(r'ThisBuild\s*/\s*scalaVersion\s*:=\s*"([^"]+)"', build)
  if scala_match is None:
    raise SystemExit("cannot resolve the Scala version for the release manifest")
  pins["scalaVersion"] = scala_match.group(1)
  sbt_properties = (repo_root / "project/build.properties").read_text()
  sbt_match = re.search(r"^sbt\.version=(.+)$", sbt_properties, flags=re.MULTILINE)
  if sbt_match is None:
    raise SystemExit("cannot resolve the sbt version for the release manifest")
  pins["sbtVersion"] = sbt_match.group(1).strip()
  if not pins or not any(key.endswith("Revision") for key in pins):
    raise SystemExit("cannot resolve provider revisions for the release manifest")
  return dict(sorted(pins.items()))


def release_inputs(repo_root: Path = REPO) -> dict[str, Any]:
  lock_hashes, _ = required_regeneration_hashes(repo_root)
  gate_files = (
    "tools/ci/first-level-gate.sh",
    "tools/ci/first-level-coverage.sh",
    "tools/ci/full-repository-tests.sh",
    "tools/ci/first-level-benchmark.sh",
    "tools/ci/check_test_inventory.py",
  )
  inventory = {
    path: sha256(repo_root / path)
    for path in gate_files
  }
  return {
    "gate_inventory_sha256": canonical_sha256({"commands": GATES, "files": inventory}),
    "provider_pins": provider_pins(repo_root),
    "reference_lock_hashes": lock_hashes,
    "scenario_manifest_sha256": sha256(repo_root / "docs/scenarios/manifest.json"),
  }


def initialize_run(path: Path) -> dict[str, Any]:
  commit, dirty, dirty_lines = current_source_state()
  inputs = release_inputs()
  manifest = {
    "schema_version": RUN_SCHEMA,
    "run_id": str(uuid.uuid4()),
    "started_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
    "source": {
      "commit": commit,
      "clean": not dirty,
      "dirty_path_count": len(dirty_lines),
    },
    "inputs": inputs,
    "inputs_sha256": canonical_sha256(inputs),
  }
  path.parent.mkdir(parents=True, exist_ok=True)
  path.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
  return manifest


def record_gate(
    manifest_path: Path,
    status_dir: Path,
    name: str,
    command: str,
    exit_code: int,
    started_at: str,
    ended_at: str,
    log_path: Path,
) -> dict[str, Any]:
  if name not in GATES:
    raise SystemExit(f"unknown release gate: {name}")
  manifest = load_object(manifest_path)
  if manifest.get("schema_version") != RUN_SCHEMA:
    raise SystemExit("cannot record a gate against an invalid run manifest")
  if not valid_gate_command(name, command):
    raise SystemExit(f"gate command for {name} is outside the gate inventory")
  if not log_path.is_file():
    raise SystemExit(f"missing gate log: {log_path}")
  if not valid_observed_at(started_at) or not valid_observed_at(ended_at):
    raise SystemExit("gate timestamps must be timezone-qualified")
  source = manifest.get("source")
  receipt = {
    "schema_version": GATE_SCHEMA,
    "name": name,
    "run_id": manifest.get("run_id"),
    "candidate_commit": source.get("commit") if isinstance(source, dict) else None,
    "inputs_sha256": manifest.get("inputs_sha256"),
    "command": command,
    "exit_code": exit_code,
    "status": "pass" if exit_code == 0 else "skipped" if exit_code == 125 else "fail",
    "started_at": started_at,
    "ended_at": ended_at,
    "log": {
      "path": relative_or_absolute(log_path),
      "sha256": sha256(log_path),
    },
  }
  status_dir.mkdir(parents=True, exist_ok=True)
  (status_dir / f"{name}.json").write_text(json.dumps(receipt, indent=2, sort_keys=True) + "\n")
  return receipt


def _timestamp(value: Any) -> Optional[datetime]:
  if not valid_observed_at(value):
    return None
  return datetime.fromisoformat(str(value).replace("Z", "+00:00"))


def gate_results(
    status_dir: Optional[Path],
    run_manifest: Optional[dict[str, Any]] = None,
    *,
    current_commit: Optional[str] = None,
    current_clean: Optional[bool] = None,
    current_inputs: Optional[dict[str, Any]] = None,
    repo_root: Path = REPO,
) -> tuple[dict[str, dict[str, Any]], list[str]]:
  errors: list[str] = []
  results: dict[str, dict[str, Any]] = {}
  if not isinstance(run_manifest, dict):
    errors.append("run manifest is missing")
  else:
    source = run_manifest.get("source")
    inputs = run_manifest.get("inputs")
    if run_manifest.get("schema_version") != RUN_SCHEMA:
      errors.append("run manifest schema is invalid")
    if not isinstance(run_manifest.get("run_id"), str) or not run_manifest["run_id"]:
      errors.append("run manifest id is missing")
    if _timestamp(run_manifest.get("started_at")) is None:
      errors.append("run manifest start time is invalid")
    if not isinstance(source, dict) or source.get("commit") != current_commit:
      errors.append("run manifest commit does not match the current candidate")
    if not isinstance(source, dict) or source.get("clean") is not True:
      errors.append("run manifest was not captured from a clean candidate")
    if current_clean is not True:
      errors.append("current candidate is not clean")
    if inputs != current_inputs or run_manifest.get("inputs_sha256") != canonical_sha256(current_inputs):
      errors.append("run manifest inputs are stale")

  receipt_paths = sorted(status_dir.glob("*.json")) if status_dir is not None and status_dir.is_dir() else []
  receipts: dict[str, dict[str, Any]] = {}
  for receipt_path in receipt_paths:
    try:
      receipt = load_object(receipt_path)
    except (OSError, UnicodeError, json.JSONDecodeError, SystemExit):
      errors.append(f"malformed gate receipt: {receipt_path.name}")
      continue
    name = receipt.get("name")
    if not isinstance(name, str) or name not in GATES:
      errors.append(f"unknown gate receipt: {receipt_path.name}")
    elif name in receipts:
      errors.append(f"duplicate gate receipt: {name}")
    else:
      receipts[name] = receipt

  for name, expected_command in GATES.items():
    receipt = receipts.get(name)
    gate_errors: list[str] = []
    if receipt is None:
      gate_errors.append("receipt is missing")
      receipt = {}
    if receipt.get("schema_version") != GATE_SCHEMA:
      gate_errors.append("schema is invalid")
    if isinstance(run_manifest, dict):
      if receipt.get("run_id") != run_manifest.get("run_id"):
        gate_errors.append("run id does not match")
      source = run_manifest.get("source")
      if receipt.get("candidate_commit") != (source.get("commit") if isinstance(source, dict) else None):
        gate_errors.append("candidate commit does not match")
      if receipt.get("inputs_sha256") != run_manifest.get("inputs_sha256"):
        gate_errors.append("input hash does not match")
      run_start = _timestamp(run_manifest.get("started_at"))
    else:
      run_start = None
    if not valid_gate_command(name, receipt.get("command")):
      gate_errors.append("command does not match the gate inventory")
    started = _timestamp(receipt.get("started_at"))
    ended = _timestamp(receipt.get("ended_at"))
    if started is None or ended is None or ended < started or (run_start is not None and started < run_start):
      gate_errors.append("timestamps are invalid")
    exit_code = receipt.get("exit_code")
    expected_status = "pass" if exit_code == 0 else "skipped" if exit_code == 125 else "fail"
    if not isinstance(exit_code, int) or receipt.get("status") != expected_status:
      gate_errors.append("exit code and status disagree")
    log = receipt.get("log")
    log_path: Optional[Path] = None
    if isinstance(log, dict) and isinstance(log.get("path"), str):
      log_path = Path(log["path"])
      if not log_path.is_absolute():
        log_path = repo_root / log_path
    if log_path is None or not log_path.is_file():
      gate_errors.append("log is missing")
    elif not isinstance(log, dict) or log.get("sha256") != sha256(log_path):
      gate_errors.append("log hash does not match")
    if gate_errors:
      errors.extend(f"{name}: {detail}" for detail in gate_errors)
      status = "invalid"
    else:
      status = str(receipt["status"])
    results[name] = {
      "command": receipt.get("command", expected_command),
      "ended_at": receipt.get("ended_at"),
      "exit_code": receipt.get("exit_code"),
      "log": receipt.get("log"),
      "started_at": receipt.get("started_at"),
      "status": status,
    }
  return results, errors


def valid_url(value: Any) -> bool:
  if not isinstance(value, str) or not value.strip():
    return False
  parsed = urlparse(value)
  return parsed.scheme in {"http", "https"} and bool(parsed.netloc)


def valid_observed_at(value: Any) -> bool:
  if not isinstance(value, str) or not value.strip():
    return False
  try:
    observed = datetime.fromisoformat(value.replace("Z", "+00:00"))
  except ValueError:
    return False
  return observed.tzinfo is not None


def evidence_result(
    kind: str,
    status: str,
    detail: str,
    *,
    record: Optional[dict[str, Any]] = None,
    bundle_path: Optional[Path] = None,
) -> dict[str, Any]:
  record = record or {}
  return {
    "bundle": relative_or_absolute(bundle_path) if bundle_path is not None else None,
    "detail": detail,
    "kind": kind,
    "observed_at": record.get("observed_at"),
    "payload_path": record.get("payload_path"),
    "payload_sha256": record.get("payload_sha256"),
    "source_url": record.get("source_url"),
    "status": status,
  }


def legacy_external_evidence(
    legacy_urls: Optional[dict[str, str]] = None,
) -> dict[str, dict[str, Any]]:
  supplied = legacy_urls if legacy_urls is not None else {
    kind: os.environ.get(variable, "").strip()
    for kind, variable in LEGACY_EXTERNAL_EVIDENCE.items()
  }
  evidence: dict[str, dict[str, Any]] = {}
  for kind, variable in LEGACY_EXTERNAL_EVIDENCE.items():
    url = supplied.get(kind, "").strip()
    if not url:
      result = evidence_result(kind, "unverified", "no evidence bundle or URL supplied")
    elif valid_url(url):
      result = evidence_result(
        kind,
        "provided",
        "legacy URL supplied without a verifiable payload bundle",
        record={"source_url": url},
      )
    else:
      result = evidence_result(kind, "unverified", "legacy evidence URL is malformed")
    result["environment_variable"] = variable
    evidence[kind] = result
  return evidence


def load_evidence_payload(
    bundle_path: Path,
    record: dict[str, Any],
) -> tuple[Optional[dict[str, Any]], Optional[str]]:
  payload_value = record.get("payload_path")
  if not isinstance(payload_value, str) or not payload_value.strip():
    return None, "payload_path is required"
  relative = Path(payload_value)
  if relative.is_absolute():
    return None, "payload_path must be relative to the evidence bundle"
  bundle_root = bundle_path.parent.resolve()
  payload_path = (bundle_root / relative).resolve()
  try:
    payload_path.relative_to(bundle_root)
  except ValueError:
    return None, "payload_path escapes the evidence bundle"
  if not payload_path.is_file():
    return None, "payload_path does not exist"
  payload_hash = record.get("payload_sha256")
  if not isinstance(payload_hash, str) or payload_hash != sha256(payload_path):
    return None, "payload_sha256 does not match the captured payload"
  try:
    payload = load_object(payload_path)
  except (OSError, UnicodeError, json.JSONDecodeError, SystemExit):
    return None, "captured payload is not a JSON object"
  return payload, None


def load_capture_files(
    bundle_path: Path,
    payload: dict[str, Any],
    required: frozenset[str],
) -> tuple[dict[str, dict[str, Any]], Optional[str]]:
  captures = payload.get("captures")
  if not isinstance(captures, dict) or set(captures) != required:
    return {}, "payload does not name the complete raw-capture inventory"
  bundle_root = bundle_path.parent.resolve()
  values: dict[str, dict[str, Any]] = {}
  for name in sorted(required):
    record = captures.get(name)
    if not isinstance(record, dict):
      return {}, f"raw capture {name!r} is malformed"
    value = record.get("path")
    if not isinstance(value, str) or not value.strip():
      return {}, f"raw capture {name!r} has no path"
    relative = Path(value)
    if relative.is_absolute():
      return {}, f"raw capture {name!r} path must be relative"
    path = (bundle_root / relative).resolve()
    try:
      path.relative_to(bundle_root)
    except ValueError:
      return {}, f"raw capture {name!r} escapes the evidence bundle"
    if not path.is_file():
      return {}, f"raw capture {name!r} is missing"
    if record.get("sha256") != sha256(path):
      return {}, f"raw capture {name!r} hash does not match"
    try:
      values[name] = load_object(path)
    except (OSError, UnicodeError, json.JSONDecodeError, SystemExit):
      return {}, f"raw capture {name!r} is not a JSON object"
  return values, None


def validate_workflow_captures(
    bundle_path: Path,
    payload: dict[str, Any],
    *,
    regeneration: bool,
) -> Optional[str]:
  required = {"run", "jobs"}
  if regeneration:
    required.update({"artifacts", "artifact_r", "artifact_python"})
  captures, error = load_capture_files(bundle_path, payload, frozenset(required))
  if error:
    return error
  if captures["run"] != payload.get("workflow_run"):
    return "raw workflow-run capture disagrees with the normalized payload"
  jobs = captures["jobs"].get("jobs")
  if jobs != payload.get("jobs") or captures["jobs"].get("total_count") != len(jobs or []):
    return "raw workflow-jobs capture disagrees with the normalized payload"
  if regeneration:
    if captures["artifacts"].get("artifacts") != payload.get("artifacts"):
      return "raw workflow-artifacts capture disagrees with the normalized payload"
    evidence = payload.get("artifact_evidence")
    if not isinstance(evidence, dict):
      return "regeneration artifact evidence is missing"
    for lane, capture_name in (("r-receipts", "artifact_r"), ("python-receipts", "artifact_python")):
      entry = evidence.get(lane)
      capture = payload["captures"].get(capture_name)
      if not isinstance(entry, dict) or not isinstance(capture, dict):
        return f"regeneration artifact evidence for {lane} is missing"
      if entry.get("path") != capture.get("path") or entry.get("sha256") != capture.get("sha256"):
        return f"regeneration artifact evidence for {lane} is not bound to its raw capture"
      if entry.get("manifest") != captures[capture_name]:
        return f"regeneration artifact manifest for {lane} disagrees with its raw capture"
  return None


def validate_common_evidence(
    kind: str,
    record: dict[str, Any],
    candidate_commit: str,
    repository: str,
) -> Optional[str]:
  if record.get("kind") != kind:
    return "record kind does not match its evidence slot"
  if record.get("repository") != repository:
    return "record names a foreign repository"
  if record.get("candidate_commit") != candidate_commit:
    return "record is stale for the requested candidate commit"
  if not valid_observed_at(record.get("observed_at")):
    return "observed_at must be a timezone-qualified timestamp"
  if not valid_url(record.get("source_url")):
    return "source_url must be an absolute HTTP(S) URL"
  return None


def workflow_run(payload: dict[str, Any]) -> Optional[dict[str, Any]]:
  value = payload.get("workflow_run")
  return value if isinstance(value, dict) else None


def validate_workflow_record(
    record: dict[str, Any],
    payload: dict[str, Any],
    *,
    workflow: str,
    required_job_ids: frozenset[str],
    repository: str,
    candidate_commit: str,
) -> tuple[str, str, set[str]]:
  run = workflow_run(payload)
  jobs = payload.get("jobs")
  required_jobs = record.get("required_jobs")
  if not isinstance(run, dict) or not isinstance(jobs, list):
    return "unverified", "payload must contain workflow_run and jobs", set()
  if not isinstance(required_jobs, dict) or set(required_jobs) != required_job_ids:
    return "unverified", "required_jobs must name the complete workflow job inventory", set()
  if any(not isinstance(name, str) or not name.strip() for name in required_jobs.values()):
    return "unverified", "required_jobs must map job ids to captured display names", set()
  expected = {
    "workflow": workflow,
    "run_id": run.get("id"),
    "run_attempt": run.get("run_attempt"),
    "head_sha": run.get("head_sha"),
    "status": run.get("status"),
    "conclusion": run.get("conclusion"),
  }
  for key, value in expected.items():
    if record.get(key) != value:
      return "unverified", f"record.{key} disagrees with the captured workflow run", set()
  if run.get("path") != workflow:
    return "unverified", "captured workflow path is not the required workflow", set()
  if payload.get("repository") != repository:
    return "unverified", "captured workflow payload names a foreign repository", set()
  if run.get("head_sha") != candidate_commit:
    return "unverified", "captured workflow run is stale for the candidate", set()
  if record.get("source_url") != run.get("html_url"):
    return "unverified", "source_url disagrees with the captured workflow URL", set()
  if not isinstance(run.get("id"), int) or not isinstance(run.get("run_attempt"), int):
    return "unverified", "run_id and run_attempt must be integers", set()

  jobs_by_name: dict[str, dict[str, Any]] = {}
  for job in jobs:
    if isinstance(job, dict) and isinstance(job.get("name"), str):
      if job["name"] in jobs_by_name:
        return "unverified", "captured jobs contain a duplicate display name", set()
      jobs_by_name[job["name"]] = job
  display_names = set(required_jobs.values())
  if not display_names.issubset(jobs_by_name):
    return "unverified", "captured jobs omit a required workflow job", set()

  status = run.get("status")
  conclusion = run.get("conclusion")
  if status in {"queued", "in_progress", "pending", "waiting", "requested"}:
    return "provided", f"workflow run is {status}", display_names
  if status != "completed":
    return "unverified", f"unknown workflow status {status!r}", set()
  if conclusion != "success":
    return "failed", f"workflow concluded {conclusion!r}", display_names
  for display_name in display_names:
    job = jobs_by_name[display_name]
    if job.get("status") != "completed" or job.get("conclusion") != "success":
      return "failed", f"required job {display_name!r} did not succeed", display_names
  return "verified", "workflow and every required job completed successfully", display_names


def required_regeneration_hashes(repo_root: Path) -> tuple[dict[str, str], dict[str, str]]:
  manifest = load_object(repo_root / "docs/scenarios/manifest.json")
  scenarios = manifest.get("scenarios")
  if not isinstance(scenarios, list):
    raise SystemExit("scenario manifest has no scenario list")
  generated_paths: set[str] = set()
  lock_paths: set[str] = set()
  for scenario in scenarios:
    reference = scenario.get("reference") if isinstance(scenario, dict) else None
    if not isinstance(reference, dict) or not reference.get("exporter_path"):
      continue
    for key in ("fixture_path", "generated_scala_fixture_path"):
      path = reference.get(key)
      if isinstance(path, str) and path.strip():
        generated_paths.add(path)
    fixture = reference.get("fixture_path")
    if isinstance(fixture, str):
      fixture_payload = load_object(repo_root / fixture)
      receipt = fixture_payload.get("receipt")
      environment = receipt.get("environment") if isinstance(receipt, dict) else None
      lock = environment.get("lock_path") if isinstance(environment, dict) else None
      if isinstance(lock, str) and lock.strip():
        lock_paths.add(lock)
  return (
    {path: sha256(repo_root / path) for path in sorted(lock_paths)},
    {path: sha256(repo_root / path) for path in sorted(generated_paths)},
  )


def status_check_contexts(required: Any) -> set[str]:
  if not isinstance(required, dict):
    return set()
  contexts = required.get("contexts")
  checks = required.get("checks")
  values = {
    value for value in contexts if isinstance(value, str)
  } if isinstance(contexts, list) else set()
  if isinstance(checks, list):
    values.update(
      check.get("context") for check in checks
      if isinstance(check, dict) and isinstance(check.get("context"), str)
    )
  return values


def ruleset_applies(ruleset: dict[str, Any], branch: str, default_branch: str) -> bool:
  if ruleset.get("enforcement") != "active" or ruleset.get("target") not in (None, "branch"):
    return False
  conditions = ruleset.get("conditions")
  ref_name = conditions.get("ref_name") if isinstance(conditions, dict) else None
  if not isinstance(ref_name, dict):
    return False
  ref = f"refs/heads/{branch}"

  def matches(pattern: Any) -> bool:
    if pattern == "~ALL":
      return True
    if pattern == "~DEFAULT_BRANCH":
      return branch == default_branch
    return isinstance(pattern, str) and fnmatch.fnmatchcase(ref, pattern)

  includes = ref_name.get("include")
  excludes = ref_name.get("exclude")
  included = isinstance(includes, list) and any(matches(pattern) for pattern in includes)
  excluded = isinstance(excludes, list) and any(matches(pattern) for pattern in excludes)
  return included and not excluded


def branch_required_contexts(payload: dict[str, Any]) -> tuple[Optional[set[str]], Optional[str]]:
  branch_ref = payload.get("branch_ref")
  snapshot = payload.get("policy_snapshot")
  if not isinstance(branch_ref, dict) or not isinstance(snapshot, dict):
    return None, "captured branch reference or policy snapshot is missing"
  repository = branch_ref.get("repository")
  default_branch = repository.get("default_branch") if isinstance(repository, dict) else None
  branch = payload.get("branch")
  if not isinstance(branch, str) or not isinstance(default_branch, str):
    return None, "captured branch metadata is incomplete"

  contexts: set[str] = set()
  policy_found = False
  classic = snapshot.get("classic")
  if not isinstance(classic, dict):
    return None, "classic branch-protection capture is missing"
  if classic.get("status") == "ok":
    classic_payload = classic.get("payload")
    if not isinstance(classic_payload, dict):
      return None, "classic branch-protection payload is malformed"
    policy_found = True
    contexts.update(status_check_contexts(classic_payload.get("required_status_checks")))
  elif classic.get("status") != "absent":
    return None, f"classic branch-protection API was not readable: {classic.get('error') or 'unknown error'}"

  rulesets = snapshot.get("rulesets")
  if not isinstance(rulesets, dict) or rulesets.get("status") != "ok":
    detail = rulesets.get("error") if isinstance(rulesets, dict) else None
    return None, f"repository rulesets API was not readable: {detail or 'unknown error'}"
  values = rulesets.get("payload")
  if not isinstance(values, list) or any(not isinstance(value, dict) for value in values):
    return None, "repository rulesets capture is malformed"
  for ruleset in values:
    if not ruleset_applies(ruleset, branch, default_branch):
      continue
    policy_found = True
    rules = ruleset.get("rules")
    if not isinstance(rules, list):
      return None, "an applicable ruleset omits its rules"
    for rule in rules:
      if not isinstance(rule, dict) or rule.get("type") != "required_status_checks":
        continue
      parameters = rule.get("parameters")
      required = parameters.get("required_status_checks") if isinstance(parameters, dict) else None
      if isinstance(required, list):
        contexts.update(
          check.get("context") for check in required
          if isinstance(check, dict) and isinstance(check.get("context"), str)
        )
  if not policy_found:
    return None, "candidate branch has no readable protection or applicable active ruleset"
  return contexts, None


def external_evidence(
    candidate_commit: str,
    *,
    bundle_path: Optional[Path] = None,
    repository: str = CANONICAL_REPOSITORY,
    candidate_branch: str = "main",
    repo_root: Path = REPO,
    legacy_urls: Optional[dict[str, str]] = None,
) -> dict[str, dict[str, Any]]:
  if bundle_path is None:
    configured = os.environ.get(EVIDENCE_BUNDLE_ENV, "").strip()
    bundle_path = Path(configured) if configured else None
  if bundle_path is None:
    return legacy_external_evidence(legacy_urls)
  bundle_path = bundle_path.resolve()
  try:
    bundle = load_object(bundle_path)
  except (OSError, UnicodeError, json.JSONDecodeError, SystemExit):
    return {
      kind: evidence_result(kind, "unverified", "evidence bundle is not a JSON object", bundle_path=bundle_path)
      for kind in REQUIRED_EVIDENCE_KINDS
    }
  records = bundle.get("evidence")
  if bundle.get("schema_version") != EVIDENCE_SCHEMA or not isinstance(records, list):
    return {
      kind: evidence_result(kind, "unverified", "evidence bundle schema is invalid", bundle_path=bundle_path)
      for kind in REQUIRED_EVIDENCE_KINDS
    }
  by_kind: dict[str, dict[str, Any]] = {}
  duplicate_kinds: set[str] = set()
  for record in records:
    kind = record.get("kind") if isinstance(record, dict) else None
    if kind in by_kind:
      duplicate_kinds.add(kind)
    elif isinstance(kind, str) and isinstance(record, dict):
      by_kind[kind] = record

  results: dict[str, dict[str, Any]] = {}
  payloads: dict[str, dict[str, Any]] = {}
  for kind in REQUIRED_EVIDENCE_KINDS:
    record = by_kind.get(kind)
    if record is None:
      results[kind] = evidence_result(kind, "unverified", "evidence record is absent", bundle_path=bundle_path)
      continue
    if kind in duplicate_kinds:
      results[kind] = evidence_result(kind, "unverified", "evidence kind is duplicated", record=record, bundle_path=bundle_path)
      continue
    common_error = validate_common_evidence(kind, record, candidate_commit, repository)
    if common_error:
      results[kind] = evidence_result(kind, "unverified", common_error, record=record, bundle_path=bundle_path)
      continue
    payload, payload_error = load_evidence_payload(bundle_path, record)
    if payload_error or payload is None:
      results[kind] = evidence_result(kind, "unverified", payload_error or "payload is absent", record=record, bundle_path=bundle_path)
      continue
    payloads[kind] = payload

  ci_names: set[str] = set()
  ci_record = by_kind.get("remote_first_level_ci")
  ci_payload = payloads.get("remote_first_level_ci")
  if ci_record is not None and ci_payload is not None:
    capture_error = validate_workflow_captures(bundle_path, ci_payload, regeneration=False)
    if capture_error:
      status, detail, ci_names = "unverified", capture_error, set()
    else:
      status, detail, ci_names = validate_workflow_record(
        ci_record,
        ci_payload,
        workflow=FIRST_LEVEL_WORKFLOW,
        required_job_ids=REQUIRED_FIRST_LEVEL_JOB_IDS,
        repository=repository,
        candidate_commit=candidate_commit,
      )
    results["remote_first_level_ci"] = evidence_result(
      "remote_first_level_ci", status, detail, record=ci_record, bundle_path=bundle_path
    )

  regen_record = by_kind.get("reference_regeneration")
  regen_payload = payloads.get("reference_regeneration")
  if regen_record is not None and regen_payload is not None:
    capture_error = validate_workflow_captures(bundle_path, regen_payload, regeneration=True)
    if capture_error:
      status, detail = "unverified", capture_error
    else:
      status, detail, _ = validate_workflow_record(
        regen_record,
        regen_payload,
        workflow=REGENERATION_WORKFLOW,
        required_job_ids=REQUIRED_REGENERATION_JOB_IDS,
        repository=repository,
        candidate_commit=candidate_commit,
      )
    if status == "verified":
      artifacts = regen_payload.get("artifacts")
      required_artifacts = regen_record.get("required_artifacts")
      artifact_names = {
        artifact.get("name") for artifact in artifacts
        if isinstance(artifact, dict) and artifact.get("expired") is False
      } if isinstance(artifacts, list) else set()
      if (
          not isinstance(required_artifacts, dict)
          or set(required_artifacts) != REQUIRED_REGENERATION_JOB_IDS
          or not set(required_artifacts.values()).issubset(artifact_names)
      ):
        status, detail = "unverified", "captured run omits a required regeneration artifact"
      else:
        try:
          expected_locks, expected_generated = required_regeneration_hashes(repo_root)
        except (OSError, UnicodeError, json.JSONDecodeError, SystemExit):
          status, detail = "unverified", "current regeneration inputs cannot be hashed"
        else:
          record_locks = regen_record.get("reference_lock_hashes")
          record_generated = regen_record.get("generated_fixture_hashes")
          artifact_evidence = regen_payload.get("artifact_evidence")
          if record_locks != expected_locks or regen_payload.get("reference_lock_hashes") != expected_locks:
            status, detail = "unverified", "reference-lock hashes do not match the candidate"
          elif record_generated != expected_generated or regen_payload.get("generated_fixture_hashes") != expected_generated:
            status, detail = "unverified", "generated fixture hashes do not match the candidate"
          elif not isinstance(artifact_evidence, dict):
            status, detail = "unverified", "regeneration artifact manifests are missing"
          else:
            expected_lock_payloads = {
              path: load_object(repo_root / path) for path in expected_locks
            }
            for lane in sorted(REQUIRED_REGENERATION_JOB_IDS):
              entry = artifact_evidence.get(lane)
              manifest = entry.get("manifest") if isinstance(entry, dict) else None
              expected_name = required_artifacts.get(lane) if isinstance(required_artifacts, dict) else None
              expected_manifest = {
                "schema_version": "scalafim-first-level-regeneration-artifact/v1",
                "repository": repository,
                "candidate_commit": candidate_commit,
                "lane": lane,
                "evidence_lane": "release",
                "reference_lock_hashes": expected_locks,
                "generated_fixture_hashes": expected_generated,
                "reference_locks": expected_lock_payloads,
              }
              if not isinstance(entry, dict) or entry.get("name") != expected_name:
                status, detail = "unverified", f"regeneration artifact metadata for {lane} is invalid"
                break
              if manifest != expected_manifest:
                status, detail = "unverified", f"regeneration artifact manifest for {lane} is stale or invalid"
                break
    results["reference_regeneration"] = evidence_result(
      "reference_regeneration", status, detail, record=regen_record, bundle_path=bundle_path
    )

  branch_record = by_kind.get("branch_protection")
  branch_payload = payloads.get("branch_protection")
  if branch_record is not None and branch_payload is not None:
    status, detail = "verified", "branch policy requires every candidate CI job"
    policy_snapshot = branch_payload.get("policy_snapshot")
    captures, capture_error = load_capture_files(
      bundle_path,
      branch_payload,
      frozenset({"branch", "protection", "rulesets"}),
    )
    if capture_error:
      status, detail = "unverified", capture_error
    elif captures.get("branch") != branch_payload.get("branch_ref"):
      status, detail = "unverified", "raw branch capture disagrees with the normalized payload"
    elif not isinstance(policy_snapshot, dict):
      status, detail = "unverified", "captured branch policy snapshot is missing"
    elif (
        captures.get("protection") != policy_snapshot.get("classic")
        or captures.get("rulesets") != policy_snapshot.get("rulesets")
    ):
      status, detail = "unverified", "raw policy capture disagrees with the normalized payload"
    elif branch_record.get("branch") != candidate_branch:
      status, detail = "unverified", "branch evidence is for a different candidate branch"
    elif branch_record.get("policy_snapshot_sha256") != branch_record.get("payload_sha256"):
      status, detail = "unverified", "policy snapshot hash does not bind the captured payload"
    elif branch_payload.get("repository") != repository or branch_payload.get("branch") != candidate_branch:
      status, detail = "unverified", "captured branch policy names a foreign repository or branch"
    elif branch_payload.get("candidate_commit") != candidate_commit:
      status, detail = "unverified", "captured branch policy is stale for the candidate commit"
    elif branch_payload.get("branch_ref", {}).get("branch", {}).get("commit", {}).get("sha") != candidate_commit:
      status, detail = "unverified", "candidate branch did not point at the captured commit"
    elif results.get("remote_first_level_ci", {}).get("status") != "verified":
      status, detail = "unverified", "branch contexts cannot be resolved without verified candidate CI"
    elif (
        not isinstance(branch_record.get("expected_required_checks"), list)
        or any(
          not isinstance(value, str)
          for value in branch_record["expected_required_checks"]
        )
        or set(branch_record["expected_required_checks"]) != ci_names
    ):
      status, detail = "unverified", "expected branch checks disagree with captured CI job names"
    else:
      actual_contexts, policy_error = branch_required_contexts(branch_payload)
      if policy_error:
        status, detail = "unverified", policy_error
      elif actual_contexts is None or not ci_names.issubset(actual_contexts):
        status, detail = "failed", "branch policy omits a required candidate CI check"
    results["branch_protection"] = evidence_result(
      "branch_protection", status, detail, record=branch_record, bundle_path=bundle_path
    )
  return results


def benchmark_evidence(
    benchmark_path: Path,
    candidate_commit: str,
    repo_root: Path = REPO,
) -> tuple[dict[str, Any], list[str]]:
  problems: list[str] = []
  evidence: dict[str, Any] = {
    "path": relative_or_absolute(benchmark_path),
    "schema_version": None,
    "sha256": None,
    "status": "missing",
    "workload_sha256": None,
    "source_files_sha256": None,
  }
  if not benchmark_path.is_file():
    return evidence, ["benchmark receipt is missing"]
  try:
    benchmark = load_object(benchmark_path)
  except (OSError, UnicodeError, json.JSONDecodeError, SystemExit):
    evidence["status"] = "invalid"
    return evidence, ["benchmark receipt is malformed"]
  evidence.update({
    "schema_version": benchmark.get("schema_version"),
    "sha256": sha256(benchmark_path),
    "status": benchmark.get("benchmark_status"),
  })
  if benchmark.get("schema_version") != "scalafim-first-level-benchmark-receipt/v2":
    problems.append("benchmark receipt schema is not candidate-bound v2")
  source = benchmark.get("source")
  source_files = source.get("files") if isinstance(source, dict) else None
  if not isinstance(source, dict) or source.get("commit") != candidate_commit:
    problems.append("benchmark receipt is for a different candidate commit")
  if not isinstance(source, dict) or source.get("dirty") is not False:
    problems.append("benchmark receipt was not captured from a clean candidate")
  if not isinstance(source_files, dict) or not source_files:
    problems.append("benchmark receipt has no source-file hashes")
  else:
    for relative, expected in source_files.items():
      path = repo_root / relative
      if not isinstance(relative, str) or not isinstance(expected, str) or not path.is_file() or sha256(path) != expected:
        problems.append("benchmark source-file hashes do not match the candidate")
        break
    evidence["source_files_sha256"] = canonical_sha256(source_files)
    if source.get("files_sha256") != evidence["source_files_sha256"]:
      problems.append("benchmark source-file inventory hash does not match")
  if not isinstance(source, dict) or source.get("provider_revisions") != provider_pins(repo_root):
    problems.append("benchmark provider revisions do not match the candidate")
  workload = {
    "budget_profile": benchmark.get("budget_profile"),
    "budget_schema_version": benchmark.get("budget_schema_version"),
    "results": [
      {
        "benchmark": row.get("benchmark"),
        "jmh": row.get("jmh"),
        "params": row.get("params"),
      }
      for row in benchmark.get("results", [])
      if isinstance(row, dict)
    ],
  }
  evidence["workload_sha256"] = canonical_sha256(workload)
  if benchmark.get("workload") != workload or benchmark.get("workload_sha256") != evidence["workload_sha256"]:
    problems.append("benchmark workload hash does not match its exact parameters")
  if benchmark.get("fixture_hashes") != {}:
    problems.append("benchmark fixture inventory is invalid")
  raw_reports = benchmark.get("raw_reports")
  if not isinstance(raw_reports, list) or not raw_reports:
    problems.append("benchmark receipt has no raw report hashes")
  else:
    for raw in raw_reports:
      name = raw.get("name") if isinstance(raw, dict) else None
      expected = raw.get("sha256") if isinstance(raw, dict) else None
      raw_path = benchmark_path.parent / str(name)
      if not isinstance(name, str) or not isinstance(expected, str) or not raw_path.is_file() or sha256(raw_path) != expected:
        problems.append("benchmark raw report hashes do not match available artifacts")
        break
  if benchmark.get("benchmark_status") != "pass" or benchmark.get("release_eligible") is not True:
    problems.append("benchmark receipt is not release eligible")
  evidence["status"] = "verified" if not problems else "failed"
  return evidence, problems


def build_report(
    status_dir: Optional[Path],
    benchmark_path: Path,
    evidence_bundle: Optional[Path] = None,
    run_manifest_path: Optional[Path] = None,
) -> dict[str, Any]:
  commit, dirty, dirty_lines = current_source_state()
  inputs = release_inputs()
  if run_manifest_path is None and status_dir is not None:
    run_manifest_path = status_dir.parent / "run-manifest.json"
  try:
    run_manifest = load_object(run_manifest_path) if run_manifest_path is not None and run_manifest_path.is_file() else None
  except (OSError, UnicodeError, json.JSONDecodeError, SystemExit):
    run_manifest = None
  gates, local_errors = gate_results(
    status_dir,
    run_manifest,
    current_commit=commit,
    current_clean=not dirty,
    current_inputs=inputs,
  )
  branch = git("branch", "--show-current") or "main"
  external = external_evidence(commit, bundle_path=evidence_bundle, candidate_branch=branch)
  benchmark, benchmark_problems = benchmark_evidence(benchmark_path, commit)
  blockers: list[dict[str, str]] = []
  if dirty:
    blockers.append(
      {
        "detail": "the source checkout contains tracked or untracked changes",
        "kind": "dirty_worktree",
      }
    )
  for detail in local_errors:
    blockers.append({"detail": detail, "kind": "local_evidence"})
  for name, result in gates.items():
    if result["status"] != "pass":
      blockers.append(
        {
          "detail": f"required local gate {name} has status {result['status']}",
          "kind": "local_gate",
        }
      )
  for detail in benchmark_problems:
    blockers.append(
      {
        "detail": detail,
        "kind": "benchmark_receipt",
      }
    )
  for name, evidence in external.items():
    if evidence["status"] != "verified":
      blockers.append(
        {
          "detail": f"external evidence {name} has status {evidence['status']}: {evidence['detail']}",
          "kind": "external_evidence",
        }
      )

  return {
    "blocking_caveats": blockers,
    "evidence": {
      "benchmark_receipt": {
        **benchmark,
      },
      "commit": f"https://github.com/canardlapin/scalafim/commit/{commit}",
      "reference_lock": "tools/r-parity/reference-lock.json",
      "scenario_manifest": "docs/scenarios/manifest.json",
    },
    "external_evidence": external,
    "generated_at": datetime.now(timezone.utc).isoformat(),
    "local_gates": gates,
    "local_run": {
      "manifest": relative_or_absolute(run_manifest_path) if run_manifest_path is not None else None,
      "run_id": run_manifest.get("run_id") if isinstance(run_manifest, dict) else None,
      "inputs_sha256": run_manifest.get("inputs_sha256") if isinstance(run_manifest, dict) else None,
      "status": "verified" if not local_errors else "invalid",
    },
    "release_eligible": not blockers,
    "schema_version": SCHEMA,
    "source": {
      "commit": commit,
      "dirty": dirty,
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
  current_commit, current_dirty, _ = current_source_state()
  if not isinstance(source, dict) or source.get("commit") != current_commit or source.get("dirty") != current_dirty:
    errors.append("source.state")
  evidence = report.get("evidence")
  benchmark = evidence.get("benchmark_receipt") if isinstance(evidence, dict) else None
  if not isinstance(benchmark, dict):
    errors.append("evidence.benchmark_receipt")
  else:
    benchmark_path = Path(str(benchmark.get("path", "")))
    if not benchmark_path.is_absolute():
      benchmark_path = REPO / benchmark_path
    actual_benchmark, _ = benchmark_evidence(benchmark_path, current_commit)
    if benchmark != actual_benchmark:
      errors.append("evidence.benchmark_receipt")
  gates = report.get("local_gates")
  if not isinstance(gates, dict) or set(gates) != set(GATES):
    errors.append("local_gates")
  local_run = report.get("local_run")
  if not isinstance(local_run, dict):
    errors.append("local_run")
  else:
    manifest_value = local_run.get("manifest")
    manifest_path = Path(manifest_value) if isinstance(manifest_value, str) and manifest_value else None
    if manifest_path is not None and not manifest_path.is_absolute():
      manifest_path = REPO / manifest_path
    if manifest_path is not None and manifest_path.is_file():
      manifest = load_object(manifest_path)
      actual_gates, local_errors = gate_results(
        manifest_path.parent / "gates",
        manifest,
        current_commit=current_commit,
        current_clean=not current_dirty,
        current_inputs=release_inputs(),
      )
      if gates != actual_gates:
        errors.append("local_gates.receipts")
      expected_local_status = "verified" if not local_errors else "invalid"
      if local_run.get("run_id") != manifest.get("run_id") or local_run.get("inputs_sha256") != manifest.get("inputs_sha256") or local_run.get("status") != expected_local_status:
        errors.append("local_run.state")
    elif local_run.get("status") != "invalid":
      errors.append("local_run.manifest")
  if report.get("release_eligible"):
    if report.get("status") != "pass" or report.get("blocking_caveats"):
      errors.append("release_eligible")
    if not isinstance(gates, dict) or any(
      not isinstance(value, dict) or value.get("status") != "pass"
      for value in gates.values()
    ):
      errors.append("local_gates.release")
    if not isinstance(local_run, dict) or local_run.get("status") != "verified":
      errors.append("local_run.release")
    if not isinstance(benchmark, dict) or benchmark.get("status") != "verified":
      errors.append("benchmark.release")
    external = report.get("external_evidence")
    if not isinstance(external, dict) or set(external) != REQUIRED_EVIDENCE_KINDS or any(
      not isinstance(value, dict) or value.get("status") != "verified"
      for value in external.values()
    ):
      errors.append("external_evidence.release")
  if errors:
    raise SystemExit(f"invalid first-level release report {path}: {', '.join(errors)}")
  print(f"validated first-level release report: {path}")


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--status-dir", type=Path)
  parser.add_argument("--benchmark", type=Path, default=DEFAULT_BENCHMARK)
  parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
  parser.add_argument("--check", type=Path)
  parser.add_argument("--external-evidence", type=Path)
  parser.add_argument("--run-manifest", type=Path)
  parser.add_argument("--initialize-run", type=Path)
  parser.add_argument("--record-gate", choices=tuple(GATES))
  parser.add_argument("--gate-command")
  parser.add_argument("--gate-exit-code", type=int)
  parser.add_argument("--gate-started-at")
  parser.add_argument("--gate-ended-at")
  parser.add_argument("--gate-log", type=Path)
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  if args.initialize_run is not None:
    if args.record_gate is not None or args.check is not None:
      raise SystemExit("--initialize-run cannot be combined with another operation")
    manifest = initialize_run(args.initialize_run.resolve())
    print(f"initialized release run {manifest['run_id']}: {args.initialize_run}")
    return 0
  if args.record_gate is not None:
    required = {
      "--run-manifest": args.run_manifest,
      "--status-dir": args.status_dir,
      "--gate-command": args.gate_command,
      "--gate-exit-code": args.gate_exit_code,
      "--gate-started-at": args.gate_started_at,
      "--gate-ended-at": args.gate_ended_at,
      "--gate-log": args.gate_log,
    }
    missing = [name for name, value in required.items() if value is None]
    if missing:
      raise SystemExit("gate recording requires " + ", ".join(missing))
    record_gate(
      args.run_manifest.resolve(),
      args.status_dir.resolve(),
      args.record_gate,
      args.gate_command,
      args.gate_exit_code,
      args.gate_started_at,
      args.gate_ended_at,
      args.gate_log.resolve(),
    )
    print(f"recorded release gate {args.record_gate}")
    return 0
  if args.check is not None:
    validate_report(args.check)
    return 0
  benchmark_path = args.benchmark.resolve()
  status_dir = args.status_dir.resolve() if args.status_dir is not None else None
  evidence_bundle = args.external_evidence.resolve() if args.external_evidence is not None else None
  run_manifest = args.run_manifest.resolve() if args.run_manifest is not None else None
  report = build_report(status_dir, benchmark_path, evidence_bundle, run_manifest)
  args.output.parent.mkdir(parents=True, exist_ok=True)
  args.output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
  print(f"wrote {args.output}: {report['status']}")
  return 0 if report["release_eligible"] else 1


if __name__ == "__main__":
  raise SystemExit(main())
