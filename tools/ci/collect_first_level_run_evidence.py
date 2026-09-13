#!/usr/bin/env python3
"""Collect candidate-bound GitHub workflow evidence for the first-level court."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
from typing import Any


from finalize_first_level_release import (
  CANONICAL_REPOSITORY,
  EVIDENCE_SCHEMA,
  FIRST_LEVEL_WORKFLOW,
  REGENERATION_WORKFLOW,
  required_regeneration_hashes,
)


REPO = Path(__file__).resolve().parents[2]
GH = REPO / "tools/github/gh-repo"
ARTIFACT_SCHEMA = "scalafim-first-level-regeneration-artifact/v1"
CI_JOBS = {
  "full-repository": "full repository JVM/Scala.js compile and test",
  "focused-first-level": "focused first-level JVM/Scala.js gates",
  "scientific-coverage": "scoped first-level scientific coverage",
}
REGENERATION_JOBS = {
  "r-receipts": "locked R receipt regeneration",
  "python-receipts": "locked Python and Nilearn receipt regeneration",
}
REGENERATION_ARTIFACTS = {
  "r-receipts": ("locked-r-receipts", "scenario-r-regeneration-evidence.json"),
  "python-receipts": ("locked-python-receipts", "scenario-python-regeneration-evidence.json"),
}


class EvidenceError(ValueError):
  pass


def sha256(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def load_object(path: Path) -> dict[str, Any]:
  value = json.loads(path.read_text())
  if not isinstance(value, dict):
    raise EvidenceError(f"expected a JSON object: {path}")
  return value


def write_object(path: Path, value: dict[str, Any]) -> None:
  path.parent.mkdir(parents=True, exist_ok=True)
  path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def full_sha(value: str) -> str:
  normalized = value.strip().lower()
  if re.fullmatch(r"[0-9a-f]{40}", normalized) is None:
    raise EvidenceError("candidate must be a full 40-character Git commit SHA")
  return normalized


def api_object(endpoint: str) -> dict[str, Any]:
  completed = subprocess.run(
    [str(GH), "api", endpoint],
    cwd=REPO,
    check=False,
    capture_output=True,
    text=True,
  )
  if completed.returncode != 0:
    detail = completed.stderr.strip() or completed.stdout.strip() or "unknown gh failure"
    raise EvidenceError(f"GitHub API request failed for {endpoint}: {detail}")
  try:
    value = json.loads(completed.stdout)
  except json.JSONDecodeError as error:
    raise EvidenceError(f"GitHub API returned malformed JSON for {endpoint}") from error
  if not isinstance(value, dict):
    raise EvidenceError(f"GitHub API response is not an object for {endpoint}")
  return value


def repository_name(run: dict[str, Any]) -> Any:
  repository = run.get("repository")
  return repository.get("full_name") if isinstance(repository, dict) else None


def parse_workflow_capture(
    run: dict[str, Any],
    jobs_response: dict[str, Any],
    *,
    repository: str,
    candidate: str,
    run_id: int,
    workflow: str,
    required_jobs: dict[str, str],
) -> tuple[dict[str, Any], list[dict[str, Any]]]:
  if run.get("id") != run_id:
    raise EvidenceError("workflow response has the wrong run ID")
  if repository_name(run) != repository:
    raise EvidenceError("workflow response names a foreign repository")
  if run.get("head_sha") != candidate:
    raise EvidenceError("workflow response is for a different candidate commit")
  if run.get("path") != workflow:
    raise EvidenceError("workflow response has the wrong workflow path")
  attempt = run.get("run_attempt")
  if not isinstance(attempt, int) or attempt < 1:
    raise EvidenceError("workflow response has an invalid attempt number")
  jobs = jobs_response.get("jobs")
  total = jobs_response.get("total_count")
  if not isinstance(jobs, list) or not isinstance(total, int) or total != len(jobs):
    raise EvidenceError("workflow jobs response is incomplete or malformed")
  if any(not isinstance(job, dict) for job in jobs):
    raise EvidenceError("workflow jobs response contains a malformed job")
  if any(job.get("run_attempt") not in (None, attempt) for job in jobs):
    raise EvidenceError("workflow jobs were captured from a different rerun attempt")
  names = [job.get("name") for job in jobs]
  if len(names) != len(set(names)):
    raise EvidenceError("workflow jobs response contains duplicate job names")
  missing = sorted(set(required_jobs.values()).difference(name for name in names if isinstance(name, str)))
  if missing:
    raise EvidenceError("workflow jobs response omits required jobs: " + ", ".join(missing))
  return run, jobs


def local_regeneration_state(repo_root: Path) -> tuple[dict[str, str], dict[str, str], dict[str, Any]]:
  lock_hashes, generated_hashes = required_regeneration_hashes(repo_root)
  locks = {path: load_object(repo_root / path) for path in lock_hashes}
  return lock_hashes, generated_hashes, locks


def regeneration_artifact_manifest(
    lane: str,
    candidate: str,
    *,
    repository: str = CANONICAL_REPOSITORY,
    evidence_lane: str,
    repo_root: Path = REPO,
) -> dict[str, Any]:
  if lane not in REGENERATION_ARTIFACTS:
    raise EvidenceError(f"unknown regeneration lane: {lane}")
  lock_hashes, generated_hashes, locks = local_regeneration_state(repo_root)
  return {
    "schema_version": ARTIFACT_SCHEMA,
    "repository": repository,
    "candidate_commit": full_sha(candidate),
    "lane": lane,
    "evidence_lane": evidence_lane,
    "reference_lock_hashes": lock_hashes,
    "generated_fixture_hashes": generated_hashes,
    "reference_locks": locks,
  }


def validate_regeneration_artifact(
    manifest: dict[str, Any],
    *,
    lane: str,
    repository: str,
    candidate: str,
    repo_root: Path,
) -> None:
  expected_locks, expected_generated, expected_lock_payloads = local_regeneration_state(repo_root)
  expected = {
    "schema_version": ARTIFACT_SCHEMA,
    "repository": repository,
    "candidate_commit": candidate,
    "lane": lane,
    "evidence_lane": "release",
    "reference_lock_hashes": expected_locks,
    "generated_fixture_hashes": expected_generated,
    "reference_locks": expected_lock_payloads,
  }
  for key, value in expected.items():
    if manifest.get(key) != value:
      raise EvidenceError(f"{lane} regeneration artifact has stale or invalid {key}")


def capture(path: Path, value: dict[str, Any], bundle_root: Path) -> dict[str, str]:
  write_object(path, value)
  return {
    "path": path.relative_to(bundle_root).as_posix(),
    "sha256": sha256(path),
  }


def download_artifact(repository: str, run_id: int, name: str, destination: Path) -> None:
  if destination.exists():
    shutil.rmtree(destination)
  destination.mkdir(parents=True)
  completed = subprocess.run(
    [str(GH), "run", "download", str(run_id), "--repo", repository, "--name", name, "--dir", str(destination)],
    cwd=REPO,
    check=False,
    capture_output=True,
    text=True,
  )
  if completed.returncode != 0:
    detail = completed.stderr.strip() or completed.stdout.strip() or "unknown gh failure"
    raise EvidenceError(f"cannot download required artifact {name!r}: {detail}")


def artifact_entry(
    *,
    bundle_root: Path,
    artifact_root: Path,
    lane: str,
    repository: str,
    candidate: str,
    repo_root: Path,
) -> tuple[dict[str, Any], dict[str, str]]:
  artifact_name, filename = REGENERATION_ARTIFACTS[lane]
  matches = list(artifact_root.rglob(filename))
  if len(matches) != 1:
    raise EvidenceError(f"artifact {artifact_name!r} must contain exactly one {filename}")
  path = matches[0]
  manifest = load_object(path)
  validate_regeneration_artifact(
    manifest,
    lane=lane,
    repository=repository,
    candidate=candidate,
    repo_root=repo_root,
  )
  captured = {
    "path": path.relative_to(bundle_root).as_posix(),
    "sha256": sha256(path),
  }
  return {
    "name": artifact_name,
    "manifest": manifest,
    **captured,
  }, captured


def workflow_record(
    *,
    kind: str,
    repository: str,
    candidate: str,
    workflow: str,
    run: dict[str, Any],
    required_jobs: dict[str, str],
    payload_path: Path,
    bundle_root: Path,
) -> dict[str, Any]:
  return {
    "kind": kind,
    "repository": repository,
    "candidate_commit": candidate,
    "observed_at": datetime.now(timezone.utc).isoformat(),
    "source_url": run.get("html_url"),
    "workflow": workflow,
    "run_id": run.get("id"),
    "run_attempt": run.get("run_attempt"),
    "head_sha": run.get("head_sha"),
    "status": run.get("status"),
    "conclusion": run.get("conclusion"),
    "required_jobs": required_jobs,
    "payload_path": payload_path.relative_to(bundle_root).as_posix(),
    "payload_sha256": sha256(payload_path),
  }


def upsert_bundle(path: Path, records: list[dict[str, Any]]) -> None:
  existing: list[dict[str, Any]] = []
  if path.is_file():
    bundle = load_object(path)
    if bundle.get("schema_version") != EVIDENCE_SCHEMA or not isinstance(bundle.get("evidence"), list):
      raise EvidenceError("existing evidence bundle has an invalid schema")
    existing = [entry for entry in bundle["evidence"] if isinstance(entry, dict)]
  kinds = {record["kind"] for record in records}
  merged = [record for record in existing if record.get("kind") not in kinds] + records
  write_object(path, {"schema_version": EVIDENCE_SCHEMA, "evidence": merged})


def collect(args: argparse.Namespace) -> Path:
  candidate = full_sha(args.candidate)
  output = args.output_dir.resolve()
  raw = output / "raw"
  payloads = output / "payloads"
  artifacts_root = output / "artifacts"
  output.mkdir(parents=True, exist_ok=True)

  ci_run = api_object(f"repos/{args.repository}/actions/runs/{args.ci_run_id}")
  ci_attempt = ci_run.get("run_attempt")
  ci_jobs_response = api_object(
    f"repos/{args.repository}/actions/runs/{args.ci_run_id}/attempts/{ci_attempt}/jobs?per_page=100"
  )
  ci_run, ci_jobs = parse_workflow_capture(
    ci_run,
    ci_jobs_response,
    repository=args.repository,
    candidate=candidate,
    run_id=args.ci_run_id,
    workflow=FIRST_LEVEL_WORKFLOW,
    required_jobs=CI_JOBS,
  )
  ci_captures = {
    "run": capture(raw / "first-level-run.json", ci_run, output),
    "jobs": capture(raw / "first-level-jobs.json", ci_jobs_response, output),
  }
  ci_payload = {
    "repository": args.repository,
    "workflow_run": ci_run,
    "jobs": ci_jobs,
    "captures": ci_captures,
  }
  ci_payload_path = payloads / "first-level.json"
  write_object(ci_payload_path, ci_payload)
  ci_record = workflow_record(
    kind="remote_first_level_ci",
    repository=args.repository,
    candidate=candidate,
    workflow=FIRST_LEVEL_WORKFLOW,
    run=ci_run,
    required_jobs=CI_JOBS,
    payload_path=ci_payload_path,
    bundle_root=output,
  )

  regen_run = api_object(f"repos/{args.repository}/actions/runs/{args.regeneration_run_id}")
  regen_attempt = regen_run.get("run_attempt")
  regen_jobs_response = api_object(
    f"repos/{args.repository}/actions/runs/{args.regeneration_run_id}/attempts/{regen_attempt}/jobs?per_page=100"
  )
  regen_artifacts_response = api_object(
    f"repos/{args.repository}/actions/runs/{args.regeneration_run_id}/artifacts?per_page=100"
  )
  regen_run, regen_jobs = parse_workflow_capture(
    regen_run,
    regen_jobs_response,
    repository=args.repository,
    candidate=candidate,
    run_id=args.regeneration_run_id,
    workflow=REGENERATION_WORKFLOW,
    required_jobs=REGENERATION_JOBS,
  )
  artifact_api = regen_artifacts_response.get("artifacts")
  artifact_total = regen_artifacts_response.get("total_count")
  if not isinstance(artifact_api, list) or artifact_total != len(artifact_api):
    raise EvidenceError("regeneration artifacts response is incomplete or malformed")
  live_names = [
    artifact.get("name") for artifact in artifact_api
    if isinstance(artifact, dict) and artifact.get("expired") is False
  ]
  required_names = {value[0] for value in REGENERATION_ARTIFACTS.values()}
  missing_artifacts = sorted(required_names.difference(live_names))
  if missing_artifacts:
    raise EvidenceError("regeneration run omits required artifacts: " + ", ".join(missing_artifacts))
  duplicate_artifacts = sorted(name for name in required_names if live_names.count(name) != 1)
  if duplicate_artifacts:
    raise EvidenceError("regeneration run has ambiguous artifacts across attempts: " + ", ".join(duplicate_artifacts))

  artifact_evidence: dict[str, dict[str, Any]] = {}
  artifact_captures: dict[str, dict[str, str]] = {}
  for lane, (name, _) in REGENERATION_ARTIFACTS.items():
    destination = artifacts_root / name
    download_artifact(args.repository, args.regeneration_run_id, name, destination)
    entry, captured = artifact_entry(
      bundle_root=output,
      artifact_root=destination,
      lane=lane,
      repository=args.repository,
      candidate=candidate,
      repo_root=REPO,
    )
    artifact_evidence[lane] = entry
    artifact_captures["artifact_r" if lane == "r-receipts" else "artifact_python"] = captured

  lock_hashes, generated_hashes, _ = local_regeneration_state(REPO)
  regen_captures = {
    "run": capture(raw / "regeneration-run.json", regen_run, output),
    "jobs": capture(raw / "regeneration-jobs.json", regen_jobs_response, output),
    "artifacts": capture(raw / "regeneration-artifacts.json", regen_artifacts_response, output),
    **artifact_captures,
  }
  regen_payload = {
    "repository": args.repository,
    "workflow_run": regen_run,
    "jobs": regen_jobs,
    "artifacts": artifact_api,
    "artifact_evidence": artifact_evidence,
    "reference_lock_hashes": lock_hashes,
    "generated_fixture_hashes": generated_hashes,
    "captures": regen_captures,
  }
  regen_payload_path = payloads / "regeneration.json"
  write_object(regen_payload_path, regen_payload)
  regen_record = workflow_record(
    kind="reference_regeneration",
    repository=args.repository,
    candidate=candidate,
    workflow=REGENERATION_WORKFLOW,
    run=regen_run,
    required_jobs=REGENERATION_JOBS,
    payload_path=regen_payload_path,
    bundle_root=output,
  )
  regen_record.update({
    "required_artifacts": {key: value[0] for key, value in REGENERATION_ARTIFACTS.items()},
    "reference_lock_hashes": lock_hashes,
    "generated_fixture_hashes": generated_hashes,
  })

  bundle_path = output / "evidence.json"
  upsert_bundle(bundle_path, [ci_record, regen_record])
  return bundle_path


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--repository", default=CANONICAL_REPOSITORY)
  parser.add_argument("--candidate")
  parser.add_argument("--ci-run-id", type=int)
  parser.add_argument("--regeneration-run-id", type=int)
  parser.add_argument("--output-dir", type=Path)
  parser.add_argument("--write-regeneration-manifest", choices=tuple(REGENERATION_ARTIFACTS))
  parser.add_argument("--evidence-lane")
  parser.add_argument("--output", type=Path)
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  try:
    if args.write_regeneration_manifest is not None:
      if args.candidate is None or args.evidence_lane is None or args.output is None:
        raise EvidenceError("manifest mode requires --candidate, --evidence-lane, and --output")
      manifest = regeneration_artifact_manifest(
        args.write_regeneration_manifest,
        args.candidate,
        repository=args.repository,
        evidence_lane=args.evidence_lane,
      )
      write_object(args.output, manifest)
      print(f"wrote {args.output}")
      return 0
    if args.repository != CANONICAL_REPOSITORY:
      raise EvidenceError(f"release evidence must target {CANONICAL_REPOSITORY}")
    if args.candidate is None or args.ci_run_id is None or args.regeneration_run_id is None or args.output_dir is None:
      raise EvidenceError("collection requires --candidate, --ci-run-id, --regeneration-run-id, and --output-dir")
    bundle = collect(args)
    print(f"wrote candidate-bound workflow evidence: {bundle}")
    return 0
  except EvidenceError as error:
    raise SystemExit(str(error)) from error


if __name__ == "__main__":
  raise SystemExit(main())
