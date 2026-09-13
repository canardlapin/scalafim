#!/usr/bin/env python3
"""Collect an offline-verifiable snapshot of effective GitHub branch policy."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
from pathlib import Path
import re
import subprocess
from typing import Any


from collect_first_level_run_evidence import (
  CI_JOBS,
  EvidenceError,
  capture,
  full_sha,
  load_object,
  sha256,
  upsert_bundle,
  write_object,
)
from finalize_first_level_release import CANONICAL_REPOSITORY, ruleset_applies


REPO = Path(__file__).resolve().parents[2]
GH = REPO / "tools/github/gh-repo"


def api_value(endpoint: str) -> Any:
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
    return json.loads(completed.stdout)
  except json.JSONDecodeError as error:
    raise EvidenceError(f"GitHub API returned malformed JSON for {endpoint}") from error


def http_status(detail: str) -> int | None:
  match = re.search(r"HTTP\s+(\d{3})", detail, flags=re.IGNORECASE)
  return int(match.group(1)) if match else None


def policy_api_capture(endpoint: str, *, absent_on_404: bool = False) -> dict[str, Any]:
  completed = subprocess.run(
    [str(GH), "api", endpoint],
    cwd=REPO,
    check=False,
    capture_output=True,
    text=True,
  )
  if completed.returncode != 0:
    detail = completed.stderr.strip() or completed.stdout.strip() or "unknown gh failure"
    status = http_status(detail)
    return {
      "status": "absent" if absent_on_404 and status == 404 else "error",
      "http_status": status,
      "endpoint": endpoint,
      "error": detail,
      "payload": None,
    }
  try:
    value = json.loads(completed.stdout)
  except json.JSONDecodeError:
    return {
      "status": "error",
      "http_status": None,
      "endpoint": endpoint,
      "error": "GitHub API returned malformed JSON",
      "payload": None,
    }
  return {
    "status": "ok",
    "http_status": 200,
    "endpoint": endpoint,
    "error": None,
    "payload": value,
  }


def collect_rulesets(repository: str) -> dict[str, Any]:
  endpoint = f"repos/{repository}/rulesets?includes_parents=true"
  listing = policy_api_capture(endpoint)
  if listing.get("status") != "ok":
    return listing
  summaries = listing.get("payload")
  if not isinstance(summaries, list):
    return {**listing, "status": "error", "error": "rulesets response is not a list", "payload": None}
  details: list[dict[str, Any]] = []
  for summary in summaries:
    if not isinstance(summary, dict):
      return {**listing, "status": "error", "error": "rulesets response contains a malformed entry", "payload": None}
    if summary.get("target") != "branch" or summary.get("enforcement") != "active":
      continue
    ruleset_id = summary.get("id")
    if not isinstance(ruleset_id, int):
      return {**listing, "status": "error", "error": "active branch ruleset has no numeric id", "payload": None}
    detail = policy_api_capture(
      f"repos/{repository}/rulesets/{ruleset_id}?includes_parents=true"
    )
    if detail.get("status") != "ok" or not isinstance(detail.get("payload"), dict):
      return {
        "status": "error",
        "http_status": detail.get("http_status"),
        "endpoint": endpoint,
        "error": f"cannot read active ruleset {ruleset_id}: {detail.get('error') or 'malformed response'}",
        "payload": None,
      }
    details.append(detail["payload"])
  return {**listing, "payload": details}


def policy_contexts(
    classic: dict[str, Any],
    rulesets: dict[str, Any],
    *,
    branch: str,
    default_branch: str,
) -> tuple[set[str] | None, str | None]:
  contexts: set[str] = set()
  policy_found = False
  if classic.get("status") == "ok":
    payload = classic.get("payload")
    if not isinstance(payload, dict):
      return None, "classic branch-protection payload is malformed"
    policy_found = True
    required = payload.get("required_status_checks")
    if isinstance(required, dict):
      values = required.get("contexts")
      if isinstance(values, list):
        contexts.update(value for value in values if isinstance(value, str))
      checks = required.get("checks")
      if isinstance(checks, list):
        contexts.update(
          check.get("context") for check in checks
          if isinstance(check, dict) and isinstance(check.get("context"), str)
        )
  elif classic.get("status") != "absent":
    return None, f"classic branch-protection API was not readable: {classic.get('error') or 'unknown error'}"

  if rulesets.get("status") != "ok" or not isinstance(rulesets.get("payload"), list):
    return None, f"repository rulesets API was not readable: {rulesets.get('error') or 'malformed response'}"
  for ruleset in rulesets["payload"]:
    if not isinstance(ruleset, dict) or not ruleset_applies(ruleset, branch, default_branch):
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


def expected_checks_from_bundle(bundle_path: Path, candidate: str) -> list[str]:
  bundle = load_object(bundle_path)
  entries = bundle.get("evidence")
  if not isinstance(entries, list):
    raise EvidenceError("existing evidence bundle has no records")
  matches = [
    entry for entry in entries
    if isinstance(entry, dict) and entry.get("kind") == "remote_first_level_ci"
  ]
  if len(matches) != 1:
    raise EvidenceError("bundle must contain exactly one remote first-level CI record")
  record = matches[0]
  if record.get("candidate_commit") != candidate:
    raise EvidenceError("remote first-level CI record is stale for the candidate")
  jobs = record.get("required_jobs")
  if not isinstance(jobs, dict) or set(jobs) != set(CI_JOBS):
    raise EvidenceError("remote first-level CI record has an incomplete job inventory")
  values = list(jobs.values())
  if any(not isinstance(value, str) or not value for value in values):
    raise EvidenceError("remote first-level CI record has invalid job names")
  return sorted(values)


def collect(args: argparse.Namespace) -> Path:
  candidate = full_sha(args.candidate)
  output = args.output_dir.resolve()
  bundle_path = output / "evidence.json"
  expected_checks = expected_checks_from_bundle(bundle_path, candidate)

  repository_response = api_value(f"repos/{args.repository}")
  branch_response = api_value(f"repos/{args.repository}/branches/{args.branch}")
  if not isinstance(repository_response, dict) or repository_response.get("full_name") != args.repository:
    raise EvidenceError("repository API response names a foreign repository")
  if not isinstance(branch_response, dict) or branch_response.get("name") != args.branch:
    raise EvidenceError("branch API response names a different branch")
  commit = branch_response.get("commit")
  if not isinstance(commit, dict) or commit.get("sha") != candidate:
    raise EvidenceError("candidate branch does not point at the requested commit")
  default_branch = repository_response.get("default_branch")
  if not isinstance(default_branch, str):
    raise EvidenceError("repository API response omits the default branch")

  classic = policy_api_capture(
    f"repos/{args.repository}/branches/{args.branch}/protection",
    absent_on_404=True,
  )
  rulesets = collect_rulesets(args.repository)
  actual_contexts, policy_error = policy_contexts(
    classic,
    rulesets,
    branch=args.branch,
    default_branch=default_branch,
  )

  raw = output / "raw"
  branch_ref = {"repository": repository_response, "branch": branch_response}
  captures = {
    "branch": capture(raw / "branch-reference.json", branch_ref, output),
    "protection": capture(raw / "branch-protection.json", classic, output),
    "rulesets": capture(raw / "repository-rulesets.json", rulesets, output),
  }
  payload = {
    "repository": args.repository,
    "branch": args.branch,
    "candidate_commit": candidate,
    "branch_ref": branch_ref,
    "policy_snapshot": {"classic": classic, "rulesets": rulesets},
    "resolved_required_status_checks": sorted(actual_contexts) if actual_contexts is not None else None,
    "collection_error": policy_error,
    "captures": captures,
  }
  payload_path = output / "payloads/branch-policy.json"
  write_object(payload_path, payload)
  payload_hash = sha256(payload_path)
  record = {
    "kind": "branch_protection",
    "repository": args.repository,
    "candidate_commit": candidate,
    "observed_at": datetime.now(timezone.utc).isoformat(),
    "source_url": f"https://api.github.com/repos/{args.repository}/branches/{args.branch}/protection",
    "branch": args.branch,
    "expected_required_checks": expected_checks,
    "payload_path": payload_path.relative_to(output).as_posix(),
    "payload_sha256": payload_hash,
    "policy_snapshot_sha256": payload_hash,
  }
  upsert_bundle(bundle_path, [record])
  return bundle_path


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--repository", default=CANONICAL_REPOSITORY)
  parser.add_argument("--branch", default="main")
  parser.add_argument("--candidate", required=True)
  parser.add_argument("--output-dir", required=True, type=Path)
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  if args.repository != CANONICAL_REPOSITORY:
    raise SystemExit(f"release evidence must target {CANONICAL_REPOSITORY}")
  try:
    bundle = collect(args)
  except EvidenceError as error:
    raise SystemExit(str(error)) from error
  print(f"wrote candidate-bound branch evidence: {bundle}")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
