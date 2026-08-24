#!/usr/bin/env python3
"""Shared, dependency-free finalization for generated R scenario receipts."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[2]
LOCK_PATH = REPO_ROOT / "tools" / "r-parity" / "reference-lock.json"
LOCK_SCHEMA = "scalafim-parity-environment-lock/v1"
COMPARISON_POLICY = {
  "alignment": "identity_only_no_post_hoc_lag_shift",
  "correlation_role": "diagnostic_only_requires_error_and_norm_bounds",
  "disagreement_triage": "truth_boundary_local",
  "tolerance_scope": "comparison_local_no_global_override",
}


def canonical_sha256(value: Any) -> str:
  encoded = json.dumps(
    value,
    ensure_ascii=False,
    allow_nan=False,
    sort_keys=True,
    separators=(",", ":"),
  ).encode("utf-8")
  return hashlib.sha256(encoded).hexdigest()


def file_sha256(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def check_requested(description: str) -> bool:
  parser = argparse.ArgumentParser(description=description)
  parser.add_argument(
    "--check",
    action="store_true",
    help="verify receipt hashes and the generated Scala fixture without writing",
  )
  return bool(parser.parse_args().check)


def load_environment_lock() -> dict[str, Any]:
  payload = json.loads(LOCK_PATH.read_text())
  if payload.get("schema_version") != LOCK_SCHEMA:
    raise SystemExit(f"unexpected environment lock schema: {payload.get('schema_version')!r}")
  return payload


def require_locked_r_source(source: dict[str, Any], lock: dict[str, Any]) -> None:
  r_lock = lock.get("r")
  if not isinstance(r_lock, dict):
    raise SystemExit("environment lock must contain an R section")
  if source.get("r_version") != r_lock.get("version"):
    raise SystemExit(
      f"receipt R version {source.get('r_version')!r} does not match lock {r_lock.get('version')!r}"
    )

  packages = r_lock.get("packages")
  if not isinstance(packages, dict):
    raise SystemExit("environment lock R packages must be an object")
  for package, package_lock in packages.items():
    if not isinstance(package_lock, dict):
      raise SystemExit(f"environment lock entry for {package} must be an object")
    revision_key = f"{package}_revision"
    version_key = f"{package}_version"
    if revision_key in source and source.get(revision_key) != package_lock.get("revision"):
      raise SystemExit(
        f"receipt {revision_key} {source.get(revision_key)!r} does not match lock {package_lock.get('revision')!r}"
      )
    if version_key in source and source.get(version_key) != package_lock.get("version"):
      raise SystemExit(
        f"receipt {version_key} {source.get(version_key)!r} does not match lock {package_lock.get('version')!r}"
      )


def receipt_environment(source: dict[str, Any], lock: dict[str, Any]) -> dict[str, str]:
  producer = source.get("producer")
  if not isinstance(producer, str) or not producer.strip():
    raise SystemExit("receipt source.producer must name the generator")
  producer_path = (REPO_ROOT / producer).resolve()
  try:
    producer_path.relative_to(REPO_ROOT)
  except ValueError as error:
    raise SystemExit(f"receipt generator is outside the repository: {producer}") from error
  if not producer_path.is_file():
    raise SystemExit(f"receipt generator does not exist: {producer}")

  r_lock = lock["r"]
  return {
    "generator_sha256": file_sha256(producer_path),
    "locale": str(r_lock["locale"]),
    "lock_path": str(LOCK_PATH.relative_to(REPO_ROOT)),
    "lock_sha256": file_sha256(LOCK_PATH),
    "runtime": f"R {source['r_version']}",
  }


def finalize_receipt(
    fixture_path: str,
    schema: str,
    scala_fixture_path: str,
    truth_boundary: str,
    *,
    check: bool,
) -> int:
  json_path = REPO_ROOT / fixture_path
  scala_path = REPO_ROOT / scala_fixture_path
  payload = json.loads(json_path.read_text())
  if payload.get("schema_version") != schema:
    raise SystemExit(f"unexpected schema_version: {payload.get('schema_version')!r}")
  if not scala_path.is_file():
    raise SystemExit(f"generated Scala fixture does not exist: {scala_path}")

  receipt = payload.get("receipt")
  if not isinstance(receipt, dict):
    raise SystemExit(f"receipt must be an object: {json_path}")
  source = payload.get("source")
  if not isinstance(source, dict):
    raise SystemExit(f"source must be an object: {json_path}")
  lock = load_environment_lock()
  require_locked_r_source(source, lock)
  expected_environment = receipt_environment(source, lock)
  expected_hashes = {
    "inputs_sha256": canonical_sha256({"schema_version": schema, "inputs": payload["inputs"]}),
    "outputs_sha256": canonical_sha256({"schema_version": schema, "outputs": payload["outputs"]}),
    "generated_scala_sha256": file_sha256(scala_path),
  }

  if check:
    stale = []
    if payload.get("truth_boundary") != truth_boundary:
      stale.append("truth_boundary")
    if receipt.get("source") != source:
      stale.append("receipt.source")
    if receipt.get("environment") != expected_environment:
      stale.append("receipt.environment")
    if receipt.get("comparison_policy") != COMPARISON_POLICY:
      stale.append("receipt.comparison_policy")
    actual_hashes = receipt.get("hashes")
    for key, expected in expected_hashes.items():
      if not isinstance(actual_hashes, dict) or actual_hashes.get(key) != expected:
        stale.append(f"receipt.hashes.{key}")
    if stale:
      raise SystemExit(f"stale receipt {json_path}: {', '.join(stale)}")
    print(f"checked {json_path.relative_to(REPO_ROOT)} and {scala_path.relative_to(REPO_ROOT)}")
    return 0

  if os.environ.get("LC_ALL") not in (None, "C"):
    raise SystemExit("receipt finalization requires LC_ALL=C")
  if os.environ.get("LANG") not in (None, "C"):
    raise SystemExit("receipt finalization requires LANG=C")

  payload["truth_boundary"] = truth_boundary
  receipt["producer_command"] = "bash tools/r-parity/regenerate_receipts.sh --r-only"
  receipt["environment"] = expected_environment
  receipt["comparison_policy"] = COMPARISON_POLICY
  receipt["hashes"] = expected_hashes
  receipt["source"] = source
  json_path.write_text(json.dumps(payload, ensure_ascii=False, sort_keys=True, indent=2) + "\n")
  print(f"finalized {json_path.relative_to(REPO_ROOT)}")
  return 0
