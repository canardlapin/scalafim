#!/usr/bin/env python3
"""Validate the executable ScalaFIM scenario manifest without third-party packages."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import sys
from typing import Any


MANIFEST_SCHEMA = "scalafim-scenario-manifest/v1"
FIXTURE_SCHEMA = "scalafim-external-fixture/v1"
R_DESIGN_FIXTURE_SCHEMA = "scalafim-r-design-fixture/v1"
R_POLICY_FIXTURE_SCHEMA = "scalafim-r-policy-fixture/v1"
R_KERNEL_FIXTURE_SCHEMA = "scalafim-r-kernel-fixture/v1"
R_WLS_FIXTURE_SCHEMA = "scalafim-r-wls-fixture/v1"
R_FIXED_EFFECTS_FIXTURE_SCHEMA = "scalafim-r-fixed-effects-fixture/v1"
R_DMS_FIXTURE_SCHEMA = "scalafim-r-dms-fixture/v1"
R_REALISTIC_NUISANCE_FIXTURE_SCHEMA = "scalafim-r-realistic-nuisance-fixture/v1"
R_AR_CENSOR_GLS_FIXTURE_SCHEMA = "scalafim-r-ar-censor-gls-fixture/v1"
LOCAL_AXIS_FIXTURE_SCHEMA = "scalafim-local-axis-oracle/v1"
SLOW_ORACLE_FIXTURE_SCHEMA = "scalafim-slow-oracle/v1"
REFERENCE_LOCK_SCHEMA = "scalafim-parity-environment-lock/v1"
REFERENCE_LOCK_PATH = Path(__file__).resolve().parents[2] / "tools" / "r-parity" / "reference-lock.json"
ALLOWED_PLATFORMS = {"jvm", "js"}
ALLOWED_STATUSES = {"pass", "pass_with_caveats", "fail"}
ALLOWED_EVIDENCE_SCOPES = {
  "hrf_evaluation",
  "design_construction",
  "fit_inference",
  "independent_end_to_end",
}
ALLOWED_COMPARISON_METRICS = {
  "absolute_l2_error",
  "relative_l2_error",
  "signed_correlation",
  "norm_ratio",
  "peak_timing",
  "integral",
  "semantic_alignment",
  "rank",
  "residual_df",
  "log_p",
}
BASE_NUMERIC_METRICS = {
  "absolute_l2_error",
  "relative_l2_error",
  "signed_correlation",
  "norm_ratio",
}
REQUIRED_METRICS_BY_SCOPE = {
  "hrf_evaluation": BASE_NUMERIC_METRICS | {"peak_timing", "integral", "semantic_alignment"},
  "design_construction": BASE_NUMERIC_METRICS | {"semantic_alignment"},
  "fit_inference": BASE_NUMERIC_METRICS | {"semantic_alignment", "rank", "residual_df"},
  "independent_end_to_end": BASE_NUMERIC_METRICS | {"semantic_alignment", "rank", "residual_df"},
}
RECEIPT_SOURCE_FIELDS = (
  "fmrimod_revision",
  "fmrimod_fitlins_parity_sha256",
  "numpy_version",
  "scipy_version",
  "nilearn_version",
  "reference_revision",
)
RECEIPT_CONVENTION_FIELDS = (
  "dtype",
  "json_encoding",
  "matrix_orientation",
  "nilearn_glm",
  "p_values",
)
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


def is_sha256(value: Any) -> bool:
  return isinstance(value, str) and len(value) == 64 and all(
    character in "0123456789abcdef" for character in value
  )


def read_json(path: Path, errors: list[str]) -> Any | None:
  try:
    return json.loads(path.read_text())
  except (OSError, UnicodeError, json.JSONDecodeError) as error:
    errors.append(f"{path}: invalid JSON ({error})")
    return None


def validate_locked_environment(
    fixture_path: Path,
    payload: dict[str, Any],
    errors: list[str],
) -> None:
  receipt = payload.get("receipt")
  source = payload.get("source")
  if not isinstance(receipt, dict) or not isinstance(source, dict):
    return

  lock = read_json(REFERENCE_LOCK_PATH, errors)
  if not isinstance(lock, dict):
    return
  if lock.get("schema_version") != REFERENCE_LOCK_SCHEMA:
    errors.append(f"{REFERENCE_LOCK_PATH}: schema_version must be {REFERENCE_LOCK_SCHEMA}")
    return

  environment = receipt.get("environment")
  if not isinstance(environment, dict):
    errors.append(f"{fixture_path}: receipt.environment must be an object")
    return
  expected_lock_path = str(REFERENCE_LOCK_PATH.relative_to(REFERENCE_LOCK_PATH.parents[2]))
  expected_lock_hash = hashlib.sha256(REFERENCE_LOCK_PATH.read_bytes()).hexdigest()
  if environment.get("lock_path") != expected_lock_path:
    errors.append(f"{fixture_path}: receipt.environment.lock_path must be {expected_lock_path}")
  if environment.get("lock_sha256") != expected_lock_hash:
    errors.append(f"{fixture_path}: receipt.environment.lock_sha256 is stale")
  if environment.get("locale") != "C":
    errors.append(f"{fixture_path}: receipt.environment.locale must be C")

  producer = source.get("producer")
  repo_root = REFERENCE_LOCK_PATH.parents[2]
  producer_path = repo_root / producer if isinstance(producer, str) else None
  if producer_path is None or not producer_path.is_file():
    errors.append(f"{fixture_path}: source.producer must name an existing generator")
  else:
    expected_generator_hash = hashlib.sha256(producer_path.read_bytes()).hexdigest()
    if environment.get("generator_sha256") != expected_generator_hash:
      errors.append(f"{fixture_path}: receipt.environment.generator_sha256 is stale")

  schema = payload.get("schema_version")
  if isinstance(schema, str) and schema.startswith("scalafim-r-"):
    r_lock = lock.get("r")
    if not isinstance(r_lock, dict):
      errors.append(f"{REFERENCE_LOCK_PATH}: r must be an object")
      return
    if source.get("r_version") != r_lock.get("version"):
      errors.append(f"{fixture_path}: source.r_version disagrees with the environment lock")
    if environment.get("runtime") != f"R {r_lock.get('version')}":
      errors.append(f"{fixture_path}: receipt.environment.runtime disagrees with the R lock")
    packages = r_lock.get("packages")
    if isinstance(packages, dict):
      for package, package_lock in packages.items():
        if not isinstance(package_lock, dict):
          continue
        for suffix in ("revision", "version"):
          key = f"{package}_{suffix}"
          if key in source and source.get(key) != package_lock.get(suffix):
            errors.append(f"{fixture_path}: source.{key} disagrees with the environment lock")
  elif schema == FIXTURE_SCHEMA:
    python_lock = lock.get("python")
    if not isinstance(python_lock, dict):
      errors.append(f"{REFERENCE_LOCK_PATH}: python must be an object")
      return
    if environment.get("runtime") != f"Python {python_lock.get('version')}":
      errors.append(f"{fixture_path}: receipt.environment.runtime disagrees with the Python lock")
    packages = python_lock.get("packages")
    if isinstance(packages, dict):
      expected = {
        "fmrimod_revision": packages.get("fmrimod", {}).get("revision"),
        "numpy_version": packages.get("numpy", {}).get("version"),
        "scipy_version": packages.get("scipy", {}).get("version"),
        "nilearn_version": packages.get("nilearn", {}).get("version"),
      }
      for key, value in expected.items():
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with the environment lock")


def validate_fixture_receipt(
    fixture_path: Path,
    expected_schema: str,
    errors: list[str],
    generated_scala_fixture_path: Path | None = None,
) -> None:
  payload = read_json(fixture_path, errors)
  if not isinstance(payload, dict):
    return

  receipt = payload.get("receipt")
  if isinstance(receipt, dict) and receipt.get("comparison_policy") != COMPARISON_POLICY:
    errors.append(
      f"{fixture_path}: receipt.comparison_policy must forbid post-hoc alignment and global tolerance overrides"
    )

  if expected_schema == FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    receipt = payload.get("receipt")
    source = payload.get("source")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    if not isinstance(receipt.get("accepted_differences"), list):
      errors.append(f"{fixture_path}: receipt.accepted_differences must be a list")

    if not isinstance(source, dict):
      errors.append(f"{fixture_path}: source must be an object")

    receipt_source = receipt.get("source")
    if not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: receipt.source must be an object")
    else:
      for key in RECEIPT_SOURCE_FIELDS:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
      if isinstance(source, dict):
        for key in RECEIPT_SOURCE_FIELDS:
          if source.get(key) != receipt_source.get(key):
            errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in RECEIPT_CONVENTION_FIELDS:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      inputs = payload.get("inputs")
      outputs = payload.get("outputs")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_DESIGN_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    receipt = payload.get("receipt")
    source = payload.get("source")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    if not isinstance(receipt.get("accepted_differences"), list):
      errors.append(f"{fixture_path}: receipt.accepted_differences must be a list")
    if not isinstance(source, dict):
      errors.append(f"{fixture_path}: source must be an object")

    receipt_source = receipt.get("source")
    required_source = (
      "fmridesign_revision",
      "fmrihrf_revision",
      "fmridesign_version",
      "fmrihrf_version",
      "r_version",
    )
    if not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: receipt.source must be an object")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
      if isinstance(source, dict):
        for key in required_source:
          if source.get(key) != receipt_source.get(key):
            errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    required_conventions = (
      "dtype",
      "json_encoding",
      "matrix_orientation",
      "hrf_basis",
      "column_order",
    )
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in required_conventions:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      inputs = payload.get("inputs")
      outputs = payload.get("outputs")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_POLICY_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if payload.get("scenario_id") != "design.modulator-policies.v1":
      errors.append(f"{fixture_path}: scenario_id must be design.modulator-policies.v1")

    receipt = payload.get("receipt")
    source = payload.get("source")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    accepted = receipt.get("accepted_differences")
    if not isinstance(accepted, list) or len(accepted) < 2 or any(not isinstance(value, str) or not value.strip() for value in accepted):
      errors.append(f"{fixture_path}: receipt.accepted_differences must name the R policy differences")

    required_source = ("r_version", "stats_version", "producer", "reference")
    receipt_source = receipt.get("source")
    if not isinstance(source, dict) or not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: source and receipt.source must be objects")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    required_conventions = ("dtype", "json_encoding", "orthogonalization", "grouping", "missing_values")
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in required_conventions:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      inputs = payload.get("inputs")
      outputs = payload.get("outputs")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_KERNEL_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if payload.get("scenario_id") != "design.heterogeneous-hrf.v1":
      errors.append(f"{fixture_path}: scenario_id must be design.heterogeneous-hrf.v1")

    receipt = payload.get("receipt")
    source = payload.get("source")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    accepted = receipt.get("accepted_differences")
    if not isinstance(accepted, list) or len(accepted) < 2 or any(not isinstance(value, str) or not value.strip() for value in accepted):
      errors.append(f"{fixture_path}: receipt.accepted_differences must name the oracle boundaries")

    required_source = ("r_version", "stats_version", "producer", "reference")
    receipt_source = receipt.get("source")
    if not isinstance(source, dict) or not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: source and receipt.source must be objects")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    required_conventions = ("dtype", "json_encoding", "matrix_orientation", "hrf_basis", "column_order")
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in required_conventions:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      inputs = payload.get("inputs")
      outputs = payload.get("outputs")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_WLS_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if payload.get("scenario_id") != "fit.structural-s18-wls.v1":
      errors.append(f"{fixture_path}: scenario_id must be fit.structural-s18-wls.v1")

    receipt = payload.get("receipt")
    source = payload.get("source")
    outputs = payload.get("outputs")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    accepted = receipt.get("accepted_differences")
    if not isinstance(accepted, list) or len(accepted) < 2 or any(not isinstance(value, str) or not value.strip() for value in accepted):
      errors.append(f"{fixture_path}: receipt.accepted_differences must name the WLS oracle boundaries")

    required_source = ("r_version", "stats_version", "producer", "reference")
    receipt_source = receipt.get("source")
    if not isinstance(source, dict) or not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: source and receipt.source must be objects")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    required_conventions = (
      "dtype",
      "json_encoding",
      "matrix_orientation",
      "weighting",
      "residual_variance",
      "dvars",
      "weight_normalization",
    )
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in required_conventions:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    required_outputs = (
      "dvars",
      "dvars_weights",
      "retained_rows_zero_based",
      "coefficients",
      "residual_variance",
      "normalized_covariance",
      "standard_errors",
      "residual_df",
      "rank",
      "contrast_estimate",
      "contrast_standard_error",
      "contrast_statistic",
      "weighted_design",
      "weighted_response",
    )
    if not isinstance(outputs, dict):
      errors.append(f"{fixture_path}: outputs must be an object")
    else:
      for key in required_outputs:
        if key not in outputs:
          errors.append(f"{fixture_path}: outputs.{key} is required")
      if not isinstance(outputs.get("residual_df"), int) or outputs.get("residual_df", 0) <= 0:
        errors.append(f"{fixture_path}: outputs.residual_df must be a positive integer")
      if not isinstance(outputs.get("rank"), int) or outputs.get("rank", 0) <= 0:
        errors.append(f"{fixture_path}: outputs.rank must be a positive integer")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      inputs = payload.get("inputs")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_FIXED_EFFECTS_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if payload.get("scenario_id") != "fit.mixed-tr-fixed-effects.v1":
      errors.append(f"{fixture_path}: scenario_id must be fit.mixed-tr-fixed-effects.v1")

    receipt = payload.get("receipt")
    source = payload.get("source")
    outputs = payload.get("outputs")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    accepted = receipt.get("accepted_differences")
    if not isinstance(accepted, list) or len(accepted) < 3 or any(not isinstance(value, str) or not value.strip() for value in accepted):
      errors.append(f"{fixture_path}: receipt.accepted_differences must name the fixed-effects and interpolation boundaries")

    required_source = (
      "fmridesign_revision",
      "fmridesign_version",
      "fmrihrf_revision",
      "fmrihrf_version",
      "r_version",
      "stats_version",
      "producer",
      "reference",
    )
    receipt_source = receipt.get("source")
    if not isinstance(source, dict) or not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: source and receipt.source must be objects")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    required_conventions = (
      "dtype",
      "json_encoding",
      "matrix_orientation",
      "sampling",
      "runwise_fit",
      "fixed_effects",
      "residual_df",
    )
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in required_conventions:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    required_outputs = (
      "run_coefficients",
      "run_residual_variance",
      "run_normalized_covariance",
      "run_residual_df",
      "fixed_coefficients",
      "fixed_covariance_by_voxel",
      "fixed_standard_errors",
      "fixed_residual_df",
      "contrast_estimate",
      "contrast_standard_error",
      "contrast_statistic",
    )
    if not isinstance(outputs, dict):
      errors.append(f"{fixture_path}: outputs must be an object")
    else:
      for key in required_outputs:
        if key not in outputs:
          errors.append(f"{fixture_path}: outputs.{key} is required")
      if outputs.get("run_residual_df") != [5, 9]:
        errors.append(f"{fixture_path}: outputs.run_residual_df must preserve the 5/9 mixed-run receipt")
      if outputs.get("fixed_residual_df") != 14:
        errors.append(f"{fixture_path}: outputs.fixed_residual_df must equal 14")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      inputs = payload.get("inputs")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_DMS_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if payload.get("scenario_id") != "fit.dms-multiphase-dsl.v1":
      errors.append(f"{fixture_path}: scenario_id must be fit.dms-multiphase-dsl.v1")

    receipt = payload.get("receipt")
    source = payload.get("source")
    inputs = payload.get("inputs")
    outputs = payload.get("outputs")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    accepted = receipt.get("accepted_differences")
    if not isinstance(accepted, list) or len(accepted) < 4 or any(not isinstance(value, str) or not value.strip() for value in accepted):
      errors.append(f"{fixture_path}: receipt.accepted_differences must name the DMS cross-system boundaries")

    required_source = (
      "fmridesign_revision",
      "fmrihrf_revision",
      "fmridesign_version",
      "fmrihrf_version",
      "r_version",
      "stats_version",
      "producer",
      "reference",
    )
    receipt_source = receipt.get("source")
    if not isinstance(source, dict) or not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: source and receipt.source must be objects")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    required_conventions = (
      "dtype",
      "json_encoding",
      "matrix_orientation",
      "convolution",
      "runwise_fit",
      "fixed_effects",
      "response",
      "response_functionals",
    )
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in required_conventions:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    required_inputs = (
      "formula",
      "trials",
      "sampling_frame",
      "precision",
      "planted_coefficients",
      "run_intercepts",
      "noise",
    )
    if not isinstance(inputs, dict):
      errors.append(f"{fixture_path}: inputs must be an object")
    else:
      for key in required_inputs:
        if key not in inputs:
          errors.append(f"{fixture_path}: inputs.{key} is required")
      trials = inputs.get("trials")
      trial_fields = (
        "trial_id",
        "run",
        "sample_onset",
        "sample_duration",
        "delay_onset",
        "delay_duration",
        "probe_onset",
        "probe_duration",
        "stimulus",
        "load",
        "match",
        "rt",
      )
      if not isinstance(trials, dict):
        errors.append(f"{fixture_path}: inputs.trials must be an object")
      else:
        for key in trial_fields:
          values = trials.get(key)
          if not isinstance(values, list) or len(values) != 60:
            errors.append(f"{fixture_path}: inputs.trials.{key} must contain 60 values")

    required_outputs = (
      "design_column_keys",
      "estimable_design",
      "response",
      "run_rank",
      "run_residual_df",
      "run_coefficients",
      "run_covariance",
      "fixed_coefficients",
      "fixed_covariance",
      "fixed_residual_df",
      "t_hypotheses",
      "f_hypotheses",
    )
    if not isinstance(outputs, dict):
      errors.append(f"{fixture_path}: outputs must be an object")
    else:
      for key in required_outputs:
        if key not in outputs:
          errors.append(f"{fixture_path}: outputs.{key} is required")
      if not isinstance(outputs.get("design_column_keys"), list) or len(outputs["design_column_keys"]) != 46:
        errors.append(f"{fixture_path}: outputs.design_column_keys must contain the 46 estimable task coordinates")
      design = outputs.get("estimable_design")
      if not isinstance(design, list) or len(design) != 360 or any(not isinstance(row, list) or len(row) != 46 for row in design):
        errors.append(f"{fixture_path}: outputs.estimable_design must be a 360 by 46 matrix")
      if not isinstance(outputs.get("response"), list) or len(outputs["response"]) != 360:
        errors.append(f"{fixture_path}: outputs.response must contain 360 scans")
      if outputs.get("run_rank") != [47, 47]:
        errors.append(f"{fixture_path}: outputs.run_rank must equal [47, 47]")
      if outputs.get("run_residual_df") != [133, 133]:
        errors.append(f"{fixture_path}: outputs.run_residual_df must equal [133, 133]")
      if outputs.get("fixed_residual_df") != 266:
        errors.append(f"{fixture_path}: outputs.fixed_residual_df must equal 266")
      expected_t = {
        "sample-face-minus-scene",
        "delay-high-minus-low",
        "probe-mismatch-at-six",
        "probe-match-by-load",
        "probe-rt-slope",
      }
      t_hypotheses = outputs.get("t_hypotheses")
      if not isinstance(t_hypotheses, dict) or set(t_hypotheses) != expected_t:
        errors.append(f"{fixture_path}: outputs.t_hypotheses must contain the five semantic DMS hypotheses")
      expected_f = {"probe-shape", "delay-high-omnibus"}
      f_hypotheses = outputs.get("f_hypotheses")
      if not isinstance(f_hypotheses, dict) or set(f_hypotheses) != expected_f:
        errors.append(f"{fixture_path}: outputs.f_hypotheses must contain the two semantic DMS omnibus hypotheses")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_REALISTIC_NUISANCE_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if payload.get("scenario_id") != "fit.realistic-nuisance.v1":
      errors.append(f"{fixture_path}: scenario_id must be fit.realistic-nuisance.v1")

    receipt = payload.get("receipt")
    source = payload.get("source")
    inputs = payload.get("inputs")
    outputs = payload.get("outputs")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    accepted = receipt.get("accepted_differences")
    if not isinstance(accepted, list) or len(accepted) < 3 or any(not isinstance(value, str) or not value.strip() for value in accepted):
      errors.append(f"{fixture_path}: receipt.accepted_differences must name the S12/S15 evidence boundaries")

    required_source = (
      "fmrihrf_revision",
      "fmrihrf_version",
      "r_version",
      "stats_version",
      "producer",
      "reference",
    )
    receipt_source = receipt.get("source")
    if not isinstance(source, dict) or not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: source and receipt.source must be objects")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    required_conventions = (
      "dtype",
      "json_encoding",
      "matrix_orientation",
      "task_design",
      "nuisance",
      "baseline",
      "fit",
    )
    if not isinstance(conventions, dict):
      errors.append(f"{fixture_path}: receipt.conventions must be an object")
    else:
      for key in required_conventions:
        value = conventions.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    required_inputs = (
      "events",
      "sampling_frame",
      "precision",
      "nuisance_runs",
      "nuisance_policy",
      "planted_coefficients",
      "noise",
    )
    if not isinstance(inputs, dict):
      errors.append(f"{fixture_path}: inputs must be an object")
    else:
      for key in required_inputs:
        if key not in inputs:
          errors.append(f"{fixture_path}: inputs.{key} is required")
      events = inputs.get("events")
      if not isinstance(events, dict):
        errors.append(f"{fixture_path}: inputs.events must be an object")
      else:
        for key in ("trial_id", "run", "onset", "condition"):
          values = events.get(key)
          if not isinstance(values, list) or len(values) != 16:
            errors.append(f"{fixture_path}: inputs.events.{key} must contain 16 values")
        if events.get("declared_condition_levels") != ["A", "B", "C"]:
          errors.append(f"{fixture_path}: inputs.events.declared_condition_levels must equal [A, B, C]")

      nuisance_runs = inputs.get("nuisance_runs")
      if not isinstance(nuisance_runs, list) or len(nuisance_runs) != 2:
        errors.append(f"{fixture_path}: inputs.nuisance_runs must contain two runs")
      else:
        for run, nuisance in enumerate(nuisance_runs, start=1):
          if not isinstance(nuisance, dict):
            errors.append(f"{fixture_path}: inputs.nuisance_runs[{run}] must be an object")
            continue
          names = nuisance.get("names")
          matrix = nuisance.get("matrix")
          if not isinstance(names, list) or len(names) != 22:
            errors.append(f"{fixture_path}: nuisance run {run} must name 22 sampled columns")
          if not isinstance(matrix, list) or len(matrix) != 80 or any(not isinstance(row, list) or len(row) != 22 for row in matrix):
            errors.append(f"{fixture_path}: nuisance run {run} matrix must be 80 by 22")

      policy = inputs.get("nuisance_policy")
      if not isinstance(policy, dict):
        errors.append(f"{fixture_path}: inputs.nuisance_policy must be an object")
      else:
        if policy.get("duplicate_report_threshold") != 0.999:
          errors.append(f"{fixture_path}: nuisance duplicate report threshold must equal 0.999")
        if not isinstance(policy.get("check"), str) or "rank" not in policy["check"]:
          errors.append(f"{fixture_path}: nuisance policy must declare rank-based dropping")

    required_outputs = (
      "task_design",
      "response",
      "retained_nuisance",
      "dropped_nuisance",
      "design_rank",
      "residual_df",
      "task_coefficients",
      "task_covariance",
      "t_hypotheses",
      "f_hypotheses",
    )
    if not isinstance(outputs, dict):
      errors.append(f"{fixture_path}: outputs must be an object")
    else:
      for key in required_outputs:
        if key not in outputs:
          errors.append(f"{fixture_path}: outputs.{key} is required")
      design = outputs.get("task_design")
      if not isinstance(design, list) or len(design) != 160 or any(not isinstance(row, list) or len(row) != 2 for row in design):
        errors.append(f"{fixture_path}: outputs.task_design must be a 160 by 2 matrix")
      if not isinstance(outputs.get("response"), list) or len(outputs["response"]) != 160:
        errors.append(f"{fixture_path}: outputs.response must contain 160 scans")
      if outputs.get("design_rank") != 48:
        errors.append(f"{fixture_path}: outputs.design_rank must equal 48")
      if outputs.get("residual_df") != 112:
        errors.append(f"{fixture_path}: outputs.residual_df must equal 112")
      retained = outputs.get("retained_nuisance")
      if not isinstance(retained, list) or len(retained) != 2 or any(not isinstance(run, list) or len(run) != 20 for run in retained):
        errors.append(f"{fixture_path}: outputs.retained_nuisance must contain 20 names per run")
      if not isinstance(retained, list) or any("rot_z_near" not in run for run in retained if isinstance(run, list)):
        errors.append(f"{fixture_path}: outputs.retained_nuisance must retain rot_z_near in both runs")
      dropped = outputs.get("dropped_nuisance")
      if dropped != [["constant_conf", "trans_x_dup"], ["constant_conf", "trans_x_dup"]]:
        errors.append(f"{fixture_path}: outputs.dropped_nuisance must record constant_conf and trans_x_dup per run")
      t_hypotheses = outputs.get("t_hypotheses")
      if not isinstance(t_hypotheses, dict) or set(t_hypotheses) != {"task-a-minus-b"}:
        errors.append(f"{fixture_path}: outputs.t_hypotheses must contain task-a-minus-b")
      f_hypotheses = outputs.get("f_hypotheses")
      if not isinstance(f_hypotheses, dict) or set(f_hypotheses) != {"task-omnibus"}:
        errors.append(f"{fixture_path}: outputs.f_hypotheses must contain task-omnibus")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == R_AR_CENSOR_GLS_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if payload.get("scenario_id") != "fit.ar-censor-boundary-gls.v1":
      errors.append(f"{fixture_path}: scenario_id must be fit.ar-censor-boundary-gls.v1")

    receipt = payload.get("receipt")
    source = payload.get("source")
    inputs = payload.get("inputs")
    outputs = payload.get("outputs")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
      return
    if receipt.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
    if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
      errors.append(f"{fixture_path}: receipt.producer_command is required")
    accepted = receipt.get("accepted_differences")
    if not isinstance(accepted, list) or len(accepted) < 2 or any(not isinstance(value, str) or not value.strip() for value in accepted):
      errors.append(f"{fixture_path}: receipt.accepted_differences must name fixed-rho and censor-boundary policy differences")

    required_source = (
      "fmrireg_revision",
      "fmrireg_version",
      "r_version",
      "stats_version",
      "producer",
      "reference",
    )
    receipt_source = receipt.get("source")
    if not isinstance(source, dict) or not isinstance(receipt_source, dict):
      errors.append(f"{fixture_path}: source and receipt.source must be objects")
    else:
      for key in required_source:
        value = receipt_source.get(key)
        if not isinstance(value, str) or not value.strip():
          errors.append(f"{fixture_path}: receipt.source.{key} is required")
        if source.get(key) != value:
          errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")

    conventions = receipt.get("conventions")
    for key in ("dtype", "matrix_orientation", "censoring", "whitening", "fit"):
      if not isinstance(conventions, dict) or not isinstance(conventions.get(key), str) or not conventions[key].strip():
        errors.append(f"{fixture_path}: receipt.conventions.{key} is required")

    required_inputs = (
      "task",
      "full_response",
      "sampling_frame",
      "selected_timepoints",
      "censored_timepoints",
      "boundary_canary_timepoints",
      "rho",
      "exact_first",
      "coefficient_names",
    )
    if not isinstance(inputs, dict):
      errors.append(f"{fixture_path}: inputs must be an object")
    else:
      for key in required_inputs:
        if key not in inputs:
          errors.append(f"{fixture_path}: inputs.{key} is required")
      task = inputs.get("task")
      response = inputs.get("full_response")
      selected = inputs.get("selected_timepoints")
      censored = inputs.get("censored_timepoints")
      canaries = inputs.get("boundary_canary_timepoints")
      sampling = inputs.get("sampling_frame")
      if not isinstance(task, list) or len(task) != 24:
        errors.append(f"{fixture_path}: inputs.task must contain 24 scans")
      if not isinstance(response, list) or len(response) != 24 or any(not isinstance(row, list) or len(row) != 4 for row in response):
        errors.append(f"{fixture_path}: inputs.full_response must be a 24 by 4 matrix")
      if not isinstance(selected, list) or selected != sorted(selected):
        errors.append(f"{fixture_path}: inputs.selected_timepoints must be ordered")
      if not isinstance(selected, list) or not isinstance(censored, list) or sorted(selected + censored) != list(range(24)) or set(selected).intersection(censored):
        errors.append(f"{fixture_path}: selected and censored timepoints must partition the 24-scan source axis")
      if not isinstance(canaries, list) or not isinstance(selected, list) or not set(canaries).issubset(selected):
        errors.append(f"{fixture_path}: boundary canaries must be selected source timepoints")
      if not isinstance(sampling, dict) or sampling.get("blocklens") != [12, 12]:
        errors.append(f"{fixture_path}: inputs.sampling_frame.blocklens must equal [12, 12]")
      if not isinstance(inputs.get("rho"), (int, float)) or not -1.0 < inputs["rho"] < 1.0:
        errors.append(f"{fixture_path}: inputs.rho must be a stationary AR(1) coefficient")
      if inputs.get("exact_first") is not True:
        errors.append(f"{fixture_path}: inputs.exact_first must be true")
      if inputs.get("coefficient_names") != ["task", "base_constant1_block_1", "base_constant1_block_2"]:
        errors.append(f"{fixture_path}: inputs.coefficient_names must name task and two run intercepts")

    required_outputs = (
      "selected_design",
      "selected_response",
      "whitened_design",
      "whitened_response",
      "segments",
      "censor_gaps",
      "coefficients",
      "normalized_covariance",
      "residual_variance",
      "standard_errors",
      "rank",
      "residual_df",
      "t_task",
      "f_task",
    )
    if not isinstance(outputs, dict):
      errors.append(f"{fixture_path}: outputs must be an object")
    else:
      for key in required_outputs:
        if key not in outputs:
          errors.append(f"{fixture_path}: outputs.{key} is required")
      matrix_shapes = {
        "selected_design": (18, 3),
        "selected_response": (18, 4),
        "whitened_design": (18, 3),
        "whitened_response": (18, 4),
        "coefficients": (3, 4),
        "normalized_covariance": (3, 3),
        "standard_errors": (3, 4),
      }
      for key, (rows, columns) in matrix_shapes.items():
        value = outputs.get(key)
        if not isinstance(value, list) or len(value) != rows or any(not isinstance(row, list) or len(row) != columns for row in value):
          errors.append(f"{fixture_path}: outputs.{key} must be a {rows} by {columns} matrix")
      if not isinstance(outputs.get("residual_variance"), list) or len(outputs["residual_variance"]) != 4:
        errors.append(f"{fixture_path}: outputs.residual_variance must contain four voxel values")
      if outputs.get("rank") != 3 or outputs.get("residual_df") != 15:
        errors.append(f"{fixture_path}: outputs rank/residual_df must equal 3/15")
      segments = outputs.get("segments")
      if not isinstance(segments, list) or len(segments) != 6 or any(not isinstance(segment, dict) for segment in segments):
        errors.append(f"{fixture_path}: outputs.segments must contain six typed whitening segments")
      gaps = outputs.get("censor_gaps")
      if not isinstance(gaps, list) or len(gaps) != 4 or any(not isinstance(gap, dict) for gap in gaps):
        errors.append(f"{fixture_path}: outputs.censor_gaps must contain four source-axis gaps")
      task_t = outputs.get("t_task")
      if not isinstance(task_t, dict) or any(not isinstance(task_t.get(key), list) or len(task_t[key]) != 4 for key in ("estimates", "standard_errors", "statistics")):
        errors.append(f"{fixture_path}: outputs.t_task must contain four estimates, standard errors, and statistics")
      task_f = outputs.get("f_task")
      if not isinstance(task_f, dict) or task_f.get("numerator_df") != 1 or task_f.get("denominator_df") != 15 or not isinstance(task_f.get("statistics"), list) or len(task_f["statistics"]) != 4:
        errors.append(f"{fixture_path}: outputs.f_task must contain a one-row, four-voxel F receipt with 15 denominator df")

    hashes = receipt.get("hashes")
    if not isinstance(hashes, dict):
      errors.append(f"{fixture_path}: receipt.hashes must be an object")
    else:
      for key in ("inputs_sha256", "outputs_sha256"):
        if not is_sha256(hashes.get(key)):
          errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
      if isinstance(inputs, dict) and isinstance(outputs, dict):
        expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
        expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
        if hashes.get("inputs_sha256") != expected_inputs:
          errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
        if hashes.get("outputs_sha256") != expected_outputs:
          errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
  elif expected_schema == LOCAL_AXIS_FIXTURE_SCHEMA:
    if payload.get("schema") != expected_schema:
      errors.append(f"{fixture_path}: schema must be {expected_schema}")
    if not isinstance(payload.get("scenario_id"), str) or not payload["scenario_id"].strip():
      errors.append(f"{fixture_path}: scenario_id is required")

    oracle = payload.get("oracle")
    if not isinstance(oracle, dict):
      errors.append(f"{fixture_path}: oracle must be an object")
    else:
      for key in ("kind", "implementation", "comparison"):
        if not isinstance(oracle.get(key), str) or not oracle[key].strip():
          errors.append(f"{fixture_path}: oracle.{key} is required")
      source_columns = oracle.get("source_columns_by_run")
      if not isinstance(source_columns, dict) or not source_columns:
        errors.append(f"{fixture_path}: oracle.source_columns_by_run must be a non-empty object")
      else:
        for run, columns in source_columns.items():
          if not isinstance(run, str) or not run.strip():
            errors.append(f"{fixture_path}: oracle.source_columns_by_run has an invalid run key")
          if not isinstance(columns, list) or not columns or any(not isinstance(column, int) or column < 0 for column in columns):
            errors.append(f"{fixture_path}: oracle.source_columns_by_run[{run!r}] must contain non-negative integer indices")

    policy = payload.get("policy_receipt")
    if not isinstance(policy, dict):
      errors.append(f"{fixture_path}: policy_receipt must be an object")
    else:
      empty_cell_policies = policy.get("empty_cell_policies")
      if (
        not isinstance(empty_cell_policies, list)
        or not empty_cell_policies
        or any(not isinstance(value, str) or not value.strip() for value in empty_cell_policies)
      ):
        errors.append(f"{fixture_path}: policy_receipt.empty_cell_policies must be a non-empty string list")
      if not isinstance(policy.get("shared_run_policy"), str) or not policy["shared_run_policy"].strip():
        errors.append(f"{fixture_path}: policy_receipt.shared_run_policy is required")
      for key in ("selected_timepoints", "censored_timepoints"):
        values = policy.get(key)
        if not isinstance(values, list) or any(not isinstance(value, int) or value < 0 for value in values):
          errors.append(f"{fixture_path}: policy_receipt.{key} must contain non-negative integer indices")
      unsupported = policy.get("unsupported_in_this_fixture")
      if not isinstance(unsupported, list) or any(not isinstance(value, str) or not value.strip() for value in unsupported):
        errors.append(f"{fixture_path}: policy_receipt.unsupported_in_this_fixture must be a list of non-empty strings")

    if not isinstance(payload.get("truth_boundary"), str) or not payload["truth_boundary"].strip():
      errors.append(f"{fixture_path}: truth_boundary is required")

  if expected_schema.startswith("scalafim-r-") or expected_schema == FIXTURE_SCHEMA:
    truth_boundary = payload.get("truth_boundary")
    if truth_boundary not in ALLOWED_EVIDENCE_SCOPES:
      errors.append(
        f"{fixture_path}: truth_boundary must be one of {sorted(ALLOWED_EVIDENCE_SCOPES)}"
      )
    validate_locked_environment(fixture_path, payload, errors)

  if (
      (expected_schema.startswith("scalafim-r-") or expected_schema == FIXTURE_SCHEMA)
      and generated_scala_fixture_path is not None
  ):
    receipt = payload.get("receipt")
    hashes = receipt.get("hashes") if isinstance(receipt, dict) else None
    generated_hash = hashes.get("generated_scala_sha256") if isinstance(hashes, dict) else None
    if not is_sha256(generated_hash):
      errors.append(f"{fixture_path}: receipt.hashes.generated_scala_sha256 must be a SHA-256 digest")
    elif generated_scala_fixture_path.is_file():
      expected_generated_hash = hashlib.sha256(generated_scala_fixture_path.read_bytes()).hexdigest()
      if generated_hash != expected_generated_hash:
        errors.append(f"{fixture_path}: generated Scala fixture hash is stale")
  elif expected_schema == SLOW_ORACLE_FIXTURE_SCHEMA:
    if payload.get("schema_version") != expected_schema:
      errors.append(f"{fixture_path}: schema_version must be {expected_schema}")
    if not isinstance(payload.get("scenario_id"), str) or not payload["scenario_id"].strip():
      errors.append(f"{fixture_path}: scenario_id is required")
    for key in ("inputs", "outputs"):
      if not isinstance(payload.get(key), dict):
        errors.append(f"{fixture_path}: {key} must be an object")

    source = payload.get("source")
    receipt = payload.get("receipt")
    if not isinstance(source, dict):
      errors.append(f"{fixture_path}: source must be an object")
    else:
      for key in ("generator", "language", "version"):
        if not isinstance(source.get(key), str) or not source[key].strip():
          errors.append(f"{fixture_path}: source.{key} is required")
    if not isinstance(receipt, dict):
      errors.append(f"{fixture_path}: receipt must be an object")
    else:
      if receipt.get("schema_version") != expected_schema:
        errors.append(f"{fixture_path}: receipt.schema_version must match {expected_schema}")
      if not isinstance(receipt.get("producer_command"), str) or not receipt["producer_command"].strip():
        errors.append(f"{fixture_path}: receipt.producer_command is required")
      if not isinstance(receipt.get("accepted_differences"), list):
        errors.append(f"{fixture_path}: receipt.accepted_differences must be a list")
      receipt_source = receipt.get("source")
      if not isinstance(receipt_source, dict):
        errors.append(f"{fixture_path}: receipt.source must be an object")
      elif isinstance(source, dict):
        for key in ("generator", "language", "version"):
          if receipt_source.get(key) != source.get(key):
            errors.append(f"{fixture_path}: source.{key} disagrees with receipt.source.{key}")
      conventions = receipt.get("conventions")
      if not isinstance(conventions, dict):
        errors.append(f"{fixture_path}: receipt.conventions must be an object")
      else:
        for key in ("dtype", "matrix_orientation", "run_time_reference", "event_integration"):
          if not isinstance(conventions.get(key), str) or not conventions[key].strip():
            errors.append(f"{fixture_path}: receipt.conventions.{key} is required")
      hashes = receipt.get("hashes")
      if not isinstance(hashes, dict):
        errors.append(f"{fixture_path}: receipt.hashes must be an object")
      else:
        for key in ("inputs_sha256", "outputs_sha256"):
          if not is_sha256(hashes.get(key)):
            errors.append(f"{fixture_path}: receipt.hashes.{key} must be a SHA-256 digest")
        inputs = payload.get("inputs")
        outputs = payload.get("outputs")
        if isinstance(inputs, dict) and isinstance(outputs, dict):
          expected_inputs = canonical_sha256({"schema_version": payload.get("schema_version"), "inputs": inputs})
          expected_outputs = canonical_sha256({"schema_version": payload.get("schema_version"), "outputs": outputs})
          if hashes.get("inputs_sha256") != expected_inputs:
            errors.append(f"{fixture_path}: receipt.hashes.inputs_sha256 is stale")
          if hashes.get("outputs_sha256") != expected_outputs:
            errors.append(f"{fixture_path}: receipt.hashes.outputs_sha256 is stale")
    if not isinstance(payload.get("truth_boundary"), str) or not payload["truth_boundary"].strip():
      errors.append(f"{fixture_path}: truth_boundary is required")


def validate_manifest(manifest_path: Path) -> list[str]:
  errors: list[str] = []
  manifest = read_json(manifest_path, errors)
  if not isinstance(manifest, dict):
    return errors
  if manifest.get("schema_version") != MANIFEST_SCHEMA:
    errors.append(f"{manifest_path}: schema_version must be {MANIFEST_SCHEMA}")

  scenarios = manifest.get("scenarios")
  if not isinstance(scenarios, list) or not scenarios:
    errors.append(f"{manifest_path}: scenarios must be a non-empty list")
    return errors

  ids: list[str] = []
  declared_metrics: set[str] = set()
  repo_root = Path(__file__).resolve().parents[2]
  for index, scenario in enumerate(scenarios):
    prefix = f"scenario[{index}]"
    if not isinstance(scenario, dict):
      errors.append(f"{prefix}: entry must be an object")
      continue

    scenario_id = scenario.get("id")
    if not isinstance(scenario_id, str) or not scenario_id.strip():
      errors.append(f"{prefix}: id must be a non-empty string")
      continue
    ids.append(scenario_id)
    prefix = scenario_id

    suite_path = scenario.get("suite_path")
    if not isinstance(suite_path, str) or not suite_path.strip():
      errors.append(f"{prefix}: suite_path is required")
    elif not (repo_root / suite_path).is_file():
      errors.append(f"{prefix}: suite_path does not exist: {suite_path}")

    platforms = scenario.get("platforms")
    if not isinstance(platforms, list) or not platforms or any(platform not in ALLOWED_PLATFORMS for platform in platforms):
      errors.append(f"{prefix}: platforms must be a non-empty subset of jvm/js")

    statuses = scenario.get("allowed_statuses")
    if not isinstance(statuses, list) or not statuses or any(status not in ALLOWED_STATUSES for status in statuses):
      errors.append(f"{prefix}: allowed_statuses contains an undeclared status")

    commands = scenario.get("acceptance_commands")
    if not isinstance(commands, list) or not commands or any(not isinstance(command, str) or not command.strip() for command in commands):
      errors.append(f"{prefix}: acceptance_commands must contain a non-empty command")

    reference = scenario.get("reference")
    if not isinstance(reference, dict) or not isinstance(reference.get("kind"), str) or not reference["kind"].strip():
      errors.append(f"{prefix}: reference.kind is required")
      continue

    exporter_path = reference.get("exporter_path")
    finalizer_path = reference.get("finalizer_path")
    fixture_path = reference.get("fixture_path")
    scala_path = reference.get("generated_scala_fixture_path")
    receipt_schema = reference.get("receipt_schema")
    evidence_scope = reference.get("evidence_scope")
    comparison_metrics = reference.get("comparison_metrics")
    is_local_receipt = receipt_schema == LOCAL_AXIS_FIXTURE_SCHEMA
    is_slow_receipt = receipt_schema == SLOW_ORACLE_FIXTURE_SCHEMA
    external_paths = (exporter_path, fixture_path, scala_path)
    if is_local_receipt or is_slow_receipt:
      if not isinstance(fixture_path, str) or not fixture_path.strip():
        errors.append(f"{prefix}: checked-in receipt requires reference.fixture_path")
      if exporter_path is not None or scala_path is not None:
        errors.append(f"{prefix}: checked-in receipt must not declare exporter_path or generated_scala_fixture_path")
    elif any(path is not None for path in external_paths) and not all(isinstance(path, str) and path.strip() for path in external_paths):
      errors.append(f"{prefix}: exporter_path, fixture_path, and generated_scala_fixture_path must be declared together")
    for label, path in (
        ("exporter_path", exporter_path),
        ("finalizer_path", finalizer_path),
        ("fixture_path", fixture_path),
        ("generated_scala_fixture_path", scala_path),
    ):
      if path is not None and isinstance(path, str) and not (repo_root / path).is_file():
        errors.append(f"{prefix}: reference.{label} does not exist: {path}")

    if isinstance(receipt_schema, str) and receipt_schema.startswith("scalafim-r-"):
      if not isinstance(finalizer_path, str) or not finalizer_path.strip():
        errors.append(f"{prefix}: generated R receipt requires reference.finalizer_path")

    if receipt_schema is not None and (not isinstance(receipt_schema, str) or not receipt_schema.strip()):
      errors.append(f"{prefix}: reference.receipt_schema must be a non-empty string")
    elif isinstance(receipt_schema, str) and receipt_schema not in {FIXTURE_SCHEMA, R_DESIGN_FIXTURE_SCHEMA, R_POLICY_FIXTURE_SCHEMA, R_KERNEL_FIXTURE_SCHEMA, R_WLS_FIXTURE_SCHEMA, R_FIXED_EFFECTS_FIXTURE_SCHEMA, R_DMS_FIXTURE_SCHEMA, R_REALISTIC_NUISANCE_FIXTURE_SCHEMA, R_AR_CENSOR_GLS_FIXTURE_SCHEMA, LOCAL_AXIS_FIXTURE_SCHEMA, SLOW_ORACLE_FIXTURE_SCHEMA}:
      errors.append(f"{prefix}: unsupported reference.receipt_schema: {receipt_schema}")
    if fixture_path is not None and isinstance(fixture_path, str):
      if evidence_scope not in ALLOWED_EVIDENCE_SCOPES:
        errors.append(
          f"{prefix}: fixture-backed reference.evidence_scope must be one of {sorted(ALLOWED_EVIDENCE_SCOPES)}"
        )
      if (
          not isinstance(comparison_metrics, list)
          or not comparison_metrics
          or any(metric not in ALLOWED_COMPARISON_METRICS for metric in comparison_metrics)
          or len(comparison_metrics) != len(set(comparison_metrics))
      ):
        errors.append(
          f"{prefix}: fixture-backed reference.comparison_metrics must be a unique non-empty subset of {sorted(ALLOWED_COMPARISON_METRICS)}"
        )
      else:
        metrics = set(comparison_metrics)
        declared_metrics.update(metrics)
        required = REQUIRED_METRICS_BY_SCOPE.get(evidence_scope, set())
        missing = required - metrics
        if missing:
          errors.append(f"{prefix}: comparison_metrics is missing {sorted(missing)} for {evidence_scope}")
      if receipt_schema is None:
        errors.append(f"{prefix}: fixture_path requires reference.receipt_schema")
      elif isinstance(receipt_schema, str):
        candidate = repo_root / fixture_path
        if candidate.is_file():
          generated_candidate = repo_root / scala_path if isinstance(scala_path, str) else None
          validate_fixture_receipt(candidate, receipt_schema, errors, generated_candidate)
    elif receipt_schema is not None:
      errors.append(f"{prefix}: reference.receipt_schema requires fixture_path")
    elif evidence_scope is not None:
      errors.append(f"{prefix}: reference.evidence_scope requires fixture_path")
    elif comparison_metrics is not None:
      errors.append(f"{prefix}: reference.comparison_metrics requires fixture_path")

  if len(ids) != len(set(ids)):
    errors.append(f"{manifest_path}: scenario ids must be unique")
  for metric in ("peak_timing", "integral", "log_p"):
    if metric not in declared_metrics:
      errors.append(f"{manifest_path}: fixture comparisons must include {metric}")
  return errors


def main() -> int:
  path = Path(sys.argv[1]) if len(sys.argv) == 2 else Path("docs/scenarios/manifest.json")
  errors = validate_manifest(path)
  if errors:
    print("invalid scenario manifest:")
    for error in errors:
      print(f"  {error}")
    return 1
  manifest = json.loads(path.read_text())
  print(f"validated {len(manifest['scenarios'])} scenario manifest entries")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
