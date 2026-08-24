#!/usr/bin/env python3
"""Export the Nilearn reference fixture for fit.public-f-contrast.v1.

The generated Scala object is checked into shared test sources so both the JVM
and Scala.js suites can compare against the same external reference without
runtime filesystem access.
"""

from __future__ import annotations

import argparse
import hashlib
import importlib.metadata
import json
from pathlib import Path
import subprocess
import sys
from typing import Any


SCENARIO_ID = "fit.public-f-contrast.v1"
COLUMN_NAMES = ("task", "base_constant", "nuis#01_1")
LOCK_SCHEMA = "scalafim-parity-environment-lock/v1"
COMPARISON_POLICY = {
  "alignment": "identity_only_no_post_hoc_lag_shift",
  "correlation_role": "diagnostic_only_requires_error_and_norm_bounds",
  "disagreement_triage": "truth_boundary_local",
  "tolerance_scope": "comparison_local_no_global_override",
}

TASK = (-1.5, -1.0, -0.25, 0.75, 1.25, -0.5, 0.5, 1.75)
MOTION = (0.2, -0.4, 0.7, -0.6, 0.1, 0.9, -0.8, 0.3)
NOISE0 = (0.05, -0.02, 0.03, -0.04, 0.01, -0.03, 0.02, -0.02)
NOISE1 = (-0.01, 0.04, -0.02, 0.03, -0.05, 0.02, -0.03, 0.01)


def default_repo_root() -> Path:
  return Path(__file__).resolve().parents[2]


def default_fmrimod_root() -> Path:
  return Path.home() / "code" / "pycode" / "fmrimod"


def package_version(name: str) -> str:
  try:
    return importlib.metadata.version(name)
  except importlib.metadata.PackageNotFoundError:
    return "uninstalled"


def source_revision(path: Path) -> str:
  try:
    result = subprocess.run(
      ["git", "-C", str(path), "rev-parse", "HEAD"],
      check=True,
      capture_output=True,
      text=True,
    )
  except (OSError, subprocess.CalledProcessError):
    return "unavailable"
  return result.stdout.strip() or "unavailable"


def sha256_file(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def sha256_json(value: Any) -> str:
  encoded = json.dumps(
    value,
    ensure_ascii=False,
    allow_nan=False,
    sort_keys=True,
    separators=(",", ":"),
  ).encode("utf-8")
  return hashlib.sha256(encoded).hexdigest()


def environment_lock() -> tuple[Path, dict[str, Any]]:
  path = default_repo_root() / "tools" / "r-parity" / "reference-lock.json"
  payload = json.loads(path.read_text())
  if payload.get("schema_version") != LOCK_SCHEMA:
    raise SystemExit(f"unexpected environment lock schema: {payload.get('schema_version')!r}")
  return path, payload


def locked_environment(fmrimod_root: Path) -> dict[str, str]:
  path, lock = environment_lock()
  python_lock = lock.get("python")
  if not isinstance(python_lock, dict):
    raise SystemExit("environment lock must contain a Python section")
  packages = python_lock.get("packages")
  if not isinstance(packages, dict):
    raise SystemExit("Python environment lock packages must be an object")

  actual_python = f"{sys.version_info.major}.{sys.version_info.minor}"
  if actual_python != python_lock.get("version"):
    raise SystemExit(f"Python runtime is {actual_python}, expected {python_lock.get('version')}")
  if source_revision(fmrimod_root) != packages.get("fmrimod", {}).get("revision"):
    raise SystemExit("fmrimod source revision does not match the environment lock")
  for package in ("numpy", "scipy", "nilearn"):
    expected = packages.get(package, {}).get("version")
    actual = package_version(package)
    if actual != expected:
      raise SystemExit(f"Python package {package} is {actual}, expected {expected}")

  generator = Path(__file__).resolve()
  return {
    "generator_sha256": sha256_file(generator),
    "locale": "C",
    "lock_path": str(path.relative_to(default_repo_root())),
    "lock_sha256": sha256_file(path),
    "runtime": f"Python {actual_python}",
  }


def design_matrix() -> np.ndarray:
  import numpy as np

  task = np.asarray(TASK, dtype=np.float64)
  motion = np.asarray(MOTION, dtype=np.float64)
  return np.column_stack([task, np.ones_like(task), motion])


def response_matrix() -> np.ndarray:
  import numpy as np

  task = np.asarray(TASK, dtype=np.float64)
  motion = np.asarray(MOTION, dtype=np.float64)
  noise0 = np.asarray(NOISE0, dtype=np.float64)
  noise1 = np.asarray(NOISE1, dtype=np.float64)
  voxel0 = 1.5 + 2.0 * task - 0.4 * motion + noise0
  voxel1 = -2.0 - 1.25 * task + 0.85 * motion + noise1
  return np.column_stack([voxel0, voxel1])


def compute_f_contrast(labels: np.ndarray, estimates: dict[Any, Any], contrast: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
  import numpy as np

  from nilearn.glm.contrasts import compute_contrast

  try:
    result = compute_contrast(labels, estimates, contrast, stat_type="F")
  except TypeError:
    result = compute_contrast(labels, estimates, contrast, contrast_type="F")
  return np.asarray(result.stat(), dtype=np.float64).reshape(-1), np.asarray(result.p_value(), dtype=np.float64).reshape(-1)


def fit_nilearn_reference_ols(x: np.ndarray, y: np.ndarray, contrast: np.ndarray) -> dict[str, np.ndarray]:
  """Match the fmrimod cross_testing.fitlins_parity Nilearn OLS reference path."""
  import numpy as np
  from scipy import stats as sp_stats

  from nilearn.glm.contrasts import compute_contrast
  from nilearn.glm.first_level import run_glm

  labels, estimates = run_glm(y, x, noise_model="ols")
  try:
    contrast_result = compute_contrast(labels, estimates, contrast, stat_type="t")
  except TypeError:
    contrast_result = compute_contrast(labels, estimates, contrast, contrast_type="t")

  n_regressors = x.shape[1]
  n_voxels = y.shape[1]
  betas = np.empty((n_regressors, n_voxels), dtype=np.float64)
  sigma2 = np.empty(n_voxels, dtype=np.float64)

  for label, result in estimates.items():
    mask = labels == label
    theta = np.asarray(result.theta, dtype=np.float64)
    theta = np.atleast_2d(theta)
    if theta.shape[0] != n_regressors and theta.shape[1] == n_regressors:
      theta = theta.T
    betas[:, mask] = theta

    dispersion = np.asarray(result.dispersion, dtype=np.float64).reshape(-1)
    if dispersion.size == 1:
      sigma2[mask] = dispersion[0]
    else:
      sigma2[mask] = dispersion

  t_values = np.asarray(contrast_result.stat(), dtype=np.float64).reshape(-1)
  p_raw = np.asarray(contrast_result.p_value(), dtype=np.float64).reshape(-1)

  dfres = float(x.shape[0] - np.linalg.matrix_rank(x))
  p_expected_two_sided = 2.0 * sp_stats.t.sf(np.abs(t_values), dfres)
  p_folded_two_sided = np.clip(2.0 * np.minimum(p_raw, 1.0 - p_raw), 0.0, 1.0)
  mae_raw = float(np.mean(np.abs(p_raw - p_expected_two_sided)))
  mae_folded = float(np.mean(np.abs(p_folded_two_sided - p_expected_two_sided)))
  p_values = p_raw if mae_raw <= mae_folded else p_folded_two_sided

  return {
    "betas": betas,
    "sigma2": sigma2,
    "t": t_values,
    "p": p_values,
  }


def matrix_payload(values: np.ndarray) -> list[list[float]]:
  import numpy as np

  return [[float(x) for x in row] for row in np.asarray(values, dtype=np.float64)]


def vector_payload(values: np.ndarray) -> list[float]:
  import numpy as np

  return [float(x) for x in np.asarray(values, dtype=np.float64).reshape(-1)]


def fixture_payload(fmrimod_root: Path) -> dict[str, Any]:
  import numpy as np

  if not fmrimod_root.exists():
    raise SystemExit(f"fmrimod root does not exist: {fmrimod_root}")
  fitlins_parity_path = (fmrimod_root / "cross_testing" / "fitlins_parity.py").resolve()
  if not fitlins_parity_path.exists():
    raise SystemExit(f"fmrimod fitlins parity source does not exist: {fitlins_parity_path}")
  from nilearn.glm.first_level import run_glm

  x = design_matrix()
  y = response_matrix()
  t_contrast = np.asarray([1.0, 0.0, 0.0], dtype=np.float64)
  task_f_contrast = np.asarray([[1.0, 0.0, 0.0]], dtype=np.float64)
  task_and_motion_f_contrast = np.asarray([[1.0, 0.0, 0.0], [0.0, 0.0, 1.0]], dtype=np.float64)

  t_reference = fit_nilearn_reference_ols(x, y, t_contrast)
  labels, estimates = run_glm(y, x, noise_model="ols")
  task_f_statistics, task_f_p_values = compute_f_contrast(labels, estimates, task_f_contrast)
  task_and_motion_f_statistics, task_and_motion_f_p_values = compute_f_contrast(
    labels,
    estimates,
    task_and_motion_f_contrast,
  )

  normalized_covariance = np.linalg.inv(x.T @ x)
  t_scale = float(t_contrast @ normalized_covariance @ t_contrast)
  t_estimates = np.asarray(t_contrast @ t_reference["betas"], dtype=np.float64).reshape(-1)
  t_standard_errors = np.sqrt(t_scale * np.asarray(t_reference["sigma2"], dtype=np.float64))

  environment = locked_environment(fmrimod_root)
  payload = {
    "schema_version": "scalafim-external-fixture/v1",
    "scenario_id": SCENARIO_ID,
    "truth_boundary": "fit_inference",
    "source": {
      "producer": "tools/scenarios/export_fit_public_f_contrast_fixture.py",
      "producer_command": "bash tools/r-parity/regenerate_receipts.sh --python-only",
      "fmrimod_root": "provided by --fmrimod-root",
      "fmrimod_revision": source_revision(fmrimod_root),
      "fmrimod_fitlins_parity": "cross_testing/fitlins_parity.py",
      "fmrimod_fitlins_parity_sha256": sha256_file(fitlins_parity_path),
      "reference": "Algorithm mirrored from fmrimod cross_testing.fitlins_parity.fit_fitlins_reference_ols plus nilearn run_glm/compute_contrast",
      "reference_revision": source_revision(fmrimod_root),
      "numpy_version": np.__version__,
      "scipy_version": package_version("scipy"),
      "nilearn_version": package_version("nilearn"),
    },
    "inputs": {
      "column_names": list(COLUMN_NAMES),
      "task": vector_payload(TASK),
      "motion": vector_payload(MOTION),
      "design": matrix_payload(x),
      "response_rows": matrix_payload(y),
      "contrasts": {
        "task_t": vector_payload(t_contrast),
        "task_f": matrix_payload(task_f_contrast),
        "task_and_motion_f": matrix_payload(task_and_motion_f_contrast),
      },
    },
    "outputs": {
      "residual_degrees_of_freedom": int(x.shape[0] - np.linalg.matrix_rank(x)),
      "coefficients": matrix_payload(t_reference["betas"]),
      "residual_variance": vector_payload(t_reference["sigma2"]),
      "task_t": {
        "estimates": vector_payload(t_estimates),
        "standard_errors": vector_payload(t_standard_errors),
        "statistics": vector_payload(t_reference["t"]),
        "p_values_two_sided": vector_payload(t_reference["p"]),
      },
      "task_f": {
        "statistics": vector_payload(task_f_statistics),
        "p_values": vector_payload(task_f_p_values),
      },
      "task_and_motion_f": {
        "statistics": vector_payload(task_and_motion_f_statistics),
        "p_values": vector_payload(task_and_motion_f_p_values),
      },
    },
    "receipt": {
      "schema_version": "scalafim-external-fixture/v1",
      "producer_command": "bash tools/r-parity/regenerate_receipts.sh --python-only",
      "accepted_differences": [],
      "comparison_policy": COMPARISON_POLICY,
      "environment": environment,
      "source": {
        "fmrimod_revision": source_revision(fmrimod_root),
        "fmrimod_fitlins_parity_sha256": sha256_file(fitlins_parity_path),
        "numpy_version": np.__version__,
        "scipy_version": package_version("scipy"),
        "nilearn_version": package_version("nilearn"),
        "reference_revision": source_revision(fmrimod_root),
      },
      "conventions": {
        "dtype": "float64",
        "json_encoding": "sorted-key UTF-8 JSON",
        "matrix_orientation": "rows=timepoints, columns=regressors or responses",
        "nilearn_glm": "run_glm(noise_model=ols)",
        "p_values": "two-sided t and Nilearn F contrast probabilities",
      },
      "hashes": {
        "inputs_sha256": sha256_json({
          "schema_version": "scalafim-external-fixture/v1",
          "inputs": {
            "column_names": list(COLUMN_NAMES),
            "task": vector_payload(TASK),
            "motion": vector_payload(MOTION),
            "design": matrix_payload(x),
            "response_rows": matrix_payload(y),
            "contrasts": {
              "task_t": vector_payload(t_contrast),
              "task_f": matrix_payload(task_f_contrast),
              "task_and_motion_f": matrix_payload(task_and_motion_f_contrast),
            },
          },
        }),
        "outputs_sha256": sha256_json({
          "schema_version": "scalafim-external-fixture/v1",
          "outputs": {
            "residual_degrees_of_freedom": int(x.shape[0] - np.linalg.matrix_rank(x)),
            "coefficients": matrix_payload(t_reference["betas"]),
            "residual_variance": vector_payload(t_reference["sigma2"]),
            "task_t": {
              "estimates": vector_payload(t_estimates),
              "standard_errors": vector_payload(t_standard_errors),
              "statistics": vector_payload(t_reference["t"]),
              "p_values_two_sided": vector_payload(t_reference["p"]),
            },
            "task_f": {
              "statistics": vector_payload(task_f_statistics),
              "p_values": vector_payload(task_f_p_values),
            },
            "task_and_motion_f": {
              "statistics": vector_payload(task_and_motion_f_statistics),
              "p_values": vector_payload(task_and_motion_f_p_values),
            },
          },
        }),
      },
    },
  }
  payload["receipt"]["hashes"]["generated_scala_sha256"] = hashlib.sha256(
    render_scala(payload).encode("utf-8")
  ).hexdigest()
  return payload


def render_json(payload: dict[str, Any]) -> str:
  return json.dumps(payload, indent=2, sort_keys=True) + "\n"


def scala_double(value: float) -> str:
  text = format(float(value), ".17g")
  if "e" not in text.lower() and "." not in text:
    text += ".0"
  return text.replace("e", "E")


def scala_string_vector(values: list[str]) -> str:
  return "Vector(" + ", ".join(json.dumps(value) for value in values) + ")"


def scala_double_vector(values: list[float]) -> str:
  return "Vector(" + ", ".join(scala_double(value) for value in values) + ")"


def scala_matrix_rows(values: list[list[float]], indent: str) -> str:
  rows = [indent + "  " + scala_double_vector(row) for row in values]
  return "Vector(\n" + ",\n".join(rows) + "\n" + indent + ")"


def render_scala(payload: dict[str, Any]) -> str:
  inputs = payload["inputs"]
  outputs = payload["outputs"]
  return f"""package scalafim.fmri.fit.scenarios

import gale.linalg.{{DMat, DVec}}

object PublicFContrastNilearnFixture:
  val scenarioId: String = {json.dumps(payload["scenario_id"])}
  val source: String = {json.dumps(payload["source"]["reference"])}
  val residualDegreesOfFreedom: Int = {outputs["residual_degrees_of_freedom"]}
  val columnNames: Vector[String] = {scala_string_vector(inputs["column_names"])}
  val task: Vector[Double] = {scala_double_vector(inputs["task"])}
  val motion: Vector[Double] = {scala_double_vector(inputs["motion"])}
  val responseRows: Vector[Vector[Double]] =
    {scala_matrix_rows(inputs["response_rows"], "    ")}
  val design: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      {scala_matrix_rows(inputs["design"], "      ")}
    )
  val coefficients: DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      {scala_matrix_rows(outputs["coefficients"], "      ")}
    )
  val residualVariance: DVec =
    DVec.fromSeq({scala_double_vector(outputs["residual_variance"])})
  val taskTEstimates: DVec =
    DVec.fromSeq({scala_double_vector(outputs["task_t"]["estimates"])})
  val taskTStandardErrors: DVec =
    DVec.fromSeq({scala_double_vector(outputs["task_t"]["standard_errors"])})
  val taskTStatistics: DVec =
    DVec.fromSeq({scala_double_vector(outputs["task_t"]["statistics"])})
  val taskTPValues: DVec =
    DVec.fromSeq({scala_double_vector(outputs["task_t"]["p_values_two_sided"])})
  val taskFStatistics: DVec =
    DVec.fromSeq({scala_double_vector(outputs["task_f"]["statistics"])})
  val taskFPValues: DVec =
    DVec.fromSeq({scala_double_vector(outputs["task_f"]["p_values"])})
  val taskAndMotionFStatistics: DVec =
    DVec.fromSeq({scala_double_vector(outputs["task_and_motion_f"]["statistics"])})
"""


def _is_sha256(value: Any) -> bool:
  return isinstance(value, str) and len(value) == 64 and all(
    character in "0123456789abcdef" for character in value
  )


def validate_local_fixture(json_path: Path, scala_path: Path) -> list[str]:
  """Validate checked-in fixture coherence without importing the reference stack."""
  if not json_path.exists():
    return [str(json_path)]
  if not scala_path.exists():
    return [str(scala_path)]

  try:
    payload = json.loads(json_path.read_text())
  except (OSError, UnicodeError, json.JSONDecodeError) as error:
    return [f"{json_path}: invalid JSON ({error})"]

  if not isinstance(payload, dict):
    return [f"{json_path}: fixture root must be an object"]

  stale: list[str] = []
  expected_schema = "scalafim-external-fixture/v1"
  if payload.get("schema_version") != expected_schema:
    stale.append(f"{json_path}: schema_version")
  if payload.get("scenario_id") != SCENARIO_ID:
    stale.append(f"{json_path}: scenario_id")

  receipt = payload.get("receipt")
  source = payload.get("source")
  conventions = receipt.get("conventions") if isinstance(receipt, dict) else None
  hashes = receipt.get("hashes") if isinstance(receipt, dict) else None
  receipt_source = receipt.get("source") if isinstance(receipt, dict) else None
  environment = receipt.get("environment") if isinstance(receipt, dict) else None
  required_source = (
    "fmrimod_revision",
    "fmrimod_fitlins_parity_sha256",
    "numpy_version",
    "scipy_version",
    "nilearn_version",
    "reference_revision",
  )
  required_conventions = (
    "dtype",
    "json_encoding",
    "matrix_orientation",
    "nilearn_glm",
    "p_values",
  )
  if not isinstance(receipt, dict) or receipt.get("schema_version") != expected_schema:
    stale.append(f"{json_path}: receipt.schema_version")
  if not isinstance(receipt, dict) or not isinstance(receipt.get("producer_command"), str):
    stale.append(f"{json_path}: receipt.producer_command")
  if not isinstance(receipt, dict) or not isinstance(receipt.get("accepted_differences"), list):
    stale.append(f"{json_path}: receipt.accepted_differences")
  if not isinstance(receipt, dict) or receipt.get("comparison_policy") != COMPARISON_POLICY:
    stale.append(f"{json_path}: receipt.comparison_policy")
  if payload.get("truth_boundary") != "fit_inference":
    stale.append(f"{json_path}: truth_boundary")
  if not isinstance(receipt_source, dict) or any(
      not isinstance(receipt_source.get(key), str) or not receipt_source.get(key)
      for key in required_source
  ):
    stale.append(f"{json_path}: receipt.source")
  if not isinstance(conventions, dict) or any(
      not isinstance(conventions.get(key), str) or not conventions.get(key)
      for key in required_conventions
  ):
    stale.append(f"{json_path}: receipt.conventions")
  if not isinstance(hashes, dict) or any(
      not _is_sha256(hashes.get(key))
      for key in ("inputs_sha256", "outputs_sha256", "generated_scala_sha256")
  ):
    stale.append(f"{json_path}: receipt.hashes")
  try:
    expected_environment = locked_environment(default_fmrimod_root())
  except SystemExit:
    lock_path, lock = environment_lock()
    expected_environment = {
      "generator_sha256": sha256_file(Path(__file__).resolve()),
      "locale": "C",
      "lock_path": str(lock_path.relative_to(default_repo_root())),
      "lock_sha256": sha256_file(lock_path),
      "runtime": f"Python {lock['python']['version']}",
    }
  if environment != expected_environment:
    stale.append(f"{json_path}: receipt.environment")

  if isinstance(source, dict) and isinstance(receipt_source, dict):
    for key in required_source:
      if source.get(key) != receipt_source.get(key):
        stale.append(f"{json_path}: source.{key}")

  if isinstance(hashes, dict) and isinstance(payload.get("inputs"), dict) and isinstance(payload.get("outputs"), dict):
    try:
      input_hash = sha256_json({
        "schema_version": payload.get("schema_version"),
        "inputs": payload["inputs"],
      })
      output_hash = sha256_json({
        "schema_version": payload.get("schema_version"),
        "outputs": payload["outputs"],
      })
      if hashes.get("inputs_sha256") != input_hash:
        stale.append(f"{json_path}: receipt.hashes.inputs_sha256")
      if hashes.get("outputs_sha256") != output_hash:
        stale.append(f"{json_path}: receipt.hashes.outputs_sha256")
    except (TypeError, ValueError):
      stale.append(f"{json_path}: non-canonical numeric payload")

  try:
    scala_text = scala_path.read_text()
    if scala_text != render_scala(payload):
      stale.append(str(scala_path))
    if isinstance(hashes, dict) and hashes.get("generated_scala_sha256") != hashlib.sha256(scala_text.encode("utf-8")).hexdigest():
      stale.append(f"{json_path}: receipt.hashes.generated_scala_sha256")
  except (OSError, KeyError, TypeError):
    stale.append(f"{scala_path}: cannot render from fixture")
  return stale


def parse_args() -> argparse.Namespace:
  repo = default_repo_root()
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--fmrimod-root", type=Path, default=default_fmrimod_root())
  parser.add_argument(
    "--json-output",
    type=Path,
    default=repo / "docs" / "scenarios" / "fixtures" / "fit.public-f-contrast.v1.nilearn.json",
  )
  parser.add_argument(
    "--scala-output",
    type=Path,
    default=repo
    / "modules"
    / "fit"
    / "shared"
    / "src"
    / "test"
    / "scala"
    / "scalafim"
    / "fmri"
    / "fit"
    / "scenarios"
    / "PublicFContrastNilearnFixture.scala",
  )
  parser.add_argument("--check", action="store_true", help="fail if generated files are absent or stale")
  parser.add_argument(
    "--check-external",
    action="store_true",
    help="regenerate through the external Nilearn reference before checking; requires the reference stack",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  if args.check_external:
    args.check = True
  if args.check and not args.check_external:
    stale = validate_local_fixture(args.json_output, args.scala_output)
    if stale:
      print("stale or invalid generated fixture files:")
      for path in stale:
        print(f"  {path}")
      return 1
    print(f"{SCENARIO_ID}: generated fixture files are current")
    return 0

  payload = fixture_payload(args.fmrimod_root.resolve())
  outputs = {
    args.json_output: render_json(payload),
    args.scala_output: render_scala(payload),
  }

  if args.check:
    stale = []
    for path, expected in outputs.items():
      if not path.exists() or path.read_text() != expected:
        stale.append(str(path))
    if stale:
      print("stale generated fixture files:")
      for path in stale:
        print(f"  {path}")
      return 1
    print(f"{SCENARIO_ID}: generated fixture files are current")
    return 0

  for path, content in outputs.items():
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)
    print(f"wrote {path}")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
