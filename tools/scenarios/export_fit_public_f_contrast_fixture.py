#!/usr/bin/env python3
"""Export the Nilearn reference fixture for fit.public-f-contrast.v1.

The generated Scala object is checked into shared test sources so both the JVM
and Scala.js suites can compare against the same external reference without
runtime filesystem access.
"""

from __future__ import annotations

import argparse
import importlib.metadata
import json
from pathlib import Path
from typing import Any

import numpy as np
from scipy import stats as sp_stats


SCENARIO_ID = "fit.public-f-contrast.v1"
COLUMN_NAMES = ("task", "base_constant", "nuis#01_1")

TASK = np.asarray([-1.5, -1.0, -0.25, 0.75, 1.25, -0.5, 0.5, 1.75], dtype=np.float64)
MOTION = np.asarray([0.2, -0.4, 0.7, -0.6, 0.1, 0.9, -0.8, 0.3], dtype=np.float64)
NOISE0 = np.asarray([0.05, -0.02, 0.03, -0.04, 0.01, -0.03, 0.02, -0.02], dtype=np.float64)
NOISE1 = np.asarray([-0.01, 0.04, -0.02, 0.03, -0.05, 0.02, -0.03, 0.01], dtype=np.float64)


def default_repo_root() -> Path:
  return Path(__file__).resolve().parents[2]


def default_fmrimod_root() -> Path:
  return Path.home() / "code" / "pycode" / "fmrimod"


def package_version(name: str) -> str:
  try:
    return importlib.metadata.version(name)
  except importlib.metadata.PackageNotFoundError:
    return "uninstalled"


def design_matrix() -> np.ndarray:
  return np.column_stack([TASK, np.ones_like(TASK), MOTION])


def response_matrix() -> np.ndarray:
  voxel0 = 1.5 + 2.0 * TASK - 0.4 * MOTION + NOISE0
  voxel1 = -2.0 - 1.25 * TASK + 0.85 * MOTION + NOISE1
  return np.column_stack([voxel0, voxel1])


def compute_f_contrast(labels: np.ndarray, estimates: dict[Any, Any], contrast: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
  from nilearn.glm.contrasts import compute_contrast

  try:
    result = compute_contrast(labels, estimates, contrast, stat_type="F")
  except TypeError:
    result = compute_contrast(labels, estimates, contrast, contrast_type="F")
  return np.asarray(result.stat(), dtype=np.float64).reshape(-1), np.asarray(result.p_value(), dtype=np.float64).reshape(-1)


def fit_nilearn_reference_ols(x: np.ndarray, y: np.ndarray, contrast: np.ndarray) -> dict[str, np.ndarray]:
  """Match the fmrimod cross_testing.fitlins_parity Nilearn OLS reference path."""
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
  return [[float(x) for x in row] for row in np.asarray(values, dtype=np.float64)]


def vector_payload(values: np.ndarray) -> list[float]:
  return [float(x) for x in np.asarray(values, dtype=np.float64).reshape(-1)]


def fixture_payload(fmrimod_root: Path) -> dict[str, Any]:
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

  return {
    "schema_version": "scalafim-external-fixture/v1",
    "scenario_id": SCENARIO_ID,
    "source": {
      "producer": "tools/scenarios/export_fit_public_f_contrast_fixture.py",
      "fmrimod_root": "provided by --fmrimod-root",
      "fmrimod_fitlins_parity": "cross_testing/fitlins_parity.py",
      "reference": "Algorithm mirrored from fmrimod cross_testing.fitlins_parity.fit_fitlins_reference_ols plus nilearn run_glm/compute_contrast",
      "numpy_version": np.__version__,
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
  }


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

import scalafim.linalg.{{DoubleMatrix, DoubleVector}}

object PublicFContrastNilearnFixture:
  val scenarioId: String = {json.dumps(payload["scenario_id"])}
  val source: String = {json.dumps(payload["source"]["reference"])}
  val residualDegreesOfFreedom: Int = {outputs["residual_degrees_of_freedom"]}
  val columnNames: Vector[String] = {scala_string_vector(inputs["column_names"])}
  val task: Vector[Double] = {scala_double_vector(inputs["task"])}
  val motion: Vector[Double] = {scala_double_vector(inputs["motion"])}
  val responseRows: Vector[Vector[Double]] =
    {scala_matrix_rows(inputs["response_rows"], "    ")}
  val design: DoubleMatrix =
    DoubleMatrix.fromRows(
      {scala_matrix_rows(inputs["design"], "      ")}
    )
  val coefficients: DoubleMatrix =
    DoubleMatrix.fromRows(
      {scala_matrix_rows(outputs["coefficients"], "      ")}
    )
  val residualVariance: DoubleVector =
    DoubleVector.fromSeq({scala_double_vector(outputs["residual_variance"])})
  val taskTEstimates: DoubleVector =
    DoubleVector.fromSeq({scala_double_vector(outputs["task_t"]["estimates"])})
  val taskTStandardErrors: DoubleVector =
    DoubleVector.fromSeq({scala_double_vector(outputs["task_t"]["standard_errors"])})
  val taskTStatistics: DoubleVector =
    DoubleVector.fromSeq({scala_double_vector(outputs["task_t"]["statistics"])})
  val taskFStatistics: DoubleVector =
    DoubleVector.fromSeq({scala_double_vector(outputs["task_f"]["statistics"])})
  val taskAndMotionFStatistics: DoubleVector =
    DoubleVector.fromSeq({scala_double_vector(outputs["task_and_motion_f"]["statistics"])})
"""


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
  return parser.parse_args()


def main() -> int:
  args = parse_args()
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
