#!/usr/bin/env python3
"""Check or regenerate every external receipt declared by the scenario manifest."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
import importlib.metadata
import json
import os
from pathlib import Path
import subprocess
import sys
from typing import Any


REPO_ROOT = Path(__file__).resolve().parents[2]
MANIFEST = REPO_ROOT / "docs" / "scenarios" / "manifest.json"
LOCK_PATH = REPO_ROOT / "tools" / "r-parity" / "reference-lock.json"
LOCK_SCHEMA = "scalafim-parity-environment-lock/v1"


@dataclass(frozen=True, order=True)
class ReceiptJob:
  exporter: str
  finalizer: str | None


def load_json(path: Path) -> dict[str, Any]:
  payload = json.loads(path.read_text())
  if not isinstance(payload, dict):
    raise SystemExit(f"expected an object in {path}")
  return payload


def receipt_jobs() -> tuple[ReceiptJob, ...]:
  manifest = load_json(MANIFEST)
  jobs = {
    ReceiptJob(reference["exporter_path"], reference.get("finalizer_path"))
    for scenario in manifest.get("scenarios", [])
    if isinstance(scenario, dict)
    for reference in (scenario.get("reference"),)
    if isinstance(reference, dict) and isinstance(reference.get("exporter_path"), str)
  }
  if not jobs:
    raise SystemExit("scenario manifest declares no external receipt generators")
  return tuple(sorted(jobs))


def run(command: list[str], *, environment: dict[str, str] | None = None) -> None:
  completed = subprocess.run(
    command,
    cwd=REPO_ROOT,
    env=environment,
    check=False,
  )
  if completed.returncode != 0:
    raise SystemExit(completed.returncode)


def git_revision(path: Path) -> str:
  completed = subprocess.run(
    ["git", "-C", str(path), "rev-parse", "HEAD"],
    capture_output=True,
    check=False,
    text=True,
  )
  if completed.returncode != 0:
    raise SystemExit(f"cannot resolve a git revision for locked source {path}")
  return completed.stdout.strip()


def locked_sources(section: dict[str, Any]) -> dict[str, Path]:
  resolved: dict[str, Path] = {}
  packages = section.get("packages")
  if not isinstance(packages, dict):
    raise SystemExit("environment lock packages must be an object")

  default_roots = {
    "fmridesign": Path.home() / "code" / "fmridesign",
    "fmrihrf": Path.home() / "code" / "fmrihrf",
    "fmrireg": Path.home() / "code" / "fmrireg",
    "fmrimod": Path.home() / "code" / "pycode" / "fmrimod",
  }
  for package, package_lock in packages.items():
    if not isinstance(package_lock, dict) or not isinstance(package_lock.get("revision"), str):
      continue
    variable = package_lock.get("environment_variable")
    if not isinstance(variable, str) or not variable:
      raise SystemExit(f"locked source {package} must declare environment_variable")
    configured = os.environ.get(variable)
    root = Path(configured).expanduser() if configured else default_roots.get(package)
    if root is None or not root.is_dir():
      raise SystemExit(f"locked source {package} is missing; set {variable}")
    actual = git_revision(root)
    if actual != package_lock["revision"]:
      raise SystemExit(
        f"locked source {package} is at {actual}, expected {package_lock['revision']} ({root})"
      )
    resolved[variable] = root.resolve()
  return resolved


def require_python_packages(lock: dict[str, Any]) -> None:
  packages = lock.get("packages")
  if not isinstance(packages, dict):
    raise SystemExit("Python environment lock packages must be an object")
  for package in ("numpy", "scipy", "nilearn"):
    package_lock = packages.get(package)
    if not isinstance(package_lock, dict) or not isinstance(package_lock.get("version"), str):
      raise SystemExit(f"Python environment lock is missing {package}.version")
    try:
      actual = importlib.metadata.version(package)
    except importlib.metadata.PackageNotFoundError as error:
      raise SystemExit(f"locked Python package is not installed: {package}") from error
    if actual != package_lock["version"]:
      raise SystemExit(
        f"Python package {package} is {actual}, expected {package_lock['version']}"
      )


def check_job(job: ReceiptJob) -> None:
  exporter = REPO_ROOT / job.exporter
  if not exporter.is_file():
    raise SystemExit(f"missing receipt generator: {job.exporter}")
  if job.finalizer is None:
    run([sys.executable, "-S", str(exporter), "--check"])
  else:
    finalizer = REPO_ROOT / job.finalizer
    if not finalizer.is_file():
      raise SystemExit(f"missing receipt finalizer: {job.finalizer}")
    run([sys.executable, "-S", str(finalizer), "--check"])


def regenerate_r(jobs: tuple[ReceiptJob, ...], lock: dict[str, Any]) -> None:
  r_lock = lock.get("r")
  if not isinstance(r_lock, dict):
    raise SystemExit("environment lock must contain an R section")
  sources = locked_sources(r_lock)
  environment = dict(os.environ)
  environment.update({name: str(path) for name, path in sources.items()})
  environment.update({"LC_ALL": str(r_lock["locale"]), "LANG": str(r_lock["locale"]), "TZ": "UTC"})

  r_jobs = tuple(job for job in jobs if job.exporter.endswith(".R"))
  for job in r_jobs:
    if job.finalizer is None:
      raise SystemExit(f"R receipt generator has no finalizer: {job.exporter}")
    run(["Rscript", str(REPO_ROOT / job.exporter)], environment=environment)
    run([sys.executable, "-S", str(REPO_ROOT / job.finalizer)], environment=environment)
  print(f"regenerated {len(r_jobs)} locked R receipts")


def regenerate_python(jobs: tuple[ReceiptJob, ...], lock: dict[str, Any]) -> None:
  python_lock = lock.get("python")
  if not isinstance(python_lock, dict):
    raise SystemExit("environment lock must contain a Python section")
  expected_python = python_lock.get("version")
  actual_python = f"{sys.version_info.major}.{sys.version_info.minor}"
  if actual_python != expected_python:
    raise SystemExit(f"Python runtime is {actual_python}, expected {expected_python}")
  sources = locked_sources(python_lock)
  require_python_packages(python_lock)
  fmrimod_root = sources.get("FMRIMOD_ROOT")
  if fmrimod_root is None:
    raise SystemExit("Python environment lock must provide FMRIMOD_ROOT")

  python_jobs = tuple(job for job in jobs if job.exporter.endswith(".py"))
  for job in python_jobs:
    if job.finalizer is not None:
      raise SystemExit(f"Python receipt unexpectedly declares a finalizer: {job.exporter}")
    run([sys.executable, str(REPO_ROOT / job.exporter), "--fmrimod-root", str(fmrimod_root)])
    run([sys.executable, "-S", str(REPO_ROOT / job.exporter), "--check"])
  print(f"regenerated {len(python_jobs)} locked Python receipts")


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument(
    "--regenerate",
    choices=("all", "r", "python"),
    help="run locked external generators before checking their portable outputs",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  jobs = receipt_jobs()
  lock = load_json(LOCK_PATH)
  if lock.get("schema_version") != LOCK_SCHEMA:
    raise SystemExit(f"environment lock schema must be {LOCK_SCHEMA}")

  if args.regenerate in ("all", "r"):
    regenerate_r(jobs, lock)
  if args.regenerate in ("all", "python"):
    regenerate_python(jobs, lock)

  for job in jobs:
    check_job(job)
  print(f"checked {len(jobs)} external receipts")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
