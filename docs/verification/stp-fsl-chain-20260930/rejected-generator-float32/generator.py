#!/usr/bin/env python3
"""Freeze native references for the existing FSL chain scenario, without fitting a new registration."""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import shutil
import subprocess
from pathlib import Path

import nibabel as nib
import numpy as np

ROOT = Path(__file__).resolve().parents[2]
ORACLE = ROOT / "modules/transform/shared/src/test/resources/scalafim/transform/oracle"
CASE = ORACLE / "neurotransform/fsl_coef_oracle/srcleft_refright_aff"
IMAGE = "sha256:3ffbceee2ab631d765c6e2d1d90eedc9f31a33e8c555ec08e85ef6d65c66c1e2"


def digest(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def save(data: np.ndarray, affine: np.ndarray, path: Path) -> None:
  image = nib.Nifti1Image(data.astype(np.float32), affine)
  image.set_sform(affine, code=1)
  image.set_qform(None, code=0)
  image.header.set_xyzt_units("mm", "sec")
  nib.save(image, path)


def scaled(image: nib.Nifti1Image) -> np.ndarray:
  result = np.diag([*image.header.get_zooms()[:3], 1.0]).astype(np.float64)
  if np.linalg.det(image.affine[:3, :3]) > 0:
    result[0, 0] *= -1
    result[0, 3] = (image.shape[0] - 1) * image.header.get_zooms()[0]
  return result


def freeze_inputs(output: Path) -> None:
  for name in ("source.nii.gz", "target.nii.gz", "coef.nii.gz"):
    shutil.copy2(CASE / name, output / name)
  highres = nib.load(output / "source.nii.gz")
  centre = (highres.affine @ np.r_[((np.array(highres.shape[:3]) - 1) / 2), 1])[:3]
  cz, sz, cx, sx = math.cos(.06), math.sin(.06), math.cos(-.04), math.sin(-.04)
  linear = np.array([[cz, -sz * cx, sz * sx], [sz, cz * cx, -cz * sx], [0, sx, cx]]) @ np.diag([1.02, .99, 1.01])
  registration = np.eye(4)
  registration[:3, :3] = linear
  registration[:3, 3] = centre + np.array([1.2, -.8, 2.1]) - linear @ centre
  cy, sy = math.cos(.1), math.sin(.1)
  rotation = np.array([[cy, 0, sy], [0, 1, 0], [-sy, 0, cy]])
  pixdim = np.array([2.2, 2., 2.6])
  corners = np.array([[i, j, k, 1] for i in (-1., float(highres.shape[0])) for j in (-1., float(highres.shape[1])) for k in (-1., float(highres.shape[2]))])
  points = (np.linalg.inv(registration) @ highres.affine @ corners.T)[:3]
  coordinates = (rotation.T @ points) / pixdim[:, None]
  low = np.floor(coordinates.min(axis=1)) - 3
  dims = (np.ceil(coordinates.max(axis=1)) + 3 - low).astype(int) + 1
  affine = np.eye(4)
  affine[:3, :3] = rotation @ np.diag(pixdim)
  affine[:3, 3] = rotation @ (low * pixdim)
  indices = np.indices(tuple(dims), dtype=np.float64)
  ramp = 40 + 1.5 * indices[0] - .7 * indices[1] + 2.2 * indices[2]
  save(ramp, affine, output / "func.nii.gz")
  # Use the stored NIfTI header, including float32 sform/pixdim storage, for both native and Scala inputs.
  func = nib.load(output / "func.nii.gz")
  matrix = scaled(highres) @ np.linalg.inv(highres.affine) @ registration @ func.affine @ np.linalg.inv(scaled(func))
  np.savetxt(output / "example_func2highres.mat", matrix, fmt="%.10f")
  homogeneous = np.vstack((indices.reshape(3, -1), np.ones(indices[0].size)))
  world = (func.affine @ homogeneous)[:3].reshape((3, *dims))
  for axis in range(3):
    # Copy the exact already-stored affine so these ramps have the same header as func.
    save(world[axis], func.affine, output / f"func_coord{axis}.nii.gz")
  save(np.ones(tuple(dims)), func.affine, output / "func_support.nii.gz")


def main() -> None:
  parser = argparse.ArgumentParser()
  parser.add_argument("--output", type=Path, default=ORACLE / "fsl_chain_native")
  args = parser.parse_args()
  output = args.output.resolve()
  if output.exists():
    raise SystemExit(f"Refusing to replace existing native evidence: {output}")
  actual = subprocess.check_output(["docker", "image", "inspect", "--format", "{{.Id}}", IMAGE], text=True).strip()
  if actual != IMAGE:
    raise SystemExit("Native image identity mismatch")
  output.mkdir(parents=True)
  freeze_inputs(output)
  inputs = {p.name: digest(p) for p in output.iterdir() if p.is_file()}
  commands = []

  def run(command: str) -> None:
    argv = ["docker", "run", "--rm", "--network", "none", "--platform", "linux/amd64", "--cpus", "2", "--memory", "1g", "--entrypoint", "/bin/sh", "-v", f"{output}:/fixture", IMAGE, "-ec", "export FSLDIR=/opt/fsl6 PATH=/opt/fsl6/bin:$PATH FSLOUTPUTTYPE=NIFTI_GZ OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1; " + command]
    result = subprocess.run(argv, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=300)
    commands.append({"command": command, "exit_code": result.returncode, "output": result.stdout})
    (output / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
    if result.returncode:
      raise RuntimeError(f"Native command failed; receipt preserved: {command}")

  for name in ("coord0", "coord1", "coord2", "support", "phantom"):
    source = "func.nii.gz" if name == "phantom" else f"func_{name}.nii.gz"
    run(f"flirt -in /fixture/{source} -ref /fixture/source.nii.gz -init /fixture/example_func2highres.mat -applyxfm -interp trilinear -out /fixture/flirt_{name}.nii.gz")
  run("convertwarp --ref=/fixture/target.nii.gz --premat=/fixture/example_func2highres.mat --warp1=/fixture/coef.nii.gz --out=/fixture/chain_abs.nii.gz --absout")
  for mode, options in (("premat", "--warp=/fixture/coef.nii.gz --premat=/fixture/example_func2highres.mat"), ("composed", "--warp=/fixture/chain_abs.nii.gz --abs")):
    for name in ("coord0", "coord1", "coord2", "support", "phantom"):
      source = "func.nii.gz" if name == "phantom" else f"func_{name}.nii.gz"
      run(f"applywarp --in=/fixture/{source} --ref=/fixture/target.nii.gz {options} --interp=trilinear --out=/fixture/{mode}_{name}.nii.gz")

  comparisons = {}
  for name, expected in inputs.items():
    if digest(output / name) != expected:
      raise RuntimeError(f"Native command changed a frozen input: {name}")
  support = np.asarray(nib.load(output / "premat_support.nii.gz").dataobj) >= .999
  support &= np.asarray(nib.load(output / "composed_support.nii.gz").dataobj) >= .999
  if int(support.sum()) < 100:
    raise RuntimeError(f"Only {support.sum()} common native supported queries")
  for name in ("coord0", "coord1", "coord2", "phantom"):
    a = nib.load(output / f"premat_{name}.nii.gz").get_fdata()
    b = nib.load(output / f"composed_{name}.nii.gz").get_fdata()
    if not np.isfinite(a[support]).all() or not np.isfinite(b[support]).all():
      raise RuntimeError(f"Non-finite native supported {name}")
    comparisons[name] = {"count": int(support.sum()), "maximum_absolute_difference": float(np.max(np.abs(a[support] - b[support])))}
  manifest = {"schema": "scalafim-fsl-chain-native/v1", "image_id": IMAGE, "generator": "tools/transform/generate_fsl_chain_native_oracle.py", "numpy_version": np.__version__, "nibabel_version": nib.__version__, "input_sha256": inputs, "fixture_sha256": {p.name: digest(p) for p in sorted(output.iterdir()) if p.is_file()}, "command_count": len(commands), "native_route_comparisons": comparisons, "scope": "Exact existing synthetic functional grid and declared FLIRT registration followed by the frozen FNIRT coefficient case. No new registration fit or real-subject qualification. Scala numerical acceptance is separate from command success."}
  (output / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
  print(json.dumps({"commands": len(commands), "comparisons": comparisons}, indent=2))


if __name__ == "__main__":
  main()
