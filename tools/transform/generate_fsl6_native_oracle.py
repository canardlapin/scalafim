#!/usr/bin/env python3
"""Generate the FSL 6 native numerical oracle from frozen FSL 5 fixtures."""
from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import tempfile
from pathlib import Path

import nibabel as nib
import numpy as np

ROOT = Path(__file__).resolve().parents[2]
ORACLE = ROOT / "modules/transform/shared/src/test/resources/scalafim/transform/oracle"
COEFFICIENTS = ORACLE / "neurotransform/fsl_coef_oracle"
DENSE = ORACLE / "neurotransform/fsl_dense_oracle"
CONVENTIONS = ORACLE / "conventions"
OUT = ORACLE / "fsl6_native"
EXPECTED_IMAGE = "sha256:3ffbceee2ab631d765c6e2d1d90eedc9f31a33e8c555ec08e85ef6d65c66c1e2"


def digest(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()


def relative(path: Path) -> str:
  return path.relative_to(ORACLE).as_posix()


def image_id(image: str) -> str:
  return subprocess.run(["docker", "image", "inspect", "--format", "{{.Id}}", image], check=True, text=True, stdout=subprocess.PIPE).stdout.strip()


def run(image: str, work: Path, command: str) -> dict[str, object]:
  setup = "export FSLDIR=/opt/fsl6 PATH=/opt/fsl6/bin:$PATH FSLOUTPUTTYPE=NIFTI_GZ OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1; "
  result = subprocess.run(
    ["docker", "run", "--rm", "--network", "none", "--platform", "linux/amd64", "--cpus", "2", "--memory", "1g", "--entrypoint", "/bin/sh", "-v", f"{work}:/input:ro", "-v", f"{OUT}:/out", image, "-ec", setup + command],
    text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=300,
  )
  receipt = {"command": command, "returncode": result.returncode, "output": result.stdout[-4000:]}
  if result.returncode != 0:
    raise RuntimeError(json.dumps(receipt, indent=2))
  return receipt


def write_ramps(source: Path, destination: Path) -> None:
  image = nib.load(str(source))
  indices = np.indices(image.shape[:3], dtype=np.float64)
  homogeneous = np.concatenate((indices.reshape(3, -1), np.ones((1, indices[0].size))))
  world = (image.affine @ homogeneous)[:3].reshape((3, *image.shape[:3]))
  for axis in range(3):
    nib.save(nib.Nifti1Image(world[axis].astype(np.float32), image.affine), str(destination / f"coord{axis}.nii.gz"))
  nib.save(nib.Nifti1Image(np.ones(image.shape[:3], dtype=np.float32), image.affine), str(destination / "support.nii.gz"))


def copy_case(source: Path, work: Path, names: tuple[str, ...]) -> Path:
  target = work / source.name
  target.mkdir()
  for name in names:
    shutil.copy2(source / name, target / name)
  write_ramps(target / "source.nii.gz", target)
  return target


def array(path: Path) -> np.ndarray:
  return np.asarray(nib.load(str(path)).get_fdata(dtype=np.float64))


def checked_difference(new: Path, old: Path, support: np.ndarray, tolerance: float, label: str) -> dict[str, object]:
  actual, expected = array(new), array(old)
  if actual.shape != expected.shape:
    raise RuntimeError(f"{label}: shape {actual.shape} != {expected.shape}")
  values = np.abs(actual[support] - expected[support])
  if not np.isfinite(actual[support]).all() or not np.isfinite(expected[support]).all():
    raise RuntimeError(f"{label}: non-finite supported value")
  maximum = float(values.max())
  if maximum > tolerance:
    raise RuntimeError(f"{label}: max difference {maximum} exceeds {tolerance}")
  return {"supported_voxels": int(support.sum()), "max_abs": maximum, "p99_abs": float(np.quantile(values, .99))}


def coefficient_cases() -> list[str]:
  return [item["id"] for item in json.loads((COEFFICIENTS / "manifest.json").read_text())["cases"]]


def dense_cases() -> list[dict[str, str]]:
  return json.loads((DENSE / "manifest.json").read_text())["cases"]


def flirt_cases() -> list[tuple[str, Path, Path, Path]]:
  keys = list(dict.fromkeys(line.split("\t", 1)[0] for line in (CONVENTIONS / "flirt_points.tsv").read_text().splitlines()[1:]))
  result = []
  for key in keys:
    if key == "fresh":
      result.append((key, CONVENTIONS / "flirt_fresh_src.nii", CONVENTIONS / "flirt_fresh_ref.nii", CONVENTIONS / "flirt_fresh.mat"))
    else:
      source, reference = key.split("_to_")
      result.append((key, CONVENTIONS / f"fsl_case{source}.nii", CONVENTIONS / f"fsl_case{reference}.nii", CONVENTIONS / f"flirt_{key}.mat"))
  return result


def voxel_probes(image: Path) -> np.ndarray:
  high = np.array(nib.load(str(image)).shape[:3], dtype=float) - 1
  return np.array([[0, 0, 0], high, high / 2, [1, high[1] - 1, 1], [high[0] - 1, 1, high[2] - 1], [2, 3, 4], [high[0] - 2, high[1] - 3, high[2] - 4]], dtype=float)


def main() -> None:
  parser = argparse.ArgumentParser()
  parser.add_argument("--image", required=True, help="approved immutable Docker image ID (sha256:...)")
  parser.add_argument("--package-lock", type=Path, required=True, help="exact installed conda package lock")
  args = parser.parse_args()
  lock = args.package_lock.resolve()
  if args.image != EXPECTED_IMAGE:
    raise SystemExit(f"--image must be {EXPECTED_IMAGE}")
  if image_id(args.image) != EXPECTED_IMAGE:
    raise SystemExit("docker image inspect did not resolve the requested immutable image")
  if not lock.is_file():
    raise SystemExit(f"missing package lock: {lock}")

  if OUT.exists():
    shutil.rmtree(OUT)
  OUT.mkdir(parents=True)
  shutil.copy2(lock, OUT / "fsl6-package-lock.json")
  receipts: list[dict[str, object]] = []
  comparisons: dict[str, object] = {"coefficients": {}, "dense": {}}
  inputs: dict[str, str] = {}

  with tempfile.TemporaryDirectory(prefix="scalafim-fsl6-native-") as temporary:
    work = Path(temporary)
    for name in coefficient_cases():
      source = COEFFICIENTS / name
      names = ("source.nii.gz", "target.nii.gz", "coef.nii.gz") + (("premat.mat",) if (source / "premat.mat").exists() else ())
      copy_case(source, work, names)
      for file in names:
        inputs[relative(source / file)] = digest(source / file)
      for axis in range(3):
        receipts.append(run(args.image, work, f"applywarp --in=/input/{name}/coord{axis}.nii.gz --ref=/input/{name}/target.nii.gz --warp=/input/{name}/coef.nii.gz --out=/out/coef_{name}_coord{axis}.nii.gz --interp=trilinear"))
      receipts.append(run(args.image, work, f"applywarp --in=/input/{name}/support.nii.gz --ref=/input/{name}/target.nii.gz --warp=/input/{name}/coef.nii.gz --out=/out/coef_{name}_support.nii.gz --interp=trilinear"))
      for kind, option in (("noaff", ""), ("aff", " --withaff")):
        receipts.append(run(args.image, work, f"fnirtfileutils --in=/input/{name}/coef.nii.gz --ref=/input/{name}/target.nii.gz --out=/out/coef_{name}_field_{kind}.nii.gz{option}"))
      receipts.append(run(args.image, work, f"fnirtfileutils --in=/input/{name}/coef.nii.gz --ref=/input/{name}/target.nii.gz --jac=/out/coef_{name}_jac.nii.gz --withaff"))
      support = (array(source / "native_support.nii.gz") >= .999) & (array(OUT / f"coef_{name}_support.nii.gz") >= .999)
      if int(support.sum()) < 100:
        raise RuntimeError(f"{name}: only {int(support.sum())} shared supported voxels")
      coordinates = [checked_difference(OUT / f"coef_{name}_coord{axis}.nii.gz", source / f"native_coord{axis}.nii.gz", support, 2e-5, f"{name} coordinate {axis}") for axis in range(3)]
      all_voxels = np.ones(array(source / "field_aff.nii.gz").shape, dtype=bool)
      fields = {kind: checked_difference(OUT / f"coef_{name}_field_{kind}.nii.gz", source / f"field_{kind}.nii.gz", all_voxels, 2e-5, f"{name} field {kind}") for kind in ("noaff", "aff")}
      jacobian = array(OUT / f"coef_{name}_jac.nii.gz")
      if not np.isfinite(jacobian).all() or np.allclose(jacobian, 0):
        raise RuntimeError(f"{name}: invalid FSL6 Jacobian")
      comparisons["coefficients"][name] = {"coordinates": coordinates, "fields": fields, "jacobian_nonzero_voxels": int(np.count_nonzero(jacobian))}

    for entry in dense_cases():
      name, representation = entry["id"], entry["representation"]
      source = DENSE / name
      names = ("source.nii.gz", "target.nii.gz", "warp.nii.gz")
      copy_case(source, work, names)
      for file in names:
        inputs[relative(source / file)] = digest(source / file)
      mode = "--rel" if representation == "relative" else "--abs"
      for axis in range(3):
        receipts.append(run(args.image, work, f"applywarp --in=/input/{name}/coord{axis}.nii.gz --ref=/input/{name}/target.nii.gz --warp=/input/{name}/warp.nii.gz --out=/out/dense_{name}_coord{axis}.nii.gz --interp=trilinear {mode}"))
      receipts.append(run(args.image, work, f"applywarp --in=/input/{name}/support.nii.gz --ref=/input/{name}/target.nii.gz --warp=/input/{name}/warp.nii.gz --out=/out/dense_{name}_support.nii.gz --interp=trilinear {mode}"))
      support = (array(source / "native_support.nii.gz") >= .999) & (array(OUT / f"dense_{name}_support.nii.gz") >= .999)
      if int(support.sum()) < 100:
        raise RuntimeError(f"{name}: only {int(support.sum())} shared supported voxels")
      comparisons["dense"][name] = [checked_difference(OUT / f"dense_{name}_coord{axis}.nii.gz", source / f"native_coord{axis}.nii.gz", support, 2e-5, f"{name} coordinate {axis}") for axis in range(3)]

    # The original source/ref geometries differ: a small FSL-scaled displacement is not a small RAS map.
    # Retain that inverse, and add a distinct control with coincident source/ref geometry below.
    inverse_dense = DENSE / "left_left_relative"
    inputs[relative(inverse_dense / "warp.nii.gz")] = digest(inverse_dense / "warp.nii.gz")
    receipts.append(run(args.image, work, "invwarp --warp=/input/left_left_relative/warp.nii.gz --ref=/input/left_left_relative/source.nii.gz --out=/out/invwarp_dense_left_left_relative.nii.gz"))
    receipts.append(run(args.image, work, "invwarp --warp=/input/left_left_relative/warp.nii.gz --ref=/input/left_left_relative/target.nii.gz --out=/out/invwarp_dense_matched_geometry.nii.gz"))
    comparisons["inverse_control"] = {"field": relative(inverse_dense / "warp.nii.gz"), "source_and_reference_geometry": relative(inverse_dense / "target.nii.gz"), "inverse": "invwarp_dense_matched_geometry.nii.gz", "policy": "distinct synthetic control with coincident source/ref geometry, not the original registration"}

    inverse_case = COEFFICIENTS / "srcleft_refright_aff"
    shutil.copy2(inverse_case / "field_aff.nii.gz", work / "srcleft_refright_aff" / "field_aff.nii.gz")
    inputs[relative(inverse_case / "field_aff.nii.gz")] = digest(inverse_case / "field_aff.nii.gz")
    receipts.append(run(args.image, work, "invwarp --warp=/input/srcleft_refright_aff/field_aff.nii.gz --ref=/input/srcleft_refright_aff/source.nii.gz --out=/out/invwarp_srcleft_refright_aff.nii.gz"))

    flirt = []
    for key, source, reference, matrix in flirt_cases():
      folder = work / f"flirt_{key}"
      folder.mkdir()
      for original, copied in ((source, "source.nii"), (reference, "reference.nii"), (matrix, "matrix.mat")):
        shutil.copy2(original, folder / copied)
        inputs[relative(original)] = digest(original)
      probes = voxel_probes(source)
      np.savetxt(folder / "input.tsv", probes, fmt="%.9f")
      receipts.append(run(args.image, work, f"img2imgcoord -src /input/flirt_{key}/source.nii -dest /input/flirt_{key}/reference.nii -xfm /input/flirt_{key}/matrix.mat -vox < /input/flirt_{key}/input.tsv > /out/flirt_{key}.tsv"))
      flirt.append({"id": key, "input_voxels": probes.tolist(), "output": f"flirt_{key}.tsv", "convention": "img2imgcoord -vox: source voxel indices to reference voxel indices using FSL-selected q/sform geometry"})

  fixtures = {path.name: digest(path) for path in sorted(OUT.iterdir()) if path.name != "manifest.json"}
  manifest = {"kind": "native-oracle", "image": args.image, "image_id": image_id(args.image), "platform": "linux/amd64", "limits": {"cpus": 2, "memory": "1g", "threads": 1, "network": "none", "timeout_seconds": 300}, "package_lock": {"name": "fsl6-package-lock.json", "sha256": digest(OUT / "fsl6-package-lock.json")}, "input_sha256": inputs, "fixture_sha256": fixtures, "commands": receipts, "comparisons": comparisons, "flirt": flirt, "notes": "Coordinate ramps and support=ones are generated from each source NIfTI affine at runtime. Comparisons require finite values and shared FSL5/FSL6 support; command exit alone is not acceptance."}
  (OUT / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")


if __name__ == "__main__":
  main()
