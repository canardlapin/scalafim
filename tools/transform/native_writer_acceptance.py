#!/usr/bin/env python3
"""Native acceptance for immutable transform writer goldens."""
from __future__ import annotations
import argparse, hashlib, json, math, re, shlex, subprocess, tempfile
from pathlib import Path
from typing import Any
import nibabel as nib
import numpy as np

TOOLS = ("ANTs", "FSL", "AFNI", "FreeSurfer")
TOLERANCE_MM = 2e-4
MINIMUM_SUPPORTED_POINTS = 8
IMAGE_RE = re.compile(r"(?:.+@sha256:|sha256:)[0-9a-f]{64}$")

def sha256(path: Path) -> str:
  return hashlib.sha256(path.read_bytes()).hexdigest()

def parse_images(values: list[str]) -> dict[str, str]:
  result = {}
  for value in values:
    if "=" not in value: raise ValueError("--image must be TOOL=immutable-reference")
    requested, image = value.split("=", 1)
    tool = next((x for x in TOOLS if x.lower() == requested.lower()), None)
    if tool is None or tool in result or not IMAGE_RE.fullmatch(image):
      raise ValueError(f"invalid or duplicate immutable image {value!r}")
    result[tool] = image
  return result

def geometry(path: Path) -> dict[str, Any]:
  image = nib.load(str(path))
  return {"shape": list(map(int, image.shape[:3])), "affine": np.asarray(image.affine).round(12).tolist()}

def hashes(goldens: Path, fixtures: Path) -> dict[str, str]:
  return {key: sha256(path) for key, path in {
    "writer_geometry/movable.nii": fixtures / "movable.nii",
    "writer_geometry/reference.nii": fixtures / "reference.nii",
    "writer_geometry/manifest.json": fixtures / "manifest.json",
    "pullback.json": goldens / "pullback.json",
  }.items()}

def pullback(goldens: Path) -> np.ndarray:
  result = np.asarray(json.loads((goldens / "pullback.json").read_text())["pullback"], dtype=float).reshape(4, 4)
  if not np.isfinite(result).all(): raise ValueError("pullback.json contains non-finite values")
  return result

def write_ramps(movable: Path, work: Path) -> None:
  image = nib.load(str(movable)); shape = image.shape[:3]
  ijk = np.stack(np.meshgrid(*(np.arange(n) for n in shape), indexing="ij"), axis=-1)
  ras = ijk @ image.affine[:3, :3].T + image.affine[:3, 3]
  for axis, name in enumerate(("x", "y", "z")):
    nib.save(nib.Nifti1Image(ras[..., axis].astype(np.float32), image.affine, image.header), str(work / f"ramp_{name}.nii.gz"))
  support = np.zeros(shape, np.float32); support[2:-2, 2:-2, 2:-2] = 1
  nib.save(nib.Nifti1Image(support, image.affine, image.header), str(work / "support.nii.gz"))

def field_reference(field: Path, work: Path) -> Path:
  image = nib.load(str(field)); output = work / f"reference_{field.stem.replace('.', '_')}.nii.gz"
  header = image.header.copy()
  header.set_intent("none")
  nib.save(nib.Nifti1Image(np.zeros(image.shape[:3], np.float32), image.affine, header), str(output))
  return output

def queries(reference: Path, matrix: np.ndarray, movable: Path) -> tuple[np.ndarray, np.ndarray, int]:
  target, source = nib.load(str(reference)), nib.load(str(movable))
  axes = [np.arange(1, n - 1, dtype=int) for n in target.shape[:3]]
  index = np.array(np.meshgrid(*axes, indexing="ij")).reshape(3, -1).T
  ras = index @ target.affine[:3, :3].T + target.affine[:3, 3]
  expected = (np.c_[ras, np.ones(len(ras))] @ matrix.T)[:, :3]
  source_index = np.c_[expected, np.ones(len(expected))] @ np.linalg.inv(source.affine).T
  valid = np.all(source_index[:, :3] >= 2, axis=1) & np.all(source_index[:, :3] <= np.asarray(source.shape[:3]) - 3, axis=1)
  return index[valid], expected[valid], int(valid.sum())

def redact(value: str, license_path: Path | None) -> str:
  return value if license_path is None else value.replace(str(license_path), "<redacted-freesurfer-license>")

def run(image: str, goldens: Path, fixtures: Path, work: Path, command: list[str], license_path: Path | None) -> dict[str, Any]:
  invocation = ["docker", "run", "--rm", "--network", "none", "--platform", "linux/amd64", "--cpus", "2", "--memory", "1g", "-e", "OMP_NUM_THREADS=1", "-e", "ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1", "-e", "FSLOUTPUTTYPE=NIFTI_GZ", "-v", f"{goldens}:/goldens:ro", "-v", f"{fixtures}:/fixtures:ro", "-v", f"{work}:/work", "--entrypoint", "/bin/sh"]
  if license_path is not None: invocation += ["-v", f"{license_path}:/license/license.txt:ro", "-e", "FS_LICENSE=/license/license.txt"]
  invocation += [image, "-ec", "export PATH=/opt/fsl6/bin:$PATH; " + shlex.join(command)]
  try:
    done = subprocess.run(invocation, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=180)
    code, output = done.returncode, done.stdout[-4000:]
  except subprocess.TimeoutExpired as error:
    raw = error.stdout.decode() if isinstance(error.stdout, bytes) else (error.stdout or "")
    code, output = 124, raw[-4000:] + chr(10) + "container timed out after 180 seconds"
  return {"command": [redact(x, license_path) for x in invocation], "returncode": code, "output": redact(output, license_path)}

def image_id(image: str) -> str | None:
  done = subprocess.run(["docker", "image", "inspect", "--format", "{{.Id}}", image], text=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
  return done.stdout.strip() if done.returncode == 0 and done.stdout.strip() else None

def commands(tool: str, transform: str, prefix: str, reference: Path, field: bool) -> list[str]:
  ref = f"/work/{reference.name}" if field else "/fixtures/reference.nii"
  result = []
  for axis in ("x", "y", "z", "support"):
    source, output = (f"/work/ramp_{axis}.nii.gz" if axis != "support" else "/work/support.nii.gz"), f"/work/{prefix}_{axis}.nii.gz"
    if tool == "ANTs": result.append(f"antsApplyTransforms -d 3 -i {source} -r {ref} -o {output} -n Linear -t /goldens/{transform}")
    elif tool == "FSL" and transform == "flirt.mat": result.append(f"flirt -in {source} -ref {ref} -applyxfm -init /goldens/flirt.mat -interp trilinear -out {output} -paddingsize 0")
    elif tool == "FSL": result.append(f"applywarp --in={source} --ref={ref} --warp=/goldens/{transform} --out={output} --interp=trilinear --rel")
    elif tool == "AFNI": result.append(f"3dAllineate -overwrite -input {source} -master {ref} -1Dmatrix_apply /goldens/{transform} -final linear -prefix {output}")
    else:
      lta = transform if transform.startswith("/") else f"/goldens/{transform}"
      result.append(f"mri_vol2vol --mov {source} --targ {ref} --lta {lta} --o {output} --interp trilin")
  return result

def outputs(work: Path, prefix: str) -> dict[str, str]:
  return {path.name: sha256(path) for axis in ("x", "y", "z", "support") if (path := work / f"{prefix}_{axis}.nii.gz").is_file()}

def compare(work: Path, prefix: str, index: np.ndarray, expected: np.ndarray, count: int) -> tuple[str, dict[str, Any]]:
  base = {"sample_count": count, "analytically_valid_query_count": count}
  if count < MINIMUM_SUPPORTED_POINTS: return "fail", {**base, "reason": f"only {count} analytically valid queries; require {MINIMUM_SUPPORTED_POINTS}", "max_mm_error": None}
  try:
    observed = np.stack([nib.load(str(work / f"{prefix}_{axis}.nii.gz")).get_fdata(dtype=np.float64)[tuple(index.T)] for axis in ("x", "y", "z")], axis=1)
    support = nib.load(str(work / f"{prefix}_support.nii.gz")).get_fdata(dtype=np.float64)[tuple(index.T)]
  except Exception as error: return "fail", {**base, "reason": f"could not read native outputs: {error}", "max_mm_error": None}
  if not np.isfinite(observed).all(): return "fail", {**base, "reason": "non-finite coordinate-ramp value at an analytically valid query", "max_mm_error": None}
  if not np.isfinite(support).all() or np.any(support < .999): return "fail", {**base, "reason": "native support below 0.999 at an analytically valid query", "max_mm_error": None}
  maximum = float(np.linalg.norm(observed - expected, axis=1).max())
  return ("pass" if math.isfinite(maximum) and maximum <= TOLERANCE_MM else "fail"), {**base, "max_mm_error": maximum, "tolerance_mm": TOLERANCE_MM}

def base(tool: str, image: str, resolved: str | None, version: dict[str, Any], goldens: Path, fixtures: Path, transform: str, reference: Path) -> dict[str, Any]:
  return {"tool": tool, "image": image, "image_id": resolved, "golden_sha256": sha256(goldens / transform), "input_hashes": hashes(goldens, fixtures), "reference_geometry": geometry(reference), "version_command": version["command"], "version_returncode": version["returncode"], "version_output": version["output"]}

def check(name: str, tool: str, transform: str, image: str, resolved: str | None, version: dict[str, Any], goldens: Path, fixtures: Path, work: Path, reference: Path, field: bool, matrix: np.ndarray, license_path: Path | None) -> dict[str, Any]:
  prefix = re.sub(r"[^A-Za-z0-9]", "_", name); index, expected, count = queries(reference, matrix, fixtures / "movable.nii")
  record = base(tool, image, resolved, version, goldens, fixtures, transform, reference)
  if count >= MINIMUM_SUPPORTED_POINTS:
    execution = run(image, goldens, fixtures, work, ["/bin/sh", "-ec", "\n".join(commands(tool, transform, prefix, reference, field))], license_path)
    record.update(commands=[execution["command"]], command_returncodes=[execution["returncode"]])
    if execution["returncode"] != 0: return {**record, "status": "fail", "reason": "native command failed", "command_output": execution["output"], "sample_count": 0, "analytically_valid_query_count": count, "max_mm_error": None, "output_hashes": outputs(work, prefix)}
  status, comparison = compare(work, prefix, index, expected, count)
  return {**record, "status": status, **comparison, "output_hashes": outputs(work, prefix)}

def pending(tool: str, reason: str) -> dict[str, Any]:
  return {"tool": tool, "status": "pending-native", "reason": reason, "sample_count": None, "analytically_valid_query_count": None, "max_mm_error": None}

def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__); parser.add_argument("goldens_dir", type=Path); parser.add_argument("--image", action="append", default=[], metavar="TOOL=IMAGE"); parser.add_argument("--license", type=Path)
  args = parser.parse_args()
  try: images = parse_images(args.image)
  except ValueError as error: parser.error(str(error))
  goldens, fixtures = args.goldens_dir.resolve(), args.goldens_dir.resolve().parent / "writer_geometry"
  if not goldens.is_dir() or not fixtures.is_dir(): parser.error("goldens-dir must have sibling writer_geometry source fixtures")
  if args.license is not None and not args.license.is_file(): parser.error("--license must name a readable file")
  receipt_path = goldens / "receipt.json"; receipt = json.loads(receipt_path.read_text()); native: dict[str, Any] = {}; failed = False
  plan = {"ANTs": [("affine.tfm", "affine.tfm"), ("affine.mat", "affine.mat"), ("field_1Warp.nii", "field_1Warp.nii")], "FSL": [("flirt.mat", "flirt.mat"), ("field_fnirt_relative.nii", "field_fnirt_relative.nii")], "AFNI": [("affine.aff12.1D", "affine.aff12.1D")], "FreeSurfer": [("affine.lta", "affine.lta")]}
  versions = {"ANTs": ["antsRegistration", "--version"], "FSL": ["flirt", "-version"], "AFNI": ["afni", "-ver"], "FreeSurfer": ["mri_vol2vol", "--version"]}
  with tempfile.TemporaryDirectory(prefix="scalafim-native-writer-") as tmp:
    work = Path(tmp); write_ramps(fixtures / "movable.nii", work); matrix = pullback(goldens)
    for tool, items in plan.items():
      if tool not in images or (tool == "FreeSurfer" and args.license is None):
        why = "no immutable image requested" if tool not in images else "FreeSurfer image requested but no --license supplied; container was not executed"
        for name, _ in items: native[name] = pending(tool, why)
        if tool == "FreeSurfer":
          native["register.dat"] = pending(tool, why); native["talairach.xfm"] = pending(tool, why)
        continue
      resolved, version = image_id(images[tool]), run(images[tool], goldens, fixtures, work, versions[tool], args.license)
      for name, transform in items:
        field = transform.startswith("field_"); reference = field_reference(goldens / transform, work) if field else fixtures / "reference.nii"
        if version["returncode"] != 0:
          native[name] = {**base(tool, images[tool], resolved, version, goldens, fixtures, transform, reference), "status": "fail", "reason": "native version command failed", "sample_count": 0, "analytically_valid_query_count": None, "max_mm_error": None, "output_hashes": {}}
        else: native[name] = check(name, tool, transform, images[tool], resolved, version, goldens, fixtures, work, reference, field, matrix, args.license)
        failed |= native[name]["status"] != "pass"
      if tool == "FreeSurfer" and version["returncode"] == 0:
        for name, flag in (("register.dat", "--inreg"), ("talairach.xfm", "--inxfm")):
          prefix = re.sub(r"[^A-Za-z0-9]", "_", name) + "_converted"; reference = fixtures / "reference.nii"; index, expected, count = queries(reference, matrix, fixtures / "movable.nii")
          record = base("FreeSurfer lta_convert + mri_vol2vol", images[tool], resolved, version, goldens, fixtures, name, reference)
          conversion = ["lta_convert", flag, f"/goldens/{name}", "--outlta", f"/work/{prefix}.lta", "--src", "/fixtures/movable.nii", "--trg", "/fixtures/reference.nii"]
          execution = run(images[tool], goldens, fixtures, work, ["/bin/sh", "-ec", shlex.join(conversion) + "\n" + "\n".join(commands("FreeSurfer", f"/work/{prefix}.lta", prefix, reference, False))], args.license)
          if execution["returncode"] != 0: native[name] = {**record, "commands": [execution["command"]], "command_returncodes": [execution["returncode"]], "status": "fail", "reason": "lta conversion or converted-LTA native resampling failed", "command_output": execution["output"], "sample_count": 0, "analytically_valid_query_count": count, "max_mm_error": None, "output_hashes": outputs(work, prefix)}
          else:
            status, comparison = compare(work, prefix, index, expected, count)
            native[name] = {**record, "commands": [execution["command"]], "command_returncodes": [execution["returncode"]], "status": status, **comparison, "converted_lta_sha256": sha256(work / f"{prefix}.lta"), "output_hashes": outputs(work, prefix)}
          failed |= native[name]["status"] != "pass"
      elif tool == "FreeSurfer":
        for name in ("register.dat", "talairach.xfm"):
          native[name] = {**base(tool, images[tool], resolved, version, goldens, fixtures, name, fixtures / "reference.nii"), "status": "fail", "reason": "native version command failed", "sample_count": 0, "analytically_valid_query_count": None, "max_mm_error": None, "output_hashes": {}}
          failed = True
  receipt["native_checks"] = native
  receipt["native_harness"] = {"tolerance_mm": TOLERANCE_MM, "minimum_supported_points": MINIMUM_SUPPORTED_POINTS, "requested_images": images, "input_hashes": hashes(goldens, fixtures)}
  receipt_path.write_text(json.dumps(receipt, indent=1) + chr(10))
  print(json.dumps({name: item["status"] for name, item in sorted(native.items())}, indent=1))
  return 1 if failed else 0
if __name__ == "__main__": raise SystemExit(main())
