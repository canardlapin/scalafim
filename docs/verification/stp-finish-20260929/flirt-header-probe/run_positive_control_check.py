from __future__ import annotations

import json
import hashlib
import subprocess
from pathlib import Path

import nibabel as nib
import numpy as np


ROOT = Path('/probe')
OUT = ROOT / 'results'
MATRIX = np.loadtxt(ROOT / 'flirt_0_to_1.mat')


def header(image: nib.Nifti1Image) -> dict:
  q, qcode = image.get_qform(coded=True)
  s, scode = image.get_sform(coded=True)
  return {
    'shape': [int(value) for value in image.shape],
    'zooms': [float(value) for value in image.header.get_zooms()],
    'qform_code': int(qcode), 'sform_code': int(scode),
    'qform': None if q is None else np.asarray(q).tolist(),
    'sform': None if s is None else np.asarray(s).tolist(),
    'best_affine': np.asarray(image.header.get_best_affine()).tolist(),
  }


def sha256(path: Path) -> str:
  digest = hashlib.sha256()
  with path.open('rb') as handle:
    for chunk in iter(lambda: handle.read(1024 * 1024), b''):
      digest.update(chunk)
  return digest.hexdigest()


def preserve_forms(image: nib.Nifti1Image, data: np.ndarray) -> nib.Nifti1Image:
  result = nib.Nifti1Image(data, None, header=image.header.copy())
  q, qcode = image.get_qform(coded=True)
  s, scode = image.get_sform(coded=True)
  result.set_qform(q, int(qcode))
  result.set_sform(s, int(scode))
  return result


def p_fsl(image: nib.Nifti1Image) -> np.ndarray:
  """Stored voxel axes to FSL scaled-voxel frame for this positive control."""
  sform, scode = image.get_sform(coded=True)
  assert scode != 0 and np.linalg.det(sform[:3, :3]) > 0.0
  pix = np.asarray(image.header.get_zooms()[:3], dtype=float)
  p = np.eye(4)
  p[0, 0], p[1, 1], p[2, 2] = -pix[0], pix[1], pix[2]
  p[0, 3] = pix[0] * (image.shape[0] - 1)
  return p


def tri(data: np.ndarray, point: np.ndarray) -> float:
  if np.any(point < 0.0) or np.any(point > np.asarray(data.shape) - 1.0):
    return float('nan')
  lo = np.floor(point).astype(int)
  hi = np.minimum(lo + 1, np.asarray(data.shape) - 1)
  frac = point - lo
  result = 0.0
  for dx in (0, 1):
    for dy in (0, 1):
      for dz in (0, 1):
        weight = (frac[0] if dx else 1.0 - frac[0]) * (frac[1] if dy else 1.0 - frac[1]) * (frac[2] if dz else 1.0 - frac[2])
        result += weight * float(data[(hi[0] if dx else lo[0], hi[1] if dy else lo[1], hi[2] if dz else lo[2])])
  return result


def run(name: str, argv: list[str]) -> dict:
  completed = subprocess.run(argv, capture_output=True, text=True, check=False)
  record = {'name': name, 'argv': argv, 'returncode': completed.returncode, 'stdout': completed.stdout, 'stderr': completed.stderr}
  (OUT / f'{name}.json').write_text(json.dumps(record, indent=2) + '\n')
  if completed.returncode:
    raise RuntimeError(record)
  return record


def main() -> None:
  source = nib.load(str(ROOT / 'consistent_positive_source.nii'))
  reference = nib.load(str(ROOT / 'consistent_positive_reference.nii'))
  axes = np.indices(source.shape[:3], dtype=np.float32)
  inputs, outputs, commands = [], [], []
  for index, label in enumerate(('x', 'y', 'z')):
    input_path = ROOT / f'positive_axis_{label}.nii'
    output_path = ROOT / f'applyxfm_positive_axis_{label}.nii'
    nib.save(preserve_forms(source, axes[index]), str(input_path))
    inputs.append(input_path)
    commands.append(run(f'flirt_applyxfm_positive_axis_{label}', [
      '/opt/fsl6/bin/flirt', '-in', str(input_path), '-ref', str(ROOT / 'consistent_positive_reference.nii'),
      '-applyxfm', '-init', str(ROOT / 'flirt_0_to_1.mat'), '-interp', 'trilinear', '-out', str(output_path),
    ]))
    outputs.append(output_path)

  output_images = [nib.load(str(path)) for path in outputs]
  output_data = [np.asanyarray(image.dataobj, dtype=float) for image in output_images]
  psrc = p_fsl(source)
  pref = p_fsl(reference)
  pullback = np.linalg.inv(psrc) @ np.linalg.inv(MATRIX) @ pref
  forward = np.linalg.inv(pullback)

  # Start with analytical source voxels safely away from every source boundary;
  # retain only targets safely interior to the *stored output array* as well.
  interior = np.indices(source.shape[:3]).reshape(3, -1).T
  interior = interior[np.all((interior >= 1) & (interior <= np.asarray(source.shape[:3]) - 2), axis=1)]
  target = (forward @ np.c_[interior, np.ones(len(interior))].T).T[:, :3]
  target_safe = np.all((target >= 1.0) & (target <= np.asarray(output_data[0].shape) - 2.0), axis=1)
  retained_source, retained_target = interior[target_safe], target[target_safe]
  recovered = np.array([[tri(data, voxel) for data in output_data] for voxel in retained_target])
  errors = np.abs(recovered - retained_source)

  # A separate calculation tests the only possible storage-axis concern: use
  # the raw output header in place of the reference header. It is diagnostic;
  # the mandated primary equation remains `pullback` above.
  pout = p_fsl(output_images[0])
  raw_output_pullback = np.linalg.inv(psrc) @ np.linalg.inv(MATRIX) @ pout
  raw_output_forward = np.linalg.inv(raw_output_pullback)
  raw_target = (raw_output_forward @ np.c_[interior, np.ones(len(interior))].T).T[:, :3]
  raw_safe = np.all((raw_target >= 1.0) & (raw_target <= np.asarray(output_data[0].shape) - 2.0), axis=1)
  raw_recovered = np.array([[tri(data, voxel) for data in output_data] for voxel in raw_target[raw_safe]])
  raw_errors = np.abs(raw_recovered - interior[raw_safe])

  source_headers_preserved = [header(nib.load(str(path))) == header(source) for path in inputs]
  result = {
    'control': 'consistent_positive_q_equals_s_at_both_endpoints',
    'commands': commands,
    'input_sha256': {path.name: sha256(path) for path in inputs},
    'output_sha256': {path.name: sha256(path) for path in outputs},
    'input_headers': {path.name: header(nib.load(str(path))) for path in inputs},
    'source_header': header(source),
    'reference_header': header(reference),
    'raw_output_headers': {path.name: header(image) for path, image in zip(outputs, output_images)},
    'input_headers_exactly_preserved': source_headers_preserved,
    'Psrc_stored_voxel_to_fsl': psrc.tolist(),
    'Pref_stored_voxel_to_fsl': pref.tolist(),
    'pullback_source_voxel_equals_Psrc_inverse_F_inverse_Pref_target_voxel': pullback.tolist(),
    'forward_target_voxel': forward.tolist(),
    'analytic_source_interior_count': int(len(interior)),
    'target_safely_interior_count': int(len(retained_source)),
    'primary_stored_axis_errors': {
      'finite_triplets': int(np.count_nonzero(np.all(np.isfinite(recovered), axis=1))),
      'max_abs_voxels': None if not len(errors) else float(np.max(errors)),
      'mean_abs_voxels': None if not len(errors) else float(np.mean(errors)),
      'per_axis_max_abs_voxels': None if not len(errors) else np.max(errors, axis=0).tolist(),
    },
    'raw_output_header_differs_from_reference': header(output_images[0]) != header(reference),
    'raw_output_header_diagnostic': {
      'target_safely_interior_count': int(np.count_nonzero(raw_safe)),
      'max_abs_voxels': None if not len(raw_errors) else float(np.max(raw_errors)),
      'mean_abs_voxels': None if not len(raw_errors) else float(np.mean(raw_errors)),
      'per_axis_max_abs_voxels': None if not len(raw_errors) else np.max(raw_errors, axis=0).tolist(),
    },
  }
  (OUT / 'positive_control_applyxfm_equation.json').write_text(json.dumps(result, indent=2) + '\n')
  print(json.dumps(result['primary_stored_axis_errors'], indent=2))


if __name__ == '__main__':
  main()
