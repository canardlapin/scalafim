from __future__ import annotations

import hashlib
import json
import os
import platform
import shutil
import subprocess
from pathlib import Path

import nibabel as nib
import numpy as np


ROOT = Path('/probe')
ORACLE = Path('/oracle')
OUT = ROOT / 'results'
OUT.mkdir(exist_ok=True)


def sha256(path: Path) -> str:
  digest = hashlib.sha256()
  with path.open('rb') as handle:
    for chunk in iter(lambda: handle.read(1024 * 1024), b''):
      digest.update(chunk)
  return digest.hexdigest()


def affine_record(image: nib.Nifti1Image) -> dict:
  qform, qcode = image.get_qform(coded=True)
  sform, scode = image.get_sform(coded=True)
  return {
    'shape': [int(value) for value in image.shape],
    'zooms': [float(value) for value in image.header.get_zooms()],
    'qform_code': int(qcode),
    'sform_code': int(scode),
    'qform': None if qform is None else np.asarray(qform).tolist(),
    'sform': None if sform is None else np.asarray(sform).tolist(),
    'best_affine': np.asarray(image.header.get_best_affine()).tolist(),
  }


def preserve_forms(image: nib.Nifti1Image, data: np.ndarray) -> nib.Nifti1Image:
  """Replace data without allowing Nibabel's constructor to choose a form."""
  header = image.header.copy()
  result = nib.Nifti1Image(data, None, header=header)
  qform, qcode = image.get_qform(coded=True)
  sform, scode = image.get_sform(coded=True)
  result.set_qform(qform, int(qcode))
  result.set_sform(sform, int(scode))
  return result


def derived(image: nib.Nifti1Image, form: np.ndarray | None, code: int) -> nib.Nifti1Image:
  result = preserve_forms(image, np.asanyarray(image.dataobj))
  result.set_qform(form, code)
  result.set_sform(form, code)
  return result


def write_image(path: Path, image: nib.Nifti1Image) -> None:
  nib.save(image, str(path))


def command(name: str, argv: list[str]) -> dict:
  completed = subprocess.run(argv, text=True, capture_output=True, check=False)
  record = {
    'name': name,
    'argv': argv,
    'returncode': completed.returncode,
    'stdout': completed.stdout,
    'stderr': completed.stderr,
  }
  (OUT / f'{name}.json').write_text(json.dumps(record, indent=2) + '\n')
  return record


def fsl_vox_to_scaled(image: nib.Nifti1Image, form: np.ndarray | None, flip_by_det: bool) -> np.ndarray:
  # FSL's scaled voxel frame. The selected-S spelling flips X only for a
  # positive-handed header; the native-LR fallback deliberately never flips.
  pix = np.asarray(image.header.get_zooms()[:3], dtype=float)
  mat = np.eye(4)
  mat[0, 0], mat[1, 1], mat[2, 2] = pix
  if flip_by_det and np.linalg.det(form[:3, :3]) > 0.0:
    mat[0, 0] = -pix[0]
    mat[0, 3] = pix[0] * (image.shape[0] - 1)
  return mat


def trilinear(data: np.ndarray, point: np.ndarray) -> float:
  x, y, z = point
  if x < 0 or y < 0 or z < 0 or x > data.shape[0] - 1 or y > data.shape[1] - 1 or z > data.shape[2] - 1:
    return float('nan')
  lo = np.floor(point).astype(int)
  hi = np.minimum(lo + 1, np.asarray(data.shape) - 1)
  frac = point - lo
  value = 0.0
  for dx in (0, 1):
    for dy in (0, 1):
      for dz in (0, 1):
        weight = (frac[0] if dx else 1 - frac[0]) * (frac[1] if dy else 1 - frac[1]) * (frac[2] if dz else 1 - frac[2])
        index = (hi[0] if dx else lo[0], hi[1] if dy else lo[1], hi[2] if dz else lo[2])
        value += weight * float(data[index])
  return value


def main() -> None:
  tool_commands = [
    command('tool_flirt_version', ['/opt/fsl6/bin/flirt', '-version']),
    command('tool_fslversion', ['/opt/fsl6/bin/fslversion']),
    command('tool_img2imgcoord_help', ['/opt/fsl6/bin/img2imgcoord', '-help']),
  ]
  source_path = ROOT / 'original_source.nii'
  reference_path = ROOT / 'original_reference.nii'
  matrix_path = ROOT / 'flirt_0_to_1.mat'
  shutil.copyfile(ORACLE / 'fsl_case0.nii', source_path)
  shutil.copyfile(ORACLE / 'fsl_case1.nii', reference_path)
  shutil.copyfile(ORACLE / 'flirt_0_to_1.mat', matrix_path)
  source = nib.load(str(source_path))
  reference = nib.load(str(reference_path))
  source_q, source_qcode = source.get_qform(coded=True)
  source_s, source_scode = source.get_sform(coded=True)
  reference_q, reference_qcode = reference.get_qform(coded=True)
  reference_s, reference_scode = reference.get_sform(coded=True)

  # The controls choose forms separately for each image so both members of a
  # pair have the stated handedness. Original files remain byte-for-byte copies.
  controls = {
    'consistent_positive': (source_s, source_scode, reference_q, reference_qcode),
    'consistent_negative': (source_q, source_qcode, reference_s, reference_scode),
    'no_forms': (None, 0, None, 0),
  }
  paths: dict[str, tuple[Path, Path]] = {'original_conflict': (source_path, reference_path)}
  for name, (src_form, src_code, ref_form, ref_code) in controls.items():
    src_out = ROOT / f'{name}_source.nii'
    ref_out = ROOT / f'{name}_reference.nii'
    write_image(src_out, derived(source, src_form, int(src_code)))
    write_image(ref_out, derived(reference, ref_form, int(ref_code)))
    paths[name] = (src_out, ref_out)

  coordinate_path = ROOT / 'source_voxel_zero.txt'
  coordinate_path.write_text('0 0 0\n')
  probe_specs = [
    ('01_original_conflict_default', 'original_conflict', []),
    ('02_original_conflict_flirt', 'original_conflict', ['-flirt']),
    ('03_consistent_positive_default', 'consistent_positive', []),
    ('04_consistent_positive_flirt', 'consistent_positive', ['-flirt']),
    ('05_consistent_negative_default', 'consistent_negative', []),
    ('06_consistent_negative_flirt', 'consistent_negative', ['-flirt']),
    ('07_no_forms_default', 'no_forms', []),
  ]
  cli_results = []
  for name, pair, extra in probe_specs:
    src, ref = paths[pair]
    cli_results.append(command(name, [
      '/opt/fsl6/bin/img2imgcoord', '-src', str(src), '-dest', str(ref), '-xfm', str(matrix_path), '-vox', *extra, str(coordinate_path)
    ]))

  # Build source S-form world coordinate ramps and a support field. Forms are
  # restored explicitly after construction, including the original conflict.
  grid = np.indices(source.shape[:3], dtype=float)
  ijk = np.stack([grid[0], grid[1], grid[2], np.ones(source.shape[:3])], axis=-1)
  source_world = ijk @ source_s.T
  ramps = []
  for axis, label in enumerate(('x', 'y', 'z')):
    ramp_path = ROOT / f'original_conflict_world_{label}.nii'
    write_image(ramp_path, preserve_forms(source, source_world[..., axis].astype(np.float32)))
    ramps.append(ramp_path)
  support_path = ROOT / 'original_conflict_support.nii'
  write_image(support_path, preserve_forms(source, np.ones(source.shape[:3], dtype=np.float32)))

  def same_form(left: nib.Nifti1Image, right: nib.Nifti1Image) -> bool:
    left_q, left_qcode = left.get_qform(coded=True)
    left_s, left_scode = left.get_sform(coded=True)
    right_q, right_qcode = right.get_qform(coded=True)
    right_s, right_scode = right.get_sform(coded=True)
    return int(left_qcode) == int(right_qcode) and int(left_scode) == int(right_scode) and np.array_equal(left_q, right_q) and np.array_equal(left_s, right_s)

  outputs = []
  for ramp in ramps + [support_path]:
    output = ROOT / f'applyxfm_{ramp.stem}.nii'
    result = command(f'flirt_applyxfm_{ramp.stem}', [
      '/opt/fsl6/bin/flirt', '-in', str(ramp), '-ref', str(reference_path), '-applyxfm', '-init', str(matrix_path), '-interp', 'trilinear', '-out', str(output)
    ])
    if result['returncode'] != 0:
      raise RuntimeError(f'flirt failed for {ramp.name}: {result["stderr"]}')
    outputs.append(output)

  # Repeat the same ramp/support application for the consistent-positive
  # header control. This is an independent native run, not a synthesized
  # interpretation of img2imgcoord's output.
  positive_source_path, positive_reference_path = paths['consistent_positive']
  positive_source = nib.load(str(positive_source_path))
  positive_s, _ = positive_source.get_sform(coded=True)
  positive_grid = np.indices(positive_source.shape[:3], dtype=float)
  positive_ijk = np.stack([positive_grid[0], positive_grid[1], positive_grid[2], np.ones(positive_source.shape[:3])], axis=-1)
  positive_world = positive_ijk @ positive_s.T
  positive_outputs = []
  for axis, label in enumerate(('x', 'y', 'z')):
    ramp_path = ROOT / f'consistent_positive_world_{label}.nii'
    write_image(ramp_path, preserve_forms(positive_source, positive_world[..., axis].astype(np.float32)))
    output = ROOT / f'applyxfm_{ramp_path.stem}.nii'
    result = command(f'flirt_applyxfm_{ramp_path.stem}', [
      '/opt/fsl6/bin/flirt', '-in', str(ramp_path), '-ref', str(positive_reference_path), '-applyxfm', '-init', str(matrix_path), '-interp', 'trilinear', '-out', str(output)
    ])
    if result['returncode'] != 0:
      raise RuntimeError(f'flirt failed for {ramp_path.name}: {result["stderr"]}')
    positive_outputs.append(output)
  positive_support_path = ROOT / 'consistent_positive_support.nii'
  write_image(positive_support_path, preserve_forms(positive_source, np.ones(positive_source.shape[:3], dtype=np.float32)))
  positive_support_output = ROOT / f'applyxfm_{positive_support_path.stem}.nii'
  result = command(f'flirt_applyxfm_{positive_support_path.stem}', [
    '/opt/fsl6/bin/flirt', '-in', str(positive_support_path), '-ref', str(positive_reference_path), '-applyxfm', '-init', str(matrix_path), '-interp', 'trilinear', '-out', str(positive_support_output)
  ])
  if result['returncode'] != 0:
    raise RuntimeError(f'flirt failed for {positive_support_path.name}: {result["stderr"]}')

  matrix = np.loadtxt(matrix_path)
  selected_src = fsl_vox_to_scaled(source, source_s, True)
  selected_ref = fsl_vox_to_scaled(reference, reference_s, True)
  fallback_src = fsl_vox_to_scaled(source, source_s, False)
  fallback_ref = fsl_vox_to_scaled(reference, reference_s, False)
  equations = {
    'current_selectedS_flip': np.linalg.inv(selected_ref) @ matrix @ selected_src,
    'native_LR_fallback_noflip': np.linalg.inv(fallback_ref) @ matrix @ fallback_src,
  }
  ramp_data = [np.asanyarray(nib.load(str(path)).dataobj, dtype=float) for path in outputs[:3]]
  support_data = np.asanyarray(nib.load(str(outputs[3])).dataobj, dtype=float)
  source_s_inv = np.linalg.inv(source_s)
  all_source_voxels = np.stack([grid[0].ravel(), grid[1].ravel(), grid[2].ravel(), np.ones(grid[0].size)], axis=1)
  validations = {}
  for label, transform in equations.items():
    predicted = all_source_voxels @ transform.T
    total = int(predicted.shape[0])
    in_bounds = np.all((predicted[:, :3] >= 0.0) & (predicted[:, :3] <= np.asarray(reference.shape[:3]) - 1.0), axis=1)
    permitted = 0
    finite = 0
    errors = []
    support_values = []
    for source_voxel, target_h in zip(all_source_voxels[in_bounds], predicted[in_bounds]):
      target = target_h[:3]
      support = trilinear(support_data, target)
      support_values.append(support)
      if not np.isfinite(support) or support < 0.999:
        continue
      permitted += 1
      world = np.array([trilinear(component, target) for component in ramp_data] + [1.0])
      if not np.all(np.isfinite(world[:3])):
        continue
      finite += 1
      recovered = source_s_inv @ world
      errors.append(float(np.max(np.abs(recovered[:3] - source_voxel[:3]))))
    validations[label] = {
      'source_voxels_total': total,
      'target_in_bounds': int(np.count_nonzero(in_bounds)),
      'support_at_least_0_999': permitted,
      'finite_ramp_samples': finite,
      'max_abs_source_voxel_error': None if not errors else max(errors),
      'mean_abs_source_voxel_error': None if not errors else float(np.mean(errors)),
      'minimum_sampled_support': None if not support_values else float(min(support_values)),
      'query_source_voxel_0_target_voxel': (transform @ np.array([0., 0., 0., 1.]))[:3].tolist(),
    }

  # This fit is measurement, rather than an assumed header convention: each
  # output ramp value decodes to the source voxel sampled by FLIRT at a target
  # grid point. Its inverse is the observed source-to-target voxel map.
  target_indices = np.argwhere(support_data >= 0.999)
  target_h = np.c_[target_indices, np.ones(len(target_indices))]
  measured_world = np.array([[component[tuple(index)] for component in ramp_data] for index in target_indices])
  measured_source = (source_s_inv @ np.c_[measured_world, np.ones(len(measured_world))].T).T
  target_to_source = np.linalg.lstsq(target_h, measured_source, rcond=None)[0].T
  measured_residual = float(np.max(np.abs(target_h @ target_to_source.T - measured_source)))
  observed_source_to_target = np.linalg.inv(target_to_source)
  validations['measured_flirt_ramp_map'] = {
    'support_at_least_0_999': int(len(target_indices)),
    'target_to_source_voxel': target_to_source.tolist(),
    'source_to_target_voxel': observed_source_to_target.tolist(),
    'max_abs_fit_residual': measured_residual,
    'source_voxel_0_target_voxel': (observed_source_to_target @ np.array([0., 0., 0., 1.]))[:3].tolist(),
    'max_abs_difference_from_current_selectedS_flip': float(np.max(np.abs(observed_source_to_target - equations['current_selectedS_flip']))),
    'max_abs_difference_from_native_LR_fallback_noflip': float(np.max(np.abs(observed_source_to_target - equations['native_LR_fallback_noflip']))),
  }
  positive_ramps = [np.asanyarray(nib.load(str(path)).dataobj, dtype=float) for path in positive_outputs]
  positive_support = np.asanyarray(nib.load(str(positive_support_output)).dataobj, dtype=float)
  validations['consistent_positive_flirt_outputs'] = {
    'output_shape': [int(value) for value in positive_support.shape],
    'support_at_least_0_999': int(np.count_nonzero(positive_support >= 0.999)),
    'support_minimum': float(np.min(positive_support)),
    'support_maximum': float(np.max(positive_support)),
    'finite_ramp_values': [int(np.count_nonzero(np.isfinite(ramp))) for ramp in positive_ramps],
    'ramp_total_values': int(positive_support.size),
  }

  inventory = {
    'image_id_expected': 'sha256:3ffbceee2ab631d765c6e2d1d90eedc9f31a33e8c555ec08e85ef6d65c66c1e2',
    'platform': platform.platform(),
    'python': platform.python_version(),
    'numpy': np.__version__,
    'nibabel': nib.__version__,
    'tool_commands': tool_commands,
    'tool_sha256': {name: sha256(Path('/opt/fsl6/bin') / name) for name in ('flirt', 'img2imgcoord', 'fslversion')},
    'probe_source_sha256': {name: sha256(ROOT / name) for name in ('img2imgcoord.cc', 'img2imgcoord-master.cc')},
    'source_original': affine_record(source),
    'reference_original': affine_record(reference),
    'derived_headers': {name: {'source': affine_record(nib.load(str(src))), 'reference': affine_record(nib.load(str(ref)))} for name, (src, ref) in paths.items() if name != 'original_conflict'},
    'hashes': {str(path.relative_to(ROOT)): sha256(path) for path in sorted(ROOT.glob('*.nii')) + [matrix_path, coordinate_path]},
    'img2imgcoord_probes': cli_results,
    'equations': {name: matrix.tolist() for name, matrix in equations.items()},
    'flirt_applyxfm_validation': validations,
    'ramp_header_forms_preserved': {
      'original_conflict_world_x': same_form(source, nib.load(str(ramps[0]))),
      'original_conflict_support': same_form(source, nib.load(str(support_path))),
      'consistent_positive_world_x': same_form(positive_source, nib.load(str(ROOT / 'consistent_positive_world_x.nii'))),
      'consistent_positive_support': same_form(positive_source, nib.load(str(positive_support_path))),
    },
  }
  (OUT / 'evidence.json').write_text(json.dumps(inventory, indent=2) + '\n')
  (OUT / 'summary.txt').write_text(json.dumps({
    'cli': {item['name']: {'returncode': item['returncode'], 'stdout': item['stdout'].strip(), 'stderr': item['stderr'].strip()} for item in cli_results},
    'validation': validations,
  }, indent=2) + '\n')


if __name__ == '__main__':
  main()
