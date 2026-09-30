from __future__ import print_function

import hashlib
import json
import os
import platform
import subprocess

import nibabel as nib
import numpy as np


ROOT = '/probe'
WARP = os.path.join(ROOT, 'oblique_WARP.nii')
RESULTS = os.path.join(ROOT, 'results')
F = np.diag([-1.0, -1.0, 1.0, 1.0])  # NIfTI/RAS <-> AFNI DICOM/LPS point coordinates


def digest(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        while True:
            b = f.read(1024 * 1024)
            if not b:
                break
            h.update(b)
    return h.hexdigest()


def serial(matrix):
    return np.asarray(matrix, dtype=float).tolist()


def header(image):
    q, qc = image.get_qform(coded=True)
    s, sc = image.get_sform(coded=True)
    return {
        'shape': [int(x) for x in image.shape],
        'zooms': [float(x) for x in image.header.get_zooms()],
        'qform_code': int(qc), 'sform_code': int(sc),
        'qform': None if q is None else serial(q),
        'sform': None if s is None else serial(s),
        'best_affine': serial(image.header.get_best_affine()),
    }


def cardinalise(affine):
    """The documented THD cardinal spelling: axis, voxel norm (4-decimal), origin."""
    out = np.eye(4)
    out[:3, 3] = affine[:3, 3]
    for column in range(3):
        values = affine[:3, column]
        row = int(np.argmax(np.abs(values)))
        size = round(float(np.linalg.norm(values)), 4)
        out[row, column] = np.sign(values[row]) * size
    return out


def tri(field, point):
    if np.any(point < 0.0) or np.any(point > np.asarray(field.shape[:3]) - 1.0):
        return None
    lo = np.floor(point).astype(int)
    hi = np.minimum(lo + 1, np.asarray(field.shape[:3]) - 1)
    frac = point - lo
    result = np.zeros(3, dtype=float)
    for dx in (0, 1):
        for dy in (0, 1):
            for dz in (0, 1):
                weight = (frac[0] if dx else 1.0 - frac[0]) * (frac[1] if dy else 1.0 - frac[1]) * (frac[2] if dz else 1.0 - frac[2])
                result += weight * field[hi[0] if dx else lo[0], hi[1] if dy else lo[1], hi[2] if dz else lo[2], 0, :]
    return result


def run(name, argv):
    p = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    stdout, stderr = p.communicate()
    result = {'name': name, 'argv': argv, 'returncode': p.returncode, 'stdout': stdout, 'stderr': stderr}
    with open(os.path.join(RESULTS, name + '.json'), 'w') as f:
        json.dump(result, f, indent=2); f.write('\n')
    if p.returncode != 0:
        raise RuntimeError(result)
    return result


def main():
    if not os.path.isdir(RESULTS):
        os.makedirs(RESULTS)
    image = nib.load(WARP)
    data = np.asanyarray(image.dataobj, dtype=float)
    q, qc = image.get_qform(coded=True)
    s, sc = image.get_sform(coded=True)
    if sc <= 0:
        raise RuntimeError('expected AFNI priority sform')
    cardinal_ras = cardinalise(s)
    raw_ras = s
    # fixed named specimen: six exact nodes and six asymmetric off-nodes
    specimen = [
        # These points keep both the documented cardinal and the raw-oblique
        # alternatives inside their lattices, so neither comparison borrows a
        # boundary convention.
        ('node_212', (2.0, 1.0, 2.0)), ('node_323', (3.0, 2.0, 3.0)),
        ('node_414', (4.0, 1.0, 4.0)), ('node_522', (5.0, 2.0, 2.0)),
        ('node_633', (6.0, 3.0, 3.0)), ('node_724', (7.0, 2.0, 4.0)),
        ('off_240_140_220', (2.40, 1.40, 2.20)), ('off_360_230_310', (3.60, 2.30, 3.10)),
        ('off_440_150_350', (4.40, 1.50, 3.50)), ('off_520_240_250', (5.20, 2.40, 2.50)),
        ('off_630_270_320', (6.30, 2.70, 3.20)), ('off_660_140_270', (6.60, 1.40, 2.70)),
    ]
    points_lps, rows = [], []
    raw_inverse = np.linalg.inv(raw_ras)
    for name, voxel in specimen:
        v = np.array([voxel[0], voxel[1], voxel[2], 1.0])
        point_lps = np.dot(F, np.dot(cardinal_ras, v))[:3]
        d_card = tri(data, np.array(voxel))
        input_ras = np.dot(F, np.r_[point_lps, 1.0])
        raw_voxel = np.dot(raw_inverse, input_ras)[:3]
        d_raw = tri(data, raw_voxel)
        rows.append({
            'name': name, 'kind': 'node' if name.startswith('node') else 'offnode',
            'cardinal_voxel': list(voxel), 'input_lps': point_lps.tolist(),
            'cardinal_predicted_lps': (point_lps + d_card).tolist(),
            'raw_oblique_voxel': raw_voxel.tolist(),
            'raw_oblique_predicted_lps': None if d_raw is None else (point_lps + d_raw).tolist(),
        })
        points_lps.append(point_lps)
    point_path = os.path.join(ROOT, 'oblique_cardinal_lps_points.1D')
    with open(point_path, 'w') as f:
        for p in points_lps:
            f.write('%.12g %.12g %.12g\n' % tuple(p))
    # Native `3dNwarpXYZ` is documented to take base DICOM/LPS points through
    # a _WARP into matching source DICOM/LPS points.
    command = run('3dNwarpXYZ_oblique', ['/opt/afni/src/../install/3dNwarpXYZ', '-nwarp', WARP, point_path])
    native_stdout_path = os.path.join(RESULTS, '3dNwarpXYZ_oblique.stdout.txt')
    with open(native_stdout_path, 'w') as f:
        f.write(command['stdout'])
    native = []
    for line in command['stdout'].splitlines():
        fields = line.split()
        if len(fields) == 3:
            try:
                native.append([float(x) for x in fields])
            except ValueError:
                pass
    if len(native) != len(rows):
        raise RuntimeError('expected %d output rows, got %d: %r' % (len(rows), len(native), command['stdout']))
    card_err, raw_err = [], []
    for row, observed in zip(rows, native):
        row['native_lps'] = observed
        row['cardinal_abs_error_mm'] = np.abs(np.asarray(observed) - np.asarray(row['cardinal_predicted_lps'])).tolist()
        card_err.extend(row['cardinal_abs_error_mm'])
        if row['raw_oblique_predicted_lps'] is not None:
            row['raw_oblique_abs_error_mm'] = np.abs(np.asarray(observed) - np.asarray(row['raw_oblique_predicted_lps'])).tolist()
            raw_err.extend(row['raw_oblique_abs_error_mm'])
    info = run('3dinfo_oblique', ['/opt/afni/src/../install/3dinfo', '-verb', WARP])
    tool = run('afni_version', ['/opt/afni/src/../install/afni', '-ver'])
    summary = {
        'container_image': 'afni/afni_make_build@sha256:6bc2b04e92e0874d7cf252006be35f2671438904c233f844633c40a8fc7cf9bf',
        'runtime': platform.platform(), 'numpy': np.__version__, 'nibabel': nib.__version__,
        'tool_version_stdout': tool['stdout'], 'input_header': header(image),
        'cardinal_ras': serial(cardinal_ras), 'raw_oblique_sform_ras': serial(raw_ras),
        'coordinate_contract': '3dNwarpXYZ input/output are AFNI DICOM/LPS mm; predictions add stored LPS displacement to base LPS point',
        'points': rows,
        'cardinal_max_abs_error_mm': float(max(card_err)), 'cardinal_mean_abs_error_mm': float(np.mean(card_err)),
        'raw_oblique_eligible': len(raw_err) == 3 * len(rows),
        'raw_oblique_max_abs_error_mm': None if not raw_err else float(max(raw_err)),
        'raw_oblique_mean_abs_error_mm': None if not raw_err else float(np.mean(raw_err)),
        'hashes': {
            'oblique_WARP.nii': digest(WARP),
            'oblique_cardinal_lps_points.1D': digest(point_path),
            '3dNwarpXYZ_oblique.stdout.txt': digest(native_stdout_path),
            '3dNwarpXYZ': digest('/opt/afni/src/../install/3dNwarpXYZ'),
            '3dinfo': digest('/opt/afni/src/../install/3dinfo'),
            'afni': digest('/opt/afni/src/../install/afni'),
        },
    }
    with open(os.path.join(RESULTS, 'evidence.json'), 'w') as f:
        json.dump(summary, f, indent=2); f.write('\n')


if __name__ == '__main__':
    main()
