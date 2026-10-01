#!/usr/bin/env python3
"""Fit and qualify a real CC0 demo1 FLIRT -> FNIRT chain with native FSL.

Inputs and their licences must be identified in source-provenance.json. This
does not use FSL course data or reproduce a historical FEAT fit. Registration
accuracy is not the claim: the frozen interior checks interpretation/composition.
Run with pinned nibabel 5.4.2 / numpy 2.5.3. Outputs are never overwritten.
"""
from __future__ import annotations

import argparse
import gzip
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

import nibabel as nib
import numpy as np

# Frozen before any registration/output inspection; every query must pass.
STANDARD_START = np.array([44, 56, 40])
STANDARD_SHAPE = np.array([9, 7, 11])
HIGHRES_CENTRE_RAS = np.array([-8.5, -14.5, 9.5])
HIGHRES_SHAPE = np.array([7, 9, 5])
POINT_BUDGET = 2e-4  # mm: float32 FSL coordinates/arithmetic at < 300 mm
FLIRT_TRACE_BUDGET = 5e-5  # mm: float32 ramp storage/interpolation, separate from step drift


def sha(path):
    result = hashlib.sha256()
    with Path(path).open('rb') as source:
        while chunk := source.read(1 << 20): result.update(chunk)
    return result.hexdigest()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n')


def save(path, values, affine, header=None):
    image = nib.Nifti1Image(np.asarray(values, dtype=np.float32), affine,
                           header=None if header is None else header.copy())
    image.set_data_dtype(np.float32)
    image.set_sform(affine, code=1)
    image.set_qform(affine, code=1)
    nib.save(image, path)


def crop(source, target, start, shape):
    image = nib.load(source)
    stop = start + shape
    if (start < 0).any() or (stop > np.array(image.shape[:3])).any():
        raise RuntimeError(f'Crop outside {source}')
    slices = tuple(slice(int(a), int(b)) for a, b in zip(start, stop))
    original = np.asarray(image.dataobj, dtype=np.float32)
    values = original[slices].copy()
    affine = image.affine.copy()
    affine[:3, 3] = (image.affine @ np.r_[start, 1])[:3]
    save(target, values, affine, image.header)
    retained = np.asarray(nib.load(target).dataobj, dtype=np.float32)
    if not np.array_equal(values.view('uint32'), retained.view('uint32')):
        raise RuntimeError(f'Crop changed sample bits: {source}')
    return {'start': start.tolist(), 'shape': shape.tolist(), 'source_sha256': sha(source),
            'retained_values': int(values.size), 'retained_float32_bits_exact': True}


def header_only(source, target):
    # Preserve every native-input header byte; zero data are geometry placeholders.
    image = nib.load(source)
    raw = gzip.decompress(source.read_bytes())
    offset = int(image.dataobj.offset)
    target.write_bytes(gzip.compress(raw[:offset] + bytes(len(raw)-offset), mtime=0))
    return {'source_sha256': sha(source), 'header_bytes': offset,
            'original_header_sha256': hashlib.sha256(raw[:offset]).hexdigest()}


def flirt_accumulation_bound(func, highres, matrix, start, shape, table):
    """A priori roundoff bound for FSL newimage's float32 y-forward differences.

    This depends on native INPUT headers, matrix and frozen queries, never output
    residuals. Coefficient rounding is bounded separately. Oblique source axes
    are mapped with the absolute voxel-to-world matrix, rather than pixdim alone.
    POINT_BUDGET covers starting arithmetic/interpolation and float32 ramp storage.
    """
    indices = np.indices(tuple(shape)).reshape(3, -1).T + start
    original_indices = indices.copy()
    if np.linalg.det(highres.affine[:3, :3]) > 0:
        indices[:, 0] = highres.shape[0]-1-indices[:, 0]
    source_sizes = [float(v) for v in func.header.get_zooms()[:3]]
    target_sizes = [float(v) for v in highres.header.get_zooms()[:3]]
    pull = np.diag([*source_sizes,1.0])
    pull = np.linalg.inv(pull) @ np.linalg.inv(matrix) @ np.diag([*target_sizes,1.0])
    endpoints = indices.copy()
    endpoints[:,1] = 0
    maximum = np.max(np.abs(nib.affines.apply_affine(pull, np.concatenate([indices,endpoints]))),axis=0)
    # nextafter avoids selecting the smaller binade at a float32 boundary.
    ulps = np.spacing(np.nextafter(maximum.astype(np.float32),np.float32(np.inf))).astype(float)
    ymax = int(indices[:,1].max())
    coefficient_error = np.abs(pull[:3] - pull[:3].astype(np.float32).astype(float))
    cast_bound = coefficient_error @ np.r_[np.max(np.abs(indices),axis=0),1.0]
    voxel_bound = .5*(ymax+4)*ulps + cast_bound
    axis_bound = np.abs(func.affine[:3,:3]) @ voxel_bound
    # Independent references: exact double algebra and native float32 stepping.
    exact_voxels = nib.affines.apply_affine(pull,indices)
    coefficients = pull[:3,:].astype(np.float32)
    trace = []
    for x,y,z in indices:
        value = np.float32(np.float32(np.float32(x)*coefficients[:,0] +
                                     np.float32(z)*coefficients[:,2]) + coefficients[:,3])
        for _ in range(int(y)): value = np.float32(value+coefficients[:,1])
        trace.append(value.astype(float))
    trace = np.array(trace)
    if np.linalg.det(func.affine[:3,:3]) > 0:
        exact_voxels[:,0] = func.shape[0]-1-exact_voxels[:,0]
        trace[:,0] = func.shape[0]-1-trace[:,0]
    rows = np.column_stack([nib.affines.apply_affine(highres.affine,original_indices),
                            nib.affines.apply_affine(func.affine,exact_voxels),
                            nib.affines.apply_affine(func.affine,trace)])
    np.savetxt(table,rows,fmt='%.17g',delimiter='\t',
               header='hx\thy\thz\tex\tey\tez\tfx\tfy\tfz',comments='')
    return {'maximum_y_index':ymax, 'maximum_abs_input_voxels':maximum.tolist(),
            'input_coordinate_ulp32':ulps.tolist(), 'coefficient_rounding_bound_voxels':cast_bound.tolist(),
            'accumulation_axis_bound_mm':axis_bound.tolist(),
            'point_budget_mm':POINT_BUDGET+float(np.max(axis_bound)),
            'trace_budget_mm':FLIRT_TRACE_BUDGET, 'exact_math_budget_mm':1e-9,
            'rule':'2e-4 mm base + max(abs(source voxel-to-world linear) @ ((ymax+4)*0.5*ulp32(maxabs input voxel coordinates)+float32 coefficient-cast bound)); no output residual used'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source-dir', type=Path, required=True)
    parser.add_argument('--fsl-dir', type=Path, required=True)
    parser.add_argument('--work-dir', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    source, fsl, work, output = [p.resolve() for p in
                               (args.source_dir, args.fsl_dir, args.work_dir, args.output)]
    if work.exists() or output.exists():
        raise SystemExit('Refusing to overwrite native evidence or fixtures')
    provenance = json.loads((source/'source-provenance.json').read_text())
    if provenance['subject']['license'] != 'CC0' or provenance['template']['identifier'] != 'MNI152NLin2009cAsym res-02':
        raise RuntimeError('Unexpected source admission')
    for name, record in provenance['assets'].items():
        if (source/name).stat().st_size != record['size'] or sha(source/name) != record['sha256']:
            raise RuntimeError(f'Source identity mismatch: {name}')
    work.mkdir(parents=True)
    output.mkdir(parents=True)
    shutil.copy2(source/'source-provenance.json', output/'source-provenance.json')
    for name in ('template-LICENSE', 'template_description.json', 'dataset_description.json'):
        shutil.copy2(source/name, output/name)
    commands = []
    binary_hashes = {name: sha(fsl/'bin'/name) for name in ('flirt', 'fnirt', 'applywarp', 'convertwarp')}
    package_lock = [json.loads(p.read_text()) for p in sorted((fsl/'conda-meta').glob('*.json'))]
    write_json(output/'package-lock.json', [{k: d.get(k) for k in
               ('name', 'version', 'build', 'subdir', 'url', 'sha256', 'md5')} for d in package_lock])
    env = os.environ.copy()
    env.update(FSLDIR=str(fsl), FSLOUTPUTTYPE='NIFTI_GZ', OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1')

    def run(name, arguments, timeout=300):
        if sha(fsl/'bin'/name) != binary_hashes[name]: raise RuntimeError('Native tool identity changed')
        argv = [str(fsl/'bin'/name)] + arguments
        print('Native', name, ' '.join(arguments), flush=True)
        started = time.monotonic()
        try:
            result = subprocess.run(argv, cwd=work, env=env, capture_output=True, text=True, timeout=timeout)
            record = {'argv': argv, 'exit_code': result.returncode, 'stdout': result.stdout, 'stderr': result.stderr}
        except subprocess.TimeoutExpired as error:
            record = {'argv': argv, 'exit_code': None, 'timed_out': True,
                      'stdout': str(error.stdout), 'stderr': str(error.stderr)}
        record['seconds'] = time.monotonic()-started
        commands.append(record)
        write_json(work/'commands.json', commands)
        if record['exit_code'] != 0: raise RuntimeError(f'Native attempt failed; receipt: {work}/commands.json')

    for name in ('example_func', 'highres', 'standard'):
        original = nib.load(source/f'{name}.nii.gz')
        mask = nib.load(source/f'{name}_mask.nii.gz')
        if original.shape != mask.shape or not np.allclose(original.affine, mask.affine, atol=1e-5, rtol=0):
            raise RuntimeError(f'Mask geometry mismatch: {name}')
        values = original.get_fdata(dtype=np.float32) * (mask.get_fdata() > .5)
        save(work/f'{name}.nii.gz', values, original.affine, original.header)
    registration_inputs = {p.name: sha(p) for p in work.iterdir() if p.is_file()}
    run('flirt', ['-in', 'example_func.nii.gz', '-ref', 'highres.nii.gz', '-dof', '6',
                  '-cost', 'normmi', '-omat', 'example_func2highres.mat', '-out', 'example_func2highres.nii.gz'], 600)
    run('flirt', ['-in', 'highres.nii.gz', '-ref', 'standard.nii.gz', '-dof', '12',
                  '-cost', 'corratio', '-omat', 'highres2standard.mat', '-out', 'highres2standard.nii.gz'], 600)
    run('fnirt', ['--in=highres.nii.gz', '--ref=standard.nii.gz', '--aff=highres2standard.mat',
                  '--cout=highres2standard_coef.nii.gz', '--fout=highres2standard_warp.nii.gz',
                  '--iout=highres2standard_fnirt.nii.gz', '--subsamp=4,2,1', '--miter=5,5,5',
                  '--warpres=10,10,10', '--infwhm=4,2,0', '--reffwhm=4,2,0',
                  '--lambda=300,75,30', '--intmod=global_linear', '--estint=1,1,1',
                  '--applyrefmask=1,1,1', '--applyinmask=1,1,1', '--jacrange=0.2,5'], 900)
    highres = nib.load(work/'highres.nii.gz')
    highres_start = np.rint((np.linalg.inv(highres.affine) @ np.r_[HIGHRES_CENTRE_RAS,1])[:3]).astype(int) - HIGHRES_SHAPE//2
    func = nib.load(work/'example_func.nii.gz')
    flirt_precision = flirt_accumulation_bound(func, highres,
        np.loadtxt(work/'example_func2highres.mat'), highres_start, HIGHRES_SHAPE,
        output/'flirt_math_and_trace.tsv')
    if flirt_precision['point_budget_mm'] > .001:
        raise RuntimeError('Native FLIRT float32 precision exceeds the declared 0.001 mm admission cap')
    indices = np.indices(func.shape)
    world = np.einsum('ab,bijk->aijk',func.affine[:3,:3],indices) + func.affine[:3,3,None,None,None]
    for axis in range(3): save(work/f'func_coord{axis}.nii.gz', world[axis], func.affine, func.header)
    save(work/'func_support.nii.gz', np.ones(func.shape), func.affine, func.header)
    run('convertwarp', ['--ref=standard.nii.gz', '--premat=example_func2highres.mat',
                        '--warp1=highres2standard_coef.nii.gz', '--out=chain_abs.nii.gz', '--absout'])
    for mode, options in [('premat', ['--warp=highres2standard_coef.nii.gz','--premat=example_func2highres.mat']),
                           ('composed', ['--warp=chain_abs.nii.gz','--abs'])]:
        for name in ('coord0','coord1','coord2','support','image'):
            input_name = 'example_func.nii.gz' if name == 'image' else f'func_{name}.nii.gz'
            run('applywarp', [f'--in={input_name}','--ref=standard.nii.gz',*options,
                              '--interp=trilinear','--datatype=float',f'--out={mode}_{name}.nii.gz'])
    for name in ('coord0','coord1','coord2','support','image'):
        input_name = 'example_func.nii.gz' if name == 'image' else f'func_{name}.nii.gz'
        run('flirt', ['-in',input_name,'-ref','highres.nii.gz','-init','example_func2highres.mat',
                      '-applyxfm','-interp','trilinear','-noresampblur','-datatype','float','-out',f'flirt_{name}.nii.gz'])
    for name, expected in registration_inputs.items():
        if sha(work/name) != expected: raise RuntimeError('Native command changed an input')

    crops = {}
    for prefix in ('premat','composed','flirt'):
        start, shape = (highres_start,HIGHRES_SHAPE) if prefix == 'flirt' else (STANDARD_START,STANDARD_SHAPE)
        for name in ('coord0','coord1','coord2','support','image'):
            path = f'{prefix}_{name}.nii.gz'
            crops[path] = crop(work/path,output/path,start,shape)
        support = nib.load(output/f'{prefix}_support.nii.gz').get_fdata()
        if not np.isfinite(support).all() or np.min(support) < .999:
            raise RuntimeError(f'Frozen query set lacks full native support: {prefix}')
    predicted = np.loadtxt(output/'flirt_math_and_trace.tsv',skiprows=1)[:,6:9]
    observed = np.column_stack([nib.load(output/f'flirt_coord{c}.nii.gz').get_fdata().ravel() for c in range(3)])
    trace_error = float(np.max(np.abs(predicted-observed)))
    if not np.isfinite(trace_error) or trace_error > FLIRT_TRACE_BUDGET:
        raise RuntimeError('Native FLIRT does not follow the independently predicted float32 trace')
    crops['chain_abs.nii.gz'] = crop(work/'chain_abs.nii.gz',output/'chain_abs.nii.gz',STANDARD_START,STANDARD_SHAPE)
    func_queries = np.concatenate([np.column_stack([nib.load(output/f'{p}_coord{c}.nii.gz').get_fdata().ravel() for c in range(3)]) for p in ('premat','composed','flirt')])
    func_indices = nib.affines.apply_affine(np.linalg.inv(func.affine),func_queries)
    low = np.floor(func_indices.min(axis=0)).astype(int)-2
    high = np.ceil(func_indices.max(axis=0)).astype(int)+3
    crops['func_crop.nii.gz'] = crop(work/'example_func.nii.gz',output/'func_crop.nii.gz',low,high-low)
    minimum_margin = float(np.minimum(func_indices-low,high-1-func_indices).min())
    if minimum_margin < 2: raise RuntimeError('Functional crop cannot preserve all trilinear neighbourhoods')
    headers = {name:header_only(work/f'{name}.nii.gz',output/f'{name}_header.nii.gz') for name in ('example_func','highres','standard')}
    for name in ('example_func2highres.mat','highres2standard.mat','highres2standard_coef.nii.gz'):
        shutil.copy2(work/name,output/name)
    shutil.copy2(work/'commands.json',output/'commands.json')
    maximum = float(np.max(np.abs(func.get_fdata())))
    steps = np.array([np.max(np.abs(np.diff(func.get_fdata(),axis=a))) for a in range(3)])
    gradient_bound = float(steps @ np.abs(np.linalg.inv(func.affine)[:3,:3]).sum(axis=1))
    scalar_budget = POINT_BUDGET*gradient_bound + 8*float(np.spacing(np.float32(maximum)))
    flirt_scalar_budget = flirt_precision['point_budget_mm']*gradient_bound + 8*float(np.spacing(np.float32(maximum)))
    comparisons = {}
    for name in ('coord0','coord1','coord2','image'):
        a=nib.load(output/f'premat_{name}.nii.gz').get_fdata();b=nib.load(output/f'composed_{name}.nii.gz').get_fdata()
        if not np.isfinite(a).all() or not np.isfinite(b).all(): raise RuntimeError('Non-finite native reference')
        comparisons[name]=float(np.max(np.abs(a-b)))
        if comparisons[name] > (scalar_budget if name=='image' else POINT_BUDGET):
            raise RuntimeError('Native routes disagree beyond predeclared arithmetic budget')
    write_json(output/'contract.json', {'standard_start':STANDARD_START.tolist(),'standard_shape':STANDARD_SHAPE.tolist(),
              'highres_start':highres_start.tolist(),'highres_shape':HIGHRES_SHAPE.tolist(),'point_budget_mm':POINT_BUDGET,
              'intensity_budget':scalar_budget,'intensity_gradient_l1_bound':gradient_bound,
              'flirt_float32_precision':flirt_precision, 'flirt_intensity_budget':flirt_scalar_budget,
              'flirt_trace_vs_native_max_abs_mm':trace_error,
              'native_route_max_abs':comparisons,'func_crop_minimum_margin_voxels':minimum_margin,
              'crops':crops,'native_input_headers':headers,'registration_input_sha256':registration_inputs})
    manifest = {'schema':'scalafim-fsl-real-chain/v1','kind':'native-oracle',
                'generator_sha256':sha(Path(__file__)),'numpy':np.__version__,'nibabel':nib.__version__,
                'binary_sha256':binary_hashes,'command_count':len(commands),
                'native_work_sha256':{p.name:sha(p) for p in sorted(work.iterdir()) if p.is_file()},
                'fixture_sha256':{p.name:sha(p) for p in sorted(output.iterdir()) if p.is_file()},
                'scope':'New native fits on real matching demo1 BOLDref/T1 and MNI2009cAsym. Fixed interior composition/resampling only; no anatomical accuracy, historical FEAT reproduction, whole-domain or Jacobian claim.'}
    write_json(output/'manifest.json',manifest)
    print(json.dumps({'commands':len(commands),'queries':int(STANDARD_SHAPE.prod()),'flirt_queries':int(HIGHRES_SHAPE.prod()),'budgets':[POINT_BUDGET,scalar_budget],'native_route_max_abs':comparisons}),flush=True)


if __name__ == '__main__': main()
