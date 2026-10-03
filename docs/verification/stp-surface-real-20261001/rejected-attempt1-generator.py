#!/usr/bin/env python3
"""Bounded real left-hemisphere orig -> ribbon -> fsaverage -> fsLR oracle.

Use numpy 2.5.3, nibabel 5.4.2 and Workbench 2.2.1. Source admission is
hash-bound; outputs are never overwritten. Workbench uses closest points;
ScalaFIM uses radial intersections. Both estimators are independently solved.
No registration quality, polyhedral ribbon or whole-mesh qualification claim.
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

QUERY_COUNT = 64
QUERY_DIRECTION = np.array([.2, -.7, .68])
MATH_BUDGET = 1e-9
PLACEMENT_BUDGET_MM = 5e-5
NATIVE_BUDGET = 1e-3
GRADIENT = np.array([1., .5, -.75]) / 100.


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def jsave(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n')


def table(path, header, values):
    np.savetxt(path, values, fmt='%.17g', delimiter='\t', header=header, comments='')


def normalize(coords):
    return coords * (100. / np.linalg.norm(coords, axis=1))[:, None]


class FullMeshOracle:
    """Exhaustive full-face search, independent of the production spatial hash.

    Radial barycentrics solve V lambda = q, then normalize lambda. Closest
    points compare every face's orthogonal projection and all three edges.
    """
    def __init__(self, coords, faces):
        self.coords, self.faces = normalize(coords), faces
        self.tri = self.coords[faces]
        self.inv = np.linalg.inv(self.tri.transpose(0, 2, 1))
        self.a = self.tri[:, 0]
        self.u, self.v = self.tri[:, 1]-self.a, self.tri[:, 2]-self.a
        self.uu = np.einsum('ij,ij->i', self.u, self.u)
        self.uv = np.einsum('ij,ij->i', self.u, self.v)
        self.vv = np.einsum('ij,ij->i', self.v, self.v)
        self.det = self.uu*self.vv-self.uv*self.uv
        n = np.cross(self.u, self.v)
        distance = abs(np.einsum('ij,ij->i', self.a, n))/np.linalg.norm(n, axis=1)
        self.sagitta = float(100.-distance.min())

    def radial(self, query):
        lam = np.einsum('ijk,k->ij', self.inv, query)
        total = lam.sum(axis=1)
        weights = lam / total[:, None]
        score = weights.min(axis=1)
        valid = (total > 0) & (score >= -1e-6)
        if not valid.any():
            raise RuntimeError('Full-source radial query has no triangle: fallback forbidden')
        face = int(np.argmax(np.where(valid, score, -np.inf)))
        w = np.maximum(0., weights[face]); w /= w.sum()
        return face, w

    def closest(self, query):
        delta = query-self.a
        du = np.einsum('ij,ij->i', delta, self.u)
        dv = np.einsum('ij,ij->i', delta, self.v)
        b = (du*self.vv-dv*self.uv)/self.det
        c = (dv*self.uu-du*self.uv)/self.det
        w = np.column_stack([1-b-c, b, c])
        point = self.a+b[:, None]*self.u+c[:, None]*self.v
        distance = np.einsum('ij,ij->i', query-point, query-point)
        distance[w.min(axis=1) < 0] = np.inf
        best = int(np.argmin(distance)); best_d = float(distance[best]); best_w = w[best].copy()
        for i, j in ((0, 1), (1, 2), (2, 0)):
            start = self.tri[:, i]; edge = self.tri[:, j]-start
            t = np.clip(np.einsum('ij,ij->i', query-start, edge)/np.einsum('ij,ij->i', edge, edge), 0, 1)
            residual = query-(start+t[:, None]*edge)
            dist = np.einsum('ij,ij->i', residual, residual)
            candidate = int(np.argmin(dist))
            if dist[candidate] < best_d:
                best, best_d = candidate, float(dist[candidate])
                best_w = np.zeros(3); best_w[i] = 1-t[best]; best_w[j] = t[best]
        return best, best_w

    def plans(self, queries):
        radial, closest = [], []
        for q in normalize(queries):
            radial.append(self.radial(q)); closest.append(self.closest(q))
        return radial, closest

    def apply(self, plans, values):
        return np.array([w @ values[self.faces[f]] for f, w in plans])


def sample(volume, voxels):
    base = np.floor(voxels).astype(int); frac = voxels-base
    if (base < 0).any() or (base+1 >= np.array(volume.shape)).any():
        raise RuntimeError('Ribbon sample outside original volume')
    result = np.zeros(len(voxels))
    for x in (0, 1):
        for y in (0, 1):
            for z in (0, 1):
                weight = np.prod(np.where(np.array([x,y,z]), frac, 1-frac), axis=1)
                idx = base+np.array([x,y,z])
                result += weight*volume[tuple(idx.T)]
    return result


def ribbon(volume, white, pial, torig):
    inverse = np.linalg.inv(torig)
    result = np.zeros(len(white))
    for fraction in np.linspace(0, 1, 7):
        points = white+(pial-white)*fraction
        result += sample(volume, nib.affines.apply_affine(inverse, points))/7.
    return result


def mesh(coords, faces, path):
    nib.save(nib.gifti.GiftiImage(darrays=[
        nib.gifti.GiftiDataArray(coords.astype(np.float32), intent='NIFTI_INTENT_POINTSET'),
        nib.gifti.GiftiDataArray(faces.astype(np.int32), intent='NIFTI_INTENT_TRIANGLE')]), path)


def metric(values, path):
    if values.ndim == 1: values = values[:, None]
    nib.save(nib.gifti.GiftiImage(darrays=[nib.gifti.GiftiDataArray(
        column.astype(np.float32), intent='NIFTI_INTENT_NONE') for column in values.T]), path)


def read_metric(path):
    return np.column_stack([a.data.astype(float) for a in nib.load(path).darrays])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source-dir', required=True, type=Path)
    parser.add_argument('--workbench', required=True, type=Path)
    parser.add_argument('--work-dir', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    source, wb, work, out = [p.resolve() for p in (args.source_dir, args.workbench, args.work_dir, args.output)]
    if work.exists() or out.exists(): raise SystemExit('Refusing to overwrite evidence/fixtures')
    work.mkdir(parents=True); out.mkdir(parents=True)
    subject = json.loads((source/'subject-provenance.json').read_text())
    templates = json.loads((source/'template-provenance.json').read_text())
    required = ['subject_L_white','subject_L_pial','subject_L_sphere.reg','subject_orig.mgz']
    for name in required:
        rec = subject['assets'][name]
        assert sha(source/name) == rec['sha256'] and (source/name).stat().st_size == rec['size']
    for name, rec in templates.items():
        assert sha(source/name) == rec['sha256'] and (source/name).stat().st_size == rec['size']
    white, faces = nib.freesurfer.read_geometry(source/'subject_L_white')
    pial, pf = nib.freesurfer.read_geometry(source/'subject_L_pial')
    registered, sf = nib.freesurfer.read_geometry(source/'subject_L_sphere.reg')
    assert np.array_equal(faces, pf) and np.array_equal(faces, sf)
    def gifti(name):
        image = nib.load(source/name)
        return image.agg_data('NIFTI_INTENT_POINTSET').astype(float), image.agg_data('NIFTI_INTENT_TRIANGLE')
    average, af = gifti('fsaverage_L.surf.gii')
    target, tf = gifti('fslr_L_in_fsaverage.surf.gii')
    for name, coords, topology in [
        ('HCP-fsaverage_std_sphere.L.164k_fsavg_L.surf.gii', average, af),
        ('HCP-fs_LR-deformed_to-fsaverage.L.sphere.32k_fs_LR.surf.gii', target, tf)]:
        original, original_faces = gifti(name)
        assert np.array_equal(coords, original) and np.array_equal(topology, original_faces)
    query = normalize(QUERY_DIRECTION[None, :])[0]
    chosen = np.argsort(np.linalg.norm(normalize(target)-query, axis=1), kind='stable')[:QUERY_COUNT]
    chosen.sort()
    image = nib.load(source/'subject_orig.mgz')
    volume = np.asarray(image.dataobj, dtype=np.float64)
    norig = image.affine; true_torig = image.header.get_vox2ras_tkr().astype(float)
    zooms = np.linalg.norm(norig[:3,:3], axis=0); dims = np.array(image.shape)
    model_torig = np.array([[-zooms[0],0,0,zooms[0]*dims[0]/2],
                            [0,0,zooms[2],-zooms[2]*dims[2]/2],
                            [0,-zooms[1],0,zooms[1]*dims[1]/2],[0,0,0,1.]])
    true_link = norig @ np.linalg.inv(true_torig)
    model_link = norig @ np.linalg.inv(model_torig)
    placement_delta = float(np.max(abs(nib.affines.apply_affine(true_link, np.concatenate([white,pial]))-
                                       nib.affines.apply_affine(model_link, np.concatenate([white,pial])))))
    assert placement_delta <= PLACEMENT_BUDGET_MM
    # Freeze geometry/limits before any native or real-field output is inspected.
    contract = {'query_count':QUERY_COUNT,'query_direction':QUERY_DIRECTION.tolist(),'target_original_ids':chosen.tolist(),
                'math_budget':MATH_BUDGET,'placement_budget_mm':PLACEMENT_BUDGET_MM,'native_budget':NATIVE_BUDGET,
                'measured_input_geometry_placement_delta_mm':placement_delta,'ribbon_fractions':np.linspace(0,1,7).tolist(),
                'real_commutativity':'descriptive only; analytic spherical linear field is gated',
                'native_method':'BARYCENTRIC closest point; public method radial; estimator difference explicit',
                'norig':norig.tolist(),'true_header_torig':true_torig.tolist(),'inferred_model_torig':model_torig.tolist()}
    jsave(work/'frozen-contract.json', contract)
    shutil.copy2(work/'frozen-contract.json',out/'contract.json')
    print('Contract frozen; full-face independent geometry search', flush=True)
    subject_oracle = FullMeshOracle(registered, faces)
    average_oracle = FullMeshOracle(average, af)
    ar, ac = average_oracle.plans(target[chosen])
    average_faces = sorted({f for f,_ in ar+ac})
    average_ids = np.unique(af[average_faces])
    sr, sc = subject_oracle.plans(average[average_ids])
    dr, dc = subject_oracle.plans(target[chosen])
    subject_faces = sorted({f for f,_ in sr+sc+dr+dc})
    subject_ids = np.unique(faces[subject_faces])
    # Keep original ordered IDs and exact stored coordinate bits, never fabricate topology.
    def compact(name, coords, topology, face_ids, ids):
        mapping = np.full(len(coords), -1, dtype=int); mapping[ids] = np.arange(len(ids))
        table(out/(name+'-vertices.tsv'),'original_id\tx\ty\tz',np.column_stack([ids,coords[ids]]))
        table(out/(name+'-faces.tsv'),'original_face_id\ta\tb\tc',np.column_stack([face_ids,mapping[topology[face_ids]]]))
        return mapping
    smap = compact('subject',registered,faces,subject_faces,subject_ids)
    amap = compact('fsaverage',average,af,average_faces,average_ids)
    # Target points do not assert a closed target mesh; public plan needs coordinates only.
    table(out/'target-vertices.tsv','original_id\tx\ty\tz',np.column_stack([chosen,target[chosen]]))
    table(out/'white.tsv','x\ty\tz',white[subject_ids])
    table(out/'pial.tsv','x\ty\tz',pial[subject_ids])
    # Exact crop covering all sample corners for the model and actual header placements.
    endpoints = np.concatenate([white[subject_ids],pial[subject_ids]])
    voxels = np.concatenate([nib.affines.apply_affine(np.linalg.inv(t),endpoints) for t in (model_torig,true_torig)])
    start = np.floor(voxels.min(axis=0)).astype(int)-1
    stop = np.floor(voxels.max(axis=0)).astype(int)+3
    assert (start >= 0).all() and (stop <= dims).all()
    crop = volume[tuple(slice(a,b) for a,b in zip(start,stop))].astype(np.uint8)
    (out/'volume.raw.gz').write_bytes(gzip.compress(crop.tobytes(order='C'),mtime=0))
    crop_affine = norig.copy(); crop_affine[:3,3] = nib.affines.apply_affine(norig,start)
    table(out/'geometry.tsv','value',np.r_[dims,norig.ravel(),crop.shape,crop_affine.ravel()][:,None])
    def plan_table(name, plans, topology, mapping):
        table(out/(name+'.tsv'),'a\tb\tc\twa\twb\twc',
              np.array([np.r_[mapping[topology[f]], w] for f,w in plans]))
    plan_table('subject-to-fsaverage',sr,faces,smap)
    plan_table('fsaverage-to-target',ar,af,amap)
    plan_table('subject-to-target',dr,faces,smap)
    print('Contributing closure',len(subject_ids),len(average_ids),len(chosen),'vertices; crop',crop.shape,flush=True)
    sampled_model = ribbon(volume,white[subject_ids],pial[subject_ids],model_torig)
    sampled_true = ribbon(volume,white[subject_ids],pial[subject_ids],true_torig)
    subject_fields = np.column_stack([sampled_model,subject_oracle.coords[subject_ids] @ GRADIENT])
    true_fields = np.column_stack([sampled_true,subject_oracle.coords[subject_ids] @ GRADIENT])
    def local(plans, topology, mapping, fields):
        return np.array([w @ fields[mapping[topology[f]]] for f,w in plans])
    stage1 = local(sr,faces,smap,subject_fields)
    staged = local(ar,af,amap,stage1)
    direct = local(dr,faces,smap,subject_fields)
    closest_stage1 = local(sc,faces,smap,true_fields)
    closest_staged = local(ac,af,amap,closest_stage1)
    closest_direct = local(dc,faces,smap,true_fields)
    table(out/'ribbon-math.tsv','intensity\tanalytic',subject_fields)
    table(out/'fsaverage-math.tsv','intensity\tanalytic',stage1)
    table(out/'staged-math.tsv','intensity\tanalytic',staged)
    table(out/'direct-math.tsv','intensity\tanalytic',direct)
    table(out/'staged-closest.tsv','intensity\tanalytic',closest_staged)
    table(out/'direct-closest.tsv','intensity\tanalytic',closest_direct)
    # Input-only estimator error bound: convex weights, union-support local range.
    def distributions(plans, topology):
        return [{int(v):float(w) for v,w in zip(topology[f],weights) if w != 0} for f,weights in plans]
    sr_d, sc_d = distributions(sr,faces), distributions(sc,faces)
    def product(plans, first):
        result = []
        for f,w in plans:
            row = {}
            for original,v in zip(af[f],w):
                for k,u in first[int(amap[original])].items(): row[k] = row.get(k,0.)+v*u
            result.append(row)
        return result
    radial_rows = [distributions(dr,faces),product(ar,sr_d)]
    closest_rows = [distributions(dc,faces),product(ac,sc_d)]
    bounds = []
    for radial, closest in zip(radial_rows,closest_rows):
        route = []
        for a,b in zip(radial,closest):
            keys = sorted(a.keys()|b.keys()); values = sampled_true[smap[keys]]
            estimator = .5*float(np.ptp(values))*sum(abs(a.get(k,0)-b.get(k,0)) for k in keys)
            placement = max(abs(sampled_model[smap[keys]]-sampled_true[smap[keys]]))
            route.append(estimator+float(placement)+NATIVE_BUDGET)
        bounds.append(route)
    table(out/'native-bounds.tsv','direct\tstaged',np.array(bounds).T)
    analytic_bound = np.linalg.norm(GRADIENT)*(2*subject_oracle.sagitta+average_oracle.sagitta)+MATH_BUDGET
    table(out/'analytic-bound.tsv','bound',np.array([[analytic_bound]]))
    # These limits are written before the first Workbench numerical command.
    jsave(work/'input-derived-bounds.json',{'analytic_commutativity':analytic_bound,
          'subject_sagitta_mm':subject_oracle.sagitta,'fsaverage_sagitta_mm':average_oracle.sagitta,
          'native_bounds_rule':'half union-support true-input range times radial/closest row L1 difference + maximum input placement sample delta + 1e-3 float32 budget',
          'native_bounds_sha256':sha(out/'native-bounds.tsv')})
    commands = []
    env = os.environ.copy(); env.update(OMP_NUM_THREADS='4',LC_ALL='en_US.UTF-8')
    def run(arguments):
        begin = time.monotonic()
        r = subprocess.run([str(wb),*map(str,arguments)],env=env,cwd=work,capture_output=True,text=True,timeout=600)
        commands.append({'argv':[str(wb),*map(str,arguments)],'exit_code':r.returncode,
                         'seconds':time.monotonic()-begin,'stdout':r.stdout,'stderr':r.stderr})
        jsave(work/'commands.json',commands)
        if r.returncode: raise RuntimeError(r.stderr)
    run(['-version'])
    # Actual orig uint8 values convert losslessly to float32; its native scanner affine is retained.
    original = work/'orig.nii.gz'
    ni = nib.Nifti1Image(volume.astype(np.float32),norig); ni.set_sform(norig,1); ni.set_qform(norig,1); nib.save(ni,original)
    native_samples = []
    for i,fraction in enumerate(np.linspace(0,1,7)):
        surface = work/f'fraction-{i}.surf.gii'; result = work/f'fraction-{i}.func.gii'
        mesh(nib.affines.apply_affine(true_link,white+(pial-white)*fraction),faces,surface)
        run(['-volume-to-surface-mapping',original,surface,result,'-trilinear'])
        native_samples.append(read_metric(result)[:,0])
    native_ribbon = np.mean(native_samples,axis=0)
    registered_file = work/'subject-registered.surf.gii'; mesh(registered,faces,registered_file)
    source_metric = work/'subject.func.gii'
    metric(np.column_stack([native_ribbon,subject_oracle.coords @ GRADIENT]),source_metric)
    average_metric = work/'fsaverage.func.gii'; staged_metric = work/'staged.func.gii'; direct_metric = work/'direct.func.gii'
    run(['-metric-resample',source_metric,registered_file,source/'fsaverage_L.surf.gii','BARYCENTRIC',average_metric])
    run(['-metric-resample',average_metric,source/'fsaverage_L.surf.gii',source/'fslr_L_in_fsaverage.surf.gii','BARYCENTRIC',staged_metric])
    run(['-metric-resample',source_metric,registered_file,source/'fslr_L_in_fsaverage.surf.gii','BARYCENTRIC',direct_metric])
    native_direct, native_staged = read_metric(direct_metric)[chosen],read_metric(staged_metric)[chosen]
    table(out/'ribbon-native.tsv','intensity',native_ribbon[subject_ids,None])
    table(out/'direct-native.tsv','intensity\tanalytic',native_direct)
    table(out/'staged-native.tsv','intensity\tanalytic',native_staged)
    closest_errors = {'direct':float(np.max(abs(native_direct-closest_direct))),
                      'staged':float(np.max(abs(native_staged-closest_staged))),
                      'ribbon':float(np.max(abs(native_ribbon[subject_ids]-sampled_true)))}
    gap = abs(staged[:,0]-direct[:,0])
    report = {'independent_closest_vs_native_max_abs':closest_errors,
              'radial_vs_native_direct_max_abs':float(np.max(abs(direct[:,0]-native_direct[:,0]))),
              'radial_vs_native_staged_max_abs':float(np.max(abs(staged[:,0]-native_staged[:,0]))),
              'real_staged_direct_gap_descriptive':{'max':float(gap.max()),'median':float(np.median(gap)),
                                                   'p95':float(np.percentile(gap,95))},
              'analytic_commutativity_bound':float(analytic_bound),
              'analytic_commutativity_max_abs':float(np.max(abs(staged[:,1]-direct[:,1]))),
              'source_vertices':len(registered),'fsaverage_vertices':len(average),'fslr_vertices':len(target),
              'subject_retained_vertices':len(subject_ids),'fsaverage_retained_vertices':len(average_ids),
              'crop_start':start.tolist(),'crop_shape':list(crop.shape),'crop_values_exact':True,
              'successful_native_commands':sum(r['exit_code']==0 for r in commands),
              'numpy':np.__version__,'nibabel':nib.__version__,'workbench_wrapper_sha256':sha(wb),
              'workbench_executable_sha256':sha(wb.parent.parent/'exe'/'exe_wb_command')}
    for name in ('subject-provenance.json','template-provenance.json','dataset_description.json','FreeSurfer-LICENSE.txt','HCP-LICENSE.md'):
        shutil.copy2(source/name,out/name)
    shutil.copy2(work/'input-derived-bounds.json',out/'input-derived-bounds.json')
    shutil.copy2(work/'commands.json',out/'commands.json')
    jsave(out/'report.json',report)
    jsave(out/'manifest.json',{'generator_sha256':sha(__file__),'files':{p.name:sha(p) for p in sorted(out.iterdir())},
           'modified_data_notice':'Cropped subject orig and contributor subsets of subject and template meshes; original indices/coordinates retained. Subject data CC0; template notices separate.'})
    print(json.dumps(report,indent=2),flush=True)
    if max(closest_errors.values()) > NATIVE_BUDGET: raise RuntimeError('Predeclared native float32 guard failed')
    assert np.all(abs(direct[:,0]-native_direct[:,0]) <= np.array(bounds[0]))
    assert np.all(abs(staged[:,0]-native_staged[:,0]) <= np.array(bounds[1]))
    assert np.max(abs(staged[:,1]-direct[:,1])) <= analytic_bound


if __name__ == '__main__': main()
