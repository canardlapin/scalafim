#!/usr/bin/env python3
"""Independent byte/index closure audit; no generator or Scala implementation import.

python verify_closure.py --source-dir INPUTS --native-dir NATIVE --fixture-dir FIXTURES
"""
import argparse
import gzip
import hashlib
import json
from pathlib import Path

import nibabel as nib
import numpy as np


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source-dir','native-dir','fixture-dir'):
        parser.add_argument('--'+name,type=Path,required=True)
    args = parser.parse_args()
    source,native,fixture = args.source_dir,args.native_dir,args.fixture_dir
    manifest = json.loads((fixture/'manifest.json').read_text())
    for name,digest in manifest['files'].items(): assert sha(fixture/name) == digest,name
    count = 0
    for record_file in ('subject-provenance.json','template-provenance.json'):
        record = json.loads((fixture/record_file).read_text())
        assets = record.get('assets',record)
        for name,identity in assets.items():
            if not isinstance(identity,dict) or 'sha256' not in identity: continue
            assert sha(source/name) == identity['sha256'],name
            assert (source/name).stat().st_size == identity['size'],name
            count += 1
    def tsv(name): return np.loadtxt(fixture/name,delimiter='\t',skiprows=1,ndmin=2)
    white,faces = nib.freesurfer.read_geometry(source/'subject_L_white')
    pial,pfaces = nib.freesurfer.read_geometry(source/'subject_L_pial')
    subject,sfaces = nib.freesurfer.read_geometry(source/'subject_L_sphere.reg')
    assert np.array_equal(faces,pfaces) and np.array_equal(faces,sfaces)
    def surface(name):
        image = nib.load(source/name)
        return image.agg_data('NIFTI_INTENT_POINTSET'),image.agg_data('NIFTI_INTENT_TRIANGLE')
    average,afaces = surface('fsaverage_L.surf.gii')
    target,tfaces = surface('fslr_L_in_fsaverage.surf.gii')
    for a,b in [('fsaverage_L.surf.gii','HCP-fsaverage_std_sphere.L.164k_fsavg_L.surf.gii'),
                ('fslr_L_in_fsaverage.surf.gii','HCP-fs_LR-deformed_to-fsaverage.L.sphere.32k_fs_LR.surf.gii')]:
        ac,at = surface(a); bc,bt = surface(b)
        assert np.array_equal(ac,bc) and np.array_equal(at,bt),(a,b)
    ids = {}
    for name,coords,topology in [('subject',subject,faces),('fsaverage',average,afaces),('target',target,tfaces)]:
        v = tsv(name+'-vertices.tsv'); f = tsv(name+'-faces.tsv')
        ids[name] = v[:,0].astype(int)
        assert np.array_equal(coords[ids[name]],v[:,1:]),name+' coordinates'
        assert np.array_equal(topology[f[:,0].astype(int)],ids[name][f[:,1:].astype(int)]),name+' ordered faces'
    sid = ids['subject']; aid = ids['fsaverage']; tid = ids['target']
    assert np.array_equal(white[sid],tsv('white.tsv')) and np.array_equal(pial[sid],tsv('pial.tsv'))
    normalized = target/np.linalg.norm(target,axis=1)[:,None]
    direction = np.array([.2,-.7,.68]); direction /= np.linalg.norm(direction)
    selected = np.sort(np.argsort(np.linalg.norm(normalized-direction,axis=1),kind='stable')[:64])
    assert np.array_equal(selected,tid)
    selected_faces = tsv('target-faces.tsv')[:,1:].astype(int)
    adjacency = [set() for _ in tid]
    for a,b,c in selected_faces:
        adjacency[a].update((b,c)); adjacency[b].update((a,c)); adjacency[c].update((a,b))
    reached = {0}; pending = [0]
    while pending:
        for vertex in adjacency[pending.pop()]-reached:
            reached.add(vertex); pending.append(vertex)
    assert len(reached) == len(tid), 'target selection must be one connected patch'
    image = nib.load(source/'subject_orig.mgz')
    original = np.asarray(image.dataobj)
    geom = tsv('geometry.tsv')[:,0]
    shape = tuple(geom[19:22].astype(int)); crop_affine = geom[22:].reshape(4,4)
    norig = geom[3:19].reshape(4,4)
    assert np.array_equal(norig,image.affine)
    start = np.rint(nib.affines.apply_affine(np.linalg.inv(norig),crop_affine[:3,3])).astype(int)
    stop = start+shape
    exact = original[tuple(slice(a,b) for a,b in zip(start,stop))]
    retained = np.frombuffer(gzip.decompress((fixture/'volume.raw.gz').read_bytes()),dtype=np.uint8).reshape(shape)
    assert original.dtype == np.uint8 and np.array_equal(exact,retained)
    native_volume = nib.load(native/'orig.nii.gz')
    assert np.array_equal(native_volume.affine,norig)
    assert np.array_equal(np.asarray(native_volume.dataobj),original)
    true_link = norig @ np.linalg.inv(image.header.get_vox2ras_tkr())
    assert np.array_equal(nib.affines.apply_affine(true_link,white[sid]),tsv('white-scanner-header.tsv'))
    def metric(name): return np.column_stack([a.data for a in nib.load(native/name).darrays]).astype(float)
    full_ribbon = np.mean([metric(f'fraction-{i}.func.gii')[:,0] for i in range(7)],axis=0)
    assert np.array_equal(full_ribbon[sid,None],tsv('ribbon-native.tsv'))
    for name in ('direct','staged'):
        assert np.array_equal(metric(name+'.func.gii')[tid],tsv(name+'-native.tsv'))
    commands = json.loads((fixture/'commands.json').read_text())
    assert len(commands) == 11 and all(c['exit_code'] == 0 for c in commands)
    assert commands == json.loads((native/'commands.json').read_text())
    # Recompute the analytic bound from stored original coordinates and weights.
    def norm100(coords):
        value = coords.astype(np.float64)
        return value*100/np.linalg.norm(value,axis=1)[:,None]
    s,a,t = norm100(subject[sid]),norm100(average[aid]),norm100(target[tid])
    first,second,direct = [tsv(n+'.tsv') for n in ('subject-to-fsaverage','fsaverage-to-target','subject-to-target')]
    def apply(plan,values): return np.array([r[3:] @ values[r[:3].astype(int)] for r in plan])
    for plan in (first,second,direct):
        assert (plan[:,3:] >= 0).all() and np.max(abs(plan[:,3:].sum(axis=1)-1)) < 1e-14
    distance1 = np.linalg.norm(apply(first,s)-a,axis=1)
    distance2 = np.linalg.norm(apply(second,a)-t,axis=1)
    distance_direct = np.linalg.norm(apply(direct,s)-t,axis=1)
    bound = np.linalg.norm(np.array([1,.5,-.75])/100)*(apply(second,distance1)+distance2+distance_direct)+1e-9
    assert np.max(abs(bound-tsv('analytic-bound.tsv')[:,0])) < 1e-14
    print(json.dumps({'verified_source_assets':count,'verified_fixture_files':len(manifest['files']),
         'full_native_input_values_exact':int(original.size),'crop_values_exact':int(retained.size),
         'subject_vertices':len(sid),'fsaverage_vertices':len(aid),'target_vertices':len(tid),
         'native_command_exits_verified':len(commands),'native_full_output_subsets_exact':True,
         'HCP_gauges_and_ordered_faces_exact':True,'analytic_bound_independently_recomputed':True,
         'target_patch_connected':True},indent=2))


if __name__ == '__main__': main()
