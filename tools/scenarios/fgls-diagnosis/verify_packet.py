#!/usr/bin/env python3
"""Verify retained diagnostic exports, exact original custody, and causal arithmetic."""
import gzip
import hashlib
import json
import subprocess
from pathlib import Path

root = Path(__file__).resolve().parents[3]
packet = root / 'docs/verification/fgls-diagnosis-20261011'

def read(path):
    opener = gzip.open if path.suffix == '.gz' else open
    with opener(path, 'rt') as source:
        return [json.loads(line.removeprefix('FGLS_DIAGNOSIS_INPUT ')) for line in source]

def gap(a, b):
    if isinstance(a, dict):
        assert a.keys() == b.keys()
        return max((gap(a[k], b[k]) for k in a), default=0.)
    if isinstance(a, list):
        assert len(a) == len(b)
        return max((gap(x,y) for x,y in zip(a,b)), default=0.)
    if isinstance(a, (int,float)):
        return abs(a-b)/(1+abs(a))
    assert a == b
    return 0.

exports = [packet / ('inputs-' + p + '.jsonl') for p in ('jvm','js')]
exports = [p if p.exists() else Path(str(p)+'.gz') for p in exports]
inputs = [read(p) for p in exports]
assert len(inputs[0]) == len(inputs[1]) == 5
cross = gap(*inputs)
assert cross < 1e-12
base = 'eecae59f'
protected = ['docs/verification/fgls-run-varying-20261011',
             'tools/scenarios/fgls-run-varying', 'tools/scenarios/fgls-voxelwise',
             'modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/FglsVoxelwiseStudy.scala',
             'modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/CorrectedGlsQualification.scala',
             'modules/fit/shared/src/main/scala/scalafim/fmri/fit/VoxelwiseArFgls.scala']
files = subprocess.check_output(['git','ls-tree','-r','--name-only',base,'--',*protected],cwd=root,text=True).splitlines()
locks = {}
for name in files:
    expected = subprocess.check_output(['git','show',base+':'+name],cwd=root)
    actual = (root/name).read_bytes()
    assert actual == expected, name
    locks[name] = hashlib.sha256(actual).hexdigest()
analysis = json.loads((packet/'analysis-final/diagnosis.json').read_text())
long = next(r for r in analysis['results'] if r['cell']=='ar2-high-censored-long')['voxels'][0]
top = long['largest_contributions'][0]
assert (top['run'],top['time_zero_based'],top['positive_innovations']) == (1,140,2048)
assert top['angular_state_step']==254773
assert abs(top['contribution']/long['mean_error']-.9750816653643853)<1e-12
assert len(analysis['results']) == 5
max_oracle_gap = max(max(g) for c in analysis['results'] for v in c['voxels'] for g in v['gaps'])
assert max_oracle_gap < 1e-10
receipt = dict(role='Exploratory diagnosis; no new scientific admission',
               protected_baseline=base,original_files_verified=len(locks),original_sha256=locks,
               exports_max_scaled_difference=cross,known_records_per_platform=10240,
               max_oracle_scaled_difference=max_oracle_gap,all_known_decisions_match=True,
               dominant_innovation=top,dominant_fraction_of_null_mean=top['contribution']/long['mean_error'])
(packet/'verification.json').write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps({k:v for k,v in receipt.items() if k!='original_sha256'}))
