#!/usr/bin/env python3
"""Check frozen assignments and archived dataset bytes without fitting a model."""
import csv
import hashlib
import json
from pathlib import Path
import sys
import tarfile
root=Path(__file__).resolve().parents[3]
sys.path.insert(0,str(root/'tools/mvpa-inference'))
from calibration_protocol import seed_record
packet=Path(__file__).resolve().parent
manifest=json.loads((packet/'pilot-manifest.json').read_text())
inputs=0
for cell in manifest['cells']:
    directory=packet/'results'/cell['id']
    with (directory/'assignments.tsv').open() as f:
        rows=list(csv.DictReader(f,delimiter='\t'))
    assert len(rows)==200 and [int(r['dataset_index']) for r in rows]==list(range(200))
    for row in rows:
        s=seed_record('pilot',cell['id'],int(row['dataset_index']))
        assert s['root_seed64']==row['root_seed64'] and s['sha256_utf8']==row['sha256_utf8']
        assert s['child_seeds64']['noise']==row['noise_seed64']
        assert s['r_noise_state']==[int(row['r_state'+str(i)]) for i in range(1,7)]
    hashes=json.loads((directory/'input-sha256.json').read_text())
    assert len(hashes)==200
    with tarfile.open(directory/'inputs.tar.gz','r:gz') as archive:
        for name,digest in hashes.items():
            raw=archive.extractfile(Path(name).name).read()
            assert hashlib.sha256(raw).hexdigest()==digest
            i=int(Path(name).stem.split('-')[1])
            expected=['case','pilot',cell['id'],str(i),rows[i]['root_seed64'],'199',str(cell['parameters']['n'])]
            assert raw.decode().splitlines()[0].split('\t')==expected
            inputs+=1
receipt=dict(cells=len(manifest['cells']),datasets=inputs,seed_assignments='exact',archived_input_hashes='exact',
             case_headers='exact',model_fits_performed=0,
             auditor_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest())
(packet/'input-audit.json').write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(receipt))
