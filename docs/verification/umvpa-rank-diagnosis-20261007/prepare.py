#!/usr/bin/env python3
"""Expose already-observed pilot inputs to diagnostics; no generator or RNG."""
import csv,gzip,hashlib,json,tarfile
from pathlib import Path
root=Path(__file__).resolve().parents[3]
packet=Path(__file__).resolve().parent
prior=root/'docs/verification/umvpa-rank-pilot-20261007'
manifest=json.loads((prior/'pilot-manifest.json').read_text())
old=json.loads((prior/'execution.json').read_text())
for kind in ('source_locks','artifact_locks'):
    for name,digest in old[kind].items(): assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest,name
scratch=root/'target/umvpa-rank-diagnosis'
scratch.mkdir(parents=True,exist_ok=False)
index=[]; selected=[]; closure=[]
for cell in manifest['cells']:
    directory=prior/'results'/cell['id']
    records=[json.loads(line) for line in gzip.decompress((directory/'records.jsonl.gz').read_bytes()).splitlines()]
    raw=[sum(row['raw_reject'][k] for row in records) for k in range(4)]
    closed=[sum(row['closed_reject'][k] for row in records) for k in range(4)]
    closure.append(dict(cell=cell['id'],datasets=200,raw_counts=raw,closed_counts=closed,
                        blocked_by_closure=[a-b for a,b in zip(raw,closed)]))
    hashes=json.loads((directory/'input-sha256.json').read_text())
    destination=scratch/cell['id']; destination.mkdir()
    rank=sum(r>0 for r in cell['parameters']['correlations'])
    with tarfile.open(directory/'inputs.tar.gz','r:gz') as archive:
        for row in records:
            ordinal=row['dataset_index']; name=f'dataset-{ordinal:05d}.tsv'
            raw=archive.extractfile(name).read()
            digest=hashes['target/umvpa-expanded-pilot/'+cell['id']+'/'+name]
            assert hashlib.sha256(raw).hexdigest()==digest
            path=destination/name;path.write_bytes(raw)
            entry=dict(cell=cell['id'],ordinal=ordinal,path=str(path),rank=rank,rate_class=cell['rate_class'],
                       n=cell['parameters']['n'],p=cell['parameters']['p'],q=cell['parameters']['q'],
                       nuisance=cell['parameters']['nuisance'],sha256=digest)
            for k in range(4):
                entry['raw'+str(k+1)]=row['raw_p_values'][k]
                entry['closed'+str(k+1)]=row['closed_p_values'][k]
            index.append(entry)
            if rank==3 and ordinal==0: selected.append(str(path))
assert len(index)==12200 and len(selected)==16
with (packet/'case-index.tsv').open('x') as output:
    writer=csv.DictWriter(output,fieldnames=list(index[0]),delimiter='\t',lineterminator='\n')
    writer.writeheader();writer.writerows(index)
(packet/'replay-case-files.txt').write_text('\n'.join(selected)+'\n')
(packet/'closure-audit.json').write_text(json.dumps(closure,indent=2)+'\n')
print('Prepared12200 existing inputs;16 fixed ordinal-zero R3 cases for cross-platform algebra only; no new draws')
