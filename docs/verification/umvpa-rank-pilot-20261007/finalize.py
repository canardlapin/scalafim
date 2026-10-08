#!/usr/bin/env python3
"""Seal completed raw evidence without changing its contents or scientific status."""
import gzip
import hashlib
import json
from pathlib import Path
import sys
root=Path(__file__).resolve().parents[3]
sys.path.insert(0,str(root/'tools/mvpa-inference'))
from calibration_protocol import file_locks
packet=Path(__file__).resolve().parent
manifest=json.loads((packet/'pilot-manifest.json').read_text())
campaign=json.loads((packet/'results/campaign-receipt.json').read_text())
sizing=json.loads((packet/'full-b-sizing/process-receipt.json').read_text())
assert len(campaign['completed_cells'])==61 and not campaign['missing_cells']
assert sizing['records_written']==8 and sizing['worker_exit']==0 and not sizing['resource_refusal']
assert file_locks(root,list(manifest['source_locks']))==manifest['source_locks']
for name,digest in manifest['prerequisite_evidence_locks'].items():
    assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest
for path in list(packet.rglob('*.log'))+[packet/'oracle-generation.txt']:
    if path.exists():
        destination=path.with_name(path.name+'.gz')
        raw=path.read_bytes()
        with gzip.open(destination,'xb') as output: output.write(raw)
        assert gzip.decompress(destination.read_bytes())==raw
        path.unlink()
artifacts=sorted(str(path.relative_to(root)) for path in packet.rglob('*') if path.is_file() and path.name!='execution.json')
execution=dict(schema=1,pilot_source_commit=campaign['source_commit'],
    full_b_source_commit=sizing['source_commit'],source_locks=manifest['source_locks'],
    artifact_locks=file_locks(root,artifacts),
    engineering={'mvpaJVM':{'passed':464,'skipped':1},'mvpaJS':{'passed':464,'skipped':1},
                 'independent_rank_oracles_per_platform':6,'python_controls_passed':16},
    pilot=json.loads((packet/'pilot-overview.json').read_text()),
    input_audit=json.loads((packet/'input-audit.json').read_text()),
    scientific_status='unavailable: pilot calibration/power warnings and uncompleted confirmation prerequisites',
    reference_performance='unavailable: diagnostic host/workload only',
    historical_pilots_rerun=False,confirmation_consumed=False)
(packet/'execution.json').write_text(json.dumps(execution,indent=2)+'\n')
print(json.dumps({'source_locks':len(execution['source_locks']),'artifact_locks':len(artifacts),
    'execution_sha256':hashlib.sha256((packet/'execution.json').read_bytes()).hexdigest()}))
