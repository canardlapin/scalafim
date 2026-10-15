#!/usr/bin/env python3
"""Archive completed evidence and bind its bytes; this script runs no experiments."""
import gzip
import hashlib
import json
import subprocess
import tarfile
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
read = lambda name: json.loads((packet/name).read_text())
manifest = read('probe-manifest.json')
for path,expected in manifest['source_locks'].items():
    assert sha(root/path)==expected,path
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()=='72847e147efd8cae0e619e8e2fecd09ab5b4852c'
provider = Path('/private/tmp/multivar-umvpa-rank-v2-20261008')
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=provider,text=True).strip()==manifest['provider_revision']
assert not subprocess.check_output(['git','status','--porcelain'],cwd=provider,text=True).strip()
probe = read('probe/process-receipt.json')
assert probe['all_assignments_completed'] and probe['evaluated']==16 and probe['source_locks_unchanged']
assert probe['worker_exit']==0 and probe['resource_refusal'] is None
for info in read('prior-evidence-check.json').values():
    assert not info['source_mismatch'] and not info['artifact_mismatch']
inputs = read('probe/input-sha256.json')
with tarfile.open(packet/'probe/inputs.tar.gz','x:gz',compresslevel=6) as archive:
    for name,expected in inputs.items():
        path=packet/'probe/inputs'/name
        assert sha(path)==expected
        archive.add(path,arcname=name)
with tarfile.open(packet/'probe/inputs.tar.gz','r:gz') as archive:
    assert len(archive.getmembers())==8
    for name,expected in inputs.items():
        assert hashlib.sha256(archive.extractfile(name).read()).hexdigest()==expected
for name in inputs:
    (packet/'probe/inputs'/name).unlink()
(packet/'probe/inputs').rmdir()
logs=read('log-inventory.json')
for path in sorted(packet.rglob('*.log')):
    raw=path.read_bytes();target=path.with_suffix('.log.gz')
    with target.open('xb') as out:out.write(gzip.compress(raw,mtime=0))
    assert gzip.decompress(target.read_bytes())==raw
    logs[str(path.relative_to(packet))]=dict(uncompressed_sha256=hashlib.sha256(raw).hexdigest(),
        bytes=len(raw),archive=str(target.relative_to(packet)))
    path.unlink()
(packet/'log-inventory.json').write_text(json.dumps(logs,indent=2)+'\n')
for original,info in logs.items():
    assert hashlib.sha256(gzip.decompress((packet/info['archive']).read_bytes())).hexdigest()==info['uncompressed_sha256']
versions={}
for label,command in [('python',['python3','--version']),('R',['Rscript','--version']),
                      ('java',['java','-version']),('node',['node','--version']),('git',['git','--version'])]:
    p=subprocess.run(command,capture_output=True,text=True)
    assert p.returncode==0
    versions[label]=(p.stdout+p.stderr).strip()
receipt=dict(status='implementation-and-resource-probe-complete-publication-pending',
    consumer_source_and_probe_freeze='72847e147efd8cae0e619e8e2fecd09ab5b4852c',
    consumer_base='edad5bcd65f7ba004458a6dd08c5fb3a31a3e6d3',provider_revision=manifest['provider_revision'],
    provider_parent='790e1bad390ccd95d15d9ed033ceecaed56fa307',
    public_consumer_pin='ab811e257dd67f77e8c3b70cb1ea600f274429a3',
    local_provider_override_required=True,default_build_integration_complete=False,
    upstream_published=False,rank_admission='PendingFrozenProtocol',
    method_identities=manifest['method_identities'],
    compile_all=read('consumer-compile-all.json'),consumer_test_process=read('consumer-final.json'),
    consumer_tests=read('consumer-tests.json'),provider_tests=read('multivar-tests.json'),
    provider_api_checks=read('multivar-api-checks.json'),
    provider_smoke='smokeCheck succeeded; retained in multivar-js-smoke.log.gz',
    resource_probe=probe,resource_summary='probe-summary.json',
    new_resource_fixture_datasets=8,new_pilot_datasets=0,confirmation_consumed=False,
    statistical_qualification=False,paired_campaign_resource_admission=False,
    prior_evidence=read('prior-evidence-check.json'),runtime_versions=versions,
    source_locks=manifest['source_locks'],
    artifact_locks={str(p.relative_to(root)):sha(p) for p in sorted(packet.rglob('*'))
                    if p.is_file() and p.name!='execution.json'})
with (packet/'execution.json').open('x') as out:out.write(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(dict(status=receipt['status'],source_locks=len(receipt['source_locks']),
    artifact_locks=len(receipt['artifact_locks']),execution_sha256=sha(packet/'execution.json'))))
