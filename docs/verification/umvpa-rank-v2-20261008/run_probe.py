#!/usr/bin/env python3
"""Execute the committed eight-fixture resource probe, with no retries."""
import csv
import datetime
import hashlib
import importlib.util
import json
import os
import re
import subprocess
import sys
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
sys.path.insert(0,str(root/'tools/mvpa-inference'))
from run_rank_pilot import monitor

manifest = json.loads((packet/'probe-manifest.json').read_text())
assert manifest['status']=='source-frozen-resource-fixtures-only'
for relative,expected in manifest['source_locks'].items():
    assert hashlib.sha256((root/relative).read_bytes()).hexdigest()==expected,relative
committed=subprocess.check_output(['git','show','HEAD:'+str((packet/'probe-manifest.json').relative_to(root))],cwd=root)
assert committed==(packet/'probe-manifest.json').read_bytes(),'probe manifest must be committed before generation'
provider=Path('/private/tmp/multivar-umvpa-rank-v2-20261008')
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=provider,text=True).strip()==manifest['provider_revision']
assert not subprocess.check_output(['git','status','--porcelain'],cwd=provider,text=True).strip()
out=packet/'probe';out.mkdir(exist_ok=False)
vm=subprocess.check_output(['vm_stat'],text=True)
page=int(re.search(r'page size of (\d+) bytes',vm).group(1))
pages={name:int(value) for name,value in re.findall(r'^([^:]+):\s+(\d+)\.',vm,re.M)}
available=sum(pages.get(name,0) for name in ('Pages free','Pages inactive','Pages speculative'))*page
java=[]
for row in subprocess.check_output(['ps','-axo','pid=,rss=,comm='],text=True).splitlines():
    fields=row.split(maxsplit=2)
    if len(fields)==3 and Path(fields[2]).name=='java':
        java.append(dict(pid=int(fields[0]),rss_bytes=int(fields[1])*1024))
host=dict(utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
    physical_memory_bytes=int(subprocess.check_output(['sysctl','-n','hw.memsize'],text=True).strip()),
    free_inactive_speculative_bytes=available,java_processes=java,required_headroom_bytes=8*1024**3)
(out/'host-before.json').write_text(json.dumps(host,indent=2)+'\n')
assert available>=8*1024**3,'insufficient host headroom; no fixtures generated'
with (out/'assignments.tsv').open('x') as stream:
    writer=csv.writer(stream,delimiter='\t',lineterminator='\n')
    writer.writerow(['id','n','p','q','nuisance','root_seed64']+['r_state'+str(i) for i in range(1,7)])
    for c in manifest['cells']:
        writer.writerow([c[k] for k in ('id','n','p','q','nuisance','root_seed64')]+c['r_noise_state'])
environment=dict(os.environ,LC_ALL='C',LANG='C',SBT_WARM_HEAP='3g',SBT_WARM_CPUS='4',
    OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1',
    JAVA_TOOL_OPTIONS='-Dscalafim.multivar.build='+str(provider))
with (out/'generator.log').open('x') as log:
    generated=subprocess.run(['Rscript',str(packet/'generate_probe.R'),str(root),str(out/'assignments.tsv'),str(out/'inputs')],
        cwd=root,env=environment,stdout=log,stderr=subprocess.STDOUT,timeout=120)
assert generated.returncode==0,'generation failed; retained, no replacement'
paths=[out/'inputs'/(c['id']+'.tsv') for c in manifest['cells']]
assert all(p.is_file() for p in paths)
(out/'case-files.txt').write_text(''.join(str(p)+'\n' for p in paths))
(out/'input-sha256.json').write_text(json.dumps({p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},indent=2)+'\n')
environment.update(SCALAFIM_RANK_V2_PROBE_LIST=str(out/'case-files.txt'),SCALAFIM_RANK_V2_PROBE_OUTPUT=str(out/'records.jsonl'))
command=['python3','tools/build/sbt-warm','mvpaJVM/testOnly scalafim.fmri.mvpa.inference.RankV2ResourceProbeSuite']
with (out/'worker.log').open('x') as log:
    receipt=monitor(root,command,environment,log,manifest['limits'])
records=[json.loads(line) for line in (out/'records.jsonl').read_text().splitlines()] if (out/'records.jsonl').exists() else []
expected={(c['id'],m) for c in manifest['cells'] for m in manifest['method_identities']}
actual={(r['scenario_id'],r['rank_method']) for r in records}
receipt.update(records=len(records),all_assignments_completed=len(records)==16 and actual==expected,
    evaluated=sum(r.get('status')=='evaluated' for r in records),source_commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),
    provider_revision=manifest['provider_revision'],scientific_qualification=False)
receipt['source_locks_unchanged'] = all(hashlib.sha256((root/p).read_bytes()).hexdigest()==expected
    for p,expected in manifest['source_locks'].items())
receipt['limits'] = manifest['limits']
receipt['startup_property'] = environment['JAVA_TOOL_OPTIONS']
(out/'process-receipt.json').write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(receipt))
assert receipt['worker_exit']==0 and not receipt['resource_refusal'] and receipt['all_assignments_completed'] and receipt['evaluated']==16 and receipt['source_locks_unchanged']
