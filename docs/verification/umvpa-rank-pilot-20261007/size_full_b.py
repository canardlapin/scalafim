#!/usr/bin/env python3
"""Eight fresh fixture streams size B1999 across the eight rank design shapes."""
import gzip
import json
import os
from pathlib import Path
import subprocess
import sys
import tarfile
import time
root=Path(__file__).resolve().parents[3]
sys.path.insert(0,str(root/'tools/mvpa-inference'))
from calibration_protocol import file_locks, seed_record, summarize, validate_manifest
from run_calibration import write_once
from run_rank_pilot import monitor, shutdown
packet=Path(__file__).resolve().parent
manifest_path=packet/'full-b-sizing-manifest.json'
manifest=json.loads(manifest_path.read_text())
assert file_locks(root,list(manifest['source_locks']))==manifest['source_locks']
validate_manifest(manifest,root,'fixture')
assert len(manifest['cells'])==8 and all(c['fixture_nonidentity_draws']==1999 for c in manifest['cells'])
output=packet/'full-b-sizing'
output.mkdir(exist_ok=False)
scratch=root/'target/umvpa-full-b-sizing'
scratch.mkdir(parents=True,exist_ok=False)
case_list=scratch/'case-files.txt'; records=scratch/'records.jsonl'
paths=[]
env=dict(os.environ,LC_ALL='C',LANG='C',SBT_WARM_HEAP='2g',SBT_WARM_CPUS='4',
         SCALAFIM_CALIBRATION_CASE_LIST=str(case_list),SCALAFIM_CALIBRATION_OUTPUT=str(records))
env.pop('SCALAFIM_CALIBRATION_CASE_FILE',None)
commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()
started=time.monotonic()
for cell in manifest['cells']:
    directory=scratch/cell['id']
    assignments=scratch/(cell['id']+'.tsv')
    s=seed_record('fixture',cell['id'],0)
    with assignments.open('x') as out:
        out.write('phase\tscenario_id\tdataset_index\troot_seed64\tsha256_utf8\tnoise_seed64\t'+'\t'.join('r_state'+str(i) for i in range(1,7))+'\n')
        out.write('\t'.join(map(str,['fixture',cell['id'],0,s['root_seed64'],s['sha256_utf8'],s['child_seeds64']['noise']]+s['r_noise_state']))+'\n')
    cmd=['/usr/local/bin/Rscript','tools/mvpa-inference/generate_known_truth.R','--emit-case-batch',
         '--manifest',str(manifest_path),'--cell',cell['id'],'--seed-records',str(assignments),
         '--phase','fixture','--draws','1999','--out-dir',str(directory)]
    with (output/(cell['id']+'.log')).open('xb') as log:
        subprocess.run(cmd,cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=120)
    paths.append(directory/'dataset-00000.tsv')
case_list.write_text(''.join(str(path)+'\n' for path in paths))
with tarfile.open(output/'inputs.tar.gz','w:gz') as archive:
    for path in sorted(scratch.rglob('*')):
        if path.is_file(): archive.add(path,arcname=str(path.relative_to(scratch)))
write_once(output/'input-sha256.json',file_locks(root,[str(path.relative_to(root)) for path in paths]))
cmd=['python3','tools/build/sbt-warm','mvpaJVM/testOnly scalafim.fmri.mvpa.inference.RankConfirmationSuite']
with (output/'server-lifecycle.log').open('xb') as lifecycle:
    shutdown(root,lifecycle)
    try:
        with (output/'worker.log').open('xb') as log:
            receipt=monitor(root,cmd,env,log,manifest['resource_approval'])
        raw=records.read_bytes() if records.exists() else b''
        with gzip.open(output/'records.jsonl.gz','xb') as target: target.write(raw)
        rows=[json.loads(line) for line in raw.splitlines()]
        summaries=[summarize(cell,'fixture',[r for r in rows if r['scenario_id']==cell['id']]) for cell in manifest['cells']]
        write_once(output/'summaries.json',summaries)
        assert file_locks(root,list(manifest['source_locks']))==manifest['source_locks']
        receipt.update(source_commit=commit,expected_records=8,records_written=len(rows),
                       total_elapsed_seconds=time.monotonic()-started,phase='fixture',
                       scope='One full-B sizing observation per shape; not a calibration or reference benchmark',
                       source_locks=manifest['source_locks'],scientific_release='unavailable')
        write_once(output/'process-receipt.json',receipt)
        assert receipt['worker_exit']==0 and not receipt['resource_refusal']
        assert all(s['outcome']=='ready-for-independent-rate-adjudication' for s in summaries)
        print(json.dumps(receipt))
    finally:
        shutdown(root,lifecycle)
