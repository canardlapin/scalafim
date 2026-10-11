#!/usr/bin/env python3
"""Execute a committed, one-use three-method exploratory pilot, never confirmation."""
import argparse
import gzip
import itertools
import json
import os
from pathlib import Path
import subprocess
import tarfile
import time
from protocol import *
from probe import environment, lock_file
from resources import host_profile, monitor, shutdown


def verify_committed(root,path):
    relative=str(path.resolve().relative_to(root))
    committed=subprocess.check_output(['git','show','HEAD:'+relative],cwd=root)
    if committed!=path.read_bytes():raise ValueError('manifest/assignment must be committed before execution: '+relative)


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--manifest',type=Path,required=True)
    p.add_argument('--historical',type=Path,required=True)
    p.add_argument('--output',type=Path,required=True)
    args=p.parse_args();root=Path(__file__).resolve().parents[3];old=args.historical.resolve()
    manifest_path=args.manifest.resolve();manifest=json.loads(manifest_path.read_text())
    verify_committed(root,manifest_path)
    if manifest['role']!='exploratory-pilot-only' or manifest['namespace']!=NAMESPACE or manifest['methods']!=METHODS or manifest['limits']!=LIMITS:
        raise ValueError('not the frozen exploratory pilot protocol')
    proposal_path=root/'docs/verification/umvpa-rank-integration-20261008/paired-inventory-proposal.json'
    if sha(proposal_path)!=manifest['original_inventory_sha256']:raise ValueError('changed original inventory')
    validate_inventory(manifest['cells'],json.loads(proposal_path.read_text()))
    seeds_path=manifest_path.with_name(manifest['assignments_file']);verify_committed(root,seeds_path)
    if sha(seeds_path)!=manifest['assignments_sha256']:raise ValueError('changed stream inventory')
    assignments=[json.loads(s) for s in gzip.decompress(seeds_path.read_bytes()).splitlines()]
    expected=[assignment('pilot',c['id'],i) for c in manifest['cells'] for i in range(200)]
    if assignments!=expected:raise ValueError('changed seed derivation or assignment inventory')
    if sha(manifest['probe_file'])!=manifest['probe_sha256']:raise ValueError('changed resource preflight')
    if json.loads(Path(manifest['probe_file']).read_text()).get('preflight_pass') is not True:
        raise ValueError('failed or incomplete preflight cannot launch pilot')
    repos={'current':root,'historical':old};snapshots=manifest['sources']
    for name,repo in repos.items():
        verify_sources(repo,snapshots[name]['locks']);verify_runtime(snapshots[name]['runtime']['files'])
        subprocess.run(['git','merge-base','--is-ancestor',snapshots[name]['commit'],'HEAD'],cwd=repo,check=True)
    profile=host_profile()
    if profile['available_bytes']<LIMITS['required_headroom_bytes']:raise ValueError('insufficient host headroom')
    # The durable marker is outside the output directory: choosing a different
    # output path cannot reopen the same frozen stream inventory in this checkout.
    spend=manifest_path.with_name('pilot-spend.json')
    write_once(spend,dict(manifest_sha256=sha(manifest_path),assignments_sha256=sha(seeds_path),
                         state='spent-on-launch',outputs=str(args.output.resolve())))
    out=args.output.resolve();out.mkdir(parents=True,exist_ok=False)
    write_once(out/'host-before.json',profile)
    scratch=root/'target/rank-paired-pilot';scratch.mkdir(parents=True,exist_ok=False)
    inputs=scratch/'cases.txt';inputs.touch(exist_ok=False)
    envs={};records={};locks={}
    for name,repo in repos.items():
        locks[name]=out/(name+'-source-lock.'+('tsv' if name=='historical' else 'json'))
        lock_file(locks[name],snapshots[name]['locks'],name=='historical')
        records[name]=scratch/(name+'-records.jsonl');records[name].touch(exist_ok=False)
        envs[name]=environment(repo,snapshots[name]['commit'],locks[name],inputs,records[name])
    source_receipts={name:{'commit':s['commit'],'provider_revision':s['provider_revision'],
        'source_lock_sha256':sha(locks[name]),'runtime_ordered_identity':s['runtime']['ordered_identity']} for name,s in snapshots.items()}
    write_once(out/'executed-sources.json',source_receipts)
    started=time.monotonic();deadline=started+LIMITS['campaign_wall_seconds'];completed=[];failure=None
    key=lambda c:tuple(c[k] for k in ('n','p','q','nuisance'))
    assigned={c['id']:[s for s in assignments if s['scenario_id']==c['id']] for c in manifest['cells']}
    with (out/'lifecycle.log').open('x') as lifecycle:
        try:
            for name,repo in repos.items():shutdown(repo,lifecycle,envs[name])
            for group,cells_iter in itertools.groupby(manifest['cells'],key):
                cells=list(cells_iter);bound_inputs={}
                for c in cells:
                    if time.monotonic()>deadline:raise TimeoutError('campaign cap before generation')
                    destination=out/c['id'];destination.mkdir()
                    table=destination/'assignments.tsv'
                    with table.open('x') as t:
                        t.write('phase\tscenario_id\tdataset_index\troot_seed64\tsha256_utf8\tnoise_seed64\t'+
                                '\t'.join('r_state'+str(i) for i in range(1,7))+'\n')
                        for s in assigned[c['id']]:
                            values=[s[k] for k in ('phase','scenario_id','dataset_index','root_seed64','sha256_utf8','noise_seed64')]+s['r_noise_state']
                            t.write('\t'.join(map(str,values))+'\n')
                    data=scratch/c['id'];generation_started=time.monotonic()
                    cmd=['Rscript','tools/mvpa-inference/paired/generate.R',str(manifest_path),c['id'],str(table),str(data)]
                    with (destination/'generator.log').open('x') as log:
                        generated=subprocess.run(cmd,cwd=root,env=envs['current'],stdout=log,stderr=subprocess.STDOUT,
                            timeout=max(1,min(120,deadline-time.monotonic())))
                    write_once(destination/'generation.json',dict(command=cmd,exit_code=generated.returncode,
                        elapsed_seconds=time.monotonic()-generation_started))
                    if generated.returncode:raise RuntimeError('generation failure retained')
                    paths=sorted(data.glob('dataset-*.tsv'))
                    if len(paths)!=200:raise ValueError('incomplete input generation')
                    bound=[dict(path=str(p),sha256=sha(p),root_seed64=assigned[c['id']][i]['root_seed64']) for i,p in enumerate(paths)]
                    write_once(destination/'input-sha256.json',bound)
                    with tarfile.open(destination/'inputs.tar.gz','w:gz',compresslevel=6) as archive:
                        for path in sorted(data.iterdir()):archive.add(path,arcname=path.name)
                    bound_inputs[c['id']]=bound
                for name in ('historical','current'):
                    repo=repos[name];other=old if name=='current' else root
                    suite='RankHistoricalPairedPilotSuite' if name=='historical' else 'RankPairedPilotSuite'
                    verify_sources(repo,snapshots[name]['locks']);verify_runtime(snapshots[name]['runtime']['files'])
                    for c in cells:
                        if time.monotonic()>deadline:raise TimeoutError('campaign cap before cell')
                        destination=out/c['id'];bound=bound_inputs[c['id']]
                        for b in bound:
                            if sha(b['path'])!=b['sha256']:raise ValueError('archived input changed')
                        inputs.write_text(''.join(b['path']+'\n' for b in bound))
                        offset=records[name].stat().st_size
                        cmd=['python3','tools/build/sbt-warm',f'mvpaJVM/testOnly *{suite}']
                        with (destination/(name+'.log')).open('x') as log:
                            receipt=monitor(repo,cmd,envs[name],log,LIMITS,deadline,other)
                        with records[name].open('rb') as f:f.seek(offset);raw=f.read()
                        (destination/(name+'-records.jsonl.gz')).write_bytes(gzip.compress(raw,mtime=0))
                        rows=[json.loads(s) for s in raw.splitlines()]
                        receipt['record_count']=len(rows)
                        write_once(destination/(name+'-process.json'),receipt)
                        failures=validate_rows(rows,c,name,snapshots[name]['commit'],sha(locks[name]),bound)
                        verify_sources(repo,snapshots[name]['locks'])
                        for b in bound:
                            if sha(b['path'])!=b['sha256']:raise ValueError('input changed during fit')
                        if receipt['worker_exit'] or receipt['resource_refusal'] or failures:
                            raise RuntimeError('failed cell retained; no retry: '+c['id']+'/'+name+': '+str(failures[:3]))
                        completed.append((c['id'],name))
                        print(json.dumps(dict(cell=c['id'],provider=name,records=len(rows),seconds=receipt['elapsed_seconds'],
                            rss=receipt['peak_rss_bytes'],completed_provider_cells=len(completed))),flush=True)
                    shutdown(repo,lifecycle,envs[name])
            for name,repo in repos.items():
                verify_sources(repo,snapshots[name]['locks']);verify_runtime(snapshots[name]['runtime']['files'])
        except BaseException as error:
            failure=type(error).__name__+': '+str(error)
            raise
        finally:
            stop_errors=[]
            for name,repo in repos.items():
                try:shutdown(repo,lifecycle,envs[name])
                except Exception as error:stop_errors.append(str(error))
            expected_slots=[(c['id'],name) for c in manifest['cells'] for name in ('historical','current')]
            missing=[s for s in expected_slots if s not in completed]
            write_once(out/'campaign.json',dict(source_manifest_sha256=sha(manifest_path),
                completed_provider_cells=completed,missing_provider_cells=missing,
                elapsed_seconds=time.monotonic()-started,limits=LIMITS,failure=failure,shutdown_errors=stop_errors,
                complete=not missing and failure is None and not stop_errors and time.monotonic()<=deadline,
                scientific_admission=False))
            if stop_errors and failure is None:raise RuntimeError('shutdown failure: '+str(stop_errors))

if __name__=='__main__':main()
