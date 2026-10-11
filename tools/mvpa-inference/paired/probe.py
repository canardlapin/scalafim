#!/usr/bin/env python3
"""Replay only the eight retained resource fixtures; never open pilot streams."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tarfile
import time
from protocol import *
from resources import host_profile, monitor, shutdown


def environment(root,commit,lock,case_list,output):
    env=dict(os.environ,SBT_WARM_HEAP='3g',SBT_WARM_CPUS='4',LC_ALL='C',LANG='C',
        SCALAFIM_RANK_PAIRED_LIST=str(case_list),SCALAFIM_RANK_PAIRED_OUTPUT=str(output),
        SCALAFIM_CALIBRATION_SOURCE_ROOT=str(root),SCALAFIM_CALIBRATION_SOURCE_COMMIT=commit,
        SCALAFIM_CALIBRATION_SOURCE_LOCK=str(lock),OMP_NUM_THREADS='1',OPENBLAS_NUM_THREADS='1',VECLIB_MAXIMUM_THREADS='1')
    for k in ('JAVA_TOOL_OPTIONS','JDK_JAVA_OPTIONS','SBT_OPTS','JAVA_OPTS',
              'SCALAFIM_CALIBRATION_CASE_FILE','SCALAFIM_CALIBRATION_CASE_LIST','SCALAFIM_CALIBRATION_OUTPUT'):
        env.pop(k,None)
    return env


def lock_file(path,locks,historical=False):
    with path.open('x') as out:
        if historical: out.write(''.join(k+'\t'+v+'\n' for k,v in sorted(locks.items())))
        else: out.write(json.dumps(locks,sort_keys=True,indent=2)+'\n')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--historical',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args();root=Path(__file__).resolve().parents[3];old=args.historical.resolve()
    out=args.output.resolve();out.mkdir(parents=True,exist_ok=False)
    prior=root/'docs/verification/umvpa-rank-v2-20261008'
    archive=prior/'probe/inputs.tar.gz'
    identities=json.loads((root/'docs/verification/umvpa-rank-integration-20261008/historical/retained-input-sha256.json').read_text())
    cases=json.loads((prior/'probe-manifest.json').read_text())['cells']
    inputs=out/'inputs';inputs.mkdir()
    with tarfile.open(archive,'r:gz') as t:
        if set(t.getnames())!=set(identities): raise ValueError('unexpected retained input inventory')
        for name,digest in identities.items():
            if Path(name).name!=name: raise ValueError('unsafe archive name')
            data=t.extractfile(name).read()
            if hashlib.sha256(data).hexdigest()!=digest: raise ValueError('changed retained input')
            (inputs/name).write_bytes(data)
    paths=[inputs/(c['id']+'.tsv') for c in cases]
    case_list=out/'cases.txt';case_list.write_text(''.join(str(p)+'\n' for p in paths))
    profile=host_profile();write_once(out/'host.json',profile)
    if profile['available_bytes']<LIMITS['required_headroom_bytes']:raise ValueError('host headroom insufficient')
    receipts={};records={};snapshots={}
    with (out/'lifecycle.log').open('x') as lifecycle:
        for repo in (root,old):shutdown(repo,lifecycle,environment(repo,'0'*40,out/'unused',case_list,out/'unused-output'))
        for name,repo,other in [('historical',old,root),('current',root,old)]:
            locks=source_locks(repo);commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=repo,text=True).strip()
            if subprocess.check_output(['git','diff','HEAD','--name-only','--',*locks],cwd=repo,text=True).strip():raise ValueError('source not committed')
            snapshot=dict(commit=commit,locks=locks);snapshots[name]=snapshot
            write_once(out/(name+'-source.json'),snapshot)
            lock=out/(name+'-source-lock.'+('tsv' if name=='historical' else 'json'))
            lock_file(lock,locks,name=='historical')
            suite='RankHistoricalPairedPilotSuite' if name=='historical' else 'RankPairedPilotSuite'
            for platform in ('JVM','JS'):
                output=out/(name+'-'+platform+'.jsonl')
                env=environment(repo,commit,lock,case_list,output)
                # The output path changes between platforms, so stop the server
                # and bind that environment before launching either worker.
                shutdown(repo,lifecycle,env)
                cmd=['python3','tools/build/sbt-warm',f'mvpa{platform}/testOnly *{suite}']
                with (out/(name+'-'+platform+'.log')).open('x') as log:
                    receipt=monitor(repo,cmd,env,log,LIMITS,time.monotonic()+900,other)
                write_once(out/(name+'-'+platform+'-process.json'),receipt)
                rows=[json.loads(line) for line in output.read_text().splitlines()] if output.exists() else []
                failures=[]
                for c in cases:
                    selected=[r for r in rows if r['scenario_id']==c['id']]
                    failures += validate_rows(selected,c,name,commit,sha(lock),
                        [dict(sha256=identities[c['id']+'.tsv'],root_seed64=c['root_seed64'])],phase='fixture')
                expected=8 if name=='historical' else 16
                if len(rows)!=expected or failures or receipt['worker_exit'] or receipt['resource_refusal']:
                    raise RuntimeError('fixture replay failed; receipts retained')
                verify_sources(repo,locks)
                records[(name,platform)]=rows;receipts[(name,platform)]=receipt
                shutdown(repo,lifecycle,env)
    max_gap=0.
    for name in ('historical','current'):
        index=lambda rows:{(r['scenario_id'],r['rank_method']):r for r in rows}
        a=index(records[(name,'JVM')]);b=index(records[(name,'JS')])
        if a.keys()!=b.keys():raise ValueError('platform slot mismatch')
        for key,r in a.items():
            for field in ('raw_p_values','closed_p_values','raw_exceedances','raw_reject','closed_reject','receipt_seed64'):
                if r[field]!=b[key][field]:raise ValueError('platform decisions differ')
            for field in ('observed_roots','observed_statistics'):
                max_gap=max(max_gap,max(abs(x-y)/(1+abs(x)) for x,y in zip(r[field],b[key][field])))
    if max_gap>1e-10:raise ValueError('platform numeric mismatch')
    # The prior oracle packets test observed roots and full small-group actions;
    # also verify that the same retained fixture has the same roots in both
    # historical/current providers before measuring mechanism differences.
    h={r['scenario_id']:r for r in records[('historical','JVM')]}
    for r in records[('current','JVM')]:
        if max(abs(x-y) for x,y in zip(r['observed_roots'],h[r['scenario_id']]['observed_roots']))>1e-10:
            raise ValueError('provider observed roots differ')
    rows=records[('historical','JVM')]+records[('current','JVM')]
    measured={(r['scenario_id'],r['rank_method']):r['elapsed_seconds'] for r in rows}
    cells=inventory(json.loads((root/'docs/verification/umvpa-rank-integration-20261008/paired-inventory-proposal.json').read_text()))
    estimated=0.;max_cell=0.
    for c in cells:
        proxy=next(x for x in cases if x['n']==(80 if c['n']==80 else 640) and
                   (x['p'],x['q'],x['nuisance'])==(c['p'],c['q'],c['nuisance']))
        times=[200*measured[(proxy['id'],m)] for m in METHODS]
        estimated+=sum(times);max_cell=max(max_cell,times[0],sum(times[1:]))
    overhead=max(receipts[(name,'JVM')]['elapsed_seconds']-sum(r['elapsed_seconds'] for r in records[(name,'JVM')]) for name in ('historical','current'))
    sensitivity=estimated+32*max(overhead,60)+1200 # fixed generation/serialization allowance
    passed=sensitivity<LIMITS['campaign_wall_seconds'] and max_cell+max(overhead,60)<900
    summary=dict(preflight_pass=passed,new_datasets=0,retained_unique_inputs=8,records_per_platform=24,
        max_platform_scaled_difference=max_gap,kernel_cost_sensitivity_seconds=estimated,
        measured_startup_overhead_seconds=overhead,aggregate_planning_seconds=sensitivity,
        maximum_cell_planning_seconds=max_cell+max(overhead,60),limits=LIMITS,
        cost_scope='Sensitivity from eight retained fixtures; n160/320 use n640 costs. Not a runtime guarantee. Hard monitors govern the actual campaign.',
        source_snapshots=snapshots,input_archive_sha256=sha(archive))
    write_once(out/'preflight.json',summary)
    print(json.dumps({k:v for k,v in summary.items() if k!='source_snapshots'}))
    if not passed:raise SystemExit(1)

if __name__=='__main__':main()
