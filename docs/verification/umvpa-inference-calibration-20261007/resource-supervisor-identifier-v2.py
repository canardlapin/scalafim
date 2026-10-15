from pathlib import Path
import argparse, hashlib, json, os, signal, subprocess, time

p=argparse.ArgumentParser()
p.add_argument('--worktree',required=True)
p.add_argument('--batch',required=True)
p.add_argument('--phase',required=True,choices=['fixture','pilot'])
p.add_argument('--attempt',default='')
args=p.parse_args()
root=Path(args.worktree)
inputs=root/'target/umvpa-calibration'/args.batch/'case-files.txt'
label=args.batch+('-'+args.attempt if args.attempt else '')
output=root/'target/umvpa-calibration'/(label+'-records.jsonl')
packet=root/'docs/verification/umvpa-inference-calibration-20261007'/('run-'+label)
packet.mkdir(exist_ok=True)
assert inputs.exists() and not output.exists(), 'Inputs must exist and output must be fresh.'
assert not (packet/'process-receipt.json').exists(), 'Receipt is write-once.'
env=os.environ.copy()
env.update(SBT_WARM_HEAP='2g',SBT_WARM_CPUS='4',SCALAFIM_CALIBRATION_CASE_LIST=str(inputs),SCALAFIM_CALIBRATION_OUTPUT=str(output))
cmd=['python3','tools/build/sbt-warm','mvpaJVM/testOnly scalafim.fmri.mvpa.inference.RankConfirmationSuite']
commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()
started=time.monotonic(); peak=0; samples=0; server_pid=None; violation=None
with (packet/'worker.log').open('xb') as log:
    worker=subprocess.Popen(cmd,cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
    try:
        while worker.poll() is None:
            if time.monotonic()-started>900:
                violation='wall_limit'; break
            active=root/'project/target/active.json'
            if server_pid is None and active.exists():
                uri=json.loads(active.read_text()).get('uri','')
                if uri.startswith('local://'):
                    owned_socket=Path(uri[len('local://'):])
                    found=subprocess.run(['lsof','-t','-a','-U',str(owned_socket)],text=True,capture_output=True)
                    ids={int(v) for v in found.stdout.split() if v.isdigit()}
                    if len(ids)==1: server_pid=ids.pop()
            if server_pid is not None:
                rss=subprocess.run(['ps','-p',str(server_pid),'-o','rss='],text=True,capture_output=True,check=True)
                value=rss.stdout.strip()
                if value:
                    peak=max(peak,int(value)*1024); samples+=1
                    if peak>3*1024**3: violation='rss_limit'; break
            time.sleep(.2)
    except Exception as error:
        violation='monitoring_error: '+str(error)
    if violation:
        if server_pid is not None:
            try: os.kill(server_pid,signal.SIGTERM)
            except ProcessLookupError: pass
        try: os.killpg(worker.pid,signal.SIGTERM)
        except ProcessLookupError: pass
    try: code=worker.wait(timeout=15)
    except subprocess.TimeoutExpired:
        os.killpg(worker.pid,signal.SIGKILL); code=worker.wait()
if server_pid is None or samples==0:
    violation=violation or 'server_pid_or_rss_unobserved'
records=output.read_text().splitlines() if output.exists() else []
sha=lambda path:hashlib.sha256(path.read_bytes()).hexdigest()
receipt={'phase':args.phase,'batch':args.batch,'source_commit':commit,'command':cmd,'input_list':str(inputs.relative_to(root)),
    'input_sha256':sha(inputs),'supervisor_sha256':sha(Path(__file__)),'elapsed_seconds':time.monotonic()-started,
    'server_pid':server_pid,'rss_samples':samples,'observed_peak_rss_bytes':peak,'worker_exit':code,
    'resource_limits':{'heap':'2g','cpus':4,'max_rss_bytes':3*1024**3,'max_wall_seconds':900,'worker_processes':1},
    'resource_refusal':violation,'records_written':len(records),'scientific_release':'unavailable',
    'status':'completed' if code==0 and not violation else 'failed_or_resource_inconclusive'}
if output.exists():
    receipt['records_sha256']=sha(output)
    parsed=[json.loads(line) for line in records]
    receipt['outcome_status_counts']={status:sum(row.get('status')==status for row in parsed) for status in sorted({row.get('status','missing') for row in parsed})}
    receipt['expected_records']=len([line for line in inputs.read_text().splitlines() if line.strip()])
    receipt['outcome_scope']='Process completion alone is not dataset success or scientific qualification.'
with (packet/'process-receipt.json').open('x') as f: json.dump(receipt,f,indent=2);f.write('\n')
print(json.dumps(receipt))
raise SystemExit(0 if code==0 and not violation else 1)
