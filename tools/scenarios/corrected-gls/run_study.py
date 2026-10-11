#!/usr/bin/env python3
"""Run both platform study targets even when a scientific gate rejects a cell."""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[3]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--profile', choices=['pilot','screen','confirmation'], required=True)
parser.add_argument('--output', type=Path, required=True)
args = parser.parse_args()
out = args.output.resolve()
out.mkdir(parents=True, exist_ok=True)
protocol = ROOT / 'tools/scenarios/corrected-gls/protocol.json'
p = json.loads(protocol.read_text())
source = [ROOT/'build.sbt'] + sorted((ROOT/'modules/first-level-laws').rglob('*.scala'))
source += sorted((ROOT/'tools/scenarios/corrected-gls').glob('*'))
source_hash = {str(f.relative_to(ROOT)): hashlib.sha256(f.read_bytes()).hexdigest() for f in source if f.is_file()}
env = dict(os.environ)
env['SCALAFIM_GLS_STUDY_PROFILE'] = args.profile
env['SCALAFIM_GLS_STUDY_LOG'] = str(out/'records')
env['SCALAFIM_LAW_PROFILE'] = 'calibration' if args.profile == 'confirmation' else 'pull-request'
# Only an idle worktree server may be stopped; sbt-warm refuses active clients.
warm = [sys.executable, str(ROOT/'tools/build/sbt-warm')]
subprocess.run(warm+['--shutdown'], cwd=ROOT, env=env, check=True)
for platform in ['jvm','js']:
 if (out/f'records-{platform}.jsonl').exists():
  raise SystemExit('refusing to overwrite an observed study; use a new output path')
receipt = {'schema_version':'scalafim-corrected-gls-study-run/v1','started_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),
 'profile':args.profile,'protocol_sha256':hashlib.sha256(protocol.read_bytes()).hexdigest(),
 'declared_replicates_per_cell':p['profiles'][args.profile]['replicates'],'source_sha256':source_hash,
 'seed':env.get('SCALAFIM_GLS_STUDY_SEED',env.get('SCALAFIM_LAW_SEED_LONG',str(p['seed']))),
 'targets':[],'source_base':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()}
(out/'run.json').write_text(json.dumps(receipt,indent=2)+'\n')
for platform in ['JVM','JS']:
 command=f'firstLevelLaws{platform}/testOnly scalafim.fmri.laws.CorrectedGlsSimulatorSuite scalafim.fmri.laws.CorrectedGlsQualificationSuite'
 log=out/(platform.lower()+'.log')
 started=datetime.datetime.now(datetime.timezone.utc)
 with log.open('w') as stream:
  result=subprocess.run(warm+[command],cwd=ROOT,env=env,stdout=stream,stderr=subprocess.STDOUT)
 receipt['targets'].append({'target':platform,'command':command,'exit_code':result.returncode,
   'started_at':started.isoformat(),'ended_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),
   'log_sha256':hashlib.sha256(log.read_bytes()).hexdigest()})
 (out/'run.json').write_text(json.dumps(receipt,indent=2)+'\n')
 print(platform,'exit',result.returncode,flush=True)
receipt['ended_at']=datetime.datetime.now(datetime.timezone.utc).isoformat()
receipt['source_unchanged']=all(hashlib.sha256((ROOT/name).read_bytes()).hexdigest()==sha for name,sha in source_hash.items())
(out/'run.json').write_text(json.dumps(receipt,indent=2)+'\n')
if not receipt['source_unchanged']:
 raise SystemExit('study source changed during execution; cannot admit results')
# Outcome is kept separately: a failure is an observed qualification result,
# never a reason to skip the paired platform or replace its seeds.
raise SystemExit(0 if all(x['exit_code']==0 for x in receipt['targets']) else 1)
