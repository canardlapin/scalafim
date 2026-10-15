#!/usr/bin/env python3
"""Execute the separately declared GLS continuous-whitening campaign on both platforms."""
import argparse, datetime, hashlib, json, os, subprocess, sys, tarfile
from pathlib import Path
ROOT = Path(__file__).resolve().parents[3]
p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--profile', choices=['pilot','screen','confirmation'], required=True)
p.add_argument('--output', type=Path, required=True)
a = p.parse_args()
out = a.output.resolve(); out.mkdir(parents=True, exist_ok=True)
if (out/'run.json').exists(): raise SystemExit('Refusing to overwrite an observed campaign')
protocol_path = ROOT/'tools/scenarios/gls-continuity/protocol.json'
protocol = json.loads(protocol_path.read_text())
files = [ROOT/'build.sbt', protocol_path, Path(__file__).resolve(), ROOT/'tools/scenarios/corrected-gls/analyze.R', ROOT/'tools/scenarios/gls-continuity/analyze.R']
for module in ['ar','model','fit','first-level-laws']:
    files += sorted(f for f in (ROOT/'modules'/module).rglob('*.scala') if '/src/' in str(f))
hashes = {str(f.relative_to(ROOT)):hashlib.sha256(f.read_bytes()).hexdigest() for f in files}
env = os.environ.copy()
for key in list(env):
    if key.startswith(('SCALAFIM_GLS_','SCALAFIM_LAW_')): env.pop(key)
env.update(SCALAFIM_GLS_CONTINUITY_PROFILE=a.profile,SCALAFIM_LAW_PROFILE='pull-request',SCALAFIM_GLS_STUDY_SEED=str(protocol['seed']),SCALAFIM_GLS_STUDY_LOG=str(out/'records'),SBT_WARM_HEAP='6g')
warm = [sys.executable,str(ROOT/'tools/build/sbt-warm')]
receipt = dict(profile=a.profile,seed=protocol['seed'],protocol_sha256=hashlib.sha256(protocol_path.read_bytes()).hexdigest(),source_sha256=hashes,targets=[],started_at=datetime.datetime.now(datetime.timezone.utc).isoformat())
def save(): (out/'run.json').write_text(json.dumps(receipt,indent=2)+'\n')
save()
with tarfile.open(out/"frozen-source.tar.gz", "w:gz") as snapshot:
    for file in files: snapshot.add(file, arcname=str(file.relative_to(ROOT)))
receipt["frozen_source_sha256"] = hashlib.sha256((out/"frozen-source.tar.gz").read_bytes()).hexdigest()
save()
for platform in ['JVM','JS']:
    subprocess.run(warm+['--shutdown'],cwd=ROOT,env=env,check=True)
    command=f'firstLevelLaws{platform}/testOnly scalafim.fmri.laws.CorrectedGlsSimulatorSuite scalafim.fmri.laws.GlsContinuityStudySuite'
    with (out/(platform.lower()+'.log')).open('w') as log:
        r=subprocess.run(warm+[command],cwd=ROOT,env=env,stdout=log,stderr=subprocess.STDOUT)
    receipt['targets'].append(dict(platform=platform,exit_code=r.returncode,command=command))
    save()
    print(platform,'exit',r.returncode,flush=True)
receipt['source_unchanged']=all(hashlib.sha256((ROOT/k).read_bytes()).hexdigest()==v for k,v in hashes.items())
receipt['ended_at']=datetime.datetime.now(datetime.timezone.utc).isoformat(); save()
if not receipt['source_unchanged']: raise SystemExit('Campaign source changed during execution')
# Preserve both outcomes; scientific failures do not suppress the other platform.
raise SystemExit(0 if all(t['exit_code']==0 for t in receipt['targets']) else 1)
