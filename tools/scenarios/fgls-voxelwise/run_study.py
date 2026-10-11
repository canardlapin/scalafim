#!/usr/bin/env python3
"""Execute the declared FGLS voxelwise study profile on JVM and JS and analyze both independently in R."""
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
parser.add_argument('--profile', choices=['pilot', 'development', 'confirmation'], required=True)
parser.add_argument('--output', type=Path, required=True)
parser.add_argument('--candidate', help='frozen candidate engine (confirmation only)')
parser.add_argument('--platforms', default='JVM,JS')
parser.add_argument('--engines', help='comma-separated engine labels (default: all declared engines)')
args = parser.parse_args()
if args.profile == 'confirmation' and not args.candidate:
    raise SystemExit('confirmation requires the frozen --candidate')
out = args.output.resolve()
out.mkdir(parents=True, exist_ok=True)
if any((out / name).exists() for name in ['records-jvm.jsonl', 'records-js.jsonl', 'run.json']):
    raise SystemExit('refusing to overwrite an observed run; choose another output directory')
protocol_file = ROOT / 'tools/scenarios/fgls-voxelwise/protocol.json'
protocol = json.loads(protocol_file.read_text())
source = [ROOT / 'build.sbt'] + sorted((ROOT / 'modules/first-level-laws').rglob('*.scala'))
for module in ['ar', 'fit', 'model', 'design', 'hrf', 'dataset', 'response']:
    source.extend(p for p in (ROOT / 'modules' / module).rglob('*.scala') if '/src/main/' in str(p))
source.extend(p for p in (ROOT / 'tools/scenarios/fgls-voxelwise').glob('*') if p.is_file())
hashes = {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(set(source))}
env = dict(os.environ)
for name in list(env):
    if name.startswith(('SCALAFIM_GLS_', 'SCALAFIM_LAW_', 'SCALAFIM_FGLS_')):
        env.pop(name)
env.update(SCALAFIM_FGLS_STUDY_PROFILE=args.profile, SCALAFIM_LAW_PROFILE='pull-request',
           SCALAFIM_GLS_STUDY_LOG=str(out / 'records'), SCALAFIM_FGLS_STUDY_SEED=str(protocol['seed']), LC_ALL='C')
if args.engines:
    env['SCALAFIM_FGLS_ENGINES'] = args.engines
warm = [sys.executable, str(ROOT / 'tools/build/sbt-warm')]
subprocess.run(warm + ['--shutdown'], cwd=ROOT, env=env, check=True)
receipt = dict(schema_version='scalafim-fgls-voxelwise-run/v1', profile=args.profile, candidate=args.candidate,
               engines=args.engines,
               protocol_sha256=hashlib.sha256(protocol_file.read_bytes()).hexdigest(),
               source_base=subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
               source_dirty=subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--', 'modules', 'tools/scenarios/fgls-voxelwise', 'build.sbt'], cwd=ROOT).returncode != 0,
               source_sha256=hashes, seed=protocol['seed'], targets=[],
               started_at=datetime.datetime.now(datetime.timezone.utc).isoformat())
def save():
    (out / 'run.json').write_text(json.dumps(receipt, indent=2) + '\n')
save()
for platform in args.platforms.split(','):
    command = f'firstLevelLaws{platform}/testOnly scalafim.fmri.laws.FglsVoxelwiseStudySuite'
    started = datetime.datetime.now(datetime.timezone.utc).isoformat()
    log = out / (platform.lower() + '.log')
    with log.open('w') as stream:
        result = subprocess.run(warm + [command], cwd=ROOT, env=env, stdout=stream, stderr=subprocess.STDOUT)
    receipt['targets'].append(dict(target=platform, command=command, exit_code=result.returncode,
        started_at=started, ended_at=datetime.datetime.now(datetime.timezone.utc).isoformat(),
        log_sha256=hashlib.sha256(log.read_bytes()).hexdigest()))
    save()
    print(platform, 'exit', result.returncode, flush=True)
subprocess.run(warm + ['--shutdown'], cwd=ROOT, env=env)
receipt['source_unchanged'] = all(hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == sha
                                for name, sha in hashes.items())
receipt['ended_at'] = datetime.datetime.now(datetime.timezone.utc).isoformat()
save()
if not receipt['source_unchanged']:
    raise SystemExit('source changed during execution; results cannot qualify')
for platform in ['jvm', 'js']:
    records = out / f'records-{platform}.jsonl'
    if not records.exists():
        continue
    other = out / ('records-js.jsonl' if platform == 'jvm' else 'records-jvm.jsonl')
    command = ['Rscript', str(ROOT / 'tools/scenarios/fgls-voxelwise/analyze.R'), str(records),
               str(out / ('analysis-' + platform)), str(protocol_file), str(other) if other.exists() else '-']
    if args.candidate:
        command.append(args.candidate)
    with (out / f'analysis-{platform}.log').open('w') as stream:
        result = subprocess.run(command, cwd=ROOT, env=env, stdout=stream, stderr=subprocess.STDOUT)
    receipt.setdefault('analyses', []).append(dict(platform=platform, exit_code=result.returncode))
    save()
exit_code = 0 if all(t['exit_code'] == 0 for t in receipt['targets']) else 1
if args.profile == 'confirmation':
    for platform in ['jvm', 'js']:
        summary = out / ('analysis-' + platform) / 'summary.json'
        if not summary.exists() or not json.loads(summary.read_text()).get('scientific_pass'):
            exit_code = 1
raise SystemExit(exit_code)
