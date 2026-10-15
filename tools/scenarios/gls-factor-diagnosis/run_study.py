#!/usr/bin/env python3
"""Execute the frozen paired diagnostic or mitigation confirmation on both platforms."""
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
parser.add_argument('--profile', choices=['pilot', 'diagnostic', 'confirmation'], required=True)
parser.add_argument('--output', type=Path, required=True)
args = parser.parse_args()
out = args.output.resolve()
out.mkdir(parents=True, exist_ok=True)
if any((out / name).exists() for name in ['records-jvm.jsonl', 'records-js.jsonl', 'run.json']):
    raise SystemExit('refusing to overwrite an observed run; choose another output directory')
protocol_file = ROOT / 'tools/scenarios/gls-factor-diagnosis/protocol.json'
protocol = json.loads(protocol_file.read_text())
selected = dict(protocol)
selected['cells'] = protocol['confirmation_cells'] if args.profile == 'confirmation' else protocol['diagnostic_cells']
(out / 'analysis-protocol.json').write_text(json.dumps(selected, indent=2) + '\n')
source = [ROOT / 'build.sbt'] + sorted((ROOT / 'modules/first-level-laws').rglob('*.scala'))
for module in ['ar', 'fit', 'model', 'design', 'hrf', 'dataset', 'response']:
    source.extend(p for p in (ROOT / 'modules' / module).rglob('*.scala') if '/src/main/' in str(p))
source.extend(p for p in (ROOT / 'tools/scenarios/gls-factor-diagnosis').glob('*') if p.is_file())
source.append(ROOT / 'tools/scenarios/corrected-gls/analyze.R')
hashes = {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(set(source))}
env = dict(os.environ)
for name in list(env):
    if name.startswith(('SCALAFIM_GLS_', 'SCALAFIM_LAW_')):
        env.pop(name)
env.update(SCALAFIM_GLS_FACTOR_PROFILE=args.profile, SCALAFIM_LAW_PROFILE='pull-request',
           SCALAFIM_GLS_STUDY_LOG=str(out / 'records'), SCALAFIM_GLS_STUDY_SEED=str(protocol['seed']),
           SCALAFIM_GLS_ANALYSIS_PROTOCOL=str(out / 'analysis-protocol.json'), LC_ALL='C')
warm = [sys.executable, str(ROOT / 'tools/build/sbt-warm')]
subprocess.run(warm + ['--shutdown'], cwd=ROOT, env=env, check=True)
receipt = dict(schema_version='scalafim-gls-factor-run/v1', profile=args.profile,
               protocol_sha256=hashlib.sha256(protocol_file.read_bytes()).hexdigest(),
               source_base=subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
               source_sha256=hashes, seed=protocol['seed'], targets=[],
               started_at=datetime.datetime.now(datetime.timezone.utc).isoformat())
def save():
    (out / 'run.json').write_text(json.dumps(receipt, indent=2) + '\n')
save()
for platform in ['JVM', 'JS']:
    command = (f'firstLevelLaws{platform}/testOnly scalafim.fmri.laws.CorrectedGlsSimulatorSuite '
               'scalafim.fmri.laws.GlsFactorSimulatorSuite scalafim.fmri.laws.GlsFactorStudySuite')
    started = datetime.datetime.now(datetime.timezone.utc).isoformat()
    log = out / (platform.lower() + '.log')
    with log.open('w') as stream:
        result = subprocess.run(warm + [command], cwd=ROOT, env=env, stdout=stream, stderr=subprocess.STDOUT)
    receipt['targets'].append(dict(target=platform, command=command, exit_code=result.returncode,
        started_at=started, ended_at=datetime.datetime.now(datetime.timezone.utc).isoformat(),
        log_sha256=hashlib.sha256(log.read_bytes()).hexdigest()))
    save()
    print(platform, 'exit', result.returncode, flush=True)
receipt['source_unchanged'] = all(hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == sha
                                for name, sha in hashes.items())
receipt['ended_at'] = datetime.datetime.now(datetime.timezone.utc).isoformat()
save()
if not receipt['source_unchanged']:
    raise SystemExit('source changed during execution; results cannot qualify')
# Retain both platforms and independent analysis, including negative gates.
for platform in ['jvm', 'js']:
    records = out / f'records-{platform}.jsonl'
    if not records.exists():
        continue
    command = ['Rscript', str(ROOT / 'tools/scenarios/corrected-gls/analyze.R'),
               str(records), str(out / ('analysis-' + platform))]
    if platform == 'jvm' and (out / 'records-js.jsonl').exists():
        command.append(str(out / 'records-js.jsonl'))
    with (out / f'analysis-{platform}.log').open('w') as stream:
        result = subprocess.run(command, cwd=ROOT, env=env, stdout=stream, stderr=subprocess.STDOUT)
    receipt.setdefault('analyses', []).append(dict(platform=platform, exit_code=result.returncode))
    save()
exit_code = 0 if all(t['exit_code'] == 0 for t in receipt['targets']) else 1
if len(receipt.get('analyses', [])) != 2 or any(a['exit_code'] for a in receipt['analyses']):
    exit_code = 1
if args.profile == 'confirmation':
    for platform in ['jvm', 'js']:
        summary = out / ('analysis-' + platform) / 'summary.json'
        if not summary.exists() or not json.loads(summary.read_text())['scientific_pass']:
            exit_code = 1
raise SystemExit(exit_code)
