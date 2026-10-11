#!/usr/bin/env python3
"""Execute the prospective run-varying AR shrinkage study profile on JVM and JS and analyze both independently in R."""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
from freeze_candidate import validate_records

ROOT = Path(__file__).resolve().parents[3]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--profile', choices=['pilot', 'development', 'confirmation'], required=True)
parser.add_argument('--output', type=Path, required=True)
parser.add_argument('--candidate', help='frozen candidate engine (confirmation only)')
parser.add_argument('--freeze', type=Path, help='committed candidate/source freeze receipt (required for confirmation)')
parser.add_argument('--platforms', default='JVM,JS')
parser.add_argument('--engines', help='comma-separated engine labels (default: all declared engines)')
args = parser.parse_args()
platforms = args.platforms.split(',')
if not platforms or len(set(platforms)) != len(platforms) or any(p not in ('JVM', 'JS') for p in platforms):
    raise SystemExit('platforms must be a nonempty, distinct subset of JVM,JS')
if args.profile == 'development' and platforms != ['JVM']:
    raise SystemExit('the prospective development protocol uses JVM only')
if args.profile == 'confirmation' and (not args.candidate or set(args.platforms.split(',')) != {'JVM', 'JS'}):
    raise SystemExit('confirmation requires the frozen --candidate and both JVM,JS')
out = args.output.resolve()
out.mkdir(parents=True, exist_ok=True)
if any((out / name).exists() for name in ['records-jvm.jsonl', 'records-js.jsonl', 'run.json']):
    raise SystemExit('refusing to overwrite an observed run; choose another output directory')
protocol_file = ROOT / 'tools/scenarios/fgls-run-varying/protocol.json'
protocol = json.loads(protocol_file.read_text())
allowed = set(protocol['engines'])
if args.profile == 'confirmation':
    if args.candidate not in protocol['candidate_engines']:
        raise SystemExit('confirmation candidate is not declared in the prospective protocol')
    expected = {'known-continuous', args.candidate}
else:
    expected = allowed
if args.engines and set(args.engines.split(',')) != expected:
    raise SystemExit('engine set must exactly match the declared phase')
args.engines = ','.join(e for e in protocol['engines'] if e in expected)
source = [ROOT / 'build.sbt'] + sorted((ROOT / 'modules/first-level-laws').rglob('*.scala'))
for module in ['ar', 'fit', 'model', 'design', 'hrf', 'dataset', 'response']:
    source.extend(p for p in (ROOT / 'modules' / module).rglob('*.scala') if '/src/main/' in str(p))
source.extend(p for directory in ['fgls-voxelwise', 'fgls-run-varying']
              for p in (ROOT / 'tools/scenarios' / directory).glob('*') if p.is_file())
hashes = {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(set(source))}
tracked = set(subprocess.check_output(['git', 'ls-files', '-z'], cwd=ROOT).decode().split('\0'))
if not set(hashes).issubset(tracked):
    raise SystemExit('commit every source file before consuming a study stream')
source_dirty = subprocess.run(['git', 'diff', '--quiet', 'HEAD', '--', *hashes], cwd=ROOT).returncode != 0
if source_dirty:
    raise SystemExit('commit source changes before consuming a study stream')
if args.profile == 'confirmation':
    if not args.freeze:
        raise SystemExit('confirmation requires the committed --freeze receipt')
    frozen = json.loads(args.freeze.read_text())
    if frozen.get('candidate') != args.candidate or frozen.get('source_sha256') != hashes:
        raise SystemExit('candidate or source bytes differ from the pre-confirmation freeze')
    if frozen.get('protocol_sha256') != hashlib.sha256(protocol_file.read_bytes()).hexdigest():
        raise SystemExit('protocol differs from the pre-confirmation freeze')
    relative = args.freeze.resolve().relative_to(ROOT)
    subprocess.run(['git', 'ls-files', '--error-unmatch', str(relative)], cwd=ROOT, check=True, stdout=subprocess.DEVNULL)
    subprocess.run(['git', 'diff', '--exit-code', 'HEAD', '--', str(relative)], cwd=ROOT, check=True)
    subprocess.run(['git', 'merge-base', '--is-ancestor', frozen['source_commit'], 'HEAD'], cwd=ROOT, check=True)
env = dict(os.environ)
for name in list(env):
    if name.startswith(('SCALAFIM_GLS_', 'SCALAFIM_LAW_', 'SCALAFIM_FGLS_')):
        env.pop(name)
env.update(SCALAFIM_FGLS_STUDY_PROFILE=args.profile, SCALAFIM_LAW_PROFILE='pull-request',
           SCALAFIM_GLS_STUDY_LOG=str(out / 'records'), SCALAFIM_FGLS_STUDY_SEED=str(protocol['seed']), LC_ALL='C')
env['SCALAFIM_FGLS_PROTOCOL'] = 'run-shrinkage-v1'
if args.engines:
    env['SCALAFIM_FGLS_ENGINES'] = args.engines
warm = [sys.executable, str(ROOT / 'tools/build/sbt-warm')]
subprocess.run(warm + ['--shutdown'], cwd=ROOT, env=env, check=True)
receipt = dict(schema_version='scalafim-fgls-run-varying-run/v1', profile=args.profile, candidate=args.candidate,
               freeze_sha256=hashlib.sha256(args.freeze.read_bytes()).hexdigest() if args.freeze else None,
               engines=args.engines, execution_route="experimental run shrinkage",
               protocol_sha256=hashlib.sha256(protocol_file.read_bytes()).hexdigest(),
               source_base=subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
               source_dirty=source_dirty,
               source_sha256=hashes, seed=protocol['seed'], targets=[],
               started_at=datetime.datetime.now(datetime.timezone.utc).isoformat())
def save():
    (out / 'run.json').write_text(json.dumps(receipt, indent=2) + '\n')
save()
if args.profile == 'confirmation':
    spent = args.freeze.parent / 'confirmation-spend.json'
    with spent.open('x') as stream:
        json.dump({'freeze_sha256': receipt['freeze_sha256'], 'output': str(out),
                   'started_at': receipt['started_at'], 'rule': 'one execution; failures remain spent'}, stream, indent=2)
        stream.write('\n')
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
receipt['validated_record_counts'] = {}
for platform in platforms:
    path = out / f'records-{platform.lower()}.jsonl'
    receipt['validated_record_counts'][platform] = validate_records(path, protocol, args.profile, expected)
save()
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
exit_code = 0 if all(t['exit_code'] == 0 for t in receipt['targets'] + receipt.get('analyses', [])) else 1
if args.profile == 'confirmation':
    for platform in ['jvm', 'js']:
        summary = out / ('analysis-' + platform) / 'summary.json'
        if not summary.exists():
            exit_code = 1
            continue
        result = json.loads(summary.read_text())
        parity = result.get('cross_platform', {})
        if not result.get('scientific_pass') or not parity.get('identical_decisions') or not (parity.get('matched', 0) == parity.get('this') == parity.get('other')):
            exit_code = 1
raise SystemExit(exit_code)
