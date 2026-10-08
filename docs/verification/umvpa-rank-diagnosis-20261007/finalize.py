#!/usr/bin/env python3
"""Validate and bind diagnostic evidence without changing prior receipts."""
import csv
import gzip
import hashlib
import io
import json
import math
import tarfile
import xml.etree.ElementTree as ET
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def table(path):
    content = gzip.decompress(path.read_bytes()).decode() if path.suffix == '.gz' else path.read_text()
    return list(csv.DictReader(content.splitlines(), delimiter='\t'))

prior_path = root / 'docs/verification/umvpa-rank-pilot-20261007/execution.json'
plan = json.loads((packet / 'diagnostic-plan.json').read_text())
assert digest(prior_path) == plan['prior_evidence_sha256']
prior = json.loads(prior_path.read_text())
for kind in ('source_locks', 'artifact_locks'):
    for name, expected in prior[kind].items():
        assert digest(root / name) == expected, name
for name, expected in plan['source_locks'].items():
    assert digest(root / name) == expected, name

for path in list(packet.rglob('*.log')) + [packet/'case-index.tsv', packet/'scala-replay.jsonl']:
    if path.exists():
        raw = path.read_bytes()
        archive = path.with_suffix(path.suffix+'.gz')
        archive.write_bytes(gzip.compress(raw, mtime=0))
        assert gzip.decompress(archive.read_bytes()) == raw
        path.unlink()

process = json.loads((packet/'diagnose-process.json').read_text())
assert process['exit_code'] == 0 and process['wall_seconds'] < process['wall_limit_seconds'] == 600
index = table(packet/'case-index.tsv.gz')
observed = table(packet/'independent-observed.tsv.gz')
summary = table(packet/'diagnostic-summary.tsv')
assert len(index) == len(observed) == 12200 and len(summary) == 61
assert sum(int(row['datasets']) for row in summary) == 12200
assert {(r['cell'], r['ordinal']) for r in index} == {(r['cell'], r['ordinal']) for r in observed}
for row in observed:
    for name in [f'{field}{k}' for field in ('root','wilks') for k in range(1,5)]:
        assert math.isfinite(float(row[name]))
    for name in ('known_axes_h4_p', 'known_third_pair_p', 'fitted_h4_beta_reference'):
        assert 0 <= float(row[name]) <= 1
for row in summary:
    if row['cell'].startswith('rank.R3.') and row['rate_class'] == 'alternative':
        assert row['raw_h3'] == row['closed_h3']

repeat_hashes = {}
for name in ('diagnostic-summary.tsv', 'independent-observed.tsv.gz'):
    first = (packet/'attempts/time-wrapper'/name).read_bytes()
    final = (packet/name).read_bytes()
    if name.endswith('.gz'):
        first, final = gzip.decompress(first), gzip.decompress(final)
    assert first == final, name
    repeat_hashes[name] = hashlib.sha256(final).hexdigest()

checks = table(packet/'replay-checks.tsv')
assert len(checks) == 32
maxima = {key: max(float(r[key]) for r in checks) for key in list(checks[0])[2:]}
assert max(maxima.values()) < 1e-10
records = [json.loads(line) for line in gzip.decompress((packet/'scala-replay.jsonl.gz').read_bytes()).splitlines()]
assert len(records) == 32 and all(r['new_random_draws'] == 0 for r in records)
gate_log = gzip.decompress((packet/'mvpa-tests.log.gz').read_bytes()).decode()
assert gate_log.count('Passed: Total 466, Failed 0, Errors 0, Passed 465, Skipped 1') == 2
assert '[warn]' not in gate_log
tests = {}
xml_files = []
for platform in ('jvm', 'js'):
    files = sorted((root/'modules/mvpa'/platform/'target/test-reports').glob('*.xml'))
    totals = {key: sum(int(ET.parse(f).getroot().get(key,0)) for f in files)
              for key in ('tests', 'failures', 'errors', 'skipped')}
    assert totals == dict(tests=466, failures=0, errors=0, skipped=1), totals
    replay = [f for f in files if f.name.endswith('.RankDiagnosticReplaySuite.xml')]
    assert len(replay) == 1 and ET.parse(replay[0]).getroot().get('skipped') == '0'
    assert f'[sbt-warm] mvpa{platform.upper()}/test: exit 0' in gate_log
    tests[platform] = {**totals, 'passed': 465, 'suites': len(files)}
    xml_files.extend(files)
buffer = io.BytesIO()
with tarfile.open(fileobj=buffer, mode='w') as archive:
    for path in xml_files:
        data = path.read_bytes()
        info = tarfile.TarInfo(str(path.relative_to(root)))
        info.size = len(data)
        archive.addfile(info, io.BytesIO(data))
(packet/'test-reports.tar.gz').write_bytes(gzip.compress(buffer.getvalue(),mtime=0))

sources = set(prior['source_locks']) | set(plan['source_locks'])
sources.update(str(p.relative_to(root)) for p in packet.iterdir() if p.suffix in ('.R','.py'))
artifacts = sorted(p for p in packet.rglob('*') if p.is_file()
                   and p.name != 'execution.json' and p.suffix not in ('.R','.py'))
receipt = dict(status='diagnosis-complete-qualification-unresolved',
    diagnostic_plan_commit='0065c6d6', prior_execution_sha256=digest(prior_path),
    preserved_prior_source_locks=len(prior['source_locks']),
    preserved_prior_artifact_locks=len(prior['artifact_locks']),
    retained_datasets_reanalysed=12200, new_datasets=0, new_random_draws=0,
    confirmation_consumed=False, threshold_or_population_changes=False,
    tests=tests, replay_platform_cases=32, actions_per_case=3,
    replay_maximum_discrepancies=maxima, repeat_uncompressed_hashes=repeat_hashes,
    successful_R_process=process,
    retained_failures=['sandbox denial in optional /usr/bin/time -l wrapper',
                       'R replay-checker vapply result-shape declaration'],
    source_locks={name: digest(root/name) for name in sorted(sources)},
    artifact_locks={str(p.relative_to(root)): digest(p) for p in artifacts})
(packet/'execution.json').write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps({key: receipt[key] for key in ('status','preserved_prior_source_locks',
     'preserved_prior_artifact_locks','tests','replay_maximum_discrepancies')},indent=2))
print('execution.json SHA256', digest(packet/'execution.json'))
