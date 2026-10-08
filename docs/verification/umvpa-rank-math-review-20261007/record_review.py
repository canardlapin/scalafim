#!/usr/bin/env python3
"""Validate and retain the mathematical review and its prospective plan."""
import csv
import gzip
import hashlib
import io
import json
import math
import re
import subprocess
import tarfile
import xml.etree.ElementTree as ET
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
plan = root/'docs/plans/unified-mvpa-rank-method-plan-v2.md'
def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def table(name):
    return list(csv.DictReader((packet/name).read_text().splitlines(),delimiter='\t'))

preserved = {}
old_sources = {}
for name in ('umvpa-rank-pilot-20261007','umvpa-rank-diagnosis-20261007'):
    path = root/'docs/verification'/name/'execution.json'
    receipt = json.loads(path.read_text())
    for kind in ('source_locks','artifact_locks'):
        for filename, expected in receipt[kind].items():
            assert sha(root/filename) == expected, filename
    old_sources.update(receipt['source_locks'])
    preserved[name] = dict(execution_sha256=sha(path),sources=len(receipt['source_locks']),
                           artifacts=len(receipt['artifact_locks']))

process = json.loads((packet/'test-process.json').read_text())
assert process['exit_code'] == 0 and process['wall_seconds'] < 600
assert not (root/process['temporary_source']).exists()
records = [json.loads(line) for line in (packet/'production-checks.jsonl').read_text().splitlines()]
assert len(records) == 2
for row in records:
    assert row['group_actions'] == 720 and row['random_draws'] == 0
    assert row['base_counts'] == [460,642] and row['sheared_counts'] == [460,672]
    assert abs(row['maximum_tail_statistic_change']-2.8042430418034) < 1e-10
affine = table('affine-completion.tsv')
assert len(affine) == 2 and float(affine[1]['max_null_h2_difference']) < 1e-10
assert float(affine[0]['max_null_h2_difference']) > 2.8
for row in table('interlacing-checks.tsv'):
    assert float(row['largest_rootwise_difference']) < 1e-12
    assert float(row['tail_wilks']) <= float(row['reference_wilks'])+1e-12
observed = root/'docs/verification/umvpa-rank-diagnosis-20261007/independent-observed.tsv.gz'
maximum = 0.0;count = 0
with gzip.open(observed,'rt') as source:
    for row in csv.DictReader(source,delimiter='\t'):
        count += 1
        for k in range(1,5):
            direct = -sum(math.log1p(-float(row['root'+str(j)])**2) for j in range(k,5))
            maximum = max(maximum,abs(direct-float(row['wilks'+str(k)])))
identity = json.loads((packet/'observed-wilks-identity.json').read_text())
assert count == identity['retained_inputs'] == 12200
assert maximum == identity['maximum_tail_sum_discrepancy'] and maximum < 1e-10

tests = {};xml = []
for platform in ('jvm','js'):
    files = sorted((root/'modules/mvpa'/platform/'target/test-reports').glob('*.xml'))
    totals = {key:sum(int(ET.parse(f).getroot().get(key,0)) for f in files)
              for key in ('tests','failures','errors','skipped')}
    assert totals == dict(tests=467,failures=0,errors=0,skipped=2),totals
    review = [f for f in files if f.name.endswith('.RankMethodReviewSuite.xml')]
    assert len(review) == 1 and ET.parse(review[0]).getroot().get('skipped') == '0'
    tests[platform] = dict(**totals,passed=465,suites=len(files))
    xml.extend(files)
buffer = io.BytesIO()
with tarfile.open(fileobj=buffer,mode='w') as archive:
    for path in xml:
        data = path.read_bytes();info = tarfile.TarInfo(str(path.relative_to(root)))
        info.size = len(data);archive.addfile(info,io.BytesIO(data))
(packet/'test-reports.tar.gz').write_bytes(gzip.compress(buffer.getvalue(),mtime=0))
for path in list(packet.rglob('*.log')):
    data = path.read_bytes();compressed = gzip.compress(data,mtime=0)
    assert gzip.decompress(compressed) == data
    path.with_suffix('.log.gz').write_bytes(compressed);path.unlink()
log = gzip.decompress((packet/'mvpa-review-tests.log.gz').read_bytes()).decode()
assert '[warn]' not in log and '[error]' not in log
assert log.count('Passed: Total 467, Failed 0, Errors 0, Passed 465, Skipped 2') == 2

for document in (packet/'README.md',plan):
    for target in re.findall(r'\]\(([^)]+)\)',document.read_text()):
        if not target.startswith('https:'):
            assert (document.parent/target.split('#')[0]).exists(),target
for path in packet.glob('*.py'):
    compile(path.read_text(),str(path),'exec')

provider = Path('/Users/bbuchsbaum/.cache/scalafim-sbt/bases/9c9d5aab8396a0b5/staging/04c54b2d3f3921f04b0c/multivar')
revision = subprocess.check_output(['git','rev-parse','HEAD'],cwd=provider,text=True).strip()
assert revision == 'ab811e257dd67f77e8c3b70cb1ea600f274429a3'
assert not subprocess.check_output(['git','status','--porcelain=v1','-uno'],cwd=provider,text=True).strip()
external = {str(path.relative_to(provider)):sha(path) for path in
            (provider/'modules/inference/shared/src/main/scala/multivar/inference').glob('*Canonical*.scala')}
sources = set(old_sources) | {str(p.relative_to(root)) for p in packet.iterdir()
                             if p.suffix in ('.R','.py','.scala')}
artifacts = [p for p in packet.rglob('*') if p.is_file() and p.name != 'execution.json'
             and p.suffix not in ('.R','.py','.scala')] + [plan]
result = dict(status='mathematical-review-complete-prospective-plan-not-frozen',
    baseline_commit='65cee783',preserved_prior_receipts=preserved,
    production_code_changed=False,confirmation_consumed=False,new_random_draws=0,
    fixed_counterexample_dimensions=[6,2,3],exhaustive_actions=720,
    platforms=['JVM','JS'],tests=tests,test_process=process,
    confirmed_findings=['coordinate dependence of Euclidean coefficient completion',
                        'Gaussian projection needs a conditional joint row model',
                        'independent imperfect deflation does not imply the rank null',
                        'rootwise interlacing bounds the unscaled Wilks tail'],
    unresolved=['uniform partial-null validity of fitted-tail permutations',
                'power and qualification of any replacement procedure'],
    retained_failure=dict(attempt='settings-reload-heap',exit_code=243,
        reason='reapplying sbt settings stalled at the 3 GiB heap limit',
        recovery='stopped verified worktree PID 88021 and used temporary test-source copy'),
    external_provider=dict(revision=revision,source_locks=external),
    source_locks={name:sha(root/name) for name in sorted(sources)},
    artifact_locks={str(p.relative_to(root)):sha(p) for p in sorted(artifacts)})
(packet/'execution.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(dict(status=result['status'],tests=tests,preserved=preserved,
                     source_files=len(sources),artifacts=len(artifacts),execution_sha256=sha(packet/'execution.json')),indent=2))
