"""Verify the native color/typed-plan evidence without starting a GUI or JVM."""
from pathlib import Path
import hashlib
import json
import tarfile

folder = Path(__file__).resolve().parent
root = folder.parents[2]
receipt = json.loads((folder/'receipt.json').read_text())
archive = folder/receipt['archive']
digest = lambda content: hashlib.sha256(content).hexdigest()
assert digest(archive.read_bytes()) == receipt['archive_sha256']
for source, expected in receipt['source_sha256'].items():
    assert digest((root/source).read_bytes()) == expected, source
with tarfile.open(archive) as bundle:
    def read(path):
        return bundle.extractfile(path).read()
    for source, expected in receipt['source_sha256'].items():
        assert digest(read('source/'+source)) == expected
    runs = json.loads(read('accepted/runs.json'))
    assert all(run['exit_code'] == 0 for run in runs)
    for name, expected in receipt['frames_sha256'].items():
        assert digest(read('accepted/frames/'+name)) == expected
    report = json.loads(read('accepted/frames/color-report.json'))
    assert report['passed'] and len(report['cases']) == 48 and len(report['permutations']) == 32
    assert all(case['passed'] for case in report['cases'])
    assert all(case['outOfBudgetPixels'] == 0 for case in report['permutations'])
    assert report['tolerance'] == 2.0
    assert json.loads(read('accepted/oracle-self-check.json'))['passed']
    assert json.loads(read('accepted/context.json'))['status'] == 'PassContextOnly'
    plan = json.loads((folder/'plan-producer/receipt.json').read_text())
    for record in plan['records']:
        assert record['original_ids_retained']
        assert digest(read('accepted/frames/'+record['name'])) == record['source_sha256'] == record['plan_sha256']
    assert plan['compile_and_test']['JVM_passed'] == plan['compile_and_test']['JS_passed'] == 68
assert not receipt['full_provider_admitted'] and not receipt['consumer_admitted']
print('Verified: 48 color fixtures, 32 permutations, original plan identities, and explicit kernel-only scope.')
