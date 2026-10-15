"""Verify retained gate counts, pinned sources and fork observations without sbt."""
from pathlib import Path
import hashlib
import io
import json
import re
import tarfile

folder = Path(__file__).resolve().parent
root = folder.parents[2]
receipt = json.loads((folder / 'receipt.json').read_text())
archive = folder / receipt['archive']
assert hashlib.sha256(archive.read_bytes()).hexdigest() == receipt['archive_sha256']
for name, digest in receipt['source_sha256'].items():
    assert hashlib.sha256((root / name).read_bytes()).hexdigest() == digest, name
with tarfile.open(archive) as bundle:
    def text(name):
        return bundle.extractfile(name).read().decode()
    runs = json.loads(text('runs.json'))
    assert len(runs) == 6 and all(r['exit_code'] == 0 for r in runs)
    for run in runs:
        content = text(run['label'] + '.log')
        counts = [dict(total=int(m[0]), failed=int(m[1]), errors=int(m[2]), passed=int(m[3]))
                  for m in re.findall(r'(?:Passed|Failed): Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', content)]
        assert counts == run['counts']
        assert all(c['failed'] == 0 and c['errors'] == 0 for c in counts)
        assert not any(line.startswith('[warn]') for line in content.splitlines())
        if run['label'] in {'atlas-jvm', 'atlas-js', 'native-repeat', 'ordinary-after'}:
            assert not run['skipped']
    assert next(r for r in runs if r['label'] == 'atlas-jvm')['counts'][0]['passed'] == 116
    assert next(r for r in runs if r['label'] == 'atlas-js')['counts'][0]['passed'] == 79
    assert next(r for r in runs if r['label'] == 'native-repeat')['counts'][0]['passed'] == 8
    native_log = text('atlas-jvm.log')
    assert 'a qualified numerical inverse gives the forward map and an executable reverse route' in native_log
    children = json.loads(text('child-processes.json'))
    parents = [p for p in json.loads(text('processes.json')) if '--detach-stdio' in p['command']]
    assert len(parents) == 1 and len(children) == 2
    assert len({c['pid'] for c in children}) == 2
    assert set(parents[0]['gates']) == {r['label'] for r in runs}
    for child in children:
        assert child['ppid'] == parents[0]['pid'] and child['flags_exit_code'] == 0
        assert 'MaxHeapSize=3221225472' in child['flags']
        assert 'ActiveProcessorCount=2' in child['flags']
    validation = json.loads(text('validation.json'))
    assert validation['children_exited'] and validation['same_server_used_through_following_JVM_JS_gates']
    assert sum(c['passed'] for run in runs for c in run['counts']) == receipt['total_passed_tests'] == 863
print('Verified source/archive hashes, passing JVM/JS gates and independent child observations.')
