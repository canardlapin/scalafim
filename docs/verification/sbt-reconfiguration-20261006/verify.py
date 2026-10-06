"""Check retained source hashes, resource measurements and final gate outcomes."""
from pathlib import Path
import hashlib
import json
import re
import tarfile

folder = Path(__file__).resolve().parent
root = folder.parents[2]
receipt = json.loads((folder / 'receipt.json').read_text())
archive = folder / receipt['archive']
assert hashlib.sha256(archive.read_bytes()).hexdigest() == receipt['archive_sha256']
for path, expected in receipt['source_sha256'].items():
    assert hashlib.sha256((root / path).read_bytes()).hexdigest() == expected, path
with tarfile.open(archive) as bundle:
    def text(path):
        return bundle.extractfile(path).read().decode()
    measured = json.loads(text('measurements/runs.json'))
    failed = next(r for r in measured if r['profile'] == '3g' and r['label'] == 'set-one')
    assert failed['exit_code'] != 0 and failed['gc_warnings'] and not failed['timed_out']
    larger = [r for r in measured if r['profile'] == '4g']
    assert len(larger) == 5 and all(r['exit_code'] == 0 and not r['gc_warnings'] for r in larger)
    for process in json.loads(text('measurements/processes.json')):
        size = 3221225472 if process['heap_profile'] == '3g' else 4294967296
        assert process['flags_exit_code'] == 0 and f'MaxHeapSize={size}' in process['flags']
    candidate = json.loads(text('measurements/candidate/runs.json'))
    assert all(r['exit_code'] == r['expected_exit_code'] and not r['warnings'] for r in candidate)
    final = json.loads(text('measurements/final/runs.json'))
    assert len(final) == 7 and all(r['exit_code'] == r['expected_exit_code'] and not r['warnings'] for r in final)
    for run in final:
        assert run['source_sha256'] == receipt['source_sha256']['tools/build/sbt-warm']
        content = text('measurements/final/' + run['label'] + '.log')
        counts = [dict(total=int(m[0]), failed=int(m[1]), errors=int(m[2]), passed=int(m[3]))
                  for m in re.findall(r'(?:Passed|Failed): Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', content)]
        assert counts == run['counts']
        assert all(c['failed'] == 0 and c['errors'] == 0 for c in counts)
    assert [r['counts'][0]['passed'] for r in final if r['counts']] == [386, 386]
    assert 'Ran 57 tests' in text('measurements/python-tests-final.log')
    assert text('measurements/python-tests-final.log').rstrip().endswith('OK')
    validation = json.loads(text('measurements/final/validation.json'))
    assert validation['validation'] == 'Pass' and validation['server_exited'] and validation['server_socket_closed']
    assert validation['malformed_active_shutdown_recovered_known_base_socket']
print('Verified resource profiles, expected refusals, final JVM/JS gates and source/archive hashes.')
