#!/usr/bin/env python3
"""Restore a verified extension of this exact frozen journal; never edit op bytes."""
from pathlib import Path
import fcntl
import hashlib
import json
import os
import subprocess

repository = Path(__file__).resolve().parents[2]
store = repository / '.mote'
preview = Path('/private/tmp/scalafim-mote-native-recovery-20261016/.mote')
work = Path(__file__).resolve().parent
snapshot = json.loads((work / 'live-before.json').read_text())
metadata = json.loads((work / 'native-full-preview.log.meta.json').read_text())
assert metadata['exit_code'] == 0, 'native preview did not finish successfully'
summary = json.loads([line for line in (work / 'native-full-preview.log').read_text().splitlines() if line.strip()][-1])
assert summary['operations'] == 24943 and summary['publishedExtras'] == 943
assert summary['fsckClean'] and summary['nativeReplayMatchesSimulation']
assert summary['allOperationBytesUnchanged'] and summary['originalAuthorityRecordsUnchanged']

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

expected = {entry['path']: entry['sha256'] for entry in snapshot['files']}
old_records = sorted(path for path in expected if path.startswith('.mote/authority/'))
new_records = sorted(preview.joinpath('authority').glob('*.json'))
assert len(old_records) == 75 and len(new_records) == 1018
assert [p.name for p in new_records] == [f'{i:020}.json' for i in range(1018)]
assert (store / 'FORMAT.json').read_bytes() == (preview / 'FORMAT.json').read_bytes()

# Read-only validation with the installed CLI is independent of the publisher helper.
for arguments in [['authority', 'status'], ['fsck']]:
    subprocess.run(['mote', '--store', str(preview), *arguments], check=True,
                   stdout=open(work / ('preview-' + '-'.join(arguments) + '.txt'), 'w'))

with (store / 'local' / 'publication.lock').open('a+b') as lock:
    fcntl.flock(lock, fcntl.LOCK_EX)
    # This is the same OS lock Mote writers use. Abort if any input changed.
    for relative, digest in expected.items():
        assert sha(repository / relative) == digest, f'live store changed: {relative}'
    live_ops = {p.name for p in store.joinpath('ops').glob('*.json')}
    assert live_ops == {p.name for p in preview.joinpath('ops').glob('*.json')}
    for name in live_ops:
        assert sha(store / 'ops' / name) == sha(preview / 'ops' / name), name
    present = sorted(store.joinpath('authority').iterdir())
    assert all(p.name in {q.name for q in new_records} for p in present), 'unexpected journal'
    # Matching records from an interrupted restore are retained, never overwritten.
    added = []
    directory_fd = os.open(store / 'authority', os.O_RDONLY)
    try:
        for source in new_records:
            destination = store / 'authority' / source.name
            if destination.exists():
                assert destination.read_bytes() == source.read_bytes(), source.name
                continue
            assert int(source.stem) >= 75, 'missing original journal record'
            temporary = store / 'authority' / ('.restore-' + source.name + '.tmp')
            with temporary.open('xb') as output:
                output.write(source.read_bytes())
                output.flush()
                os.fsync(output.fileno())
            os.link(temporary, destination)  # fails rather than replacing any record
            temporary.unlink()
            os.fsync(directory_fd)
            added.append(source.name)
    finally:
        os.close(directory_fd)
    for relative, digest in expected.items():
        assert sha(repository / relative) == digest, relative
    # Read-only commands do not take the publication lock.
    result = subprocess.run(['mote', '--store', str(store), 'authority', 'status'],
                            check=True, text=True, capture_output=True)
    (work / 'live-authority-status.json').write_text(result.stdout)
    receipt = {'schema': 'scalafim.mote-native-recovery.v1',
               'policy': 'Restore native-publisher extension after the frozen prefix; original historical order of extras remains unknown',
               'originalRecordsUnchanged': 75, 'operationFilesUnchanged': 24943,
               'newAdmissionRecords': len(added), 'authorityRecords': 1018,
               'preview': summary, 'authorityStatus': json.loads(result.stdout),
               'newRecordSha256': [{'name': p.name, 'sha256': sha(p)} for p in new_records[75:]]}
    (work / 'restore-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(json.dumps({k: v for k, v in receipt.items() if k not in ['newRecordSha256', 'preview']}))
