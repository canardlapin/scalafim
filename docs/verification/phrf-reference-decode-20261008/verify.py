#!/usr/bin/env python3
"""Verify this integration packet without optional Python packages."""
import gzip
import hashlib
import json
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
manifest = json.loads((packet / 'manifest.json').read_text())
for relative, expected in manifest['sha256'].items():
    assert hashlib.sha256((root / relative).read_bytes()).hexdigest() == expected, relative
protocol_bytes = (packet / 'protocol.json').read_bytes()
protocol_hash = hashlib.sha256(protocol_bytes).hexdigest()
assert (packet / 'protocol.sha256').read_text().strip() == protocol_hash
protocol = json.loads(protocol_bytes)
receipt = json.loads((packet / 'diagnostic.json').read_text())
assert receipt['protocolSha256'] == protocol_hash
selection = root / 'docs/verification/phrf-reference-bank-20261008/selected.json'
assert receipt['selectionSha256'] == hashlib.sha256(selection.read_bytes()).hexdigest()
assert not receipt['productionAdmitted']
assert protocol['frozenCandidates'] == [1, 6]
assert protocol['trials'] == [30, 300]
assert [r['trials'] for r in receipt['results']] == [30, 300]
for result in receipt['results']:
    records = result['records']
    assert len(records) == result['attemptedVoxels'] == protocol['voxelsPerGeometry']
    assert [r['voxel'] for r in records] == list(range(protocol['voxelsPerGeometry']))
    assert int(result['setupReferenceAttempts']) == 8
    assert int(result['nodeScores']) == 2 * len(records)
    assert int(result['jets']) <= 2 * len(records)
    assert int(result['factorAttempts']) >= int(result['continuousFactors']) >= 0
    assert int(result['continuousFactors']) == int(result['factorAttempts']) == 0
    assert int(result['readoutExactFactors']) == 0
    assert all(r['status'] == 'CurvatureNotPositive' for r in records)
    assert int(result['reconstructedBands']) >= 6 * len(records)
    for record in records:
        assert record['startingReference'] in protocol['frozenCandidates']
        if record['output'] == 'Emitted':
            assert record['status'] == 'Accepted'
            assert record['readoutReference'] in protocol['frozenCandidates']
            assert int(record['referenceInverseAttempts']) == 3
            assert record['residualCorrections'] == 1
        elif record['output'].startswith('DecodeRefused('):
            assert record['status'] != 'Accepted'
validation = json.loads((packet / 'validation.json').read_text())
assert validation['fitJVM'] == 720 and validation['fitJS'] == 662
assert validation['lawsJVM'] == validation['lawsJS'] == 13
assert validation['compileAll'] and not validation['productionAdmitted']
log = gzip.decompress((packet / 'final-gates.log.gz').read_bytes()).decode()
for count in (720, 662, 13):
    assert f'Passed: Total {count}, Failed 0, Errors 0, Passed {count}' in log
assert 'scalafimCompileAll: exit 0' in log
assert '[error]' not in log
warnings = [line for line in log.splitlines() if '[warn]' in line]
assert all('multiple main classes detected' in line for line in warnings), warnings
followup = gzip.decompress((packet / 'final-followup.log.gz').read_bytes()).decode()
assert 'fitJVM/testOnly' in followup and 'fitJS/testOnly' in followup
assert '[error]' not in followup
assert 'TrialReferenceDecodeMain' in followup and 'exit 0' in followup
assert followup.count('Passed: Total 53, Failed 0, Errors 0, Passed 53') == 2
assert all('multiple main classes detected' in line for line in followup.splitlines() if '[warn]' in line)
print(f'Verified {len(manifest["sha256"])} hashes; all 16 attempted diagnostic cases; JVM/JS gates. Production unqualified.')
