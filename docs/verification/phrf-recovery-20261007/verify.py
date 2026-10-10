#!/usr/bin/env python3
"""Check retained recovery evidence without promoting it to qualification."""
from collections import Counter
import hashlib
import json
from pathlib import Path
import zipfile

packet = Path(__file__).resolve().parent
root = packet.parents[2]
manifest = json.loads((packet / 'manifest.json').read_text())
assert manifest['qualification'] == 'not-admitted'
for name, expected in manifest['sha256'].items():
    actual = hashlib.sha256((root / name).read_bytes()).hexdigest()
    if actual != expected:
        raise SystemExit(f'hash mismatch: {name}')

reference = json.loads((packet / 'reference.json').read_text())
assert reference['complete'] and reference['qualification'] == 'not-admitted'
with zipfile.ZipFile(packet / 'reference-input.zip') as archive:
    for name, expected in reference['inputSha256'].items():
        assert hashlib.sha256(archive.read(name)).hexdigest() == expected, name
    metadata = json.loads(archive.read('input.json'))
ratios = metadata['noiseRatios']
keys = {(ratio, voxel) for ratio in ratios for voxel in range(16)}
ref = {(row['noiseRatio'], row['voxel']): row for row in reference['records']}
assert len(reference['records']) == 80 and set(ref) == keys
assert all(len(row['starts']) == 9 and row['qrRank'] == 306 for row in ref.values())
assert all(row['qrAmplitudeMaxError'] <= 1e-7 for row in ref.values())
assert all(row['scalaEnergyScaledError'] <= 1e-9 for row in ref.values())
assert all(row['scalaTruthGradientScaledError'] <= 1e-9 for row in ref.values())
for stage in metadata['stages']:
    assert len(stage['decoder']) == 16
    assert all(row['status'] != 'Accepted' for row in stage['decoder'])

comparison = json.loads((packet / 'comparison.json').read_text())
assert comparison['qualification'] == 'not-admitted'
for filename, field in [('center-final.json', 'centerStatuses'),
                        ('bank-expanded-control.json', 'bankExpandedStatuses')]:
    candidate = json.loads((packet / filename).read_text())
    assert candidate['qualification'] == 'not-admitted'
    assert len(candidate['records']) == 80
    assert {(r['noiseRatio'], r['voxel']) for r in candidate['records']} == keys
    for row in comparison['rows']:
        ratio = row['noiseRatio']
        selected = [r for r in candidate['records'] if r['noiseRatio'] == ratio]
        assert dict(Counter(r['status'] for r in selected)) == row[field]
        stage = next(s for s in candidate['stages'] if s['noiseRatio'] == ratio)
        assert stage['decoder']['voxels'] == 16
        assert stage['decoder']['jets'] <= 16 * 20
        assert stage['decoder']['exactEvaluations'] <= 16 * 40
        assert stage['decoder']['candidateAttempts'] <= 16 * (16 * 12 + 2)
        if filename == 'center-final.json':
            accepted = [r for r in selected if r['status'] == 'Accepted']
            widths = [u-l for l, u in zip(metadata['lower'], metadata['upper'])]
            shape = max(max(abs(a-b)/w for a, b, w in
                            zip(r['coordinates'], ref[(ratio, r['voxel'])]['coordinates'], widths))
                        for r in accepted)
            energy = max(abs(r['energy'] - ref[(ratio, r['voxel'])]['energy']) /
                         (metadata['rows'] * metadata['signalRms'][r['voxel']]**2) for r in accepted)
            assert abs(shape - row['acceptedMaxShapeDifferenceInChartWidths']) < 1e-15
            assert abs(energy - row['acceptedMaxAbsoluteEnergyGapScaled']) < 1e-15
        rr = [r for r in ref.values() if r['noiseRatio'] == ratio]
        assert sum(r['boundary'] for r in rr) == row['referenceBoundary']
        assert sum(r['interiorStationaryPositive'] for r in rr) == row['referenceInteriorStationaryPositive']

print(f"Verified {len(manifest['sha256'])} hashes, archived input, 80 reference cells and controlled receipts; PHRF remains not admitted.")
