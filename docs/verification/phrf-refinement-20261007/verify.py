#!/usr/bin/env python3
"""Verify bounded-search diagnostic evidence; never grant qualification."""
from collections import Counter
import hashlib
import gzip
import json
import math
from pathlib import Path
import zipfile

PACKET = Path(__file__).resolve().parent
ROOT = PACKET.parents[2]
REFERENCE = PACKET.parent / 'phrf-recovery-20261007'


def count(stage, name):
    # ujson preserves Scala Long counters as decimal strings.
    raw = stage[name]
    assert isinstance(raw, str) and raw.isascii() and raw.isdecimal(), (name, raw)
    value = int(raw)
    assert str(value) == raw
    return value


def comparison():
    reference = json.loads((REFERENCE / 'reference.json').read_text())
    assert reference['complete'] and reference['qualification'] == 'not-admitted'
    with zipfile.ZipFile(REFERENCE / 'reference-input.zip') as archive:
        for name, expected in reference['inputSha256'].items():
            assert hashlib.sha256(archive.read(name)).hexdigest() == expected, name
        metadata = json.loads(archive.read('input.json'))
    final = json.loads(gzip.decompress((PACKET / 'final.json.gz').read_bytes()))
    assert final['complete'] and final['qualification'] == 'not-admitted'
    assert final['format'] == 'phrf-bounded-search-diagnostic/1'
    assert final['budget'] == 'DecodeBudget(2,16,901,40,Vector(),0.0,30,1.0E-6,BoundedMultistart)'
    ratios = metadata['noiseRatios']
    keys = {(ratio, voxel) for ratio in ratios for voxel in range(16)}
    ref = {(r['noiseRatio'], r['voxel']): r for r in reference['records']}
    actual = {(r['noiseRatio'], r['voxel']): r for r in final['records']}
    assert len(reference['records']) == len(final['records']) == 80
    assert set(ref) == set(actual) == keys
    assert len(final['stages']) == 5
    assert {s['noiseRatio'] for s in final['stages']} == set(ratios)
    widths = [u-l for l, u in zip(metadata['lower'], metadata['upper'])]
    starts = {(0.5, 0.5, 0.5)} | {
        tuple(0.75 if bits & (1 << axis) else 0.25 for axis in range(3))
        for bits in range(8)
    }
    rows = []
    for ratio in ratios:
        records = [actual[(ratio, voxel)] for voxel in range(16)]
        stage = next(s for s in final['stages'] if s['noiseRatio'] == ratio)
        interior = sum(ref[(ratio, v)]['interiorStationaryPositive'] for v in range(16))
        boundary = sum(ref[(ratio, v)]['boundary'] for v in range(16))
        assert interior + boundary == 16
        errors, shape_errors, search_evaluations = [], [], 0
        search_stops = Counter()
        for r in records:
            expected = ref[(ratio, r['voxel'])]
            assert (r['status'] == 'Accepted') == expected['interiorStationaryPositive']
            assert r['budgetExit'] is None
            assert len(r['coordinates']) == 3 and len(r['hessian']) == 9
            assert all(math.isfinite(v) for v in [r['energy']] + r['coordinates'] + r['hessian'])
            scale = metadata['rows'] * metadata['signalRms'][r['voxel']] ** 2
            error = abs(r['energy'] - expected['energy']) / scale
            shape_error = max(abs(a-b)/w for a, b, w in
                              zip(r['coordinates'], expected['coordinates'], widths))
            assert error <= 1e-9, (ratio, r['voxel'], error)
            if ratio <= 0.1 or (ratio == 1 and r['voxel'] == 3):
                assert shape_error <= 1e-5, (ratio, r['voxel'], shape_error)
            errors.append(error)
            shape_errors.append(shape_error)
            search = r['search']
            assert search['requestedStarts'] == len(search['trajectories']) == 9
            assert search['energyTieToleranceScaled'] == 1e-12
            assert math.isfinite(search['scale']) and search['scale'] >= 1
            assert {tuple(t['initialUnit']) for t in search['trajectories']} == starts
            lowest = math.inf
            for t in search['trajectories']:
                assert 1 <= t['evaluations'] <= 90
                assert 0 <= t['iterations'] <= 89
                assert 0 <= t['rejectedSteps'] <= t['evaluations']
                assert 0 <= t['metricResets'] <= t['iterations']
                p = t['point']
                assert p is not None
                assert len(p['unit']) == len(p['gradientScaled']) == 3
                assert all(0 <= x <= 1 for x in p['unit'])
                assert all(math.isfinite(v) for v in p['gradientScaled'] +
                           [p['energyScaled'], p['projectedGradient']])
                assert p['projectedGradient'] >= 0
                lowest = min(lowest, p['energyScaled'])
                search_evaluations += t['evaluations']
                search_stops[t['status']] += 1
            assert r['energy'] / search['scale'] <= lowest + 1e-9
        decoder = stage['decoder']
        assert count(stage, 'attempted') == decoder['voxels'] == 16
        assert decoder['nodeScores'] == 16 * 8
        assert search_evaluations + 16 <= decoder['jets'] <= 16 * 901
        assert decoder['exactEvaluations'] <= 16 * 40
        assert decoder['candidateAttempts'] <= 16 * (16 * 30 + 1 + 901)
        assert decoder['terminalVerifications'] >= 16
        assert count(stage, 'emitted') == count(stage, 'offNodeOutputs') == interior
        assert count(stage, 'outputValues') == interior * metadata['trials']
        assert count(stage, 'float32BytesConsumed') == count(stage, 'outputValues') * 4
        assert count(stage, 'retainedSinkBytes') == metadata['trials'] * 4
        assert math.isfinite(stage['outputChecksum'])
        assert 0 <= stage['maxPreparedResidual'] < 1e-8
        assert 0 <= stage['maxFloat32Error'] < 1e-5
        assert math.isfinite(stage['seconds']) and stage['seconds'] > 0
        rows.append({
            'noiseRatio': ratio,
            'referenceInterior': interior,
            'referenceBoundary': boundary,
            'statuses': dict(Counter(r['status'] for r in records)),
            'maxAbsoluteEnergyGapScaled': max(errors),
            'maxShapeDifferenceInChartWidths': max(shape_errors),
            'searchEvaluations': search_evaluations,
            'searchStops': dict(search_stops),
            'chargedJets': decoder['jets'],
            'executionSeconds': stage['seconds'],
        })
    assert sum(count(s, 'emitted') for s in final['stages']) == 60
    return {
        'qualification': 'not-admitted',
        'rows': rows,
        'counterexamples': [actual[(0.1, 10)], actual[(1, 3)]],
        'totalExecutionSeconds': sum(s['seconds'] for s in final['stages']),
        'totalFloat32BytesConsumed': sum(count(s, 'float32BytesConsumed') for s in final['stages']),
    }


def main():
    manifest = json.loads((PACKET / 'manifest.json').read_text())
    assert manifest['qualification'] == 'not-admitted'
    for name, expected in manifest['sha256'].items():
        actual = hashlib.sha256((ROOT / name).read_bytes()).hexdigest()
        if actual != expected:
            raise SystemExit(f'hash mismatch: {name}')
    assert comparison() == json.loads((PACKET / 'comparison.json').read_text())
    pin = manifest['galeRevision']
    assert f'lazy val galeRevision = "{pin}"' in (ROOT / 'build.sbt').read_text()
    pr = json.loads((PACKET / 'gale-pr-21.json').read_text())
    assert pr['state'] == 'MERGED' and pr['mergeCommit']['oid'] == pin
    with zipfile.ZipFile(PACKET / 'gale-source.zip') as archive:
        for name, expected in manifest['galeSourceSha256'].items():
            assert hashlib.sha256(archive.read(name)).hexdigest() == expected, name
    validation = json.loads((PACKET / 'validation.json').read_text())
    assert validation['noLocalGaleOverride'] and validation['galeRevision'] == pin
    assert all(gate['exit'] == 0 for gate in validation['pinnedGates'])
    assert all(gate['exit'] == 0 for gate in validation['restoredSuiteGates'])
    fullopt = json.loads((PACKET / 'fullopt-comparison.json').read_text())
    assert fullopt == validation['fullOptDiagnostic']
    assert fullopt['qualification'] == 'not-admitted'
    assert fullopt['fullOptConfirmed'] and fullopt['testsPassed'] == 1 and fullopt['voxels'] == 16
    suite = ROOT / 'modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/TrialRefinementSuite.scala'
    assert hashlib.sha256(suite.read_bytes()).hexdigest() == fullopt['originalSuiteSha256']
    probe = (PACKET / 'fullopt-probe-source.scala').read_bytes()
    assert hashlib.sha256(probe).hexdigest() == fullopt['temporarySuiteSha256']
    selector = (b'  // Temporary fullOpt diagnostic selector; the complete suite is restored after capture.\n'
                b'  override def munitTests() = super.munitTests().filter(_.name.endsWith("noise ratio 0.1"))\n')
    assert probe.replace(selector, b'') == suite.read_bytes()
    replay = json.loads((PACKET / 'kernel-replay.json').read_text())
    assert replay['qualification'] == 'not-admitted'
    assert all(error == 0 for error in replay['errors'].values())
    for name, values in replay['milliseconds'].items():
        assert len(values) == 7 and all(math.isfinite(v) and v > 0 for v in values)
        assert sorted(values)[3] == replay['medianMilliseconds'][name]
    for archive, summary in [('js-live.cpuprofile.gz', 'js-profile-summary.json'),
                             ('js-live-second.cpuprofile.gz', 'js-profile-second-summary.json')]:
        profile = json.loads(gzip.decompress((PACKET / archive).read_bytes()))
        retained = json.loads((PACKET / summary).read_text())
        assert retained['qualification'] == 'not-admitted'
        assert retained['samples'] == len(profile['samples']) == len(profile['timeDeltas'])
        assert retained['seconds'] == sum(profile['timeDeltas']) / 1e6
        assert sum(row['microseconds'] for row in retained['selfTime']) == sum(profile['timeDeltas'])
    print(f"Verified {len(manifest['sha256'])} hashes, provider pin, 80 cells, "
          "720 searches and 60 public outputs; PHRF remains not admitted.")


if __name__ == '__main__':
    main()
