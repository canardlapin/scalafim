#!/usr/bin/env python3
"""Verify the frozen selection, every outcome, resource receipts and source hashes."""
import hashlib
import json
import math
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
def read(name):
    return json.loads((packet / name).read_text())
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

manifest = read('manifest.json')
for group, base in [('sources', root), ('artifacts', packet)]:
    for name, expected in manifest[group].items():
        assert digest(base / name) == expected, name
protocol = read('protocol.json')
protocol_hash = digest(packet / 'protocol.json')
assert protocol_hash == (packet / 'protocol.sha256').read_text().strip()
assert protocol_hash == '3d68e7e4fe106f877608c09468c3cfd62b97b35ec578347bdf5ab92adc88eda1'
assert protocol['targetRelativeAmplitudeL2'] == .001
assert protocol['requiredPrimaryCoverage'] == .95 and protocol['referenceLimit'] == 8
selected = read('selected.json')
development = read('development.json')
assert selected['protocolSha256'] == protocol_hash
assert selected['developmentSha256'] == digest(packet / 'development.json')
# Scala Tuple2 is serialized with named _1 / _2 fields.
def fresh(result):
    return next(c for c in result['coverage'] if c['cohort'] == 'fresh')
best = min(enumerate(development), key=lambda entry: (
    -fresh(entry[1]['_2'])['correctedPasses'], fresh(entry[1]['_2'])['originalP95'], entry[0]))
assert selected['name'] == best[1]['_1']
assert [v['_1'] for v in development] == protocol['development']['candidates']

lower = [math.log(1 / 3), math.log(.4 / .6), .1]
upper = [math.log(.5), math.log(.6 / .4), .3]
width = [b - a for a, b in zip(lower, upper)]
def points(name):
    counts = [int(c) for c in name.split('x')]
    result = []
    for index in range(math.prod(counts)):
        unit = []
        for count in counts:
            unit.append((index % count + .5) / count)
            index //= count
        result.append([lower[i] + unit[i] * width[i] for i in range(3)])
    return result
assert selected['coordinates'] == points(selected['name'])

def verify_result(result, name, n, fresh_count, sensitivity):
    refs = points(name)
    records = result['records']
    assert len(records) == fresh_count + 27 + 2 * sensitivity
    assert len({r['sample']['id'] for r in records}) == len(records)
    assert result['geometry']['rank'] == 8 and result['geometry']['trials'] == n
    groups = {}
    for r in records:
        sample = r['sample']
        groups.setdefault((sample['cohort'], sample['noiseRatio']), []).append(r)
        assert all(0 <= v <= 1 for v in sample['unit'])
        expected = [max(lower[i], min(upper[i], lower[i] + sample['unit'][i] * width[i])) for i in range(3)]
        assert all(abs(a - b) < 1e-14 for a, b in zip(r['coordinates'], expected))
        distances = [sum(((r['coordinates'][i] - p[i]) / width[i]) ** 2 for i in range(3)) for p in refs]
        assert distances[r['reference']] <= min(distances) + 1e-14
        w = r['work']
        assert r['evaluatedReferences'] == 1 and w['residualCorrections'] == 1
        assert w['referenceInverseAttempts'] == w['bandedSolveAttempts'] == w['normalActionApplications'] == 3
        assert w['factorAttempts'] == w['exactReadoutFactorAttempts'] == 0
        assert w['referenceInverseFailures'] == w['bandedSolveFailures'] == 0
        assert w['retainedTrialAmplitudeValues'] == n
        assert r['exactOriginalRelativeError'] <= .001
        for field in ['preparedRelativeError', 'originalRelativeError', 'exactOriginalRelativeError',
                      'originalContrastError', 'relativeTailDesignError', 'relativeBasisDesignError']:
            assert math.isfinite(r[field]) and r[field] >= 0
    assert len(groups[('fresh', .1)]) == fresh_count and len(groups[('boundary', .1)]) == 27
    assert len(groups.get(('sensitivity', 0), [])) == sensitivity
    assert len(groups.get(('sensitivity', 1), [])) == sensitivity
    for coverage in result['coverage']:
        rows = groups[(coverage['cohort'], coverage['noiseRatio'])]
        assert coverage['count'] == len(rows)
        for passed, p95, field in [('correctedPasses', 'originalP95', 'originalRelativeError'),
            ('preparedPasses', 'preparedP95', 'preparedRelativeError'), ('exactPasses', 'exactP95', 'exactOriginalRelativeError')]:
            assert coverage[passed] == sum(r[field] <= .001 for r in rows)
            assert coverage[p95] == sorted(r[field] for r in rows)[math.ceil(.95 * len(rows)) - 1]
    primary = fresh(result)
    passed = primary['correctedPasses'] / primary['count'] >= .95
    assert result['conditionalScenario']['ciPass'] == passed
    assert result['productionScenario']['status'] == 'Fail' and not result['productionScenario']['ciPass']
    return len(records)

count = sum(verify_result(v['_2'], v['_1'], 300, 256, 0) for v in development)
confirmation = read('confirmation.json')
assert confirmation['protocolSha256'] == protocol_hash
assert confirmation['selectionSha256'] == digest(packet / 'selected.json')
for result, n, fc, sc in zip(confirmation['results'], [30, 300], [64, 256], [32, 64]):
    count += verify_result(result, selected['name'], n, fc, sc)
    dev = development[0]['_2']['records']
    fresh_records = [r for r in result['records'] if r['sample']['cohort'] == 'fresh']
    assert not ({r['sample']['responseSeed'] for r in fresh_records} & {r['sample']['responseSeed'] for r in dev})
    assert not ({tuple(r['sample']['unit']) for r in fresh_records} & {tuple(r['sample']['unit']) for r in dev})
    for r in result['records']:
        if r['sample']['cohort'] == 'sensitivity':
            primary = next(v for v in fresh_records if v['sample']['responseSeed'] == r['sample']['responseSeed'])
            assert primary['sample']['unit'] == r['sample']['unit']

stress = read('stress.json')
for key in ['fullReferenceBytes', 'referenceBytes', 'reconstructionScratchBytes', 'scopedEightWorkersBytes', 'sharedBytes', 'workerBytes', 'retainedCurvatureScratchBytes', 'retainedBankScratchBytes']:
    stress[key] = int(stress[key])
assert stress['selectedBank'] == selected['name']
assert stress['curvatureWorkersWarmed'] == 8
assert stress['retainedCurvatureScratchBytes'] == 8 * stress['reconstructionScratchBytes']
assert stress['retainedBankScratchBytes'] == 0
assert stress['protocolSha256'] == protocol_hash and stress['rank'] == 8 and stress['bandwidth'] == 240
assert stress['fullReferenceBytes'] - stress['referenceBytes'] == 6 * 1200 * 241 * 8
assert stress['reconstructionScratchBytes'] == 1200 * 241 * 8
assert stress['scopedEightWorkersBytes'] == stress['sharedBytes'] + 8 * stress['workerBytes']
assert stress['terminalJetCompleted']
work = stress['terminalWork']['attempted']
assert work['reconstructedBands'] == 6 and work['reconstructedBandProducts'] == 6 * 36 * 1200 * 241
assert work['factorAttempts'] == 0 and work['solveAttempts'] == 10
assert stress['readoutWork']['referenceInverseAttempts'] == 3 and stress['readoutWork']['residualCorrections'] == 1
assert int(stress['reachableBankEightCurvatureAndEightReadoutWorkers']['bytes']) > int(stress['reachableBankAndEightWorkers']['bytes'])
assert int(stress['reachableBankEightCurvatureAndEightReadoutWorkers']['bytes']) < 256 * 1024**2
validation = read('validation.json')
assert validation['fitJVM'] > 700 and validation['fitJS'] > 640
assert validation['lawsJVM'] >= 12 and validation['lawsJS'] >= 12
assert not validation['defaultsChanged'] and not validation['productionAdmitted']
assert validation['compilerWarnings'] == []
print(f'verified {sum(len(v) for v in manifest.values())} hashes and all {count} outcomes; bank {selected["name"]}; production remains unqualified')
