#!/usr/bin/env python3
"""Check the frozen domain protocol, all attempted outcomes and source provenance."""
import hashlib
import json
import math
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]


def read(name):
    return json.loads((packet / name).read_text())


manifest = read('manifest.json')
checked = 0
for group, base in [('sources', root), ('artifacts', packet)]:
    for name, digest in manifest[group].items():
        assert hashlib.sha256((base / name).read_bytes()).hexdigest() == digest, name
        checked += 1
protocol = read('protocol.json')
protocol_hash = hashlib.sha256((packet / 'protocol.json').read_bytes()).hexdigest()
assert protocol_hash == (packet / 'protocol.sha256').read_text().split()[0]
assert protocol_hash == '29fc8e066b28d3d15cf1879868b7096555e059e318d93d868f849cdc2c586aa3'
assert protocol['referenceBank']['count'] == 8
assert protocol['referenceBank']['evaluatedReferences'] == 1
assert not protocol['referenceBank']['exactFallback']
assert protocol['coverage']['amplitudeRelativeL2'] == .001
assert protocol['supportSeconds'] == 96

lower = [math.log(1 / 3), math.log(.4 / .6), .1]
upper = [math.log(.5), math.log(.6 / .4), .3]
width = [b - a for a, b in zip(lower, upper)]
refs = [[lower[i] + .25 * width[i] if (n & (1 << i)) == 0 else upper[i] - .25 * width[i]
         for i in range(3)] for n in range(8)]

gate = read('gate.json')
assert gate['protocolSha256'] == protocol_hash and gate['qualification'] == 'not-admitted'
assert 0 < gate['tailMassUpper'] < .0004
assert len(gate['shapeSummaries']) == 27
for result, n, fresh, sensitive, total, expected_passes in zip(
    gate['result'], [30, 300], [64, 256], [32, 64], [155, 411], [64, 232]
):
    assert result['geometry']['trials'] == n and result['geometry']['rank'] == 8
    records = result['records']
    assert len(records) == total and len({r['sample']['id'] for r in records}) == total
    groups = {}
    for r in records:
        s = r['sample']
        key = (s['cohort'], s['noiseRatio'])
        groups.setdefault(key, []).append(r)
        assert r['evaluatedReferences'] == 1
        w = r['work']
        assert w['referenceInverseAttempts'] == w['bandedSolveAttempts'] == 3
        assert w['residualCorrections'] == 1 and w['normalActionApplications'] == 3
        assert w['exactReadoutFactorAttempts'] == w['factorAttempts'] == 0
        assert w['referenceInverseFailures'] == w['bandedSolveFailures'] == 0
        assert w['retainedTrialAmplitudeValues'] == n
        assert all(0 <= x <= 1 for x in s['unit'])
        for i, x in enumerate(r['coordinates']):
            expected = max(lower[i], min(upper[i], lower[i] + s['unit'][i] * width[i]))
            assert abs(x - expected) < 1e-14
        distances = [sum(math.pow((r['coordinates'][i] - ref[i]) / width[i], 2) for i in range(3)) for ref in refs]
        assert distances[r['reference']] <= min(distances) + 1e-14
        for k in ['preparedRelativeError', 'originalRelativeError', 'exactOriginalRelativeError',
                  'originalContrastError', 'relativeTailDesignError', 'relativeBasisDesignError']:
            assert math.isfinite(r[k]) and r[k] >= 0
        assert r['exactOriginalRelativeError'] <= .001
    assert len(groups[('fresh', .1)]) == fresh
    assert len(groups[('boundary', .1)]) == 27
    assert len(groups[('sensitivity', 0)]) == len(groups[('sensitivity', 1)]) == sensitive
    for c in result['coverage']:
        rows = groups[(c['cohort'], c['noiseRatio'])]
        assert c['count'] == len(rows)
        for metric, field in [('correctedPasses', 'originalRelativeError'), ('preparedPasses', 'preparedRelativeError'),
                              ('exactPasses', 'exactOriginalRelativeError')]:
            assert c[metric] == sum(r[field] <= .001 for r in rows)
        for metric, field in [('originalP95', 'originalRelativeError'), ('preparedP95', 'preparedRelativeError'),
                              ('exactP95', 'exactOriginalRelativeError')]:
            assert c[metric] == sorted(r[field] for r in rows)[math.ceil(.95 * len(rows)) - 1]
    primary = next(c for c in result['coverage'] if c['cohort'] == 'fresh')
    assert primary['correctedPasses'] == expected_passes
    passed = primary['correctedPasses'] / primary['count'] >= .95
    assert result['conditionalScenario']['ciPass'] == passed
    assert result['conditionalScenario']['status'] == ('Pass' if passed else 'Fail')
    assert result['productionScenario']['status'] == 'Fail' and not result['productionScenario']['ciPass']

stress = read('stress.json')
assert stress['protocolSha256'] == protocol_hash and stress['qualification'] == 'not-admitted'
assert stress['result']['status'] == 'prepared'
g = stress['result']['geometry']
assert g['trials'] == 1200 and g['rank'] == 8
assert g['preparationScalars'] == 11390325 < 16000000
assert g['sharedWithFullBankBytes'] == 327114344 > 256 * 1024**2
assert g['sharedWithValueBankBytes'] == 148042280 < 256 * 1024**2
assert g['scopedFullBankEightWorkersBytes'] == g['sharedWithFullBankBytes'] + 8 * g['workerBytes']

validation = read('validation.json')
assert validation['jvmPassed'] == validation['jsPassed'] == 9
assert not validation['productionAdmitted'] and not validation['defaultsChanged']
assert not validation['productionSourceChanged'] and not validation['galeRevisionChanged']
assert validation['compilerWarnings'] == []
print(f'verified {checked} hashes and all 566 outcomes; N=300 coverage and full-bank stress memory remain NO-GO')
