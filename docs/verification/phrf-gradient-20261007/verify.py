#!/usr/bin/env python3
"""Check first-order search parity/work evidence; this never grants PHRF admission."""
import gzip
import hashlib
import importlib.util
import json
import re
from pathlib import Path
import statistics
import sys
from collections import Counter
sys.dont_write_bytecode = True

PACKET = Path(__file__).resolve().parent
ROOT = PACKET.parents[2]
PREVIOUS = PACKET.parent / 'phrf-refinement-20261007'
spec = importlib.util.spec_from_file_location('refinement_checks', PREVIOUS / 'verify.py')
previous = importlib.util.module_from_spec(spec)
spec.loader.exec_module(previous)
previous.PACKET = PACKET


def comparison():
    result = previous.comparison()
    before = json.loads(gzip.decompress((PREVIOUS / 'final.json.gz').read_bytes()))
    after = json.loads(gzip.decompress((PACKET / 'final.json.gz').read_bytes()))
    assert len(before['records']) == len(after['records']) == 80
    # Preserve every trajectory, terminal status, energy, coordinate and Hessian.
    # This is stronger than independent-oracle tolerances, and excludes wall times.
    assert before['records'] == after['records']
    for old, new, row in zip(before['stages'], after['stages'], result['rows']):
        assert old['noiseRatio'] == new['noiseRatio']
        for key, value in old['decoder'].items():
            assert new['decoder'][key] == value, (key, old['noiseRatio'])
        assert new['decoder']['firstOrderAttempts'] == row['searchEvaluations']
        assert new['decoder']['jets'] > new['decoder']['firstOrderAttempts']
        attempts = new['trialAttempted']
        assert attempts['firstOrderAttempts'] == row['searchEvaluations']
        assert attempts['firstOrderFailures'] == 0
        old_numbers = list(map(int, re.findall(r'\d+', old['trialWork'])))
        assert len(old_numbers) == 31
        old_attempts = old_numbers[9:]
        assert old_attempts[6] - attempts['solveAttempts'] == 12 * row['searchEvaluations']
        assert old_attempts[8] - attempts['rightHandSideAttempts'] == 60 * row['searchEvaluations']
        assert old_attempts[10] - attempts['jetAttempts'] == row['searchEvaluations']
        for key in ['attempted', 'emitted', 'offNodeOutputs', 'outputValues', 'float32BytesConsumed',
                    'outputChecksum', 'maxFloat32Error', 'maxPreparedResidual', 'retainedSinkBytes']:
            assert old[key] == new[key], (key, old['noiseRatio'])
    result['identicalTrajectoriesAndOutputs'] = True
    result['firstOrderAttempts'] = sum(s['decoder']['firstOrderAttempts'] for s in after['stages'])
    result['fullJetAttempts'] = sum(s['decoder']['jets'] - s['decoder']['firstOrderAttempts'] for s in after['stages'])
    result['previousExecutionSeconds'] = sum(s['seconds'] for s in before['stages'])
    result['diagnosticEndToEndSpeedup'] = result['previousExecutionSeconds'] / result['totalExecutionSeconds']
    return result


def main():
    manifest = json.loads((PACKET / 'manifest.json').read_text())
    assert manifest['qualification'] == 'not-admitted'
    for name, expected in manifest['sha256'].items():
        assert hashlib.sha256((ROOT / name).read_bytes()).hexdigest() == expected, name
    assert comparison() == json.loads((PACKET / 'comparison.json').read_text())
    assert f'lazy val galeRevision = "{manifest["galeRevision"]}"' in (ROOT / 'build.sbt').read_text()
    timing = json.loads((PACKET / 'paired-timing.json').read_text())
    for platform in ['JVM', 'FullOpt']:
        rows = timing[platform]['rows']
        assert len(rows) == 10
        assert {r['repeat'] for r in rows} == set(range(5))
        for r in rows:
            assert r['calls'] == 256
            assert r['solves'] == 256 * (9 if r['gradient'] else 21)
            assert r['rhs'] == 256 * (43 if r['gradient'] else 103)
        assert len({r['checksum'] for r in rows}) == 1
        medians = {mode: statistics.median(r['seconds'] for r in rows if r['gradient'] == gradient)
                   for mode, gradient in [('full', False), ('firstOrder', True)]}
        assert timing[platform]['medianSeconds'] == medians
        assert timing[platform]['speedup'] == medians['full'] / medians['firstOrder']
    for label in ['baseline', 'first-order']:
        profile = json.loads(gzip.decompress((PACKET / f'fullopt-{label}.cpuprofile.gz').read_bytes()))
        receipt = json.loads((PACKET / f'{label}-cpu-shares.json').read_text())
        assert receipt['samples'] == len(profile['samples']) == len(profile['timeDeltas'])
        nodes = {n['id']: n for n in profile['nodes']}
        samples = Counter()
        for node, micros in zip(profile['samples'], profile['timeDeltas']):
            samples[nodes[node]['callFrame']['functionName']] += micros
        total = sum(samples.values())
        assert receipt['seconds'] == total / 1e6
        for part, expected in receipt['shares'].items():
            assert expected == sum(v for k, v in samples.items() if part in k) * 100 / total
    validation = json.loads((PACKET / 'validation.json').read_text())
    assert validation['noLocalGaleOverride']
    assert all(g['exit'] == 0 for g in validation['gates'])
    assert validation['fullOptTestsPassed'] == 8
    assert validation['fullOptCombinedCommandExit'] == 1
    assert validation['defaultStageRestored']
    fullopt_log = gzip.decompress((PACKET / 'paired-fullopt.log.gz').read_bytes()).decode()
    assert 'Passed: Total 8, Failed 0, Errors 0, Passed 8' in fullopt_log
    final_log = gzip.decompress((PACKET / 'final-gates.log.gz').read_bytes()).decode()
    assert '[info] FastOpt' in final_log
    assert not (ROOT / 'modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/TrialOracleProfileSuite.scala').exists()
    print(f"Verified {len(manifest['sha256'])} hashes, 80 identical results, 720 identical trajectories, "
          "60 identical outputs and first-order work/timing receipts; PHRF remains not admitted.")


if __name__ == '__main__':
    main()
