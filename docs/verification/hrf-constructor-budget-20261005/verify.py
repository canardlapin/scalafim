#!/usr/bin/env python3
"""Run constructor gates and bounded mutants, restoring source after each run.

Run only while holding the shared checkout's build/mutation slot:
  python3 docs/verification/hrf-constructor-budget-20261005/verify.py
Logs and receipt.json are written beside this runner; source, logs and command
metadata are retained in logs-and-sources.tar.gz. Mutants run only named tests
that cannot enter oversized calibration loops or allocate oversized arrays.
"""
from pathlib import Path
import datetime
import hashlib
import json
import re
import subprocess
import tarfile
import time

ROOT = Path(__file__).resolve().parents[3]
OUT = Path(__file__).resolve().parent
FUNCTIONS = ROOT / 'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/HrfFunctions.scala'
POLICY = ROOT / 'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/HrfConstructorCalibration.scala'
SPEC = ROOT / 'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/HrfSpec.scala'
SUITE = 'scalafim.fmri.hrf.HrfConstructorCalibrationSuite'
SELECTOR = ROOT / 'modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/HrfConstructorMutationSuite.scala'
records = []

def run(label, commands, expected):
    command = ['python3', 'tools/build/sbt-warm', *commands]
    start = time.monotonic()
    result = subprocess.run(command, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    log = OUT / (label + '.log')
    log.write_text(result.stdout)
    counts = [dict(zip(('total', 'failed', 'errors', 'passed'), map(int, match))) for match in
        re.findall(r'Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', result.stdout)]
    record = dict(label=label, command=command, exit_code=result.returncode,
                  expected=expected, seconds=round(time.monotonic() - start, 3), counts=counts)
    records.append(record)
    print(json.dumps(record), flush=True)
    if expected == 'pass' and result.returncode != 0:
        raise RuntimeError('gate failed: ' + label)
    if expected == 'test-failure' and (result.returncode == 0 or not any(c['failed'] > 0 for c in counts)):
        raise RuntimeError('mutant not killed by a test failure: ' + label)

run('gates-before', [f'{p}/testOnly {SUITE} scalafim.fmri.hrf.HrfRParitySuite scalafim.fmri.hrf.LwuSuite'
                    for p in ('hrfJVM', 'hrfJS')], 'pass')
mutants = [
    ('lwu-old-count', POLICY,
     'NormalizationReferenceGrid.stepped("lwu", span, LwuStep, 2.0)\n          .left.map(HrfConstructorError.InvalidCalibration.apply)',
     'Right(math.ceil(span.value / LwuStep.value).toInt + 1)',
     'LWU-calibration-sample-boundary'),
    ('daguerre-basis-only-work', POLICY,
     '2.0 * nBasis.toDouble + math.max(0.0, nBasis.toDouble - 2.0)', 'nBasis.toDouble',
     'Daguerre-calibration-recurrence-work-boundary'),
    ('nonfinite-area-scale', FUNCTIONS,
     'if !factor.isFinite then', 'if false then',
     'finite-LWU-inputs-refuse-overflowing-area-calibration'),
    ('spec-throwing-route', SPEC,
     'Hrfs.daguerreValidated(nBasis = nbasis, span = span.seconds).left.map(HrfSpecError.InvalidConstructor.apply)',
     'Right(Hrfs.daguerre(nBasis = nbasis, span = span.seconds))',
     'HrfSpec-constructor-refusal-is-typed')
]
for label, path, old, new, test in mutants:
    original = path.read_text()
    if original.count(old) != 1:
        raise RuntimeError('mutation anchor must be unique: ' + label)
    try:
        path.write_text(original.replace(old, new))
        (OUT / (label + '.scala')).write_text(path.read_text())
        selector_text = f'''package scalafim.fmri.hrf

class HrfConstructorMutationSuite extends HrfConstructorCalibrationSuite:
  override def munitTests(): Seq[munit.Test] =
    val selected = super.munitTests().filter(_.name == {json.dumps(test)})
    require(selected.size == 1)
    selected
'''
        if SELECTOR.exists():
            raise RuntimeError('mutation selector already exists')
        SELECTOR.write_text(selector_text)
        (OUT / (label + '-selector.scala')).write_text(selector_text)
        for platform in ('hrfJVM', 'hrfJS'):
            run(label + '-' + platform, [f'{platform}/testOnly scalafim.fmri.hrf.HrfConstructorMutationSuite'], 'test-failure')
    finally:
        path.write_text(original)
        SELECTOR.unlink(missing_ok=True)
run('gates-restored', [f'{p}/testOnly {SUITE}' for p in ('hrfJVM', 'hrfJS')], 'pass')
paths = [POLICY, SPEC, ROOT / 'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/HrfFunctions.scala',
         ROOT / 'modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/HrfConstructorCalibrationSuite.scala']
archive = OUT / 'logs-and-sources.tar.gz'
with tarfile.open(archive, 'w:gz') as tar:
    for path in sorted(OUT.glob('*.log')) + sorted(OUT.glob('*.scala')) + [Path(__file__).resolve()]:
        tar.add(path, arcname=path.name)
    for path in paths:
        tar.add(path, arcname=str(path.relative_to(ROOT)))
receipt = dict(verified_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
    mote='bd-01M47BDP95QPW2SZEV7TF52RQX', runs=records,
    mutation_safety='Named selectors exercise count/work admission only, or typed specification mapping; no oversized loops or allocations run under mutants.',
    source_sha256={str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},
    archive=archive.name, archive_sha256=hashlib.sha256(archive.read_bytes()).hexdigest(),
    scope_limits=['Known family scalar work units bound loops and allocation, not wall clock time.',
                  'Unnormalized LWU does not use the calibration grid; valid parameters and span are still required.',
                  'Existing R parity fixtures and declared Daguerre fixed-grid semantics are preserved.'])
(OUT / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
for path in list(OUT.glob('*.log')) + list(OUT.glob('*.scala')):
    path.unlink()
