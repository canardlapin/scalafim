#!/usr/bin/env python3
"""Run focused JVM/JS gates and bounded admission mutations in an exclusive build slot."""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tarfile
import tempfile
import time

ROOT = Path(__file__).resolve().parents[3]
OUT = Path(__file__).resolve().parent
SOURCE = ROOT / 'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/FamilySummaryGrid.scala'
SELECTOR = ROOT / 'modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/family/FamilySummaryMutationSuite.scala'
SELECTOR_TEXT = '''package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{PositiveSeconds, Seconds}

class FamilySummaryMutationSuite extends munit.FunSuite:
  private def positive(value: Double): PositiveSeconds = PositiveSeconds(value).toOption.get

  test("tail sample cap mutation is detected at one extra sample"):
    val result = FamilySummaryGrid.tail(positive(250000.0), positive(1.0), 4.0)
    assertEquals(result, Left(FamilySummaryError.SampleLimitExceeded(1000001.0, FamilySummaryGrid.MaxSamples)))

  test("LWU sample cap mutation is detected at one extra sample"):
    val family = LwuFamily.make(horizon = Seconds(10000.0)).toOption.get
    val point = family.chart.point(6.0, math.log(2.0), 0.4).toOption.get
    assertEquals(family.summariesEither(point), Left(FamilySummaryError.SampleLimitExceeded(1000001.0, FamilySummaryGrid.MaxSamples)))

  test("finite last lag mutation is detected with three safe samples"):
    val result = FamilySummaryGrid.tail(positive(1e308), positive(1e308), 1.1)
    assert(result.swap.exists(_.isInstanceOf[FamilySummaryError.NonFiniteLastTime]))
'''

SOURCES = [
    'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/FamilySummaryGrid.scala',
    'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/ParametricHrfFamily.scala',
    'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/LwuFamily.scala',
    'modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/family/FamilySummaryGridSuite.scala',
    'modules/design/shared/src/main/scala/scalafim/fmri/design/hrf/KernelBasis.scala',
    'modules/design/shared/src/test/scala/scalafim/fmri/design/HrfKernelBasisSuite.scala',
]

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--skip-mutations', action='store_true')
    args = parser.parse_args()
    runs = []
    env = os.environ.copy()
    env['COURSIER_REPOSITORIES'] = 'https://repo.maven.apache.org/maven2'
    original = SOURCE.read_text()
    assert not SELECTOR.exists(), 'temporary selector already exists'
    hashes = {path: digest(ROOT / path) for path in SOURCES}
    with tempfile.TemporaryDirectory(prefix='scalafim-family-summary-') as directory:
        temp = Path(directory)
        for path in SOURCES:
            snapshot = temp / 'sources' / path
            snapshot.parent.mkdir(parents=True, exist_ok=True)
            snapshot.write_bytes((ROOT / path).read_bytes())

        def run(label, commands, expected_failure=False):
            command = ['python3', 'tools/build/sbt-warm', *commands]
            start = time.monotonic()
            process = subprocess.run(command, cwd=ROOT, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
            log = temp / (label + '.log')
            log.write_text(process.stdout)
            counts = [dict(zip(('total', 'failed', 'errors', 'passed'), map(int, row))) for row in re.findall(r'Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', process.stdout)]
            record = dict(label=label, command=command, exit_code=process.returncode, seconds=round(time.monotonic() - start, 3), counts=counts, log=log.name)
            runs.append(record)
            print(json.dumps(record), flush=True)
            if expected_failure:
                assert process.returncode != 0 and counts and any(row['failed'] > 0 for row in counts), process.stdout[-12000:]
                assert not re.search(r'\[error\].*(?:-- |Compilation failed|not found)', process.stdout), process.stdout[-12000:]
            else:
                assert process.returncode == 0 and counts and all(row['failed'] == row['errors'] == 0 for row in counts), process.stdout[-12000:]

        suites = 'scalafim.fmri.hrf.family.FamilySummaryGridSuite scalafim.fmri.hrf.family.GaussianFamilySuite scalafim.fmri.hrf.family.LwuFamilySuite'
        run('baseline-family', ['hrfJVM/testOnly ' + suites, 'hrfJS/testOnly ' + suites])
        run('baseline-consumer', ['designJVM/testOnly scalafim.fmri.design.HrfKernelBasisSuite', 'designJS/testOnly scalafim.fmri.design.HrfKernelBasisSuite'])
        try:
            if not args.skip_mutations:
                SELECTOR.write_text(SELECTOR_TEXT)
                (temp / 'FamilySummaryMutationSuite.scala').write_text(SELECTOR_TEXT)
                selector = 'testOnly scalafim.fmri.hrf.family.FamilySummaryMutationSuite'
                run('mutation-selectors-baseline', ['hrfJVM/' + selector, 'hrfJS/' + selector])
                mutations = [
                    ('sample-cap', 'count > MaxSamples.toDouble', 'count > MaxSamples.toDouble + 1.0'),
                    ('last-time', 'if !last.isFinite then', 'if false then'),
                ]
                for label, before, after in mutations:
                    assert original.count(before) == 1
                    SOURCE.write_text(original.replace(before, after))
                    (temp / (label + '.scala')).write_text(SOURCE.read_text())
                    try:
                        for platform in ('JVM', 'JS'):
                            run('mutant-' + label + '-' + platform.lower(), ['hrf' + platform + '/' + selector], expected_failure=True)
                    finally:
                        SOURCE.write_text(original)
                run('restored-selectors', ['hrfJVM/' + selector, 'hrfJS/' + selector])
        finally:
            SOURCE.write_text(original)
            SELECTOR.unlink(missing_ok=True)
        run('restored-family', ['hrfJVM/testOnly ' + suites, 'hrfJS/testOnly ' + suites])
        assert hashes == {path: digest(ROOT / path) for path in SOURCES}, 'source changed during exclusive verification'
        archive = OUT / 'logs-and-sources.tar.gz'
        with tarfile.open(archive, 'w:gz') as tar:
            for path in sorted(temp.rglob('*')):
                if path.is_file():
                    tar.add(path, arcname=str(path.relative_to(temp)))
        receipt = dict(verified_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(), mote='bd-01M47BDR7J7FKGXESZFY9J370P', runs=runs, source_sha256=hashes, archive=archive.name, archive_sha256=digest(archive), source_restored=True, mutation_limits='Sample-cap mutant admits only one extra sample (1,000,001), at most 2,000,002 array cells; last-time mutant uses three samples. Temporary selectors are removed.', scientific_fixtures='Independent Python scalar Gaussian/LWU formula fixtures; existing LWU family tests compare raw library values. No statistical algorithm was ported.', scope_limits=['Bounds lag/value arrays and scalar sample count for one summary call, not custom family callback internals.', 'KernelBasis preflights tail grid and propagates typed diagnostic failures. Full nodes/jet/matrix/SVD and total held-out work admission remain a follow-up.'])
        (OUT / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')

if __name__ == '__main__':
    main()
