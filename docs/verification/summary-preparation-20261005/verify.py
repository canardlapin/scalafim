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
SOURCES = [
    'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/ParametricHrfFamily.scala',
    'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/LwuFamily.scala',
    'modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/FamilySummaryGrid.scala',
    'modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/family/FamilySummaryGridSuite.scala',
    'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/ConditionProfileFit.scala',
    'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/CompactCondition.scala',
    'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/ProfileHrfFit.scala',
    'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/ProfileHrfTrialOutputs.scala',
    'modules/fit/shared/src/test/scala/scalafim/fmri/fit/profile/SummaryPreparationSuite.scala',
]
MUTATIONS = [
    ('condition-preflight', 'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/ConditionProfileFit.scala',
     'policy.basis.family.validateSummaryGrid', 'Right[FamilySummaryError, Unit](())'),
    ('compact-preflight', 'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/CompactCondition.scala',
     'expanded.basis.family.validateSummaryGrid', 'Right[FamilySummaryError, Unit](())'),
    ('runtime-constructor', 'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/CompactCondition.scala',
     'if emitSummaries then', 'if emitSummaries && !emitSummaries then'),
    ('raw-fixed-summary', 'modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/ProfileHrfFit.scala',
     'val fit = worker.fitRaw(', 'val fit = worker.fit('),
]

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--skip-mutations', action='store_true')
    args = parser.parse_args()
    originals = {path: (ROOT / path).read_bytes() for path in SOURCES}
    hashes = {path: digest(ROOT / path) for path in SOURCES}
    runs = []
    env = os.environ.copy()
    env['COURSIER_REPOSITORIES'] = 'https://repo.maven.apache.org/maven2'
    with tempfile.TemporaryDirectory(prefix='scalafim-summary-preparation-') as directory:
        temp = Path(directory)
        for path, source in originals.items():
            snapshot = temp / 'sources' / path
            snapshot.parent.mkdir(parents=True, exist_ok=True)
            snapshot.write_bytes(source)
        def run(label, commands, expect_failure=False):
            command = ['python3', 'tools/build/sbt-warm', *commands]
            start = time.monotonic()
            process = subprocess.run(command, cwd=ROOT, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
            log = temp / (label + '.log')
            log.write_text(process.stdout)
            counts = [dict(zip(('total', 'failed', 'errors', 'passed'), map(int, row))) for row in re.findall(r'Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', process.stdout)]
            record = dict(label=label, command=command, exit_code=process.returncode, seconds=round(time.monotonic()-start, 3), counts=counts, warnings=[line for line in process.stdout.splitlines() if '[warn]' in line], log=log.name)
            runs.append(record)
            print(json.dumps(record), flush=True)
            if expect_failure:
                assert process.returncode != 0 and counts and any(row['failed'] > 0 for row in counts), process.stdout[-16000:]
                assert not re.search(r'\[error\].*(?:-- |Compilation failed|not found)', process.stdout), process.stdout[-16000:]
            else:
                assert process.returncode == 0 and counts and all(row['failed'] == row['errors'] == 0 for row in counts), process.stdout[-16000:]
        focused = 'testOnly scalafim.fmri.fit.profile.SummaryPreparationSuite'
        run('baseline-summary', ['hrfJVM/testOnly scalafim.fmri.hrf.family.FamilySummaryGridSuite', 'hrfJS/testOnly scalafim.fmri.hrf.family.FamilySummaryGridSuite', 'fitJVM/' + focused, 'fitJS/' + focused])
        try:
            if not args.skip_mutations:
                for label, path, before, after in MUTATIONS:
                    original = originals[path].decode()
                    assert original.count(before) == 1, (label, original.count(before))
                    target = ROOT / path
                    target.write_text(original.replace(before, after))
                    (temp / (label + '.scala')).write_bytes(target.read_bytes())
                    try:
                        for platform in ('JVM', 'JS'):
                            run('mutant-' + label + '-' + platform.lower(), ['fit' + platform + '/' + focused], expect_failure=True)
                    finally:
                        target.write_bytes(originals[path])
        finally:
            for path, source in originals.items():
                (ROOT / path).write_bytes(source)
        run('restored-summary', ['fitJVM/' + focused, 'fitJS/' + focused])
        run('existing-profile-parity', ['fitJVM/testOnly scalafim.fmri.fit.profile.ProfileHrfFitSuite', 'fitJS/testOnly scalafim.fmri.fit.profile.ProfileHrfFitSuite'])
        assert hashes == {path: digest(ROOT / path) for path in SOURCES}, 'exact source restoration failed'
        archive = OUT / 'logs-and-sources.tar.gz'
        with tarfile.open(archive, 'w:gz') as tar:
            for path in sorted(temp.rglob('*')):
                if path.is_file():
                    tar.add(path, arcname=str(path.relative_to(temp)))
        receipt = dict(verified_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(), mote='bd-01M47GK9VZNQRGCERREZ09PXW1', runs=runs, source_sha256=hashes, archive=archive.name, archive_sha256=digest(archive), source_restored=True, mutation_limits='All mutation fits use admitted Gaussian grids and 60 rows / 2 voxels. Summary cap refusal is a declared probe; removing admission cannot allocate a large grid. Raw fixed summary mutant only calls a sentinel summary callback.', scope_limits=['Structural admission is point-independent; decoded summary errors are handled during fitting.', 'Unified raw fixed, compact and trial outputs request no summaries and invoke neither summary admission nor summary evaluation.', 'Reusable compact geometry preparation does not require summary admission; prepareWithSummaries opts in, and summary runtime construction independently admits before callbacks/scratch.', 'Custom sampled-summary families must declare allocation-free grid admission by overriding validateSummaryGrid.'])
        (OUT / 'receipt.json').write_text(json.dumps(receipt, indent=2)+'\n')

if __name__ == '__main__':
    main()
