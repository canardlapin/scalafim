#!/usr/bin/env python3
"""Supplement verify.py with existing public output, parallel, and condition fixtures."""
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

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def main():
    receipt_path = OUT / 'receipt.json'
    receipt = json.loads(receipt_path.read_text())
    expected = receipt['source_sha256']
    assert expected == {path: digest(ROOT / path) for path in expected}
    archive = OUT / receipt['archive']
    assert digest(archive) == receipt['archive_sha256']
    env = os.environ.copy()
    env['COURSIER_REPOSITORIES'] = 'https://repo.maven.apache.org/maven2'
    commands = [
        ('public-output-parallel', [
            'fitJVM/testOnly scalafim.fmri.fit.profile.ProfileHrfTrialOutputsSuite scalafim.fmri.fit.profile.ProfileHrfFitParallelSuite',
            'fitJS/testOnly scalafim.fmri.fit.profile.ProfileHrfTrialOutputsSuite',
        ]),
        ('condition-compact-fixtures', [
            'firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.ConditionProfileFitSuite scalafim.fmri.laws.profile.CompactConditionRuntimeSuite',
            'firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.ConditionProfileFitSuite scalafim.fmri.laws.profile.CompactConditionRuntimeSuite',
        ]),
    ]
    with tempfile.TemporaryDirectory(prefix='scalafim-summary-consumers-') as directory:
        temp = Path(directory)
        with tarfile.open(archive) as tar:
            tar.extractall(temp, filter='data')
        for label, targets in commands:
            command = ['python3', 'tools/build/sbt-warm', *targets]
            start = time.monotonic()
            result = subprocess.run(command, cwd=ROOT, env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            (temp / (label + '.log')).write_text(result.stdout)
            counts = [dict(zip(('total', 'failed', 'errors', 'passed'), map(int, row))) for row in re.findall(r'Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)', result.stdout)]
            warnings = [line for line in result.stdout.splitlines() if '[warn]' in line]
            compiler_warnings = [line for line in warnings if 'multiple main classes detected' not in line]
            record = dict(label=label, command=command, exit_code=result.returncode, seconds=round(time.monotonic()-start, 3), counts=counts, warnings=warnings, compiler_warnings=compiler_warnings, log=label+'.log')
            receipt['runs'].append(record)
            print(json.dumps(record), flush=True)
            assert result.returncode == 0 and len(counts) == 2 and all(row['failed'] == row['errors'] == 0 and row['passed'] > 0 for row in counts), result.stdout[-20000:]
            assert not compiler_warnings, compiler_warnings
        assert expected == {path: digest(ROOT / path) for path in expected}
        for name in ('verify.py', 'consumer-gates.py'):
            (temp / name).write_bytes((OUT / name).read_bytes())
        with tarfile.open(archive, 'w:gz') as tar:
            for path in sorted(temp.rglob('*')):
                if path.is_file():
                    tar.add(path, arcname=str(path.relative_to(temp)))
        receipt['archive_sha256'] = digest(archive)
        receipt['runner_sha256'] = {name: digest(OUT/name) for name in ('verify.py', 'consumer-gates.py')}
        receipt_path.write_text(json.dumps(receipt, indent=2)+'\n')

if __name__ == '__main__':
    main()
