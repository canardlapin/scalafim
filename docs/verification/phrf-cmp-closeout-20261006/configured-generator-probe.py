import json
import os
from pathlib import Path
import subprocess

env = dict(os.environ)
env['PHRF_GENERATOR_PYTHON'] = '/private/tmp/scalafim-phrf-closeout-python-20261006/bin/python'
env['SBT_WARM_HEAP'] = '5g'
records = []

def gate(name, commands, failure=False):
    log = Path('/private/tmp/phrf-closeout-' + name + '.log')
    with log.open('w') as stream:
        result = subprocess.run(['python3', 'tools/build/sbt-warm', *commands], env=env, stdout=stream, stderr=subprocess.STDOUT)
    records.append(dict(name=name, commands=commands, exit_code=result.returncode, expected_failure=failure))
    Path('/private/tmp/phrf-closeout-negative-check.json').write_text(json.dumps(records, indent=2) + '\n')
    print(name, 'exit', result.returncode, flush=True)
    if failure:
        assert result.returncode != 0, 'configured generator failure was accepted'
        text = log.read_text()
        assert 'fixtures are byte-identical' in text and 'Failed 1, Errors 0, Passed 9' in text, text[-3000:]
    else:
        assert result.returncode == 0, log.read_text()[-3000:]

gate('negative-configure', ['set Seq(phrfComparisonJVM / Test / fork := true, phrfComparisonJVM / Test / envVars := Map("PHRF_GENERATOR_PYTHON" -> "/usr/bin/false"))'])
try:
    gate('negative-generator', ['phrfComparisonJVM/testOnly scalafim.phrfcmp.ingest.RealGeneratorSuite'], failure=True)
finally:
    gate('negative-reset', ['set Seq(phrfComparisonJVM / Test / fork := false, phrfComparisonJVM / Test / envVars := Map.empty[String, String])'])
gate('restored-generator', ['phrfComparisonJVM/testOnly scalafim.phrfcmp.ingest.RealGeneratorSuite'])
gate('compile-all', ['scalafimCompileAll'])
