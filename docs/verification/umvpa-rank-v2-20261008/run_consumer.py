#!/usr/bin/env python3
"""Run bounded consumer gates with a local provider even after sbt restarts.

Usage: run_consumer.py PROVIDER_CHECKOUT LOG_LABEL SBT_COMMAND [SBT_COMMAND...]
Run provider and consumer builds sequentially: their provider targets are shared.
"""
import json
import os
import subprocess
import sys
import time
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
provider = Path(sys.argv[1]).resolve()
label = sys.argv[2]
assert provider.joinpath('build.sbt').is_file()
assert label and all(c.isalnum() or c == '-' for c in label)
assert not any(c.isspace() for c in str(provider)), 'use a provider path without whitespace'
commands = sys.argv[3:]
assert commands
environment = dict(os.environ, SBT_WARM_HEAP='3g', SBT_WARM_CPUS='4')
environment['JAVA_TOOL_OPTIONS'] = (environment.get('JAVA_TOOL_OPTIONS','') +
    ' -Dscalafim.multivar.build=' + str(provider)).strip()
command = [sys.executable, str(root/'tools/build/sbt-warm'), *commands]
start = time.monotonic()
with (packet/(label+'.log')).open('w') as log:
    result = subprocess.run(command, cwd=root, env=environment, stdout=log,
                            stderr=subprocess.STDOUT, timeout=900)
receipt = dict(command=command, provider=str(provider), exit_code=result.returncode,
               wall_seconds=time.monotonic()-start, heap='3g', processors=4,
               startup_property='JAVA_TOOL_OPTIONS: -Dscalafim.multivar.build='+str(provider))
(packet/(label+'.json')).write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(receipt))
sys.exit(result.returncode)
