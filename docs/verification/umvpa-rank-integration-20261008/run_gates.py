#!/usr/bin/env python3
"""Bounded validation of the public Git pin, with no local provider override."""
import json
import os
import subprocess
import sys
import time
from pathlib import Path

packet=Path(__file__).resolve().parent
root=packet.parents[2]
label=sys.argv[1]
assert label and all(c.isalnum() or c=='-' for c in label)
commands=sys.argv[2:]
assert commands
env=dict(os.environ,SBT_WARM_HEAP='3g',SBT_WARM_CPUS='4')
removed=[]
for key in ['JAVA_TOOL_OPTIONS','JDK_JAVA_OPTIONS','SBT_OPTS','JAVA_OPTS']:
    if key in env:
        removed.append(key)
        del env[key]
command=['python3','tools/build/sbt-warm',*commands]
start=time.monotonic()
with (packet/(label+'.log')).open('x') as out:
    stopped=subprocess.run(['python3','tools/build/sbt-warm','--shutdown'],cwd=root,env=env,
        stdout=out,stderr=subprocess.STDOUT,timeout=65)
    assert stopped.returncode==0,'cannot reuse a server with an unknown provider override'
    result=subprocess.run(command,cwd=root,env=env,stdout=out,stderr=subprocess.STDOUT,timeout=900)
receipt=dict(command=command,exit_code=result.returncode,wall_seconds=time.monotonic()-start,
    heap='3g',processors=4,cleared_environment_variable_names=removed,
    local_provider_override=False,public_provider_revision='edb05de01401ec0b3aea4dc1190dd3100e70ee51')
(packet/(label+'.json')).write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(receipt))
sys.exit(result.returncode)
