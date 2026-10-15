#!/usr/bin/env python3
"""Temporarily expose the review fixture without retaining a shipping test."""
import json
import os
import subprocess
import time
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
source = (packet/'RankMethodReviewSuite.scala').read_bytes()
temporary = root/'modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/inference/RankMethodReviewSuite.scala'
assert not temporary.exists(), 'refuse to replace an existing test source'
command = ['python3','tools/build/sbt-warm','mvpaJVM/test','mvpaJS/test']
started = time.time()
temporary.write_bytes(source)
try:
    with (packet/'mvpa-review-tests.log').open('xb') as log:
        try:
            result = subprocess.run(command,cwd=root,stdout=log,stderr=subprocess.STDOUT,timeout=600,
                env={**os.environ,'SBT_WARM_HEAP':'3g','SBT_WARM_CPUS':'4',
                     'SCALAFIM_RANK_MATH_REVIEW_DIRECTORY':str(packet)})
            status = result.returncode
        except subprocess.TimeoutExpired:
            status = 124
finally:
    assert temporary.read_bytes() == source, 'review source changed concurrently; preserve it'
    temporary.unlink()
receipt = dict(command=command,exit_code=status,wall_seconds=time.time()-started,
               wall_limit_seconds=600,heap='3g',processors=4,
               temporary_source=str(temporary.relative_to(root)),temporary_source_removed=True)
(packet/'test-process.json').write_text(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(receipt,indent=2))
raise SystemExit(status)
