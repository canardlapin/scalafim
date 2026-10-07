#!/usr/bin/env python3
"""Bound the deterministic R re-analysis; preserve an explicit process receipt."""
import json
import os
import resource
import subprocess
import sys
import time
from pathlib import Path

packet = Path(__file__).resolve().parent
root = packet.parents[2]
command = ['/usr/local/bin/Rscript', 'tools/mvpa-inference/diagnose_rank.R',
           str(packet / 'case-index.tsv'), str(packet / 'oracle-power-bound.tsv'), str(packet)]
started = time.time()
with (packet / 'diagnose.log').open('xb') as log:
    try:
        result = subprocess.run(command, cwd=root, stdout=log, stderr=subprocess.STDOUT,
                                timeout=600, env={**os.environ, 'LC_ALL': 'C', 'LANG': 'C'})
        status = result.returncode
    except subprocess.TimeoutExpired:
        status = 124
usage = resource.getrusage(resource.RUSAGE_CHILDREN)
receipt = dict(command=command, exit_code=status, wall_seconds=time.time()-started,
               wall_limit_seconds=600, child_max_rss=usage.ru_maxrss,
               child_max_rss_unit='bytes' if sys.platform=='darwin' else 'KiB',
               new_datasets=0, new_random_draws=0)
(packet / 'diagnose-process.json').write_text(json.dumps(receipt, indent=2)+'\n')
print(json.dumps(receipt, indent=2))
raise SystemExit(status)
