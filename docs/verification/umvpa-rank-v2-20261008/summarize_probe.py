#!/usr/bin/env python3
"""Summarize resource measurements; never select a method from fixture outcomes."""
import json
import statistics
from pathlib import Path

packet = Path(__file__).resolve().parent
manifest = json.loads((packet/'probe-manifest.json').read_text())
records = [json.loads(line) for line in (packet/'probe/records.jsonl').read_text().splitlines()]
cases = {c['id']:c for c in manifest['cells']}
assert len(records)==16 and all(r['status']=='evaluated' for r in records)
groups = []
paired = {}
for n in (80,640):
    paired[n] = [sum(r['elapsed_seconds'] for r in records if r['scenario_id']==c['id'])
                 for c in manifest['cells'] if c['n']==n]
    for method in manifest['method_identities']:
        rows = [r for r in records if cases[r['scenario_id']]['n']==n and r['rank_method']==method]
        times = [r['elapsed_seconds'] for r in rows]
        assert len(rows)==4
        groups.append(dict(n=n,method=method,fixtures=4,mean_seconds=statistics.mean(times),
            median_seconds=statistics.median(times),minimum_seconds=min(times),maximum_seconds=max(times),
            maximum_planned_owned_cells=max(r['planned_owned_cells'] for r in rows)))
# A deliberately explicit planning sensitivity, not a runtime bound: use the
# measured n640 pair for all unmeasured n160/n320 datasets. No extrapolation of
# p-values, error rates, power or the historical method is permitted here.
estimate = lambda aggregate: 5600*aggregate(paired[80]) + 7200*aggregate(paired[640])
summary = dict(status='resource-fixtures-complete-no-rate-qualification',
    records=16,unique_datasets=8,failed_evaluations=0,groups=groups,
    pair_seconds_by_n={str(n):times for n,times in paired.items()},
    planning_sensitivity=dict(proposed_unique_datasets=12800,
        method_scope='Only the two new methods; excludes historical comparator, generation, startup, serialization and longer-run GC.',
        assumption='Use n80 measurements for 5600 datasets and n640 measurements for all 7200 n160/n320/n640 datasets.',
        using_observed_means_seconds=estimate(statistics.mean),using_observed_maxima_seconds=estimate(max),
        resource_admission=False),
    limitations=[
        'One fixture per shape, fixed method order and startup/JIT effects; no timing confidence interval or performance guarantee.',
        'RSS samples begin after the resident sbt server exposes its socket; early startup memory is not observed.',
        'The 4 GiB check covers the sampled resident sbt JVM, excluding the thin client and generator.',
        'Historical-method timing and the complete aggregate budget remain necessary before the paired campaign.',
        'No pilot/confirmation streams consumed and no error-rate or power conclusion follows from these fixtures.'
    ])
with (packet/'probe-summary.json').open('x') as out:
    out.write(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
