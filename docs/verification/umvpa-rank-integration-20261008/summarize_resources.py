#!/usr/bin/env python3
"""Combine paired fixture costs, without interpreting their statistical outcomes."""
import json
import statistics
from pathlib import Path

packet=Path(__file__).resolve().parent
prior=packet.parent/'umvpa-rank-v2-20261008'
manifest=json.loads((prior/'probe-manifest.json').read_text())
rows=[json.loads(line) for path in [prior/'probe/records.jsonl',packet/'historical/probe/records.jsonl']
      for line in path.read_text().splitlines()]
assert len(rows)==24 and all(r['status']=='evaluated' for r in rows)
methods=sorted({r['rank_method'] for r in rows})
assert len(methods)==3
groups=[];totals={}
for n in (80,640):
    cells=[c for c in manifest['cells'] if c['n']==n]
    totals[n]=[]
    for cell in cells:
        paired=[r for r in rows if r['scenario_id']==cell['id']]
        assert len(paired)==3 and {r['rank_method'] for r in paired}==set(methods)
        assert all(r['root_seed64']==cell['root_seed64'] and r['completed_draws']==199 for r in paired)
        totals[n].append(sum(r['elapsed_seconds'] for r in paired))
    for method in methods:
        values=[r['elapsed_seconds'] for r in rows if r['rank_method']==method and r['scenario_id'] in {c['id'] for c in cells}]
        groups.append(dict(n=n,method=method,fixtures=len(values),minimum_seconds=min(values),
            median_seconds=statistics.median(values),mean_seconds=statistics.mean(values),maximum_seconds=max(values)))
projection=lambda f:5600*f(totals[80])+7200*f(totals[640])
summary=dict(status='three-method-resource-fixtures-complete-no-rate-qualification',
    datasets=8,evaluations=24,failed_evaluations=0,new_datasets_this_continuation=0,
    groups=groups,paired_seconds_by_n=totals,
    planning_sensitivity=dict(proposed_datasets=12800,proposed_method_evaluations=38400,
        assumption='Measured n80 costs cover 5600 datasets; measured n640 costs provisionally stand in for 7200 n160/n320/n640 datasets.',
        using_observed_means_seconds=projection(statistics.mean),using_observed_maxima_seconds=projection(max),
        omitted='Input generation, parsing/serialization outside the timed adapter, worker startups and longer-run GC.',
        one_hour_campaign_supported=False,runtime_bound=False),
    limitations=['Four fixtures per n; fixed ordering and JIT/GC/system variation preclude timing confidence intervals or speedup claims.',
        'Historical and current providers retain their own published dependency versions; these are complete-pipeline resource measurements.',
        'All matrices are identical across methods; numeric outcomes are retained for audit but support no size/power inference.',
        'Sampled RSS excludes early startup and clients; hard runtime monitors remain required.'])
with (packet/'resource-summary.json').open('x') as out:out.write(json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
