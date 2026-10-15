#!/usr/bin/env python3
"""Derive descriptive counts and capacity estimates from the frozen pilot receipts."""
import csv
import gzip
import hashlib
import json
from pathlib import Path
import statistics
import sys

root = Path(__file__).resolve().parents[3]
sys.path.insert(0,str(root/'tools/mvpa-inference'))
from calibration_protocol import proposal, seed_record, summarize, validate_manifest
packet = Path(__file__).resolve().parent
manifest = json.loads((packet/'pilot-manifest.json').read_text())
validate_manifest(manifest,root,'pilot')
write = lambda path,value: path.write_text(json.dumps(value,indent=2)+'\n')
all_rows = []
counts = []
by_cell = {}
processes = []
for cell in manifest['cells']:
    directory = packet/'results'/cell['id']
    process = json.loads((directory/'process-receipt.json').read_text())
    rows = [json.loads(line) for line in gzip.decompress((directory/'records.jsonl.gz').read_bytes()).splitlines()]
    summary = summarize(cell,'pilot',rows)
    assert summary == json.loads((directory/'summary.json').read_text())
    assert process['status']=='completed' and len(rows)==200
    assert all(r['completed_compact_fits']==4*199 for r in rows)
    rho=cell['parameters']['correlations']
    truth=[all(v==0 for v in rho[k:]) for k in range(len(rho))]
    assert all(r['declared_population_null']==truth and r['actual_conditional_null']==truth for r in rows)
    assert all(r['resampling_seed64']==seed_record('pilot',cell['id'],r['dataset_index'])['child_seeds64']['resampling'] for r in rows)
    assert not summary['missing_indices'] and not summary['estimand_contradiction_indices']
    all_rows.extend(rows)
    by_cell[cell['id']] = rows
    processes.append(process)
    for metric in summary['metric_counts']:
        counts.append(dict(cell=cell['id'],member=','.join(metric['members']),binding=metric['id'],
            metric=metric['metric'],p_value_scope=metric['p_value_scope'],role=metric['role'],
            successes=metric['successes'],datasets=200,expected_datasets=200,failures=0,missing=0,
            estimand_contradictions=0))
with (packet/'metric-counts.tsv').open('w') as output:
    writer=csv.DictWriter(output,fieldnames=list(counts[0]),delimiter='\t',lineterminator='\n')
    writer.writeheader(); writer.writerows(counts)

shape = lambda c: tuple(c['parameters'][k] for k in ('n','p','q','nuisance'))
quantile = lambda values,p: sorted(values)[min(len(values)-1,int(p*len(values)))]
groups = []
for key in sorted({shape(c) for c in manifest['cells']}):
    selected=[c for c in manifest['cells'] if shape(c)==key]
    rows=[r for c in selected for r in by_cell[c['id']]]
    timings=[r['elapsed_seconds'] for r in rows]
    receipts=[p for p in processes if p['cell'] in {c['id'] for c in selected}]
    groups.append(dict(n=key[0],p=key[1],q=key[2],nuisance=key[3],cells=len(selected),datasets=len(rows),
        mean_dataset_seconds=statistics.mean(timings),median_dataset_seconds=statistics.median(timings),
        p95_dataset_seconds=quantile(timings,.95),max_dataset_seconds=max(timings),
        max_planned_owned_cells=max(r['planned_owned_cells'] for r in rows),
        observed_peak_server_rss_bytes=max(p['observed_peak_rss_bytes'] for p in receipts)))

future=[c for c in proposal()['cells'] if c['procedure']=='rank' and c['availability']=='candidate']
projections=[]
for cell in future:
    if cell['id'] in by_cell:
        donors=[cell['id']]
    else:
        donors=[c['id'] for c in manifest['cells'] if shape(c)==shape(cell)]
    # Historical cells have no new schema2 timing sample: conservative same-shape donor.
    mean=max(statistics.mean(r['elapsed_seconds'] for r in by_cell[name]) for name in donors)
    p95=max(quantile([r['elapsed_seconds'] for r in by_cell[name]],.95) for name in donors)
    datasets=10000 if cell['rate_class']=='null' else 5000
    scale=1999/199
    projections.append(dict(cell=cell['id'],datasets=datasets,draws=1999,
        timing_donors=donors,method='cell mean' if len(donors)==1 else 'maximum same-shape donor mean',
        pilot_mean_dataset_seconds=mean,mean_scaled_seconds=datasets*mean*scale,
        p95_scaled_seconds=datasets*p95*scale))
plan=dict(scope='Independent Gaussian rank grid only; planning estimates, not admission or a runtime bound',
    confirmation_execution_enabled=False,inventory_complete=False,
    cells=64,datasets=sum(p['datasets'] for p in projections),
    nonidentity_draws=sum(p['datasets']*1999 for p in projections),
    compact_fits=sum(p['datasets']*1999*4 for p in projections),
    mean_scaled_hours=sum(p['mean_scaled_seconds'] for p in projections)/3600,
    p95_scaled_hours=sum(p['p95_scaled_seconds'] for p in projections)/3600,
    scaling='Whole measured dataset evaluation at B199 multiplied by1999/199. Includes fixed preparation in scaled cost; excludes build, input generation, receipts and bootstrap/adjudication.',
    limitations=['Observed shared Mac14,12 host differs from frozen M3 Max benchmark hardware.',
                 'B1999 runtime and RSS are unmeasured across the expanded grid; linear scaling is not an upper bound.',
                 'Resident sbt RSS includes build state but excludes generator/thin client; it is not standalone application RSS.',
                 'Cold-start RSS before the server socket identifies its PID is unobserved; sampled peaks are not whole-lifetime maxima.',
                 'B1999 retains ten times the null draws and permutation keys; B199 RSS cannot certify that footprint.',
                 'Complete voxel/component/refusal and group campaign definitions and resources remain separate prerequisites.',
                 'A strong-versus-weak same-rank power contrast is not defined by R1 versus R4.'],
    admission='quarantined: complete inventory, full-B sizing and source/runtime/resource approval still required',
    projections=projections)
write(packet/'rank-confirmation-resource-plan.json',plan)
campaign=json.loads((packet/'results/campaign-receipt.json').read_text())
write(packet/'pilot-overview.json',dict(cells=len(by_cell),datasets=len(all_rows),failures=0,missing=0,
    nonidentity_draws=len(all_rows)*199,compact_fits=len(all_rows)*4*199,
    historical_pilots_rerun=False,confirmation_consumed=False,scientific_release='unavailable-pilot-only',
    campaign_elapsed_seconds=campaign['elapsed_seconds'],
    sum_dataset_evaluation_seconds=sum(r['elapsed_seconds'] for r in all_rows),
    maximum_observed_server_rss_bytes=max(p['observed_peak_rss_bytes'] for p in processes),groups=groups))
print(json.dumps({'cells':len(by_cell),'datasets':len(all_rows),'confirmation_mean_hours':plan['mean_scaled_hours']}))
