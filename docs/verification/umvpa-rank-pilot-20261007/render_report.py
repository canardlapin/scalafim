#!/usr/bin/env python3
import csv
import gzip
import json
from pathlib import Path

packet=Path(__file__).resolve().parent
overview=json.loads((packet/'pilot-overview.json').read_text())
plan=json.loads((packet/'rank-confirmation-resource-plan.json').read_text())
with (packet/'metric-intervals.tsv').open() as f:
    metrics=list(csv.DictReader(f,delimiter='\t'))
primary_null=[m for m in metrics if m['role']=='primary' and m['metric'] in ('type-i','fwer')]
power=[m for m in metrics if m['metric']=='standard-power']
below=[m for m in primary_null if float(m['cp90_upper'])<.035]
above=[m for m in primary_null if float(m['cp90_lower'])>.065]
low_power=[m for m in power if float(m['cp90_upper'])<.8]
warning=dict(scope='Pilot diagnostics against frozen targets; no confirmatory admission decision',
    primary_null_cells=len(primary_null),primary_null_cp90_below_band=below,primary_null_cp90_above_band=above,
    standard_power_cells=len(power),standard_power_cp95_upper_below_target=low_power,
    thresholds_changed=False,populations_changed=False)
(packet/'pilot-warnings.json').write_text(json.dumps(warning,indent=2)+'\n')
sizing=json.loads((packet/'full-b-sizing/process-receipt.json').read_text())
full_rows=[json.loads(line) for line in gzip.decompress((packet/'full-b-sizing/records.jsonl.gz').read_bytes()).splitlines()]
assert len(full_rows)==8 and all(r['completed_draws']==1999 and r['completed_compact_fits']==4*1999 for r in full_rows)
plan['full_b_sizing']=dict(datasets=8,draws_per_dataset=1999,nonidentity_draws=8*1999,
    observed_peak_server_rss_bytes=sizing['observed_peak_rss_bytes'],
    total_worker_wall_seconds=sizing['elapsed_seconds'],
    one_fixture_per_shape_scaled_hours=sum(r['elapsed_seconds']*60000 for r in full_rows)/3600,
    scope='One independently seeded R3 fixture per shape. Multiplication by60000 datasets/shape is illustrative, with no uncertainty bound or warmup correction.',
    per_shape=[dict(scenario_id=r['scenario_id'],elapsed_seconds=r['elapsed_seconds'],planned_owned_cells=r['planned_owned_cells']) for r in full_rows])
plan['limitations']=[s.replace('B1999 runtime and RSS are unmeasured across the expanded grid; linear scaling is not an upper bound.',
    'B1999 has one sizing observation per shape, not a replicate distribution across all truth cells; neither projection is an upper bound.') for s in plan['limitations']]
(packet/'rank-confirmation-resource-plan.json').write_text(json.dumps(plan,indent=2)+'\n')
fmt=lambda x: f'{100*float(x):.1f}%'
lines=['# Rank metric, oracle, and expanded-pilot evidence','',
 'The accepted raw/closed metric split and independent rank oracles are complete. All 61 fresh pilot cells completed, but their statistical diagnostics remain unfavorable: higher-rank null tests are conservative and standard-root power is low. No confirmation stream was consumed and no inferential claim was admitted.','',
 '## What was frozen','',
 '- Source and metric clarification: `38f98dab`.',
 '- Expanded pilot manifest: `e5ba1827`, committed before generation/execution, with 175 source locks.',
 '- Eight full-B sizing fixtures: `89e1a46a`, committed before generation/execution.',
 '- [Metric clarification](../../plans/unified-mvpa-rank-metric-bindings-v1.md): raw H2/H3/H4 pointwise calibration, closed R0 FWER, and closed detectable-rank/power decisions. Only the new .20 root has the standard-power target.',
 '- The original three n80/p6/q4/intercept null pilots remain unchanged (closed counts 8/200, 6/200, 2/200). Their missing raw values are not invented or regenerated.','',
 '## Statistical findings','',
 f'The expansion retains **{overview["datasets"]:,} datasets**, all evaluated, with zero failures or missing records. Each used B199: {overview["nonidentity_draws"]:,} nonidentity draws and {overview["compact_fits"]:,} compact fits. Every record retains raw counts, raw/closed probabilities, both decisions, seeds and population truth.',
 '',f'Of {len(primary_null)} fresh primary null cells, **{len(below)}** have a descriptive CP90 interval wholly below the frozen lower calibration target .035; **{len(above)}** lie wholly above .065. Raw-stage recording therefore does not by itself resolve the earlier conservatism warning.',
 '',f'All **{len(low_power)}/{len(power)}** standard-power cells have a one-sided CP95 upper bound below .80. Their observed closed power ranges from {fmt(min(float(m["rate"]) for m in power))} to {fmt(max(float(m["rate"]) for m in power))}.',
 '', '| Standard-root cell | Closed detections | Descriptive CP90 |', '| --- | ---: | --- |']
for m in power:
    lines.append(f'| {m["cell"]} | {m["successes"]}/200 | {fmt(m["cp90_lower"])}–{fmt(m["cp90_upper"])} |')
lines += ['', 'These are pilot diagnostics, not replacements for the 10,000-null/5,000-alternative confirmation criteria. All cells remain in the receipt; neither populations nor thresholds were changed after observing the results. Full primary and secondary counts/intervals are in [metric-intervals.tsv](metric-intervals.tsv), with machine-readable warnings in [pilot-warnings.json](pilot-warnings.json).',
 '', '## Independent and engineering checks','',
 '- Base R QR/SVD agrees with production roots and every tail statistic for unequal dimensions in both directions and all 720 actions of a complete finite group. Exact exceedances, plus-one probabilities, closure and detectable rank agree; nuisance projectors and the production nuisance-adjusted binding agree at combined tolerance 1e-10.',
 '- Six new oracle tests pass on JVM and Scala.js. Full MVPA gates: **464 passed plus one opt-in skip on each platform**. Python controls: **16 passed**.',
 '- All exact pilot input bytes, headers, SHA-derived seeds, named resampling seeds and complete truth vectors are checked. Inputs are retained in per-cell archives.',
 '- The first oracle build exceeded the JVM method-size limit; compact fixture serialization fixed it without changing the numeric expectations. That failure log is retained. A GC warning during Scala.js linking under the 2 GiB heap is also retained; the full owning gates passed.',
 '', '## Resource evidence','',
 f'The pilot campaign took {overview["campaign_elapsed_seconds"]/60:.2f} minutes including input generation, archives, builds and serial coordination. Recorded dataset evaluations sum to {overview["sum_dataset_evaluation_seconds"]:.2f} seconds. Maximum sampled resident sbt JVM RSS was {overview["maximum_observed_server_rss_bytes"]/1024**3:.3f} GiB.',
 '', '| n | p/q | Nuisance | Pilot datasets | Mean evaluation (s) | Sampled JVM RSS (GiB) |', '| ---: | --- | --- | ---: | ---: | ---: |']
for g in overview['groups']:
    lines.append(f'| {g["n"]} | {g["p"]}/{g["q"]} | {g["nuisance"]} | {g["datasets"]} | {g["mean_dataset_seconds"]:.5f} | {g["observed_peak_server_rss_bytes"]/1024**3:.3f} |')
lines += ['', f'The eight fresh B1999 fixtures completed {8*1999:,} draws and {8*1999*4:,} compact fits. Their sampled JVM peak was {sizing["observed_peak_rss_bytes"]/1024**3:.3f} GiB. These are one-observation sizing probes per shape, not a rate study.',
 '', f'The rank-only confirmation inventory remains **480,000 datasets / 959,520,000 nonidentity draws**. Scaling pilot mean evaluation costs by B gives approximately **{plan["mean_scaled_hours"]:.2f} hours**; the p95-cost illustration is **{plan["p95_scaled_hours"]:.2f} hours**. Multiplying each single full-B fixture by its 60,000-dataset shape inventory gives **{plan["full_b_sizing"]["one_fixture_per_shape_scaled_hours"]:.2f} hours**. These estimates are planning evidence, not an upper bound or resource admission. See [the full resource plan](rank-confirmation-resource-plan.json).',
 '', 'RSS covers the resident sbt JVM after its socket identifies the PID; cold-start memory before that point, thin clients and R generators are outside the measurement. Build state is included. The observed Mac14,12 / macOS15 / Node24 host differs from the frozen reference benchmark profile. No standalone J-RANK/JS-RANK performance gate is claimed.',
 '', '## Next admission work','',
 'Review conservative partial-null calibration and low standard-root power before spending the full confirmation budget. Any changed scientific method or population needs its own predeclared evidence; this packet changes neither. M4.07/M4.09 stay open, and M4.10 reference performance and the broader voxel/component/group gates remain pending. The [campaign readiness ledger](campaign-readiness.md) identifies the remaining definitions and dependencies.',
 '', 'The final `execution.json` locks source and artifact bytes. `pilot-manifest.json` and `full-b-sizing-manifest.json` remain the pre-execution authorities; `results/` and `full-b-sizing/` retain every generated input and observed output.','']
(packet/'README.md').write_text('\n'.join(lines))
print(json.dumps(dict(below_null_band=len(below),above_null_band=len(above),low_power=len(low_power),standard_power_cells=len(power))))
