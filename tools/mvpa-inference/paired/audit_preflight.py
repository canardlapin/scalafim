#!/usr/bin/env python3
"""Audit retained preflight records without evaluating or generating a dataset."""
import argparse
import json
from pathlib import Path
from protocol import sha, validate_rows, METHODS


def audit(path):
    root=Path(__file__).resolve().parents[3]
    prior=root/'docs/verification/umvpa-rank-v2-20261008'
    cases=json.loads((prior/'probe-manifest.json').read_text())['cells']
    identities=json.loads((root/'docs/verification/umvpa-rank-integration-20261008/historical/retained-input-sha256.json').read_text())
    mismatches=[]; decisions=[]; max_gap=0.; processes=[]; count=0
    for provider in ('historical','current'):
        source=json.loads((path/(provider+'-source.json')).read_text())
        lock=path/(provider+'-source-lock.'+('tsv' if provider=='historical' else 'json'))
        by_platform={}
        for platform in ('JVM','JS'):
            rows=[json.loads(line) for line in (path/(provider+'-'+platform+'.jsonl')).read_text().splitlines()]
            count+=len(rows)
            for c in cases:
                failures=validate_rows([r for r in rows if r['scenario_id']==c['id']],c,provider,source['commit'],sha(lock),
                    [dict(sha256=identities[c['id']+'.tsv'],root_seed64=c['root_seed64'])],phase='fixture')
                if failures:raise ValueError('evaluation refusal retained')
            by_platform[platform]={(r['scenario_id'],r['rank_method']):r for r in rows}
            receipt=json.loads((path/(provider+'-'+platform+'-process.json')).read_text())
            processes.append(dict(provider=provider,platform=platform,exit_code=receipt['worker_exit'],
                resource_refusal=receipt['resource_refusal'],elapsed_seconds=receipt['elapsed_seconds'],peak_rss_bytes=receipt['peak_rss_bytes']))
        if by_platform['JVM'].keys()!=by_platform['JS'].keys():raise ValueError('platform inventory differs')
        for key,jvm in by_platform['JVM'].items():
            js=by_platform['JS'][key]
            if jvm['receipt_seed64']!=js['receipt_seed64']:raise ValueError('platform seed receipts differ')
            for field in ('observed_roots','observed_statistics'):
                max_gap=max(max_gap,max(abs(a-b)/(1+abs(a)) for a,b in zip(jvm[field],js[field])))
            if jvm['raw_exceedances']!=js['raw_exceedances']:
                mismatches.append(dict(provider=provider,scenario_id=key[0],rank_method=key[1],
                    jvm_counts=jvm['raw_exceedances'],js_counts=js['raw_exceedances'],
                    jvm_raw_p=jvm['raw_p_values'],js_raw_p=js['raw_p_values'],
                    maximum_absolute_p_difference=max(abs(a-b) for a,b in zip(jvm['raw_p_values'],js['raw_p_values']))))
            if jvm['raw_reject']!=js['raw_reject'] or jvm['closed_reject']!=js['closed_reject']:
                decisions.append(dict(provider=provider,scenario_id=key[0],rank_method=key[1]))
    assert count==48
    return dict(schema='scalafim-rank-paired-preflight-audit/v1',preflight_pass=False,launch_authority=False,
        records=48,unique_retained_inputs=8,new_datasets=0,pilot_streams_opened=0,
        reason='JVM_JS_MONTE_CARLO_COUNTS_DIFFER' if mismatches else 'none',
        same_actual_seed_receipts=True,max_scaled_observed_numeric_difference=max_gap,
        platform_count_mismatches=mismatches,platform_rejection_mismatches=decisions,processes=processes,
        source_commits={p:json.loads((path/(p+'-source.json')).read_text())['commit'] for p in ('historical','current')},
        files={p.name:sha(p) for p in sorted(path.glob('*')) if p.is_file()})

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('directory',type=Path);p.add_argument('output',type=Path)
    args=p.parse_args()
    with args.output.open('x') as f:json.dump(audit(args.directory),f,sort_keys=True,indent=2);f.write('\n')
