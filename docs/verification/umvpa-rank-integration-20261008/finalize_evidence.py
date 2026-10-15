#!/usr/bin/env python3
"""Bind publication, public-pin gates and retained-input replay evidence."""
import hashlib
import json
import subprocess
from pathlib import Path

packet=Path(__file__).resolve().parent
root=packet.parents[2]
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
read=lambda p:json.loads((packet/p).read_text())
ci=read('upstream-ci.json');pr=read('upstream-pr.json')
assert ci['status']=='completed' and ci['conclusion']=='success'
assert pr['headRefOid']=='edb05de01401ec0b3aea4dc1190dd3100e70ee51' and pr['isDraft'] and pr['state']=='OPEN'
gate=read('public-pin-verification.json')
assert gate['build_sbt_sha256']==sha(root/'build.sbt') and not gate['local_provider_override']
assert read('public-pin-mvpa.json')['exit_code']==0
old_packet=packet.parent/'umvpa-rank-v2-20261008'
prior=json.loads((old_packet/'execution.json').read_text())
assert sha(old_packet/'execution.json')=='2cb4cba316347abb4706d65ff229b8a538c2c6d3132a759b617ba85bc37978e8'
for p,h in prior['artifact_locks'].items():assert sha(root/p)==h,p
for p,h in prior['source_locks'].items():
    data=subprocess.check_output(['git','show',prior['consumer_source_and_probe_freeze']+':'+p],cwd=root)
    assert hashlib.sha256(data).hexdigest()==h,p
historical=read('historical/historical-manifest.json')
replay=read('historical/probe/process-receipt.json')
assert replay['worker_exit']==0 and replay['resource_refusal'] is None and replay['evaluated']==8
assert replay['all_assignments_completed'] and replay['source_locks_unchanged'] and replay['new_datasets']==0
for p,h in historical['source_locks'].items():
    data=subprocess.check_output(['git','show',replay['source_commit']+':'+p],cwd=root)
    assert hashlib.sha256(data).hexdigest()==h,p
old_inputs=json.loads((old_packet/'probe/input-sha256.json').read_text())
assert historical['inputs']==old_inputs
paths=subprocess.check_output(['git','ls-files','modules/mvpa','modules/scenario-testkit',
 'build.sbt','.jvmopts','project/build.properties','project/plugins.sbt','tools/build/sbt-warm'],cwd=root,text=True).splitlines()
paths += [str(p.relative_to(root)) for p in packet.glob('*.py')]
receipt=dict(status='published-provider-integrated-historical-resource-replay-complete',
 consumer_parent_before_pin_update=subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),
 provider_revision=pr['headRefOid'],upstream_pr=pr['url'],upstream_ci=ci['url'],upstream_ci_conclusion=ci['conclusion'],
 provider_pr_is_draft=True,upstream_main_merged=False,default_public_pin_verified=True,
 public_pin_gates=read('public-pin-mvpa.json'),public_pin_verification=gate,
 historical_source_commit=replay['source_commit'],historical_results_commit='6dd0d0310b28e5cafdd024758e940ce9b2c771ee',
 historical_gate_verification=read('historical/historical-gate-verification.json'),historical_replay=replay,
 combined_resource_summary=read('resource-summary.json'),
 preserved_prior_receipt_sha256=sha(old_packet/'execution.json'),preserved_prior_artifacts=len(prior['artifact_locks']),
 prior_sources_verified_at=prior['consumer_source_and_probe_freeze'],historical_source_locks_verified=len(historical['source_locks']),
 new_datasets=0,pilot_or_confirmation_consumed=False,rank_admission='PendingFrozenProtocol',
 campaign_status='four-hour resource revision proposed; source and stream freeze still required',
 source_locks={p:sha(root/p) for p in sorted(set(paths))},
 artifact_locks={str(p.relative_to(root)):sha(p) for p in sorted(packet.rglob('*')) if p.is_file() and p.name!='execution.json'})
with (packet/'execution.json').open('x') as out:out.write(json.dumps(receipt,indent=2)+'\n')
print(json.dumps(dict(status=receipt['status'],source_locks=len(receipt['source_locks']),
 artifact_locks=len(receipt['artifact_locks']),receipt_sha256=sha(packet/'execution.json'))))
