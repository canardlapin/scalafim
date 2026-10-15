#!/usr/bin/env python3
import hashlib,json,tarfile
from pathlib import Path
p=Path(__file__).resolve().parent
r=json.loads((p/'receipt.json').read_text())
a=p/'protected-results.tar.gz'
assert hashlib.sha256(a.read_bytes()).hexdigest()==r['archive_sha256']
assert r['conclusion']=='success' and len(r['checks'])==3 and set(r['checks'].values())=={'success'}
with tarfile.open(a,'r:gz') as t:
 assert {m.name for m in t.getmembers()}==set(r['files'])
 for n,h in r['files'].items():
  assert hashlib.sha256(t.extractfile(n).read()).hexdigest()==h,n
 run=json.loads(t.extractfile('run.json').read())
 assert run['head_sha']==r['head'] and run['conclusion']=='success'
 assert json.loads(t.extractfile('pr21.json').read())['head']['sha']==r['head']
 inv=json.loads(t.extractfile('inventory.json').read())
 assert len(inv['supported_test_targets'])==95 and len(inv['display_exclusions'])==2 and len(inv['extra_example_tests'])==4
 assert len(inv['batches'])==16 and sum(map(len,inv['batches'].values()))==97
 assert set(inv['batches'])==set(r['full']['batches'])
 assert all(v['failed']==v['errors']==0 for v in r['full']['batches'].values())
print('PASS: exact-head all-green protected-check evidence')
