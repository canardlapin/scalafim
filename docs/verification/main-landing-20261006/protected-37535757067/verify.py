#!/usr/bin/env python3
import hashlib,json,tarfile
from pathlib import Path
p=Path(__file__).resolve().parent
r=json.loads((p/'receipt.json').read_text())
a=p/'protected-results.tar.gz'
assert hashlib.sha256(a.read_bytes()).hexdigest()==r['archive_sha256']
with tarfile.open(a,'r:gz') as t:
 assert {m.name for m in t.getmembers()}==set(r['files'])
 for n,h in r['files'].items():
  assert hashlib.sha256(t.extractfile(n).read()).hexdigest()==h,n
 assert json.loads(t.extractfile('run.json').read())['head_sha']==r['head']
 assert json.loads(t.extractfile('run.json').read())['conclusion']=='failure'
print('PASS: exact-head protected-check failure evidence')
