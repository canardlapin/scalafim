"""Verify the original PHRF closeout survives in a prospective publication ref."""
from pathlib import Path
import hashlib
import json
import subprocess
import sys

root=Path(__file__).resolve().parents[3]
manifest=json.loads(Path(__file__).with_name('phrf-closeout-preservation.json').read_text())
ref=sys.argv[1] if len(sys.argv)>1 else 'HEAD'
head=subprocess.check_output(['git','-C',str(root),'rev-parse','--verify','--end-of-options',ref+'^{commit}'],text=True).strip()
subprocess.run(['git','-C',str(root),'merge-base','--is-ancestor',manifest['commit'],head],check=True)
for record in manifest['files']:
    data=subprocess.check_output(['git','-C',str(root),'show',head+':'+record['path']])
    assert hashlib.sha256(data).hexdigest()==record['sha256'], record['path']+' differs from the qualified batch'
print('PASS:',manifest['commit'],'is an ancestor and all',len(manifest['files']),'batch paths match in',head)
