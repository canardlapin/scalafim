#!/usr/bin/env python3
"""Verify immutable external inputs before and after both-platform execution."""
import argparse
import csv
import hashlib
import json
from pathlib import Path

def sha(path):
 h=hashlib.sha256()
 with path.open('rb') as f:
  for b in iter(lambda:f.read(1048576),b''):h.update(b)
 return h.hexdigest()

def verify(directory):
 root=Path(__file__).resolve().parent
 r=json.loads((directory/'receipt.json').read_text())
 assert r['phase'] in ('qa','control') and r['passed'] and not r['scientific_admission']
 assert r['replicates']==2048
 for name,digest in r['source_locks'].items():assert sha(root/name)==digest,name
 cells=json.loads((root/'cells.json').read_text())
 assert {i['cell'] for i in r['inputs']}=={c['cell'] for c in cells}
 for i in r['inputs']:
  cell=next(c for c in cells if c['cell']==i['cell'])
  assert i['shape']==[2048,4,len(cell['design'])]
  path=directory/i['file'];assert path.parent==directory and path.name==cell['cell']+'.f64le'
  assert path.stat().st_size==i['bytes']==2048*4*len(cell['design'])*8
  assert sha(path)==i['sha256'],str(path)
 states=directory/'stream-states.tsv';assert sha(states)==r['stream_states_sha256']
 rows=list(csv.DictReader(states.open(),delimiter='\t'))
 assert len(rows)==5*2048*4*2
 keys={(x['cell'],int(x['dataset']),int(x['voxel']),int(x['run'])) for x in rows}
 expected={(c['cell'],d,v,t) for c in cells for d in range(2048) for v in range(4) for t in range(2)}
 assert keys==expected
 seeds={tuple(x['s'+str(i)] for i in range(1,7)) for x in rows}
 assert len(seeds)==len(rows) and {x['phase'] for x in rows}=={r['phase']}
 return r,seeds

def main():
 p=argparse.ArgumentParser(description=__doc__);p.add_argument('qa',type=Path);p.add_argument('control',type=Path);a=p.parse_args()
 qa,s1=verify(a.qa);control,s2=verify(a.control)
 assert qa['phase']=='qa' and control['phase']=='control' and not (s1&s2)
 assert qa['scalar_checks']<=1000000 and not qa['failures'] and qa['legacy_negative_control']['rejected']
 print(json.dumps({'verified':True,'datasets_per_phase':10240,'noise_substreams_per_phase':len(s1),'overlap':0,'scientific_admission':False}))
if __name__=='__main__':main()
