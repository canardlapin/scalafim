#!/usr/bin/env python3
"""Generate S10 harness inputs with the frozen v0 overrides; never a pilot root."""
import argparse
from dataclasses import replace
import hashlib
import json
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'generator'))
from phrf_gen.cells import CELLS
from phrf_gen.io import code_sha256, write_dataset
from phrf_gen.seeds import HARNESS_ROOT
PILOT_CELLS=('C-TX-.5','C-TS-1','C-TS-.5','C-TG-.5','T-TX-fast','T-TX-jit','T-TS-fast')

def main():
 p=argparse.ArgumentParser(description=__doc__);p.add_argument('output',type=Path);a=p.parse_args()
 a.output.mkdir(parents=True,exist_ok=False)
 records=[]
 for cid in PILOT_CELLS:
  cell=replace(CELLS[cid],n_voxels=40,n_pool=4000 if cid in ('T-TX-fast','T-TX-jit') else 0)
  # Ten T-TX-fast datasets support the original no-deviation check; only 0/1
  # enter the seven-cell two-dataset rehearsal.
  for d in range(10 if cid=='T-TX-fast' else 2):
   m=write_dataset(cell,HARNESS_ROOT,'harness',d,a.output)
   records.append({'cell':cid,'dataset':d,'npz_sha256':m['npz_sha256'],'spec':cell.to_json()})
   print(cid,d,m['npz_sha256'],flush=True)
 receipt={'schema':'phrf-s10-real-rehearsal-inputs/v1','root_kind':'harness','root_hex':f'{HARNESS_ROOT:016x}',
  'generator_code_sha256':code_sha256(),'generator_entry_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
  'datasets_per_cell':2,'no_deviation_datasets':10,'scored_voxels':40,'cells':list(PILOT_CELLS),'records':records,
  'scientific_admission':False,'pilot_launch_authority':False}
 (a.output/'rehearsal-inputs.json').write_text(json.dumps(receipt,sort_keys=True,indent=2)+'\n')
if __name__=='__main__':main()
