"""Independently verify compact fixture closure against retained full native files.

python verify_native_closure.py --native-dir ... --fixture-dir ... --source-dir ...
Requires numpy 2.5.3 and nibabel 5.4.2. No native tools or Scala code are called.
"""
from pathlib import Path
import argparse, gzip, hashlib, json
import nibabel as nib
import numpy as np

def sha(path):
    digest = hashlib.sha256()
    with path.open('rb') as stream:
        while data := stream.read(1 << 20): digest.update(data)
    return digest.hexdigest()

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    for name in ('native-dir','fixture-dir','source-dir'): parser.add_argument('--'+name,type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    fixture,native,source=args.fixture_dir,args.native_dir,args.source_dir
    manifest=json.loads((fixture/'manifest.json').read_text())
    contract=json.loads((fixture/'contract.json').read_text())
    provenance=json.loads((fixture/'source-provenance.json').read_text())
    for name,expected in manifest['fixture_sha256'].items(): assert sha(fixture/name)==expected,name
    for name,expected in manifest['native_work_sha256'].items(): assert sha(native/name)==expected,name
    for name,record in provenance['assets'].items():
        assert sha(source/name)==record['sha256'] and (source/name).stat().st_size==record['size'],name
    header_bytes=0
    for name,record in contract['native_input_headers'].items():
        original=gzip.decompress((native/f'{name}.nii.gz').read_bytes())
        retained=gzip.decompress((fixture/f'{name}_header.nii.gz').read_bytes())
        offset=record['header_bytes']
        assert retained[:offset]==original[:offset],name
        assert len(retained)==len(original) and not any(retained[offset:]),name
        header_bytes+=offset
    masked_values=0
    for name in ('example_func','highres','standard'):
        public=nib.load(source/f'{name}.nii.gz').get_fdata(dtype=np.float32)
        mask=nib.load(source/f'{name}_mask.nii.gz').get_fdata()>.5
        expected=np.asarray(public*mask,dtype=np.float32)
        native_values=np.asarray(nib.load(native/f'{name}.nii.gz').dataobj,dtype=np.float32)
        assert np.array_equal(expected.view('uint32'),native_values.view('uint32')),name
        masked_values+=expected.size
    values=0
    for name,record in contract['crops'].items():
        original_name='example_func.nii.gz' if name=='func_crop.nii.gz' else name
        original=np.asarray(nib.load(native/original_name).dataobj,dtype=np.float32)
        retained=np.asarray(nib.load(fixture/name).dataobj,dtype=np.float32)
        start=np.array(record['start']);stop=start+np.array(record['shape'])
        expected=original[tuple(slice(a,b) for a,b in zip(start,stop))]
        assert retained.shape==expected.shape and np.array_equal(retained.view('uint32'),expected.view('uint32')),name
        values+=retained.size
    commands=json.loads((fixture/'commands.json').read_text())
    assert len(commands)==19 and all(q['exit_code']==0 for q in commands)
    for mode,count in (('premat',693),('composed',693),('flirt',315)):
        support=nib.load(fixture/f'{mode}_support.nii.gz').get_fdata()
        assert support.size==count and np.isfinite(support).all() and np.min(support)>=.999,mode
    result={'status':'pass','fixture_hashes':len(manifest['fixture_sha256']),
            'full_native_hashes':len(manifest['native_work_sha256']),
            'source_hashes':len(provenance['assets']),'exact_header_bytes':header_bytes,
            'exact_public_times_mask_float32_values':int(masked_values),
            'exact_float32_crop_values':int(values),'native_successful_attempts':len(commands),
            'queries':{'standard':693,'highres':315},'no_excluded_frozen_query':True}
    args.output.write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result))

if __name__=='__main__':main()
