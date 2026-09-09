"""Extract a historical receipt into an empty directory, verifying every payload."""
import argparse,pathlib,json,gzip,base64,hashlib
p=argparse.ArgumentParser();p.add_argument('receipt',type=pathlib.Path);p.add_argument('destination',type=pathlib.Path);a=p.parse_args()
a.destination.mkdir(parents=True,exist_ok=True);assert not any(a.destination.iterdir()),'destination must be empty'
r=json.loads(gzip.decompress(a.receipt.read_bytes()))
assert r['schema']=='scalafim.group.controls.receipt/v1'
for name,item in r['files'].items():
    relative=pathlib.PurePosixPath(name);assert not relative.is_absolute() and '..' not in relative.parts
    data=base64.b64decode(item['data']) if item['encoding']=='base64' else item['data'].encode('utf-8')
    assert hashlib.sha256(data).hexdigest()==item['sha256'],name
    target=a.destination.joinpath(*relative.parts);target.parent.mkdir(parents=True,exist_ok=True);target.write_bytes(data)
print('Verified and extracted',len(r['files']),'files')
