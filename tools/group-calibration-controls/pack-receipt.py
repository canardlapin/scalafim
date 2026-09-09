"""Pack exact reusable evidence, excluding regenerable bulk binary simulation arrays."""
import pathlib,hashlib,json,gzip,base64,datetime
P=pathlib.Path(__file__).resolve().parent
excluded={'receipt-sha256.txt','group-controls-2026-09-08.md','issue.txt','installed-manifest.json','receipt-smoke.log'}
extensions={'.py','.R','.scala','.md','.txt','.json','.jsonl','.log','.svg','.png'}
files={}
for f in sorted(P.iterdir()):
    if not f.is_file() or f.suffix not in extensions or f.name in excluded:continue
    raw=f.read_bytes();binary=f.suffix=='.png'
    files[f.name]=dict(sha256=hashlib.sha256(raw).hexdigest(),encoding='base64' if binary else 'utf8',data=base64.b64encode(raw).decode() if binary else raw.decode())
r=dict(schema='scalafim.group.controls.receipt/v1',recordedAt=datetime.datetime.now(datetime.timezone.utc).isoformat(),scientificScope='Independent-subject scalar group contrast; research qualification; no new general default',independentStudies=642000,
       stages={'pilot':82000,'confirmation':80000,'estimated':240000,'feasible-weighting':240000},
       nativeIssue='bd-01M219Z3QVF32QFAAJNVSC3SSW',nextResearchIssue='bd-01M21BNZR9ZBRAYY9JD5WCQ8KX',
       omitted='Regenerable bulk native binaries, per-study NPZ arrays, compiled classes, renderer dependency installs and caches. Original scratch run retains them; methods, sources, seeds, hashes and all aggregate evidence are included.',files=files)
raw=json.dumps(r,separators=(',',':'),allow_nan=False).encode();out=P/'group-controls-2026-09-08.json.gz';out.write_bytes(gzip.compress(raw,compresslevel=9,mtime=0))
digest=hashlib.sha256(out.read_bytes()).hexdigest();(P/'receipt-sha256.txt').write_text(digest+'\n')
print(len(files),'files;',out.stat().st_size,'bytes; SHA256',digest)
