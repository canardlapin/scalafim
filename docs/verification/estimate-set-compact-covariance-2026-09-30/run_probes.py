"""Serialized fresh-process resource, relocation, scientific and transaction evidence.

Run affected JVM tests first. The controller takes the project sbt lock so the
compiled closure cannot change during its probes. Every Java job gets its own
64 MiB heap and raw /usr/bin/time receipt. No timing superiority is asserted.
"""
from pathlib import Path
from fractions import Fraction
import fcntl
import hashlib
import json
import os
import platform
import re
import shutil
import struct
import subprocess
import sys
import time

WORK = Path(__file__).resolve().parents[3]
EXECUTION = Path('/private/tmp/scalafim-execution-20260929')
OUTPUT = Path(sys.argv[1]).resolve()
if OUTPUT.exists(): raise SystemExit(f'refuse existing campaign directory {OUTPUT}')
OUTPUT.mkdir(parents=True)

def sha(path):
    with Path(path).open('rb') as stream: return hashlib.file_digest(stream,'sha256').hexdigest()

def export(module):
    return (WORK/f'modules/{module}/jvm/target/streams/test/fullClasspath/_global/streams/export').read_text().strip().split(os.pathsep)

def closure(paths):
    entries=[]
    for item in paths:
        p=Path(item)
        if p.is_dir():
            files=[{'path':str(f.relative_to(p)),'bytes':f.stat().st_size,'sha256':sha(f)} for f in sorted(p.rglob('*.class'))]
            entries.append({'path':str(p),'classes':files})
        else: entries.append({'path':str(p),'bytes':p.stat().st_size,'sha256':sha(p)})
    return entries

receipts=[]

def run(label, paths, cls, args, expected=0, marker=None):
    cp=os.pathsep.join(paths)
    command=['/usr/bin/time','-l','java','-Xmx64m','-XX:ActiveProcessorCount=2','-cp',cp,cls,*map(str,args)]
    log=OUTPUT/f'{label}.log'
    start=time.time()
    with log.open('w') as stream:
        completed=subprocess.run(command,stdout=stream,stderr=subprocess.STDOUT)
    raw=log.read_text()
    valid=completed.returncode==expected and (marker is None or marker in raw)
    parsed=[json.loads(line) for line in raw.splitlines() if line.startswith('{')]
    rss=re.search(r'^\s*(\d+)\s+maximum resident set size\s*$',raw,re.M)
    entry={'label':label,'command':command,'exit_code':completed.returncode,'expected_exit':expected,'passed':valid,
        'duration_seconds':time.time()-start,'raw_log':str(log),'raw_log_sha256':sha(log),
        'classpath_sha256':hashlib.sha256(cp.encode()).hexdigest(),'measurements':parsed,
        'maximum_rss_bytes':int(rss.group(1)) if rss else None}
    receipts.append(entry)
    (OUTPUT/'receipt.json').write_text(json.dumps(receipts,indent=2)+'\n')
    print(f'{label}: exit={completed.returncode} passed={valid}',flush=True)
    if not valid: raise RuntimeError(raw[-5000:])
    return entry

def nifti(ref, root):
    p=root/ref['Path']; assert p.stat().st_size==ref['Bytes'] and sha(p)==ref['SHA256']
    with p.open('rb') as stream: h=stream.read(352)
    assert struct.unpack_from('<i',h)[0]==348
    dim=struct.unpack_from('<8h',h,40); dtype=struct.unpack_from('<h',h,70)[0]
    assert h[123]==2 and struct.unpack_from('<h',h,254)[0]==1
    return p,dim,dtype,struct.unpack_from('<f',h,108)[0],struct.unpack_from('<f',h,112)[0],struct.unpack_from('<f',h,116)[0]

def independent_check(root, compact, n):
    pointer=json.loads((root/'current.json').read_text())['Content']
    c_ref=pointer['manifest']; assert sha(root/c_ref['Path'])==c_ref['SHA256']
    collection=json.loads((root/c_ref['Path']).read_text())['Content']
    published=collection['units'][0][1]['reference']
    mref=published['manifest']; assert sha(root/mref['Path'])==mref['SHA256']
    manifest=json.loads((root/mref['Path']).read_text())
    assert manifest['Schema']==f'scalafim-estimates-core-nifti-{2 if compact else 1}'
    content=manifest['Content']; assert content['domain']['Support']==list(range(n))
    reps=content['Representations']
    if compact: reps=[r['Content'] for r in reps]
    upper=[]
    for i in range(64):
        for j in range(i,64):
            a=Fraction((1 if i%2==0 else -1)*(i%4+1),8)
            b=Fraction((1 if j%2==0 else -1)*(j%4+1),8)
            upper.append(float(a*b+(i+1 if i==j else 0)))
    scalar_cells=0; u_cells=0; integrity_bytes=0
    stored_upper=[]; stored_scales=[None]*n
    for rep in reps:
        if 'table' in rep:
            ref=rep['table']; p=root/ref['Path']; assert sha(p)==ref['SHA256'] and p.stat().st_size==ref['Bytes']
            table=json.loads(p.read_text()); assert table['Estimands']==[f'coefficient-{i}' for i in range(64)]
            assert len(table['Pairs'])==2080
            expected_pairs=[(i,j) for i in range(64) for j in range(i,64)]
            for row,expected,pair in zip(table['Pairs'],upper,expected_pairs):
                assert row['Value']==expected and row['Validity']==0 and (row['First'],row['Second'])==pair
                stored_upper.append(row['Value'])
            u_cells+=2080; integrity_bytes+=ref['Bytes']
            continue
        p,dim,dtype,offset,slope,intercept=nifti(rep['values'],root)
        v,vdim,vtype,voffset,vslope,vintercept=nifti(rep['validity'],root)
        count=2080 if rep['product']=='U' else 64
        assert dim[1:5]==(n,1,1,count) and vdim==dim and dtype==64 and vtype==2
        integrity_bytes+=rep['values']['Bytes']+rep['validity']['Bytes']
        with p.open('rb') as values,v.open('rb') as validity:
            values.seek(int(offset)); validity.seek(int(voffset))
            for k in range(count):
                for first in range(0,n,512):
                    chunk=struct.unpack('<512d',values.read(512*8)); codes=validity.read(512)
                    assert codes==b'\0'*512
                    for x,stored in enumerate(chunk):
                        sample=first+x
                        actual=stored if slope==0 else stored*slope+intercept
                        expected=upper[k] if rep['product']=='U' else (k*1000+sample if rep['product']=='effect' else (0 if sample%17==0 else float(Fraction(4+sample%5,4))))
                        assert actual==expected,(rep['product'],k,sample,actual,expected)
                        if rep['product']=='U':
                            u_cells+=1
                            if sample==0: stored_upper.append(actual)
                        else:
                            scalar_cells+=1
                            if rep['product']=='scale' and k==0: stored_scales[sample]=actual
    # Every conceptual Sigma entry is independently determined by rational U and
    # the complete checked scale map; selected full matrices are checked in JVM.
    reconstructed=0
    reference_sigma={s:tuple(float(Fraction(u)*Fraction(s)) for u in upper) for s in (0.,1.,1.25,1.5,1.75,2.)}
    assert len(stored_upper)==2080 and all(s is not None for s in stored_scales)
    for sample in range(n):
        actual_scale=stored_scales[sample]
        expected_scale=0. if sample%17==0 else float(Fraction(4+sample%5,4))
        for actual_u,expected_sigma in zip(stored_upper,reference_sigma[expected_scale]):
            assert actual_u*actual_scale==expected_sigma
            reconstructed+=1
    return {'root':str(root),'compact':compact,'samples':n,'scalar_cells_checked':scalar_cells,
        'stored_U_cells_checked':u_cells,'physical_Sigma_entries_checked':reconstructed,'integrity_bytes':integrity_bytes,
        'U_bytes':sum(r['table']['Bytes'] if 'table' in r else r['values']['Bytes']+r['validity']['Bytes'] for r in reps if r['product']=='U'),
        'unit_manifest_bytes':mref['Bytes'],'catalog_bytes':content['Catalog']['Bytes'],
        'tsv_bytes':sum(r['Bytes'] for r in content['Tables'].values())}

with (EXECUTION/'sbt.lock').open('a') as lock:
    print('Waiting for shared build resource slot',flush=True)
    fcntl.flock(lock,fcntl.LOCK_EX)
    io=export('estimates-io'); fit=export('fit-estimates')
    forbidden=('/modules/fit/jvm/','/modules/fit-estimates/jvm/target/scala-3.7.4/classes')
    consumer=[p for p in fit if not any(s in p for s in forbidden) and '/test-classes' not in p]
    standalone=OUTPUT/'consumer-classes'; standalone.mkdir()
    prefix=WORK/'modules/fit-estimates/jvm/target/scala-3.7.4/test-classes'
    for source in prefix.rglob('CompactGroupReadbackProbe*.class'):
        target=standalone/source.relative_to(prefix); target.parent.mkdir(parents=True,exist_ok=True); shutil.copyfile(source,target)
    consumer.insert(0,str(standalone))
    assert not any('/modules/fit/jvm/' in p for p in io+consumer)
    before={'io':closure(io),'group_consumer':closure(consumer),'fit_writer':closure(fit)}
    (OUTPUT/'runtime-closure.json').write_text(json.dumps(before,indent=2)+'\n')
    checks=[]
    for n in (2048,8192):
        for compact in (False,True):
            mode='compact' if compact else 'core1'; root=OUTPUT/f'{mode}-{n}'
            run(f'{mode}-{n}-write',io,'scalafim.estimates.io.CompactCovarianceProbe',['write',root,mode,n],marker='"mode":"write"')
            relocated=OUTPUT/f'{mode}-{n}-relocated'; shutil.move(root,relocated)
            run(f'{mode}-{n}-read',io,'scalafim.estimates.io.CompactCovarianceProbe',['read',relocated,mode,n],marker='"fitterPresent":false')
            checks.append(independent_check(relocated,compact,n))
            (OUTPUT/'independent-checks.json').write_text(json.dumps(checks,indent=2)+'\n')
    compact_checks=[c for c in checks if c['compact']]
    assert compact_checks[0]['U_bytes']==compact_checks[1]['U_bytes']
    writer_stats=[r['measurements'][0] for r in receipts if r['measurements'] and r['measurements'][0]['mode']=='write' and r['measurements'][0]['compact']]
    assert [r['pairCoverageEntries'] for r in writer_stats]==[2080,2080]
    group=OUTPUT/'fitted-group'
    run('group-producer',fit,'scalafim.fmri.fit.estimates.CompactGroupProducerProbe',[group],marker='GROUP_PRODUCER_PASS')
    relocated=OUTPUT/'fitted-group-relocated'; shutil.move(group,relocated)
    run('group-reader',consumer,'scalafim.fmri.fit.estimates.CompactGroupReadbackProbe',[relocated],marker='GROUP_READER_PASS')
    for cls,stages in [('EstimateCrashProbe',['crash-before-seal','crash-after-seal','crash-after-collection','crash-after-cas']),
        ('CompactPublicationProbe',['crash-before-table','crash-after-table','crash-after-unit','crash-after-cas'])]:
        for stage in stages:
            root=OUTPUT/f'{cls}-{stage}'
            run(f'{cls}-{stage}-seed',io,'scalafim.estimates.io.'+cls,['seed',root],marker='SEED_PASS')
            run(f'{cls}-{stage}-halt',io,'scalafim.estimates.io.'+cls,[stage,root],87)
            run(f'{cls}-{stage}-read',io,'scalafim.estimates.io.'+cls,['read',root,'new' if stage=='crash-after-cas' else 'base'],marker='READ_PASS')
    assert before=={'io':closure(io),'group_consumer':closure(consumer),'fit_writer':closure(fit)},'runtime class closure changed'
    (OUTPUT/'complete.json').write_text(json.dumps({'jobs':len(receipts),'all_passed':True,'runtime_closure_sha256':sha(OUTPUT/'runtime-closure.json'),
        'independent_checks_sha256':sha(OUTPUT/'independent-checks.json'),'runtime':subprocess.check_output(['java','-version'],stderr=subprocess.STDOUT,text=True),
        'platform':platform.platform(),'timing_claim':'descriptive only','power_loss_qualified':False},indent=2)+'\n')
print('PROBES_COMPLETE',flush=True)
