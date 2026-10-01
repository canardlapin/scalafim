"""Compare historical DEV fingerprints, available literal response words and bounded output evidence.

No fresh responses, fitting, policy or gate changes. New source must replay the
same 200 JVM and 200 JS historical rows. Passing diagnostics is not admission.
"""
import argparse
from collections import Counter
import hashlib,json,math,importlib.util
from fractions import Fraction as F
from pathlib import Path

ROOT=Path(__file__).resolve().parents[3]
OLD=ROOT/'docs/verification/condition-lwu-diagnostics-20260930'
HELPER=ROOT/'docs/verification/lwu-condition-repair-design-20260930/verify_saved_kkt.py'
spec=importlib.util.spec_from_file_location('saved_kkt',HELPER)
kkt=importlib.util.module_from_spec(spec);spec.loader.exec_module(kkt)

def read(path):
    raw=path.read_bytes();records=[]
    for line in raw.decode().splitlines():
        line=line.removeprefix('[info] ').strip()
        if line.startswith('{"kind":"lwu-diagnostic"'):records.append(json.loads(line))
    assert len(records)==200,(str(path),len(records))
    assert len({(r['snr'],r['voxel']) for r in records})==200
    return records,hashlib.sha256(raw).hexdigest()

def check(old,new):
    assert len(new)==len(old)
    changes=[];literal_pairs=0;accepted_checked=0
    for a,b in zip(old,new):
        for key in ['platform','snr','seed','voxel','truth','truthAmplitudes','whitenedFingerprint']:
            assert a[key]==b[key],(key,a['platform'],a['snr'],a['voxel'])
        # Refused records retain literal inputs; an admitted record retains the
        # exact-word fingerprint rather than exposing a new raw response.
        for key in ['rawInputHex','whitenedInputHex']:
            if a[key] is not None and b[key] is not None:
                assert a[key]==b[key]
                if key=='rawInputHex':literal_pairs+=1
            assert (b[key] is not None)==(b['status']!='Accepted')
            if b[key] is not None:assert len(b[key])==600
        w=b['work'];assert len(w)==7 and all(v>=0 for v in w)
        assert w[0]<=819 and w[1]<=8 and w[2]<=2 and w[3]<=6*4+1 and w[5]<=6 and w[6]<=1
        assert w[3]<=w[1]-1+w[2] and w[4]<=w[1]-1 and w[5]<=w[3]
        assert b['sourceId']=='tangent-dev-8840dbfcebb53ccf60396b8244b75235eb589bd4'
        t=b['terminal'];assert t is not None
        for key,n in [('coordinates',3),('amplitudes',3),('dataHessian',9),('augmentedHessian',9),('conditionalSd',3)]:assert len(b[key])==n
        for key,n in [('gradient',3),('hessian',9),('amplitudes',3),('gradientHex',3),('hessianHex',9)]:
            assert len(t[key])==n
        for key in ['gradient','hessian','amplitudes']:
            assert all(v is not None and math.isfinite(v) for v in t[key])
        assert all(v is not None and math.isfinite(v) for v in b['coordinates'])
        for a1,b1 in zip(b['amplitudes'],t['amplitudes']):assert math.isclose(a1,b1,rel_tol=0.,abs_tol=1e-9)
        assert math.isclose(t['energy'],b['fitEnergy'],rel_tol=0.,abs_tol=1e-9)
        assert math.isclose(t['energy'],b['decodeEnergy'],rel_tol=0.,abs_tol=1e-9)
        h=b['dataHessian'];ah=b['augmentedHessian']
        if all(v is not None and math.isfinite(v) for v in h):
            for x,y,z in zip(h,ah,t['hessian']):
                assert math.isclose(x,z,rel_tol=0.,abs_tol=1e-8)
                assert math.isclose(y,z,rel_tol=0.,abs_tol=1e-8)
        else:assert b['status']!='Accepted'
        if b['status']=='Stalled':assert b['budgetExit'] is None
        if b['status']=='Accepted':
            g=[F(float.fromhex(v)) for v in t['gradientHex']]
            hv=[F(float.fromhex(v)) for v in t['hessianHex']];h=[hv[i*3:i*3+3] for i in range(3)]
            assert kkt.spd(h)
            direction=kkt.direction(b['coordinates'],g,h)
            assert direction is not None and max(map(abs,direction))<=F(float(1e-9))
            accepted_checked+=1
        if a['coordinates']!=b['coordinates'] or a['status']!=b['status'] or a['work']!=b['work']:
            changes.append({'snr':b['snr'],'voxel':b['voxel'],'beforeStatus':a['status'],'afterStatus':b['status'],
                'beforeCoordinates':a['coordinates'],'afterCoordinates':b['coordinates'],
                'beforeEnergy':a['decodeEnergy'],'afterEnergy':b['decodeEnergy'],
                'beforeWork':a['work'],'afterWork':b['work'],'afterBudgetExit':b['budgetExit']})
    return changes,literal_pairs,accepted_checked

def analyze(jvm,js):
    result={'scope':'Historical DEV fingerprint agreement, available literal-word comparisons, schema/energy/amplitude/Hessian/counter coherence and accepted recorded-jet algebra on JVM and JS; fingerprints are noncryptographic and do not prove all accepted response words; no fresh or inference/global qualification',
        'sourceSha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'platforms':{}}
    for platform,newpath,oldname in [('JVM',jvm,'lwu-diagnostic-v3-jvm.log'),('JS',js,'lwu-diagnostic-v4-js-node.log')]:
        old,oldhash=read(OLD/oldname);new,newhash=read(newpath);changes,literal_pairs,accepted_checked=check(old,new)
        groups={}
        for snr in [1.,.5]:
            before=Counter(r['status'] for r in old if r['snr']==snr);after=Counter(r['status'] for r in new if r['snr']==snr)
            groups[str(snr)]={'before':dict(before),'after':dict(after),'admitted':after['Accepted'],'95of100GateMet':after['Accepted']>=95}
        result['platforms'][platform]={'oldSha256':oldhash,'newSha256':newhash,'records':200,'allHistoricalFingerprintsMatch':True,'literalResponsePairsCompared':literal_pairs,'allAvailableLiteralWordsMatch':True,'acceptedInputEvidence':'64-bit rotate/XOR fingerprint only; literal words unavailable',
            'allSchemaEnergyAmplitudeHessianAndCounterControlsPass':True,'acceptedRecordedJetAlgebraChecked':accepted_checked,'acceptedRecordedJetSpdAndFreeCorrectionControlsPass':True,'groups':groups,'changes':changes}
    return result

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('jvm',type=Path);p.add_argument('js',type=Path);p.add_argument('--out',required=True,type=Path);a=p.parse_args()
    r=analyze(a.jvm,a.js)
    with a.out.open('x') as f:json.dump(r,f,indent=2);f.write('\n')
    print(json.dumps({p:v['groups'] for p,v in r['platforms'].items()},indent=2))
