"""Read-only record/terminal algebra audit of the historical LWU diagnostic."""
import argparse
import collections
from decimal import Decimal, getcontext
import hashlib
import json
import math
from pathlib import Path
import struct

getcontext().prec = 60
D = lambda x: Decimal(str(x))

def determinant(a):
    n = len(a)
    if n == 0: return D(1)
    if n == 1: return a[0][0]
    return sum(((-1)**j)*a[0][j]*determinant([row[:j]+row[j+1:] for row in a[1:]]) for j in range(n))

def inverse_spd(a):
    n = len(a)
    if not all(determinant([row[:i] for row in a[:i]]) > 0 for i in range(1, n+1)): return None
    det = determinant(a)
    return [[D((-1)**(i+j))*determinant([row[:i]+row[i+1:] for k,row in enumerate(a) if k != j])/det
             for j in range(n)] for i in range(n)]

def near(a,b):
    return math.isfinite(a) and math.isfinite(b) and math.isclose(a,b,rel_tol=2e-10,abs_tol=2e-12)

def fingerprint(values):
    h = 0
    mask = (1<<64)-1
    for value in values:
        bits = struct.unpack('>Q',struct.pack('>d',value))[0]
        h = (((h<<5)&mask)|(h>>59)) ^ bits
    return format(h,'x')

def analyze(path, source):
    raw = Path(path).read_bytes()
    rows=[]
    for line in raw.decode().splitlines():
        line=line.removeprefix('[info] ').strip()
        if line.startswith('{') and '"kind":"lwu-diagnostic"' in line:
            rows.append(json.loads(line))
    assert len(rows)==200
    platforms={r['platform'] for r in rows};assert len(platforms)==1
    platform=platforms.pop();assert platform in ['JVM','JS']
    expected={'JVM':{1.0:91,.5:81},'JS':{1.0:89,.5:82}}[platform]
    cells=[];details=[]
    bounds=[(3,8),(math.log(.8),math.log(3)),(0,.8)]
    for snr,seed in [(1.0,111),(.5,112)]:
        cell=[r for r in rows if r['snr']==snr]
        assert len(cell)==100 and [r['voxel'] for r in cell]==list(range(100))
        statuses=collections.Counter();exits=collections.Counter();postfit=collections.Counter()
        for r in cell:
            assert r['seed']==seed and r['sourceId']==source and r['cohort']=='historical-dev'
            assert len(r['coordinates'])==len(r['truth'])==len(r['truthAmplitudes'])==len(r['amplitudes'])==3
            assert len(r['dataHessian'])==len(r['augmentedHessian'])==9 and len(r['conditionalSd'])==3
            assert all(math.isfinite(v) for v in r['coordinates']+r['truth']+r['truthAmplitudes']+r['amplitudes'])
            assert all(lo-1e-12<=v<=hi+1e-12 for v,(lo,hi) in zip(r['coordinates'],bounds))
            work=r['work'];assert len(work)==7 and all(type(v) is int and v>=0 for v in work)
            caps=[819,8,2,9,8,6,1];assert all(v<=cap for v,cap in zip(work,caps))
            assert work[4]<=work[1]
            statuses[r['status']]+=1
            if r['budgetExit'] is not None: exits[r['budgetExit']]+=1
            assert (r['budgetExit'] is not None)==(r['status']=='BudgetExceeded')
            refused=r['status']!='Accepted'
            assert (r['rawInputHex'] is not None)==refused and (r['whitenedInputHex'] is not None)==refused
            if refused:
                raw_input=[float.fromhex(v) for v in r['rawInputHex']]
                whitened=[float.fromhex(v) for v in r['whitenedInputHex']]
                assert len(raw_input)==len(whitened)==600 and all(math.isfinite(v) for v in raw_input+whitened)
                assert fingerprint(whitened)==r['whitenedFingerprint']
            t=r['terminal'];assert t is not None
            assert len(t['hessian'])==len(t['hessianHex'])==9 and len(t['gradient'])==len(t['gradientHex'])==3
            assert len(t['amplitudes'])==3
            assert float.fromhex(t['energyHex'])==t['energy']
            assert [float.fromhex(v) for v in t['gradientHex']]==t['gradient']
            assert [float.fromhex(v) for v in t['hessianHex']]==t['hessian']
            assert all(math.isfinite(v) for v in [t['energy']]+t['gradient']+t['hessian']+t['amplitudes'])
            assert near(r['decodeEnergy'],t['energy']) and near(r['fitEnergy'],t['energy'])
            assert all(near(a,b) for a,b in zip(r['amplitudes'],t['amplitudes']))
            h=[[D(t['hessian'][3*i+j]) for j in range(3)] for i in range(3)]
            assert all(near(float(h[i][j]),float(h[j][i])) for i in range(3) for j in range(3))
            inv=inverse_spd(h)
            sd=[float((2*inv[i][i]).sqrt()) for i in range(3)] if inv is not None else None
            if inv is not None: assert t['curvature']=='PositiveDefinite'
            gradient=list(map(D,t['gradient']))
            free=[i for i,(x,(lo,hi)) in enumerate(zip(r['coordinates'],bounds))
                  if not ((x<=lo+1e-12 and gradient[i]>0) or (x>=hi-1e-12 and gradient[i]<0))]
            sub=[[h[i][j] for j in free] for i in free]
            subinv=inverse_spd(sub) if free else inv
            step=(float(max(abs(sum(subinv[i][j]*gradient[k] for j,k in enumerate(free))) for i in range(len(free))))
                  if free and subinv is not None else (0.0 if not free and inv is not None else None))
            stationary=step is not None and step<=1e-9
            weak=sd is not None and any(v>cap for v,cap in zip(sd,[.5,1,1]))
            boundary=any(x<=lo+1e-12 or x>=hi-1e-12 for x,(lo,hi) in zip(r['coordinates'],bounds))
            returned=all(v is not None and math.isfinite(v) for v in r['dataHessian'])
            if returned:
                assert all(near(a,b) for a,b in zip(r['dataHessian'],t['hessian']))
                assert all(near(a,b) for a,b in zip(r['augmentedHessian'],t['hessian']))
                if sd is None: assert r['conditionalSd']==[None]*3
                else: assert all(v is not None and near(a,v) for a,v in zip(sd,r['conditionalSd']))
            else:
                assert r['dataHessian']==[None]*9 and r['augmentedHessian']==[None]*9
                assert r['conditionalSd']==[None]*3
            if r['status']=='Accepted': assert stationary and not boundary and not weak and returned and inv is not None
            elif r['status']=='WeaklyIdentified': assert weak or sd is None
            elif r['status']=='Boundary': assert boundary
            postfit['positiveCurvature']+=inv is not None
            postfit['stationary']+=stationary
            postfit['weakSd']+=weak
            postfit['boundary']+=boundary
            postfit['returnedCurvatureUnavailable']+=not returned
            if refused:
                details.append({'platform':platform,'snr':snr,'voxel':r['voxel'],'status':r['status'],'budgetExit':r['budgetExit'],
                                'coordinates':r['coordinates'],'truth':r['truth'],'newtonCorrection':step,
                                'postfitSd':sd,'weakSd':weak,'boundary':boundary,'returnedCurvatureAvailable':returned,'work':work})
        assert statuses['Accepted']==expected[snr]
        cells.append({'snr':snr,'seed':seed,'attempts':100,'admitted':statuses['Accepted'],'statuses':dict(statuses),
                      'budgetExits':dict(exits),'postfitTerminalChecks':dict(postfit),'admissionGateMet':statuses['Accepted']>=95})
    return {'platform':platform,'sourceId':source,'rawSha256':hashlib.sha256(raw).hexdigest(),'records':200,'cells':cells,
            'refusalDetails':details,'scope':'Historical DEV reproduction and recorded terminal algebra. Additional audit jets are outside decoder work; no fresh validation, direct-objective oracle, performance or policy promotion.'}

if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('raw');p.add_argument('--source',required=True);p.add_argument('--out',required=True)
    a=p.parse_args();x=analyze(a.raw,a.source)
    with open(a.out,'x') as f:json.dump(x,f,indent=2);f.write('\n')
    print(json.dumps({k:x[k] for k in ['platform','records','rawSha256','cells']},indent=2))
