"""Verify JVM trace reproduction and exact LS energy of each represented design."""
import argparse
from fractions import Fraction as F
import hashlib,json,math
from pathlib import Path
import runpy

HERE=Path(__file__).resolve().parent
helpers=runpy.run_path(str(HERE/'verify_saved_kkt.py'))
solve=helpers['solve'];direction=helpers['direction'];spd=helpers['spd']
def numbers(values):return [float.fromhex(v) for v in values]
def profile(row,projection):
    values=[F(x) for x in numbers(row['designHex'])];z=[F(x) for x in numbers(projection['zHex'])]
    assert len(values)==3*len(z)
    d=[values[i*3:i*3+3] for i in range(len(z))]
    gram=[[sum(r[i]*r[j] for r in d) for j in range(3)] for i in range(3)]
    b=[sum(r[i]*y for r,y in zip(d,z)) for i in range(3)]
    assert spd(gram)
    beta=solve(gram,b)
    assert max(abs(float(x)-v) for x,v in zip(beta,numbers(row['amplitudesHex'])))<2e-12
    return sum(x*y for x,y in zip(beta,b))
def main(raw,cases):
    source=Path(raw).read_bytes();rows=[json.loads(l) for l in source.decode().splitlines() if l.startswith('{')]
    controls=json.loads(Path(cases).read_text());details=[]
    for c in controls:
        name=c['file'];r=next(r for r in rows if r['kind']=='trace-result' and r['case']==name)
        assert r['status']==c['expectedStatus'] and r['work']==c['expectedWork']
        assert numbers(r['coordinatesHex'])==c['expectedCoordinates']
        projection=next(r for r in rows if r['kind']=='trace-projection' and r['case']==name)
        events=[r for r in rows if r['kind']=='jvm-lwu-trace' and r['case']==name]
        current=max((e for e in events if e['coordinatesHex']==r['coordinatesHex']),key=lambda e:e['ordinal'])
        baseline=profile(current,projection)
        item={'case':name,'status':r['status'],'work':r['work'],'reproducedStatusWorkCoordinates':True}
        if r['status']=='BudgetExceeded':
            current_energy=float.fromhex(r['energyHex']);ulp=math.ulp(current_energy)
            candidates=[]
            for e in events:
                if e['ordinal']<=current['ordinal']:continue
                assert e['ok']
                energy=float.fromhex(e['energyHex'])
                explained=profile(e,projection);delta=baseline-explained
                norm=None
                if e['event']=='jet':
                    g=[F(v) for v in numbers(e['gradientHex'])];v=[F(z) for z in numbers(e['hessianHex'])];h=[v[3*i:3*i+3] for i in range(3)]
                    p=direction(numbers(e['coordinatesHex']),g,h)
                    norm=float(max(map(abs,p))) if p is not None else None
                candidates.append({'ordinal':e['ordinal'],'event':e['event'],'coordinatesHex':e['coordinatesHex'],'roundedEnergyChange':energy-current_energy,'roundedChangeInUlps':(energy-current_energy)/ulp,'exactRepresentedDesignEnergyChange':float(delta),'exactChangeInUlps':float(delta)/ulp,'newtonCorrection':norm,'stationary':norm is not None and norm<=1e-9})
            item['rejectedEvents']=candidates
            item['stationaryRejectedDespiteRepresentedDecrease']=any(e['stationary'] and e['roundedEnergyChange']>0 and e['exactRepresentedDesignEnergyChange']<0 for e in candidates)
        details.append(item)
    assert len(details)==9
    caps=[d for d in details if d['status']=='BudgetExceeded'];assert len(caps)==7
    return {'scope':'Nine JVM saved-input reproductions; exact rational linear LS at exported binary64 D and z. This does not establish exact continuous-kernel energy or JS callback behavior.','rawSha256':hashlib.sha256(source).hexdigest(),'records':9,'statusWorkCoordinatesReproduced':9,'candidateCaps':7,'capsWithRejectedStationaryRepresentedDecrease':sum(d['stationaryRejectedDespiteRepresentedDecrease'] for d in caps),'details':details}
if __name__=='__main__':
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('raw');p.add_argument('cases');p.add_argument('--out',required=True);a=p.parse_args()
    result=main(a.raw,a.cases)
    with Path(a.out).open('x') as f:json.dump(result,f,indent=2);f.write('\n')
    print(json.dumps({k:v for k,v in result.items() if k!='details'},indent=2))
    for d in result['details']:
        if 'rejectedEvents' in d:
            print(d['case'],[(e['event'],e['roundedChangeInUlps'],e['exactChangeInUlps'],e['stationary']) for e in d['rejectedEvents']])
