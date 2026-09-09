"""Independent numerical parity, coverage, RNG/replay and scientific admission regressions."""
import json,pathlib,sys,numpy as np
from scipy.stats import beta
from calibrate import setup,evaluate
P=pathlib.Path(sys.argv[1]).resolve() if len(sys.argv)>1 else pathlib.Path(__file__).resolve().parent

def rows(name): return [json.loads(l) for l in (P/name).read_text().splitlines()]
reference=json.loads((P/'reference.json').read_text());maxima=dict(covariance=0.,df=0.,p=0.,influence=0.,leverage=0.)
for case in reference['cases']:
 X=np.array(case['X']);y=np.array(case['y']);d=setup(X);res=y-X@np.linalg.lstsq(X,y,rcond=None)[0]
 V=(d['inv']*(res*res/(1-d['h'])))@d['inv'].T
 maxima['covariance']=max(maxima['covariance'],float(np.max(np.abs(V-case['covariance']))))
 maxima['df']=max(maxima['df'],abs(d['df']-case['df'][1]));maxima['p']=max(maxima['p'],abs(evaluate(y[None,:],d)[0][0]-case['p'][1]))
 maxima['influence']=max(maxima['influence'],float(np.max(np.abs(d['inv']-case['influence']))));maxima['leverage']=max(maxima['leverage'],float(np.max(np.abs(d['h']-case['leverage']))))
assert max(maxima.values())<1e-10,maxima
r=json.loads((P/'bootstrap-reference.json').read_text());d=setup(np.array(r['X']));Y=np.array([r['y']]);signs=np.array([r['actions']]);calc=evaluate(Y,d,signs)
assert abs(calc[2][0]-r['monteCarloP'])<1e-14
q,a,h=[d[k] for k in ['q','a','h']];e=Y-(Y@q)@q.T;er=(e+(Y@a)[:,None]*a/np.sum(a*a))/np.sqrt(1-h+a*a/np.sum(a*a));E=er[:,None,:]*signs;res=E-(E@q)@q.T;st=(E@a)/np.sqrt(np.sum(res*res*(a*a/(1-h)),axis=-1))
maxBootstrapDelta=float(np.max(np.abs(st[0]-r['statistics'])));assert maxBootstrapDelta<1e-10
pilot=rows('pilot.jsonl');confirmation=rows('confirmation.jsonl');assert len(pilot)==108 and len(confirmation)==8
assert sum(r['trials'] for r in pilot)==216000 and sum(r['trials'] for r in confirmation)==160000
summary={}
for label,data in [('pilot',pilot),('confirmation',confirmation)]:
 failures={m:[] for m in ['CR2','WCR2']}
 for r in data:
  assert max(r['nonFiniteP'].values())==0
  for m in ['CR2','OLS','WCR2']:assert r['coverage'][m]+r['null'][m]==r['trials'],(r,m)
  assert r['coverageInversionMaxPDelta']<1e-10
  for m in failures:
   k=r['null'][m];lo=float(beta.ppf(.001/(2*len(data)),k,r['trials']-k+1)) if k else 0.
   if lo>.05:failures[m].append(dict(cell=[r[key] for key in ['n','group','variance','error','tau2']],rate=k/r['trials'],simultaneousLower=lo))
 for m in failures:assert failures[m],f'{m} no longer reproduces known calibration failures; review admission evidence'
 summary[label]=dict(studies=sum(r['trials'] for r in data),cells=len(data),familyConfidence=.999,failures=failures,nonFiniteP=0,maxShiftPDelta=max(r['coverageInversionMaxPDelta'] for r in data))
 # When the original two-outcome run is retained, ensure adding coverage checks did not alter its RNG stream.
 original=P/(label+'-original.jsonl')
 if original.exists():
  old=rows(original.name);assert len(old)==len(data)
  for before,after in zip(old,data):
   for key in ['null','power','df','maxLeverage']:assert before[key]==after[key],(key,before,after)
# Subject-bound native export is compared to the independent R calculation.
native=json.loads((P/'native-review-20-0.json').read_text());ref=next(c for c in reference['cases'] if c['n']==20 and c['group']=='quarter');a=np.array(ref['influence'][1]);a/=np.linalg.norm(a)
assert abs(native['workingDf']-ref['df'][1])<1e-10
for i,row in enumerate(native['rows']):
 assert row['subject']==f'subject-{i+1}'
 assert abs(row['leverage']-ref['leverage'][i])<1e-11
 assert abs(row['influence']-a[i])<1e-11
summary.update(referenceMaxErrors=maxima,bootstrapActions=256,bootstrapMaxStatisticDelta=maxBootstrapDelta,nativeRows=20,admission='Neither tested candidate admitted as a general inference default')
(P/'summary.json').write_text(json.dumps(summary,indent=2));print(json.dumps(summary,indent=2))
