import json, math, sys, hashlib
from pathlib import Path
import mpmath as mp
mp.mp.dps=90
source=Path(sys.argv[1])
rows=[json.loads(l) for l in source.read_text().splitlines() if l.startswith('{"kind":"roundoff-input"')]
assert len(rows) in (4,8), len(rows)
results=[]
for start,platform in ((0,'JVM'),(4,'JS'))[:len(rows)//4]:
 for offset in (0,2):
  old,new=rows[start+offset:start+offset+2]
  assert old['voxel']==new['voxel'] and old['response']==new['response']
  z=mp.matrix([mp.mpf(v) for v in old['response']]); pairs=[]
  for p in (old,new):
   d=mp.matrix(p['rows'],p['cols'],[0])
   for i in range(p['rows']):
    for j in range(p['cols']):d[i,j]=mp.mpf(p['design'][i*p['cols']+j])
   b=mp.matrix([mp.mpf(v) for v in p['amplitudes']]); g=d.T*d; rhs=d.T*z
   refit=mp.lu_solve(g,rhs)
   def rss(beta):
    r=z-d*beta;return (r.T*r)[0]
   pred=[sum(p['design'][i*3+j]*p['amplitudes'][j] for j in range(3)) for i in range(p['rows'])]
   rounded=mp.matrix([mp.mpf(v) for v in pred]);r=z-rounded
   pairs.append({'rss':rss(b),'refit':rss(refit),'rounded':(r.T*r)[0],'pred':pred,'solveResidual':mp.norm(g*b-rhs,p=mp.inf),'betaError':mp.norm(b-refit,p=mp.inf)})
  a,b=pairs
  terms=[(p-q)*((r-p)+(r-q)) for p,q,r in zip(a['pred'],b['pred'],old['response'])]
  item={'platform':platform,'voxel':old['voxel'],'reportedDifference':new['profileEnergy']-old['profileEnergy'],'reportedUlps':(new['profileEnergy']-old['profileEnergy'])/math.ulp(old['profileEnergy']),'fixedDoubleInputsResidualDifference':mp.nstr(b['rss']-a['rss'],50),'refittedFixedDesignDifference':mp.nstr(b['refit']-a['refit'],50),'fixedRoundedPredictorDifference':mp.nstr(b['rounded']-a['rounded'],50),'stableDoubleDifference':math.fsum(terms),'sumAbsDifferenceTerms':math.fsum(map(abs,terms)),'maxSolveResidual':mp.nstr(max(a['solveResidual'],b['solveResidual']),30),'maxAmplitudeError':mp.nstr(max(a['betaError'],b['betaError']),30)}
  results.append(item)
output={'source':str(source),'sourceSha256':hashlib.sha256(source.read_bytes()).hexdigest(),'mpmath':mp.__version__,'decimalDigits':mp.mp.dps,'scope':'Fixed binary64 design and projected response; does not certify ideal HRF coefficient evaluation. Diagnostic only.','results':results}
print(json.dumps(output,indent=2))
