import json, math, sys, hashlib
from pathlib import Path
import mpmath as mp
mp.mp.dps=90
source=Path(sys.argv[1])
records=[json.loads(l) for l in source.read_text().splitlines() if l.startswith('{"kind":"roundoff-')]
assert sum(r['kind']=='roundoff-basis' for r in records)==2
results=[]
for platform,start in (('JVM',0),('JS',5)):
 common=records[start]; assert common['kind']=='roundoff-basis'
 m,k,n=common['basisRank'],common['rank'],len(common['lags'])
 phi=mp.matrix([[mp.mpf(common['phi'][j*n+i]) for i in range(n)] for j in range(m)])
 rh=mp.matrix([[mp.mpf(common['rHat'][i*3*m+j]) for j in range(3*m)] for i in range(k)])
 def design(h):
  coeff=phi*h
  return mp.matrix([[mp.fsum(rh[i,j*3+c]*coeff[j] for j in range(m)) for c in range(3)] for i in range(k)])
 def rss(d,z):
  b=mp.lu_solve(d.T*d,d.T*z);r=z-d*b;return (r.T*r)[0]
 for offset in (1,3):
  old,new=records[start+offset:start+offset+2]; pairs=[]
  for point in (old,new):
   z=mp.matrix(point['response']);tau,v=map(mp.mpf,point['coordinates']);
   h=mp.matrix([mp.exp(-mp.mpf('.5')*(mp.mpf(t)-tau)**2*mp.exp(-2*v)) for t in common['lags']])
   stored=mp.matrix(point['kernel'])
   inv=math.exp(-2*point['coordinates'][1]); direct=mp.matrix([math.exp(-.5*(t-point['coordinates'][0])**2*inv) for t in common['lags']])
   pairs.append({'storedKernel':rss(design(stored),z),'analyticKernel':rss(design(h),z),'directDoubleKernel':rss(design(direct),z),'maxKernelAbsError':max(abs(a-b) for a,b in zip(h,stored)),'maxKernelRelErrorAbove1e16':max(abs((a-b)/a) for a,b in zip(h,stored) if a>mp.mpf('1e-16'))})
  a,b=pairs
  results.append({'platform':platform,'voxel':old['voxel'],'storedKernelMpDownstreamDelta':mp.nstr(b['storedKernel']-a['storedKernel'],50),'analyticKernelMpDownstreamDelta':mp.nstr(b['analyticKernel']-a['analyticKernel'],50),'directDoubleKernelMpDownstreamDelta':mp.nstr(b['directDoubleKernel']-a['directDoubleKernel'],50),'maxKernelAbsError':mp.nstr(max(a['maxKernelAbsError'],b['maxKernelAbsError']),30),'maxKernelRelErrorAbove1e16':mp.nstr(max(a['maxKernelRelErrorAbove1e16'],b['maxKernelRelErrorAbove1e16']),30)})
print(json.dumps({'scope':'Fixed stored basis/rHat/projected response; isolate Gaussian samples from downstream sums and amplitude solve. No original-family qualification.','sourceSha256':hashlib.sha256(source.read_bytes()).hexdigest(),'decimalDigits':mp.mp.dps,'mpmath':mp.__version__,'results':results},indent=2))
