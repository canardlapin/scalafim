import os
for key in ['OPENBLAS_NUM_THREADS','VECLIB_MAXIMUM_THREADS','OMP_NUM_THREADS']: os.environ[key]='1'
import numpy as np
from scipy.stats import t, beta as beta_dist
import itertools,json,time,argparse,pathlib
P=pathlib.Path(__file__).resolve().parent

def design(n,group):
 g=(np.arange(n)<(n//2 if group=='balanced' else max(2,n//4))).astype(float)
 z=np.cos((np.arange(n)+.5)*2*np.pi/n)
 return np.column_stack([np.ones(n),g,z])

def setup(X):
 q=np.linalg.qr(X)[0]; inv=np.linalg.pinv(X); a=inv[1]; H=q@q.T; M=np.eye(len(X))-H; h=np.diag(H)
 g=a[:,None]*M/np.sqrt(1-h)[:,None]; B=g.T@g; df=np.trace(B)**2/np.sum(B*B)
 return dict(q=q,a=a,h=h,df=df,M=M,inv=inv)

def evaluate(Y,d,signs=None):
 q,a,h,df,M=[d[k] for k in ['q','a','h','df','M']]
 res=Y-(Y@q)@q.T; est=Y@a; se=np.sqrt(np.sum(res**2*(a*a/(1-h)),axis=-1)); stat=est/se
 p=2*t.sf(np.abs(stat),df)
 ols=2*t.sf(np.abs(est)/np.sqrt(np.sum(res*res,axis=-1)/(Y.shape[-1]-q.shape[1])*np.sum(a*a)),Y.shape[-1]-q.shape[1])
 if signs is None: return p,ols
 # Restricted fit of c'beta=0. Its leverage is h-a^2/(a'a).
 er=(res+est[:,None]*a/np.sum(a*a))/np.sqrt(1-h+a*a/np.sum(a*a))
 E=er[:,None,:]*signs
 estar=E@a; rstar=E-(E@q)@q.T
 sestar=np.sqrt(np.sum(rstar*rstar*(a*a/(1-h)),axis=-1))
 bstat=estar/sestar
 wp=(1+np.sum(np.abs(bstat)>=np.abs(stat)[:,None]-1e-12,axis=1))/(1+signs.shape[1])
 return p,ols,wp

def cell(n,grp,profile,error,tau,trials,draws,root,actionroot,index):
 X=design(n,grp);d=setup(X);v=np.linspace(.04,1,n)
 if profile=='reverse': v=v[::-1]
 if profile=='outlier': v=np.full(n,.04);v[0]=10.
 rng=np.random.default_rng(np.random.SeedSequence([root,index])); arng=np.random.default_rng(np.random.SeedSequence([actionroot,index]))
 sums=np.zeros((2,3),int);coverage=np.zeros(3,int);failures=np.zeros(3,int);coverage_max=0.;start=time.time()
 for st in range(0,trials,20):
  k=min(20,trials-st)
  if error=='normal': e=rng.normal(size=(k,n))
  elif error=='t3': e=rng.standard_t(3,size=(k,n))/np.sqrt(3)
  else: e=(rng.lognormal(size=(k,n))-np.exp(.5))/np.sqrt(np.e*(np.e-1))
  Y=X@np.array([1.7,0,.8])+e*np.sqrt(v+tau)
  signs=arng.integers(0,2,size=(k,draws,n),dtype=np.int8)*2-1
  for alt,shift in enumerate([0.,.7]):
   p=evaluate(Y+shift*X[:,1],d,signs)
   sums[alt]+=np.array([np.count_nonzero(z<=.05) for z in p])
   failures+=np.array([np.count_nonzero(~np.isfinite(z)) for z in p])
   if alt==0: nullp=p
  # test inversion: subtract known .7 shift then compare all methods on the exact same actions.
  Yback=(Y+.7*X[:,1])-.7*X[:,1]
  backp=evaluate(Yback,d,signs)
  coverage+=np.array([np.count_nonzero(z>.05) for z in backp])
  failures+=np.array([np.count_nonzero(~np.isfinite(z)) for z in backp])
  coverage_max=max(coverage_max,max(np.max(np.abs(x-y)) for x,y in zip(nullp,backp)))
 return dict(n=n,group=grp,variance=profile,error=error,tau2=tau,trials=trials,draws=draws,df=d['df'],maxLeverage=d['h'].max(),null=dict(zip(['CR2','OLS','WCR2'],sums[0].tolist())),power=dict(zip(['CR2','OLS','WCR2'],sums[1].tolist())),coverage=dict(zip(['CR2','OLS','WCR2'],coverage.tolist())),nonFiniteP=dict(zip(['CR2','OLS','WCR2'],failures.tolist())),coverageInversionMaxPDelta=coverage_max,seconds=time.time()-start)

def main():
 p=argparse.ArgumentParser();p.add_argument('--trials',type=int,default=2000);p.add_argument('--draws',type=int,default=499);p.add_argument('--out',default='pilot.jsonl');p.add_argument('--root',type=int,default=2026091301);p.add_argument('--action-root',type=int,default=2026091302);p.add_argument('--selected',default=None);args=p.parse_args()
 grid=list(itertools.product([8,20,80],['balanced','quarter'],['spread','reverse','outlier'],['normal','t3','skew'],[0.,.2]))
 if args.selected: grid=[tuple(x) for x in json.loads(pathlib.Path(args.selected).read_text())]
 with (P/args.out).open('w') as f:
  for i,c in enumerate(grid):
   r=cell(*c,args.trials,args.draws,args.root,args.action_root,i);f.write(json.dumps(r)+'\n');f.flush();print(json.dumps(r),flush=True)
if __name__=='__main__': main()
