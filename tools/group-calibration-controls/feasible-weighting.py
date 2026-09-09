"""Exploratory fixed inverse-variance HC3; no true tau supplied to candidate."""
from estimated import plugin
from controls import *
import pathlib,json,itertools,time
P=pathlib.Path(__file__).resolve().parent
methods=('oracle_WLS_t','unweighted_HC3_Satt','fixed_vhat_GLS_t','fixed_vhat_HC3_Satt')

def main():
    fixtures=[]
    with (P/'feasible-weighting.jsonl').open('w') as out:
        for index,(n,profile,tau,df) in enumerate(itertools.product([20,80],['spread','reverse'],[0.,.2],[0,8,40])):
            c=dict(n=n,group='quarter',nuisance='smooth',variance=profile,error='normal',tau2=tau,firstLevelDf=df)
            x=make_design(c);v=make_variance(c);d=prepare(x,v+tau);rng=np.random.default_rng(np.random.SeedSequence([2026091901,index]));vrng=np.random.default_rng(np.random.SeedSequence([2026091902,index]));N=10000
            y=mean_vector(x)+rng.normal(size=(N,n))*np.sqrt(v)+rng.normal(size=(N,n))*np.sqrt(tau)
            vhat=np.broadcast_to(v,(N,n)).copy() if df==0 else v*vrng.chisquare(df,size=(N,n))/df
            totals={kind:{m:0 for m in methods} for kind in ['null','power','coverage','nonFinite']};dfValues=[];maxShift=0.;start=time.time()
            for st in range(0,N,64):
                end=min(st+64,N);ys=y[st:end];variances=vhat[st:end];nullp=None
                for kind,response in [('null',ys),('power',ys+.7*x[:,1]),('coverage',(ys+.7*x[:,1])-.7*x[:,1])]:
                    # Candidate receives ONLY first-level estimated variances.
                    pp,dof=plugin(response,x,variances);pa=analytic(response,d)
                    ps=dict(oracle_WLS_t=pa['oracle_WLS_t'],unweighted_HC3_Satt=pa['HC3_Satt'],fixed_vhat_GLS_t=pp['plugin_GLS_t'],fixed_vhat_HC3_Satt=pp['plugin_HC3_Satt'])
                    if kind=='null':nullp=ps;dfValues.extend(dof.tolist())
                    for m,p in ps.items():
                        totals[kind][m]+=int(np.sum(p>.05 if kind=='coverage' else p<=.05))
                        totals['nonFinite'][m]+=int(np.sum(~np.isfinite(p)))
                        if kind=='coverage':maxShift=max(maxShift,float(np.max(np.abs(p-nullp[m]))))
                    if st==0 and kind=='null':
                        for study in [0,1,2]:fixtures.append(dict(index=index,study=study,cell=c,X=x.tolist(),y=ys[study].tolist(),vhat=variances[study].tolist(),p=float(ps['fixed_vhat_HC3_Satt'][study]),df=float(dof[study])))
            r=dict(index=index,cell=c,studies=N,roots=[2026091901,2026091902],results=totals,dfQuantiles=np.quantile(dfValues,[0,.05,.5,.95,1]).tolist(),maxShiftPDelta=maxShift,seconds=time.time()-start)
            out.write(json.dumps(r)+'\n');out.flush();print(json.dumps(r),flush=True)
    (P/'feasible-reference-input.json').write_text(json.dumps(fixtures,indent=2))
if __name__=='__main__':main()
