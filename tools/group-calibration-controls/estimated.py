"""Gaussian first-level SE uncertainty, paired with native PM/mKH execution."""
from controls import *
import pathlib,json,itertools,struct,subprocess,hashlib,time,os
P=pathlib.Path(__file__).resolve().parent
JAVA=os.environ.get('GROUP_CALIBRATION_JAVA','java')

def plugin(y,x,variance):
    """Plug-in GLS t and HC3/Satt, conditional on the supplied whitening."""
    xw=x[None,:,:]/np.sqrt(variance)[:,:,None]
    q=np.linalg.qr(xw,mode='reduced')[0]
    inverse=np.linalg.pinv(xw)
    a=inverse[:,1,:];h=np.sum(q*q,axis=2)
    yw=y/np.sqrt(variance)
    coef=np.einsum('bpn,bn->bp',inverse,yw)
    residual=yw-np.einsum('bnp,bp->bn',q,np.einsum('bnp,bn->bp',q,yw))
    n,p=x.shape
    conventional=np.sqrt(np.sum(residual*residual,axis=1)/(n-p)*np.sum(a*a,axis=1))
    stat=coef[:,1]/conventional
    hc3se=np.sqrt(np.sum((residual*a/(1-h))**2,axis=1))
    # Stable nonnegative trace contraction; no cancellation in the low-rank identity.
    normalized_a=a/np.sqrt(np.sum(a*a,axis=1))[:,None]
    diagonal=(normalized_a/(1-h))**2
    m=np.eye(n)[None,:,:]-q@np.swapaxes(q,1,2)
    trace=np.sum(diagonal*(1-h),axis=1)
    trace2=np.sum(m*m*diagonal[:,:,None]*diagonal[:,None,:],axis=(1,2))
    df=trace*trace/trace2
    return {'plugin_GLS_t':2*t.sf(np.abs(stat),n-p),'plugin_HC3_Satt':2*t.sf(np.abs(coef[:,1]/hc3se),df)},df

def write_native(path,x,y,v):
    with path.open('wb') as out:
        out.write(struct.pack('>iiii',0x47504331,x.shape[0],x.shape[1],y.shape[0]))
        for a in [x,y.T,v.T]:out.write(np.asarray(a,dtype='>f8').tobytes(order='C'))

def read_native(path):
    with path.open('rb') as inp:
        magic,count=struct.unpack('>ii',inp.read(8));assert magic==0x47505231
        values=np.frombuffer(inp.read(),dtype='>f8').reshape(count,5).astype(float)
        return values

def main():
    plan=json.loads((P/'protocol.json').read_text());n_studies=plan['estimatedStudies'];root,vr=plan['estimatedRoots'];cp=(P/'runtime-classpath.txt').read_text().strip()
    cases=list(itertools.product([20,80],['spread','reverse'],[0.,.2],[0,8,40]));fixtures=[]
    with (P/'estimated.jsonl').open('w') as output:
        for index,(n,profile,tau,df) in enumerate(cases):
            start=time.time();c=dict(n=n,group='quarter',nuisance='smooth',variance=profile,error='normal',tau2=tau,firstLevelDf=df)
            x=make_design(c);v=make_variance(c);d=prepare(x,v+tau)
            rng=np.random.default_rng(np.random.SeedSequence([root,index]));variance_rng=np.random.default_rng(np.random.SeedSequence([vr,index]))
            # Real two-level generator: Gaussian sampling error plus independent random effect.
            y=mean_vector(x)+rng.normal(size=(n_studies,n))*np.sqrt(v)+rng.normal(size=(n_studies,n))*np.sqrt(tau)
            vhat=np.broadcast_to(v,(n_studies,n)).copy() if df==0 else v*variance_rng.chisquare(df,size=(n_studies,n))/df
            ps={key:np.empty(n_studies) for key in ['oracle_WLS_z','oracle_WLS_t','plugin_GLS_t','plugin_HC3_Satt','unweighted_HC3_Satt']}
            powers={key:np.empty(n_studies) for key in ps};coverage={key:np.empty(n_studies) for key in ps};dfs=np.empty(n_studies)
            for st in range(0,n_studies,64):
                end=min(st+64,n_studies);ys=y[st:end];total=vhat[st:end]+tau
                for responses,destination,record in [(ys,ps,True),(ys+.7*x[:,1],powers,False),((ys+.7*x[:,1])-.7*x[:,1],coverage,False)]:
                    pplug,dof=plugin(responses,x,total);pa=analytic(responses,d)
                    if record:dfs[st:end]=dof
                    for key in ['oracle_WLS_z','oracle_WLS_t']:destination[key][st:end]=pa[key]
                    destination['unweighted_HC3_Satt'][st:end]=pa['HC3_Satt']
                    for key in pplug:destination[key][st:end]=pplug[key]
            native_in=P/'native-input.bin';native_out=P/f'native-result-{index:02d}.bin'
            write_native(native_in,x,y,vhat);input_hash=hashlib.sha256(native_in.read_bytes()).hexdigest()
            command=[JAVA,'-Xmx1g','-XX:ActiveProcessorCount=3','-cp',cp,'scalafim.fmri.group.NativePmCalibration',str(native_in),str(native_out)]
            proc=subprocess.run(command,text=True,capture_output=True);proc.check_returncode();native=read_native(native_out)
            (P/f'native-process-{index:02d}.json').write_text(json.dumps(dict(command=command,stdout=proc.stdout,stderr=proc.stderr,inputSha256=input_hash,outputSha256=hashlib.sha256(native_out.read_bytes()).hexdigest()),indent=2))
            # Refit a shifted design signal through the native API; test the known
            # nonzero value using the returned estimate/SE, preserving PM tau.
            write_native(native_in,x,y+.7*x[:,1],vhat)
            shift_out=P/f'native-shift-{index:02d}.bin';shift_proc=subprocess.run(command[:-1]+[str(shift_out)],text=True,capture_output=True);shift_proc.check_returncode();shift=read_native(shift_out)
            ps['native_PM_mKH']=native[:,3]
            powers['native_PM_mKH']=shift[:,3]
            coverage['native_PM_mKH']=2*t.sf(np.abs((shift[:,0]-.7)/shift[:,1]),n-3)
            comparisons={'estimateShiftError':float(np.max(np.abs(shift[:,0]-native[:,0]-.7))),'seShiftError':float(np.max(np.abs(shift[:,1]-native[:,1]))),'tauShiftError':float(np.max(np.abs(shift[:,4]-native[:,4])))}
            assert comparisons['estimateShiftError']<1e-8 and comparisons['seShiftError']<1e-8 and comparisons['tauShiftError']<1e-8
            result=dict(cell=c,index=index,studies=n_studies,roots=[root,vr],inputSha256=input_hash,
                null={m:int(np.sum(a<=.05)) for m,a in ps.items()},power={m:int(np.sum(a<=.05)) for m,a in powers.items()},
                coverage={m:int(np.sum(a>.05)) for m,a in coverage.items()},nonFinite={m:int(np.sum(~np.isfinite(a))) for m,a in ps.items()},
                pluginDfQuantiles=np.quantile(dfs,[0,.05,.5,.95,1]).tolist(),nativeShiftChecks=comparisons,
                meanNativeTau=float(native[:,4].mean()),nativeTauAtZero=int(np.sum(native[:,4]==0.)),seconds=time.time()-start)
            np.savez_compressed(P/f'estimated-pvalues-{index:02d}.npz',**ps)
            # Explicit inputs for independent R parity; no seed alignment assumed.
            for study in [0,1,2]:
                fixtures.append(dict(cell=c,index=index,study=study,X=x.tolist(),y=y[study].tolist(),vhat=vhat[study].tolist(),trueV=(v+tau).tolist(),native=dict(zip(['estimate','se','t','p','tau2'],native[study].tolist())),plugin={m:float(ps[m][study]) for m in ['plugin_GLS_t','plugin_HC3_Satt']},pluginDf=float(dfs[study])))
            output.write(json.dumps(result)+'\n');output.flush();print(json.dumps(result),flush=True)
    (P/'estimated-reference-input.json').write_text(json.dumps(fixtures,indent=2))

if __name__=='__main__':main()
