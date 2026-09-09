"""Audit every estimated-stage input and all null/power/coverage counts."""
from estimated import *
import tempfile

def main():
    rows=[json.loads(s) for s in (P/'estimated.jsonl').read_text().splitlines()]
    report=[]
    for r in rows:
        index=r['index'];c=r['cell'];N=r['studies'];n=c['n'];tau=c['tau2'];df=c['firstLevelDf'];root,vr=r['roots']
        x=make_design(c);v=make_variance(c);d=prepare(x,v+tau)
        rng=np.random.default_rng(np.random.SeedSequence([root,index]));vrng=np.random.default_rng(np.random.SeedSequence([vr,index]))
        y=mean_vector(x)+rng.normal(size=(N,n))*np.sqrt(v)+rng.normal(size=(N,n))*np.sqrt(tau)
        vh=np.broadcast_to(v,(N,n)).copy() if df==0 else v*vrng.chisquare(df,size=(N,n))/df
        with tempfile.TemporaryDirectory() as td:
            path=pathlib.Path(td)/'input.bin';write_native(path,x,y,vh)
            assert hashlib.sha256(path.read_bytes()).hexdigest()==r['inputSha256']
        native=read_native(P/f'native-result-{index:02d}.bin');shift=read_native(P/f'native-shift-{index:02d}.bin')
        assert np.all(np.isfinite(native)) and np.all(np.isfinite(shift))
        counts={k:{m:0 for m in r['null']} for k in ['null','power','coverage']}
        max_null_error=0.
        with np.load(P/f'estimated-pvalues-{index:02d}.npz') as saved:
            for st in range(0,N,64):
                end=min(N,st+64);ys=y[st:end];variance=vh[st:end]+tau
                for kind,response in [('null',ys),('power',ys+.7*x[:,1]),('coverage',(ys+.7*x[:,1])-.7*x[:,1])]:
                    pp,_=plugin(response,x,variance);pa=analytic(response,d)
                    p={**pp,'unweighted_HC3_Satt':pa['HC3_Satt'],**{k:pa[k] for k in ['oracle_WLS_z','oracle_WLS_t']}}
                    p['native_PM_mKH']=(native[st:end,3] if kind=='null' else shift[st:end,3] if kind=='power' else 2*t.sf(np.abs((shift[st:end,0]-.7)/shift[st:end,1]),n-3))
                    for m,pv in p.items():
                        assert np.all(np.isfinite(pv)) and np.all((pv>=0)&(pv<=1))
                        counts[kind][m]+=int(np.sum(pv>.05 if kind=='coverage' else pv<=.05))
                        if kind=='null':max_null_error=max(max_null_error,float(np.max(np.abs(pv-saved[m][st:end]))))
        assert all(counts[k]==r[k] for k in counts)
        assert max_null_error<1e-11
        report.append(dict(index=index,inputHashVerified=True,allNullPowerCoverageFinite=True,allCountsReproduced=True,maxNullPDelta=max_null_error,nativeOutputSha256=hashlib.sha256((P/f'native-result-{index:02d}.bin').read_bytes()).hexdigest(),nativeShiftSha256=hashlib.sha256((P/f'native-shift-{index:02d}.bin').read_bytes()).hexdigest()))
        print(index,'passed',flush=True)
    (P/'estimated-replay-audit.json').write_text(json.dumps(report,indent=2))
if __name__=='__main__':main()
