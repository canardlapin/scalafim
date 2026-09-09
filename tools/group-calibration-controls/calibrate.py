"""Predeclared controlled nuisance screen and fresh confirmation."""
from controls import *
import argparse, pathlib, json, time
P=pathlib.Path(__file__).resolve().parent

def run_cell(c, index, trials, draws, roots):
    x=make_design(c); v=make_variance(c)+c['tau2'];d=prepare(x,v)
    rng=np.random.default_rng(np.random.SeedSequence([roots[0],index]))
    arng=np.random.default_rng(np.random.SeedSequence([roots[1],index]))
    totals={k:{m:0 for m in ALL} for k in ('null','power','coverage','nonFinite')}
    shift_delta=0.;start=time.time()
    for st in range(0,trials,20):
        count=min(20,trials-st)
        y=mean_vector(x)+errors(rng,(count,len(x)),c['error'])*np.sqrt(v)
        uniform=arng.random((count,draws,len(x)))
        actions={family:multipliers(uniform,family) for family in ('rad','mammen')}
        pnull=evaluate(y,d,actions)
        ppower=evaluate(y+.7*x[:,1],d,actions)
        ptrue=evaluate((y+.7*x[:,1])-.7*x[:,1],d,actions)
        for m in ALL:
            totals['null'][m]+=int(np.sum(pnull[m]<=.05))
            totals['power'][m]+=int(np.sum(ppower[m]<=.05))
            totals['coverage'][m]+=int(np.sum(ptrue[m]>.05))
            totals['nonFinite'][m]+=sum(int(np.sum(~np.isfinite(v[m]))) for v in (pnull,ppower,ptrue))
            shift_delta=max(shift_delta,float(np.max(np.abs(pnull[m]-ptrue[m]))))
    return dict(cell=c,index=index,studies=trials,draws=draws,roots=roots,results=totals,
                workingDf=d['information'],actualHc2VarianceDf=d['actualHc2Moments'][0],
                expectedHc2VarianceRatio=d['actualHc2Moments'][1],maxLeverage=float(d['h'].max()),
                maxShiftPDelta=shift_delta,seconds=time.time()-start)

def main():
    parser=argparse.ArgumentParser();parser.add_argument('stage',choices=['pilot','confirmation']);args=parser.parse_args()
    plan=json.loads((P/'protocol.json').read_text())
    grid=plan['pilot'] if args.stage=='pilot' else json.loads((P/'confirmation-cells.json').read_text())
    with (P/(args.stage+'.jsonl')).open('w') as out:
        for index,c in enumerate(grid):
            result=run_cell(c,index,plan[args.stage+'Studies'],plan[args.stage+'Draws'],plan[args.stage+'Roots'])
            out.write(json.dumps(result)+'\n');out.flush();print(json.dumps(result),flush=True)
if __name__=='__main__':main()
