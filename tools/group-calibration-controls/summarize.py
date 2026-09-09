"""Report all cells, intervals, and prespecified failure checks without hiding failures."""
import pathlib,json,hashlib,platform,sys
import numpy as np
from scipy.stats import beta
from controls import FEASIBLE
P=pathlib.Path(__file__).resolve().parent

def interval(k,n,alpha=.05):
    return [0. if k==0 else float(beta.ppf(alpha/2,k,n-k+1)),1. if k==n else float(beta.ppf(1-alpha/2,k+1,n-k))]

def main():
    summary={};total=0
    for stage,expected_count in [('pilot',41),('confirmation',8),('estimated',24),('feasible-weighting',24)]:
        rs=[json.loads(s) for s in (P/(stage+'.jsonl')).read_text().splitlines()];assert len(rs)==expected_count
        family=len(rs)*(len(FEASIBLE) if stage in ('pilot','confirmation') else len(rs[0].get('results',rs[0])['null']))
        rows=[];failures=[]
        for r in rs:
            n=r['studies'];counts=r.get('results',r);out={}
            assert not any(counts['nonFinite'].values())
            for m,k in counts['null'].items():
                assert k+counts['coverage'][m]==n
                lower=0. if k==0 else float(beta.ppf(.001/family,k,n-k+1))
                # Apply the originally specified family only to feasible methods.
                eligible=m in FEASIBLE if stage in ('pilot','confirmation') else True
                out[m]=dict(nullRate=k/n,nullCI95=interval(k,n),powerAtNominalAlpha=counts['power'][m]/n,powerCI95=interval(counts['power'][m],n),coverage=counts['coverage'][m]/n,
                            lowerFamily999=lower if eligible else None,definiteInflation=eligible and lower>.05)
                if eligible and lower>.05:failures.append(dict(cell=r['cell'],method=m,nullRate=k/n,lowerFamily999=lower))
            rows.append(dict(cell=r['cell'],index=r['index'],studies=n,methods=out))
        total+=sum(r['studies'] for r in rs)
        summary[stage]=dict(cells=len(rs),independentStudies=sum(r['studies'] for r in rs),familyComparisons=family,
                            familyLowerConfidence=.999,rows=rows,definiteInflation=failures)
    assert total==642000
    summary['totalIndependentStudies']=total
    summary['notes']=['Power uses alpha=.05 as implemented; inflated methods do not have comparable calibrated power.',
                      'Coverage is inversion at the known nonzero center, not a verified interval endpoint algorithm.',
                      'Estimated and feasible-stage family bounds use the same conservative reporting rule as confirmation, extended to all methods in each stage.',
                      'No family band establishes calibration in untested populations.']
    (P/'summary.json').write_text(json.dumps(summary,indent=2))
    print('studies',total)
    for stage in ['confirmation','estimated','feasible-weighting']:
        print(stage,'definite inflation',len(summary[stage]['definiteInflation']))
    for stage,method in [('estimated','native_PM_mKH'),('feasible-weighting','fixed_vhat_HC3_Satt')]:
        rows=summary[stage]['rows'];lo=min(rows,key=lambda r:r['methods'][method]['nullRate']);hi=max(rows,key=lambda r:r['methods'][method]['nullRate'])
        print(stage,method,'min',lo,'max',hi)
if __name__=='__main__':main()
