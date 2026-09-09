"""Independent-reference and contract regressions for the calibration tools."""
import unittest,json,pathlib
from controls import *
P=pathlib.Path(__file__).resolve().parent

class ControlsTests(unittest.TestCase):
    def test_independent_R_all_covariance_targets_and_explicit_bootstrap_actions(self):
        inputs=json.loads((P/'reference-input.json').read_text())
        expected=json.loads((P/'reference.json').read_text())['results']
        errors={m:0. for m in ALL};stat_error=0.;df_error=0.;variance_error=0.
        for case,ref in zip(inputs,expected,strict=True):
            x=np.array(case['X']);v=np.array(case['v']);y=np.array([case['y']]);d=prepare(x,v)
            actions={f:np.array([a]) for f,a in case['actions'].items()}
            result=evaluate(y,d,actions)
            for m in ALL:
                errors[m]=max(errors[m],abs(float(result[m][0])-ref['tests'][m]['p']))
            for m,adj in d['adjustment'].items():
                df_error=max(df_error,abs(d['information'][m]-ref['tests'][m]['df']))
                residual=components(y,d)[1]
                variance_error=max(variance_error,abs(float(np.sum((residual*d['a']*adj)**2))-ref['tests'][m]['variance']))
            for hc in [1,2]:
                for f,a in actions.items():
                    m=f'wild_HC{hc+1}_{f}';obs,star=wild_statistics(y,d,a,hc)
                    stat_error=max(stat_error,float(np.max(np.abs(star[0]-ref['tests'][m]['statistics']))),abs(float(obs[0])-ref['tests'][m]['observed']))
        self.assertLess(max(errors.values()),1e-10)
        self.assertLess(stat_error,1e-9)
        self.assertLess(df_error,1e-9)
        self.assertLess(variance_error,1e-10)
        (P/'parity-summary.json').write_text(json.dumps(dict(cases=len(inputs),pErrors=errors,maxStatisticError=stat_error,maxDfError=df_error,maxVarianceError=variance_error),indent=2))

    def test_Mammen_moments(self):
        s=np.sqrt(5.);values=np.array([(1-s)/2,(1+s)/2]);p=np.array([(s+1)/(2*s),(s-1)/(2*s)])
        for power,expected in [(1,0.),(2,1.),(3,1.)]:self.assertAlmostEqual(float(np.sum(p*values**power)),expected,places=13)

    def test_equivalent_units_covariate_coding_and_subject_reordering(self):
        c=json.loads((P/'protocol.json').read_text())['pilot'][0];x=make_design(c);v=make_variance(c);d=prepare(x,v)
        rng=np.random.default_rng(882713);y=mean_vector(x)+rng.normal(size=(10,len(x)))*np.sqrt(v)
        u=rng.random((10,31,len(x)));actions={f:multipliers(u,f) for f in ['rad','mammen']};original=evaluate(y,d,actions)
        order=np.roll(np.arange(len(x)),7);xp=x[order].copy();xp[:,2]=10+2*xp[:,2];dp=prepare(xp,v[order])
        perm=evaluate(y[:,order],dp,{f:a[:,:,order] for f,a in actions.items()})
        scaled=evaluate(-3*y,prepare(x,9*v),actions)
        shifted=evaluate((y+.7*x[:,1])-.7*x[:,1],d,actions)
        for m in ALL:
            for actual in [perm,scaled,shifted]:np.testing.assert_allclose(actual[m],original[m],atol=1e-10,rtol=1e-10)

    def test_flat_variance_oracle_t_matches_ordinary_t(self):
        c=dict(n=20,group='quarter',nuisance='smooth',variance='flat',error='normal',tau2=0.)
        x=make_design(c);d=prepare(x,make_variance(c));y=mean_vector(x)+np.random.default_rng(71281).normal(size=(100,20))
        p=analytic(y,d);np.testing.assert_allclose(p['OLS_t'],p['oracle_WLS_t'],atol=1e-12)
        self.assertAlmostEqual(d['information']['HC2_Satt'],d['information']['CR2_true_target'],places=11)

if __name__=='__main__':unittest.main()
