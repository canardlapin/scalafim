"""Explicit independent R fixtures and invariants for feasible weighting."""
import unittest,json,pathlib
import numpy as np
from estimated import plugin,read_native
P=pathlib.Path(__file__).resolve().parent

class EstimatedTests(unittest.TestCase):
    def test_native_PM_and_privileged_plugin_match_independent_R(self):
        fixtures=json.loads((P/'estimated-reference-input.json').read_text())
        references=json.loads((P/'estimated-reference.json').read_text())['results']
        errors={k:0. for k in ['estimate','se','t','p','tau2','plugin_GLS_t','plugin_HC3_Satt','pluginDf']}
        self.assertEqual(len(fixtures),72)
        for f,r in zip(fixtures,references,strict=True):
            self.assertEqual((f['index'],f['study']),(r['index'],r['study']))
            actual={**f['native'],**f['plugin'],'pluginDf':f['pluginDf']}
            p,df=plugin(np.array([f['y']]),np.array(f['X']),np.array([f['vhat']])+f['cell']['tau2'])
            for key in p:self.assertAlmostEqual(p[key][0],actual[key],places=12)
            self.assertAlmostEqual(df[0],actual['pluginDf'],places=10)
            for key in errors:errors[key]=max(errors[key],abs(actual[key]-r[key]))
        self.assertLess(max(errors.values()),1e-7)
        (P/'estimated-parity-summary.json').write_text(json.dumps(dict(fixtures=len(fixtures),maxErrors=errors),indent=2))

    def test_feasible_HC3_matches_independent_R_without_true_tau(self):
        fixtures=json.loads((P/'feasible-reference-input.json').read_text())
        references=json.loads((P/'feasible-reference.json').read_text())['results']
        errors={'p':0.,'df':0.};self.assertEqual(len(fixtures),72)
        for f,r in zip(fixtures,references,strict=True):
            self.assertEqual((f['index'],f['study']),(r['index'],r['study']))
            p,df=plugin(np.array([f['y']]),np.array(f['X']),np.array([f['vhat']]))
            self.assertAlmostEqual(p['plugin_HC3_Satt'][0],f['p'],places=12)
            self.assertAlmostEqual(df[0],f['df'],places=10)
            errors['p']=max(errors['p'],abs(f['p']-r['p']))
            errors['df']=max(errors['df'],abs(f['df']-r['df']))
        self.assertLess(errors['p'],1e-10);self.assertLess(errors['df'],1e-9)
        (P/'feasible-parity-summary.json').write_text(json.dumps(dict(fixtures=len(fixtures),maxErrors=errors),indent=2))

    def test_weighted_units_reordering_and_nuisance_coding(self):
        f=json.loads((P/'feasible-reference-input.json').read_text())[4]
        x=np.array(f['X']);y=np.array([f['y']]);v=np.array([f['vhat']]);p,df=plugin(y,x,v)
        order=np.arange(len(x))[::-1];xp=x[order].copy();xp[:,2]=3+7*xp[:,2]
        pp,dp=plugin(-13*y[:,order],xp,169*v[:,order])
        for k in p:np.testing.assert_allclose(p[k],pp[k],rtol=1e-11,atol=1e-12)
        np.testing.assert_allclose(df,dp,rtol=1e-11)

    def test_inverse_chisquare_moments_by_independent_quadrature(self):
        # An explanatory intercept-only asymptotic control, NOT a PM correction.
        from scipy.integrate import quad
        from scipy.stats import chi2,norm
        rows=[]
        for nu in [8,40]:
            ew=quad(lambda q:nu/q*chi2.pdf(q,nu),0,np.inf,epsabs=1e-10)[0]
            ew2=quad(lambda q:(nu/q)**2*chi2.pdf(q,nu),0,np.inf,epsabs=1e-10)[0]
            self.assertAlmostEqual(ew,nu/(nu-2),places=10)
            self.assertAlmostEqual(ew2,nu**2/((nu-2)*(nu-4)),places=10)
            ratio=(nu-2)/(nu-4)
            self.assertAlmostEqual(ew2/ew**2,ratio,places=10)
            rows.append(dict(df=nu,varianceUnderestimateFactor=ratio,limitingTypeI=2*norm.sf(norm.isf(.025)/np.sqrt(ratio))))
        (P/'noisy-weighting-analytic.json').write_text(json.dumps(rows,indent=2))

if __name__=='__main__':unittest.main()
