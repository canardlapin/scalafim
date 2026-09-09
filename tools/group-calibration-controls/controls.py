"""Specified scalar nuisance methods for calibration, not production inference."""
import os
for key in ('OPENBLAS_NUM_THREADS', 'VECLIB_MAXIMUM_THREADS', 'OMP_NUM_THREADS'):
    os.environ[key] = '1'
import numpy as np
from scipy.stats import t, norm

FEASIBLE = ('OLS_t', 'HC2_Satt', 'HC3_Satt', 'wild_HC2_rad', 'wild_HC2_mammen', 'wild_HC3_rad', 'wild_HC3_mammen')
CORRECTED = ('HC3_Satt', 'wild_HC2_mammen', 'wild_HC3_rad', 'wild_HC3_mammen')
ORACLES = ('CR2_true_target', 'oracle_WLS_z', 'oracle_WLS_t')
ALL = FEASIBLE + ORACLES

def make_design(c):
    n = c['n']
    count = n // 2 if c['group'] == 'balanced' else max(2, n // 4)
    group = (np.arange(n) < count).astype(float)
    x = [np.ones(n), group]
    if c['nuisance'] != 'none':
        z = np.cos((np.arange(n) + .5) * 2 * np.pi / n)
        if c['nuisance'] == 'spike':
            z[0] += 5
        x.append(z)
    return np.column_stack(x)

def make_variance(c):
    n = c['n']
    if c['variance'] == 'flat':
        v = np.full(n, .52)
    elif c['variance'] == 'dominant':
        v = np.full(n, .04)
        v[0] = 10.
    else:
        v = np.linspace(.04, 1, n)
        if c['variance'] == 'reverse':
            v = v[::-1]
    return v

def mean_vector(x, effect=0.):
    return x @ np.array([1.7, effect] + ([.8] if x.shape[1] == 3 else []))

def errors(rng, shape, family):
    if family == 'normal':
        return rng.normal(size=shape)
    if family == 't3':
        return rng.standard_t(3, size=shape) / np.sqrt(3)
    if family == 'skew':
        return (rng.lognormal(size=shape) - np.exp(.5)) / np.sqrt(np.e * (np.e - 1))
    raise ValueError(family)

def working_information(a, m, adjustment, phi):
    b = (m * (a * adjustment)**2) @ m
    whitened = np.sqrt(phi)[:, None] * b * np.sqrt(phi)[None, :]
    expectation = np.trace(whitened)
    df = expectation**2 / np.sum(whitened * whitened)
    return float(df), float(expectation / np.sum(a*a*phi))

def prepare(x, true_v):
    q = np.linalg.qr(x, mode='reduced')[0]
    inverse = np.linalg.pinv(x)
    a = inverse[1]
    h = np.sum(q*q, axis=1)
    m = np.eye(len(x)) - q @ q.T
    assert np.max(h) < 1 - 1e-12
    hc2 = 1 / np.sqrt(1-h)
    hc3 = 1 / (1-h)
    true_adj = np.sqrt(true_v / np.sum(m*m*true_v, axis=1))
    information = {
        'HC2_Satt': working_information(a, m, hc2, np.ones(len(x)))[0],
        'HC3_Satt': working_information(a, m, hc3, np.ones(len(x)))[0],
        'CR2_true_target': working_information(a, m, true_adj, true_v)[0],
    }
    xw = x / np.sqrt(true_v)[:, None]
    qw = np.linalg.qr(xw, mode='reduced')[0]
    aw = np.linalg.pinv(xw)[1]
    return dict(x=x, q=q, inverse=inverse, a=a, h=h, m=m, true_v=true_v,
                adjustment={'HC2_Satt': hc2, 'HC3_Satt': hc3, 'CR2_true_target': true_adj},
                information=information, qw=qw, aw=aw,
                actualHc2Moments=working_information(a,m,hc2,true_v))

def components(y, d):
    return y @ d['a'], y - (y @ d['q']) @ d['q'].T

def analytic(y, d):
    estimate, residual = components(y, d)
    n, p = d['x'].shape
    result = {}
    for method, adjustment in d['adjustment'].items():
        se = np.sqrt(np.sum((residual * (d['a'] * adjustment))**2, axis=1))
        result[method] = 2*t.sf(np.abs(estimate/se), d['information'][method])
    conventional_se = np.sqrt(np.sum(residual**2, axis=1) / (n-p) * np.sum(d['a']**2))
    result['OLS_t'] = 2*t.sf(np.abs(estimate/conventional_se), n-p)
    yw = y / np.sqrt(d['true_v'])
    estw = yw @ d['aw']
    rw = yw - (yw @ d['qw']) @ d['qw'].T
    variancew = np.sum(d['aw']**2)
    result['oracle_WLS_z'] = 2*norm.sf(np.abs(estw) / np.sqrt(variancew))
    result['oracle_WLS_t'] = 2*t.sf(np.abs(estw) / np.sqrt(np.sum(rw*rw,axis=1)/(n-p)*variancew), n-p)
    return result

def multipliers(uniform, family):
    if family == 'rad':
        return np.where(uniform < .5, -1., 1.)
    root5 = np.sqrt(5.)
    return np.where(uniform < (root5+1)/(2*root5), (1-root5)/2, (1+root5)/2)

def wild_statistics(y, d, weights, hc):
    """Full HCk statistic and refits from null-restricted adjusted residuals."""
    a, h, q = d['a'], d['h'], d['q']
    estimate, residual = components(y, d)
    aa = np.sum(a*a)
    restricted_residual = residual + estimate[:, None] * a / aa
    restricted_leverage = h - a*a/aa
    adjustment = (1-h)**(-hc/2)
    restricted_adjustment = (1-restricted_leverage)**(-hc/2)
    observed = estimate / np.sqrt(np.sum((residual*a*adjustment)**2,axis=1))
    # The restricted fitted mean is in the nuisance span. It cancels from the
    # tested coefficient and full residual, so the algebra omits it exactly.
    e = restricted_residual[:,None,:] * restricted_adjustment * weights
    estimates = e @ a
    residuals = e - (e @ q) @ q.T
    se = np.sqrt(np.sum((residuals*a*adjustment)**2,axis=-1))
    return observed, estimates/se

def evaluate(y, d, weights):
    result = analytic(y,d)
    for hc in (1,2):
        for family, actions in weights.items():
            observed, star = wild_statistics(y,d,actions,hc)
            count = np.sum(np.abs(star) >= np.abs(observed)[:,None]-1e-12,axis=1)
            result[f'wild_HC{hc+1}_{family}'] = (1+count)/(1+actions.shape[1])
            if not np.isfinite(star).all():
                raise FloatingPointError('non-finite bootstrap statistic')
    return result
