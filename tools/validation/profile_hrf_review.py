#!/usr/bin/env python3
"""Independent dense research probes for docs/plans/profile-hrf.md.

Run: OPENBLAS_NUM_THREADS=1 python3 tools/validation/profile_hrf_review.py
Requires NumPy and SciPy. This is an oracle, not a production backend or benchmark.
Second-order jets below store Taylor coefficients, including factorial divisors.
"""
import json
import math
import platform

import numpy as np
import scipy
from scipy.integrate import quad
from scipy.linalg import block_diag, cholesky_banded, cho_solve_banded, expm


def rel(a, b):
    return float(np.linalg.norm(a - b) / max(np.linalg.norm(b), 1e-15))


def logdet(a):
    sign, value = np.linalg.slogdet(a)
    assert sign > 0
    return value


def fd(fun, x, step=2e-4):
    eye = np.eye(len(x)) * step
    f = fun(x)
    g = np.array([(fun(x + e) - fun(x - e)) / (2 * step) for e in eye])
    h = np.empty((len(x), len(x)))
    for p, e in enumerate(eye):
        h[p, p] = (fun(x + e) - 2 * f + fun(x - e)) / step**2
        for q in range(p):
            z = eye[q]
            h[p, q] = h[q, p] = (
                fun(x + e + z) - fun(x + e - z)
                - fun(x - e + z) + fun(x - e - z)
            ) / (4 * step**2)
    return g, h


KEYS = ((0, 0), (1, 0), (0, 1), (2, 0), (1, 1), (0, 2))


class Jet:
    """Small independent second-order forward algebra for matrix expressions."""

    def __init__(self, value, parameter=None):
        value = np.asarray(value, dtype=float)
        self.a = {k: np.zeros_like(value) for k in KEYS}
        self.a[(0, 0)] = value
        if parameter is not None:
            self.a[KEYS[parameter + 1]] = np.ones_like(value)

    @staticmethod
    def lift(x):
        return x if isinstance(x, Jet) else Jet(x)

    def __add__(self, other):
        other = self.lift(other)
        out = Jet(self.a[(0, 0)] + other.a[(0, 0)])
        out.a = {k: self.a[k] + other.a[k] for k in KEYS}
        return out

    __radd__ = __add__

    def __neg__(self):
        return self * -1

    def __sub__(self, other):
        return self + -self.lift(other)

    def __rsub__(self, other):
        return self.lift(other) + -self

    def product(self, other, op):
        other = self.lift(other)
        out = Jet(op(self.a[(0, 0)], other.a[(0, 0)]))
        for k in KEYS:
            out.a[k] = sum(
                op(self.a[i], other.a[j])
                for i in KEYS for j in KEYS
                if (i[0] + j[0], i[1] + j[1]) == k
            )
        return out

    def __mul__(self, other):
        return self.product(other, np.multiply)

    __rmul__ = __mul__

    def __matmul__(self, other):
        return self.product(other, np.matmul)

    @property
    def T(self):
        out = Jet(self.a[(0, 0)].T)
        out.a = {k: self.a[k].T for k in KEYS}
        return out

    def power(self, exponent):
        v = self.a[(0, 0)]
        z = (self - v) * (1 / v)
        return (1 + z * exponent + z * z * (exponent * (exponent - 1) / 2)) * v**exponent

    def solve(self, rhs):
        rhs = self.lift(rhs)
        out = Jet(np.linalg.solve(self.a[(0, 0)], rhs.a[(0, 0)]))
        for k in KEYS[1:]:
            correction = sum(
                self.a[i] @ out.a[j]
                for i in KEYS[1:] for j in KEYS
                if (i[0] + j[0], i[1] + j[1]) == k
            )
            out.a[k] = np.linalg.solve(self.a[(0, 0)], rhs.a[k] - correction)
        return out


def gaussian_probe(rng):
    T, N, C, lam = 120, 24, 3, 2.3
    onsets = np.linspace(2, 100, N)
    lag = np.arange(T)[:, None] - onsets
    M = np.eye(C)[np.arange(N) % C]
    F = np.column_stack((np.ones(T), np.linspace(-1, 1, T)))
    Q = np.linalg.qr(F)[0]
    J = np.eye(T) - Q @ Q.T
    Mbar = M / np.sqrt(M.sum(axis=0))
    P = np.eye(N) - Mbar @ Mbar.T
    theta = np.array([5.0, math.log(1.8)])

    def kernels(th):
        tau, logwidth = th
        width = np.exp(logwidth)
        z = (lag - tau) / width
        # Unit full-Gaussian-area convention, then causal truncation: explicit.
        X = np.where(lag >= 0, np.exp(-z*z/2) / (width*np.sqrt(2*np.pi)), 0)
        lp = [z / width, z*z - 1]
        lpp = [[-np.ones_like(z)/width**2, -2*z/width], [-2*z/width, -2*z*z]]
        Xp = [X*l for l in lp]
        Xpq = [[X*(lp[p]*lp[q] + lpp[p][q]) for q in range(2)] for p in range(2)]
        return X, Xp, Xpq

    X, Xp, Xpq = kernels(theta)
    y = X @ rng.normal(size=N) + F @ np.array([0.2, -0.1]) + rng.normal(size=T)*0.07
    yy, A = J @ y, J @ X
    Ap = [J @ x for x in Xp]
    Apq = [[J @ x for x in row] for row in Xpq]
    G = A.T @ A + lam*P
    b = A.T @ yy
    a = np.linalg.solve(G, b)
    gp = [x.T @ A + A.T @ x for x in Ap]
    gpq = [[Apq[p][q].T@A + A.T@Apq[p][q] + Ap[p].T@Ap[q] + Ap[q].T@Ap[p]
            for q in range(2)] for p in range(2)]
    bp = [x.T @ yy for x in Ap]
    bpq = [[x.T @ yy for x in row] for row in Apq]
    u = [bp[p] - gp[p]@a for p in range(2)]
    da = [np.linalg.solve(G, x) for x in u]
    gradient = np.array([bp[p]@a - a@gp[p]@a/2 for p in range(2)])
    hessian = np.array([[bpq[p][q]@a - a@gpq[p][q]@a/2 + u[p]@da[q]
                         for q in range(2)] for p in range(2)])

    def score(th):
        aa = J @ kernels(th)[0]
        bb = aa.T@yy
        return bb @ np.linalg.solve(aa.T@aa + lam*P, bb)/2

    fg, fh = fd(score, theta)
    S = np.eye(T) + X@X.T/lam
    R = np.linalg.solve(S, np.eye(T))
    Z = np.column_stack((F, X@M))
    c = np.linalg.solve(Z.T@R@Z, Z.T@R@y)
    q = R@(y-Z@c)
    ar = M@c[2:] + X.T@q/lam
    E = y@R@y - (Z.T@R@y)@c
    K = np.eye(T) + X@P@X.T/lam
    Ki = np.linalg.solve(K, np.eye(T))
    ck = np.linalg.solve(Z.T@Ki@Z, Z.T@Ki@y)
    Ek = (y-Z@ck)@Ki@(y-Z@ck)
    U = X@Mbar / np.sqrt(lam)
    det_release = logdet(S) + logdet(np.eye(C) - U.T@R@U)
    Br = A.T@A + lam*np.eye(N)
    det_projected = logdet(Br) + logdet(Mbar.T@np.linalg.solve(Br, Mbar)) - (N-C)*np.log(lam)
    projected_cov = np.eye(T) + A@P@A.T/lam
    Xother = kernels(theta + np.array([1.5, 0.7]))[0]
    Aother = J@Xother
    other_det_gap = logdet(np.eye(T)+Aother@P@Aother.T/lam) - logdet(np.eye(T)+Xother@P@Xother.T/lam)
    Ufull = np.column_stack((X.T@Q, np.sqrt(lam)*Mbar))
    gram_identity = rel(X.T@X + lam*np.eye(N) - Ufull@Ufull.T, G)
    errors = []
    for factor in (1, 0.5, 0.25):
        delta = np.array([0.1, 0.04])*factor
        linear = a + sum(delta[p]*da[p] for p in range(2))
        bt = b + sum(delta[p]*bp[p] for p in range(2)) + sum(
            delta[p]*delta[z]*bpq[p][z]/2 for p in range(2) for z in range(2))
        gt = G + sum(delta[p]*gp[p] for p in range(2)) + sum(
            delta[p]*delta[z]*gpq[p][z]/2 for p in range(2) for z in range(2))
        corrected = linear + np.linalg.solve(G, bt-gt@linear)
        an = J@kernels(theta+delta)[0]
        gn, bn = an.T@an + lam*P, an.T@yy
        exact = np.linalg.solve(gn, bn)
        residual = bn-gn@corrected
        chol = np.linalg.cholesky(G)
        D = np.linalg.solve(chol, gn-G)
        D = np.linalg.solve(chol, D.T).T
        eta = max(abs(np.linalg.eigvalsh(D)))  # Pointwise, NOT a cell certificate.
        bound = residual@np.linalg.solve(G, residual)/(1-eta)
        actual = (exact-corrected)@gn@(exact-corrected)
        assert eta < 1 and actual <= bound*(1+1e-7)
        errors.append({'step_factor': factor, 'linear_relative': rel(linear, exact),
                       'corrected_relative': rel(corrected, exact), 'energy_bound_ratio': float(actual/bound)})
    # Truncate explicitly only for the independent structured-solve check.
    Xt = np.where((lag >= 0) & (lag <= 16), X, 0)
    ridge = Xt.T@Xt + lam*np.eye(N)
    ii, jj = np.nonzero(ridge)
    bandwidth = int(np.max(abs(ii-jj)))
    ab = np.zeros((bandwidth+1, N))
    for k in range(bandwidth+1):
        ab[k, :N-k] = np.diag(ridge, -k)
    band_solution = cho_solve_banded((cholesky_banded(ab, lower=True), True), b)
    out = dict(gram_identity_relative=gram_identity, ridge_release_relative=rel(ar, a),
               banded_ridge_relative=rel(band_solution, np.linalg.solve(ridge, b)),
               gradient_max_abs=float(max(abs(gradient-fg))), hessian_max_abs=float(np.max(abs(hessian-fh))),
               contrast_constraint=float(np.linalg.norm(M.T@(ar-M@c[2:]))),
               energy_profile_abs=float(abs(E-(yy@yy-b@a))), energy_covariance_abs=float(abs(E-Ek)),
               full_determinant_abs=float(abs(det_release-logdet(K))),
               projected_determinant_identity_abs=float(abs(det_projected-logdet(projected_cov))),
               wrong_projected_vs_full_logdet_difference=float(logdet(projected_cov)-logdet(K)),
               wrong_projected_vs_full_logdet_difference_other_shape=float(other_det_gap),
               amplitude_convergence=errors)
    assert max(gram_identity, out['ridge_release_relative'], out['banded_ridge_relative']) < 1e-10
    assert out['gradient_max_abs'] < 1e-6 and out['hessian_max_abs'] < 2e-5
    assert max(out['energy_profile_abs'], out['energy_covariance_abs'], out['full_determinant_abs'],
               out['projected_determinant_identity_abs'], out['contrast_constraint']) < 1e-9
    assert abs(out['wrong_projected_vs_full_logdet_difference']) > 1e-3
    assert abs(other_det_gap-out['wrong_projected_vs_full_logdet_difference']) > 1e-3
    assert errors[1]['corrected_relative']/errors[0]['corrected_relative'] < 0.2
    return out


def state_probe(rng):
    T, N, C, lam = 72, 18, 3, 1.7
    onsets = np.arange(N)*3
    M = np.eye(C)[np.arange(N) % C]
    F = np.column_stack((np.ones(T), np.linspace(-1, 1, T)))
    y = rng.normal(size=T)
    theta = np.array([0.65, 0.35])
    # Discrete two-cascade research family. One trial drives BOTH branches.
    # It is deliberately not claimed to equal a sampled continuous cascade.
    shift3, shift4 = np.eye(3, k=-1)*0.3, np.eye(4, k=-1)*0.25
    b = np.zeros((7, 1)); b[0, 0] = b[3, 0] = 1

    def setup(th, differentiated):
        A = Jet(block_diag(shift3, shift4)) + Jet(th[0], 0 if differentiated else None)*block_diag(np.eye(3), np.zeros((4, 4)))
        A = A + block_diag(np.zeros((3, 3)), np.eye(4)*0.8)
        c = Jet(np.array([[0., 0., 1., 0., 0., 0., 0.]])) + Jet(th[1], 1 if differentiated else None)*np.array([[0., 0., 0., 0., 0., 0., -1.]])
        return A, c

    A, c = setup(theta, True)
    P = Jet(np.zeros((7, 7)))
    m = Jet(np.zeros((7, T)))
    design_state = Jet(np.zeros((7, N)))
    Wrows, Xrows, nus = [], [], []
    for t in range(T):
        drive = (onsets == t).astype(float)[None, :]
        design_state = A@design_state + b@drive
        Xrows.append(c@design_state)
        P = A@P@A.T + (b@b.T)*(drive.sum()/lam)
        nu = c@P@c.T + 1
        gain = (P@c.T)*nu.power(-1)
        pred = A@m
        innovation = Jet(np.eye(T)[t:t+1]) - c@pred
        Wrows.append(innovation*nu.power(-0.5))
        m = pred + gain@innovation
        P = P - gain@(c@P)
        nus.append(float(nu.a[(0, 0)][0, 0]))

    def stack(rows):
        out = Jet(np.concatenate([r.a[(0, 0)] for r in rows]))
        out.a = {k: np.concatenate([r.a[k] for r in rows]) for k in KEYS}
        return out

    W, X = stack(Wrows), stack(Xrows)
    Z = Jet(np.column_stack((F, X.a[(0, 0)]@M)))
    for k in KEYS[1:]:
        Z.a[k] = np.column_stack((np.zeros_like(F), X.a[k]@M))
    v, B = W@Jet(y[:, None]), W@Z
    H, tt = B.T@B, B.T@v
    score = (tt.T@H.solve(tt) - v.T@v)*0.5
    gradient = np.array([score.a[k].item() for k in KEYS[1:3]])
    hessian = np.array([[2*score.a[(2, 0)].item(), score.a[(1, 1)].item()],
                        [score.a[(1, 1)].item(), 2*score.a[(0, 2)].item()]])

    def dense(th):
        aa, cc = setup(th, False)
        aa, cc = aa.a[(0, 0)], cc.a[(0, 0)]
        xx = np.zeros((T, N))
        for i, onset in enumerate(onsets):
            state = b.copy()
            for t in range(onset, T):
                xx[t, i] = (cc@state).item()
                state = aa@state
        rr = np.linalg.solve(np.eye(T)+xx@xx.T/lam, np.eye(T))
        zz = np.column_stack((F, xx@M))
        ccfit = np.linalg.solve(zz.T@rr@zz, zz.T@rr@y)
        residual = y-zz@ccfit
        return -residual@rr@residual/2

    fg0, fh0 = fd(dense, theta, step=2e-4)
    fg1, fh1 = fd(dense, theta, step=1e-4)
    fg, fh = (4*fg1-fg0)/3, (4*fh1-fh0)/3  # Independent Richardson check.
    xx, ww = X.a[(0, 0)], W.a[(0, 0)]
    S = np.eye(T)+xx@xx.T/lam
    Ri = np.linalg.solve(S, np.eye(T))
    zz = Z.a[(0, 0)]
    cf = H.solve(tt).a[(0, 0)][:, 0]
    q = ww.T@ww@(y-zz@cf)
    amplitude = M@cf[2:] + xx.T@q/lam
    proj = np.eye(T)-F@np.linalg.solve(F.T@F, F.T)
    contrast = np.eye(N)-M@np.linalg.solve(M.T@M, M.T)
    exact = np.linalg.solve(xx.T@proj@xx+lam*contrast, xx.T@proj@y)
    out = dict(inverse_relative=rel(ww.T@ww, Ri),
               innovation_logdet_abs=float(abs(sum(np.log(nus))-logdet(S))),
               score_abs=float(abs(score.a[(0, 0)].item()-dense(theta))),
               gradient_max_abs=float(max(abs(gradient-fg))),
               hessian_max_abs=float(np.max(abs(hessian-fh))),
               readout_relative=rel(amplitude, exact), largest_profile_solve=C+F.shape[1], states=7)
    assert max(out['inverse_relative'], out['readout_relative'], out['score_abs'], out['innovation_logdet_abs']) < 1e-9
    assert out['gradient_max_abs'] < 1e-6 and out['hessian_max_abs'] < 1e-5
    return out


def delay_counterexample():
    # Boundary-free to machine accuracy; pure translation still changes the sampled Gram.
    t = np.arange(-50, 51, dtype=float)
    def energy(delay):
        return np.sum(np.exp(-((t-delay)/0.35)**2))
    a, b = energy(0), energy(0.5)
    assert abs(a-b)/a > 0.5
    return dict(integer_grid_energy=float(a), half_sample_delay_energy=float(b), relative_change=float(abs(a-b)/a))


def half_cosine_join():
    # Sample exactly at the nominal peak; move the rise duration through it.
    # The two one-sided second derivatives disagree, although the first is zero.
    t, h1, h3, f2 = 6.0, 1.0, 7.0, -0.2
    def sample(h2):
        if t <= h1+h2:
            return (1-np.cos(np.pi*(t-h1)/h2))/2
        return 1+(f2-1)*(1-np.cos(np.pi*(t-h1-h2)/h3))/2
    e = 1e-4
    right = (sample(5+2*e)-2*sample(5+e)+sample(5))/e**2
    left = (sample(5-2*e)-2*sample(5-e)+sample(5))/e**2
    assert abs(right-left) > 0.05
    return dict(left_second_derivative=float(left), right_second_derivative=float(right))


def continuous_cascade_probe():
    """Proposed continuous family: unit-area gamma components, common impulse."""
    kp, ku, rho = 0.4, 0.2, 0.35
    generator = block_diag(kp*(np.eye(3, k=-1)-np.eye(3)),
                           ku*(np.eye(4, k=-1)-np.eye(4)))
    injection = np.array([kp, 0., 0., ku, 0., 0., 0.])
    observation = np.array([0., 0., 1., 0., 0., 0., -rho])

    def gamma_component(t, n, rate):
        return rate**n * t**(n-1)*np.exp(-rate*t)/math.factorial(n-1)

    def kernel(t):
        return gamma_component(t, 3, kp)-rho*gamma_component(t, 4, ku)

    def transition(dt):
        blocks = []
        for n, rate in ((3, kp), (4, ku)):
            shift = np.eye(n, k=-1)
            blocks.append(np.exp(-rate*dt)*sum(
                (rate*dt)**j/math.factorial(j)*np.linalg.matrix_power(shift, j)
                for j in range(n)))
        return block_diag(*blocks)

    times = (0., 0.01, 0.27, 1.3, 5., 12.7, 30., 85.)
    transition_error = max(np.max(abs(transition(t)-expm(generator*t))) for t in times)
    kernel_error = max(abs(observation@transition(t)@injection-kernel(t)) for t in times)
    semigroup_error = np.max(abs(transition(0.37)@transition(1.13)-transition(1.5)))
    signed_area = quad(kernel, 0, np.inf)[0]
    # Arbitrary impulse times: compare event-driven covariance to full columns.
    observations = np.arange(0., 40., 1.3)
    events = np.array([0.27, 1.71, 1.71, 6.03, 18.91])
    lam = 2.1
    X = np.array([[kernel(t-e) if t >= e else 0. for e in events] for t in observations])
    cov = np.zeros((7, 7))
    inverse_mean = np.zeros((7, len(observations)))
    rows = []
    previous = 0.
    for index, t in enumerate(observations):
        A = transition(t-previous)
        cov = A@cov@A.T
        for event in events[(events > previous) & (events <= t)]:
            v = transition(t-event)@injection
            cov += np.outer(v, v)/lam
        nu = 1+observation@cov@observation
        gain = cov@observation/nu
        prediction = A@inverse_mean
        innovation = np.eye(len(observations))[index]-observation@prediction
        rows.append(innovation/np.sqrt(nu))
        inverse_mean = prediction+np.outer(gain, innovation)
        cov -= np.outer(gain, observation@cov)
        previous = t
    W = np.array(rows)
    R = np.linalg.solve(np.eye(len(observations))+X@X.T/lam, np.eye(len(observations)))
    inverse_error = rel(W.T@W, R)
    assert max(transition_error, kernel_error, semigroup_error, abs(signed_area-(1-rho)), inverse_error) < 1e-11
    return dict(transition_max_abs=float(transition_error), kernel_max_abs=float(kernel_error),
                semigroup_max_abs=float(semigroup_error), signed_area=float(signed_area),
                off_grid_inverse_relative=inverse_error)


if __name__ == '__main__':
    rng = np.random.default_rng(20260909)
    result = dict(seed=20260909, python=platform.python_version(), numpy=np.__version__, scipy=scipy.__version__,
                  gaussian=gaussian_probe(rng), finite_state=state_probe(rng),
                  fractional_delay=delay_counterexample(), half_cosine_join=half_cosine_join(),
                  continuous_cascade=continuous_cascade_probe())
    print(json.dumps(result, indent=2, allow_nan=False))
