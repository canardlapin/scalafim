"""PHRF comparative-evaluation v0 gate: TX distance check (protocol v3.1, sections 5 and 8).

Independent of PHRF/scalafim code.  The TX kernel comes from the generator package
(generator/phrf_gen/kernels.py, `il`), the single frozen TX definition.  The two PHRF chart
families are implemented here from their definitions:

Gaussian chart C0      h(t) = exp(-(t-tau)^2/(2 sigma^2)), t>=0; tau in [3,8] s, sigma in [0.8,3] s.
    modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/family/GaussianFamily.scala:15-16 (definition),
    :40-45 (evalInto), :100-101 (DefaultChart: tau 3..8, logSd log0.8..log3).
Cascade34 (PHRF-04)    h(t) = g3(t;kP) - rho g4(t;kU), unit-area Erlang-3 / Erlang-4 densities
    (g_n(t;k) = k (kt)^(n-1) e^{-kt} / (n-1)!), t>=0, kU = kP * q.
    .../hrf/Cascade34.scala:27-29 (definition, doc), :43-55 (erlang), :62-63 (value).
    Chart (logKappaP, logitRateRatio, rho): .../hrf/family/Cascade34Family.scala:5 (doc),
    :23-24 (parameters), :95-105 (DefaultChart: kappaP 0.25..1 /s, q=kU/kP 0.1..0.8,
    rho 0..0.8; logit axis log(.1/.9)..log(.8/.2)).
CAN = fixed-latency canonical SPM: gamma(6)-gamma(16)/6 (docs/audits/spmg-correction.md:3-5).
INF3 = span{CAN, d/dt CAN, dispersion derivative (positive component only, d=1 vs 1.01,
    mean-shape 6/d, scale d; undershoot cancels)}: docs/audits/spmg-correction.md:11-14.
Every kernel is causal, sampled on a 0.1 s grid over [0,48] s (481 points).
"""
from __future__ import annotations

import hashlib
import json
import os
import sys

import numpy as np
from scipy.optimize import minimize
from scipy.stats import gamma, qmc

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "generator"))
from phrf_gen import kernels as K  # noqa: E402  (TX = IL, frozen there)

T = np.arange(0, 481) * 0.1
N_SOBOL = 1024
GRID_N = 41
N_REFINE = 5
THRESHOLD = 0.05


# ---- chart shapes (independent implementations) --------------------------------------------

def gauss_shape(tau, sigma):
    return np.exp(-0.5 * ((T - tau) / sigma) ** 2)


def erlang(n, k, t):
    x = k * t
    return k * x ** (n - 1) * np.exp(-x) / np.prod(np.arange(1, n))


def cascade_shape(kp, ku, rho):
    return erlang(3, kp, T) - rho * erlang(4, ku, T)


def logit(p):
    return np.log(p / (1 - p))


def expit(x):
    return 1 / (1 + np.exp(-x))


GAUSS_LO = np.array([3.0, np.log(0.8)])
GAUSS_HI = np.array([8.0, np.log(3.0)])
CAS_LO = np.array([np.log(0.25), logit(0.1), 0.0])
CAS_HI = np.array([np.log(1.0), logit(0.8), 0.8])


def gauss_from_chart(z):
    return gauss_shape(z[0], np.exp(z[1]))


def cascade_from_chart(z):
    kp = np.exp(z[0])
    return cascade_shape(kp, kp * expit(z[1]), z[2])


# ---- distance machinery ---------------------------------------------------------------------

def rel_dist(h, g):
    """min_a ||h - a g|| / ||h||, a closed-form (sign free)."""
    hh = h @ h
    gg = g @ g
    if gg == 0:
        return 1.0
    return float(np.sqrt(max(0.0, 1.0 - (h @ g) ** 2 / (gg * hh))))


def fit_chart(h, shape_fn, lo, hi, G, Gnorm2):
    """41-per-axis grid (precomputed rows G) then Nelder-Mead from the best 5 grid points."""
    hh = h @ h
    proj = G @ h
    d2 = 1.0 - proj**2 / (Gnorm2 * hh)
    order = np.argsort(d2)[:N_REFINE]
    best_d = float(np.sqrt(max(0.0, d2[order[0]])))
    best_z = order[0]
    return order, best_d


def refine(h, shape_fn, lo, hi, z0):
    f = lambda z: rel_dist(h, shape_fn(z)) ** 2  # noqa: E731
    r = minimize(f, z0, method="Nelder-Mead", bounds=list(zip(lo, hi)),
                 options=dict(xatol=1e-7, fatol=1e-14, maxiter=4000, maxfev=8000))
    return float(np.sqrt(max(0.0, r.fun))), r.x


def build_grid(shape_fn, lo, hi):
    axes = [np.linspace(a, b, GRID_N) for a, b in zip(lo, hi)]
    mesh = np.stack(np.meshgrid(*axes, indexing="ij"), axis=-1).reshape(-1, len(lo))
    G = np.stack([shape_fn(z) for z in mesh])
    return mesh, G, np.einsum("ij,ij->i", G, G)


def chart_distance(h, shape_fn, lo, hi, mesh, G, G2):
    hh = h @ h
    with np.errstate(invalid="ignore", divide="ignore"):
        d2 = 1.0 - (G @ h) ** 2 / (np.where(G2 > 0, G2, np.inf) * hh)
    order = np.argsort(d2)[:N_REFINE]
    grid_best = float(np.sqrt(max(0.0, d2[order[0]])))
    cands = [refine(h, shape_fn, lo, hi, mesh[i]) for i in order]
    return min([grid_best] + [c[0] for c in cands]), grid_best


# ---- CAN / INF3 -------------------------------------------------------------------------------

def can_shapes():
    pos = gamma.pdf(T, 6.0)
    under = gamma.pdf(T, 16.0)
    can = pos - under / 6.0
    with np.errstate(divide="ignore", invalid="ignore"):
        dpos = np.where(T > 0, pos * (5.0 / T - 1.0), 0.0)
        dunder = np.where(T > 0, under * (15.0 / T - 1.0), 0.0)
    tder = dpos - dunder / 6.0
    disp = (gamma.pdf(T, 6.0, scale=1.0) - gamma.pdf(T, 6.0 / 1.01, scale=1.01)) / 0.01
    return can, tder, disp


def span_distance(h, B):
    coef, *_ = np.linalg.lstsq(B.T, h, rcond=None)
    r = h - B.T @ coef
    return float(np.linalg.norm(r) / np.linalg.norm(h))


# ---- TX set --------------------------------------------------------------------------------------

def tx_points():
    """First 1024 accepted points of the unscrambled 7-D Sobol sequence over the TX box."""
    m = 13  # 8192 candidates; asserted sufficient
    u = qmc.Sobol(d=len(K.TX_LOWER), scramble=False).random_base2(m)
    th = K.TX_LOWER + u * (K.TX_UPPER - K.TX_LOWER)
    acc, sums = [], []
    n_seen = 0
    for x in th:
        n_seen += 1
        ok, s = K.tx_accept(x)
        if ok:
            acc.append(x)
            sums.append(s)
            if len(acc) == N_SOBOL:
                break
    assert len(acc) == N_SOBOL, "not enough accepted Sobol candidates"
    return np.array(acc), sums, n_seen


def pct(x):
    return dict(median=float(np.median(x)), p10=float(np.percentile(x, 10)),
                min=float(np.min(x)), p90=float(np.percentile(x, 90)), max=float(np.max(x)))


def main(out_path):
    pts, sums, n_seen = tx_points()
    H = np.stack([K.il(T, *p) for p in pts])  # causal, 0.1 s grid; amplitude irrelevant
    g_mesh, g_G, g_G2 = build_grid(gauss_from_chart, GAUSS_LO, GAUSS_HI)
    c_mesh, c_G, c_G2 = build_grid(cascade_from_chart, CAS_LO, CAS_HI)
    can, tder, disp = can_shapes()
    B = np.stack([can, tder, disp])
    d_g, d_c, d_can, d_inf3, d_g_grid, d_c_grid = [], [], [], [], [], []
    for h in H:
        a, ag = chart_distance(h, gauss_from_chart, GAUSS_LO, GAUSS_HI, g_mesh, g_G, g_G2)
        b, bg = chart_distance(h, cascade_from_chart, CAS_LO, CAS_HI, c_mesh, c_G, c_G2)
        d_g.append(a); d_c.append(b); d_g_grid.append(ag); d_c_grid.append(bg)
        d_can.append(rel_dist(h, can))
        d_inf3.append(span_distance(h, B))
    d_g, d_c, d_can, d_inf3 = map(np.array, (d_g, d_c, d_can, d_inf3))
    res = dict(
        protocol="phrf-comparative-evaluation-protocol v3.1 (1dd49723), section 5 TX distance check, D15",
        n_points=N_SOBOL, sobol_candidates_examined=n_seen,
        acceptance_rate=N_SOBOL / n_seen,
        points_sha256=hashlib.sha256(np.ascontiguousarray(pts).tobytes()).hexdigest(),
        tx_box_lower=K.TX_LOWER.tolist(), tx_box_upper=K.TX_UPPER.tolist(),
        tx_param_names=list(K.TX_PARAM_NAMES), tx_summary_ranges=K.TX_RANGES,
        attained_summaries={k: pct(np.array([s[k] for s in sums])) for k in ("peak_time", "fwhm", "undershoot_ratio")},
        grid=dict(dt=0.1, t_max=48.0, n=int(len(T)), grid_points_per_axis=GRID_N, refine_from_best=N_REFINE,
                  gaussian_chart=dict(tau=[3.0, 8.0], sigma=[0.8, 3.0]),
                  cascade34_chart=dict(kappaP=[0.25, 1.0], rate_ratio=[0.1, 0.8], rho=[0.0, 0.8])),
        gaussian=pct(d_g), cascade34=pct(d_c),
        gaussian_grid_only=pct(np.array(d_g_grid)), cascade34_grid_only=pct(np.array(d_c_grid)),
        can=pct(d_can), inf3=pct(d_inf3),
        threshold=THRESHOLD,
        pass_gaussian=bool(np.median(d_g) >= THRESHOLD),
        pass_cascade34=bool(np.median(d_c) >= THRESHOLD),
    )
    res["verdict"] = "PASS" if (res["pass_gaussian"] and res["pass_cascade34"]) else "FAIL"
    res["script_sha256"] = hashlib.sha256(open(__file__, "rb").read()).hexdigest()
    res["distances_sha256"] = hashlib.sha256(np.stack([d_g, d_c, d_can, d_inf3]).tobytes()).hexdigest()
    res["numpy"], res["scipy"] = np.__version__, __import__("scipy").__version__
    with open(out_path, "w") as f:
        json.dump(res, f, indent=2, sort_keys=True)
        f.write("\n")
    np.savetxt(os.path.splitext(out_path)[0] + "_distances.csv", np.column_stack([d_g, d_c, d_can, d_inf3]),
               delimiter=",", header="gaussian,cascade34,can,inf3", comments="")
    return res


if __name__ == "__main__":
    r = main(sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "receipt.json"))
    for k in ("n_points", "acceptance_rate", "gaussian", "cascade34", "can", "inf3", "verdict"):
        print(k, r[k])
