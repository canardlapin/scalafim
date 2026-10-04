"""Truth kernels from their published / mathematical definitions (no PHRF code shared).

All kernels are causal (zero for t < 0) and are returned with UNIT PEAK on a fine grid
(`unit_peak`), so amplitudes are specified in data units by the drive builder, never by a
family normalization.

Conventions matched to scalafim (read only to confirm parameter meaning):
  TG  exp(-(t-tau)^2 / (2 sigma^2)), t >= 0            GaussianFamily.scala:11-12, 40-45
  TL  TG - rho exp(-(t-tau-2 sigma)^2 / (2 (1.6 sigma)^2))   LwuFamily.scala:6-7, 27, 43-44
  TS  SPM double gamma: gamma_pdf(t; shape p1, scale d1) - r gamma_pdf(t; shape p2, scale d2);
      canonical (p1,p2,d1,d2,r)=(6,16,1,1,1/6)        HrfFunctions.scala:43,70-75 and
      docs/audits/spmg-correction.md:3-5 ("dgamma(t,6) - dgamma(t,16)/6").  The protocol's
      "peak delay" / "undershoot delay" are the gamma SHAPE parameters in this SPM convention
      (canonical 6 and 16; attained peak is at shape-1 = 5 s for d=1).
  TX  Lindquist-Wager inverse logit (IL), Lindquist et al. 2009 NeuroImage 45:S187, eq. A10:
        h(t) = a1 L((t-T1)/D1) + a2 L((t-T2)/D2) + a3 L((t-T3)/D3),  L(x)=1/(1+exp(-x)).
      Return-to-baseline constraint (frozen here): a1+a2+a3 = 0, with a1 = 1, a3 = c >= 0
      (recovery of the undershoot) and a2 = -(1+c).  The paper text available (A10) gives the
      form and the roles of alpha (amplitude/direction), T (shift), D (slope scale); the
      explicit sum-to-zero constraint is this harness's reading of "return to baseline" and is
      frozen as TX v0.  Causality: h is truncated to 0 for t<0, so the box is constrained by
      rejection to |h(0)| <= 0.02 * peak (negligible step) and |h(48)| <= 0.01 * peak.
"""
from __future__ import annotations

import numpy as np
from scipy.stats import gamma as _gamma

FINE_DT = 0.01
FINE_T = np.arange(0.0, 48.0 + FINE_DT / 2, FINE_DT)  # fine grid for peaks / summaries
OUT_DT = 0.1
OUT_T = np.arange(0.0, 48.0 + OUT_DT / 2, OUT_DT)  # 481 points, stored truth grid


def _causal(t, f):
    t = np.asarray(t, float)
    out = np.zeros_like(t)
    m = t >= 0
    out[m] = f(t[m])
    return out


def gaussian(t, tau, sigma):
    return _causal(t, lambda x: np.exp(-0.5 * ((x - tau) / sigma) ** 2))


def lwu(t, tau, sigma, rho):
    ks = 1.6 * sigma
    return _causal(
        t,
        lambda x: np.exp(-0.5 * ((x - tau) / sigma) ** 2)
        - rho * np.exp(-0.5 * ((x - tau - 2 * sigma) / ks) ** 2),
    )


def spm_gamma(t, p1, p2, d1, d2, ratio):
    return _causal(t, lambda x: _gamma.pdf(x, p1, scale=d1) - ratio * _gamma.pdf(x, p2, scale=d2))


def spm_canonical(t):
    return spm_gamma(t, 6.0, 16.0, 1.0, 1.0, 1.0 / 6.0)


def logistic(x):
    return 0.5 * (1.0 + np.tanh(0.5 * x))


def il(t, T1, D1, g2, D2, g3, D3, c):
    """Inverse-logit HRF. T2 = T1+g2, T3 = T2+g3 (gaps keep the ordering rise < fall < recovery)."""
    T2 = T1 + g2
    T3 = T2 + g3
    return _causal(
        t,
        lambda x: logistic((x - T1) / D1) - (1 + c) * logistic((x - T2) / D2) + c * logistic((x - T3) / D3),
    )


# ---- summaries and unit-peak normalization -------------------------------------------------

def summaries(h_fine: np.ndarray) -> dict:
    """Attained summaries on FINE_T: peak time/height, FWHM (linear-interp crossings), undershoot ratio."""
    k = int(np.argmax(h_fine))
    pk = float(h_fine[k])
    half = pk / 2
    i = k
    while i > 0 and h_fine[i] > half:
        i -= 1
    left = FINE_T[i] + (half - h_fine[i]) / (h_fine[i + 1] - h_fine[i]) * FINE_DT if h_fine[i] < half else 0.0
    j = k
    n = len(h_fine)
    while j < n - 1 and h_fine[j] > half:
        j += 1
    right = (
        FINE_T[j - 1] + (h_fine[j - 1] - half) / (h_fine[j - 1] - h_fine[j]) * FINE_DT
        if h_fine[j] <= half
        else FINE_T[-1]
    )
    under = max(0.0, -float(h_fine[k:].min())) / pk
    return {"peak_time": float(FINE_T[k]), "peak": pk, "fwhm": float(right - left), "undershoot_ratio": under}


def unit_peak(h: np.ndarray, peak: float) -> np.ndarray:
    return h / peak


# ---- TX parameter box ----------------------------------------------------------------------

TX_PARAM_NAMES = ("T1", "D1", "g2", "D2", "g3", "D3", "c")
TX_LOWER = np.array([2.5, 0.5, 2.5, 0.8, 1.5, 1.0, 0.0])
TX_UPPER = np.array([5.0, 1.2, 8.0, 2.5, 7.0, 3.0, 0.5])
TX_RANGES = {"peak_time": (4.0, 8.0), "fwhm": (3.0, 7.0), "undershoot_ratio": (0.0, 0.4)}


def tx_accept(theta) -> tuple[bool, dict]:
    h = il(FINE_T, *theta)
    s = summaries(h)
    ok = (
        all(lo <= s[k] <= hi for k, (lo, hi) in TX_RANGES.items())
        and s["peak"] > 0
        and abs(h[0]) <= 0.02 * s["peak"]
        and abs(h[-1]) <= 0.01 * s["peak"]
    )
    return bool(ok), s


def tx_draw(rng: np.random.Generator):
    """Uniform over the TX box, rejection on attained summaries (exactly as in the gate)."""
    while True:
        theta = TX_LOWER + rng.random(len(TX_LOWER)) * (TX_UPPER - TX_LOWER)
        ok, _ = tx_accept(theta)
        if ok:
            return theta


# ---- per-family truth drawing ---------------------------------------------------------------

def draw_truth(family: str, rng: np.random.Generator):
    """Return (param_names, params, fine_kernel_unit_peak, out_kernel_unit_peak_on_0.1_grid)."""
    if family == "TG":
        tau = rng.uniform(3.5, 7.5)
        sigma = float(np.exp(rng.uniform(np.log(1.0), np.log(2.5))))
        names, p = ("tau", "sigma"), (tau, sigma)
        f = lambda t: gaussian(t, *p)  # noqa: E731
    elif family == "TL":
        tau = rng.uniform(3.5, 7.5)
        sigma = float(np.exp(rng.uniform(np.log(1.0), np.log(2.5))))
        rho = rng.uniform(0.0, 0.8)
        names, p = ("tau", "sigma", "rho"), (tau, sigma, rho)
        f = lambda t: lwu(t, *p)  # noqa: E731
    elif family == "TS":
        pd = rng.uniform(4.0, 7.0)
        ud = pd + rng.uniform(8.0, 12.0)
        d1 = rng.uniform(0.7, 1.3)
        d2 = rng.uniform(0.7, 1.3)
        r = rng.uniform(1 / 8, 1 / 4)
        names, p = ("peak_delay", "undershoot_delay", "disp1", "disp2", "ratio"), (pd, ud, d1, d2, r)
        f = lambda t: spm_gamma(t, pd, ud, d1, d2, r)  # noqa: E731
    elif family == "TX":
        p = tuple(float(x) for x in tx_draw(rng))
        names = TX_PARAM_NAMES
        f = lambda t: il(t, *p)  # noqa: E731
    else:
        raise ValueError(f"unsupported truth family {family}")
    hf = f(FINE_T)
    pk = float(np.max(np.abs(hf)))
    return names, np.array(p, float), f, pk
