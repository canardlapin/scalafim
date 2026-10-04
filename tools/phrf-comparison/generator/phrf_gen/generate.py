"""Dataset generation: design, truth and noise each come from their own SplitMix64 stream."""
from __future__ import annotations

import numpy as np
from scipy.signal import lfilter

from . import kernels as K
from .cells import Cell
from .seeds import rng_for, stream_seed

N_NUIS = 6
LOWRANK_RANK = 3
LOWRANK_FRAC = 0.30  # assumption: share of marginal noise variance in the shared low-rank part
HEAVY_DF = 3
COND_TOTAL_SECONDS = 600.0
N_EVENTS_COND = 300
N_COND = 3
N_STIM_PER_COND = 12
N_REPEATS = 4
TRIAL_SPACING = {"slow": ((6.0, 10.0), 330), "fast": ((2.0, 4.0), 150)}  # ((ISI lo, hi) s, run samples)
DURATION_QUAD = 40  # midpoint-rule points for a boxcar neural duration


# ---- AR noise ----------------------------------------------------------------------------------

def ar_innovation_sd(ar) -> float:
    """Innovation SD giving unit marginal variance for AR(1)/AR(2)."""
    if len(ar) == 1:
        return float(np.sqrt(1 - ar[0] ** 2))
    p1, p2 = ar
    g0 = (1 - p2) / ((1 + p2) * ((1 - p2) ** 2 - p1**2))  # marginal var for unit innovation var
    return float(1 / np.sqrt(g0))


def ar_noise(rng, ar, shape, burn=500):
    """Unit-marginal-variance stationary AR(p) along the last axis."""
    n = shape[-1]
    w = rng.standard_normal(shape[:-1] + (n + burn,)) * ar_innovation_sd(ar)
    a = np.concatenate([[1.0], -np.asarray(ar, float)])
    return lfilter([1.0], a, w, axis=-1)[..., burn:]


# ---- design -----------------------------------------------------------------------------------

def nuisance_block(n: int) -> np.ndarray:
    lin = np.linspace(-1.0, 1.0, n)
    quad = lin**2 - np.mean(lin**2)
    idx = np.arange(n)
    cols = [np.ones(n), lin, quad] + [np.cos(np.pi * k * (idx + 0.5) / n) for k in (1, 2, 3)]
    return np.stack(cols, axis=1)


def _grid(x):  # snap to the 0.1 s onset grid
    return np.round(np.asarray(x, float) * 10.0) / 10.0


def condition_design(cell: Cell, rng):
    if cell.tr_aligned_onsets:
        raise ValueError('tr_aligned_onsets is defined for trial cells only')
    n_samp = int(round(COND_TOTAL_SECONDS / cell.tr))
    if cell.spacing == "dense":
        k = rng.choice(int(round((COND_TOTAL_SECONDS - 49.0) * 10)), size=N_EVENTS_COND, replace=False)
        onsets = np.sort(k) / 10.0
    else:  # sparse: jittered ISI 10-14 s, as many as fit
        t = float(rng.integers(0, 50)) / 10.0
        out = []
        while t < COND_TOTAL_SECONDS - 49.0:
            out.append(t)
            t += float(rng.integers(100, 141)) / 10.0
        onsets = np.array(out)
    n = len(onsets)
    cond = np.resize(np.arange(N_COND), n)
    cond = rng.permutation(cond).astype(np.int32)
    ev = dict(
        onset=onsets, onset_true=onsets.copy(), cond=cond, stim=-np.ones(n, np.int32),
        run=np.zeros(n, np.int32), duration=np.full(n, cell.duration),
    )
    runs = [dict(n=n_samp, events=np.arange(n))]
    return ev, runs


def trial_design(cell: Cell, rng):
    (lo, hi), n_samp = TRIAL_SPACING[cell.spacing]
    per_run = N_COND * N_STIM_PER_COND
    onset, cond, stim, run = [], [], [], []
    runs = []
    for r in range(N_REPEATS):
        order = rng.permutation(per_run)  # stimulus id = cond * 12 + stim, each once per run
        while True:
            if cell.tr_aligned_onsets:  # integer multiples of TR (GLMsingle sees the true design)
                g_lo, g_hi = int(np.ceil(lo / cell.tr - 1e-9)), int(np.floor(hi / cell.tr + 1e-9))
                gaps = rng.integers(g_lo, g_hi + 1, size=per_run - 1) * cell.tr
                first = rng.integers(int(np.ceil(2.0 / cell.tr)), int(np.floor(6.0 / cell.tr)) + 1) * cell.tr
            else:
                gaps = rng.integers(int(lo * 10), int(hi * 10) + 1, size=per_run - 1) / 10.0
                first = rng.integers(20, 61) / 10.0
            ons = first + np.concatenate([[0.0], np.cumsum(gaps)])
            if ons[-1] <= n_samp * cell.tr - 12.0:  # rejection: keep the last response inside the run
                break
        start = len(onset)
        onset += list(ons)
        cond += list(order // N_STIM_PER_COND)
        stim += list(order)
        run += [r] * per_run
        runs.append(dict(n=n_samp, events=np.arange(start, start + per_run)))
    n = len(onset)
    ev = dict(
        onset=np.array(onset), onset_true=np.array(onset), cond=np.array(cond, np.int32),
        stim=np.array(stim, np.int32), run=np.array(run, np.int32), duration=np.zeros(n),
    )
    return ev, runs


# ---- response construction ------------------------------------------------------------------------

def _response_matrix(f, pk, sample_t, onsets, duration):
    """Unit-peak kernel response at sample_t to delta (or boxcar) impulses at onsets: (T, N)."""
    lag = sample_t[:, None] - onsets[None, :]
    if duration <= 0:
        return f(lag) / pk
    s = (np.arange(DURATION_QUAD) + 0.5) / DURATION_QUAD * duration
    return sum(f(lag - si) for si in s) / (DURATION_QUAD * pk)


def generate_dataset(cell: Cell, root: int, dataset: int) -> dict:
    """Return dict(arrays=..., meta=...) for one dataset."""
    r_design = rng_for(root, cell.cell_id, dataset, "design")
    r_truth = rng_for(root, cell.cell_id, dataset, "truth")
    r_noise = rng_for(root, cell.cell_id, dataset, "nullcal" if cell.null_cal else "noise")

    is_trial = cell.kind == "trial"
    ev, runs = (trial_design if is_trial else condition_design)(cell, r_design)
    n_ev = len(ev["onset"])
    V, Vp = cell.n_voxels, (cell.n_pool if is_trial else 0)
    C = N_COND

    # sample grid and nuisance
    sample_t, run_id, nuis_blocks = [], [], []
    for ri, run in enumerate(runs):
        sample_t.append(np.arange(run["n"]) * cell.tr)
        run_id.append(np.full(run["n"], ri, np.int32))
        nuis_blocks.append(nuisance_block(run["n"]))
    sample_t_all = np.concatenate(sample_t)
    run_id_all = np.concatenate(run_id)
    T = len(sample_t_all)
    P = N_NUIS * len(runs)
    nuis = np.zeros((T, P))
    off = 0
    for ri, b in enumerate(nuis_blocks):
        nuis[off : off + len(b), ri * N_NUIS : (ri + 1) * N_NUIS] = b
        off += len(b)

    # ---- truth stream ----
    # event-level jitter (truth only): shared across voxels
    if is_trial and cell.latency_jitter:
        jit = np.round(r_truth.uniform(-1.0, 1.0, n_ev) * 10.0) / 10.0
        ev["onset_true"] = ev["onset"] + jit
    truth_params, kernels_out = [], np.zeros((V, len(K.OUT_T)))
    signal = np.zeros((V, T))
    cond_coef = np.zeros((V, C))
    cond_peak = np.zeros((V, C))
    scale = np.zeros(V)
    trial_beta = np.zeros((V, n_ev)) if is_trial else None
    sig_cm = np.zeros((V, T)) if is_trial else None
    names = ()
    heavy = cell.deviation == "heavy"

    def dev_draw(size):  # unit-SD deviation draw
        if heavy:
            return r_truth.standard_t(HEAVY_DF, size) / np.sqrt(HEAVY_DF / (HEAVY_DF - 2))
        return r_truth.standard_normal(size)

    for v in range(V):
        if cell.truth == "NULL":
            truth_params.append(np.zeros(0))
            continue
        names, p, f, pk = K.draw_truth(cell.truth, r_truth)
        truth_params.append(p)
        kernels_out[v] = f(K.OUT_T) / pk
        amp = r_truth.uniform(0.5, 2.0, C) * r_truth.choice([-1.0, 1.0], C)  # relative, random sign
        # regressors per condition (unit-peak kernel), sampled at sample times, per run
        x = np.zeros((C, T))
        xt = None
        if is_trial:
            xt = np.zeros((T, n_ev))
        for ri, run in enumerate(runs):
            sl = slice(int(sum(rr["n"] for rr in runs[:ri])), int(sum(rr["n"] for rr in runs[: ri + 1])))
            idx = run["events"]
            Hm = _response_matrix(f, pk, sample_t[ri], ev["onset_true"][idx], cell.duration)
            for c in range(C):
                x[c, sl] = Hm[:, ev["cond"][idx] == c].sum(axis=1)
            if is_trial:
                xt[sl][:, idx] = Hm
        if is_trial:
            m = np.mean(np.abs(amp))
            # raw (pre-rescale) trial amplitudes in units where kernel peak = 1
            a_tr = amp[ev["cond"]]
            if cell.deviation == "iid":
                dev = np.sqrt(0.5**2 + 0.25**2) * m * dev_draw(n_ev)
            else:
                s_eff = 0.5 * m * dev_draw(C * N_STIM_PER_COND)
                dev = s_eff[ev["stim"]] + 0.25 * m * dev_draw(n_ev)
            beta_raw = a_tr + dev
            sig_mean = xt @ a_tr  # condition-mean signal defines the SNR
            k = cell.snr / np.std(sig_mean)
            signal[v] = xt @ (beta_raw * k)
            trial_beta[v] = beta_raw * k
            sig_cm[v] = sig_mean * k
            cond_coef[v] = amp * k
            cond_peak[v] = amp * k
            scale[v] = k
        else:
            mpk = np.max(np.abs(x), axis=1)
            coef_rel = amp / mpk  # condition peak height of the drive-convolved response = amp
            sig = coef_rel @ x
            k = cell.snr / np.std(sig)
            signal[v] = sig * k
            cond_coef[v] = coef_rel * k
            cond_peak[v] = amp * k
            scale[v] = k
    if cell.truth == "NULL":
        kernels_out[:] = 0.0

    # ---- noise stream (marginal noise SD = 1 in data units) ----
    Va = V + Vp
    nuis_coef = r_noise.standard_normal((Va, P))
    if is_trial:
        W = r_noise.standard_normal((Va, LOWRANK_RANK))
        W *= np.sqrt(LOWRANK_FRAC) / np.linalg.norm(W, axis=1, keepdims=True)
    noise = np.zeros((Va, T))
    off = 0
    for ri, run in enumerate(runs):
        n = run["n"]
        idio = ar_noise(r_noise, cell.ar, (Va, n))
        if is_trial:
            Z = ar_noise(r_noise, cell.ar, (LOWRANK_RANK, n))
            noise[:, off : off + n] = np.sqrt(1 - LOWRANK_FRAC) * idio + W @ Z
        else:
            noise[:, off : off + n] = idio
        off += n
    drift = nuis_coef @ nuis.T
    y_all = np.zeros((Va, T))
    y_all[:V] = signal
    y_all += drift + noise

    arrays = dict(
        y=y_all[:V], signal=signal, nuisance=nuis, nuisance_coef=nuis_coef[:V],
        sample_time=sample_t_all, run_id=run_id_all,
        ev_onset=ev["onset"], ev_onset_true=ev["onset_true"], ev_cond=ev["cond"].astype(np.int32),
        ev_stim=ev["stim"].astype(np.int32), ev_run=ev["run"].astype(np.int32), ev_duration=ev["duration"],
        truth_t=K.OUT_T, truth_kernel=kernels_out,
        truth_params=np.array(truth_params) if names else np.zeros((V, 0)),
        cond_coef=cond_coef, cond_peak_amp=cond_peak, signal_scale=scale,
    )
    if is_trial:
        arrays["trial_beta"] = trial_beta
        arrays["signal_condition_mean"] = sig_cm
        arrays["y_pool"] = y_all[V:]
        arrays["nuisance_coef_pool"] = nuis_coef[V:]
    meta = dict(
        cell=cell.to_json(), dataset=int(dataset), truth_param_names=list(names),
        n_samples=int(T), n_events=int(n_ev), n_voxels=V, n_pool=Vp, n_nuisance_cols=P,
        noise_sd=1.0, snr_definition="std_t(signal)/marginal_noise_sd, per voxel, ddof=0",
        truth_grid={"start": 0.0, "stop": 48.0, "step": 0.1, "n": int(len(K.OUT_T))},
        streams={p: stream_seed(root, cell.cell_id, dataset, p) for p in ("design", "truth", "noise", "nullcal")},
    )
    return dict(arrays=arrays, meta=meta)
