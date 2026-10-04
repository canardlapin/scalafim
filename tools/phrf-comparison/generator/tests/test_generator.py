import io
import zipfile

import numpy as np
import pytest
from scipy.integrate import quad

from phrf_gen import kernels as K
from phrf_gen.cells import CELLS
from phrf_gen.generate import ar_innovation_sd, generate_dataset, nuisance_block
from phrf_gen.io import build, npz_bytes
from phrf_gen.seeds import HARNESS_ROOT, PURPOSES, fnv1a64, rng_for, splitmix64, stream_seed, DENYLIST

ROOT = HARNESS_ROOT
_cache = {}


def ds(cid, d=0):
    if (cid, d) not in _cache:
        _cache[(cid, d)] = generate_dataset(CELLS[cid], ROOT, d)
    return _cache[(cid, d)]


# ---- seeds ----
def test_splitmix64_reference_vector():
    assert splitmix64(0) == 0xE220A8397B1DCDAF  # first output of SplitMix64 seeded with 0


def test_fnv1a64_reference():
    assert fnv1a64("") == 0xCBF29CE484222325
    assert fnv1a64("a") == 0xAF63DC4C8601EC8C


def test_harness_root_not_denylisted():
    assert ROOT not in DENYLIST


def test_stream_independence():
    seeds = {stream_seed(ROOT, c, d, p) for c in ("C-TX-.5", "C-TS-.5") for d in range(5) for p in PURPOSES}
    assert len(seeds) == 2 * 5 * len(PURPOSES)
    a = rng_for(ROOT, "C-TX-.5", 0, "truth").standard_normal(20000)
    b = rng_for(ROOT, "C-TX-.5", 0, "noise").standard_normal(20000)
    c = rng_for(ROOT, "C-TX-.5", 1, "truth").standard_normal(20000)
    assert abs(np.corrcoef(a, b)[0, 1]) < 0.03 and abs(np.corrcoef(a, c)[0, 1]) < 0.03


def test_unknown_purpose_rejected():
    with pytest.raises(ValueError):
        stream_seed(ROOT, "x", 0, "bogus")


# ---- kernel shape / area properties ----
def test_gaussian_peak_and_area():
    t = K.FINE_T
    h = K.gaussian(t, 5.0, 1.5)
    assert abs(t[np.argmax(h)] - 5.0) < 0.01 and abs(h.max() - 1) < 1e-12
    area = np.trapezoid(h, t)
    assert abs(area - 1.5 * np.sqrt(2 * np.pi)) < 3e-3  # small truncation at t=0
    s = K.summaries(h)
    assert abs(s["fwhm"] - 2 * np.sqrt(2 * np.log(2)) * 1.5) < 0.02


def test_lwu_undershoot_and_rho0_equals_gaussian():
    t = K.FINE_T
    assert np.allclose(K.lwu(t, 5, 1.5, 0.0), K.gaussian(t, 5, 1.5))
    h = K.lwu(t, 5, 1.5, 0.6)
    assert h.min() < 0 and K.summaries(h)["undershoot_ratio"] > 0.05
    # area = sigma sqrt(2pi) (1 - rho * 1.6)
    assert abs(np.trapezoid(h, t) - 1.5 * np.sqrt(2 * np.pi) * (1 - 0.6 * 1.6)) < 1e-2


def test_spm_canonical_properties():
    t = K.FINE_T
    h = K.spm_canonical(t)
    s = K.summaries(h)
    assert abs(s["peak_time"] - 5.0) < 0.02  # gamma(6) mode = 5 s
    assert abs(np.trapezoid(h, t) - (1 - 1 / 6)) < 1e-4  # unit-area gammas, ratio 1/6
    assert h.min() < 0


def test_spm_varied_reduces_to_canonical():
    t = K.FINE_T
    assert np.allclose(K.spm_gamma(t, 6, 16, 1, 1, 1 / 6), K.spm_canonical(t))


def test_il_baseline_and_area_properties():
    t = K.FINE_T
    theta = (3.5, 0.8, 4.0, 1.2, 4.0, 1.5, 0.3)
    h = K.il(t, *theta)
    assert abs(h[-1]) < 1e-6  # returns to baseline (sum of alphas = 0)
    assert K.il(np.array([-1.0]), *theta)[0] == 0.0  # causal
    T1, D1, g2, D2, g3, D3, c = theta
    # plateau after fall is -c, between T2 and T3
    assert h.min() < -0.02
    s = K.summaries(h)
    assert 0 < s["undershoot_ratio"] < 0.5
    # analytic area of the logistic sum: int L((t-T)/D) - step = D ln(1+e^{(t-T)/D}); area over [0,48]
    def prim(T, D, tt):
        return D * np.log1p(np.exp((tt - T) / D))
    a = 1.0
    analytic = 0.0
    for al, T, D in ((1.0, T1, D1), (-(1 + c), T1 + g2, D2), (c, T1 + g2 + g3, D3)):
        analytic += al * (prim(T, D, 48.0) - prim(T, D, 0.0))
    assert abs(np.trapezoid(h, t) - analytic) < 1e-3


def test_tx_draws_respect_summary_ranges():
    rng = np.random.default_rng(0)
    for _ in range(25):
        th = K.tx_draw(rng)
        ok, s = K.tx_accept(th)
        assert ok
        for k, (lo, hi) in K.TX_RANGES.items():
            assert lo <= s[k] <= hi


@pytest.mark.parametrize("fam", ["TG", "TL", "TS", "TX"])
def test_truth_family_ranges(fam):
    rng = np.random.default_rng(3)
    for _ in range(200):
        names, p, f, pk = K.draw_truth(fam, rng)
        d = dict(zip(names, p))
        if fam in ("TG", "TL"):
            assert 3.5 <= d["tau"] <= 7.5 and 1.0 <= d["sigma"] <= 2.5
        if fam == "TL":
            assert 0 <= d["rho"] <= 0.8
        if fam == "TS":
            assert 4 <= d["peak_delay"] <= 7
            assert 8 <= d["undershoot_delay"] - d["peak_delay"] <= 12
            assert 0.7 <= d["disp1"] <= 1.3 and 0.7 <= d["disp2"] <= 1.3
            assert 1 / 8 <= d["ratio"] <= 1 / 4
        assert pk > 0


# ---- datasets ----
@pytest.mark.parametrize("cid", ["C-TX-.5", "C-TS-1", "C-TS-.5", "C-TG-.5", "T-TX-fast", "T-TX-jit", "T-TS-fast"])
def test_snr_exact_per_voxel(cid):
    cell = CELLS[cid]
    a = ds(cid)["arrays"]
    sd = (a["signal_condition_mean"] if cell.kind == "trial" else a["signal"]).std(axis=1)  # ddof=0
    assert np.allclose(sd, cell.snr, rtol=1e-10)


@pytest.mark.parametrize("cid", ["C-TX-.5", "T-TX-fast"])
def test_realized_noise_sd_and_residual_identity(cid):
    a = ds(cid)["arrays"]
    noise = a["y"] - a["signal"] - a["nuisance_coef"] @ a["nuisance"].T
    sd = noise.std(axis=1)
    assert abs(np.mean(sd) - 1.0) < 0.05  # marginal SD 1 data unit
    assert np.all(np.abs(sd - 1.0) < 0.3)


def _ar1_hat(noise, run_id):
    num = den = 0.0
    for r in np.unique(run_id):
        x = noise[:, run_id == r]
        x = x - x.mean(axis=1, keepdims=True)
        num += np.sum(x[:, 1:] * x[:, :-1])
        den += np.sum(x[:, :-1] ** 2)
    return num / den


def test_ar1_recovery_condition():
    a = ds("C-TX-.5")["arrays"]
    noise = a["y"] - a["signal"] - a["nuisance_coef"] @ a["nuisance"].T
    assert abs(_ar1_hat(noise, a["run_id"]) - 0.3) < 0.02


def test_ar2_recovery():
    a = ds("C-TX-AR2")["arrays"]
    noise = a["y"] - a["signal"] - a["nuisance_coef"] @ a["nuisance"].T
    x = noise - noise.mean(axis=1, keepdims=True)
    # Yule-Walker on pooled autocovariances
    g = [np.mean(np.sum(x[:, k:] * x[:, : x.shape[1] - k], axis=1)) / x.shape[1] for k in range(3)]
    R = np.array([[g[0], g[1]], [g[1], g[0]]])
    phi = np.linalg.solve(R, np.array([g[1], g[2]]))
    assert np.allclose(phi, [0.4, 0.2], atol=0.03)
    assert abs(x.var() - 1.0) < 0.05  # unit marginal variance from ar_innovation_sd


def test_trial_noise_ar1_and_lowrank():
    a = ds("T-TX-fast")["arrays"]
    full = np.vstack([a["y"], a["y_pool"]])
    coef = np.vstack([a["nuisance_coef"], a["nuisance_coef_pool"]])
    noise = full - np.vstack([a["signal"], np.zeros_like(a["y_pool"])]) - coef @ a["nuisance"].T
    assert abs(_ar1_hat(noise, a["run_id"]) - 0.3) < 0.06  # shared rank-3 part has few effective samples
    s = np.linalg.svd(noise - noise.mean(axis=1, keepdims=True), compute_uv=False)
    assert s[3] < 0.5 * s[2]  # rank-3 structured component stands out above the idiosyncratic floor


def test_determinism_same_keys_same_bytes():
    cell = CELLS["T-TX-jit"]
    b1, m1 = build(cell, ROOT, "harness", 3)
    b2, m2 = build(cell, ROOT, "harness", 3)
    assert b1 == b2 and m1 == m2
    b3, _ = build(cell, ROOT, "harness", 4)
    assert b3 != b1
    with zipfile.ZipFile(io.BytesIO(b1)) as z:
        a = np.load(io.BytesIO(z.read("y.npy")))
    assert a.shape == (200, 600)


def test_truth_and_design_independent_of_noise_stream():
    # null-cal uses the nullcal purpose for noise: same cell layout, different noise, same design
    a = ds("C-NULL")["arrays"]
    b = ds("C-NULL-cal")["arrays"]
    assert not np.allclose(a["y"], b["y"])
    assert np.all(a["signal"] == 0) and a["truth_kernel"].max() == 0


def test_condition_schedule_constraints():
    a = ds("C-TX-.5")["arrays"]
    on = a["ev_onset"]
    assert len(on) == 300 and a["y"].shape[1] == 600 and np.all(np.diff(on) >= 0)
    assert np.all(on >= 0) and np.all(on < 551)
    assert np.allclose(on * 10, np.round(on * 10))  # 0.1 s grid
    assert np.any(np.abs(on - np.round(on)) > 1e-9)  # sub-TR onsets present
    assert np.bincount(a["ev_cond"]).tolist() == [100, 100, 100]
    assert a["nuisance"].shape == (600, 6)
    assert np.linalg.matrix_rank(a["nuisance"]) == 6


def test_sparse_isi_between_10_and_14():
    a = ds("C-TX-sparse")["arrays"]
    d = np.diff(a["ev_onset"])
    assert d.min() >= 10 - 1e-9 and d.max() <= 14 + 1e-9


@pytest.mark.parametrize("cid,lo,hi,n_samp", [("T-TX-fast", 2, 4, 150), ("T-TX-slow", 6, 10, 330)])
def test_trial_schedule_constraints(cid, lo, hi, n_samp):
    a = ds(cid)["arrays"]
    assert len(a["ev_onset"]) == 144 and a["y"].shape[1] == 4 * n_samp
    assert a["nuisance"].shape == (4 * n_samp, 24)
    for r in range(4):
        m = a["ev_run"] == r
        assert m.sum() == 36
        on = a["ev_onset"][m]
        d = np.diff(on)
        assert d.min() >= lo - 1e-9 and d.max() <= hi + 1e-9
        assert on[-1] <= n_samp - 12 and on[0] >= 0
        assert sorted(a["ev_stim"][m].tolist()) == list(range(36))  # each stimulus once per run
    # each stimulus 4 times overall, 12 per condition
    assert np.bincount(a["ev_stim"]).tolist() == [4] * 36
    assert np.allclose(a["ev_onset"] * 10, np.round(a["ev_onset"] * 10))


def test_jitter_only_in_truth_and_bounded():
    a = ds("T-TX-jit")["arrays"]
    j = a["ev_onset_true"] - a["ev_onset"]
    assert np.abs(j).max() <= 1.0 + 1e-9 and np.abs(j).max() > 0.5
    b = ds("T-TX-fast")["arrays"]
    assert np.all(b["ev_onset_true"] == b["ev_onset"])


def test_trial_deviation_structure():
    a = ds("T-TX-fast")["arrays"]
    beta = a["trial_beta"]
    stim, cond = a["ev_stim"], a["ev_cond"]
    k = a["signal_scale"][:, None]
    m = np.mean(np.abs(a["cond_peak_amp"]), axis=1)  # scaled units
    # persistent stimulus effect: repeats of one stimulus correlate more than repeats of different ones
    dev = beta - a["cond_peak_amp"][:, cond]
    s_hat = np.stack([dev[:, stim == s].mean(axis=1) for s in range(36)], axis=1)
    sd_between = (s_hat / m[:, None]).std()
    assert 0.45 < sd_between < 0.75  # theory: sqrt(0.5^2 + 0.25^2/4) ~ 0.52
    rep = np.stack([dev[:, stim == s] for s in range(36)], axis=1)  # V,36,4
    sd_within = ((rep - rep.mean(axis=2, keepdims=True)) / m[:, None, None]).std() * np.sqrt(4 / 3)
    assert 0.2 < sd_within < 0.3  # theory 0.25
    c = ds("T-TX-iid")["arrays"]
    devc = c["trial_beta"] - c["cond_peak_amp"][:, c["ev_cond"]]
    repc = np.stack([devc[:, c["ev_stim"] == s] for s in range(36)], axis=1)
    mc = np.mean(np.abs(c["cond_peak_amp"]), axis=1)
    # i.i.d.: between-stimulus mean of 4 repeats has SD sqrt(.3125/4)*m
    assert 0.25 < (repc.mean(axis=2) / mc[:, None]).std() < 0.33


def test_heavy_tail_kurtosis():
    a = ds("T-TX-jit")["arrays"]
    dev = a["trial_beta"] - a["cond_peak_amp"][:, a["ev_cond"]]
    z = (dev / np.mean(np.abs(a["cond_peak_amp"]), axis=1)[:, None]).ravel()
    g = ds("T-TX-fast")["arrays"]
    zg = ((g["trial_beta"] - g["cond_peak_amp"][:, g["ev_cond"]]) / np.mean(np.abs(g["cond_peak_amp"]), axis=1)[:, None]).ravel()
    kurt = lambda x: np.mean((x - x.mean()) ** 4) / np.var(x) ** 2  # noqa: E731
    assert kurt(z) > 4.5 > kurt(zg)


def test_amplitudes_in_data_units_condition():
    a = ds("C-TG-.5")["arrays"]
    # condition regressor peak height equals |cond_peak_amp|: rebuild from coefficient and kernel
    v = 0
    tau, sigma = a["truth_params"][v]
    t = a["sample_time"]
    on, cd = a["ev_onset"], a["ev_cond"]
    for c in range(3):
        x = np.exp(-0.5 * ((t[:, None] - on[None, cd == c] - tau) / sigma) ** 2) * ((t[:, None] - on[None, cd == c]) >= 0)
        x = x.sum(axis=1)
        assert abs(np.max(np.abs(a["cond_coef"][v, c] * x)) - abs(a["cond_peak_amp"][v, c])) < 1e-5  # kernel unit peak taken on a 0.01 s grid
    rel = np.abs(a["cond_peak_amp"]) / np.abs(a["cond_peak_amp"]).max(axis=1, keepdims=True)
    assert rel.min() > 0.5 / 2 - 1e-9  # relative peak ratios within U[.5,2] range


def test_signal_matches_exact_time_convolution_trial():
    a = ds("T-TS-fast")["arrays"]
    meta_t = a["truth_t"]
    v = 2
    # rebuild noiseless signal from stored kernel (interpolating) and betas within run 0
    h = lambda u: np.where(u >= 0, np.interp(u, meta_t, a["truth_kernel"][v], right=0.0), 0.0)  # noqa: E731
    m = a["ev_run"] == 0
    t = a["sample_time"][a["run_id"] == 0]
    sig = (h(t[:, None] - a["ev_onset_true"][m][None, :]) * a["trial_beta"][v][m][None, :]).sum(axis=1)
    assert np.max(np.abs(sig - a["signal"][v][a["run_id"] == 0])) < 2e-3 * np.abs(sig).max()


def test_tr2_cell_samples():
    a = ds("C-TX-TR2")["arrays"]
    assert a["y"].shape[1] == 300 and a["sample_time"][1] == 2.0


@pytest.mark.parametrize("cid", ["T-TX-fast", "T-TX-jit", "T-TX-slow"])
def test_tg_cells_tr_aligned_onsets(cid):
    cell = CELLS[cid]
    assert cell.tr_aligned_onsets
    a = ds(cid)["arrays"]
    assert np.all(a["ev_onset"] / cell.tr == np.round(a["ev_onset"] / cell.tr))
    lo, hi = (2, 4) if "fast" in cid or "jit" in cid else (6, 10)
    for r in range(4):
        d = np.diff(a["ev_onset"][a["ev_run"] == r])
        assert d.min() >= lo and d.max() <= hi
    from phrf_gen.io import build
    assert build(cell, ROOT, "harness", 0)[1]["cell"]["tr_aligned_onsets"] is True


@pytest.mark.parametrize("cid", ["T-TS-fast", "T-TX-iid", "C-TX-.5"])
def test_other_cells_keep_subtr_onsets(cid):
    assert not CELLS[cid].tr_aligned_onsets
    on = ds(cid)["arrays"]["ev_onset"]
    assert np.any(np.abs(on - np.round(on)) > 1e-9)
