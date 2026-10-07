"""v0b gate: numerical unit/normalization/parity checks for the pinned GLMsingle.

Usage: .venv/bin/python gate_checks.py [check ...]   (default: all)
Writes results/<check>.json. Synthetic generator: 36 conditions (3 x 12 stimuli),
each once per run, 4 runs, fast-cell timing (150 volumes, ISI 2-4 s), TR 1 s,
stimdur 1 s, baseline 100 data units per voxel, onsets on the volume grid.
"""
import json, os, sys, time
import numpy as np
from glmsingle.hrf.gethrf import getcanonicalhrflibrary, getcanonicalhrf
from run_glmsingle import run_glmsingle

HERE = os.path.dirname(os.path.abspath(__file__))
TR, STIMDUR, NRUN, NCOND, T = 1.0, 1.0, 4, 36, 150
BASE = 100.0


def library():
    raw = getcanonicalhrflibrary(STIMDUR, TR)  # (20, L) unnormalised, as shipped
    return raw


def schedule(rng):
    ons, cds = [], []
    for _ in range(NRUN):
        isi = rng.integers(2, 5, size=NCOND)
        o = 4 + np.concatenate([[0], np.cumsum(isi[:-1])])
        assert o[-1] + 40 > 0
        ons.append(o.astype(float)); cds.append(rng.permutation(NCOND))
    return ons, cds


def regressors(ons, cds, hrf):
    """Per-run (n_trials, T) responses of a unit-amplitude trial with HRF `hrf` at volume onsets."""
    X = []
    for o in ons:
        M = np.zeros((len(o), T))
        for i, v in enumerate(o.astype(int)):
            n = min(len(hrf), T - v)
            M[i, v:v + n] = hrf[:n]
        X.append(M)
    return X


def make_signal_voxels(rng, ons, cds, hrf_idx, amps, raw_lib):
    """Return noiseless signal (list per run of (V, T)) in data units; amps (V, NRUN*NCOND chrono)."""
    V = len(hrf_idx)
    sig = [np.zeros((V, T)) for _ in range(NRUN)]
    for v in range(V):
        X = regressors(ons, cds, raw_lib[hrf_idx[v]])
        k = 0
        for r in range(NRUN):
            sig[r][v] = amps[v, k:k + NCOND] @ X[r]
            k += NCOND
    return sig


def rel_err(est, tru):
    return np.linalg.norm(est - tru, axis=1) / np.linalg.norm(tru, axis=1)


def chrono_amps(rng, V, ons):
    """Amplitudes ordered chronologically: trials in run order then onset order (ons sorted already)."""
    mag = rng.uniform(0.5, 2.0, size=(V, NRUN * NCOND))
    sign = rng.choice([-1.0, 1.0], size=(V, NRUN * NCOND))
    return mag * sign * 2.0  # data units; ~1-4 units on a baseline of 100


def save(name, d):
    os.makedirs(os.path.join(HERE, "results"), exist_ok=True)
    with open(os.path.join(HERE, "results", name + ".json"), "w") as f:
        json.dump(d, f, indent=1, default=float)
    print(name, json.dumps(d, default=float)[:1500])


def check_units_and_norm():
    """Noise-free, types A and B: units (psc vs raw), library normalization, exact HRF index, 1e-6 recovery."""
    rng = np.random.default_rng(11)
    raw = library()
    ons, cds = schedule(rng)
    peaks = raw.max(axis=1)
    idx = np.repeat(np.arange(20), 3)  # 20 HRFs x 3 voxels, all library members
    V = len(idx)
    amps = chrono_amps(rng, V, ons)  # amplitude multiplies the UNNORMALISED raw library HRF
    sig = make_signal_voxels(rng, ons, cds, idx, amps, raw)
    # scored voxels noise-free; 400 noise-only pool voxels (GLMsingle's tail-threshold GMM needs variance)
    pool = noise_block(rng, 400)
    data = [BASE + np.vstack([s, p]) for s, p in zip(sig, pool)]
    out = {"n_scored": V, "n_pool": 400, "library_raw_peak_min": float(peaks.min()), "library_raw_peak_max": float(peaks.max())}
    o = run_glmsingle(data, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types="AB")
    # library normalisation: returned library columns have max exactly 1
    out["library_used_peak_min"] = float(o["hrf_library"].max(axis=1).min())
    out["library_used_peak_max"] = float(o["hrf_library"].max(axis=1).max())
    # HRF index
    out["B_hrf_index_exact_fraction"] = float(np.mean(o["b_HRFindex"][:V] == idx))
    # truth in data units: data = amp * raw = (amp*peak_raw) * h_norm  -> beta_data = amp*peak_raw
    truth = amps * peaks[idx][:, None]
    # chrono order == generation order since schedules are sorted by onset within run
    # map chronological output to input order (trial_order: chrono k -> input index); input order is per-run shuffled
    inp = np.zeros_like(o["b_beta_data"]); inp[:, o["trial_order"]] = o["b_beta_data"]; inp = inp[:V]
    # our amps are chronological by construction of make_signal_voxels? They index by position in the
    # per-run shuffled list -> amps[v, r*NCOND + i] belongs to input trial (r,i); compare in input order.
    e_data = rel_err(inp, truth)
    out["B_beta_data_relerr_max"] = float(e_data.max())
    out["B_beta_data_elementwise_max_rel"] = float(np.max(np.abs(inp - truth) / np.abs(truth)))
    # raw vs psc: psc = data/|meanvol|*100
    inp_psc = np.zeros_like(o["b_beta_psc"]); inp_psc[:, o["trial_order"]] = o["b_beta_psc"]; inp_psc = inp_psc[:V]
    out["B_psc_equals_data_over_meanvol_x100_max_rel"] = float(
        np.max(np.abs(inp_psc - truth / o["meanvol"][:V, None] * 100) / np.abs(truth / o["meanvol"][:V, None] * 100)))
    mean_direct = np.concatenate(data, axis=1).mean(axis=1)
    out["meanvol_equals_mean_of_data_max_abs"] = float(np.max(np.abs(mean_direct - o["meanvol"])))
    out["meanvol_min"], out["meanvol_max"] = float(o["meanvol"][:V].min()), float(o["meanvol"][:V].max())
    # using 100 instead of meanvol would bias: ratio
    out["naive_baseline100_bias_max_rel"] = float(np.max(np.abs(o["meanvol"][:V] / 100 - 1)))
    # Explicit raw (wantpercentbold=0) units
    o0 = run_glmsingle(data, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types="B",
                       params_override={"wantpercentbold": 0})
    inp0 = np.zeros_like(o0["b_beta_psc"]); inp0[:, o0["trial_order"]] = o0["b_beta_psc"]; inp0 = inp0[:V]
    out["B_wantpercentbold0_equals_raw_data_units_max_rel"] = float(np.max(np.abs(inp0 - truth) / np.abs(truth)))
    # Type A: equal-amplitude positive trials with the assumed canonical HRF
    hassume = getcanonicalhrf(STIMDUR, TR); hassume = hassume / hassume.max()
    sA = []
    Ava = 3.0
    dA = []
    for r in range(NRUN):
        M = regressors([ons[r]], None, hassume)[0].sum(0)
        dA.append(BASE + np.vstack([Ava * np.tile(M, (5, 1)), pool[r][:200]]))
    oa = run_glmsingle(dA, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types="A")
    out["A_output_shape"] = list(oa["a_beta_psc"].shape)
    out["A_beta_data_relerr_max"] = float(np.max(np.abs(oa["a_beta_data"][:5] - Ava) / Ava))
    out["A_beta_psc_expected_ratio_max_rel"] = float(np.max(np.abs(oa["a_beta_psc"][:5] / (Ava / oa["meanvol"][:5] * 100) - 1)))
    out["pass_B_exact_index"] = out["B_hrf_index_exact_fraction"] == 1.0
    out["pass_B_beta_1e-6"] = out["B_beta_data_relerr_max"] <= 1e-6 and out["B_beta_data_elementwise_max_rel"] <= 1e-6
    save("units_and_norm", out)


def noise_block(rng, n, W=None):
    """Unit-marginal-SD noise for n voxels, list per run: sqrt(.5) AR(1, phi=.3) + sqrt(.5) shared rank-3."""
    if W is None:
        W = rng.standard_normal((n, 3)); W /= np.linalg.norm(W, axis=1, keepdims=True)
    out = []
    for r in range(NRUN):
        lat = rng.standard_normal((3, T)); w = rng.standard_normal((n, T)); phi = 0.3
        ar = np.zeros_like(w); ar[:, 0] = w[:, 0]
        for t in range(1, T): ar[:, t] = phi * ar[:, t - 1] + w[:, t]
        ar *= np.sqrt(1 - phi ** 2)
        out.append(np.sqrt(0.5) * ar + np.sqrt(0.5) * (W @ lat))
    return out


def check_float32_floor():
    """Type B noise-free recovery vs data baseline, raw units (wantpercentbold=0): isolates the float32 input cast."""
    rng = np.random.default_rng(11)
    raw = library(); peaks = raw.max(axis=1)
    ons, cds = schedule(rng)
    idx = np.repeat(np.arange(20), 3); V = len(idx)
    amps = chrono_amps(rng, V, ons)
    sig = make_signal_voxels(rng, ons, cds, idx, amps, raw)
    pool = noise_block(rng, 400)
    truth = amps * peaks[idx][:, None]
    res = {}
    for base in (0.0, 1.0, 100.0, 1000.0):
        data = [base + np.vstack([s, p]) for s, p in zip(sig, pool)]
        o = run_glmsingle(data, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types="B",
                          params_override={"wantpercentbold": 0})
        est = np.zeros_like(o["b_beta_psc"]); est[:, o["trial_order"]] = o["b_beta_psc"]; est = est[:V]
        res[f"baseline_{base:g}"] = {
            "hrf_index_exact": float(np.mean(o["b_HRFindex"][:V] == idx)),
            "relerr_L2_max": float(rel_err(est, truth).max()),
            "elementwise_rel_max": float(np.max(np.abs(est - truth) / np.abs(truth))),
            "abs_err_max_vs_float32_ulp_of_baseline": [float(np.abs(est - truth).max()), float(np.spacing(np.float32(max(base, 1e-3))))]}
    # float64 reference OLS with the same model (poly deg 0..1 per run + 144 single-trial regressors, h = library col / peak)
    from scipy.linalg import block_diag
    ref = np.zeros((V, NRUN * NCOND)); conds = []
    for v in range(V):
        h = raw[idx[v]] / peaks[idx[v]]
        X = regressors(ons, cds, h)  # per run (36, T)
        cols = [np.vstack([np.concatenate([np.zeros(0)]) if False else X[r]]) for r in range(NRUN)]
        Dm = block_diag(*[X[r].T for r in range(NRUN)])  # (4T, 144)
        P = block_diag(*[np.vander(np.linspace(-1, 1, T), 2, increasing=True) for _ in range(NRUN)])
        Pr = np.zeros((NRUN * T, 2 * NRUN))
        for r in range(NRUN): Pr[r * T:(r + 1) * T, 2 * r:2 * r + 2] = np.vander(np.linspace(-1, 1, T), 2, increasing=True)
        A = np.hstack([Dm, Pr]); y = np.concatenate([sig[r][v] for r in range(NRUN)])
        ref[v] = np.linalg.lstsq(A, y, rcond=None)[0][:NRUN * NCOND]
        if v == 0: res["design_condition_number_float64"] = float(np.linalg.cond(A))
    res["float64_reference_OLS_relerr_L2_max"] = float(rel_err(ref, truth).max())
    res["note"] = "data are cast to float32 at check_inputs.py:53 and glm_estimatemodel.py:434; betas are float32 (glm_estimatemodel.py:790)"
    save("float32_floor", res)


def check_lownoise(snr=100.0, n_per=20, n_pool=400, seed=5, regime="repeat"):
    """SNR-100 check of types C and D (and B) with a noise pool; also records noise-free C/D behaviour."""
    rng = np.random.default_rng(seed)
    raw = library(); peaks = raw.max(axis=1)
    ons, cds = schedule(rng)
    libidx = [1, 5, 9, 13, 17]  # five library members
    idx = np.repeat(libidx, n_per); V = len(idx)
    amps = chrono_amps(rng, V, ons)
    if regime == "repeat":  # every repeat of a stimulus has the same amplitude (GLMsingle's design assumption)
        a_stim = amps[:, :NCOND].copy()
        amps = np.hstack([a_stim[:, cds[r]] for r in range(NRUN)])
    sig = make_signal_voxels(rng, ons, cds, idx, amps, raw)
    sd_sig = np.concatenate(sig, axis=1).std(axis=1)  # noiseless signal SD over time
    # rescale so that realized SNR == target given noise sd fixed at 1 per voxel: signal sd = snr
    scale = snr / sd_sig
    sig = [s * scale[:, None] for s in sig]
    truth_data = amps * scale[:, None] * peaks[idx][:, None]
    # noise: unit SD marginal per voxel = sqrt(.5) AR(1,phi=.3) + sqrt(.5) shared rank-3 (over runs)
    Vtot = V + n_pool
    nz = noise_block(rng, Vtot)
    data = []
    for r in range(NRUN):
        sg = np.zeros((Vtot, T)); sg[:V] = sig[r]
        data.append(BASE + sg + nz[r])
    truth_in = truth_data
    res = {"regime": regime, "snr": snr, "n_signal": V, "n_pool_generated": n_pool}
    for types in ("BCD",):
        o = run_glmsingle(data, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types=types)
        for t in "bcd":
            est = np.zeros((Vtot, NRUN * NCOND)); est[:, o["trial_order"]] = o[t + "_beta_data"]
            e = rel_err(est[:V], truth_in)
            hit = o[t + "_HRFindex"][:V] == idx
            res[t] = {"hrf_index_correct_fraction": float(hit.mean()),
                      "beta_relerr_median": float(np.median(e)), "beta_relerr_p95": float(np.percentile(e, 95)),
                      "beta_relerr_max": float(e.max()),
                      "frac_voxels_relerr_le_1e-2": float(np.mean(e <= 1e-2))}
            # amplitude-gain diagnostic: slope of est on truth pooled
            res[t]["slope_est_on_truth"] = float((est[:V] * truth_in).sum() / (truth_in ** 2).sum())
        res["pcnum"] = o["pcnum"]; res["noisepool_size"] = o["noisepool_size"]
        res["noisepool_contains_signal_voxels"] = int(o["noisepool"][:V].sum())
        res["FRACvalue_median"] = float(np.median(o["d_FRACvalue"][:V]))
        res["cpu_s"] = o["meta"]["cpu_s"]; res["wall_s"] = o["meta"]["wall_s"]
    res["pass_C"] = res["c"]["hrf_index_correct_fraction"] >= .95 and res["c"]["beta_relerr_max"] <= 1e-2
    res["pass_D"] = res["d"]["hrf_index_correct_fraction"] >= .95 and res["d"]["beta_relerr_max"] <= 1e-2
    save(f"lownoise_snr{int(snr)}_{regime}", res)


def check_noisefree_cd():
    """Descriptive: what do C and D do on noise-free data (degenerate?)."""
    rng = np.random.default_rng(3)
    raw = library(); ons, cds = schedule(rng)
    idx = np.repeat([1, 5, 9, 13, 17], 20)
    amps = chrono_amps(rng, len(idx), ons)
    sig = make_signal_voxels(rng, ons, cds, idx, amps, raw)
    res = {}
    data = [BASE + s for s in sig]  # purely noise-free dataset, no noise pool
    try:
        run_glmsingle(data, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types="B")
        res["purely_noisefree_type_B"] = "ran"
    except BaseException as e:
        res["purely_noisefree_type_B"] = f"raised {type(e).__name__}: {str(e)[:160]}"
    pool = noise_block(rng, 400)
    data = [BASE + np.vstack([s, p]) for s, p in zip(sig, pool)]
    try:
        o = run_glmsingle(data, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types="BCD")
        res["status"] = "ran"; res["pcnum"] = o["pcnum"]; res["noisepool_size"] = o["noisepool_size"]
        for t in "cd":
            res[t + "_hrf_frac"] = float(np.mean(o[t + "_HRFindex"][:len(idx)] == idx))
            res[t + "_finite"] = bool(np.isfinite(o[t + "_beta_data"]).all())
    except BaseException as e:  # record, do not hide
        res["status"] = "raised"; res["error"] = f"{type(e).__name__}: {str(e)[:300]}"
    save("noisefree_CD", res)


def check_runtime(n_scored=40, n_pool=4000, snr=1.0, slow=False, threads=None, tag=""):
    """Time one pilot-sized dataset (protocol: 40 scored + 4000 pool voxels) at default options, types A-D."""
    global T
    T0 = T
    T = 330 if slow else 150
    try:
        rng = np.random.default_rng(21)
        raw = library(); peaks = raw.max(axis=1)
        ons, cds = [], []
        for _ in range(NRUN):
            isi = rng.integers(6, 11, size=NCOND) if slow else rng.integers(2, 5, size=NCOND)
            ons.append((4 + np.concatenate([[0], np.cumsum(isi[:-1])])).astype(float)); cds.append(rng.permutation(NCOND))
        idx = rng.integers(0, 20, size=n_scored)
        amps = chrono_amps(rng, n_scored, ons)
        sig = make_signal_voxels(rng, ons, cds, idx, amps, raw)
        sd = np.concatenate(sig, axis=1).std(axis=1)
        sig = [s * (snr / sd)[:, None] for s in sig]
        nz = noise_block(rng, n_scored + n_pool)
        data = []
        for r in range(NRUN):
            sg = np.zeros((n_scored + n_pool, T)); sg[:n_scored] = sig[r]
            data.append(BASE + sg + nz[r])
        o = run_glmsingle(data, ons, cds, tr=TR, stimdur=STIMDUR, seed=1, types="ABCD", threads=threads)
        m = o["meta"]
        res = {"n_scored": n_scored, "n_pool_generated": n_pool, "snr": snr, "volumes_per_run": T, "threads": threads,
               "wall_s": m["wall_s"], "cpu_s": m["cpu_s"], "noisepool_size": o["noisepool_size"], "pcnum": o["pcnum"],
               "hrf_index_correct_fraction_B": float(np.mean(o["b_HRFindex"][:n_scored] == idx)),
               "hrf_index_correct_fraction_D": float(np.mean(o["d_HRFindex"][:n_scored] == idx))}
    finally:
        T = T0
    save(f"runtime_{n_scored}_{n_pool}_{'slow' if slow else 'fast'}_thr{threads}{tag}", res)


def check_wrapper():
    """CLI round-trip, determinism under a fixed seed, and converter identity (beta_data == psc/100*|meanvol|*peak)."""
    import subprocess, tempfile
    rng = np.random.default_rng(8)
    raw = library(); ons, cds = schedule(rng)
    # sub-TR onsets to exercise the documented rounding (half-up); shift <= 0.5 s
    ons_sub = [o + rng.integers(0, 10, size=len(o)) / 10.0 for o in ons]
    idx = rng.integers(0, 20, size=30); amps = chrono_amps(rng, 30, ons)
    sig = make_signal_voxels(rng, ons, cds, idx, amps, raw)
    nz = noise_block(rng, 330)
    data = [BASE + np.vstack([np.vstack([s, np.zeros((300, T))]) [:330]]) + n for s, n in zip(sig, nz)]
    with tempfile.TemporaryDirectory() as d:
        z = {"n_runs": NRUN}
        for r in range(NRUN): z[f"data_{r}"] = data[r]; z[f"onsets_{r}"] = ons_sub[r]; z[f"conds_{r}"] = cds[r]
        np.savez(f"{d}/in.npz", **z)
        for k in (1, 2):
            subprocess.run([sys.executable, os.path.join(HERE, "run_glmsingle.py"), "--input", f"{d}/in.npz",
                            "--output", f"{d}/out{k}.npz", "--seed", "7", "--tr", "1", "--stimdur", "1", "--threads", "1"],
                           check=True, capture_output=True)
        a, b = np.load(f"{d}/out1.npz"), np.load(f"{d}/out2.npz")
        res = {"deterministic_same_seed_all_arrays_identical":
               all(np.array_equal(a[k], b[k]) for k in a.files if k != "meta_json")}
        pk = a["hrf_library"].max(axis=1)
        for t in "bcd":
            conv = a[f"{t}_beta_psc"] / 100 * np.abs(a["meanvol"])[:, None] * pk[a[f"{t}_HRFindex"]][:, None]
            res[f"converter_{t}_max_abs_diff"] = float(np.abs(conv - a[f"{t}_beta_data"]).max())
        meta = json.loads(str(a["meta_json"]))
        res["onset_quantization_max_abs_shift_s"] = meta["onset_quantization_max_abs_shift_s"]
        res["meta_versions"] = meta["versions"]
    save("wrapper", res)


if __name__ == "__main__":
    which = sys.argv[1:] or ["units", "f32", "lownoise", "nf"]
    if "f32" in which: check_float32_floor()
    if "wrap" in which: check_wrapper()
    if "rt" in which:
        check_runtime(threads=None); check_runtime(threads=1); check_runtime(slow=True, threads=1)
    if "rt200" in which: check_runtime(n_scored=200, threads=1)
    if "rt20k" in which: check_runtime(n_pool=20000, threads=1)
    if "units" in which: check_units_and_norm()
    if "lownoise" in which:
        check_lownoise(regime="repeat"); check_lownoise(regime="independent")
    if "nf" in which: check_noisefree_cd()
