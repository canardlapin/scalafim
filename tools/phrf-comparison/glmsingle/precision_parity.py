"""Type-B noise-free parity against the FROZEN float32 bound (results/precision_bound_frozen.json).

Reference = float64 fit of the same model (same design, same HRF = GLMsingle's returned float32 library column,
same poly projection, psc conversion with GLMsingle's own meanvol). Not compared to truth (truth error reported separately).
Negative controls: reference with a wrong HRF index (+1, +10 mod 20) and with a 1-TR onset shift.
"""
import json, os, sys
import numpy as np
import gate_checks as gc
import precision_bound as pb
from run_glmsingle import run_glmsingle
from glmsingle.ols.make_poly_matrix import make_projection_matrix, make_polynomial_matrix

frozen = json.load(open("results/precision_bound_frozen.json"))
assert frozen["frozen_before_rerun"]
T, NR, NC = gc.T, gc.NRUN, gc.NCOND
P = make_projection_matrix(make_polynomial_matrix(T, 1))


def ref_fit(ons, hrf, y_runs):
    X = pb.design_matrix(ons, hrf, T, P)
    y = np.concatenate([P @ y for y in y_runs])
    return X, np.linalg.lstsq(X, y, rcond=None)[0]


def l2(a, b): return float(np.linalg.norm(a - b) / np.linalg.norm(b))


rng = np.random.default_rng(11)
raw = gc.library(); peaks = raw.max(axis=1)
ons, cds = gc.schedule(rng)
idx = np.repeat(np.arange(20), 3); V = len(idx)
amps = gc.chrono_amps(rng, V, ons)
sig = gc.make_signal_voxels(rng, ons, cds, idx, amps, raw)
pool = gc.noise_block(rng, 400)
truth_data = amps * peaks[idx][:, None]
res = {"bound_formula": frozen["formula"], "frozen_in": "results/precision_bound_frozen.json", "baselines": {}}
for base in (1.0, 100.0, 1000.0):
    data = [base + np.vstack([s, p]) for s, p in zip(sig, pool)]
    o = run_glmsingle(data, ons, cds, tr=1., stimdur=1., seed=1, types="B")
    assert np.array_equal(o["trial_order"], np.arange(NR * NC))
    lib = o["hrf_library"].astype(np.float64)
    mv = o["meanvol"].astype(np.float64)
    rows = []
    for v in range(V):
        h = int(o["b_HRFindex"][v]); hrf = lib[h]
        y_runs = [data[r][v] for r in range(NR)]
        X, b64 = ref_fit(ons, hrf, y_runs)
        psc64 = b64 * 100.0 / abs(mv[v])
        err = l2(o["b_beta_psc"][v].astype(np.float64), psc64)
        dnorm = np.sqrt(sum(float(np.sum(y ** 2)) for y in y_runs))
        ratio = dnorm / np.linalg.norm(X @ b64)
        kappa = float(np.linalg.cond(X))
        bd = pb.bound(kappa, frozen["n"], frozen["p"], ratio, u=frozen["u"], c=frozen["c"])
        # kappa consistency with the frozen table (design alone)
        kfz = frozen["kappa_by_library_index"][h]
        # truth error of the float64 reference (data units)
        truth_err = l2(b64 * mv[v] / 100.0 * 0 + b64, truth_data[v])  # b64 is in data units (ref fit of raw data)
        row = {"err": err, "bound": bd, "kappa": kappa, "kappa_frozen": kfz, "ratio": float(ratio), "truth_err_f64": truth_err}
        # negative controls (error of the SAME GLMsingle output vs a wrong reference model)
        for name, hh, sh in (("wrong_hrf_+1", (h + 1) % 20, 0), ("wrong_hrf_+10", (h + 10) % 20, 0), ("onset_shift_1TR", h, 1)):
            ons2 = [np.minimum(oo + sh, T - 1) for oo in ons]
            X2, b2 = ref_fit(ons2, lib[hh], y_runs)
            row[name] = l2(o["b_beta_psc"][v].astype(np.float64), b2 * 100.0 / abs(mv[v]))
        rows.append(row)
    A = {k: np.array([r[k] for r in rows]) for k in rows[0]}
    b = {
        "obs_err_max": float(A["err"].max()), "obs_err_median": float(np.median(A["err"])),
        "bound_min": float(A["bound"].min()), "bound_max": float(A["bound"].max()),
        "frac_voxels_within_bound": float(np.mean(A["err"] <= A["bound"])),
        "min_margin_bound_over_err": float((A["bound"] / A["err"]).min()),
        "median_margin_bound_over_err": float(np.median(A["bound"] / A["err"])),
        "kappa_matches_frozen_max_rel": float(np.max(np.abs(A["kappa"] / A["kappa_frozen"] - 1))),
        "hrf_index_exact": float(np.mean(o["b_HRFindex"][:V] == idx)),
        "f64_reference_vs_truth_max_L2": float(A["truth_err_f64"].max()),
        "neg_control_min_err_over_bound": {k: float((A[k] / A["bound"]).min()) for k in ("wrong_hrf_+1", "wrong_hrf_+10", "onset_shift_1TR")},
        "neg_control_min_err": {k: float(A[k].min()) for k in ("wrong_hrf_+1", "wrong_hrf_+10", "onset_shift_1TR")},
    }
    b["pass"] = b["frac_voxels_within_bound"] == 1.0 and b["hrf_index_exact"] == 1.0
    res["baselines"][f"{base:g}"] = b
    print(base, json.dumps(b)[:1400])
res["pass_all"] = all(v["pass"] for v in res["baselines"].values())
json.dump(res, open("results/precision_parity.json", "w"), indent=1)
print("PASS_ALL", res["pass_all"])
