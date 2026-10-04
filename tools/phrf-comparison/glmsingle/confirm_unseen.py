"""Confirm the frozen float32 bound on unseen designs (plan: results/unseen_design_plan.json). Nothing is tuned."""
import json, numpy as np
import gate_checks as gc
import precision_bound as pb
from run_glmsingle import run_glmsingle
from glmsingle.utils.alt_round import alt_round
from glmsingle.ols.make_poly_matrix import make_projection_matrix, make_polynomial_matrix

plan = json.load(open("results/unseen_design_plan.json"))
frozen = json.load(open("results/precision_bound_frozen.json"))
NR, NC = gc.NRUN, gc.NCOND
out = {"plan": "results/unseen_design_plan.json", "cells": [], "summary": {}}
for sched, spec in plan["schedules"].items():
    T = spec["volumes"]; gc.T = T
    deg = int(alt_round(((T * 1.0) / 60) / 2))
    P = make_projection_matrix(make_polynomial_matrix(T, deg))
    for seed in plan["harness_root_seeds"]:
        rng = np.random.default_rng(seed)
        raw = gc.library(); peaks = raw.max(axis=1)
        ons, cds = [], []
        for _ in range(NR):
            isi = rng.integers(spec["isi_s"][0], spec["isi_s"][1] + 1, size=NC)
            ons.append((4 + np.concatenate([[0], np.cumsum(isi[:-1])])).astype(float)); cds.append(rng.permutation(NC))
        idx = np.repeat(np.arange(20), 3); V = len(idx)
        amps = gc.chrono_amps(rng, V, ons)
        sig = gc.make_signal_voxels(rng, ons, cds, idx, amps, raw)
        pool = gc.noise_block(rng, 400)
        for base in plan["baselines"]:
            data = [float(base) + np.vstack([s, p]) for s, p in zip(sig, pool)]
            o = run_glmsingle(data, ons, cds, tr=1., stimdur=1., seed=1, types="B")
            lib = o["hrf_library"].astype(np.float64); mv = o["meanvol"].astype(np.float64)
            errs, bds = [], []
            for v in range(V):
                h = int(o["b_HRFindex"][v]); y_runs = [data[r][v] for r in range(NR)]
                X = pb.design_matrix(ons, lib[h], T, P)
                y = np.concatenate([P @ y for y in y_runs])
                b64 = np.linalg.lstsq(X, y, rcond=None)[0]
                e = float(np.linalg.norm(o["b_beta_psc"][v] - b64 * 100 / abs(mv[v])) / np.linalg.norm(b64 * 100 / abs(mv[v])))
                ratio = np.sqrt(sum(float(np.sum(yy ** 2)) for yy in y_runs)) / np.linalg.norm(X @ b64)
                bds.append(pb.bound(float(np.linalg.cond(X)), NR * T, NR * NC, ratio, u=frozen["u"], c=frozen["c"]))
                errs.append(e)
            errs, bds = np.array(errs), np.array(bds)
            cell = {"schedule": sched, "seed": seed, "baseline": base, "hrf_index_exact": float(np.mean(o["b_HRFindex"][:V] == idx)),
                    "err_max": float(errs.max()), "bound_min": float(bds.min()), "voxels": V,
                    "n_exceed": int(np.sum(errs > bds)), "min_margin": float((bds / errs).min())}
            out["cells"].append(cell); print(cell, flush=True)
for base in plan["baselines"]:
    c = [x for x in out["cells"] if x["baseline"] == base]
    out["summary"][str(base)] = {"cells": len(c), "voxels": sum(x["voxels"] for x in c), "voxels_exceeding": sum(x["n_exceed"] for x in c),
                                 "min_margin": min(x["min_margin"] for x in c), "hrf_index_exact_all": all(x["hrf_index_exact"] == 1.0 for x in c)}
out["pass"] = all(v["voxels_exceeding"] == 0 for v in out["summary"].values())
json.dump(out, open("results/unseen_design_confirmation.json", "w"), indent=1)
print(json.dumps(out["summary"], indent=1), "PASS" if out["pass"] else "FAIL")
