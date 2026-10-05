"""Thin wrapper around the official GLMsingle (family T-G of the PHRF comparison).

Pinned: github.com/cvnlab/GLMsingle @ 1ab54a65edd3ea41a6133d4b4ecb78a9c7296684
(see pinned_commit.txt, requirements.lock, and docs/verification/phrf-cmp-v0b-20261001.md).
Reference: Prince, Charest, Kurzawski, Pyles, Tarr & Kay (2022), eLife 11:e77599.

Python API
----------
run_glmsingle(data_runs, onsets_runs, conds_runs, *, tr, stimdur, seed,
              types="ABCD", params_override=None, threads=None) -> dict

Inputs (all per run r = 0..R-1, runs in the order given):
  data_runs[r]   float array (V, T_r): UNWHITENED voxel time series in DATA units
                 (raw scanner-like units; GLMsingle derives its own voxel mean).
  onsets_runs[r] float array (n_r,): trial onsets in SECONDS from the start of run r.
                 GLMsingle's design is volume-resolution, so each onset is rounded
                 to the nearest volume, round-half-up: vol = floor(onset/tr + 0.5).
                 Two trials in one volume raise ValueError (GLMsingle forbids it).
  conds_runs[r]  int array (n_r,): condition id in 0..C-1 for each trial. Every
                 condition should occur in every run (required by the GLMdenoise /
                 fracridge cross-validation, which folds by run).
  tr, stimdur    seconds. stimdur selects the canonical HRF library (20 HRFs).
  seed           int, recorded and passed as params["seed"]. Estimation is
                 deterministic; GLMsingle's only use of the global numpy RNG is a
                 jitter in a diagnostic figure that is disabled here (figuredir=None).
  types          subset of "ABCD" to return (memory outputs only, no files written).
  params_override dict merged over the wrapper's params (default options are the
                 GLMsingle defaults; only output/seed params are set here).

Output dict (numpy arrays and plain scalars):
  meta                dict: commit, versions, tr, stimdur, seed, n_trials, V, timing,
                      onset quantization (max_abs_shift_s per run)
  trial_order         (n_trials,) int: chronological position k -> index into the
                      concatenation of the input trials (runs in input order, trials
                      in input order within run). Betas below are in chronological
                      order (GLMsingle convention); to get input order use
                      out_in_input_order[trial_order] = out_chrono.
  trial_run, trial_cond, trial_vol   chronological order
  hrf_library         (20, L) the peak-1 library actually used (TR-sampled)
  a_beta_psc (V,), a_beta_data (V,), a_R2 (V,)                       type A (ON-OFF)
  {b,c,d}_beta_psc (V, n_trials)   GLMsingle output, percent signal change
  {b,c,d}_beta_data (V, n_trials)  = beta_psc/100 * |meanvol| * peak(h_used)
                                   (peak(h_used)=1 for the normalised library)
  {b,c,d}_HRFindex (V,) 0-based library index; {b,c,d}_R2 (V,) percent
  meanvol (V,)        the voxel mean GLMsingle used (mean over all volumes, all runs)
  c,d: pcnum (int), noisepool (V,) bool, noisepool_size (int);  d: FRACvalue (V,)

CLI
---
python run_glmsingle.py --input in.npz --output out.npz --seed 1 --tr 1.0 --stimdur 1.0
  in.npz keys: n_runs; data_{r}, onsets_{r}, conds_{r} for r in 0..n_runs-1.
  out.npz keys: as above (meta stored as a JSON string under "meta_json").
"""
from __future__ import annotations

import argparse
import contextlib
import io
import json
import os
import sys
import time

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
PINNED_COMMIT = "1ab54a65edd3ea41a6133d4b4ecb78a9c7296684"


def build_design(onsets_runs, conds_runs, n_vols, tr, n_cond):
    """Volume-resolution 0/1 design matrices (T_r, C) plus chronological trial table."""
    design, table, shifts = [], [], []
    for r, (ons, cds, T) in enumerate(zip(onsets_runs, conds_runs, n_vols)):
        ons = np.asarray(ons, float)
        cds = np.asarray(cds, int)
        vol = np.floor(ons / tr + 0.5 + 1e-9).astype(int)
        if vol.min(initial=0) < 0 or vol.max(initial=0) >= T:
            raise ValueError(f"run {r}: onset outside data")
        if len(np.unique(vol)) != len(vol):
            raise ValueError(f"run {r}: two trials share a volume after rounding")
        D = np.zeros((T, n_cond))
        D[vol, cds] = 1
        design.append(D)
        shifts.append(float(np.max(np.abs(vol * tr - ons))) if len(ons) else 0.0)
        for i in np.argsort(vol, kind="stable"):
            table.append((r, i, int(cds[i]), int(vol[i])))
    return design, table, shifts


def run_glmsingle(data_runs, onsets_runs, conds_runs, *, tr, stimdur, seed,
                  types="ABCD", params_override=None, threads=None):
    from importlib import metadata
    from threadpoolctl import threadpool_limits
    from glmsingle.glmsingle import GLM_single

    data = [np.ascontiguousarray(d, dtype=np.float32) for d in data_runs]
    n_cond = int(max(np.max(c) for c in conds_runs)) + 1
    design, table, shifts = build_design(
        onsets_runs, conds_runs, [d.shape[-1] for d in data], tr, n_cond)

    want = [1 if t in types.upper() else 0 for t in "ABCD"]
    params = {
        "wantfileoutputs": [0, 0, 0, 0],
        "wantmemoryoutputs": want,
        "seed": int(seed),
        "wantglmdenoise": int(want[2] == 1 or want[3] == 1),
        "wantfracridge": int(want[3] == 1),
    }
    # GLMsingle defaults are kept for everything else (incl. n_pcs=10, pcstop=1.05,
    # fracs=linspace(1,0.05,20), wantpercentbold=1, wantautoscale=1).
    if params_override:
        params.update(params_override)
    glm = GLM_single(params)

    buf = io.StringIO()
    t_wall, t_cpu = time.perf_counter(), time.process_time()
    with contextlib.redirect_stdout(buf), threadpool_limits(limits=threads):
        res = glm.fit(design, data, float(stimdur), float(tr),
                      outputdir=None, figuredir=None)
    wall, cpu = time.perf_counter() - t_wall, time.process_time() - t_cpu

    # position in input concatenation
    offs = np.cumsum([0] + [len(o) for o in onsets_runs])
    order = np.array([offs[r] + i for (r, i, _, _) in table])
    out = {
        "trial_order": order,
        "trial_run": np.array([t[0] for t in table]),
        "trial_cond": np.array([t[2] for t in table]),
        "trial_vol": np.array([t[3] for t in table]),
        "hrf_library": np.asarray(glm.params["hrflibrary"]).T.copy(),
    }
    lib = np.asarray(glm.params["hrflibrary"])  # (L, 20)
    peaks = lib.max(axis=0)
    flat = lambda a: np.asarray(a).reshape(a.shape[0], -1) if np.ndim(a) > 1 else np.asarray(a).reshape(-1)
    for t in "abcd":
        if t.upper() not in types.upper():
            continue
        r = res["type" + t]
        mv = flat(r["meanvol"]).reshape(-1)
        if t == "a":
            psc = np.asarray(r["betasmd"]).reshape(len(mv), -1)[:, 0]
            scale = np.abs(mv) / 100.0 * float(np.max(glm.params["hrftoassume"]))
            out["a_beta_psc"], out["a_beta_data"] = psc, psc * scale
            out["a_R2"] = np.asarray(r["onoffR2"]).reshape(-1)
            out["meanvol"] = mv
            continue
        psc = np.asarray(r["betasmd"]).reshape(len(mv), -1)
        hi = np.asarray(r["HRFindex"]).reshape(-1).astype(int)
        out[t + "_beta_psc"] = psc
        out[t + "_beta_data"] = psc / 100.0 * np.abs(mv)[:, None] * peaks[hi][:, None]
        out[t + "_HRFindex"] = hi
        out[t + "_R2"] = np.asarray(r["R2"]).reshape(-1)
        out["meanvol"] = mv
        if t in "cd":
            out["pcnum"] = int(r["pcnum"])
            npool = np.asarray(r["noisepool"]).reshape(-1).astype(bool)
            out["noisepool"], out["noisepool_size"] = npool, int(npool.sum())
        if t == "d":
            out["d_FRACvalue"] = np.asarray(r["FRACvalue"]).reshape(-1)
    out["meta"] = {
        "glmsingle_commit": PINNED_COMMIT, "tr": tr, "stimdur": stimdur, "seed": int(seed),
        "n_voxels": int(data[0].shape[0]), "n_trials": len(table), "types": types.upper(),
        "wall_s": wall, "cpu_s": cpu, "threads": threads,
        "onset_quantization_max_abs_shift_s": shifts,
        "versions": {p: metadata.version(p) for p in
                     ("GLMsingle", "numpy", "scipy", "scikit-learn", "fracridge", "numba")},
        "log_tail": buf.getvalue()[-2000:],
    }
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--input", required=True)
    ap.add_argument("--output", required=True)
    ap.add_argument("--seed", type=int, required=True)
    ap.add_argument("--tr", type=float, required=True)
    ap.add_argument("--stimdur", type=float, required=True)
    ap.add_argument("--types", default="ABCD")
    ap.add_argument("--threads", type=int, default=None)
    a = ap.parse_args(argv)
    z = np.load(a.input)
    R = int(z["n_runs"])
    out = run_glmsingle([z[f"data_{r}"] for r in range(R)], [z[f"onsets_{r}"] for r in range(R)],
                        [z[f"conds_{r}"] for r in range(R)], tr=a.tr, stimdur=a.stimdur,
                        seed=a.seed, types=a.types, threads=a.threads)
    meta = out.pop("meta")
    np.savez_compressed(a.output, meta_json=json.dumps(meta), **out)
    print(json.dumps({k: v for k, v in meta.items() if k != "log_tail"}))


if __name__ == "__main__":
    sys.exit(main())
