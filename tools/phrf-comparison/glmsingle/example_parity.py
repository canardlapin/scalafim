"""Shipped-example check (examples/example1.ipynb, NSD subj01 nsd01 single slice, OSF k89b2).

GLMsingle ships NO reference output files; the only same-commit reference obtainable is a first run of the
example's own call. This script runs example 1 (default options, as in the notebook cell 14/15) twice from
fresh objects and compares types B, C, D (betasmd) and HRFindex, R2, FRACvalue. Data: work/nsdcore.mat
(downloaded from https://osf.io/download/k89b2/; not committed).
"""
import json, os, time
import numpy as np, scipy.io as sio, scipy.sparse
from glmsingle.glmsingle import GLM_single
HERE = os.path.dirname(os.path.abspath(__file__))
X = sio.loadmat(os.path.join(HERE, "work", "nsdcoreexampledataset.mat" if os.path.exists(os.path.join(HERE, "work", "nsdcoreexampledataset.mat")) else "nsdcore.mat"))
data = [X["data"][0, r] for r in range(X["data"].shape[1])]
design = [scipy.sparse.csr_matrix.toarray(X["design"][0, r]) for r in range(X["design"].shape[1])]
stimdur, tr = int(X["stimdur"][0][0]), int(X["tr"][0][0])
runs = []
for k in range(2):
    opt = dict(wantlibrary=1, wantglmdenoise=1, wantfracridge=1, wantfileoutputs=[0, 0, 0, 0], wantmemoryoutputs=[1, 1, 1, 1], seed=1)
    t0, c0 = time.perf_counter(), time.process_time()
    r = GLM_single(opt).fit(design, data, stimdur, tr, outputdir=None, figuredir=None)
    runs.append((r, time.perf_counter() - t0, time.process_time() - c0))
out = {"n_runs": len(data), "data_shape_run0": list(data[0].shape), "stimdur": stimdur, "tr": tr,
       "wall_s": [x[1] for x in runs], "cpu_s": [x[2] for x in runs]}
for t in ("typeb", "typec", "typed"):
    a, b = runs[0][0][t], runs[1][0][t]
    for f in ("betasmd", "R2", "HRFindex", "FRACvalue"):
        if f not in a: continue
        x, y = np.asarray(a[f], float), np.asarray(b[f], float)
        den = np.nanmax(np.abs(x))
        out[f"{t}.{f}"] = {"max_abs_diff_over_max_abs": float(np.nanmax(np.abs(x - y)) / den) if den > 0 else 0.0,
                           "identical": bool(np.array_equal(x, y, equal_nan=True))}
roi = X["ROI"].astype(bool).reshape(-1)
out["roi_voxels"] = int(roi.sum())
out["typed_HRFindex_hist_roi"] = np.bincount(np.asarray(runs[0][0]["typed"]["HRFindex"]).reshape(-1)[roi].astype(int), minlength=20).tolist()
out["n_voxels_slice"] = int(roi.size)
out["typed_pcnum"] = int(runs[0][0]["typed"]["pcnum"]); out["typed_noisepool_size"] = int(np.sum(runs[0][0]["typed"]["noisepool"]))
out["typed_FRACvalue_median_roi"] = float(np.median(np.asarray(runs[0][0]["typed"]["FRACvalue"]).reshape(-1)[roi]))
os.makedirs(os.path.join(HERE, "results"), exist_ok=True)
json.dump(out, open(os.path.join(HERE, "results", "example1_parity.json"), "w"), indent=1)
print(json.dumps(out)[:2500])
