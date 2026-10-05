"""Spike-only exporter: generator .npz -> flat text files readable by the JVM spike (S1 replaces this)."""
import sys, json, numpy as np, pathlib
src, out = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
out.mkdir(parents=True, exist_ok=True)
z = np.load(src)
m = json.load(open(str(src).replace(".npz", ".manifest.json")))
for k in ["y","nuisance","run_id","ev_onset","ev_cond","ev_run","ev_duration","sample_time","truth_t","truth_kernel","cond_coef","trial_beta"]:
    if k in z.files:
        np.savetxt(out/f"{k}.txt", np.atleast_2d(z[k]).reshape(len(z[k]) if z[k].ndim>1 else 1,-1), fmt="%.17g")
json.dump({"tr": m["cell"]["tr"], "ar": m["cell"]["ar"], "cell_id": m["cell"]["cell_id"]}, open(out/"meta.json","w"))
print({k: z[k].shape for k in z.files})
