"""Float32 precision bound for GLMsingle type B vs a float64 fit of the SAME model (frozen before the rerun).

Solver actually used (type B, HRF assumed per library member): ols/fit_model.py:230 -> olsmatrix2(X) with
X = vstack_r(P_r @ D_r(h)) stored as float32 (probe: X float32 (n x p)), then
ols/olsmatrix2.py: part1 = X.T @ X (float32 BLAS) + 0*I (promotes to float64), f = solve(part1, X.T) in float64,
betas = f @ data2 (float32 data) via mtimes_stack, cast to float32 (glm_estimatemodel.py:790), psc = beta*100/|meanvol|
(glm_estimatemodel.py:945-960) with meanvol the float32 mean of the data (:523). No column normalization,
so the normal equations see kappa(X)^2.

Per-voxel relative 2-norm error E = ||b32 - b64|| / ||b64|| (psc units; b64 = float64 fit of the same design,
same HRF, same poly projection, converted with GLMsingle's own meanvol):

  E <= c * [ kappa^2 * (sqrt(n) + 2*sqrt(p)) * u          # float32 Gram X'X (sqrt(n) u, random-rounding model)
                                                          #   + float32 rounding of X (relative u per entry; ||dX||_F <= u sqrt(p) ||X||_2)
           + kappa * u * ||d||_2 / ||X b64||_2             # float32 data and projection P@d: absolute error ~ u*||d||,
                                                          #   d = raw data INCLUDING baseline (why baseline matters)
           + (log2(n) + 4) * u ]                           # float32 meanvol (pairwise sum), 100/meanvol, final float32 casts
  u = 2^-24, c = 1 (fixed a priori; not tuned to observations), kappa = cond_2(X) in float64 for the voxel's HRF.
n = total volumes, p = number of single-trial regressors.
"""
import json, math, os
import numpy as np

U32 = 2.0 ** -24
C = 1.0
FORMULA = ("c*(kappa^2*(sqrt(n)+2*sqrt(p))*u + kappa*u*||d||2/||X b64||2 + (log2(n)+4)*u), "
           "u=2^-24, c=1, kappa=cond2(X) with X=vstack_r(P_r D_r(h)), n=volumes total, p=trials")


def bound(kappa, n, p, data_ratio, u=U32, c=C):
    return c * (kappa ** 2 * (math.sqrt(n) + 2 * math.sqrt(p)) * u
                + kappa * u * data_ratio + (math.log2(n) + 4) * u)


def design_matrix(ons, hrf, T, P):
    """float64 X = vstack_r P @ D_r (T x n_r) for onsets on the volume grid."""
    import scipy.linalg as sl
    blocks = []
    for o in ons:
        D = np.zeros((T, len(o)))
        for i, v in enumerate(np.asarray(o).astype(int)):
            m = min(len(hrf), T - v)
            D[v:v + m, i] = hrf[:m]
        blocks.append(P @ D)
    return sl.block_diag(*blocks)


def kappa_table(ons, lib, T, P):
    return [float(np.linalg.cond(design_matrix(ons, lib[k] / lib[k].max(), T, P))) for k in range(lib.shape[0])]


if __name__ == "__main__":
    import gate_checks as gc
    from glmsingle.ols.make_poly_matrix import make_projection_matrix, make_polynomial_matrix
    rng = np.random.default_rng(11)  # same schedule seed as the float32-floor sweep
    raw = gc.library(); ons, cds = gc.schedule(rng)
    P = make_projection_matrix(make_polynomial_matrix(gc.T, 1))  # maxpolydeg = round(150 s/60/2) = 1
    lib = raw  # per-column peak normalisation is applied inside kappa_table
    out = {"formula": FORMULA, "u": U32, "c": C, "n": gc.NRUN * gc.T, "p": gc.NRUN * gc.NCOND, "maxpolydeg": 1,
           "design": "gate_checks.schedule(default_rng(11)), 4 runs x 36 trials, 150 volumes, TR 1, stimdur 1",
           "kappa_by_library_index": kappa_table(ons, lib, gc.T, P),
           "frozen_before_rerun": True}
    os.makedirs("results", exist_ok=True)
    json.dump(out, open("results/precision_bound_frozen.json", "w"), indent=1)
    print(out["kappa_by_library_index"])
