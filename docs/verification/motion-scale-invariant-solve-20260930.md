# Motion estimator: scale-invariant Gauss-Newton solve (2026-09-30)

Mote: `bd-01KXYJPWYV08VFWRMCYYDPZCAV`, the `MotionEstimator.solve6` target
only. The other targets in the audit are unchanged:

- The HRF-local `Mat`/`Vec`.
- The design QR adapter.
- MVPA `Rsa.solveLinearSystem`. It is left for the UMVPA relational packets
  (M2.x), which rework RSA.

## Defects

`solve6` was a private Gauss-Jordan elimination with an absolute pivot cutoff
of `1e-12`. The Levenberg-Marquardt damping added an absolute `λ·1e-6` to each
diagonal. The normal system scales with intensity², so both constants made the
estimate depend on the image intensity scale.

When the solve failed, the level loop also set `levelConverged = true`, so a
singular system was reported as `converged` in `FrameFitDiagnostics`.

Both defects were reproduced on the unmodified code (log `0b035ff4f87e1ad8…`),
where both new estimator tests fail:

- A 0.6-voxel x shift estimated on images scaled by 1e-6 did not match the
  unscaled estimate.
- A spatially flat moving frame, whose image gradients are all zero, was
  reported as converged.

## Change

- **Solver.** `solveNormal6` factors the Jacobi-scaled matrix
  `D^-1/2 H D^-1/2`, which has a unit diagonal, with Gale's Cholesky, using a
  relative pivot tolerance of 1e-12, and then unscales the solution. There is
  no private generic elimination left. The solver refuses:
  - a non-positive or non-finite diagonal;
  - a failed factorization;
  - a non-finite solution.
- **Damping floor.** The floor is `λ · max(|h_dd|, 1e-6 · maxDiag)`, relative
  to the largest diagonal, so a zero system stays singular. For
  typical-intensity data, `|h_dd| ≫ 1e-6`, so the damping is unchanged up to a
  relative 1e-6 / |h_dd|.
- **Convergence.** A failed solve ends the level without convergence:
  `converged = levelConverged && !levelSolverFailed`.

## Evidence

Base `fa2193ab`, branch `motion/scale-invariant-solve-20260930`.

| Gate | Result | Log SHA-256 prefix |
| --- | --- | --- |
| `motionJVM/test` | 95/95, no warnings (includes `VolreggerParitySuite`) | `c55b040b1738bdef` |
| `motionJS/test` | 82/82, no warnings | `6a8dcd18bf539e28` |
| New estimator tests on the unmodified code | both fail, reproducing the defects | `0b035ff4f87e1ad8` |

New `MotionSolverScalingSuite` (shared):

- **Intensity-scale invariance.** The pose on images scaled by 1e-6 matches
  the unscaled pose to 1e-6, with identical iteration counts.
- **Singular systems.** A flat frame is not reported as converged.
- **Direct solver oracle.** On badly scaled SPD systems (`S A S`, with scales
  from 1e-8 to 1e3), the relative residual `‖Hx − b‖ / ‖b‖` is below 1e-10.
- **Refusals.** Rank-deficient (rank 5) and indefinite systems are refused.

The existing Volregger parity and estimator suites pass unchanged at their
declared tolerances. Per-step allocation is two 6×6 Gale matrices and one
6×1 matrix; this was not benchmarked, because the per-iteration cost is
dominated by the O(voxels) system build.
