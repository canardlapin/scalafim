# Motion estimator: scale-invariant Gauss-Newton solve and stopping rule (2026-09-30)

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
- **Stopping rule.** The relative cost drop now divides by `|cost|` itself.
  It used to divide by `max(1e-12, |cost|)`, which declared convergence after
  one step on very low-intensity images; that was found in review.

## Scope of the invariance claim

The **solve, the damping and the stopping rule** no longer depend on the
image intensity scale. The estimator as a whole is **not** fully
intensity-scale invariant: the robust Huber threshold (`MotionControl`
`huberK = 1.5`) is in absolute intensity units, so robust weighting changes
with scale whenever it is active. An independent reviewer's probe measured
this, for example rz 0.0189 at scale 1 against 0.0345 at 1e3 with 0.6/1.4-voxel
shifts. The invariance tests use fixtures whose residuals stay below the
Huber threshold at every tested scale. The fix is a behaviour decision, filed
as Mote `bd-01M3R7YC946W1YRSX87YHBYWWV`, which also records the line-search
"converged" case.

## Evidence

Base `fa2193ab`, branch `motion/scale-invariant-solve-20260930`.

| Gate | Result | Log SHA-256 prefix |
| --- | --- | --- |
| `motionJVM/test` | 97/97, no warnings (includes `VolreggerParitySuite`) | `8b96314dc940d05a` |
| `motionJS/test` | 84/84, no warnings | `c8510d0ef3f38ee4` |
| Mutation: pivot tolerance 0; old absolute cost floor | each caught by its own new test; source restored | local logs |
| New estimator tests on the unmodified code | both fail, reproducing the defects | `0b035ff4f87e1ad8` |

New `MotionSolverScalingSuite` (shared):

- **Intensity-scale invariance with Huber inactive.** At scales 1e-6 and
  1e-9, the pose matches the unscaled pose to 1e-6, with identical iteration
  counts.
- **Singular systems.** A flat frame is not reported as converged.
- **Direct solver oracle.** On badly scaled SPD systems (`S A S`, with scales
  from 1e-8 to 1e3), the relative residual `‖Hx − b‖ / ‖b‖` is below 1e-10.
- **Refusals.** Rank-deficient (rank 5) and indefinite systems are refused,
  and so is a unit-diagonal system whose Schur complement is about 2e-14.
  That last case exercises the relative tolerance itself.

## Independent review

A fresh-context reviewer returned ACCEPT-WITH-FIXES on `a2d96f71`. It
confirmed the following:

- The Jacobi-scaling algebra is correct.
- Gale's pivot semantics compare the Schur diagonal before the sqrt, on the
  portable path on both platforms, so the tolerance is genuinely relative.
- Damping changes the poses by only about 1e-9 on typical data.
- The convergence-flag consumers are only the template refresh, which now
  correctly skips singular frames, and a report writer.

The following review items are addressed:

- The overclaimed invariance, now scoped above.
- The absolute cost floor, now relative.
- The pivot tolerance not being exercised, now covered by the near-singular
  case.

These review items are not addressed:

- The ulp-level asymmetry of H (Gale reads the lower triangle), which
  changes the pose by about 1e-10.
- The measured allocation and conditioning that the ticket asks for.

The existing Volregger parity and estimator suites pass unchanged at their
declared tolerances. Per-step allocation is two 6×6 Gale matrices and one
6×1 matrix; this was not benchmarked, because the per-iteration cost is
dominated by the O(voxels) system build.
