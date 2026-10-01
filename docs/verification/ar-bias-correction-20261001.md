# AR residual-bias correction: parity receipt (2026-10-01)

Scope: slices 1 and 2 of the scalafim AR parity plan. `modules/ar` gains fmriAR's
design-aware residual-bias (ACVF) correction behind an explicit `EstimationPolicy`.
The non-design-aware path was already at parity with fmriAR 0.3.3 and is unchanged.

Branch `ar/bias-correction-20261001`, bead `bd-01M3WA0GGQ4Q0064E4FEP7WGF1`.

## What was implemented

| Piece | Location |
| --- | --- |
| `EstimationPolicy` (`Raw` default, `DesignCorrected(design, budget)`), `CorrectionBudget` (`Fixed`, `Adaptive`), `RunCorrection` | `EstimationPolicy.scala` |
| Bias matrix `A` per run, orthogonality validation, df cap, lag-budget rules, rcond gate, `A gamma = gamma_raw` solve | `AcvfBias.scala` |
| `noise_acvf()` mirror, raw and corrected | `NoiseAcvf.scala` |
| gamma / sigma2 / per-run correction status beside the plan | `NoiseFit.scala` |
| `fitNoise(..., policy)` overloads, correction threaded through `PooledAutocovariance.through` | `ArEstimation.scala` (minimal edit) |
| Five typed errors | `ArError.scala` |

Linear algebra is Gale only: column-pivoted QR (basis and rank), `DMat` product, LU `solve`,
and `conditionEstimate`. Nothing was hand-rolled; Gale already provides an estimate-based
condition number, so no capability was missing. Hot loops (the `A` build) use
`Array[Double]` and `while`.

`WhiteningPlan.scala` is untouched. fmriAR's plan carries `gamma` and `sigma2`; here they
are returned by `NoiseFit` next to the plan instead (deferred: carrying them on the plan
itself needs a `WhiteningPlan` edit).

## Parity with fmriAR (reference: fmriAR 0.3.3 at ca27f27)

Inputs are deterministic (sine-hash designs and noise); residuals are OLS residuals computed
in R; designs and residuals are rounded to the receipt's 13 significant digits before R
fits anything, so Scala receives fmriAR's inputs bit for bit. No RNG runs at check time.
Generator: `modules/ar/tools/generate_fmriar_bias_parity.R` (uses
`tools/r-parity/receipt_serialization.R`). Fixture:
`modules/ar/shared/src/test/scala/scalafim/fmri/ar/fixtures/FmriArBiasRFixture.scala`.
Suite tolerance is 1e-10; achieved maxima are in the table.

| Quantity | Cases | Achieved max abs error |
| --- | --- | --- |
| `acvf_bias_matrix()` A entries | 5 designs: shared intercept + drift, per-run intercepts with unequal runs and adjacent/isolated censoring, a single shared intercept over two runs (centring not absorbed), no intercept at all over three unequal runs with censoring, budget above rdf | 5.0e-14 |
| `fit_noise(design=)` phi | single run p = 1..4 and auto; two censored runs p = 1, 2, auto with global and run pooling; three runs, no intercept, global and run | 4.9e-14 |
| `fit_noise(design=)` gamma | same fits | 4.7e-14 |
| `fit_noise(design=)` sigma2 | same fits | 4.0e-14 |
| `noise_acvf(design=)` acvf, pairs, segment counts, `corrected` flag | 4 cases (global and run) | 4.0e-14, pairs exact |
| budget above rdf | n = 16, rank 10, budget 8 -> df 6, lag capped to 5, `budgetCapped` | exact |
| rcond gate | n = 60, budget 57, R rcond 1.16e-7: run reported `IllConditioned`, phi and gamma equal R's uncorrected fit | 4.9e-14 |

The residual error is the 13-digit serialisation floor, not algorithmic.

Reciprocal condition numbers are compared as a ratio inside [0.5, 2]. Gale's
`conditionEstimate` and LAPACK's `rcond` are different 1-norm estimators; the 1e-6 gate only
needs the order of magnitude, and the rejection case sits a factor of 8 below the floor.

## Theory test

`AcvfBiasTheorySuite` computes E[gamma_raw_h] = (1/pairs_h) trace(R' T_h R Sigma) from the
hat matrix (a normal-equations inverse local to the test), per-run centring and a known
covariance, without the lag decomposition the production code uses, and compares it with
`A gamma_true` at 1e-12.

- AR(1) Sigma on two runs of six rows (every within-run lag is inside the budget, so there is
  no tail): exact, with and without censoring that splits a run into segments.
- Noise that is exactly zero beyond lag 2, budget 5, two runs, with and without censoring:
  exact, and solving `A gamma = E[gamma_raw]` recovers gamma to 1e-9.

## Mutation check

Source: `AcvfBias.scala`, sha256 `71915254ad847ab1e99b355c9b25441d6341434f6669c8f7e287c2593b2512dc`.

1. Per-run centring removed from the A build (`-= 0.0 * mean`): 18 of 94 JVM tests fail
   (3 theory, 15 parity). The single-run cases with an intercept still pass, as they should:
   a global intercept makes centring redundant.
2. Lag operator allowed to cross run boundaries (run-equality guard replaced by a
   tautology): 17 of 94 fail (3 theory, 14 parity).

Both mutations were reverted by copying the original back; the restored file's sha256
matched the value above before and after.

## Deliberate differences from R

- Failures are typed `ArError`s or reported statuses, not warnings. A design leaving no
  residual degrees of freedom is `NoResidualDegreesOfFreedom` (R warns and proceeds at lag 1).
  Rejected conditioning is `RunCorrection.IllConditioned` on `NoiseFit`/`PreparedCorrection`,
  and the run falls back to raw as in R.
- A fixed AR order beyond the estimable lags remains `ArOrderNotEstimable`, matching the
  existing raw path; R silently takes `min(p, p_cap)`.
- Pair counts in `NoiseAcvfUnit.pairs` are summed over residual columns (R reports per
  column). Pooling weights are unaffected.
- The design basis comes from Gale's column-pivoted QR rank, not LINPACK `dqrdc2` (tol 1e-7).
  Identical for full-rank designs; may differ on numerically near-deficient ones.
- Parcel pooling and ARMA are not offered with a design (R also refuses them).
- `NoiseAcvf.estimate` with `CorrectionBudget.Adaptive` takes the AR order from `maxLag`.
- ArError gained five cases appended near the end of the enum; this is the one non-additive
  hunk that may need a manual merge on a diverged integration line.
- Reference revision ca27f27 is a descendant of the f563f2d revision pinned for the existing
  fixtures; the existing generator and fixtures were not touched.

## Verification

Run through `python3 tools/build/sbt-warm`:

- `arJVM/test`: 94 passed, 0 failed (44 new: 29 parity, 4 theory, 11 contract; 50 pre-existing).
- `arJS/test`: 92 passed, 0 failed (the same 44 new; the JVM-only guardrail suite accounts for the difference).
- `scalafimCompileAll`: exit 0, no warnings (the build is `-Werror`).
- `scalafimTestAll` was not run.
