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

Follow-up cases (review nits), all inside the same 1e-10 bar:

| Case | Coverage | Achieved max abs error |
| --- | --- | --- |
| rank-deficient design (duplicated column), 2 runs, global, p = 2 | A, phi, gamma, sigma2 | 5.1e-14 |
| column constant on run 1 and zero on run 2 beside per-run intercepts, run pooling, p = 2 | A, phi, gamma, sigma2 | 4.7e-14 |
| run 2 entirely censored, run and global pooling, p = 1 | A (identity for the empty run), phi, gamma, sigma2 (NA carried as NaN, asserted `None`) | 5.0e-14 |
| censoring leaving isolated single-row segments, global, p = 2 | A, phi, gamma, sigma2 | 5.0e-14 |
| runs of 70 and 14 rows, auto order (p_max 4), budget 25, run pooling | A, phi, gamma, sigma2, and per-run status: only the 14-row run is `IllConditioned` (R agrees) | 5.0e-14 |
| fmrireg `.ar_correction_lag_budget` (6 cases: multiple runs, censoring, run-restricted rdf, ceiling binding, order floor, rank-deficient design) | integer budget, exact | exact |

The adaptive-budget fixture calls `fmrireg:::.ar_correction_lag_budget` directly, so the
formula is checked against R, not against our reading of it. Overall maximum across every
parity comparison: 5.1e-14.

The residual error is the 13-digit serialisation floor, not algorithmic.

Reciprocal condition numbers are compared as a ratio inside [0.5, 2] for every case above 1e-12 (including the near-gate 1.16e-7 case); only numerically singular matrices, where the estimate is rounding noise, are checked for the same side of the gate instead.

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

Source: `AcvfBias.scala`, sha256 `aa3993887cdb14321146beff80e5e9956f56564366c67f93810c36c238df520c`
(first pass, before the follow-up: `71915254ad847ab1e99b355c9b25441d6341434f6669c8f7e287c2593b2512dc`).

(Re-run after the follow-up commit on the 120-test JVM suite; first-pass figures in
brackets.)

1. Per-run centring removed from the A build (`-= 0.0 * mean`): 22 of 120 JVM tests fail
   [18 of 94]. The single-run cases with an intercept still pass, as they should:
   a global intercept makes centring redundant.
2. Lag operator allowed to cross run boundaries (run-equality guard replaced by a
   tautology): 29 of 120 fail [17 of 94].

Both mutations were reverted by copying the original back; the restored file's sha256
matched the value above before and after, in both passes.

## Deliberate differences from R

- Failures are typed `ArError`s or reported statuses, not warnings. A design leaving no
  residual degrees of freedom is `NoResidualDegreesOfFreedom` (R warns and proceeds at lag 1).
  Rejected conditioning is `RunCorrection.IllConditioned` on `NoiseFit`/`PreparedCorrection`,
  and the run falls back to raw as in R.
- A fixed AR order beyond the estimable lags remains `ArOrderNotEstimable`, matching the
  existing raw path; R silently takes `min(p, p_cap)`.
- Pair counts in `NoiseAcvfUnit.pairs` are summed over residual columns: scalafim's pooled
  accumulator adds one pair per (row pair, column) and divides the product sum by that total,
  whereas fmriAR's `.pooled_acvf_segments` averages the product sum over columns and keeps the
  per-column count (`pairs[h]`). Every estimate is the same ratio either way, so acvf, phi and
  gamma are unaffected; the reported count is exactly R's times the number of columns, and the
  parity test divides by it. Global pooling weights use surviving observations, not these
  counts, so they are unaffected too.
- `CorrectionBudget.Adaptive(ceiling)` with `ceiling < 1` is `InvalidCorrectionLag`; fmrireg
  silently falls back to 25.
- One rank source: Gale's column-pivoted QR (default tolerance) supplies the design basis, the
  residual df behind the cap, and the per-run rdf of the adaptive budget, so they cannot
  disagree (`AcvfBias.numericalRank`; near-deficient test in `AcvfBiasSuite`). R's `qr()` uses
  tol 1e-7 on column norms and may differ on near-deficient designs.
- Parcel pooling and ARMA are not offered with a design (R also refuses them).
- `NoiseAcvf.estimate` with `CorrectionBudget.Adaptive` takes the AR order from `maxLag`.
- ArError gained five cases appended near the end of the enum; this is the one non-additive
  hunk that may need a manual merge on a diverged integration line.
- Reference revision ca27f27 is a descendant of the f563f2d revision pinned for the existing
  fixtures; the existing generator and fixtures were not touched.

## Verification

Run through `python3 tools/build/sbt-warm`:

- `arJVM/test`: 120 passed, 0 failed (includes a reviewer's print-only `ProbeTmpSuite`, left untouched and uncommitted).
- `arJS/test`: 117 passed, 0 failed.
- `scalafimCompileAll`: exit 0, no warnings (the build is `-Werror`).
- `scalafimTestAll` was not run.

`NoiseFit.estimate` now builds the bias matrices once (`ArEstimation.fitNoisePrepared`).
The generated fixture emits each case as its own lazy val because a single Vector literal
exceeded the JVM's 64 KB static-initialiser limit.
