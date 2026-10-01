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

## Solve-fallback provenance and prepared corrections (S2 review)

**Fallback reporting.** fmriAR's `.apply_acvf_correction_result` silently returns the raw
autocovariance when the solved leading block fails `rcond >= 1e-6`, the solve errors, the
solution is non-finite, or the corrected lag-zero variance is not positive. Scalafim matches
that numeric behaviour exactly (the raw vector is kept) but now reports it:
`RunCorrection.SolveFallback(CorrectionFallback)` with `IllConditionedBlock`, `SingularSystem`,
`NonFiniteSolution`, `NonPositiveVariance` and `NonPositiveRawVariance`. `NoiseFit.corrections`
and `NoiseAcvfEstimate.corrections` take it from the per-run estimation, overriding the
prepare-time gate status, so a run can no longer be reported `Applied` after using raw.
`NoiseAcvfUnit.fallback` carries the reason per unit. Each path is forced in
`AcvfBiasFallbackSuite` (the solver is injectable for the two paths a real LU cannot reach after
the condition gate), plus end-to-end checks through `NoiseAcvf` and `NoiseFit` with a
hand-built correction.

Difference: when the raw lag-zero variance is not positive R returns an empty autocovariance
(a null fit); scalafim keeps its existing raw behaviour (zero coefficients of the requested
length) and reports `NonPositiveRawVariance`.

**Prepared corrections.** `AcvfBias.prepare(design, layout, budget, targetOrder)` builds the bias
matrices and keeps the design basis once. The result is accepted by
`ArEstimation.fitNoise(..., prepared)`, `NoiseFit.estimate(..., prepared)` and
`NoiseAcvf.estimate(..., prepared)`. It is bound to its design and layout: `AcvfBias.bind`
rejects a different row count (`DesignRowMismatch`) or an unequal layout
(`PreparedCorrectionLayoutMismatch`), and still checks orthogonality of every residual set
(`DesignResidualMismatch`). Prepared and `DesignCorrected` calls share one code path and are
tested bit-identical (plan coefficients, gamma, sigma2, statuses, acvf; run and global pooling;
single-column residual sets). Error order for the policy path changed slightly: lag-budget errors
now precede the orthogonality check.

Mutation check on the new reporting (sha256 before and after, both restores verified):
`NoiseAcvf.scala` `9f7a5080c50179be6f23a089427564a36c50514f08e9a1046328084d48c73512`,
`AcvfBias.scala` `8c36f28dcd7a0ea22f64eaf5f272921c9138eb775f73900d1d86dbf9d44dd90a`.
1. Fallback reason dropped when assembling per-run status: 2 tests fail (NoiseAcvf and NoiseFit
   reporting).
2. Non-positive-variance check disabled in `correct`: 3 tests fail (the unit path and both
   reporting paths).

## Review of 8fc0a5df: status hole, binding (follow-up)

- **Status independent of units.** A run whose raw lag-zero variance is not positive has no
  unit, so its fallback used to be lost and the gate's `Applied` was reported. Reproduced first
  (per-run intercepts, run 2 residuals exactly zero: `Applied, Applied`), then fixed: per-run
  status is now computed alongside the units (`NoiseAcvf.RunUnits`) and reads
  `Applied, SolveFallback(NonPositiveRawVariance)`. Runs that never reach a solve are
  `RunCorrection.NotAttempted(FewerThanTwoObservations | NoLagZeroPairs)`; gate-rejected and
  uncorrected runs keep their gate status. Global pooling's pooled unit keeps only the first
  run's fallback as a summary; the full picture is `corrections`.
- **Binding is enforced.** Prepared overloads and `AcvfBias.bind` take the design. It must match
  the prepared one by an exact fingerprint (rows, columns, FNV-1a over `doubleToLongBits` of every
  entry; the rank it was prepared with is stored in the fingerprint, and is a function of those
  entries so it is not recomputed per call): `PreparedCorrectionDesignMismatch` otherwise. A nested
  design therefore cannot be substituted. The prepared value also records its budget and target
  order, and `bind` refuses a different order (`PreparedCorrectionOrderMismatch`) for every
  budget, even though a `Fixed` budget's matrices would not change.
- **Fail fast.** The unprepared path computes the basis and validates orthogonality before any
  bias matrix is built (a bad-residual error now wins over a later lag-budget error).
- **Tests.** Prepared vs unprepared bit-identity with a fallback run and with Auto order (statuses
  and bias matrices compared bitwise), nested/tweaked-design refusal, order refusal, `NotAttempted`.
- **Counts excluding the reviewer's `ProbeTmpSuite`:** JVM 141 (142 including it); JS 139.
- **Mutation** on the fix (`NoiseAcvf.scala` sha256
  `23cb3e9e9f28051758d6bfcccbad46991990d9dee507226c8151a47335e81b66`, restore verified): taking
  the fallback from the unit instead of the pooled result reopens the hole and fails 2 tests.

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

- `arJVM/test` (at the first follow-up commit): 120 passed, 0 failed (includes a reviewer's print-only `ProbeTmpSuite`, left untouched and uncommitted).
- `arJS/test`: 117 passed, 0 failed.
- `scalafimCompileAll`: exit 0, no warnings (the build is `-Werror`).
- `scalafimTestAll` was not run.

`NoiseFit.estimate` now builds the bias matrices once (`ArEstimation.fitNoisePrepared`).
The generated fixture emits each case as its own lazy val because a single Vector literal
exceeded the JVM's 64 KB static-initialiser limit.
