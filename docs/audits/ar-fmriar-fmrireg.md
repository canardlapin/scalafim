# AR audit: fmriAR/fmrireg consumer closure

Mote: `bd-01M3WA0GGQ4Q0064E4FEP7WGF1`. Assessment and implementation: 2026-10-07.
This closes the three gaps identified in the status assessment: public GLS
correction policy and outcomes, corrected end-to-end consumer parity, and the
consolidated audit/regeneration ledger. It does not claim complete feature
coverage of the R packages or a new PHRF pilot qualification.

The coverage statements below describe the locked fmriAR 0.3.3 reference.
The [2026-10-09 delta assessment](#fmriar-040041-delta-assessment--2026-10-09)
identifies correctness and performance changes in fmriAR 0.4.0/0.4.1 that
ScalaFIM has not yet adopted.

## Supported public correction contract

The low-level AR API continues to accept `EstimationPolicy.DesignCorrected` with
fixed or adaptive budgets. Public fitting now accepts an inspectable
`ArBiasCorrection.OlsDesign` policy through `AutocorrelationConfig` and legacy
`ArOptions`. `ArBiasCorrection.Ols` selects the fmrireg adaptive budget with a
ceiling of 25; `ArBiasCorrection.olsDesign(ceiling)` validates another ceiling.
Raw estimation remains the explicit compatibility default.

```scala
for
  correction <- ArBiasCorrection.olsDesign(25)
  ar <- AutocorrelationConfig(order = 2, global = true, biasCorrection = correction)
yield FitPlan(model, FitStrategy.GeneralizedLeastSquares(ar))
```

The design used for correction is the effective design that produced the initial
OLS residuals, including selected rows and run-local coefficient projection.
Dense, voxelwise, runwise, bounded pooled and shared reduced-rank preparation
use the same AR provider. Bounded preparation validates each residual block,
accumulates raw lag statistics over the complete correction budget, merges them
with Gale `ExactSum`, and applies correction once after the spatial reduction.
It never retains the population residual matrix.

Correction outcomes are reported on `ArRunDiagnostic.correction` for shared
noise estimation and `voxelwiseCorrections` for voxelwise estimation. They
preserve `Applied`, `IllConditioned`, `SolveFallback` and `NotAttempted`, including
fully censored runs. `ArDiagnostics.biasCorrection` records the requested
policy. Spatial subsetting and merging preserve voxel alignment and outcomes.
Completed corrected GLS artifacts use schema v2 and retain the outcomes;
uncorrected schema-v1 artifacts remain readable with their existing encoding.
Restoration checks the correction policy as part of the preparation identity.

OLS correction requires estimated coefficients and exactly one estimation pass.
Typed configuration refuses correction with fixed coefficients or iterated GLS;
robust AR configuration also refuses it. GLS and IRLS residuals are not the
ordinary OLS residuals of the original design. Applying that projection's bias
matrix to them would claim an unsupported correction. Raw iterative/robust
estimation remains available. A future corrected iterative/weighted estimator
needs a separately derived residual operator and independent oracle.

## Gap and disposition table

| Area | R reference / ScalaFIM behavior | Evidence | Disposition |
| --- | --- | --- | --- |
| ACVF and fixed-order Yule-Walker | Pair-count lag products, per-run centering, censor/run segmentation and positive-definite repair precede estimation. | `FmriArParitySuite`, `FmriArBiasParitySuite`, `ArEstimationSuite` | Covered for the admitted pure-AR family. |
| Automatic order and stationarity | BIC search, effective-observation order cap and stationarity/root-margin enforcement. | `FmriArParitySuite`, adversarial estimation tests | Covered through the low-level AR API; public fitting still requests a fixed AR order. |
| Global/run pooling | Estimate each run, observation-weight coefficients and re-enforce stationarity; preserve a filter per run for run pooling. Spatial summaries use exact cross-voxel reduction. | Raw R fixtures; `CorrectedNoiseSummarySuite`; corrected GLS R cases for both poolings | Covered for pooled ACVF. |
| OLS residual-projection correction | Build per-run bias matrices from the matching design, correct over the complete lag budget before truncation, apply rdf and rcond guards. | Analytic `AcvfBiasTheorySuite`; bias fixtures and correction/fallback suites | Delivered and available through public GLS. |
| Adaptive lag budget | Call fmrireg's actual pinned `.ar_correction_lag_budget` function; Scala derives the same budget. | Six bias-fixture budget cases and corrected GLS R cases | Covered; invalid ceilings are typed errors rather than silently reset to 25. |
| Public dense/runwise/voxelwise GLS | Correct the initial OLS residuals, whiten matched design and response, then fit by QR. Preserve final covariance, standard errors, residual variance and rdf. | `CorrectedGlsSuite`: six shared/voxelwise and two runwise R cases | Covered for one corrected OLS estimation pass. |
| Bounded pooling and saved preparation | Correct after exact raw-statistic reduction; retain policy/outcomes in diagnostics and artifacts. Full-rank voxelwise factors may have different QR pivot orders; deficient alias partitions must still agree. | Block-size/merge-order tests, public corrected fits, artifact restoration and `OlsDiagnosticsSuite` | Covered on both platforms; no population residual retention. |
| AR/ARMA whitening | Matched row-preserving filtering, initial AR(1) scaling, run/censor resets and transpose operations. | `FmriArParitySuite`, whitening and transpose suites, GLS receipts | Covered for fixed admitted filters. |
| ARMA estimation | R supports an ARMA estimator; ScalaFIM currently estimates pure AR and accepts fixed ARMA filters. | Public type/API audit; existing AR README | Deferred feature expansion, not an implemented estimator or a parity claim. |
| Parcel pooling | ScalaFIM's AR pooling ADT offers global/run units. fmriAR also refuses design correction with parcel pooling. | Pinned `fit_noise` trust-boundary checks and Scala pooling ADT | Excluded from this corrected-GLS contract; a parcel estimand needs a separate typed API and fixtures. |
| Shared mean-series estimation | fmrireg also offers `mean_series`; ScalaFIM's public GLS policy pools within-column ACVF instead. | Pinned `.shared_ar_residual_input` and Scala summary implementation | Explicit scientific scope difference; no silent averaging of response columns. |
| Gamma/sigma2 placement | R attaches these to its plan; ScalaFIM returns them alongside the plan in `NoiseFit`. | Bias parity fixtures including gamma/sigma2 | Intentional API difference; whitening plans remain scale-free operators. |
| Pair counts and fixed-order refusal | Scala pair counts sum across columns; R reports per-column counts. Ratios agree. An unestimable fixed order is a typed error rather than R's silent cap. | Bias/estimation fixtures and adversarial tests | Intentional, documented differences. |
| Rank and ill-conditioned correction | Gale and R use different QR/condition estimators. Their rank can differ near numerical deficiency; correction outcomes are explicit rather than warnings. | Rank-deficient fixtures, rcond gate and fallback suites | Covered within the fixture contracts; no promise of bit identity between distinct rank estimators at their thresholds. |
| Corrected iterative GLS / robust residuals | The current correction is derived and validated for OLS residuals. | Configuration/refusal tests | Explicitly refused; requires separate mathematical and numerical qualification. |

## Reference locks and regeneration

The original receipts retain `tools/r-parity/reference-lock.json`, including
fmriAR `f563f2df20264ffa7db9be116f11d631551e9132`. Corrected receipts use the
separate `tools/r-parity/ar-reference-lock.json`: R 4.5.1, fmriAR 0.3.3 at
`ca27f2772bb4675573fae717a992c993c2f986c8`, and fmrireg 0.3.0 at
`5bd3ad141fe7361c5577d5bef565861a49a315ec`. The fmrireg adapter is sourced directly
from that checkout; its unrelated model and IO dependencies are not installed.

Both generators are declared in `tools/r-parity/auxiliary-manifest.json`:

- `modules/ar/tools/generate_fmriar_bias_parity.R` produces the existing Scala
  bias fixture plus `docs/scenarios/fixtures/ar.fmriar-bias.v1.r.json`.
- `modules/fit/tools/generate_fmriar_corrected_gls_parity.R` produces the public-fit
  Scala fixture plus `docs/scenarios/fixtures/fit.fmriar-corrected-gls.v1.r.json`.

Each finalizer checks input/output hashes, generated Scala bytes, generator and
serialization hashes, runtime, reference versions/revisions, and the exact lock
hash. Ordinary PR checks need no R runtime. Live regeneration selects one lock
while freshness checks always cover every declared receipt:

```sh
python3 -S tools/r-parity/check_receipts.py
FMRIAR_AR_R=/path/to/locked/fmriAR FMRIREG_AR_R=/path/to/locked/fmrireg \
  python3 tools/r-parity/check_receipts.py --regenerate r \
  --lock-path tools/r-parity/ar-reference-lock.json
```

The scheduled `scenario-receipts.yml` job installs and regenerates the original
reference group first, then the corrected reference group, and captures both
JSON and Scala fixture diffs. This preserves the old oracles while adding
reproducible evidence for the corrected public consumer.

## Verification boundary

The numerical comparisons use explicit 1e-10 tolerances for the serialized R
consumer oracle. Correction-summary tests additionally require bit identity to
dense estimation across every spatial block size and reverse merge order.
Correction policy round trips through portable model JSON; invalid ceilings,
non-OLS recipes, mixed summary bindings and incompatible artifact restoration
are refused.

The independent review's fully-censored-run diagnostics issue is resolved.
Shared and voxelwise diagnostics represent an IID filter as zeros at the common
AR order before averaging and validation. Native whitening filters keep their
empty coefficients and order 0, and correction outcomes remain `NotAttempted`.
The regressions cover AR(1)/AR(2) across global, run and voxelwise pooling and
compare dense execution with bounded block sizes 1, 2 and 4. The updated full fit
gates passed 704 JVM and 647 Scala.js tests with no compiler warnings; the same
reviewer confirmed the P2 is resolved with no new actionable findings. See
[the remediation receipt](../verification/ar-consumer-closure-20261007/censor-fix/summary.json).

Original implementation gates passed: AR 158 JVM / 156 Scala.js tests, model 56 tests on each
platform, and fit 699 JVM / 642 Scala.js tests (1,767 total). `scalafimCompileAll`
passed with no compiler warnings. Both corrected R generators were rerun in the
pinned R 4.5.1 container; all four generated JSON/Scala files were byte-identical.
The receipt checks cover 13 declared external oracles, and the six Python
regression tests plus the 38-entry scenario manifest validation passed.

Source hashes, full compressed logs and the qualification limits are retained in
[the verification summary](../verification/ar-consumer-closure-20261007/summary.json). The optional PHRF heavy
rho-bias experiment was not part of this implementation gate; its historical S2
receipt remains separate scientific evidence.

## fmriAR 0.4.0/0.4.1 delta assessment — 2026-10-09

**Two directly shared defects are confirmed: automatic-order BIC and ACF
aggregation. Higher-order stationary whitening, stabilized residual-bias
correction, and low-rank bias-matrix construction are also relevant upstream
improvements.** This assessment changes documentation and retains observation
evidence; it does not change production estimation, reference locks or previous
scientific verdicts.

Upstream merged [PR #10](https://github.com/bbuchsbaum/fmriAR/pull/10) at
`00e6a98e54719c8f3ba40373b76d875d7fdb67fa` (0.4.0) and
[PR #11](https://github.com/bbuchsbaum/fmriAR/pull/11) at
`de42cd9dc3e290d0653c5145749c27e03085f7d6` (0.4.1). Both merged on
2026-10-09. The comparison starts at the corrected-parity lock
`ca27f2772bb4675573fae717a992c993c2f986c8` (0.3.3), rather than a moving
installed package. Assessed ScalaFIM AR sources at `4756c99c` are byte-identical
to the sources used for the probes at `a7ec63cf`.

### Cross-reference

| Change | ScalaFIM location and disposition | Priority |
| --- | --- | --- |
| Standard AR BIC: `n log(sigma2) + (p + 1) log(n)` | `ArEstimation.selectByBic` still uses `2n log(sigma2)`, halving the effective complexity penalty. Confirmed shared defect. Low-level default estimation uses automatic order; public GLS currently requests fixed order. | P1: direct correction with a new reference fixture. |
| Aggregate per-voxel ACFs, rather than compute ACF of voxel-mean/median series | Both `AcorrDiagnostics.compute` overloads still aggregate series first. The no-run `None` mode already computes each voxel's ACF; run-aware `None` additionally differs in normalization, using lag-specific pair counts rather than the constant lag-zero denominator now used by R. | P1: correct aggregation and declare diagnostic normalization explicitly. |
| Exact stationary start-up for AR(p), MA and ARMA | `InitialConditionPolicy.ExactAr1` and `ArmaCoefficients.exactAr1FirstScale` scale only pure AR(1). Higher orders use truncated, zero-history recurrences. This is the existing admitted policy, but it is not exact stationary whitening for the larger family. | P1: add an explicit stationary policy and qualify public GLS consumers. |
| Stabilize isolated near-null bias-map directions using a short-memory tail | `AcvfBias.correct` still performs the exact solve after an rcond gate; no singular-direction anchoring exists. Passing rcond does not prevent substantial amplification along isolated weak directions. | P1 investigation: explicit correction policy and lag-budget review. |
| Build the bias map through the low-rank residual projection | `AcvfBias.runMatrix` materializes both `resid` and `shifted` with `n_valid * n` entries, plus a dense projection. It retains the pre-0.4.1 construction. The new map has the same mathematical target and matches it in the probes. | P2 performance: adopt the expansion and measure JVM/JS time and allocation. |
| Global AR pooling: solve Yule-Walker after averaging normalized run ACVFs | Dense and bounded-summary paths both use `poolRunCoefficients`, which averages per-run coefficient vectors. AR(1) agrees on the control; AR(2) differs. New R auto-order selection also occurs on the pooled frame count instead of per-run selection followed by averaging. | P2 scientific policy: choose and qualify the common-filter estimand. |
| Honour explicit `p > p_max` | Scala's `ArOrder.Fixed` has no independent `p_max` cap and refuses unestimable requested orders. Fixed AR(8) is retained in the probe. | Existing protection. |
| Logical censor masks, plan censor fallback, per-run application mismatch | Scala uses checked row sets and immutable, row-bound layouts rather than R masks or call-time fallbacks. `CoefficientScope.validate` checks run count, and the transform checks covered rows. | Existing protection; R API failure mode does not transfer directly. |
| MA unit roots and C++ reflection with a zero highest-order coefficient | Checked fixed filters refuse unit MA roots. Scala stationarity enforcement uses PACF plus a Gale recurrence-root check, not the affected `arma::roots()` coefficient reconstruction. `(1.5, 0)` is repaired to `(0.99, 0)` in the control. | Existing protection / different implementation. |
| Voxel-pooled, segmented Hannan-Rissanen; ARMA order selection and burn-in | Scala estimates pure AR only. Fixed ARMA filters exist and therefore do need the start-up improvement above; the estimator changes are future feature guidance. | Deferred estimator expansion. |
| Voxel-pooled parcel estimation, parcel design correction and scale-free multiscale weights | No parcel/multiscale pooling policy exists in ScalaFIM's AR provider. These changes supersede the old R limitation recorded above, but do not reveal a bug in an implemented Scala parcel estimator. | Deferred typed pooling API. |
| AFNI negative-pole checks and `vrt` handling | No matching AFNI restricted-plan constructor exists in the AR provider. | Deferred feature guidance. |
| Rank-aware sandwich, vectorized HC0 and new HAC option | These are separate inference features in R; no matching `sandwich_from_whitened_resid` API exists in the AR module. Adopting HAC requires an explicit fit-level inference contract. | Separate inference assessment. |
| OpenMP, C++ lag sums, parcel design cache, R `inplace` deprecation | Shared Scala kernels already use primitive loops; bounded lag summaries guarantee exact spatial reduction across block sizes and merge order. Native parallelism needs a platform capability, and parcel caching has no current parcel consumer. Immutable transforms avoid the R aliasing surface. | Preserve portable behavior and reduction guarantees when optimizing. |

### Numerical observations

The [manifest](../verification/ar-fmriar-041-cross-reference-20261009/manifest.json)
retains source hashes, input records, generators, compressed scratch suite and
logs. Ten observation tests passed on JVM and ten on Scala.js; their maximum
reported numeric difference was `4.44e-16`. The isolated fmriAR 0.4.1 audit
regression file passed 39 test blocks / 180 assertions, with no failures,
warnings or skips.

| Identical input | Current ScalaFIM | fmriAR 0.4.1 / independent stationary oracle |
| --- | --- | --- |
| Seeded 200-frame single-voxel AR(1), automatic order | Order 2 | Order 1 |
| Shared slow signal plus voxel noise, mean lag-1 diagnostic | `0.853878` | Mean per-voxel ACF `0.577378` |
| Same input, median lag-1 diagnostic | `0.870063` | Median per-voxel ACF `0.558390` |
| Unequal-run pooled AR(2) | `(0.563236, 0.048467)` | `(0.557199, 0.059361)` |
| Stationary AR(2) `(1.2, -0.5)`, unit innovation variance, segment start | First two whitened variances `3.703704`, `1.925926` | `1`, `1`; R transform matches dense Cholesky within `8.88e-16` |
| Stationary ARMA(1,1) `(0.6, 0.5)`, unit innovation variance | First two whitened variances `2.890625`, `1.472656` | `1`, `1`; R transform matches dense Cholesky within `3.89e-16` |
| Stationary AR(1) `0.7` start-up control | Variance `1`; transform difference `1.11e-16` | Variance `1` |
| One selected censored drift-design realization, correction budget 25 | Phi `0.238088`, matching the locked exact-solve behavior | Phi `0.512070`, using tail anchoring |
| Same realization, adaptive/fixed budget 5 | Phi `0.312423` | Phi `0.312423`; anchoring is inactive |

The whitening variances are deterministic calculations of `W Sigma W'` using
the stationary covariance, not Monte Carlo variance estimates. The selected
single bias realization demonstrates a changed solver, not an RMSE estimate or
a qualification of its new statistical policy. The new low-rank bias matrices
match Scala's dense matrices at `1e-10` on this censored, two-run design.

The R probes used R 4.3.3 and an isolated, locally built fmriAR 0.4.1 library.
Old R estimation code was sourced at the locked 0.3.3 revision with unchanged
Yule-Walker/PACF C++ exports supplied by that library; old whitening was not
called. Legacy estimates match Scala at `1e-10`, with `1e-8` for the bias solve.
These are advisory delta observations, not replacements for the R 4.5.1
hash-locked parity receipts. Initial scratch constructor and empty-test-discovery
attempts are retained in the evidence and are not counted as passed gates.

### Bias stabilization and performance details

R's new `.solve_correction` returns the exact solve for matrices with fewer
than eight columns. Otherwise it uses SVD, treats singular values at or below
`0.1 * median(singularValues)` as weak, solves well-determined directions and
chooses the weak-direction coefficients to minimize the last quarter of the
corrected ACVF. The median-relative threshold is the follow-up correction in
`bd4e03f`; largest-relative thresholding incorrectly anchored much of the
space in low-residual-df designs. The old `1e-6` rcond refusal still precedes
this solve.

Scala's public `ArBiasCorrection.Ols` uses the fmrireg adaptive rule. With
sufficient residual df, AR(1) and AR(2) both request five lags, producing a
six-column system. Thus copying R's new solver alone would not stabilize those
default fits. A widened budget and the short-memory assumption must be explicit
and independently assessed. The selected budget-25 maps have rcond `0.00760`
and `0.00303`, comfortably passing the old gate, but each contains one singular
direction below R's median-relative threshold.

For performance, expand the restricted, centered residual operator as
`R = B - U Q'`, where `B` selects and centers valid rows and `U` is the
corresponding centered design basis. Computing `R S_k R'` through this
expansion avoids the two dense `n_valid * n` buffers. The same pair selection,
run restriction and centering must be preserved. R's reported `3.3 s -> 0.19 s`
at `n = 800` is upstream evidence, not a measured Scala speedup. Its current
inner lag loop also recomputes `U_pairs * G`; a Scala implementation should
hoist `U * G` once per covariance lag rather than inherit that repeated
rank-squared multiplication. General decomposition/factor capabilities belong
in Gale; AR retains the scientific policy and adapters.

### Consequences for the previous GLS qualification

The BIC fix does not explain the previous fixed-order GLS failures, and the
diagnostic aggregation fix does not change fitted betas or covariance. Exact
higher-order start-up is relevant to short runs and frequent censor resets.
However, an exact covariance within each segment still discards covariance
across censor gaps in an otherwise continuous run; it does not resolve that
separate model approximation. The new bias solver is inactive at the existing
AR(1)/AR(2) adaptive budget. No new code change can therefore be inferred to
clear the retained voxelwise F/coverage failures from these probes.

Recommended sequence: correct BIC and diagnostic aggregation with separately
versioned 0.4.1 fixtures; implement and qualify explicit stationary whitening
including its adjoint, bounded consumers and saved-artifact identities; then
assess bias stabilization together with budgets and tail assumptions. Optimize
the equivalent bias-map construction independently, and assess global pooling
as a declared estimator change. Preserve historical locks and negative
qualification receipts; rerun the original adverse cases and fresh confirmation
before changing an inference claim.


## Implemented follow-up — 2026-10-09

The BIC and diagnostic defects are corrected. Explicit stationary whitening,
short-memory tail correction, and estimation-only censor continuity are wired
through public GLS, bounded preparation, contrast preparation and saved
artifacts. Low-rank bias-map construction retains the existing analytic target.
The separate fmriAR 0.4.1 / R 4.5.1 receipt covers these contracts; the original
0.3.3 source locks remain historical references. Global pooling still follows
the pinned coefficient-average policy and has not silently adopted the newer
normalized-ACVF estimand.

The wider tail correction failed the short-run engineering pilot because some
corrected lag-zero variances were negative. Exact stationary initialization
alone did not change the OLS AR-estimation RMSE. Neither observation clears the
previous voxelwise inference failures. Fresh continuous-whitening qualification
and retained negative receipts are recorded in
[the follow-up report](../verification/ar-correctness-followup-20261009.md).
