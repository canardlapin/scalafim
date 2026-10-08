# AR audit: fmriAR/fmrireg consumer closure

Mote: `bd-01M3WA0GGQ4Q0064E4FEP7WGF1`. Assessment and implementation: 2026-10-07.
This closes the three gaps identified in the status assessment: public GLS
correction policy and outcomes, corrected end-to-end consumer parity, and the
consolidated audit/regeneration ledger. It does not claim complete feature
coverage of the R packages or a new PHRF pilot qualification.

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
