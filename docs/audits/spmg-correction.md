# SPMG correction (2026-09-12)

The default continuous canonical is `dgamma(t, 6) - dgamma(t, 16)/6` before
any requested normalization. The former positive coefficient `0.0833` was
approximately ten times `1/120`, changing shape as well as scale. The exact
undershoot coefficient is `1/(6*15!)`.

SPMG2 adds the analytic time derivative. SPMG3 adds the response dispersion
column `(h(d=1) - h(d=1.01))/0.01`, with response mean and mass held fixed.
Only the positive component changes; the undershoot cancels. The second time
derivative remains available under its accurate low-level name and is used
when differentiating the temporal column. The third column's time derivative
and primitive now follow the dispersion definition.

These are raw continuous bases. Neither implicit orthogonalization nor new
normalization is introduced. Existing fixed-reference-grid and legacy scaling
modes retain their policies. In particular, `spm` normalization specifies a
scale convention, not complete sampled SPM/Nilearn design equivalence. The
24-second nominal span is unchanged. SPM and Nilearn also differ in temporal
finite-difference steps, sampling, and where orthogonalization is performed.

Raw beta coordinates are not normalized response amplitudes. Use the declared
basis geometry or reconstruct the response before computing peak/projection
summaries; do not assume a Euclidean norm of beta coordinates is invariant to
basis scaling. Orthogonalization, when requested by a consumer, needs an
explicit reference domain and coefficient transform.

This is a breaking scientific correction: refit existing analyses. Changing
normalization cannot convert the old canonical shape or second-time-derivative
basis into the corrected basis. No existing historical receipt is recertified
by this change. The separate corrected structural fixture uses the local source
hashes in `spmg-correction-source.json`; the old locked structural receipt and
fixture remain historical artifacts.

Validation uses independent SciPy 1.17.1 gamma PDF/CDF values, finite differences
of the public basis, R normalization contracts, refreshed R HRF/design fixtures,
and downstream design tests. R additionally fixes a pre-existing loop block
support mismatch exposed by the corrected scale: the kernel is masked at the
regressor span before integrating, matching the convolution engine. A
convolution-of-boxes oracle tests that boundary.

References:
- https://github.com/spm/spm/blob/main/spm_hrf.m
- https://github.com/spm/spm/blob/main/spm_get_bf.m
- https://github.com/spm/spm/blob/main/spm_orth.m
- https://github.com/nilearn/nilearn/blob/main/nilearn/glm/first_level/hemodynamic_models.py

## Reproducing the reference values

Load the corrected local HRF package before loading `fmridesign` (otherwise R
may resolve the installed, older dependency):

```r
pkgload::load_all("~/code/fmrihrf", quiet = TRUE)
source("tools/r-parity/generate_fmrihrf_r_parity_fixtures.R")
source("tools/r-parity/generate_fmridesign_r_parity_fixtures.R")
```

For the corrected structural fixture, direct `STRUCTURAL_2X2_RECEIPT_OUT` to a
temporary JSON path and `STRUCTURAL_2X2_SCALA_OUT` to the corrected fixture path
before sourcing `generate_structural_2x2_design_receipt.R` in the same R process.
Rename the generated Scala object to `CorrectedSpmgStructuralRFixture`. Do not
run the historical receipt finalizer against these modified local sources.

The normalization constants use `seq(0,32,length.out=1600)` for the `spm` sum
and `seq(0,24,by=.02)` for canonical peak, trapezoidal area, and per-column
absolute peaks. The block peak references use `gen_hrf(HRF_SPMG1,width=4,
precision=p)(0:40)` at p = 1, .5, .1, and .001. Projection references compare
`conv` and `loop` on `seq(0,118,by=2)`, using the onsets and precisions in
`RegressorMethodAccuracySuite`.

## Qualification

On 2026-09-12, the isolated candidate passed `hrfJVM/test`, `hrfJS/test`
(235 each), `designJVM/test`, `designJS/test` (238 each), and
`hrfLawsJVM/test`, `hrfLawsJS/test` (81 each): 1,108 tests in total.
`scalafimCompileAll` passed with no warnings or errors using the default
pinned dependencies. The R testthat directory passed 732 assertions with
zero failures/errors and 55 metadata warnings. The changed Rd files parsed
successfully. R CMD check was not run.

The correction was subsequently applied cleanly on top of Cascade34 commit
`fd62ea7` after its session ended and reservations cleared. Combined JVM and
Scala.js HRF (250 each), law (82 each), and design (238 each) suites passed:
1,140 tests total. The integrated `scalafimCompileAll` gate also passed with
no warnings or errors. Source hashes for the tested R changes still match.
