# Held-out loading statistic kernel, 2026-10-02

Mote `bd-01M2BNHC2523X7WRDK7G8CNANH`, baseline M3 gate
`f3c831bf`. This packet implements candidate C1 statistics; probability,
coverage, multiplicity and Monte Carlo qualification remain M4.08/M4.09 gates.
No finite fixture or closed-form statistic is promoted to calibrated inference.

## Scientific model and numerical route

`VoxelLoadingConfirmation.fit` uses the actual held-out native targets to
construct `T = Y C` with the frozen discovery target projection. It regresses
held-out brain columns on `[nuisance, T]`, with no sparsity penalty and no
brain-derived `X W` regressor. Component loading tests condition on the other
component and nuisance columns; the omnibus null is zero joint loading.
Results concern task-linked forward association in the fixed discovery basis,
not causal necessity or unique decoding value.

The actual confirmation row descriptor, neural endpoint and frozen target
endpoint must match. Known Gaussian row covariance is interpreted as a known
shape with separately estimated voxel scale. Dependent-time and known repeated
Gaussian models whiten through the retained structured covariance's Gale
augmented QR. The trailing transformed coordinates satisfy `W^T W = V^-1`
and have identity covariance under that declared shape. They are numerical
whitening coordinates, not newly independent physical subjects. Actual row,
unit-axis and row-to-unit mapping remain in the result. Estimated covariance
and unsupported error laws cannot enter through the admitted design.

One pivoted Gale QR factors the common weighted design. Each voxel batch uses
one native brain block and the retained QR least-squares solve. Coefficient
covariance is derived from the small triangular factor, including its column
permutation, rather than inverting normal equations. The component submatrix
supports a Gale Cholesky omnibus quadratic. Residual scale is weighted RSS
per residual degree of freedom. No dense row-by-row covariance or voxel-by-voxel
covariance is formed; output maps are neural-by-component.

Rank deficiency, target/nuisance aliasing, missing/nonfinite scores and zero
residual scale return typed outcomes. Metadata shape, owned-cell and replay
preflight precedes either target or brain reads. Replay is explicitly declared;
multiple batches do not silently reacquire a single-pass source. Retained
outputs, basis/read blocks, common design/factors and covariance applications
are charged conservatively with checked arithmetic. Borrowed sources and
private backend/provider scratch remain excluded; the bound is not process
peak RSS. Intervals are arithmetic at a supplied positive finite critical
multiplier. Their API deliberately makes no automatic confidence-level or
probability claim.

## Independent fixtures

The 8-row Walsh fixture has two target columns, an intercept and a nuisance
column. Exact coefficients are `[2,-1]` and `[-1,3]`; nuisance effects are
`4` and `-2` and intercepts `7` and `-3`. Independent residual columns yield
RSS `[2,.5]`, df 4, standard errors `[.25,.125]`, component t values
`[8,-4,-8,24]` and omnibus F `[40,320]`. One-column and full-width single-pass
batches agree. At independently generated R `qt(.975,4)`, arithmetic interval
endpoints agree within `1e-12`; empirical coverage remains unqualified.

A frozen nonorthogonal target projection changes coefficients to
`[1.25,-1]` and `[-1.25,3]`, changes the first standard error as dictated by
the coordinate covariance, and preserves the omnibus hypothesis. This checks
actual use of `Y C` and the distinction between coordinates and a subspace null.

The independent R 4.5.1 fixture constructs the full 8-by-8 covariance and uses
`solve(V)` and weighted cross-products, sharing no ScalaFIM numerical helper.
It yields coefficients `[2.0409555566340161,-.90468934939161416]` and
`[-.98928414474021209,3.0076631374039833]`; standard errors and F are checked
at explicit tolerances. A separate rank-one Sherman--Morrison scalar calculation
checks the whitening inner product. An exact two-row cofactor precision and
identity covariance check the covariance adapter itself.

Aliased targets, missing scores, foreign actual rows/endpoints, exhausted
budgets and unavailable replay refuse without brain reads. A poison target
operator also proves zero target reads at budget refusal. These are engineering
and finite numerical checks, not the frozen 10,000-dataset calibration run.
R startup emitted existing locale warnings; numerical stdout is retained separately.

## Final verification and status

Every result carries typed `PendingFrozenProtocol` calibration status.
The implementation returns no p-value or qualified probability wrapper.
Gate143 passes the full affected published-pin suites: core 362 each, fit 45
each, dataset 108 each, spatial 20 each and archives 13 JVM/5 JS, totaling
548 JVM and 540 Scala.js tests. Compile-all passes warning-clean. After the
final explicit status edit, gate144 passes all 362 core tests on each platform
and warning-clean compile-all. The rotation draft was excluded and restored;
provider pins did not change. Archive child processes retain the existing
Java 25 Unsafe runtime warning, separate from compiler warnings.

Evidence root: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.

`gate143-loading-owning.log` SHA-256: `456aa08c719f64b3297e3550986b47a5e84c245b220beee34cc29c7b69000da8`.

`gate144-loading-calibration-status.log` SHA-256: `7f00f4c40f8f309f394d31bb184f1e8e658d70db8efb914cd2e8af54860f8156`.

`gate144-pinned-sources.json` SHA-256: `ff4d741c88b33a554d8a04cb52a9a053034815c0a43cff7eb925413c2be2b187`.

`loading-confirmation-r-oracle.R` SHA-256: `837f57f11f20c98e8c03325f298b6f83cd96ed30c36d30c2285e566642d24b9e`.

`loading-confirmation-r-oracle.log` SHA-256: `37c7218c8bb496472825bdc283158d458221c323f347df79e5fa4942e8f1e53f`.

