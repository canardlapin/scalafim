# Compressed reduced-rank GLS with voxelwise AR

Design decision, 2026-09-29. Mote: `bd-01KX50HA4TGKKWR1CC2G8HRY2D`.
Implementation follow-up: `bd-01M3QWEYR7925NAFZDCH60Q6R8`. Fixed-rank execution
is available through explicit `EstimatesOnly` and `VoxelwiseBootstrap` policies.
The qualification record below distinguishes numerical parity from inference
calibration; generic T/F inference and adaptive voxelwise rank remain refused.

**Current qualification:** fixed-rank execution and named bootstrap contrast
summaries are implemented and checked on JVM and Scala.js. The expanded study
found **substantial undercoverage of requested 95% intervals**, including 82%
coverage for one strong-AR coordinate. These intervals remain an experimental,
model-conditional approximation. They are not qualified as nominal 95%
inference. See [expanded coverage evidence](#expanded-coverage-evidence) below.
Follow-up Mote: `bd-01M3R15Y3QC2VVAQE1WGTBH0QX`.

## Decision and current behavior

The intended extension imposes **one global rank constraint on the target
coefficient matrix in the original predictor/voxel coordinates**, using the
sum of each voxel's whitened residual sums of squares. It does not define a
common latent time series by mixing differently whitened response columns.
Voxels need not be grouped. Exact groups may share geometry calculations inside
the global fit; independently reducing groups defines a different estimand.

The existing [prepared RRG](../../modules/fit/shared/src/main/scala/scalafim/fmri/fit/ReducedRankGls.scala)
supports a common whitening transform, nuisance residualization, task QR/SVD,
and response-loading conditional/bootstrap inference. The legacy voxelwise AR
path allows an explicit full-rank request with `Conditional` inference, delegates
to voxelwise GLS, and records `ReducedRankFullRankVoxelwiseFallback` with
event-only inference. Legacy `Conditional`/`Bootstrap` compression remains
refused. The new policies return `VoxelwiseReducedRankFmriFitResult`, whose
uncertainty is either explicitly unavailable or a separate bootstrap payload.

The extension needs a new geometry and inference identity. Removing the guard,
averaging AR coefficients, or reusing current `ReducedRankConditional` for the
new covariance formula is not an implementation of this decision.

## Objective and nuisance elimination

Let `Y` be `n × m`, target design `X` be `n × p`, and nuisance design `Z`
be `n × q`, all on the same resolved row axis. Target coefficients `B` are
`p × m`; nuisance coefficients `Gamma` are unrestricted. Each voxel has a
frozen, nonsingular, segment-aware temporal transform `W_v`. Define

```text
N_v = W_v Z
M_v = I - N_v (N_v' N_v)^-1 N_v'
D_v = M_v W_v X             z_v = M_v W_v y_v
G_v = D_v' D_v              h_v = D_v' z_v

minimize L(B) = sum_v ||z_v - D_v b_v||², subject to rank(B) <= r.
```

Use QR projections/solves rather than materializing `M_v` or matrix inverses.
Require full column rank of `[W_v X, W_v Z]`, positive residual df, finite
inputs, and admitted whitening conditioning. Singular design/transform failures
are typed failures, not instructions to drop columns, voxels or rows silently.
With no nuisance columns `M_v = I`. Recover nuisance estimates after reduction:

```text
gamma_v = argmin_gamma ||W_v(y_v - X b_v) - N_v gamma||².
```

Residualization must follow whitening separately for each voxel. A common
unwhitened nuisance projection generally gives another loss. Respect original
timepoint identity, run breaks, censor resets and initial-condition scaling.
AR estimation exclusion rows and fit row inclusion are distinct inputs; do not
turn noise-estimation censoring into silent removal of fitted observations.

This is a unit-weight marginal GLS loss conditional on the frozen transforms.
It assumes neither independent voxels nor one matrix-normal noise covariance.
If voxel innovation scales differ, it is not automatically their joint Gaussian
likelihood: likelihood weights would include inverse scales. Such weighting is
a separate explicit policy, not a hidden normalization or an iteratively changed
objective. Coefficient units and design scaling must be preserved.

## Shared subspace and limiting cases

`B = A C` with `A` of size `p × r` and `C` of size `r × m` expresses a
coherent shared **predictor subspace**. Its row space also supplies a shared
response loading space algebraically, but heterogeneous whitening does not
commute with response-column mixing. There is generally no one transformed
`X` and `Y` to which the existing task QR/SVD may be applied.

Completing squares gives the heterogeneous coefficient metric

```text
L(B) = L(B_GLS) + sum_v (b_v - b_GLS,v)' G_v (b_v - b_GLS,v).
```

An ordinary SVD of `B_GLS` minimizes a Euclidean coefficient loss, not this
loss. When all `G_v = G`, an invertible square root of `G` reduces the problem
to a spectral one; common whitening recovers the current task QR/SVD after
nuisance elimination. Proportional metrics `G_v = alpha_v G`, `alpha_v > 0`,
also admit a spectral reduction with explicit response-column scaling. Different
AR coefficients therefore do not, by themselves, prove that every spectral
shortcut is impossible. General nonproportional metrics require another solver.

For `r >= min(p,m)` the rank constraint is inactive. Bypass the reduced model
and return ordinary voxelwise GLS coefficients and its marginal covariance,
while retaining event-only inference. This also covers `m < p`: freezing a
learned `p × m` basis for covariance would incorrectly restrict uncertainty
even though the rank constraint is inactive.

## Solver and rank policy

The implemented target is **fixed rank only** (including inactive `Full`).
Variable projection over predictor subspaces is an equivalent formulation,
representing `A' A = I` as a gauge:

```text
c_v(A) = solve(A' G_v A, A' h_v)
F(A) = sum_v [z_v' z_v - h_v' A solve(A' G_v A, A' h_v)]
b_v = A c_v(A).
```

Optimize `F` on the Grassmann space, or an equivalent rank-preserving
representation. Use stable Gale capabilities for solves and optimization
primitives; do not introduce private eigensolver/inverse families in `fit`.
Evaluate residual loss as well as the profiled expression to detect cancellation.
Check finite values, conditioning, rank, and basis-rotation invariance.

This objective is nonconvex. A production algorithm must declare deterministic
initializations/restarts, tie rules, iteration budget, stationarity criterion,
conditioning thresholds, achieved objective and convergence/failure status.
Convergence establishes a stationary candidate, not a certified global optimum.
An approximate solver must have an explicit API admission policy and report its
status; it may not silently claim the exact minimizing estimand. Generic weighted
low-rank approximation is difficult even at rank one
([Gillis and Glineur](https://arxiv.org/abs/1012.0197)); that result is background,
not a hardness proof for this restricted AR family.

Initially retain typed refusal for `EnergyRetained` and
`ResidualSumsOfSquaresBudget`: there is no generally shared singular spectrum
to reuse. A future adaptive policy must define the objective improvements,
optimization error, baseline loss and tie/zero-signal behavior before assigning
energy or an excess-RSS budget. It must refit the complete selection per rank.
Existing common-whitening policies keep their current semantics.

The implementation uses Gale's projected-gradient primitive and exact truncated
SVD projections onto `rank(B) <= r`. It scales target coefficient rows by pooled
residualized design norms, and all responses by their residualized RMS. These
invertible transformations preserve the original rank constraint and objective
while making stopping invariant to overall design/response units. Starts are
the truncated unrestricted estimate, zero, and the first `r` normalized target
rows. The lowest-loss converged start wins (ties use start index); no converged
start is a typed failure. Diagnostics retain every start's status, iteration
count, objective and projected-gradient residual. Defaults are 4000 iterations
and `1e-8` absolute/relative tolerance in normalized coordinates. QR rank tests
use Gale's scale-relative tolerance, and per-voxel target Gram conditioning is
bounded by `1e12`. No grouping or approximate whitening is performed.
The returned iterate's projected-gradient mapping is recomputed before accepting
convergence. Whitening segments require a finite initial scale above `1e-10`;
a singular action is refused even when its transformed design retains rank.

Inactive rank bypasses optimization. On the new estimates-only policy uncertainty
stays absent; on the new bootstrap policy it remains explicitly bootstrap
uncertainty. Ordinary GLS inference is available through the legacy full-rank
`Conditional` fallback. Reduced RSS divided by unrestricted residual df is
descriptive lack of fit, not an unbiased innovation variance under truncation.

## Exact and approximate grouping

Exact cache keys identify the actual whitening action: ordered fitted row
identity, run/segment boundaries, censor reset topology, resolved per-segment
AR/MA coefficients, recurrence convention and actual initial scaling. Include
design/target/nuisance identity and conditioning policy when caching projected
geometry. Method/estimation origin belongs in provenance even when actions agree.
Use canonical finite numeric representations and verify exact content after a
hash match; rounded coefficients, summary rho and hash equality alone are not
equality of geometry. Start conservatively with structurally identical actions;
do not search approximately for operator equality in the hot path.

Grouping is an optimization, not required scientific preprocessing. Distinct
estimated coefficients often make every voxel a singleton; neither compression
nor a speedup follows from grouping. Independent rank-`r_g` group fits allow
assembled rank up to `min(p, sum_g r_g)` and require a separately named model,
rank-per-group policy, group membership and separate inference provenance.
No such group-fitting model is admitted here.

Approximate grouping is rejected for this capability. A future approximation
would require user-visible tolerances, operator/loss perturbation bounds,
conditioning-sensitive coefficient and contrast-error bounds, qualification on
the original per-voxel loss, and approximate-geometry provenance. Small rho
differences alone are not an error contract.

## Covariance and contrast meaning

For an externally fixed predictor basis `A`, fixed `W_v`, an adequate restricted
mean model and marginal innovations with covariance `sigma_v² I`,

```text
Cov(b_hat_v | fixed estimator) = sigma_v² A solve(A' G_v A, A').
```

This is the covariance of a fixed predictor-subspace estimator. If `A` was
learned from these same responses, plugging it into that formula omits basis and
rank selection uncertainty; it is not the exact conditional distribution given
the learned selection. Estimated AR contributes another omitted uncertainty.
The corresponding target is the restricted/projection coefficient: rank
truncation can bias estimates of the unrestricted coefficient matrix.

Use a distinct method identity such as `VoxelwiseRankFixedPredictorSubspace`,
with external/frozen versus same-response plug-in preparation stated explicitly.
Use voxel-specific covariance, embedding only target rows; restrict nuisance
and baseline T/F inference as today. Estimate marginal `sigma_v²` from the
unrestricted frozen-whitening GLS residuals with `n-p-q` df, separately from the
reduced model's reported residual RSS. Reduced RSS includes discarded signal
and must not silently become an unbiased innovation-variance estimate.
This df is a variance-estimation convention; same-response plug-in basis or AR
does not acquire exact nominal t/F calibration from it.

Fixed-`A` target covariance has rank at most `r`. Admit joint contrasts only
when their transformed covariance is nonsingular and their inference target is
declared. Refuse zero-variance and singular joint contrasts with a typed
non-estimability result; do not floor eigenvalues or use an unexplained inverse
to manufacture a statistic. A contrast outside the restricted subspace can
refer to biased unrestricted coefficients even when its variance is nonzero.

Freezing the current **response loading** `V` is a different estimator:
`b_v = T v_v`. Its shared factor solves a system involving
`sum_v (v_v v_v') tensor G_v`. This couples voxels, and its uncertainty depends
on cross-voxel noise covariance. The current shared latent-residual covariance
formula cannot be transplanted to the new fixed-predictor covariance semantics.
Neither formula licenses arbitrary spatial aggregates from marginal covariance
alone. Export cross-voxel covariance only if actually modeled and retained.

## Bootstrap contract

Define two distinct targets before enabling bootstrap:

1. **Frozen-whitening refit:** freeze row/design identity, `W_v` and fixed rank;
   refit the global basis, coefficients and nuisance for every replicate. This
   includes basis variability under the fitted bootstrap model, but excludes AR
   estimation and rank selection uncertainty.
2. **Pipeline refit:** regenerate responses and rerun AR estimation, nuisance
   preparation, global fitting and any explicitly admitted rank policy per
   replicate. Do not freeze original group membership if whitening is re-estimated.

For a candidate residual bootstrap, obtain innovation residuals from unrestricted
GLS, center them within declared exchangeability units, resample **joint voxel
vectors synchronously**, and reconstruct raw responses from the chosen fitted
restricted mean plus each voxel's inverse whitening action. Whiten and nuisance
project each reconstructed response again. This approximates variability under
the fitted restricted model; it does not correct truncation bias against arbitrary
unrestricted truth. Define residual/leverage correction in the bootstrap policy
and qualify it rather than inheriting one accidentally.

Do not resample already projected `z_v` rows as if projection preserved iid
noise. Marginal AR whitening does not prove that joint voxel innovation vectors
are iid across time: differing filters can leave cross-voxel lag dependence.
Row bootstrap needs an explicit joint-innovation assumption; block bootstrap
needs declared block length and dependence qualification. Blocks stay within
run/censor segments, never wrap across resets, and define short-segment handling
and initial-condition treatment. Full-pipeline uncertainty requires a qualified
innovation-generating model, not only a deterministic row sampler.

Freeze replicate identities, seed/stream version, resampling indices, rank and
solver-start policy globally. Retry/failure policy is explicit; do not silently
discard failed replicates or substitute full-rank fits. Accumulate full target
coefficient covariance per voxel from decoded original-coordinate coefficients;
basis sign/rotation matching is unnecessary for that covariance. Same seed across
platforms proves reproducibility, not bootstrap coverage or nominal t/F inference.

## Execution and exports

Preparation is a barrier over the complete resolved selection. It estimates AR
according to the declared policy, binds each plan to stable voxel identity,
builds sufficient statistics, resolves/fits the one global subspace and prepares
the requested inference payload. Responses must be replayable with bound content
identity if preparation makes multiple passes. Voxel chunks then decode/select
immutable coefficient, nuisance, residual and covariance columns. They may not
re-estimate AR, choose a rank, refit a basis or allocate independent RNG streams.

For a bounded implementation, accumulate loss/gradient statistics in a canonical
voxel order independent of task completion order; floating-point associativity
otherwise changes optimization decisions. Reordered user selection maps to
canonical preparation identities and back to requested output order. Equal-rank
ties compare achieved loss then declared deterministic initialization identity.
Reject chunk/preparation membership or row/content mismatches. Sufficient
statistics need `O(m p²)` storage if retained; replay/streaming alternatives and
bootstrap work must be explicit in receipts. This is complexity accounting,
not a measured memory or speed claim.

The current implementation keeps the complete response and per-voxel whitened/
residualized designs in memory: approximately `O(m n (p+q) + m p²)` geometry
storage, plus `O(R p m)` target samples for `R` bootstrap replicates and exact
percentiles. It is not a streaming or measured memory-reduction implementation.

Required exported provenance includes a schema/method version, original target
and nuisance axes, row/voxel selection and content digests, frozen per-voxel
whitening actions and estimation configuration, loss/weighting identity, requested
and achieved rank, solver starts/status/objective/conditioning tolerances,
subspace identity, inference target and omitted uncertainty, variance estimator
and df, covariance scope, and bootstrap mode/replicate/failure/RNG/segment policy.
Record subspace/projector identity robust to basis rotations as well as any stored
factor representation. Summary AR coefficients are insufficient to reproduce
the fitted metric. Existing method labels must not misidentify the new estimator.
Estimate readers must reject unknown required semantics or preserve an explicitly
unavailable inference state. Nuisance estimates remain exportable, their SE and
covariance maps remain excluded, and marginal covariance must not masquerade as
a joint spatial covariance artifact.

## Original design evidence and admission gates

The independent base-R [oracle](../../tools/r-parity/verify_voxelwise_ar_rrg_design.R)
uses 12 rows, two target columns, an intercept, four voxels and two AR-reset
segments. It profiles rank one over the projective circle using all grid-local
minima and periodic edges, with 4096/8192-grid refinement. This is a discriminating
numerical fixture, not a general optimality proof. The [receipt](../audits/voxelwise-ar-rrg-design-oracle.json)
records:

| Fit evaluated in the original heterogeneous metric | Residual loss | Global target rank |
| --- | ---: | ---: |
| Global rank-one numerical oracle | 12.6119039072 | 1 |
| Ordinary coefficient SVD | 13.8650460162 | 1 |
| Pooled-whitening QR/SVD | 13.3097383788 | 1 |
| Independent exact-group rank-one fits | 3.5304205109 | 2 |
| Unrestricted voxelwise GLS | 0.1975716326 | 2 |

Full-rank FWL versus direct joint-design GLS agrees within `4.45e-16`;
common-whitening QR/SVD versus the angle oracle within `9.21e-10`;
fixed-predictor covariance versus the complete constrained-design linear
propagation within `5.56e-17`. The latter is an algebraic covariance check,
not empirical coverage or uncertainty of a learned basis. All fixtures are
synthetic; no restricted data or production implementation is involved.

The design specified the following admission evidence:

- Independent heterogeneous objective/coefficient fixtures, common-geometry
  spectral and inactive-rank GLS limits, nuisance/segment/initial-condition
  cases, scaling and conditioning failures, basis rotations and solver failures.
- JVM **and** JS direct/sequential/Future batch-versus-chunk tests at several
  widths, worker counts, selection permutations and completion orders. Compare
  coefficients, original-metric loss, rank/status, nuisance, every marginal
  covariance, contrasts, exclusions and export provenance. Never accept a
  chunk-local basis as equivalent merely because dimensions match.
- Fixed externally supplied basis/noise covariance tests independent of learned
  basis tests; independent simulation for plug-in and bootstrap calibration
  claims, including heterogeneous AR and joint temporal/spatial dependence.
- Typed geometry/inference/provenance lowering and physical export/readback tests;
  singular contrasts, omitted uncertainty and unsupported adaptive policies
  remain explicit. Provider capabilities are landed in Gale where required.

At the original design handoff, `ChunkedFitExecutorSuite` checked typed refusal for fixed/energy/RSS
compression under conditional and bootstrap policies, plus full-rank bootstrap,
through direct, sequential and Future execution with reordered voxels and two
chunk widths. Existing suites cover supported common-whitening compression,
bootstrap fixtures and full-rank voxelwise fallback. These checks protect the
boundary; they do **not** satisfy compressed voxelwise runtime admission.

Verification on 2026-09-29: the oracle passed and reproduced its receipt byte
for byte; the receipt binds the generator source by MD5. A separate mathematical
review found no required corrections. Each of the following bounded commands
passed 68 tests with no build warnings, on sbt 1.11.7 / Java 25.0.1:

```sh
sbt 'fitJVM/testOnly scalafim.fmri.fit.ChunkedFitExecutorSuite scalafim.fmri.fit.FitPlanExecutorSuite'
sbt 'fitJS/testOnly scalafim.fmri.fit.ChunkedFitExecutorSuite scalafim.fmri.fit.FitPlanExecutorSuite'
LC_ALL=C Rscript tools/r-parity/verify_voxelwise_ar_rrg_design.R
```

Those design-only checks did not execute compressed voxelwise fitting or measure
bootstrap coverage. The implementation evidence below supersedes that boundary
for the explicitly named new policies.

## Implementation qualification

The [qualification receipt](../audits/voxelwise-ar-rrg-qualification.json) binds
source hashes, both-platform checks, independent oracle output and the complete
pointwise simulation summaries. The new solver matches the heterogeneous
projective-circle oracle (`12.611903907225837` loss, coefficient tolerance
`2e-6`, loss tolerance `1e-9`). Tests cover common-whitening QR/SVD and inactive
GLS limits, nuisance reparameterization, design/response unit changes, a
three-target rank-two exact fit, zero signal, iteration exhaustion, singular
design/whitening, row/content identity and direct/sequential/Future execution.
Preparation canonicalizes voxel identity and chunks only select fitted columns.

The independent [bootstrap oracle](../../tools/r-parity/qualify_voxelwise_ar_rrg_bootstrap.R)
uses explicit R whitening matrices, direct inverse solves, QR residualization
and a scalar circle search for 32 frozen-whitening replicates. Its 128/256-grid
refinement changes coefficients/objectives by at most `2.49e-8`. Production
target covariance agrees within `1e-8`, pointwise interval endpoints within
`2e-6` and replicate losses within `1e-8`. Separate tests check ARMA inverse
recurrences, segment resets, singleton segments, excluded donor rows, short
donor refusal, unit-leverage refusal, boundary RNG seeds and failure propagation
with replicate identity. Actual NIfTI exports are read back; JSON sidecars are
also parsed independently to catch serialization defects.

The historical bounded calibration pilot used 80 independent synthetic datasets per regular
scenario: 80 timepoints, two rank-one targets, an intercept, three voxels with
AR(1) coefficients `[0.1, 0.5, -0.2]`, innovation SD `0.6`, and contemporaneous
correlation either `0` or `0.5`. Both bootstrap modes use 99 replicates, block
size one, one run and a single initial reset. The first three datasets in each
scenario also use 399 nested replicates to measure endpoint Monte Carlo
sensitivity. A separate 40-dataset zero-signal pilot records behavior at the
rank boundary without treating it as regular-model qualification.

Regular per-coefficient coverage ranges from `0.8875` to `1.0`, and mean
bootstrap variance divided by empirical sampling variance from approximately
`0.8764` to `1.1753`. All regular cases pass the prespecified broad investigation
thresholds (coverage below `0.85` or variance ratio outside `[0.5, 2]`). These
thresholds only detect gross failures. Some coverage estimates are below the
requested `0.95`; 80 datasets and 99 replicates do not establish nominal
calibration. The receipt retains per-coefficient Wilson intervals, estimator
bias/RMSE, variance estimates and higher-replicate endpoint sensitivity instead
of pooling correlated coefficients into an inflated sample size.

Those pilot thresholds are superseded by the expanded evidence below, which
demonstrates undercoverage in several of these settings. The supported
uncertainty remains an explicitly labeled, model-conditional residual
approximation. Its HC2 correction is marginal and does not exactly restore joint
covariance under heterogeneous hat matrices. No generic nominal T/F inference,
adaptive rank, global-optimality certificate, production-data validation,
performance advantage or publication is claimed.

Example configuration (replace `EstimatesOnly` with `uncertainty` below to request
bootstrap covariance and pointwise intervals):

```scala
val uncertainty = ReducedRankInferencePolicy.VoxelwiseBootstrap(
  VoxelwiseReducedRankBootstrapConfig.unsafe(
    resampling = ReducedRankBootstrapConfig.unsafe(replicates = 399, blockSize = 1, seed = 19),
    mode = VoxelwiseBootstrapMode.RefitAutocorrelation
  )
)
val config = ReducedRankGlsConfig.unsafe(
  components = ReducedRankComponentSpec.unsafeFixed(1),
  autocorrelation = AutocorrelationConfig.unsafe(order = 1, voxelwise = true),
  inference = ReducedRankInferencePolicy.EstimatesOnly
)
val plan = FitPlan(model, FitStrategy.ReducedRankGls(config))
```

## Expanded coverage evidence

The [study design](../audits/voxelwise-ar-rrg-study-design.json),
[coordinate summaries](../audits/voxelwise-ar-rrg-coverage.json), and
[compressed raw records](../audits/voxelwise-ar-rrg-coverage.jsonl.gz) retain every
planned attempt, failed fits, both miss tails, Wilson intervals, bias, sampling
variance, bootstrap variance and paired sensitivity comparisons. There are
5,560 fitting attempts across paired settings and replicate budgets; these are
**not 5,560 independent datasets**. The study requests 2,858,440 bootstrap
replicates. Each regular scenario has 400 independently generated datasets;
stress scenarios have 200 and the stationary-reset diagnostic has 100. The
independent scalar AR generator uses 500 burn-in steps and continues latent
noise through omitted timepoints in the censor-continuous scenario.

The following ranges describe the 12 separate coefficient/contrast coordinates
at 399 replicates. They do not pool correlated coordinates or measure
simultaneous coverage. Each mode uses the same underlying generated responses.

| Scenario | Datasets per mode | Frozen whitening coverage | Refit AR coverage |
| --- | ---: | ---: | ---: |
| Baseline heterogeneous AR | 400 | 0.8800–0.9600 | 0.8875–0.9575 |
| Two runs, run-specific intercepts | 400 | 0.8675–0.9200 | 0.8725–0.9200 |
| Strong heterogeneous AR | 400 | 0.8200–0.9475 | 0.8250–0.9450 |
| Censor gaps, continuous latent noise | 400 | 0.8600–0.9125 | 0.8725–0.9200 |
| Independent stationary reset segments | 100 | 0.8300–0.9000 | 0.8300–0.9000 |
| Lagged joint innovations, block 1 | 200 | 0.8750–0.9600 | 0.8750–0.9650 |
| Same lagged inputs, block 4 | 200 | 0.8600–0.9400 | 0.8500–0.9350 |
| Weak signal | 200 | 0.8392–0.9497* | 0.8550–0.9550 |
| Zero signal, rank boundary | 200 | 0.8950–0.9700 | 0.8900–0.9700 |

*Frozen weak-signal coverage conditions on 199 successful fits. Dataset 116
failed at replicate 364 after 363 completed replicates because no solver start
converged within 4,000 iterations. The raw error is retained; it was not retried
or replaced. Summaries also report success-and-coverage over all 200 attempts.
Overall, 5,559 of 5,560 attempts produced intervals. Boundary results are
descriptive and do not establish regular-model inference.

Re-estimating AR within every replicate does not resolve the observed
undercoverage. At the worst baseline coordinate, `coefficient_1_voxel_1`,
frozen coverage is 0.8800 (Wilson 95% interval 0.8445–0.9083), versus 0.8875
(0.8528–0.9149) with AR refitting. Bias is approximately 0.00117; mean bootstrap
variance is only 0.746/0.742 times empirical sampling variance. This coordinate's
deficit is associated with interval width rather than appreciable mean bias.

Increasing the budget from 399 to 1,999 on the first 100 baseline datasets
changes this coordinate's coverage from 0.88 to 0.87 (frozen) and 0.89 to 0.88
(refit), with paired change MCSE 0.0174. In the censor-continuous subset,
coverage stays at 0.81/0.82. Five independent seeds on 20 baseline datasets also
measure endpoint Monte Carlo variability. More replicates do not explain away
the baseline deficit. Block size four does not improve the lagged-dependence
experiment overall; neither adjustment is supported here as a calibration fix.

An **exploratory diagnostic selected after inspecting the baseline results**
repeats the same 400 responses with known true whitening. Response hashes match
between rank settings, and all 400 public estimated-AR point fits reproduce the
archived baseline values within `1e-12`. At the selected coordinate, known
rank-one whitening raises coverage from 0.8800 to 0.9425 (0.9152–0.9614), with
25 datasets covered only under known whitening and none only under estimated
whitening; the paired increase is 0.0625, MCSE 0.0121. Known full-rank whitening
gives 0.9525 (0.9270–0.9694). See the
[known-whitening summary](../audits/voxelwise-ar-rrg-known-whitening.json) and
[raw records](../audits/voxelwise-ar-rrg-known-whitening.jsonl.gz).

This implicates estimated whitening or its interaction with the bootstrap and
rank estimator. It does not isolate AR-estimation variance as the sole cause,
validate joint HC2 correction, or constitute a prespecified confirmation.
Known rank-one coverage still ranges from 0.9175 to 0.9625, and the selected
coordinate's variance ratio remains 0.880. Known full-rank coverage ranges from
0.9250 to 0.9550. Stationary joint segment-start noise and the bootstrap's reset
approximation also differ. A calibrated inferential method would need a new
methodological change and independent qualification; nominal inference remains
disabled.

The complete study used an immutable compiled snapshot. Its
[archived source bodies](../audits/voxelwise-ar-rrg-study-sources.json.gz) match
the source hashes captured at launch. Later finite-summary guards and export
fixes do not change ordinary arithmetic: all fields except elapsed time match
exactly for 30 attempts across every scenario, both modes, and the larger/seed
sensitivity budgets using the final executable. The
[follow-up receipt](../audits/voxelwise-ar-rrg-followup.json) records these
identities, checks and limits.

The drivers are opt-in JVM test mains, not part of the ordinary test suite:

```sh
sbt -J-Xmx3g -J-XX:ActiveProcessorCount=4 'fitJVM/Test/runMain scalafim.fmri.fit.VoxelwiseReducedRankQualification study /tmp/rrg-study.jsonl'
python3 tools/qualification/summarize_voxelwise_rrg.py /tmp/rrg-study.jsonl /tmp/rrg-summary.json --kind study
sbt -J-Xmx3g -J-XX:ActiveProcessorCount=4 'fitJVM/Test/runMain scalafim.fmri.fit.VoxelwiseReducedRankKnownWhitening /tmp/rrg-known.jsonl'
```

Outputs must be new paths. Summarizing the archived compressed JSONL directly
reproduces the recorded coordinate results without rerunning the fits.

## Measured resources and integration

The [resource receipt](../audits/voxelwise-ar-rrg-resources.json) records 11 fresh
JVM processes on a Mac15,11 (36 GiB RAM, 14 logical CPUs), Homebrew OpenJDK
25.0.1, with `-Xmx3g -XX:ActiveProcessorCount=4`. Workloads have 240 timepoints
in two runs, eight targets, two run intercepts, rank two, and voxelwise AR(1)
coefficients spanning -0.2 to 0.6. Bootstrap runs refit AR and include two named
contrasts. All runs succeeded at rank two; repeated objectives match exactly.

| Voxels | Bootstrap replicates | Fresh processes | Fit seconds, median (range) | Peak process RSS, GiB range |
| ---: | ---: | ---: | ---: | ---: |
| 1,000 | 0 | 3 | 1.91 (1.87–2.62) | 0.414–0.428 |
| 10,000 | 0 | 3 | 34.01 (30.48–36.84) | 1.176–1.308 |
| 25,000 | 0 | 1 | 114.01 | 2.841 |
| 1,000 | 25 | 3 | 44.59 (43.58–45.77) | 0.691–0.837 |
| 1,000 | 99 | 1 | 170.91 | 0.697 |

The fit timer includes preparation, the global solve and all requested bootstrap
replicates. It excludes startup, ten small warm-up fits, input construction and
result extraction. Peak RSS comes from macOS `/usr/bin/time -l` and includes
all those phases. The separately recorded sum of heap-pool peaks is not a
simultaneous heap maximum and can exceed the heap cap; use RSS for the process
memory claim. Garbage-collection deltas cover timed preparation. Task-owned
builds and studies finished before measurement, but shared-host quietness was
not guaranteed. These synthetic runs do not establish a speedup, whole-brain
streaming, or realistic spatial covariance. The two largest-budget rows are
single measurements. Bootstrap work remains expensive and scales with its
replicate budget.

For process measurements, supply a resolved `fitJVM / Test / fullClasspath`
as one colon-separated line in `classpath.txt`, then run:

```sh
python3 tools/qualification/measure_voxelwise_rrg.py classpath.txt /tmp/rrg-resources --voxels 10000 --replicates 0 --repeats 3
```

The scoped branch passes 58 focused JVM and 49 Scala.js tests. After the final
explicit-space NIfTI test migration, all nine writer tests passed again. A
separate integration snapshot combines 271 concurrent root source changes with
13 RRG overlays: `scalafimCompileAll` succeeds, as do 69 RRG/chunk/writer plus
14 fit-estimates tests on JVM and 60 RRG/chunk plus 11 fit-estimates tests on JS.
The integration compile reports four pre-existing concurrent deprecation
warnings (two on each platform) at `SurfaceResamplingBinding.scala:51`; it is
not warning-clean. Its original writer-test compile failure was fixed by using
the explicit-space series reader and the affected checks rerun. Source hashes,
exact commands, exit codes, and archived logs are in the follow-up receipt.
The shared checkout was preserved, and neither local commit has been pushed.

## Named bootstrap contrasts

Declare target contrasts before preparation so that their scalar values can be
computed within every bootstrap replicate. Weights refer to **original full-design
column indices**, not positions within the target partition. Every referenced
column must belong to that partition, including columns assigned zero weight.
Names are nonblank and unique within the request; weights are finite, column
indices are unique, and at least one weight is nonzero.

```scala
val difference = VoxelwiseBootstrapContrast.unsafe(
  "condition_a_minus_b", Vector(0 -> 1.0, 1 -> -1.0)
)
val bootstrap = VoxelwiseReducedRankBootstrapConfig.unsafe(
  resampling = ReducedRankBootstrapConfig.unsafe(399, 1, 19),
  mode = VoxelwiseBootstrapMode.RefitAutocorrelation,
  contrasts = Vector(difference)
)
```

The bootstrap payload's ordered `contrasts` vector contains each definition,
the original fitted `estimate`, bootstrap `standardErrors`, and type-7 pointwise
`lower`/`upper` bounds. These intervals use the distribution of each replicate's
weighted coefficient sum. Subtracting marginal coefficient interval endpoints
does not produce a difference contrast interval. The covariance propagation
identity is useful for checking the reported SE, but cannot reconstruct these
percentile endpoints after samples have been discarded.

Selection and chunk merging preserve definitions and voxel ordering. Exports use
the parameter namespace `bootstrap_contrast:<name>` with coefficient, SE, lower,
and upper maps; sidecars retain original column identities, weights, confidence
level and pointwise scope. Colliding generated map labels are rejected. Overflow
is a typed fit failure rather than a fabricated interval. A zero-spread interval
is allowed. No T statistic, p-value, contrast degrees of freedom, simultaneous
coverage, or spatial aggregation follows from these summaries.

The R bootstrap oracle also computes difference bounds directly from its 32
independently fitted replicate matrices. Shared tests compare those endpoints,
unit and negated contrasts, covariance propagation, selection and merged chunks
on JVM and Scala.js. These are implementation checks; coverage is a separate
question addressed by the larger qualification study.
