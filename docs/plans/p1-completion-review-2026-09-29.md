# P1 completion review — 29 September 2026

This is a source-backed completion plan for the 16 requested Mote tickets. It
does not qualify implementations or authorize publication. Mote remains the
status/ownership authority. No ticket was closed by this review.

## Review basis and findings

Reviewed checkout: `8d0dd730b60a40b093fae0f7ff61996fa071a811`, with substantial
tracked and untracked edits. Local `main` is 92 ahead/28 behind its local
`origin/main` reference; no fetch or remote-parity claim was made. In particular,
the decoder repair remains in dirty `ShapeDecoder.scala`, `WorkReceipt.scala`
and `ShapeDecoderSuite.scala`. HEAD alone does not identify that implementation.
Mote doctor reported a clean operation store. Read all 16 live ticket bodies and
notes, the three additional ProfileHrf prerequisite tickets, relevant source,
tests and plans. Tests and simulations were **not rerun** for this planning review.

The main findings, in completion-risk order:

1. **ProfileHrf's condition qualification is currently invalidated.** PHRF-09 is
   closed with a local decoder repair, but the latest PHRF-21 receipt records 0/24
   compact and 0/20 Gram/compact admissions under the frozen Gaussian budgets;
   Gaussian and LWU SNR 1.0 milestone cells also admit 0%. Terminal curvature is
   now checked at the returned point. The old milestone document still declares
   qualification; retain its results as historical and replace its current
   verdict during PHRF-21. Increasing a justified work budget and remeasuring is
   preferable to loosening scientific tolerances to recover green tests.
2. **Certified condition preparation is still missing.**
   `ConditionProfileFit.prepare` checks dimensions, normalization, noise and
   retention, but does not enforce `ObservedFamilyCertification`. PHRF-12 is a
   real correctness prerequisite, not paperwork.
3. **The atlas boundary work is a candidate, not current-main functionality.**
   Commit `535977a86270bd3cc5c033eda165a0370870dd22` exists and is not an ancestor
   of HEAD. Its recorded 91 JVM/63 JS atlas passes deserve credit. However, the
   cited `/private/tmp/scalafim-viewer-combined-qualification-20260912/qualification.json`
   no longer exists here. Candidate documentation explicitly allows refusal at
   a symmetric cube centre because a unique coordinate witness is ambiguous.
   Resolve that against the original one-voxel-centre distance example before
   calling the requested contract complete.
4. **Atlas coordinate overflow is still present**, including on that candidate.
   `Query.scala` rounds Double to Long then narrows to Int; radius offset
   construction and addition also use unchecked Int arithmetic. Fix this as a
   small independent safety change.
5. **Stock Metal has contrary evidence, not merely incomplete visual review.**
   The latest ticket note records 39/48 colour-oracle passes for both named
   JavaFX versions, versus 48/48 for the correctly activated ES2 control. The
   referenced diagnostic tarball is absent here. Earlier visually similar
   screenshots do not override this failure. Recover evidence or reproduce it,
   then make an explicit admit/reject decision.
6. **First-level calibration's group prerequisite is stale.** Reviewed repair
   `dc2b2ba4200896c1a5a6b1f7a2b19d1c11364227` is an ancestor of HEAD; current
   `GroupWeighting`, `GroupModel`, and `GroupEngine` include PM/mKH. Its Mote
   prerequisite is closed. The known-truth first-level study remains undone.
7. **The remaining IO/performance tickets are substantive.** The pinned image4s
   model (`26a74ad99b9ee49a9555344e19b82d69a2ba50e4`) exposes temporal unit/step,
   but no temporal origin; ScalaFIM defaults series output to one second.
   Estimate IO still declares `scalafim-estimates-development-1`. Precision
   pooling still factors per voxel and allocates run response/QR/readout state.

## Execution order

Use small, reviewable candidates with exact path ownership. Do not incorporate
the shared checkout wholesale. Recover candidate commits and evidence from Git
or their owner before rebuilding already completed work. Missing temporary
worktrees are not a reason to delete their registered branches or profiles.

| Wave | Work | Exit and sequencing |
| --- | --- | --- |
| 0 | Freeze an integration base, preserve the local decoder patch, recover evidence bundles | Source/provider hashes and test commands identify each candidate. Resolve existing owners before edits. |
| 1 | Atlas overflow; boundary candidate review; Metal decision | Small safety win plus closure of two long-running candidate/decision items. Boundary landing remains separately authorized. |
| 2 | Temporal origin; stable Core-NIfTI format; bounded residual traces | Independent workstreams where paths permit. Format work can start without waiting for unrelated workflow serialization. |
| 3 | Precision-pooling baseline and optimization; first-level calibration protocol/simulator | Freeze the estimator candidate for confirmation; performance changes must pass equivalence before being used in the study. Simulator preparation can start earlier. |
| P1 | PHRF-12 and PHRF-32 | Reuse closed PHRF-09; preserve its source patch and tests. These two tasks can proceed independently. |
| P2 | PHRF-21 condition policy/requalification; PHRF-29 trial integration | Condition milestone stays independent of trial completion. Resolve shared decoder policy before freezing downstream evidence. |
| P3 | PHRF-11 → PHRF-33 | Complete public readout, then measure actual decoded B0 and settle the backend decision. |
| P4 | PHRF-14 → PHRF-15 → PHRF-16 → epic | Scientific evidence, repeated performance, release readiness, then parent closure. |

Do not run expensive confirmation or performance grids while source/policy is
still moving. No wall-clock estimate is credible until the requested pilots
measure per-cell cost. The first implementation slice should be atlas overflow;
the first substantial numerical slice should be PHRF-12, followed by PHRF-32.

## Per-ticket completion contracts

### 1. Estimate-set Core-NIfTI — bd-01KX6G9B8R86MRBZ9S8K8F5G7V

Credit existing `NiftiEstimateSink`, `NiftiEstimateSource`, `LocalEstimateStoreSuite`
and `EstimateMetadataSuite`: bounded writes, fresh handles, corruption checks,
partial abort and stale-pointer tests exist. They are not a stable wire schema
or a process-crash matrix.

Complete in three checkpoints:

- **Schema:** move the canonical scientific specification into ScalaFIM; freeze
  versioning, metadata authority, ordered estimand/unit/voxel/covariance axes,
  units, validity and admission. Generate golden JSON/TSV/NIfTI bundles with an
  independent producer. Check effects-only, statistic-only, deficient rank,
  non-estimable contrasts, missing products and covariance reconstruction.
  Reject conflicting axes, duplicate IDs, altered lengths/digests and unsupported
  versions. Round trips using one codec are supplementary evidence only.
- **Durability:** subprocess fault injection at payload close, metadata publish,
  unit publish and collection-pointer CAS; concurrent writers synchronized at
  CAS; retry/idempotence and no-clobber tests. After each interruption a fresh
  reader sees either the previous complete revision or the complete new one,
  never partial scientific state. Do not label process-kill tests power-loss
  proof without the required filesystem flush/ordering evidence.
- **Full retained scope:** compact covariance/support metadata, gzip/frame
  policy, producer/exporter replacement, pinned group/workflow references and
  bounded estimate HDF5 using the same logical fixtures. Measure sparse/full
  access and peak live memory. Fresh-process group reuse must need no fit object.

The ticket still retains HDF5 and integration scope: the Core-NIfTI checkpoint
alone cannot close it. If split into children, preserve a parent gate for those
requirements. Its workflow-serialization blocker is still open; review that
edge for the schema checkpoint without silently deleting it. Incremental
image4s output adoption is already closed. Consumer W5 admission is separate.

### 2. Temporal origin — bd-01M1W6JR7PBT8FYM7DT808V6F4

Implement typed origin in image4s's authoritative header/write/read contracts,
then adopt an exact qualified provider revision. Define precedence between known
series-axis sampling and explicit options: conflicting declarations must refuse
or require an explicit override. A default must not silently write known TR=2
as TR=1. Preserve unknown-unit policy and generic non-time axes.

Tests: second/millisecond units, TR=2 and 0.75, zero and nonzero origins,
negative origins if allowed by the declared contract, nonfinite origin/step,
nonpositive step, metadata conflicts, unknown units and unchanged 3D behavior.
Inspect independent bytes/reader output, including pixdim[4], xyzt_units and
toffset; do not rely only on provider round trips. Exercise provider JVM/JS
contracts and actual supported IO runtimes, ScalaFIM image/dataset reads and
incremental output. Distinguish stored volume times from effective fit sampling.

### 3. Residual traces — bd-01M1Z1KN91XKADQ4Z80PBB781Q

Add a bounded, identity-bound diagnostic request/result, carrying voxel/row/run
axes, source/model/preparation identity, residual space, whitening scope and ACF
lag/gap policy. Retain or replay the exact fitted operator and required nuisance
state. Selected estimate-only artifacts lacking that state must refuse; never
silently refit. Do not expose `Gls.residualMatrix` as a sufficient public API.

Tests: analytic OLS projection and independently solved known-covariance GLS;
check `y = fitted + residual` in each named space, OLS orthogonality and GLS
weighted orthogonality with scale-aware tolerances. Add estimated/shared/voxelwise
AR, different voxel whitening operators, run resets, censor gaps, unavailable
nuisance state, reordered axes, stale source/model, reader failure and cancellation.
ACF must not correlate across declared gaps/runs. Instrument reads and allocation
to enforce selected-voxel bounds; vary chunk size without changing identity or
values. Shared JVM/JS tests plus a file-backed diagnostic consumer are required.

### 4. Precision pooling — bd-01M208TXSYF5Y5GZQ2DY4GZP8S

Measure the current public selected path before changing it. Historical
451/1494 MiB figures are not a current baseline. Separate response gathering,
QR coordinates, precision accumulation/factorization, readout, source and sink.
Target worker-owned reusable buffers and provider-backed solve capabilities only
where profiling demonstrates a benefit. Generic algebra belongs in Gale.

Keep full multivariate pooling and stable QR residual variance. Compare against
independent rational/small dense SVD oracles with correlated coefficients,
unequal/mixed-TR runs, variance imbalance and near-rank boundaries. Exercise
coefficients, contrasts, FIR, marginal/joint uncertainty, exclusions and exact
run/source alignment. Assert unrequested outputs are not produced. Test worker
isolation and block invariance. Reuse `FixedEffectsEstimatesSuite`,
`FirstLevelFixedEffectsEstimatesSuite`, `CompactFixedEffectsSuite` and
`MixedTrFixedEffectsScenarioSuite` on JVM/JS.

Benchmark matched before/after resident and file-backed workloads, including
8192 voxels/block512 and the 64-output FIR case; retain raw warm-up/repetition,
allocation, GC, CPU/wall and RSS receipts. Predeclare the useful-benefit criterion
after measuring baseline variability and before selecting a winner. A documented
decline of an unstable/unhelpful optimization is a valid outcome.

### 5. First-level calibration — bd-01M21VA8DYMNJS9PZ8076SBWWJ

Refresh the consumer protocol against the now-landed group baseline. Remove
universal claims that summed df is necessarily optimistic or that a single
`d_crit=12` proves safety. Freeze/hash the protocol and exact fitter snapshot.
Measure public contrast SEs, not representative voxelwise normalized covariance.

First qualify the simulator: analytic covariance/AR and run/censor behavior,
noiseless recovery, known-covariance GLS and null controls. Complete the declared
group-only critical-df ladder for the actual configurations. Pilot public
`FitPlanExecutor` cells to measure cost, then select estimand-specific replicate
budgets before fresh-seed confirmation.

Cover HRF/FIR, OLS/estimated-AR GLS, shared/voxelwise AR, censoring, unequal runs,
all three run strategies, nuisance count and whitening-block size. Separate
correctly specified and misspecified noise. Report effect bias, empirical
variance, mean estimated/empirical variance ratio, moment df and inverse-moment
df, nonlinear effect/variance dependence, coverage and null rejection. Retain
every cell and failure, with TOST for equivalence claims and declared binomial/
bootstrap uncertainty. Validate reference calculations independently in NumPy/R.
Require reviewed figures with named dispositions and a small deterministic
JVM/JS regression subset. A scientifically adverse or inconclusive result can
complete the study; it does not grant inference admission.

### 6. Unified ProfileHrf epic — bd-01M24MQABKEBQXW89VZ8XWBB8H

Use the adopted remediation graph, not a flat list of apparent P1 tasks. PHRF-09
is closed locally; PHRF-07's backend and PHRF-18's mathematical certificate remain
credited. Add the omitted required **PHRF-32**
(`bd-01M2DF9F9D49JMWGJ2N4YT2K88`) and **PHRF-33**
(`bd-01M2DFAXBST1R9P4GJ29MKT6GM`) to the execution view. Keep finite-state
PHRF-10/30 conditional on the measured B0 decision. Close the epic only after
PHRF-16 evidence is reviewed; child status alone is insufficient.

### 7. PHRF-11 readout — bd-01M24MSJQZH70BTAFYTXD3MPZZ

After PHRF-29, return signed normalized amplitudes and queries at the **returned
continuous shape**, with exact trial/condition identities, nuisance recovery and
constrained ML determinant/derivatives or explicit unsupported outcomes.
Use independent dense augmented equations and a full-data covariance determinant
oracle; test all derivative coordinates with finite-difference step sweeps.
Test cancellation-prone signed queries, equal-sized wrong axes, permutations,
normalization changes, original-equation residuals and Float32 conversion as
separate error sources. Exercise bounded correction and opt-in exact readout;
record every solve and certification cost. Frozen-shape linear operators and
response-adaptive outputs must have distinct contracts.

### 8. PHRF-12 preparation — bd-01M24MSQS8HFQBPY2TAVW1FJGM

Bind admission to family/chart/horizon, basis, drives, trial/condition/run IDs,
sampling/precision, selected rows, nuisance projection and whitening at both
Gram and compact entry points. Require finite nonempty held-out evidence,
projector-error bounds and direct/approximate rank/conditioning margins.

Mutate each identity independently, including same-size wrong bases and designs;
prove typed refusal **before any response read or worker allocation**. Include
near-aliasing, projected rank loss, adequate projected rank despite deficient
expanded Gram, unsupported whitening, empty/nonfinite/out-of-domain certification
points and good frozen C0 geometry. Run `ConditionProfileFitSuite`,
`ObservedFamilyCertificationSuite`, `CompactConditionRuntimeSuite` and fixed-plan
compatibility tests on both platforms, then warning-clean compile.

### 9. PHRF-14 scientific qualification — bd-01M24MSZ4Z6BFWRV5R4S67VDN0

After complete B0, distinguish numerical approximation, search error, scientific
recovery/coverage and resource claims. Use direct-family and same-model dense
references to separate basis approximation from optimization error. Freeze
confirmation cohorts and report null, weak/prior-dominated, boundary,
misspecified, pooling and noise-estimation cases. Use at least 500 independent
replicates per claimed interval regime, increasing counts when uncertainty is
too wide for the declared decision. Include refusals and failed fits in explicit
denominators; conditional curvature is not unconditional interval calibration.
Land small shared regression laws; retain the full seeded study and adverse cells.

### 10. PHRF-15 performance — bd-01M24MT58X8W2Z0KJ25JJG2SGP

Measure only the correctness-qualified complete path. At least five measured
runs after declared warm-up, with raw samples, median/p95, exact host/runtime/
provider/source and actual completed work. Label coarse p95 estimates honestly.
Include preparation, certification, all attempts/fallbacks, whitening, pooling,
readout and IO in separated phases and declared totals. Measure live engine
memory, allocations/GC and process RSS separately. Fill C0/B0 absolute and
same-work ratio cells, JVM/JS evidence and schedule/bandwidth/scaling cases.
No kernel-only timings, extrapolation or failed-admission throughput may fill a
qualified cell. Preserve unmet targets.

### 11. PHRF-16 release — bd-01M24MTB363KSHN1KDRSAM2KNH

Inspect exact PHRF-14/15/21 receipts, supported family/backend domains and all
required dependencies. Run a public consumer example and inspect exported
trial/condition/query axes, units and uncertainty status. Update the work map
and docs so historical and current qualification cannot be confused. Require
warning-clean full compile and required JVM/JS tests in bounded batches.
Optional backend work must not become a hidden release requirement. Release
readiness and actual publication are distinct; publication needs authorization.

### 12. PHRF-21 condition milestone — bd-01M25Q0JY8GA7CJRKZF2934PJ0

After PHRF-12, settle terminal-verification policy using independent Gaussian
and LWU cohorts. Prefer a justified per-family jet budget with explicit old/new
work accounting over a tolerance selected from the failed 24-voxel sweep.
Preserve simultaneous admission/accuracy criteria: Gaussian SNR >=0.5 admission
>=95%, p95 peak error <=0.02 s, FWHM <=0.05 s and relative amplitude L2 <=1e-3.
Retain LWU's historical SNR0.5 shortfall; do not silently narrow the cohort.

Test prior/no-prior, null/weak/ambiguous data, boundary solutions, exhausted
budgets, failed candidate recovery and stale certificates. Compare energy,
shape, signed amplitudes, curvature/SD and status at the returned point against
an independently searched time-domain oracle. Rerun compact/shared-AR and Gram
paths on JVM/JS. Measure actual C0 T600/C3/V100000, <=30 s JVM compute and
<=256 MiB live engine goals; report all phases and status denominators. Replace
the old current verdict with new dated evidence and explicit unmet scopes.

### 13. PHRF-29 plan/executor — bd-01M26A285YAQ3H8TGS26BK9T7J

After PHRF-12/32 and preserved PHRF-09, add the typed sibling `ProfileHrfPlan`.
Exact alpha=0 must use compact conditions without trial-sized preparation;
positive trial variance uses the existing TrialBanded backend. Attach the real
shared decoder, retain returned off-node theta and count terminal checks and
every continuous factor/solve. Reuse immutable preparation and worker-local
scratch with <=8 workers/<=256-voxel blocks and the existing bounded sinks.

Use tiny known-truth fixtures with off-node shape, shared AR/run resets,
reordered identities, varied chunk/worker counts, cancellation, reader/sink
failure and unsupported criteria. Verify isolation/no stale worker state and
no retained output blocks. Preserve `TrialReadout`'s response-independent
contract; separately test a frozen-shape conditional transpose, without calling
it the full adaptive estimator derivative. PHRF-11 supplies final public readout.

### 14. Atlas boundary — bd-01M2B8MAZ0H5PVGD4YES7N36RC

Recover/review candidate `535977a8` and its dependencies; do not rewrite it from
the old issue description. Inspect the public tie/distance contract before
adoption: distinguish all equally near **regions** from multiple equally near
face witnesses within one region. Recommend returning a valid distance with
explicit nonunique witness information where feasible; if retaining refusal,
record the reduced supported domain and its acceptance explicitly.

Tests: analytic 2mm cube centre has distance1mm (or the explicitly accepted
typed ambiguity outcome), face/edge/corner and inside/outside distances;
same-label internal faces excluded; shared different-label interfaces; all
equal-distance region ties; empty atlas and exact-radius inclusion/exclusion.
Use rotated/anisotropic/sheared analytic and high-precision fixtures, affine
translation, extreme magnitudes, inverse-error bounds, candidate budgets and
mid-query cancellation. Preserve the candidate's Decimal counterexample and
ambiguous-witness tests. Rerun `AtlasBoundaryQuerySuite` and atlas JVM/JS on the
actual integration candidate. Consumer adoption and performance remain separate.

### 15. Atlas overflow — bd-01M2BAAYDG9CJRD60YF01MG3B3

Check transformed coordinates before rounding/narrowing; check radius extents,
offset arithmetic and enumeration budgets before allocation. Use a clipped,
bounded atlas-index traversal or a typed refusal for unsupported size. Avoid
wrapping integers and allocating a radius cube proportional to unbounded input.

Regression: identity affine, a labelled voxel at zero, query x=4294967296 must
never hit voxel zero. Add negative/positive Int-edge values, half-voxel rounding
boundaries, values beyond Long, transform-induced overflow, extreme finite
radii/tiny spacing and centre-plus-offset overflow. Nonfinite values should
refuse at whichever public construction/query boundary owns validation.
Property-test finite safe queries against a small brute-force voxel oracle;
preserve normal exact/radius semantics. Run shared atlas tests on JVM and JS.

### 16. Stock JavaFX Metal — bd-01M2BEHG491V7SQAVPGMKQNXG5

First recover or rerun the exact 48-fixture colour gate from the recorded
`7c3ff0a0` candidate and named 25.0.4/26.0.2 runtime closures. Assert actual
MTLPipeline, no fallback/no private patch, exact module provenance and unchanged
oracle. Preserve the 9 face-permutation failures and ES2 setup-error history.
These version numbers identify the ticket's fixtures, not a claim about latest
JavaFX releases.

If a bounded encoding/runtime fix passes, then run real bilateral beta/FIR
correctness, >=1200 same-face picks per admitted scenario, original IDs and
barycentrics, >=20 alternating state updates, stale-cache checks and cancellation/
resource cleanup. Capture matched lateral/medial views at 1x/2x/4x/8x, AA on/off,
screen scales where supported, lighting/underlay/threshold/missingness cases.
Require named human visual dispositions in addition to numerical checks.
Measure painted-frame latency, first show, export, allocation/RSS, mesh/atlas/
upload bytes on packaged production-width cortex. JVM native evidence and
shared JVM/JS semantics have separate scopes.

Otherwise close with **reject/defer stock Metal**, retained failure evidence
and a bounded alternative-backend provider ticket consuming the same shared
plan. Do not build an entire JOGL backend inside this decision ticket, blur the
scientific field, or admit a default based on similar screenshots. Windows/Linux
need their own qualification or typed unsupported behavior. Consumer admission
remains separate.

## Two additional required ProfileHrf gates

**PHRF-32:** repair `TrialBandedSuite`'s purported permutation test: it currently
turns the original two 60-row runs into one 120-row run. Permute whole trial
records while retaining run/response/nuisance/whitening geometry; inverse-permute
amplitudes/query weights and compare with the original result, in addition to
dense parity. Add 3D Cascade34 interior shapes, all first/six unique second
derivatives, constrained determinant, rho=0 nonidentification, no/shared AR with
run resets and lambda 1e-6/1e4. Use step sweeps and conditioning-aware tolerances.
Show that a planted wrong run ID or omitted mixed derivative breaks the intended
assertion. Shared JVM/JS laws and compile must pass.

**PHRF-33:** replace the component benchmark's discarded jets/truth-coordinate
readout with actual PHRF-29/11 decode, returned-point readout and certification.
Freeze B0 T600/N300/C3/F6/V100000/d3, Cascade34, lambda1/shared AR0.3 and the
declared bank/workers/block/output settings. Count all work and verify a small
off-node oracle before timing. Measure <=120 s/<=256 MiB and <=12x/<=24x ratio
goals against equivalent exact fixed-shape work, plus dense/regular schedules
and N1200/V10000 stress. Close with a measured backend decision, including a
failure disposition. If finite-state work is activated, promote PHRF-10/30 and
wire their release dependencies before claiming eligibility.

## Test execution and evidence rules

- Fast deterministic regressions and independent small numerical laws run in
  ordinary shared MUnit CI. Put physical IO/process crash checks on supported
  runtimes. Keep native graphics, Monte Carlo and benchmarks in explicit jobs.
- For each implementation slice run the affected `<module>JVM/test` and
  `<module>JS/test`. Profile laws live in `firstLevelLawsJVM/JS`; selected fitting
  in `fitJVM/JS`; persistence spans `estimates`, `estimatesIo`, `fitEstimates` and
  affected archive/group consumers. IO/native platform exceptions must be named.
- Run `sbt scalafimCompileAll` as the warning-clean gate. Final release testing
  uses bounded module batches with sbt exiting between JS batches; never one
  `scalafimTestAll` invocation on the constrained VM.
- Define tolerances from scale, conditioning and approximation budgets. Preserve
  adverse fixtures. Numerical equivalence, statistical calibration, throughput,
  native visual quality and downstream admission each need their own verdict.
- Store durable receipts with exact source/provider hashes, dirty patch identity
  where applicable, runtime/host, commands/exit codes, fixture hashes, seeds,
  raw results and failed/unrun cells. Temporary paths alone are not evidence
  retention. Recovery/rerun is required where this review found missing bundles.

Planning is complete when all 16 tickets have an implementation/decision path,
independent tests and honest closure criteria. Implementation closure remains
with the named Mote tickets and their reviewed candidate evidence.
