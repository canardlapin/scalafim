# P1 execution evidence, 2026-09-29

This is a local candidate and qualification record. Mote is authoritative for
current issue state. Nothing in this record implies publication, release,
power-loss durability, scientific calibration, or downstream admission.
The original shared checkout contains unrelated edits; implementation candidates
are integrated in an isolated clone.

## Completed decisions and implementation gates

| Work | Reviewed result | Evidence boundary |
| --- | --- | --- |
| Atlas overflowing voxel coordinates | Reject before unsafe integer narrowing; clipped radius search remains bounded | 131 JVM and 95 JS atlas tests; integrated `b07ee661` and `c0b4c9c8` |
| Exact labelled-region boundary distance | Full affine world-space faces, closest witness and ties; independent cube/shear cases | Same atlas receipt; downstream PLS adoption is separate |
| Stock JavaFX 25/26 Metal | Both actual stock runtimes fail visual admission: 39/48 oracle cases | Reviewed reject/defer decision at `39cb4959`; 329 evidence payload hashes verified; 17 JVM tests. Historical patched ES2 is not a fresh rerun |
| Bounded identified residual traces | Actual fitted/prepared response and source-scan segment identity; runwise GLS refusal before read | Focused7JVM/6JS and full340JVM/328JS; integrated warning-clean compile192s. Original trace issue closed |
| Selected precision pooling | Same estimator/accumulation order; measured full-call allocation down3.52%/3.53%, whole-call timing unchanged within observed range | Reviewed20-payload archive, both full fit suites and integrated compile passed. Original pooling issue closed |
| PHRF-12 condition integration | Admission binds actual source, condition labels, weights, frame, precision, nuisance, whitening and lowered column values; cancellation stays typed | Both admission entries 4/4, required condition/certification/compact suites 11/11 on JVM and JS; source `b1df154d`, warning-clean compile-all exit 0 |

## Reviewed local changes whose remaining gates are separate

- Residual capture is opt-in and bounded, binds the actual fit and prepared
  response, and retains physical source-scan lag/segment identity. Runwise GLS
  is explicitly refused before reading rather than accidentally routed through
  a shared GLS engine. Focused tests pass 7 JVM/6 JS; full fit tests pass
  340 JVM/328 JS. The integrated warning-clean compile completed successfully
  (`integration-warning-clean-r2.log`, exit 0,192.02 seconds).
- Selected precision pooling retains the estimator and original accumulation
  order. Paired actual 8192-voxel, two-run, mixed-TR, 64-output full calls reduce
  allocation by 3.52% resident and 3.53% mapped. Full-call times change by
  -0.06%/+0.31%; this is not evidence of a whole-fit speed improvement.
  [The 20-file archive](../precision-pooling-2026-09-29-evidence/SHA256SUMS)
  includes all paired raw samples and the full fit/failed smoke/corrected smoke
  receipts. All hashes were independently verified.
- The model-only `ProfileHrfPlan` declaration at `4f47a4a6` has 5 JVM/5 JS
  tests. It is not an executing trial fitter. Fixed-source compatibility binds
  the exact kernel object rather than a same-sized descriptor.
- Parallel profile execution uses at most one window of `workers` payloads and
  ordered delivery. Worker/sink failures and caller interruption are typed and
  cleaned up; a proposed fixed workload deadline was removed. Exact author
  candidates `74f4adcf`/`fed64de7` pass 11 JVM/4 JS focused tests.
- Decoder equality termination at `bd367f03` passes 19 tests on each platform
  and independent transactional-state review. Exact finite equal energy may
  terminate only through the candidate's own coherent, finite, prior-augmented
  stationary jet with the unchanged tolerance. The frozen JVM Gaussian SNR1
  cohort still admits only 94.5%, below its unchanged 95% gate. PHRF-21 is open.

## Trial-law evidence and identity-preserving interleaved lowering

`7e857fcb` passes eight TrialBanded tests on each platform. It exercises
Gaussian and three-dimensional Cascade34 off-node jets, all unique Hessian
entries at two finite-difference steps, independently applied scalar AR run
resets, dense augmented equations, constrained observation-space covariance
determinants, singleton/coincident conditions, nonzero durations and lambda
extremes. Supported complete-record permutations actually change packed
bandwidth, preserve backend trial coefficients, and carry signed trial-query
weights with the records. Leaving that mapping wrong changes the fixture by
more than `1e-5` against a `2e-8` comparison.

Final candidate `4134c23c`, integrated as `d9a4005b`, adds stable canonical event
rows while preserving caller-order trial columns, membership and labels. Explicit
forward/inverse maps bind those two orders. Generic `EventSchedule` retains its
checked invariant; `ExpandedTrialDesign.lower` now accepts interleaved complete
trial records through this boundary. Actual two-run IID and independently reset
AR laws preserve energy, means, all jets, determinant, trial amplitudes and signed
query values while changing packed bandwidth. Malformed lengths/run IDs/durations
return typed errors before indexing. The label control reaches10001 trials without
lexicographic column reordering.

Author and integrated final checks each pass7 design and9 TrialBanded tests on JVM
and JS; integrated `scalafimCompileAll` is warning-clean, exit 0,174.41 seconds.
Final integration `63df576f` also passes the receipt suite 2/2 and combined
TrialBanded/provider/reduction laws 12/12 on each platform, followed by
warning-clean `scalafimCompileAll` (exit 0,181.25 seconds). PHRF-32
meets its bounded numerical-law gates. `ProfileTrialDrive` still consumes
a checked source schedule; full interleaved adaptive workflow admission is not
claimed. Wrong-map sensitivity is demonstrated; no omitted-mixed-term mutation
claim is inferred.

An independent 60-digit Decimal pivoted solve reuses only exported binary64
dense inputs. At lambda `1e-6`, the augmented system's infinity condition number
is approximately `1.1823e9`; maximum coefficient discrepancy is `1.87e-9`.
At lambda `1e4`, condition number is `1.5247e5` and discrepancy is `1.86e-12`.
Energy discrepancies are below `3.1e-16`. These substantiate the dense-reference
tolerance margins; they do not independently validate basis compilation or
event convolution, which the oracle shares.

## IO and calibration are not completed qualifications

The temporal IO source and receipt bundle was independently verified: 31
payloads and 46 changed-source hashes, exact provider/consumer Git bundle heads,
five passing recorded test commands, and three independently read estimate
bundles. Review found unknown-unit raw temporal spacing zero can normalize to
one under a units-only assumption. The correction is reviewed and tested at provider `2695f891` and consumer
`52b26c52`: provider 59 JVM/40 JS and image 380 JVM/351 JS pass. The pre-fix
regression fails as expected. Refreshed transport archive SHA-256
`045e69d29bffdccc0d27aa75af9c48336b1201b3e412861f6295f49aadccec69`
and all 38 payload/46 source hashes were independently verified. No qualified provider publication or pin exists yet.

Estimate wire format remains `scalafim-estimates-development-1`. Bounded HDF5
slice writes, compact covariance/support, producer/exporter/workflow/group
adoption and broader access/performance gates remain. The crash record is four
completed public-operation boundaries times three fresh processes, not twelve
distinct fault boundaries or power-loss proof. Historical runner metadata
records commands/times, not cryptographic source-tree identity; the crash
receipt hashes a classpath string, not compiled class contents.

Known-truth calibration foundation `c2d6d401` includes independent simulator and
OLS/known-AR fixtures and a completed native 144-cell by 10000-study PM/mKH
ladder. Its conditional threshold is 12 residual degrees of freedom. The
historical Python reference uses known tau-squared where native PM estimates
it, so agreement of that threshold is not cross-provider parity. The actual
500-replicate effect/null first-level pilot completed, exit 0 in2880.14 seconds:
72 cells,96 streams (including diagnostic second runs),384000 rows, zero missing
replicates or failed fits. Eight voxels and additional runs are diagnostics rather
than independent Monte Carlo replicates. Matched descriptive variance ratios span
0.907–1.083 and coverage 0.928–0.966. These are exploratory pilot results.

Independent scientific review holds confirmation: the inverse-variance degrees
of freedom proxy is poorly identified at this high df and replicate count; Step0
must validate its uncertainty, scale sensitivity and undefined outcomes using
known Gaussian/chi-square controls. Keep the original estimators and gates.
The proposed 2000 effect coverage replication count has no passing count under its
adjusted exact interval; 5000 has nominal per-endpoint power about0.955. Null 15000
has nominal per-endpoint power about0.833, without an all-endpoint assurance.
Exploratory2000-draw percentile bootstrap cannot qualify the proposed adjusted
ratio tails. Explicit primary families, simultaneous inference, prospective power,
separate effect/null launch streams and disjoint seed ranges must be frozen and
reviewed first. Current include-null launch runs both streams at the same count;
15000would take roughly24 hours before excursions/analysis. No confirmation has
been launched and no scientific calibration gate is closed.

## Durable embedded receipts

[SHA256SUMS](SHA256SUMS) hashes gzip-compressed JSON archives. Each payload
contains its original relative path, exact SHA-256 and full text:

- `initial-profile-evidence.json.gz`: fourteen historical source-bound
  admission/compile/cohort/law logs and metadata, plus the then-current law diff.
  Its law snapshot predates the final signed trial-query addition.
- `trial-laws-supported-domain.json.gz`: historical supported-domain law source,
  both-platform raw log/metadata and independent precision-check receipt.
- `trial-laws-interleaved-final.json.gz`: the final three interleaved-lowering/law
  sources plus author and integrated raw logs/metadata and precision controls.
- `bounded-banded-work-final.json.gz`: exact reviewed source/test, author and
  integrated counter/law/compile raw logs and metadata.
- `condition-admission-open-receipts.json.gz`: both unchanged cohort gate failures
  and the separate passing all-attempt diagnostic; it records open qualification.

Full adaptive TrialBanded execution, public normalized trial readout/query and
ML determinant derivatives, whole-engine attempted-work accounting, actual C0/B0
measurements, numerical/scientific qualification, backend crossovers and release
remain separate gates. A component benchmark or small-voxel extrapolation cannot
satisfy those gates.

## Bounded banded-work accounting

Counter candidate `5b9c7b8e` plus reviewed comments-only correction `b3ac2fbd`
adds immutable shared node-bank setup and fresh per-worker attempted/failure
receipts. The new suite 2/2 and historical laws 5/5 passed on each platform; both
fit compile targets passed. Independent review verified every banded factor and
all six banded solve sites charge entry attempts before calls. Reference failures
retain preceding work; the test reaches six derivative solves/24 RHS before release
rank refusal, then a separately counted exact-reference readout failure.

These metrics cover reference-build Cholesky and banded solves only. Small release
solves, reduction/curvature checks and failed-bank setup receipts are excluded.
Exact-readout factor attempts are reference requests, including dimension refusal
before numerical work. The outside-chart underflow control is an accounting
regression, not evidence of an actual rejected decoder trajectory. Whole-engine
accounting and measured B0 execution remain separate gates.

## Review delivery and acceptance

Fray's durable transport is reachable. Bounded controller timeouts and long worker
turns require explicit handoff/re-entry; an idle host has no automatic wake without
a live listener. Mid-turn interpreter objections were queued until the next managed
turn. Preliminary interpreter `30851a87` passes 6 tests per platform but retains the
rejected production hash; it is not accepted or integrated. The worker now read
the review and received the authoritative Mote claim for six corrections. Acceptance
requires corrected source identity, meaningful both-platform regressions and a new
independent verdict. No stored message or test artifact was lost.
