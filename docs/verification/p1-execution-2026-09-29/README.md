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
| Temporal origin and sampling policy | Reviewed hosted image4s pin preserves temporal origin and rejects invalid spacing without fabrication | Final default-pin image380/351, estimates11/11, IO20/4 and warning-clean compile; provider draft PR13, no merge/release |

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
  ordered delivery. Exact author candidates `74f4adcf`/`fed64de7` pass
  11 JVM/4 JS focused cooperative-worker tests. Review of the new interpreter
  exposed a remaining cleanup defect: timeout after an earlier failure or an
  interrupted termination wait can return while a blocked reader remains active.
  Final mutable counters and reader lifetime cannot be claimed until workers stop.
  The repair is independently accepted at `d1879b4d`, with a nonfinal
  termination handle and deterministic blocked-reader regressions. Its final
  counter-adoption is independently accepted at `7bd14ec9`; exact integration
  `9235c3c7` passes 56 JVM/25 JS tests and warning-clean CompileAll in
  163.56 seconds. The public readout seam remains open. The older cooperative tests do
  not establish this stronger contract.
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
and all 38 payload/46 source hashes were independently verified. The owner authorized
publication of only the provider review branch and draft PR. Exact local,
tracking and remote `review/nifti-temporal-origin-20260929` refs agree at
`2695f891cbec31a7f565a9b39e2554fe3b6d4b40`; [image4s draft PR13](https://github.com/canardlapin/image4s/pull/13)
has that head and unchanged main base `26a74ad`. This does not authorize merge,
release or ScalaFIM publication.

Default-pin candidate `b6501047` passed image380/351, estimates IO20/4 and
warning-clean CompileAll without local override. The final integrated candidate
`2e230d4e` passed image380/351, estimates11/11, estimates IO20/4 and warning-clean
CompileAll, exit0 in218.804 seconds. Raw log SHA256
`b32b99044f683ab8c4139cbfbca0f20cac08ecb560fe4c6f59631eb53c7b99cf`. The temporal ticket is closed; stableCore remains open.

The final Core-NIfTI-1 milestone `43673482`, integrated unchanged as
`3ad53957`/`536a1a67`, is independently accepted. The reader enforces valid decoded
Float32 representability, owns malformed-gzip streams through construction,
preserves explicitly Unknown legacy correspondence without inferring alignment,
and admits the independently authored complete literal Core bundle. The earlier
`11fe7e99` rejection remains recorded as historical review evidence.
All19 final source/blob/hash identities match. Integrated estimates12/12,
IO30/5, group75/74 and fit-estimates14/11 pass on JVM/JS, followed by warning-clean
CompileAll, exit0 in182.357464 seconds. Raw SHA256
`03a031794f3343a234321fb8fb2ff072c2bce57b651458011f5cecc4a3bf6598`.
The reader still supports the explicit development discriminator; its fixtures
are not relabelled as Core conformance. The final archive retains43 payloads.
Bounded HDF5 slice writes, compact covariance/support, producer/exporter/workflow
adoption and broader access/performance gates remain open. The earlier crash
record is four completed public-operation boundaries times three fresh processes,
not twelve distinct fault boundaries or power-loss proof. Historical runner
metadata records commands/times, not cryptographic source-tree identity; the
crash receipt hashes a classpath string, not compiled class contents.

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
adjusted exact interval. Historical alpha/72 in each tail supports equivalence
Pass tests but does not give a simultaneous two-sided confidence family for both
Pass and Fail. The revised protocol uses alpha/(2*72) in each tail at family
alpha0.001: coverage5000 has nominal per-endpoint power about0.9333 and null15000
about0.7757, without an all-endpoint assurance.
Exploratory2000-draw percentile bootstrap cannot qualify the proposed adjusted
ratio tails. Explicit primary families, simultaneous inference, prospective power,
separate effect/null launch streams and disjoint seed ranges must be frozen and
reviewed first. Source `446e96b8` now has separate effect/null modes, shared campaign-entry guards,
protocol-derived budgets, pilot completeness checks, explicit disjoint seed ranges
and Step0 synthetic scale/lower-tail controls. The legacy joint mode is explicitly
blocked for confirmation. The candidate 5000-effect/15000-null backbone plus
14 proposed excursions costs about17.36 hours before overhead. Source `4bc68c5d`
fixes undefined inverse-moment bootstrap handling and pre-fit launch guards, and
uses all5000 planned backbone effect replicates for regular moments. Its Gaussian
influence family method failed independent synthetic controls:24/4000 family
misses, with95% lower error bound approximately0.00414 against a0.001 target.
The54-coordinate training-covariance validator also does not reproduce the
300-coordinate native procedure or2000-replicate excursions. The method is not
admitted; no critical-value tuning is authorized. Native regular-moment,
distance-correlation and conditional group-transfer conclusions remain Unresolved.
Final reporting/proposal `42c394dc` is independently accepted: all50 planned
cell rows retain original descriptive moments, failure/nonfinite/missing IDs and
valid neighboring points. Every regular/dependence/group-transfer decision remains
Unresolved. Independent R checks reproduce the corrected CP thresholds and marginal
powers; 9/9 JVM and JS and CompileAll pass. Parent inspected the revised standalone
df legend and accepted it as descriptive. The concrete748000 replicate-block,
16-command campaign is frozen at a measured projection62506.25 seconds (17.36h)
before overhead. Actual user cost approval is pending; no confirmation has been
launched and no scientific calibration gate is closed.

## Current independently reviewed prerequisites and open candidates

The actual trial executor candidate `2c22772e` passes32 JVM test executions,
13 JS and full compilation. Independent review accepts its scientific and
bounded-worker phase: distinct readers, actual1/2/8 workers and1/2/256 chunks,
ordered output, physical identities and fieldwise attempted counters. The historical
blocked-reader defect is repaired in independently accepted `d1879b4d`: monotonic
bounded cleanup retries interrupted waits; expiry returns explicit nonfinal failure,
ordered delivery IDs and an owned termination/lifetime handle. Final progress is
available only after join. Its47 JVM/17 JS and CompileAll receipts are verified.
The author is adapting the accepted22-field schema and improving nonexpiry latch
controls before a final parent integration gate. Public readout wiring and literal
PHRF29 closure remain separate.

Stage1 corrected conditional trial solve `bf05dde6` plus `72ed9bc6` is now
independently accepted and locally integrated as `bf304201`/`5cb42af9`.
Corrected error ratios7.9146/7.9664/7.9869 separate cubic behavior from the
actual omitted-correction predictor's4.1783/4.1038/4.0558. Separate conditional
request/inverse/correction attempt and failure counts preserve legacy semantics.
The actual snapshot has22 fields; the peer's earlier20-field prose was corrected
against source. Benchmark adapter `498021e3` aggregates and reports all22.
Final integrated core/accounting7/7 and laws9/9 pass on each platform, explicit
`fitBenchJVM/compile` and warning-clean full compilation exit0 in170.473552s.
Raw SHA256 `b7a35f10d95e8eaa221bb4cfd2709b7d6a632a8e69735b214164c8878136837e`.
The receipt identifies the exact seven changed-source files, including three
accepted diagnostic files uncommitted during that historical gate; that tested
HEAD alone is incomplete. The exact unchanged prerequisite files are now recorded
locally in `041514a6`. Executor aggregation adoption is a pending consumer gate.
This is internal prepared-basis readout, not normalized public queries, ML or
certified original-family error. No benchmark runtime or speed claim follows.

Gale's generic accepted-factor determinant-jet prerequisite is independently
accepted at an exact three-file uncommitted snapshot, with20/20 tests on JVM and
JS and mutation sensitivity. Review package SHA256
`8d3a7b3fed1b7c9b5a2983fc9ff2fbacd22f68d03c25e6ee433aa9a0253f7c23`
is frozen pending separate publication authorization. No Gale commit, hosted
pin, fit ML criterion or consumer qualification is inferred.

The C0 harness has repaired row-major gathering and chunk-independent checksums,
but its second pre-fresh review still requires exact projected Newton stationarity,
complete per-voxel/audit/cap records, bounded continuous reference storage and a
full paired development-path control across the declared reference panel. The
subsequent targeted test command was invalid: JVM ran zero tests and JS rejected
its arguments. That exit1 receipt is preserved; it is not passing evidence.
An unfiltered development-only rerun is queued. No fresh comparison or100k run
has occurred. The contaminated7000930101 stream is entirely development-only;
replacement7000930201 and the other two fresh streams remain unconsumed.
Exactly one prospective D2 policy candidate and unchanged scientific gates apply.

Run-aware AR transpose is independently accepted:19 tests on each platform plus
warning-clean CompileAll, independent dense defining-equation oracle and two
sensitive planted mutants. Exact unchanged files are local commit `d797a0dd`;
the10-payload archive retains the original uncommitted snapshot and receipts.
Full conditional consumer adjoint qualification belongs to public Stage2.

The public trial output/query/normalization/conditional-adjoint writer is active
in its isolated tree, with the global22 attempted counters unchanged and separate
local action/visit/output/scratch receipts. The ML helper has a separate decided
three-new-path scope using accepted local Gale capabilities: full whitened trial
design, intrinsic fixed lambda, explicit3D derivative-layout remapping and typed
solve/residual/work receipts. The existing owners will wire coherent reference,
criterion and executor routes after review. Public ML remains explicitly refused;
local provider tests cannot satisfy the hosted exact-pin gate.

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
- `trial-conditional-stage1-final.json.gz`: fourteen exact source/raw-log/metadata
  payloads for the accepted corrected core, native diagnostic prerequisites,
  benchmark adapter and author/integrated gates; all payload hashes verified.

[Current review dispositions](current-review-dispositions.json) preserve exact
rejected candidate identities and the concrete repairs required before admission.

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

## Hosted temporal-pin and benchmark follow-up receipts

`temporal-hosted-pin-final.json.gz` embeds the five default-pin author logs,
the final combined integrated gate, the narrow benchmark compile, matching
exit receipts, publication receipt and exact temporal/pin source files.
`io-handoff-20260930-r2.tar.gz` retains the independently verified38-payload
provider/consumer handoff (incremental Git bundles require their stated bases).
Neither archive claims a stable Core format or image4s main merge.

The benchmark constructor was repaired in `cb1e71d1`, integrated `1d2bdf5d`:
all16attempted fields are summed from disjoint workers; banksetup remains
separate. `fitBenchJVM/compile` passed warning-free (50.36 seconds). This target
is outside CompileAll and must be an explicit gate for future counter or B0
harness changes. No JMH harness or throughput run is claimed by this fix.

## Final bounded review archives

- [Core final qualification](io-core-final-qualified.json.gz):43 embedded source,
  handoff and raw/meta payloads; author43673482 and tested integrated536a1a67.
- [AR transpose prerequisite](ar-adjoint-final.json.gz):10 embedded payloads,
  original snapshot, both-platform receipt and sensitive mutant evidence.
- [Calibration reporting/proposal review](calibration-final-reporting-reviewed.json.gz):
  20 payloads; negative method validation and frozen reviewed rate proposal,
  with no campaign or inference admission.
- [Accepted local prerequisite commits](accepted-prerequisite-local-commits.json):
  exact unchanged six-source identities, no publication or work-policy promotion.

All transport hashes and embedded payload hashes were read back and verified.
The SHA manifest covers the named archives and disposition JSON; this README is
not itself included in that manifest. Original7/16 ticket completion is unchanged.

## Latest independent reviews and integrated executor gate

The executor lifecycle and 22-field work aggregate are locally integrated at
`9235c3c7`. Its exact eight source paths match reviewed `7bd14ec9`; the
final integrated gate passed 56 JVM and 25 JS tests plus warning-clean
CompileAll. [The archive](trial-executor-final-integrated.json.gz) embeds
all eight paths, independent review, author source manifest and actual integrated
raw log/metadata/receipt. PHRF-29 remains open for the public consumer seam.

Public readout `bab9a99f` needs truthful storage receipts: its wrapper buffers
and retained expanded source are not total worker or engine memory. The
review found no numerical solve, transpose or normalization blocker.
Local ML helper `28382eae` needs a canonical factor/value identity binding;
a solve residual does not prove that binding. Both repairs are assigned
to their original owners. Neither candidate is admitted as full PHRF-11.

The C0 full development test ran for 260.51 seconds and timed out under
MUnit's 30-second default; JS did not run. This is preserved as non-evidence,
with no inference of scientific failure or success. A bounded unit panel and
separate standalone cohorts are being implemented. No fresh qualification or
actual 100000-voxel workload has been launched.
[Review history](review-history-20260930-0530.json.gz) preserves these verdicts
and the actual timeout receipt. Gale review publication and the projected
17.36-hour calibration campaign still require the separately requested approval.

## Reviewed public and criterion milestones; C0 oracle gate

The repaired prepared-basis public readout `d7a9e900` is accepted and integrated
at `43db2e83`, together with the reviewed executor. The combined gate passed
67 fit tests plus nine laws on JVM and 36 fit tests plus nine laws on JS, then
warning-clean CompileAll. All fourteen source hashes were checked. Its storage
receipt names wrapper buffers, retained adjoint rows and expanded source
separately; it does not claim total or peak engine memory.
[Exact source and receipts](trial-public-executor-qualified.json.gz).

The immutable factor/value helper at actual `4ec40483` is accepted for its
bounded local-provider contract: 17 tests per platform and CompileAll passed,
with sensitive factor-identity and derivative mutants. The initial handoff
text named a nonexistent SHA and was explicitly corrected by its author.
Gale remains a local override; Java25 evidence is not JDK21 or hosted-pin
qualification. [Helper review and evidence](trial-ml-helper-local-qualified.json.gz).

The typed criterion adapter `0017dafd`, integrated `d71e5ca9`, passed independent
review and 26 tests on each platform plus warning-clean CompileAll. Its analytic
controls exercise every unchanged decoder path for `J=E+sigma2 D`, identity and
epoch binding, bounded terminal evidence, and prior-free conditional SD.
[Adapter evidence](trial-criterion-qualified.json.gz). These controls do not
admit native trial ML; a separate coherent native backend is now assigned.
The executor still refuses ML.

The complete bounded C0 harness `179af271` passed seven unit controls per
platform, but the independent search oracle failed its predeclared amplitude
adequacy margin on both platforms. Five of six random development responses
also have lower direct energy at decoded coordinates than the searched
reference. Independent review identified premature seven-level mesh exhaustion.
[The exact negative evidence](c0-oracle-unresolved-179af271.json.gz) is preserved;
it is neither a candidate scientific failure nor successful qualification.
One fixed development-only oracle resolution repair is authorized, with
unchanged margins, policies and seeds. No full frozen200, fresh cohort or
100000-voxel qualification has been launched. Original7/16 ticket completion
is unchanged. Separate Gale publication and calibration-cost approvals remain
pending actual user replies.
