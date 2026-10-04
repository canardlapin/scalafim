# Prepared public trial outputs

This additive seam keeps `PreparedProfileHrf.run` and its raw adaptive trial
readout unchanged. `trialOutputs` binds the exact prepared bank, caller trial
and condition order, structural compiled baseline column IDs, and one-based
physical response rows. Both condition routes refuse a trial view. A nonempty
baseline needs an existing, valid compiled schema whose matrix and full ordered
row frame match the actual prepared nuisance columns; no fallback IDs are made.

```scala
val prepared = ProfileHrfFit.prepare(plan, selection, whitening, decodePolicy)
  .fold(error => throw new IllegalArgumentException(error.message), identity)
val outputs = prepared.trialOutputs
  .fold(error => throw new IllegalArgumentException(error.message), identity)
val weights = Vector.tabulate(outputs.axis.trialIds.length)(i =>
  if i == 0 then 1.0 else if i == 1 then -1.0 else 0.0)
val query = ProfileTrialSignedQuery.make("first minus second", outputs.axis, weights, 1e-6)
  .fold(error => throw new IllegalArgumentException(error.message), identity)
val request = OutputRequest.TrialQueries(Vector(query), NormalizationRule.Density)
val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
  def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock) =
    payload.results.foreach(result => consume(result.selection, result.output))
    Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))
val corrected = outputs.run(reader, request, ProfileTrialReadoutMode.CorrectedReference, sink)
val exact = outputs.run(reader, request, ProfileTrialReadoutMode.ExactShape, sink)
// JVM only, one distinct caller-owned reader for each actual worker:
import ProfileHrfTrialOutputsParallel.*
val parallel = outputs.runParallel(readers, request, ProfileTrialReadoutMode.CorrectedReference, sink)
```

`consume`, `reader`, `readers`, and the typed model inputs are supplied by the
application. The tests exercise the same prepare/view/query/explicit-run flow.
The mode has no default. The corrected mode applies three reference inverses
and one residual correction. Exact mode separately counts a factor at the
returned continuous shape. Both report the nearest bank node using grid-interval
scaled chart distances and the lower-index tie rule, independently of the
starting decoder node and the basis compilation grid.

Only `DecodeStatus.Accepted` receives conditional output. Every other status
retains the full `ShapeDecodeResult` as an indexed refusal, with no readout or
exact retry. Raw objective energy, Hessians, amplitudes and SD remain selection
quantities. Public trial/condition coefficients are native coefficients divided
by the normalization scale; nuisance coefficients retain native units, and the
equivalent normalized penalty is scale squared times native lambda. An already
whitened response is passed to the public readout, so AR is applied once.

Requests, query identity, supported rule and certificate kind are checked before
reader descriptors, cancellation callbacks, workers or sinks. Scale and transported
lambda are checked at the frozen continuous shape. Original-equation certification
is refused. Prepared-normal-equation residual evidence and Float32 query conversion
audits are distinct; neither qualifies an adaptive-estimator Jacobian or SD.

One executor handles gather, cancellation, errors, delivery and termination for
both payload types. The immutable axis/bank is shared and decoder state is local.
Each accepted voxel owns one ephemeral frozen public worker. Its actual numerical
snapshot is accumulated in `finally`, including failures. `progress.trial` counts
decoder plus public numerical work once; `publicReadout.numerical` is an overlapping
subtotal, not another additive total. The legacy `voxels` counter counts `pointAt`
installations and can be twice physical `attemptedVoxels`. Setup is separate.
Local actions and row visits are explicitly completed-result-only. Scoped wrapper
storage high-water values and produced/delivered trial vector values exclude engine
scratch, responses, other result vectors and object overhead. Query-only results
retain no full trial vector, although numerical scratch remains O(N).

Parallel failure can return `WorkersStillRunning` with receipts and a termination
handle, without final progress. Readers remain caller-owned and in use until the
handle confirms termination. `awaitFinal()` takes one immutable final snapshot,
including in-flight conditional work, and returns the same value on repeat calls.
There is no sink delivery after return. The integration tests use a held real reader,
sink rejection with caller interruption, prompt release and cleanup deadline expiry.

## Verification scope

The source-bound manifest and raw/meta receipts accompany the handoff. Required
checks are the new shared suite, existing fit/readout/output-request suites on JVM
and JS, the full JVM parallel suite, then warning-clean `scalafimCompileAll` via the
shared-lock wrapper. The new seam tests compare actual decoded coordinates against
separately invoked existing readout and independent dense prepared equations;
exercise two-run AR resets, signed/off-node/nonsequential responses and baseline,
normalization, signed near-cancellation, admission, nearest-reference selection,
failed numerical accounting, and actual workers 1/2/8 × chunks 1/2/256.

This is local prepared-basis seam evidence. Early ML refusal is unchanged. PHRF-29
full closure, original-family accuracy/certification, scientific calibration,
throughput, release and publication are not established by these checks.

## Checked public execution intent

The public view's pure executionDeclaration(request, mode, evidence) admits
the request and returns immutable execution intent before any reader or sink
access. ProfileRunSummary.publicExecution retains this declaration, including
the exact physical axis/preparation, public contract version, requested mode,
normalization, evidence kind, output kind and ordered query labels, weights and
absolute tolerances. The query weights are retained immutable request data;
query-only execution still emits no full trial-amplitude vector. Query tolerances
remain conversion-audit tolerances, not original-equation certificates.

The public provenance removes the legacy exact-readout=true suffix and adds
the actual requested public route. Its explicit intent-only label makes no claim
that a factor ran for a decode-refused or cancelled voxel. Actual attempted and
successful operations remain in the work receipts. Ordered identities use
length-framed labels and exact hexadecimal doubles; changing mode, normalization,
output kind, query order, weights or tolerance changes the declaration. Equivalent
views of the same preparation and sequential/parallel runs preserve its identity.
Legacy PreparedProfileHrf.run keeps its existing provenance unchanged.

ProfileFitError.publicExecution retains admitted intent on execution errors,
including descriptor/reader/sink/cancellation/numerical failures. A runtime budget
refusal uses TrialExecutionAdmission with the same declaration. Invalid scientific
requests and unavailable certificates refuse before creating an admitted declaration.
WorkersStillRunning carries the public provenance and exposes the exact declaration
through its termination handle; awaitFinal() retains that same immutable object
without reading live work counters. The held-reader regression checks identity
before and after termination as well as the existing lifetime/delivery controls.

The original production defect is preserved at e6543fb639b53b1fe938b7465540bd9747d5424f,
with original receipts below. The provenance regression was added against unchanged
production before this repair; its separate source snapshot, actual raw log and
wrapper metadata accompany the corrected handoff. No numerical kernel, readout
math, decoder, counter semantics, cleanup policy, ML admission or build pin changes.

## Original e654 source-bound gate receipts

Final frozen checks completed with real wrapper exit 0: JVM 72/72 test executions
(shared seam 23, parallel full suite 24, existing fit/readout/output suites 25),
JS 48/48 (shared seam 23, existing fit 14, readout 8, output requests 3), and
`scalafimCompileAll`. Both complete raw logs contain no warning/error/failure/skip
lines. Inherited legacy tests are included in the named suite execution counts.

The exact commands, wrapper metadata, seven-path source closure, and 24 unchanged
base prerequisites are retained in
`/private/tmp/scalafim-execution-20260929/profile-public-seam-final-receipt.json`.
For the original e654 candidate, only that receipt section was appended after the
frozen gates; all six original Scala source/test paths matched its gate manifest
byte for byte. The corrected candidate has separate source-bound receipts.

- `/private/tmp/scalafim-execution-20260929/logs/profile-public-seam-check8.log` (suite totals [47]), SHA256 `1fb1c0a132ba166b2e775dd370ae618a441b538181a4e05aa674f4f05a80289d`; metadata SHA256 `6d9895a85dd402fe2e1a298ca126f81049b8b2827b692c72ac5c9d040fa8c181`.
- `/private/tmp/scalafim-execution-20260929/logs/profile-public-seam-final-gates.log` (suite totals [25, 48]), SHA256 `07fe9d67d348ce192f3c8486498af079bdfe134bc5807a36e79b6f8d925d84a2`; metadata SHA256 `bc3116d3d90c7ac44e7ffe4734f68b8bd324d3726c9e4acb8528247a611c3170`.

## Corrected provenance gate receipts

The original-production regression ran 24 JVM test executions: 23 passed and the
sole new assertion failed because corrected output declared exact-readout=true
after its actual exact-factor-attempt count was verified to be zero. Its complete
raw log and actual wrapper metadata are preserved separately. The two earlier
reproduction attempts stopped before tests on sandbox boot-lock access and a
generated graph4s build-settings class; they are environment evidence only.
The derived config-classes cache was preserved by reversible rename under the
shared lock, with unchanged dependency source/pin hashes recorded in the cache
repair receipt.

The corrected frozen gate completed with wrapper/child exit 0: JVM 75/75 test
executions (shared public 25, full parallel 25, existing fit/readout/output 25),
JS 50/50 (shared public 25, existing fit 14, readout 8, output requests 3), then
warning-clean scalafimCompileAll. Counts include inherited legacy tests. The
complete raw log contains no warning/error/failure/skip lines.

All seven files matched logs/profile-public-provenance-check1-source.json while
the gate ran; all 24 prerequisites stayed byte-identical to the accepted base.
Only this documentation receipt section was appended after those gates. The six
Scala paths still match the frozen source snapshot. The final seven actual file
hashes, exact commit, commands, metadata and original reproduction evidence are
bound by /private/tmp/scalafim-execution-20260929/profile-public-provenance-final-receipt.json.

Independent corrected-SHA review and integration remain pending. Original PHRF-29
full closure, native ML routing, original-family certification, calibration,
throughput, release and publication remain outside this bounded repair.

- Frozen source manifest SHA256: 370e1e250a864db8ea89facb6fde22b97445ed7e762da03de0efaaaf3922229b.
- Original regression raw: /private/tmp/scalafim-execution-20260929/logs/profile-public-provenance-repro3.log, SHA256 a3ffd6978b2686121b49ea8e16c04bb6fa342775b49f999758bdba115739759d.
- Original regression metadata: /private/tmp/scalafim-execution-20260929/logs/profile-public-provenance-repro3.log.meta.json, SHA256 79da11603cbed2e469afe68cff134397052459d7823c5e84a37ffdec5c16963b.
- Corrected gate raw: /private/tmp/scalafim-execution-20260929/logs/profile-public-provenance-check1.log, SHA256 7635195b894ed74a9d74f521a7c5e245912833e7aff5b2ddaf718fac0d542aed.
- Corrected gate metadata: /private/tmp/scalafim-execution-20260929/logs/profile-public-provenance-check1.log.meta.json, SHA256 46075bfa78d0d50eadfddba93d26681bdcbf68991029f4eb73c33dc51bf77512.
