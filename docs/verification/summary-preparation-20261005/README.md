# Sampled summary preparation verification

Mote `bd-01M47GK9VZNQRGCERREZ09PXW1` admits sampled summary work before
preparing standalone condition outputs that return a `ShapeSummary`.

`ParametricHrfFamily.validateSummaryGrid` is allocation-free. Analytic families
admit without a sampled grid; LWU checks its fixed 0.01-second floor grid.
Custom sampled-summary families must advertise admission by overriding this
capability. `summariesEither` separately validates decoded points, catches
summary callback failures and rejects nonfinite returned summary scalars.

`ConditionProfileFit.prepare` admits summaries before retention geometry.
`CompactConditionPreparation.prepareWithSummaries` explicitly opts in before
geometry preparation; ordinary compact preparation remains reusable.
`CompactConditionRuntime.prepare` returns typed admission/construction errors.
Its legacy public constructor checks summary admission before decoder callbacks
and scratch allocation. Standalone fitting keeps the existing result types,
with typed fit alternatives and throwing compatibility facades.

Unified raw profile outputs contain coordinates and amplitudes, without a shape
summary. Their fixed/compact paths now share the same numerical core through
private raw readouts and omit discarded summary evaluation; the trial path
continues to request no summary work. No placeholder summary is constructed.
Existing public provenance and execution declarations remain unchanged.

Run `python3 docs/verification/summary-preparation-20261005/verify.py`, followed by
`python3 docs/verification/summary-preparation-20261005/consumer-gates.py`, in an
exclusive shared-worktree build slot. The runner tests HRF capabilities and fit
sentinels on JVM and Scala.js, runs four safe source mutations, restores exact
source bytes, then checks the existing profile scientific/progress tests. The
receipt and archive retain commands, counts, logs and source snapshots. The
supplementary runner adds public trial outputs, JVM parallel execution and the
existing condition/compact scientific fixtures on both platforms.

Mutation fits use only admitted Gaussian grids, 60 rows and two voxels. Summary
refusals are declared probe errors, so removing preflight cannot allocate a
large grid. The raw-output mutation invokes a sentinel summary callback.

Tests cover refusal before scientific callbacks/readers, opt-in compact
preparation and legacy construction, admitted Gaussian summary values, decoded
callback failure after a delivered block, and all three raw output routes.
Raw route tests compare emitted numeric IEEE bits, status, provenance and
progress with/without an unused custom analytic summary returning Inf/NaN.
The existing numerical algorithms are unchanged; no R algorithm was ported.
