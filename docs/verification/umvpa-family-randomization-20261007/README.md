# UMVPA complete voxel omnibus family candidate

## Implemented scope

This source implements one complete, predeclared omnibus forward-association
family: one hypothesis for each ordered neural voxel. Its complete member
vector must equal the frozen `C1Contract.multiplicityFamily`. Each voxel's null
sets **all** coefficients of the discovery-frozen target coordinates to zero.
The target coordinates, actual confirmation targets, nuisance design, known
row covariance shape, sources, exposure account, alpha, seed and fixed B are
bound before execution. The initial source-capture replay policy is mandatory
and bound in the same reference identity. This is conditional fixed-discovery inference.

The additional caller declaration is a separable joint Gaussian brain error
law, conditional on those fixed targets and nuisance:

`E | T,Z ~ matrix-normal(0, known row shape, arbitrary voxel covariance)`.

Marginal Gaussian columns alone do not establish this joint law. The declaration
does not authenticate the physical source or prove a caller's scientific model.
Independent Gaussian rows use the identity row shape. Existing known temporal
or repeated-unit covariance capabilities supply the common whitening transform;
estimated uncalibrated covariance does not qualify.

## Actual numerical route and candidate strong-control argument

1. Include the original intercept direction in the nuisance design.
2. Apply the same known row-whitening transform W to nuisance, target scores and
   brain columns.
3. Obtain the provider Huh–Jhun complement Q of WZ, without recentering its new
   coordinates; retain `T* = Q'WT` and `X* = Q'WX`.
4. Sample distinct nonidentity permutations using the pinned Multivar action.
   Apply each permutation to **every** column of X* with the same row order.
5. Refit the complete fixed target design for each permuted brain response via
   Gale QR and compute its omnibus F. The numerator is the fitted-response norm
   squared; the denominator is full-model residual variance times target rank.
6. Stream each complete upper-tail field to its maximum and content digest.

For any true-null voxel subset J, `X*_J` has zero target mean and identity row
shape with arbitrary joint voxel covariance. Its joint distribution is invariant
under the common row permutation. A voxel's statistic uses only its own response
column and the fixed target geometry, so nonnull columns cannot enter true-null
statistics. This supplies a **candidate** null-subset invariance argument for
strong max-statistic FWER control. It is not the diagonal component-association
null: zero paired correlations do not imply joint cross-block independence.
Frozen M4.09 partial-null and rate qualification remains necessary.

## Complete execution, reuse and resources

The factory uses the existing M3.10 `LocalExecution` runtime. Each work unit is
one whole-family replicate. Its contribution retains a maximum, field digest,
action receipt, total/completed/failed member counts and at most the first
failure's index and error. Every observed member outcome remains available.
The null summary is not a full failure mask: the digest binds every member's
status/value, and exact masks require replay from the frozen prepared data and
action receipt. This is not a durable checkpoint. Complete
statistical inference requires every observed member and every member of the
original fixed B to succeed. Operational completion with failed voxels retains
usable local coefficient estimates and remains statistically incomplete.
Cancellation, omitted work and refused/conflicting commits cannot produce a
complete fixed-B receipt. Identical retries do not change B or count twice.

Actual action seeds derive through adopted `WorkAddress.seed`, including the
outer analysis plan and full frozen reference identity; receipts expose the
derived seed, candidate index and permutation digest. Traversal does not assign
new actions. Targets, nuisance geometry and known W are cached specifically
because this null conditions on them. Brain response fits are recomputed. This
is not an adapter for label-dependent split construction or full-selection
inference; those procedures require their own null-specific recomputation law.

The conservative simultaneous owned numeric-cell plan is admitted before any
brain acquisition. It includes prepared n-by-p data, p-by-batch selectors,
provider residual-basis geometry, working blocks, observed coefficients,
B-by-residual-row transformation indices, and O(B) maxima/failure summaries.
The reference budget/identity and cache receipt bind planned B; the prepared
receipt exposes the maximum B failure-summary entries. Constructor invariants
require completed+failed=total and a first failure exactly when failed>0.
It does not retain
a B-by-voxel statistic matrix. Borrowed inputs, collection/object overhead and
private Gale kernel scratch are explicitly outside this numeric-cell bound.
Brain acquisition is batched and reads each column once. A full-width capture
can declare `PatternReplay.SinglePass`; multiple capture batches require an
explicit nonblank `Repeatable` receipt bound to the actual frozen sources.
Incompatible SinglePass or blank Repeatable declarations refuse before target
or brain source access. Every null fit subsequently uses the owned prepared
matrix, with no further source applications. The replay declaration concerns
initial capture only; it is not a claim that the source is queried B times.

## Repaired threshold capability and conventions

The spatial consumer accepts only the core's sealed complete receipt and checks
the exact reference, ordered family, source identities, observed digest, mode,
B, action-seed binding and completed counts. It uses public
`MaxNullDistribution.fromOrientedMaxima`, `MaxNull.pValues` and `MaxNull.cutoff`;
it does not implement a private p-value or cutoff kernel. Omnibus F uses the
upper tail, inclusive `nullMaximum >= observedF` extremeness and Monte Carlo
`(1 + exceedances)/(B + 1)`. Exclusive cutoffs retain equality semantics from the
repaired threshold module, including explicit `NoRejections` when alpha is below
the attainable probability.

The actual source factory supports distinct nonidentity Monte Carlo only.
Separate `ExactGroupIncludingIdentity` requests are typed unavailable because
the current provider action does not supply complete enumeration. A tiny test
exhausts all 4!−1 nonidentity provider actions; plus-one restores the observed
identity and the exact denominator 24. Its independent scalar-regression
enumeration establishes numerical agreement with that finite group, not a new
production enumeration capability or mathematical tie certificate.

The scalar oracle uses the factory's exact declared augmented working nuisance
geometry and independently checks complement orthonormality and nuisance
orthogonality. Two equivalent nuisance spans can select different orthogonal
coordinate bases and therefore different finite permutation orbits. The first
focused JVM attempt passed 18/19 checks, with this oracle alone failing because
it originally omitted the redundant supplied intercept from its working
geometry. Only the test geometry was corrected; production policy and source
were unchanged. The initial failed log is retained alongside the retry evidence.

## Evidence and remaining boundaries

The four frozen source hashes are in [source-manifest.json](source-manifest.json).
The first pre-refinement JVM production compilation passed with zero warnings.
The corrected focused retry passed all 47 checks on each platform: 9 new core,
4 new spatial and 34 existing loading/coordinate/group controls. The subsequent
bounded-failure-metadata refinement adds an all-failed control and requires its
own focused JVM/JS re-gate. That final re-gate **passed 56/56 on each platform**,
including all 10 new core and 4 new spatial controls. That historical tested source's
hashes and raw-log hash are bound in
[focused-verified-gates.json](focused-verified-gates.json). Historical receipts
and failures remain in [focused-gates.json](focused-gates.json),
[early-observations.json](early-observations.json), and [logs](logs/).
Do not infer that an older passing receipt qualifies a newer source manifest.

The subsequent mandatory capture-replay refinement adds two controls (12 core
plus 4 spatial): both-source zero-read refusal and full SinglePass/batched
Repeatable equivalence over the tiny complete MC group, with zero additional
source reads during null fits. Its current source manifest was frozen for the
coordinator's full owning-module JVM/JS gates. Those final gates passed
**662 JVM and 661 JS checks**, with one explicitly declared unexecuted scientific
campaign skip on each platform and no errors or warnings. The included module
counts are:

| Module | JVM passed | JS passed |
| --- | ---: | ---: |
| MVPA, including 12 new family controls | 454 | 454 |
| MVPA spatial, including 4 new threshold controls | 24 | 24 |
| New subject-group bridge | 8 | 8 |
| Native group | 75 | 74 |
| Estimates | 11 | 11 |
| Threshold | 90 | 90 |

The 56/56 receipt qualifies its own historical source hashes rather than the
newer replay-contract sources; the final [integration receipt](integration-gates.json)
and its repository-relative central raw-log archive qualify the current
[source manifest](source-manifest.json). The declared
campaign skip is a visible qualification boundary, not a completed calibration.

After the original full gates, the coordinator approved a test-only opt-in
rank-suite timeout change. Its previous full receipt is preserved, and the
current integration receipt explicitly binds the changed test source to fresh
full MVPA JVM/JS runs (454 passed plus one campaign skip each). The other five
modules' sources are unchanged. The original full logs are not relabelled as
new executions, and these repeated checks are not added to the module totals.

Twelve core controls cover independent/known-dependent GLS parity, the tiny scalar
oracle, ordered retry equivalence, incomplete execution, retained local failures,
column locality, law/source/exposure refusal, resource caps, all-failed bounded
metadata, source-capture replay admission/equivalence and adopted action
seed binding. Four spatial controls cover the sealed complete adapter,
foreign-reference refusal, repaired tie boundaries and unattainable alpha.

This result is **candidate-only**. Released C1, FDR, arbitrary exact enumeration,
component-specific coefficient family correction and the original mixed 2r
association/predictive-value family are unavailable. The fixed-head Gaussian
prediction route has no qualified marginal p-value/CDF provider or justified
common action with component association; no family member is silently removed
to work around that gap. Native M4.08 also remains blocked on the open M4.07
frozen rank-calibration prerequisite. M4.09's frozen 10,000 null / 5,000 alternative
datasets and B=1,999 resampling admission have not been replaced by these fixtures.

The source/numerical review boundaries, including the separate read-only group
review, are recorded in [review-scope.md](review-scope.md). The implementation
author ran no build, committed no code, and made no tracker/publication changes;
the coordinator executed all JVM/JS gates.
