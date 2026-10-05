# Provenance v2 readiness receipt (2026-10-04)

Branch `review/c2-provenance-v2-20261004`, cut from main `3f261add` with the
net diff of preserved cluster C2 applied to it. Not landed; awaiting
independent review.

## Origin

- Mote `bd-01M3W1XYAHHM2RM8SB6HQCAQSY`, "KernelBasisProvenance.canonical
  interpolates Doubles (JVM/JS provenance drift)". It was filed by
  `phrf-claude-20260929` during review of identity fix `3d513f14` (Fray #153).
  A consumer-audit note from `backlog-worker-20260930` extended it to
  `ConditionProfileProvenance`.
- Implemented on 2026-10-02 by actor `provenance-bd01m3w1x-20261002`. That
  actor closed the bead after focused design, HRF and fit JVM/JS runs (logs
  `/private/tmp/bd-01M3W1XYAHHM2RM8SB6HQCAQSY-*.log`), but never committed the
  edits. They stayed dirty in the canonical checkout until they were snapshotted
  to `preserve/dirty-provenance-20261004`: `3230ffd2`, plus `df539b6b`, which
  adds the dirty `HrfKernelBasisSuite`. The snapshot's base is `53097f3f`.
- Triage cluster `C2-provenance-v2` in `dirty-triage.json`.

### Why a fresh branch instead of merging the snapshot

The snapshot commit also carried `build.sbt` hunks (an `atlas` upickle
dependency and a new `atlasWorkflows` module). Triage assigns those to cluster
C1 (atlas typed). Main has no `modules/atlas-workflows`, so those hunks don't
belong here. This branch therefore takes only the four `modules/` paths from
`git diff 53097f3f df539b6b`. The one conflict was the import line of
`ConditionProfileFit.scala`. Main had since added `EstimateExecutionOutcome`
to the `scalafim.fmri.fit` import, and the resolution keeps both that import
and `KernelBasisProvenance`.

### Not redundant

Main and origin still emit `kernel-basis/v1` and `condition-profile/v1`, with
display-rendered doubles. No other ref introduces either v2 string (checked
with `git log --all -S`). No verification receipt covers this change.

## Semantics

Before this change, both canonical strings interpolated `Double`s through
`s"..."`. The JVM renders `24.0` where Scala.js renders `24`, so the same basis
had different provenance on the two platforms. `ConditionProfileProvenance` also
embedded `DecodeBudget` through its case-class `toString`, and it joined
user-supplied names with unescaped `+`/`;`/`x` separators, which made the
encoding ambiguous.

The v2 encodings use the convention that `HrfIdentity` already established
(`hrf-descriptor/v2`):

- **Numbers** are written as `bits:<doubleToLongBits>`. Signed zero is kept,
  and NaN is canonicalized.
- **Strings** are length-framed (`<len>:<text>`), so separators inside a name
  can't forge structure.
- **Sequences** are tagged records, `tag(<framed>,<framed>,...)`.
- `kernel-basis/v2` frames the family name and encodes each chart axis as
  `axis(name, lower, upper)` inside `chart(...)`. Horizon, step and tolerance
  are IEEE bits, and nodes are `nodes(...)`. The rank budget `maxRank`, the
  `heldOutPoints` count, the achieved `rank` and the `seed` are written as
  decimal integers. They are `Int`/`Long`, whose rendering is the same on both
  platforms, so `bits:` does not apply.
- `condition-profile/v2` is a typed record. `ConditionProfileProvenance` now
  holds the `ResponsePreparationProvenance`, `Option[ShapePrior]` and
  `OutputRequest` values themselves, not their `toString`, and `canonical`
  encodes them structurally, in this order:
  - the basis canonical, length-framed;
  - `structure=conditions(condition(...),...)`;
  - `preparation=`, a length-framed `response-preparation/v1|...` record;
  - `nodesPerAxis=nodes(...)`;
  - `budget=`, a length-framed `decode-budget/v1|...` record that names every
    field;
  - `prior=none` or `prior=some(shape_prior(mean=values(...),precision=values(...)))`;
  - `output=condition_queries(<rule>,queries(query(label=<label>,weights=values(...),absoluteTolerance=bits:...),...))`,
    or `condition_amplitudes(<rule>)`;
  - `noiseVariance=bits:...`.
- `response-preparation/v1` is new, in `fit/ResponsePreparationIdentity.scala`.
  It encodes every `ResponsePreparationRecord` as
  `record(step=<step>, disposition=<disposition>(<detail>))`, with an explicit label for each
  enum case and every double as IEEE bits. It covers:
  - the missing-data policy;
  - censored timepoints;
  - volume weighting: disabled, a DVARS estimator (function parameters
    `threshold`/`steepness`, plus scope), or fixed weights with their
    alignment;
  - nuisance projection: disabled, or `matrix_projection`, recording the matrix
    as dimensions plus a content digest and the regularization as `auto`, `gcv`
    or `fixed(lambda)`;
  - every `ArOptions` field, including `rho`/`phi`;
  - every `RobustOptions` field, including Huber `k` and Bisquare `c`;
  - the optional executed `VolumeWeightingReceipt`: source, normalization, all
    timepoint sets, weights, quality metric and run partitions.
- **Matrices** (`KernelBasisProvenance.matrix`) are encoded as
  `matrix(rows, cols, fnv1a64:<16 hex>)`. The digest is 64-bit FNV-1a over the
  little-endian bytes of each row-major entry's `doubleToLongBits`, computed in
  shared code with no `MessageDigest`, so it is identical on the JVM and
  Scala.js. It replaces the previous lossy `NuisanceMatrix@<hash>`/`DMat`
  `toString` rendering. It is an identity digest, not a security hash: two
  distinct matrices of equal dimensions collide only with 64-bit-hash
  probability.

### Completeness of the condition-profile identity

`ConditionProfileProvenance.encodedPolicyFields` lists the
`ConditionProfilePolicy` fields that are recorded: `basis`, `structure`,
`nodesPerAxis`, `budget`, `prior`, `noiseVariance` and `output`.
`unencodedPolicyFields` lists the deliberate omissions and the reason for each:

- `admission` only decides whether preparation is admitted. It cannot change an
  accepted result.
- `blockSize` only controls batching. Each voxel's post-solve is independent of
  the block it is read in.

A shared test reads the policy's field names from its compile-time `Mirror` and
requires the encoded and unencoded lists to partition them exactly. A new
policy field therefore fails the build until it is classified.

Inside the encoders, three mechanisms cover every type:

- **New enum cases** are caught at compile time by exhaustive matches.
- **New case-class fields** are caught at compile time because every encoded
  case class is destructured positionally (`case DvarsWeightEstimator(function,
  scope) =>`), so a pattern of the wrong arity does not compile. This covers
  `ConditionProfileProvenance`, `DecodeBudget`, `ShapePrior`, `SignedQuery`,
  `ResponsePreparationProvenance`, `ResponsePreparationRecord`, `ArOptions`,
  `RobustOptions`, `DvarsWeightEstimator`, `VolumeWeightingReceipt` and
  `VolumeWeightPartitionReceipt`.
- **Field names** are checked by a scoped `Mirror` test. Each of those types is
  encoded alone, through its own encoder, and every field name must appear in
  that record. A field dropped from one record therefore cannot be masked by a
  same-named field elsewhere, such as the receipt's `weights` versus a query's
  `weights`.

`ProfileTrialSignedQuery` is a plain class with no extractor, so it is still
read through accessors. It belongs to the unreachable trial-output branch
described below.

The human-readable disposition detail strings, such as "censoring is consumed
by AR/GLS preparation", are part of the identity. Rewording one of them splits
identities.

Two residual gaps are deliberately left, and both are recorded on
`bd-01M44E76ZC0J12GACBSGXAV6GA`:

- `OutputRequest.TrialQueries` encodes each query in full, but its
  `ProfileTrialAxis` only by trial and condition counts. The axis is bound by
  reference identity (`sameBinding` uses `eq`). This branch is unreachable,
  because `ConditionProfileFit.prepare` refuses trial outputs before any
  provenance exists.
- `ParametricHrfFamily` is unsealed, and `kernel-basis/v2` identifies a family
  by name, chart and horizon. A custom family that reused a shipped name with a
  different `evalInto` would collide. The shipped families are fully
  identified by those fields.

The helpers are `KernelBasisProvenance.{number, field, record, option, matrix}`, scoped
`private[scalafim]`. They duplicate `HrfIdentity`'s private helpers because that
object is `private[hrf]`; a shared bits helper remains a possible later
consolidation, as the bead suggests. The change is limited to encoding: no
arithmetic, fit or decoder behavior changes, and no R parity question arises
because these strings are ScalaFIM-native identities.

## Compatibility and identity impact

- Both schema strings bump from v1 to v2, so every `HrfKernelBasis.provenance`
  and every `ConditionProfileProvenance.canonical` value changes on both
  platforms.
- These strings are consumed only in process.
  `ObservedFamilyAdmission.admits` compares a certificate's recorded basis
  provenance with `basis.provenance.canonical`, and both sides are recomputed in
  the same process. `ObservedFamilyAdmission.geometry` and
  `ProfileHrfFit`'s `profile-fit/v2` string embed the basis canonical as a
  substring. No decoder or persisted artifact in this repository parses either
  string, and no test, fixture, doc or tool references the v1 literals. The same
  holds for downstream checkouts (`eidolon`, `PLSNeuro`). Any externally
  persisted v1 provenance will simply fail to match, which is the intended
  refusal of old opaque values.
- `KernelBasisProvenance` gains two fields (`maxRank`, `heldOutPoints`) and
  `ConditionProfileProvenance`'s `preparation` and `output` change type from
  `String` to `ResponsePreparationProvenance` and `OutputRequest`, with a new
  `prior: Option[ShapePrior]` field. Both case classes are public, so this is
  source-incompatible for anyone constructing them directly; no caller outside
  their producers exists in this repository or the downstream checkouts, and
  neither type has landed in its v2 form. Everything else added is
  `private[...]`.
- `firstLevelLaws` and the design identity goldens (`DesignIdentitySuite`,
  `DesignSchemaSuite`) were re-run and are unaffected. The design fingerprint
  does not embed kernel-basis provenance.

## Completed in this pass

### Round 3: confirmation review of `68f60213` (APPROVE-WITH-NITS)

- N1: the nested `Mirror` guards are now scoped. Each type is encoded alone
  through its own encoder (the encoders became `private[fit]`/`private[profile]`
  for this), rather than searched for in the full identity string.
- N2: all encoded case classes are destructured positionally, so adding a field
  fails compilation, and the five unguarded types are now in the scoped test.
  The five are `ResponsePreparationProvenance`, `ResponsePreparationRecord`,
  `DvarsWeightEstimator`, `VolumeWeightPartitionReceipt` and `SignedQuery`.
  For the names to be checkable, sub-records now carry field labels (`step=`,
  `disposition=`, `function=`, `scope=`, `threshold=`, `steepness=`,
  `weights=`, `alignment=`, `runIndex=`, `timepoints=`, `label=`).
- **Golden changed.** These labels changed the golden. A diff that ignores
  length prefixes shows that only labels were added, and every IEEE value and
  the digest are unchanged. v2 is unlanded, so there is no version bump.
- N3: the disposition-detail note above.

### Round 2: review `7099f1f9` (CHANGES-REQUIRED on condition-profile)

- F1: `preparation` and `output` were only length-framed `toString`s
  (`1.0E-6` on JVM versus `1e-6` on JS; `NuisanceMatrix@<hash>`). Both are now
  typed and structurally encoded as described above, with the new matrix
  digest. Golden: `ConditionProfileProvenanceSuite` "condition profile
  provenance from the real producer has a platform-independent golden" builds
  the provenance through the producer `ConditionProfileProvenance.of(policy,
  preparation)` (the function `ConditionProfilePreparation.provenance` calls)
  from a real compiled `HrfKernelBasis`, `TaskBasisStructure`,
  `SignedQuery.make` queries, a real `ResponsePreparationPlan(...).records`
  with fixed weights, a 2x2 nuisance matrix, `Regularization.Fixed(0.1)`,
  AR(1) `rho = -0.1`, Huber `1.345`, and an executed DVARS
  `VolumeWeightingReceipt`, using `1e-6`, `-1.0`, `0.1`, `-0.0` and `0.5`
  throughout. It asserts one literal (after the separately goldened basis
  canonical) on both JVM and JS. The IEEE bit values in the literal and the
  FNV-1a digest were cross-checked independently in Python (`struct.pack('<d')`).
  `ConditionProfileFitSuite` (first-level-laws) additionally checks that a
  real `ConditionProfileFit.prepare` carries the policy's typed `output` and
  `prior` and the retention plan's preparation, and contains no `@`.
- F2: `prior` is encoded as `none` or `some(shape_prior(...))`. A test checks
  that both its presence and a change to one precision entry (`1e-6` to `2e-6`)
  change the identity. The policy-field partition guard described above was
  added in the same round.
- F3: `kernel-basis/v2` records `maxRank` and `heldOutPoints`; its golden is
  updated (v2 is unlanded, so no second bump).
- F4: the unsealed-family caveat is documented above and on the follow-up
  bead.

### Round 1

- Rebased the cluster onto current main and resolved the import conflict.
- Dropped the C1 atlas `build.sbt` hunks.
- Added the shared test `decode budget canonical encodes every DecodeBudget
  field` to `ConditionProfileProvenanceSuite`. It checks that every
  `DecodeBudget.productElementNames` entry appears as `|name=` in
  `budgetCanonical`. A field added to `DecodeBudget` therefore fails the build
  until the explicit encoding, and its version, is updated. Without the test,
  the hand-written record could silently drop a new field.

## Follow-ups (out of this bead's scope; not changed)

The following identities still interpolate doubles through display rendering
and so remain platform-dependent:

- `ObservedFamilyAdmission.geometry`, which contains onsets, durations, TR,
  expanded values, `precision`, whitening coefficients and nuisance values.
- `ProfileHrfFit`'s `profile-fit/v2` string, which contains
  `decode=${policy.budget}` (case-class `toString`), `lambda`, `grid` and
  `config`.

Both are in-process admission and fingerprint strings, so they are consistent
within one platform. Making them portable would be a separate identity bump; it
is filed as `bd-01M44E76ZC0J12GACBSGXAV6GA`, which also records the residuals
above and the reusable encoders this branch adds.

## Gates

All runs used `tools/build/sbt-warm` from this worktree, one batch at a time.
`memory_pressure` showed at least 42% free before every batch, and the server
was shut down between batches. Every project compile was warning-clean. The
only `warn` lines were sbt's "multiple main classes detected" notice and, in
round 1, a meta-build warning in the staged `linops4s/build.sbt` dependency.

The modules touched are `design` and `fit` (plus a test in `first-level-laws`
in round 2). Their transitive dependents, from `build.sbt`, are `model`,
`fitEstimates`, `firstLevelLaws`, `group`, `fmriWorkflow`, `mvpaFit`,
`mvpaDataset`, `mvpaSpatial` and `datasetZarr`. `atlas` is not in the set, so
`MniTemplateBridgeFilesSuite` does not apply.

### Round 3 (guard-strength nits)

Memory was at least 39% free before each batch, and the server was shut down
between batches.

| Target | JVM passed | JS passed |
|---|---|---|
| `ConditionProfileProvenanceSuite` (focused) | (in fit) | 4 |
| design | 447 | 446 |
| fit | 574 | 519 |
| firstLevelLaws | 84 | 84 |

All runs had 0 failures. The scoped-guard test and the refreshed golden passed
on both platforms. `scalafimCompileAll` succeeded (456 s) and was
warning-clean. Round 3 changes only encoder internals, test scoping and the
golden. No signature visible to the other dependents changed, so their round-2
results stand.

### Round 2 (`68f60213`)

| Target | JVM passed | JS passed |
|---|---|---|
| design | 447 | 446 |
| fit | 574 | 519 |
| model | 53 | 53 |
| fitEstimates | 16 | 12 |
| firstLevelLaws | 84 | 84 |
| group | 75 | 74 |
| fmriWorkflow | 23 | 19 |
| mvpaFit | 45 | 45 |
| mvpaDataset | 108 | 108 |
| mvpaSpatial | 20 | 20 |
| datasetZarr | 15 | 7 |
| **total** | **1460** | **1387** |

All runs had 0 failures and 0 errors. JS counts are lower where suites live in
`jvm/` test directories. Compared with round 1, the extra tests are:

- design: +1, the matrix-digest golden;
- fit: +2, from four `ConditionProfileProvenanceSuite` tests replacing two;
- firstLevelLaws: +1, the prepared-provenance wiring test.

Every new or changed provenance test passed on both platforms. The JS runs were
also confirmed with explicit `fitJS/testOnly ...ConditionProfileProvenanceSuite`
(4/4) and `designJS/testOnly ...HrfKernelBasisSuite` (7/7). The
`scalafimCompileAll` (success, 485 s) and `examplesCompile` (success, 46 s)
runs were both warning-clean. The identity strings changed, so
`firstLevelLawsJVM` and `firstLevelLawsJS` were run (84/84 each), together with
the design identity goldens in `designJVM/JS`.

### Round 1 (`7099f1f9`)

The same targets passed with JVM 1456 and JS 1383, 0 failures. Both
`scalafimCompileAll` and `examplesCompile` passed.
