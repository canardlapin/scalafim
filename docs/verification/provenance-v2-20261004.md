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
  are encoded as IEEE bits, and nodes are written as `nodes(...)`.
- `condition-profile/v2` frames basis, preparation and output, and nests the
  structure as `conditions(condition(...),...)`. `sigma2` is IEEE bits, and the
  budget is an explicit `decode-budget/v1|...` record that names every field.

The helpers are `KernelBasisProvenance.{number, field, record}`, scoped
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
- The public API only grows: the `KernelBasisProvenance` and
  `ConditionProfileProvenance` companions gain `private[...]` helpers. No
  signature is removed.
- `firstLevelLaws` and the design identity goldens (`DesignIdentitySuite`,
  `DesignSchemaSuite`) were re-run and are unaffected. The design fingerprint
  does not embed kernel-basis provenance.

## Completed in this pass

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
within one platform. Making them portable would be a separate identity bump and
should be filed as its own bead.

## Gates

All runs used `tools/build/sbt-warm` from this worktree, one batch at a time,
with `memory_pressure` at 42% free or more before each batch and a server
shutdown between batches. Every project compile was warning-clean. The only
`warn` lines were sbt's "multiple main classes detected" notice and a
meta-build warning in the staged `linops4s/build.sbt` dependency.

The modules touched are `design` and `fit`. Their transitive dependents, from
`build.sbt`, are `model`, `fitEstimates`, `firstLevelLaws`, `group`,
`fmriWorkflow`, `mvpaFit`, `mvpaDataset`, `mvpaSpatial` and `datasetZarr`.
`atlas` is not in the set, so `MniTemplateBridgeFilesSuite` does not apply.

| Batch | Target | JVM passed | JS passed |
|---|---|---|---|
| 1 | design | 446 | 445 |
| 1 | fit | 572 | 517 |
| 2/3 | model | 53 | 53 |
| 2/3 | fitEstimates | 16 | 12 |
| 2/3 | firstLevelLaws | 83 | 83 |
| 2/3 | group | 75 | 74 |
| 2/4 | fmriWorkflow | 23 | 19 |
| 2/4 | mvpaFit | 45 | 45 |
| 2/4 | mvpaDataset | 108 | 108 |
| 2/4 | mvpaSpatial | 20 | 20 |
| 2/4 | datasetZarr | 15 | 7 |
| **total** | | **1456** | **1383** |

All runs had 0 failures and 0 errors. JS counts are lower where suites live in
`jvm/` test directories. The new or changed provenance tests passed on both
platforms:

- `HrfKernelBasisSuite`: "kernel basis provenance has a platform-independent
  IEEE and string-framed golden"
- `ConditionProfileProvenanceSuite`: "decode budget canonical encodes every
  DecodeBudget field" and "condition profile provenance has a
  platform-independent IEEE and string-framed golden"

Batch 5 ran `scalafimCompileAll` (success, 537 s) and `examplesCompile`
(success, 62 s), both warning-clean.

The schema strings changed, so `firstLevelLawsJVM` and `firstLevelLawsJS` were
run (83/83 each), together with the design identity goldens in `designJVM/JS`.
