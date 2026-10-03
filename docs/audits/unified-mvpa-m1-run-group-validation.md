# Unified MVPA M1.E1 identified run-group validation evidence

Historical packet evidence. Legacy API references below describe the recorded
base revision, not the current public surface. See the
[native MVPA documentation](../../modules/mvpa/README.md) for current callers.

Recorded: 2026-09-14

Packet: M1.E1 (`bd-01M0Z5MJ46C70JQ9NNN4GPMJQQ`)

Base revision: `77360cf2d41f7c38bc7b400fb04a623be9bf73ee`

## Result

Predictive validation can now bind an exact `Column[S, G]` of run or sample
groups to the same identified sample space as its predictors and targets.
`LeaveOneGroupOutDesign` compiles that evidence through the admitted
Resample4s `FixedPartitions.once` provider and the M1.02 `ValidationDesign`;
it does not add a second fold engine or assign new semantics to legacy
`FoldPlan`.

The bound design retains:

- the exact grouping-column `ColumnIdentity`;
- complete sample-stable-key to group-stable-key membership;
- a deterministic SHA-256 membership signature;
- stable group keys distinct from display labels and provider ordinals;
- the exact-once Resample4s plan receipt; and
- identified analysis and assessment `ReindexingLeg` values for every unit.

Provider membership is checked against the complete grouping evidence when a
unit is opened. Drift fails through `GroupingError.SplitMembershipMismatch`,
whose split role and first differing position are typed data. Invalid stable
keys, fewer than two groups, and unknown units also remain typed failures.

## Scientific and type laws

1. Every sample appears in exactly one assessment leg and each assessment leg
   contains exactly one stable group. Analysis and assessment root sample keys
   are disjoint and their union is the complete input axis.
2. Target columns restrict through the identified legs and agree with an
   independent direct-ordinal oracle. No target value participates in design
   construction.
3. Renaming display labels while retaining stable group keys preserves the
   group-keyed schedule and membership signature. The exact source
   `ColumnIdentity` and whole validation receipt still change, so the supplied
   evidence is not erased from provenance.
4. Reordering rows requires a matching identified axis and grouping column.
   A same-size foreign grouping column is rejected at compile time; a forged
   runtime record is rejected at construction. With the matching reordered
   evidence, group-keyed assessment membership remains equivalent.
5. The new schedule is numerically identical, by held-out stable group, to the
   independent legacy `FoldPlan.leaveOneBlockOut` oracle, including block IDs
   whose numeric order differs from first occurrence.
6. The public API contains no casts, erased row spaces, hidden seed derivation,
   broad implicit conversions, or duplicate resampling implementation.

## Verification

Executed in the isolated implementation clone with Scala 3.7.4, sbt 1.11.7,
Java 25.0.1, and the installed Scala.js Node runner:

```text
sbt -Dsbt.supershell=false \
  "mvpaJVM/testOnly scalafim.fmri.mvpa.GroupedValidationSuite"
  5 passed, 0 failed, 0 errors

sbt -Dsbt.supershell=false mvpaJVM/test
  148 passed, 0 failed, 0 errors

sbt -Dsbt.supershell=false mvpaJS/test
  148 passed, 0 failed, 0 errors

sbt -Dsbt.supershell=false scalafimCompileAll
  passed, warning-clean
```

The first full JVM run after implementation passed 146 tests and exposed two
test-side assumptions: the check compared reindexed child keys rather than
their root parent keys, and one compile-negative assertion depended on the
compiler's rendering of a generic type. The court was corrected to assert root
sample separation and the stable public-boundary identifier `foreignGroups`.
The implementation contract was unchanged, and the focused plus full JVM and
Scala.js reruns above are green.

The three Scala sources were formatted through targeted `scalafmtOnly` before
the recorded gates.

## Scope boundary

This packet binds validation scheduling to identified sample and grouping
evidence. It does not migrate classifiers or other legacy analyses onto the new
design, claim a target-free fitted-estimator lifecycle, add permutation or
bootstrap inference, or claim release publication. No push or artifact
publication was performed.
