# Threshold: `PSide` and `StatKind.T.df` removed; `hier_descend` edge divergences analysed (2026-09-30)

Mote: `bd-01M3RNPCE1X9GDK98DNZVZ1FPQ`. This packet covers two sub-items of the
successor ticket:

- "Admit or remove the unused `PSide` and `StatKind.T.df`": **removed**.
- "Edge divergences from `hier_descend`": a written analysis with a
  recommendation for each divergence. No code changed for this sub-item.

Base: `9504d696`. Branch `threshold/pside-df-20260930`.

## 1. Decision: remove `PSide` and `StatKind.T.df`

### Every use, found before deciding

At `9504d696`, `git grep` across the repository (`*.scala`, `*.md`, `*.sbt`)
and a search of the sibling checkouts under `~/code/scala` found:

| Symbol | Uses |
| --- | --- |
| `PSide` | Declared in `Settings.scala`. It was a payload of `StatKind.NegLog10P` and a parameter of `StatisticMap.negLog10P`. Four test call sites passed `PSide.OneSided` (`ThresholdCoreSuite` ×2, `DecisionScaleSuite`, `HierScanSuite`). **No code ever read the value.** `StatKind.orientation` matched `NegLog10P(_)`. |
| `StatKind.T.df` | A payload of `StatKind.T` and a parameter of `StatisticMap.t`. **No caller, test or example constructed a `T` map, and no code read the df.** `orientation` matched `T(_)`. |
| `DegreesOfFreedom` (threshold's opaque type) and `ThresholdError.InvalidDegreesOfFreedom` | These existed only to carry `T.df`. The only use was one constructor test in `ThresholdCoreSuite`. The `DegreesOfFreedom` types in `fit`, `group` and `estimates` are separate types and are unaffected. |
| Other modules and examples | None. No module in `build.sbt` depends on `threshold`. `~/code/scala/scalafim-spike-raster` is an older copy of this module, not a consumer. |
| Docs | `docs/plans/neurothresh.md` (lines 82 and 171) describes a planned `CanonicalStat` for t(df)→Z and p→Z. It was never implemented. That plan text is now stale on this point; it is not claimed by this packet and is left unedited (a pointer to this receipt would be a follow-up). The packet 1–4 receipts list the item as open. |

### Why removal, not admission

Every admitted procedure is a permutation procedure: `MaxT`,
`WestfallYoung` and `HierScan`. Each judges the observed map only against
null draws of the same statistic, oriented by one `ThresholdAlternative`.
Neither payload can affect a result.

- **df cannot matter to maxT or Westfall-Young.** Their adjusted p-values
  depend on the observed and null values only through the order of the
  oriented values. A t→z conversion `g` with one df shared by all voxels is
  strictly increasing and odd. Under Greater and Less, `g` preserves order.
  Under TwoSided, `|g(x)| = g(|x|)`, so `g` preserves order there too. A df
  would therefore be dead data, and a type that carries dead data implies a
  guarantee it cannot keep. Three conditions bound this claim. It covers
  adjusted p-values and rejections, not the cutoff, which is on the value
  scale and moves with `g`. It requires the *same* `g` applied to the map and
  to every null draw. And it holds in exact arithmetic: a floating-point
  `pt`/`qnorm` saturates at large |t| and can create ties that change
  p-values unless it is computed in log space. A per-voxel df vector is not
  a common transform and is not covered.
- **df would matter to HierScan only through a conversion that does not
  exist.** HierScan's set scores sum evidence non-linearly, so a t→z
  conversion would change its results. Admitting df would mean implementing
  that conversion: a Student-t CDF and an inverse normal CDF in `shared`, with
  R parity fixtures, a decision on whether HierScan scores t or z-equivalent
  evidence, and a new calibration. No consumer asks for this. UMVPA-M4.08,
  the expected first consumer, builds on the admitted methods as they stand.
- **Sidedness is already carried by other state.** For a `NegLog10P` map,
  one- versus two-sided is part of the hypothesis. It lives in two places
  that the procedures do use: the alternative, which must be Greater for
  unsigned evidence, and the null draws, which must be computed with the same
  sidedness. A label that nothing checks against the draws gives false
  assurance. Checking it would need provenance on the null family that the
  module does not have. That is the "consumer-level null-family identity"
  already listed as remaining scope.

The smaller correct change is to remove the payloads. `StatKind` stays as a
closed label, `Z | T | NegLog10P`, because its orientation is used: it
decides the admissible alternatives and whether negative evidence is
refused. `T` stays as an honest provenance label. It is recorded as
`statKind` in the `params` of both maxT and HierScan results. HierScan
recording it matters most, because HierScan is the procedure whose scores
t versus z would change. Without the label a t map would have to be
mislabelled `Z`.

### Change

- `Settings.scala`: `StatKind` is `case Z, T, NegLog10P`, with a scaladoc
  stating the contract above. `PSide` and threshold's `DegreesOfFreedom` are
  removed.
- `StatisticMap.scala`: `StatisticMap.t(volume)` and
  `StatisticMap.negLog10P(volume)`.
- `ThresholdError.scala`: `InvalidDegreesOfFreedom` is removed.
- Tests: the four existing `PSide.OneSided` call sites and the
  `DegreesOfFreedom` constructor assertion are updated. `StatKindSuite` is
  new.
- `modules/threshold/README.md`: a paragraph on what a `StatKind` label means
  and does not mean.

The API change breaks source compatibility. It is safe because nothing
outside the module used either payload.

### Tests grounded in the theory (`StatKindSuite`, shared, JVM and JS)

1. **Orientation table.** The test is exhaustive over `StatKind.values` ×
   `ThresholdAlternative.values`. `Z` and `T` are signed and admit all three
   alternatives. `NegLog10P` is unsigned and admits only Greater; any other
   alternative returns `IncompatibleAlternative`.
2. **Signedness.** A t map accepts negative evidence under Less. An unsigned
   map refuses it with `NegativeUnsignedEvidence(3, -2.5)`.
3. **The label has no numeric effect.** The same values are run as a `T` map
   and as a `Z` map, under every alternative. For maxT, p-value maps, reject
   masks and cutoffs are identical. `params` differ only in `statKind`. For
   HierScan, hit paths, adjusted p-values, scores and node decisions are
   identical. Non-vacuity is asserted: both procedures reject something.
4. **Invariance theorem.** With `g(x) = x + x³`, which is odd with
   `g' = 1 + 3x² > 0`, maxT and Westfall-Young adjusted p-values and
   rejections are exactly equal on the raw and transformed observed/null
   pairs. This holds for 2 policies × 3 alternatives × 2 null references,
   including exact enumeration with the identity row. The inputs are quarter
   integers with `|x| ≤ 4`, so `g` is computed exactly and ties are
   preserved bit for bit.
5. **Negative control.** A non-monotone transform (`x²` on signed evidence)
   does change the adjusted p-values, so test 4 has discriminating power.
6. **Sidedness is carried by the alternative and the draws.** Take
   unsigned evidence `u(|t|)` for two-sided or `u(t + 4)` for one-sided,
   with `u(a) = a + a³`. Threshold it as a `NegLog10P` map with Greater
   against identically transformed draws. It reproduces the t map's maxT
   p-values and rejections under TwoSided and Greater respectively. Rejection
   is non-vacuous in both cases.

### Evidence

All builds went through the shared serialized runner,
`/private/tmp/scalafim-execution-20260929/run-sbt.py`. Logs are in
`/private/tmp/scalafim-execution-20260929/logs/`.

| Run | Result | Log SHA-256 |
| --- | --- | --- |
| `threshold-pside-df-first-20260930.log`: `thresholdJVM/test thresholdJS/test` | exit 1; JVM 88/89. Test 6 was vacuous because the null spread matched the signal, so the non-vacuity guard fired. The null draws were narrowed to `[-2, 2]`. No assertion was weakened. | `d3321ab8b70c765655c3f571b42affdb937313d8f2088044f0f287fa74055218` |
| `threshold-pside-df-second-20260930.log`: same | exit 0; JVM 89/89, JS 89/89 | `2cf7f10734553dc5a3907388639b22e0d152ada06cf0142f70dfa9a708bdb854` |
| **Mutation 1** `threshold-pside-df-mutation1-20260930.log`: `StatKind.orientation` groups `T` with `NegLog10P` as Unsigned | exit 1; JVM 85/89. Four `StatKindSuite` tests fail: orientation table, signedness, t/z identity, sidedness. | `12159744d3c7db74d131010b30a145ca646968c63115ae786e2f265329670fba` |
| **Mutation 2** `threshold-pside-df-mutation2-20260930.log`: `StatisticMap.t` labels the map `Z` | exit 1; JVM 88/89. The t/z identity test fails on `params.statKind`. | `4916b71bfd2809006284a0e9cd4af36c16d9844f5ccf6f4237585bf0cdcd4c32` |
| **Final gate** `threshold-pside-df-final-20260930.log`: `thresholdJVM/test thresholdJS/test scalafimCompileAll` | exit 0; JVM 89/89, JS 89/89, all compiles succeed, 0 `[warn]` lines | `d9eeb3dfb29262701639fdc2caafec1d2f328c5e205f8c2e2fc38e52dfc0cc14` |

The threshold suite had 83 tests before this packet, and `StatKindSuite`
adds 6. After each mutation, the file was restored from a scratch copy and
its SHA-256 re-checked against the pre-mutation hash:

- `Settings.scala`: `602e62522c9237688dca7dad030e974045012ed39a8f08e6a48ba8d2c5968b18`
- `StatisticMap.scala`: `f622f726caeaa3b38ff74a2094479c9f31ac552a089a5abd3133e5eac2b0553e`

`modules/threshold/README.md` was edited after the final gate. It is prose
only and is not compiled.

### Review revision (after independent review of `50ac01f5`: ACCEPT-WITH-FIXES)

- **Required fix.** `HierScan` results now record `statKind` in `params`, as
  maxT results already did (`HierScan.params`). HierScan is the procedure
  whose scores t versus z would change, so erasing the label there was the
  one place where it mattered. `StatKindSuite`'s t/z test now compares HierScan
  `params` with `statKind` removed, and asserts separately that the entries
  are `T` and `Z`. A new test, "every procedure records the stat kind of the
  map it was run on", checks both procedures over all three kinds. The suite
  now has 7 tests, and the module has 90.
- **Doc fixes.** Four `PSide` call sites, not three. The zero-mass failure
  is `InvalidArgument("observed child score", "scoring set has zero prior
  mass")`. The invariance now states its scope: p-values and rejections
  only, the same transform on the map and its draws, one shared df, and
  exact arithmetic. The README says the same. `docs/plans/neurothresh.md` is
  recorded as a stale plan reference.

| Run | Result | Log SHA-256 |
| --- | --- | --- |
| `threshold-pside-df-r2-gate-20260930.log`: `thresholdJVM/test thresholdJS/test scalafimCompileAll` | exit 0; JVM 90/90, JS 90/90, all compiles succeed, 0 `[warn]` lines | `a670b2e2c2f6e6a86c1d0ace5de25c0f83f48dae1ccbf081c6b10b9e7496aa1d` |
| **Mutation 3** `threshold-pside-df-r2-mutation3-20260930.log`: HierScan `params` drops the `statKind` key | exit 1; JVM 88/90. The t/z test and the new recording test fail. | `4368af622ce598d4eac2739ea22b642e300b052f81b1006cf3f90d42a3748751` |

After mutation 3, `HierScan.scala` was restored from a scratch copy, and its
SHA-256 matches the pre-mutation hash
`b1c250ea88ed46e8f70da180acdf63ead161edc4b68afa3611bdb1a6ab83119f`. This
receipt was edited after the gate; it is prose only.

### Not claimed

- No t→z, p→z or df-aware conversion exists or is claimed. HierScan on a t
  map scores t values as given. Its calibration was run on z-scale
  sign-flip statistics, and its behaviour on heavy-tailed small-df t maps is
  not separately calibrated. The invariance in test 4 is proven for maxT and
  Westfall-Young only.
- Nothing checks that null draws share the observed map's statistic or
  sidedness. That remains the caller's contract. It is part of the
  null-family identity work already on the ticket.
- `QValue` and `EvidenceScore`, which the earlier receipts list alongside
  this item, are not in this packet's scope and are not present at
  `9504d696`.

## 2. Analysis: edge divergences from `neurothresh::hier_descend`

Sources: `~/code/neurothresh/R/hierarchical.R` (`hier_descend`) and
`src/hier_scan_rcpp.cpp` (`octree_split_info_cpp`, `min_pi_mass = 1e-10`),
compared with `Octree.split` and `HierScan.descend` at `9504d696`.

Two facts frame all three divergences:

- Region bounding boxes are tight (`BoundingBox.fromIndices`), and the
  midpoint is `(lo + hi) / 2`. Whenever `lo < hi` on an axis, the minimum
  and maximum voxels fall on opposite sides, so a region of two or more
  voxels always has at least two non-empty octants before the mass filter.
- Complete-null FWER ≤ γα, proved in packet 4, uses only the root family:
  descent happens only after a rejection. None of the three divergences
  touches the root family's level, so none can affect the complete-null
  bound.

### 2a. Child-mass inclusion: Scala keeps `mass > minPriorMass`, R keeps `>= min_pi_mass`

- **When it differs.** Only for a non-empty octant whose prior mass equals
  the threshold exactly. At the shared default of 1e-10, this does not occur
  for realistic priors. At `minPriorMass = 0`, the setting several existing
  tests use, it differs for every non-empty octant with zero prior mass.
  Such octants arise when a user prior has zeros and `priorEta = 1`, the
  default, which keeps the raw prior.
- **Which is right.** A zero-mass region has no prior-weighted score:
  `log Σπ = −∞` and `1/√Σπ² = ∞`. Scala's scorer returns
  `NotScored(ZeroPriorMass)`. `HierScan` would then abort through
  `finiteOrError` with
  `InvalidArgument("observed child score", "scoring set has zero prior mass")`
  (`ScoreSet.scala:26-30`, `HierScan.scala:176`). R would feed non-finite scores into `wy_stepdown`.
  Its descendant budget share would be 0 in both implementations anyway.
  Strict `>` excludes exactly the regions that cannot be scored, so it is
  the sound choice.
- **Recommendation.** Keep `>` as a documented, deliberate divergence. Add
  one scaladoc line on `HierScanConfig.minPriorMass` and `Octree.split`.
  Do not change the R-parity fixtures, which never reach the boundary.
- **Tests that would pin it**, in `ThresholdCoreSuite` or an Octree suite:
  1. On a uniform 2×2×2 field with dyadic mass 1/8 per octant,
     `Octree.split(root, minPriorMass = 0.125)` returns no children. This
     pins strictness, since `>=` would return 8.
  2. With a prior that is zero on one octant and `minPriorMass = 0`, that
     octant is absent from the split.
  3. With the same prior, `HierScan.runMap` completes rather than returning
     `InvalidArgument("observed child score", …)`. This is the end-to-end consequence.

### 2b. Singleton-bbox regions: Scala returns no children, R returns one child equal to the parent

- **When it differs.** By the tightness fact above, a singleton bbox is
  exactly a one-voxel region. `HierScan` reaches it only if
  `minVoxels <= 1` and a one-voxel child was rejected.
- **What R does.** R splits the voxel into one child identical to itself and
  calls `wy_stepdown` with one column. In neurothresh 0.1.0 this crashes
  (reproducer in `threshold-hierscan-alignment-20260930.md`). If the crash
  were fixed, R would retest the same voxel at `γ(1 − γ)·budget`, record a
  duplicate hit at a deeper path, and recurse until `min_alpha`. The retest
  adds no information, spends alpha on a region already decided, and
  duplicates the hit.
- **Recommendation.** Keep Scala's rule: a singleton region is a leaf. Treat
  R's behaviour as a reference defect, and report it upstream together with
  the `wy_stepdown` one-column crash. When R-parity fixtures are regenerated
  after an upstream fix, parity should be defined as "R minus duplicate
  self-children". The fixture should not adopt the duplicates.
- **Related case.** A one-child family can still arise legitimately from a
  multi-voxel region whose other octants the mass filter drops. Scala's
  step-down handles one column correctly. R crashes there too.
- **Existing pin.** `HierScanSuite` "HierScan marks a rejected singleton child
  in the whole-mask split" asserts `nodeTests.size == 8` after a singleton
  child is rejected with `minVoxels = 1`. R semantics would add a ninth
  test.
- **Tests that would pin it further:**
  1. `Octree.split` on a one-voxel region returns `Vector.empty` for every
     `minPriorMass`.
  2. HierScan with `minVoxels = 1` on a single strong voxel reports that
     voxel in at most one hit, and no hit path extends a singleton's path.
  3. A one-column family from the mass filter produces the same adjusted
     p-value as `MaxT.singleStep` on that column. For one test, step-down
     and single-step coincide.

### 2c. `minVoxels` default: 1 in Scala, 8 in R

- **What it controls.** In both implementations a region with fewer than
  `minVoxels` voxels is not split. Its own children are not tested, but it
  was itself tested as a child of its parent. The packet 4 phrase "regions
  … are not tested" is loose, and the Scala scaladoc ("not split and
  tested") is the precise form. The default therefore changes only how
  deeply descent can localize, and at what cost. At depth d the budget is
  about `γ(1 − γ)^d α`, so very small regions are tested at tiny levels.
- **Validity.** Complete-null FWER ≤ γα is independent of `minVoxels`.
  Region-level strong control is not proven for any value (packet 4).
- **Evidence.** Every admitted calibration ran at `minVoxels = 1`: the
  calibration protocol, line 31, and the reallocation addendum, whose
  `HierScanReallocationScenarioSuite` config takes the default. Changing the
  default to 8 would move the default configuration outside the calibrated
  conditions. It would also silently change results for every caller that
  relies on the default.
- **Recommendation.** Keep the default at 1 and record it as a deliberate
  divergence from R. If R-default parity becomes a goal, the alternative is
  to remove the default, so every caller chooses. Either way, a move to 8
  should come with a predeclared partial-null calibration at 8.
- **Tests that would pin it:**
  1. A config test asserting `HierScanConfig().minVoxels == 1`, with a
     comment citing the calibration protocol. Any change then fails loudly
     and points at the evidence that must be rerun.
  2. An R-parity fixture case at `min_voxels = 8`, R's default, on a map
     large enough to have regions of 2–7 voxels under a rejected parent.
     This would show that the Scala and R semantics agree at R's default.
     It needs `tools/r-parity/generate_threshold_hierscan_fixtures.R` and
     the fixture file to be regenerated. That is outside this packet's
     claimed paths.

### Other divergence noted, out of scope

`maxDepth` exists only in Scala, with a default of 32. With tight bboxes it
does not bind on a lattice smaller than 2³² per axis, so it is a guard
rather than a semantic difference.
