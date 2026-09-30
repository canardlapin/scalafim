# Threshold null reference and orientation — packet 1 (2026-09-30)

Mote: `bd-01M2GTMY1KYB94RGJARBB1GBK8` (UMVPA prerequisite for M4.08). This is
the first bounded packet of that ticket, not its closure.

## Scope delivered

- **Method inventory.** `ThresholdMethod` had six cases; only `HierScan` and
  `MaxT` had a decision path. `Tfce`, `ClusterFdr`, `RftPeak` and `RftCluster`
  were enum-only, referenced nowhere, and could be stamped onto a result through
  `MapThresholdResult.fromLegacy`. They are removed, so an unavailable method
  is a compile error rather than a runtime refusal. No module outside
  `threshold` depends on it (`build.sbt` has no `dependsOn(threshold)`), and
  plsneuro does not reference these cases.
- **Null reference convention.** New `NullReference` (`MonteCarlo`,
  `ExactEnumeration`), declared by each `NullDraw`. Monte Carlo keeps the
  plus-one rule `(1 + c) / (B + 1)` with unchanged arithmetic. Exact
  enumeration uses `c / B`. The matrix procedures (WY, maxT, and HierScan per
  node) require a null row equal to the oriented observed statistics
  (`MissingIdentityRow`). The max-null path sees only maxima, so it can check
  only the necessary condition: some maximum must reach each observed statistic
  (`MissingIdentityAction`). This limit is documented on `NullReference`.
- **Orientation.** `WestfallYoung.stepDown`, `MaxT.singleStep` and
  `MultipleTesting.adjust` take an explicit `ThresholdAlternative` and orient
  observed and null values with the same transform. The new
  `MaxNullDistribution` carries its alternative and reference, and
  `MaxNull.pValues` orients raw observed values with it. A mismatched
  orientation is therefore not expressible. The `MaxNull.cutoff` threshold is
  on the oriented scale. The legacy `MaxNull.threshold(Array, Alpha)` is
  removed. It had no callers outside tests, and it could not state its scale.
- **Streaming.** `MaxNull.reduce(nullDraw, fieldSize, alternative, orientation)`
  requests each draw once and keeps one maximum per draw. It never builds a
  draws-by-voxels matrix. It validates length, finiteness, unsigned sign, and
  whether the alternative is admissible for the evidence orientation.
- **Contracts.** `NullDraw` now documents the raw, mask-space, deterministic
  per-index draw contract. HierScan records `nullReference` in its params.

The strict/inclusive max-null cutoff repair (`f0c9f5c`) is not reopened. Its
exhaustive oracle suite still passes unchanged under `MonteCarlo`/`Greater`.

## Evidence

Base `8d0dd730`, branch `threshold/null-reference-20260930`, run in an isolated
worktree.

| Gate | Result | Log SHA-256 |
| --- | --- | --- |
| `sbt thresholdJVM/test thresholdJS/test` | 34/34 JVM, 34/34 JS, exit 0, no warnings | `ca03603a…c3ed39e` |
| Mutation check: exact enumeration using plus-one, and WY not orienting nulls | `NullReferenceSuite` 5/10 fail, exit 1; sources restored | `084e3385…aae436e` |
| After review fixes: `sbt thresholdJVM/test thresholdJS/test` | 35/35 JVM, 35/35 JS, exit 0, no warnings | `52c7654b…` |
| Mutation check: identity-row check disabled | refusal test fails, exit 1; source restored | `84760167…` |

## Independent review

A fresh-context reviewer ran the gate and returned ACCEPT-WITH-FIXES on
`6d7ad148`, with no statistical defects. It independently derived the
conventions, the cutoff/p-value equivalence (0 mismatches over an exact sweep
with B ≤ 6), WY validity under orientation, and unchanged Monte Carlo/Greater
arithmetic. All of its findings are addressed in the follow-up commit:

- The identity check was only necessary, so a mislabelled Monte Carlo sample
  could pass as exact. The matrix procedures now require an identity row, and
  the reviewer's reproducer is a regression test.
- The matrix APIs are documented as orientation-agnostic primitives, and
  `fromOrientedMaxima` as a trust boundary.
- `AdjustedTest.score` is documented as the raw supplied value.
- The HierScan law tests now use a 4×4×4 field and assert rejections and
  descent past depth 1, under all three alternatives.
- The stale signatures in `docs/plans/neurothresh.md` are updated.

New `NullReferenceSuite` (shared, so it runs on both platforms):

- **Raw-draw oracle, exhaustive.** Covers 4,368 draw sets × 3 alternatives
  × 2 references × alphas × observed values, more than 10⁶ comparisons. The
  oracle orients values from the definitions, not through `applyTo`. It
  checks the p-value exactly, the cutoff decision against `p ≤ α`, and
  refusal when an exact set lacks the identity action.
- **Law: exact enumeration with identity ≡ Monte Carlo over the rest.**
  Checked for WY, maxT and max-null on random data, all alternatives,
  bit-for-bit.
- **Law: alternatives.** `Less` ≡ `Greater` on negated inputs, and
  `TwoSided` ≡ `Greater` on absolute values (WY).
- **HierScan.** The Less ≡ negated-Greater law and the exact ≡ Monte Carlo
  law both hold on asymmetric random draws, which rules out ULP drift between
  the observed-score and null-score paths.
- **Exact cutoffs.** Only attainable p-values are admitted. Cutoffs are
  checked by hand for B = 10.
- **Method refusal.** Removed methods fail to compile (`compileErrors`).
- **Streaming.** Each draw is requested exactly once, in order, and every
  validation error is typed.

## Remaining scope on the ticket

- HierScan's reported cutoff is on the set-score scale, not the voxel scale,
  so `result.threshold` applied to the map disagrees with `reject`.
- There is no draw-failure accounting (missing or failed draws abort the run),
  no null-family or subject-action identity, and no check of draw determinism
  across nodes.
- Neither a common subject-bound null action nor a fixed search mask across
  consumers is enforced.
- Strong-control (subset pivotality) conditions for Westfall-Young are
  neither documented nor checked.
- There is no calibration yet: spatial FWER/FDR on complete- and partial-null
  fields, adaptive HierScan regions and priors under the complete null, and
  first-level uncertainty.
- The p-value, cutoff, mask, legend and native-result round trip is untested.
  There are no consumers yet.
- The unused `QValue`, `EvidenceScore`, `PSide` and `StatKind.T.df` should be
  either admitted or removed.
