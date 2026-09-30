# Threshold decision scale and null-draw accounting — packet 2 (2026-09-30)

Mote: `bd-01M2GTMY1KYB94RGJARBB1GBK8` (UMVPA prerequisite for M4.08). This
packet is the second bounded packet and builds on packet 1
(`6c3cb4fb`, see `threshold-null-reference-20260930.md`).

## Scope delivered

- **Decision scale.** HierScan reported `cutoff = Inclusive(min hit set
  score)`. That is a set-score value, so applying `result.threshold` to the
  voxel map disagreed with `reject`. `cutoff` and `threshold` now live
  only on the voxelwise `MapThresholdResult`. `HierScanResult` has neither,
  and its reject mask is documented and tested as exactly the union of its
  significant regions. A voxel threshold can no longer be misapplied to it,
  because the calls do not compile.
- **MaxT map path.** Until now `ThresholdMethod.MaxT` had no builder.
  `MaxT.runMap(statistic, nullDraw, mask, alpha, alternative)` streams draws
  through `MaxNull.reduce`. From that single distribution it derives the
  reject mask, an adjusted p-value map (`ThresholdPValues.Adjusted(_,
  MaxTSingleStep)`, NaN outside the mask) and the oriented-scale cutoff.
- **Null-draw accounting.** The new private `NullDrawLedger` fetches draws
  for `MaxNull.reduce` and `HierScan`:
  - A failed, wrong-length, non-finite or invalid unsigned draw aborts with
    `NullDrawFailed(index, cause)`. Nothing is dropped, because shrinking the
    null family would change the reference distribution.
  - Each draw is fingerprinted on first fetch (FNV-1a over the raw bits,
    one `Long` per draw). If HierScan's per-node re-fetch returns a
    different field, the ledger reports `NondeterministicNullDraw(index)`.

## Evidence

Branch `threshold/decision-scale-20260930`, isolated worktree, based on `6c3cb4fb`.

| Gate | Result | Log SHA-256 prefix |
| --- | --- | --- |
| `sbt thresholdJVM/test thresholdJS/test` | 40/40 JVM, 40/40 JS, exit 0, no warnings | `cda1058f17233ed3` |
| Mutation check: ledger stops wrapping failures and stops comparing fingerprints | `DecisionScaleSuite` 2/5 fail, exit 1; source restored | `9b23b6b052159496` |

New `DecisionScaleSuite` (shared):

- **Raw-draw oracle for maxT maps.** Covers all three alternatives on a
  masked 4×4×4 field. For every voxel it checks p, reject, and the cutoff
  decision on the oriented score. Outside the mask, p is NaN and nothing is
  rejected.
- **Agreement with the matrix path.** `MaxT.runMap` equals
  `MaxT.singleStep` on the same draws, bit for bit.
- **HierScan decision scale.** `reject` equals the union of the
  significant regions, and `cutoff` and `threshold` fail to compile on
  `HierScanResult`.
- **Draw accounting.** A failed draw is reported with its index by both
  maxT and HierScan. A drifting draw is refused at the deeper-node revisit,
  and a deterministic control shows the scan does reach that revisit.

## Remaining scope on the ticket

- Strong-control (subset pivotality) conditions for Westfall-Young, and the
  admission of HierScan's adaptive regions and priors under the complete null.
- A common subject-bound null action and a fixed search mask across
  consumers. That needs a consumer-level null-family identity; the per-run
  fingerprint does not cover it.
- Calibration: spatial FWER/FDR on complete- and partial-null fields with
  spatial dependence, small n, heterogeneity, and first-level uncertainty.
- The legend and native-result round trip, which needs a consumer.
- The unused `QValue`, `EvidenceScore`, `PSide` and `StatKind.T.df`
  should be either admitted or removed.
