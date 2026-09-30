# HierScan reference alignment and region-level calibration — packet 4 (2026-09-30)

Mote: `bd-01M2GTMY1KYB94RGJARBB1GBK8`, packet 4. This packet resolves the
packet 3 finding (see `threshold-null-calibration-20260930.md`) that HierScan
reported only terminal hits and spent alpha differently from the reference.

## Change

`HierScan` now follows `neurothresh::hier_descend`, the whole-brain step-down
path:

- **Every rejected node is a hit.** This includes a coarse region whose own
  children are not rejected. `HierScanRegionHit` gains `alphaTest`, the level
  its step-down test ran at. `reject` is the union of all hits, and nested hits
  are deduplicated.
- **Gamma alpha spending.** A node tests its children at `γ · budget`
  (`HierScanConfig.gamma`, default 0.5, the R default). The remaining
  `(1 − γ) · budget` is shared among the rejected children in proportion to
  prior mass, so a whole tree spends at most α. The old scheme tested at the
  full budget and passed `budget / nChildren` down.
- **`minVoxels` follows R's meaning.** Regions with fewer than `minVoxels`
  voxels are not tested. The old meaning was "a region of at most `minVoxels`
  is terminal".
- **Parameters.** `gamma` is recorded in `params`.

## Parity with the R reference

`tools/r-parity/generate_threshold_hierscan_fixtures.R`, run with neurothresh
0.1.0, calls `hier_descend` on fixed 4×4×4 maps with fixed null matrices and
emits `HierScanRParityFixtures.scala`. `HierScanRParitySuite` requires the
following to match R:

- the same set of significant regions, as sorted mask indices;
- scores to a relative tolerance of 1e-9;
- adjusted p-values to 1e-12, and test alphas to 1e-15;
- a reject mask equal to their union.

There are two cases:

- **Block signal:** 8 hits across two levels, alpha_test 0.1 and 0.025.
- **Single strong voxel:** a coarse region at 0.15 and its voxel at 0.075.

Both pass on the JVM and on Scala.js.

**Reference defect found.** neurothresh 0.1.0's `wy_stepdown` fails when a node
has exactly one child column:
`wy_stepdown(c(2), matrix(c(1, 3, 0), 3, 1), alpha = 0.5)` gives
`subscript out of bounds in successive_max[, j + 1]`. `hier_descend` reaches
this with `min_voxels = 1`. The fixtures therefore use `min_voxels = 2`. The
Scala step-down handles one column. This should be reported upstream.

## Region-level calibration (held-out)

The [protocol](threshold-hierscan-calibration-protocol-20260930.md) was
committed as `7805714b` before any run. The profile was R = 2000, held-out
seed 5550930, on the JVM, run once. All 16 conditions **Pass**, and all
32,000 replicates completed. Full observations are in
[calibration-results.txt](threshold-hierscan-calibration-20260930/calibration-results.txt).
Log `f9b5ac6006d15ffa…`.

Values are region-level errors out of 2000, with power in parentheses for
partial nulls.

| Condition | Complete null | Partial null |
| --- | --- | --- |
| exact n=6, white, greater | 26 (0.013) | 22 (0.011); power 0.167 |
| exact n=6, white, two-sided | 0 | 0; power 0 |
| exact n=6, smooth, greater | 28 (0.014) | 25 (0.0125); power 0.847 |
| exact n=6, smooth, two-sided | 0 | 0; power 0 |
| MC n=10, white, greater | 35 (0.0175) | 48 (0.024); power 0.758 |
| MC n=10, white, two-sided | 43 (0.0215) | 44 (0.022); power 0.877 |
| MC n=10, smooth, greater | 39 (0.0195) | 59 (0.0295); power 0.976 |
| MC n=10, smooth, two-sided | 41 (0.0205) | 48 (0.024); power 0.985 |

- **Level.** Every rate is well below α = 0.05. The complete-null rates sit
  at or below γα = 0.025, as the design implies.
- **Exact two-sided zeros.** These were predeclared. With 64 sign flips,
  paired ±v ties make the smallest attainable p equal to 2/64 = 0.031, which
  is above the root test level of 0.025. HierScan cannot reject in these
  conditions. This is a power limit of small exact two-sided families, not a
  validity defect.

## Pull-request profile, refactor and mutation evidence

- **Gate.** `sbt thresholdJVM/test thresholdJS/test` gives 78/78 on JVM and
  78/78 on JS, exit 0, with no warnings. Log `2cc6125b9ec497af…`.
- **Refactor check.** The shared simulation world moved to
  `scenarios/SignFlipWorld.scala`. The packet 3 suite's pull-request output is
  line-for-line identical before and after the move, which confirms the
  refactor preserved its random-draw order.
- **Mutation check.** Testing at the full budget instead of `γ · budget` fails
  both parity cases and the descent test. Log `0a763cae4413f2a4…`.

## Method admission after packet 4

| Method | Status |
| --- | --- |
| `MaxT`, `WestfallYoung` | Admitted in packet 3 for voxel-level FWER, including strong control in the tested partial nulls. |
| `HierScan` | **Admitted for region-level FWER.** The conditions are sign-flip-exchangeable nulls, uniform priors, whole-brain descent, white and smooth fields, n = 6 exact and n = 10 Monte Carlo. Voxel-level FWER is not claimed: a significant region may contain null voxels. Small exact two-sided families have no power at γα. |
| TFCE, cluster-FDR, RFT, voxelwise FDR, parcel-level HierScan, `hier_descend_alpha` | Not implemented. |

## Remaining scope on the ticket

- First-level uncertainty propagation into the group statistic.
- Adaptive and non-uniform HierScan priors.
- Non-symmetric error distributions.
- A consumer round trip for the legend and native results.
- The unused `QValue`, `EvidenceScore`, `PSide` and `StatKind.T.df` should be
  either admitted or removed.
