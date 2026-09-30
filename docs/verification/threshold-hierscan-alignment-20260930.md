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

There are three cases:

- **Block signal:** 8 hits across two levels, alpha_test 0.1 and 0.025.
- **Single strong voxel:** a coarse region at 0.15 and its voxel at 0.075.
- **Unequal extents and non-uniform prior:** a 5×3×4 grid with prior η = 0.5.
  Three sibling regions are rejected, so the descendant budget is split by
  mass (alpha_test 0.04128), and the adjusted p-values sit above the
  1/(B + 1) floor.

All three pass on the JVM and on Scala.js. The fixture regenerates
byte-identically; the reviewer checked this independently.

**Layout note.** R flattens arrays x-fastest, while scalafim's canonical
ordinal is z-fastest. The suite converts values in and region indices out.
An earlier version fed R vectors unconverted. It passed on the two 4×4×4
cases only because their signals and octants are symmetric under x↔z; the
5×3×4 case exposed this. There was no defect in `HierScan` itself.

`nodeTests` are not compared: `hier_descend` does not expose non-rejected
node tests.

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

- **Level.** Every rate is well below α = 0.05. Under the complete null,
  descent happens only after a rejection, so the first error must come from
  the level-1 family, and complete-null FWER is at most γα = 0.025 exactly.
  The observed rates, 0.013–0.022, agree.
- **Exact two-sided zeros.** These were predeclared. With 64 sign flips,
  paired ±v ties make the smallest attainable p equal to 2/64 = 0.031, which
  is above the root test level of 0.025. HierScan cannot reject in these
  conditions. This is a power limit of small exact two-sided families, not a
  validity defect.

## Budget-reallocation calibration (held-out addendum)

An independent review found that a single corner block never exercises
mass-weighted reallocation among rejected non-null siblings. A
[predeclared addendum](threshold-hierscan-reallocation-protocol-20260930.md),
committed as `130c906c` before any run, adds two signal blocks in different
root octants and a non-uniform prior. The profile was R = 2000, held-out seed
9190930, run once. All 4 conditions **Pass**, as recorded in
[reallocation-results.txt](threshold-hierscan-calibration-20260930/reallocation-results.txt)
(log `1d52462911388a17…`).

| Condition | Region-level errors | Both blocks reported | Power, block A / B |
| --- | --- | --- | --- |
| MC n=10, white, greater | 43 (0.0215) | 0.371 | 0.764 / 0.418 |
| MC n=10, white, two-sided | 34 (0.017) | 0.422 | 0.887 / 0.441 |
| MC n=10, smooth, greater | 36 (0.018) | 0.810 | 0.972 / 0.827 |
| MC n=10, smooth, two-sided | 40 (0.020) | 0.808 | 0.977 / 0.809 |

## Pull-request profile, refactor and mutation evidence

- **Gate after the review fixes.** `sbt thresholdJVM/test thresholdJS/test`
  gives 83/83 on JVM and 83/83 on JS, with no warnings (log `dc41465f19a44891…`).

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
| `HierScan` | **Admitted for region-level FWER as empirically calibrated in the tested conditions, not proven.** The tested conditions are sign-flip-exchangeable nulls, whole-brain descent, white and smooth fields, n = 6 exact and n = 10 Monte Carlo, uniform priors, and a non-uniform prior with reallocation among two rejected non-null siblings. Complete-null FWER ≤ γα is proven. There is no theorem of strong region-level control: the descendant budget is data-dependent and non-monotone, so the sequential-rejection argument does not apply. Voxel-level FWER is not claimed; `reject` can contain null voxels inside a significant region, and this is documented on `HierScanResult`. Small exact two-sided families have no power at γα. |
| TFCE, cluster-FDR, RFT, voxelwise FDR, parcel-level HierScan, `hier_descend_alpha` | Not implemented. |

## Independent review

A fresh-context reviewer returned ACCEPT-WITH-FIXES on `9f0d2902`. It found
no implementation defect:

- It verified the γ split, mass weighting, `minVoxels`/`minAlpha`, octree
  midpoints and the whole-tree budget bound against `hier_descend`.
- The fixture regenerated byte-identically.
- The R one-column bug was confirmed.
- The complete-null bound was proven exact.
- The SignFlipWorld refactor preserves the draw order.

Its findings are addressed as follows:

- **Medium 1: overclaimed admission.** Reworded as empirical, with no proof
  of strong control.
- **Medium 2: reallocation never exercised.** Covered by the held-out
  addendum above.
- **Low 3: thin parity fixture.** Covered by the unequal-extent,
  non-uniform-prior case. That case also exposed the test-layout bug
  described above.
- **Low 4: reject-mask semantics.** `HierScanResult` now documents them.

Edge divergences it recorded, which the tests do not reach:

- Child-mass inclusion: Scala keeps `> minPriorMass`, R keeps `>= min_pi_mass`.
- A singleton bbox gives no children in Scala and one child in R, which is
  where R's `wy_stepdown` crash comes from.
- `maxDepth` exists only in Scala.
- Default `minVoxels` is 1 in Scala and 8 in R.

## Remaining scope on the ticket

- First-level uncertainty propagation into the group statistic.
- Adaptive and non-uniform HierScan priors.
- Non-symmetric error distributions.
- A consumer round trip for the legend and native results.
- The unused `QValue`, `EvidenceScore`, `PSide` and `StatKind.T.df` should be
  either admitted or removed.
