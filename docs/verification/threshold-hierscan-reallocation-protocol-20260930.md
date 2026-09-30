# HierScan budget-reallocation calibration protocol (predeclared addendum, 2026-09-30)

Mote: `bd-01M2GTMY1KYB94RGJARBB1GBK8`, packet 4. This addendum is committed
before any run. It was prompted by an independent review, which found that the
[HierScan protocol](threshold-hierscan-calibration-protocol-20260930.md)
never exercises mass-weighted budget reallocation among several rejected
non-null siblings. On a 6³ grid, a single corner block forms one chain of
non-null nodes.

## Design

The data-generating process, statistic and null families are those of the
HierScan protocol, restricted to Monte Carlo n = 10. The two changes are:

- **Two signal blocks** in different root octants, each with δ = 1.2:
  - Block A covers the 2×2×2 box where all three grid coordinates are
    below 2 (8 voxels).
  - Block B covers the 2×2×1 box where the first two coordinates are at
    least 4 and the third equals 5 (4 voxels, lower power).
  - The root split puts the two blocks in different 3×3×3 children, so both
    can be rejected as siblings at level 1.
- **A non-uniform prior.** Prior weight 2 applies where the slowest grid
  coordinate is at least 3, and weight 1 elsewhere. `priorEta` is 1, so the
  prior is used without shrinkage and the rejected sibling octants have
  unequal masses.

Coordinates here are scalafim canonical coordinates, with the ordinal written
as `a + 6·(b + 6·c)`, where "first" is `a` and "slowest" is `c`.

**Conditions.** {white, smooth} × {greater, two-sided}: 4 conditions, indexed
0–3 in that nesting, with the last factor fastest.

## Error event and acceptance

- **Error event.** Any significant region with no voxel in block A or block B.
- **Recorded, not gated.**
  - The rate of replicates in which both a block-A region and a block-B region
    are reported. This shows that reallocation among rejected siblings
    actually occurred.
  - Power for each block separately.
- **Acceptance.** Every replicate completes, and HierScan is not liberal:
  fail if P(Binom(R, 0.05) ≥ k) < 0.001.

## Profiles and seeds

- **Pull request.** R = 200, development seed 20260932, on the JVM and on
  Scala.js.
- **Calibration** (`SCALAFIM_THRESHOLD_CALIBRATION=calibration`, JVM). R = 2000,
  held-out seed 9190930, not used before this commit.

A condition's seed is `base + 1000 × conditionIndex`.

## Claim boundary

A pass is empirical evidence in these conditions only. There is no theorem of
strong region-level FWER control for this procedure. The descendant budget is
data-dependent and non-monotone, so the sequential-rejection argument
(Goeman & Solari) does not apply.
