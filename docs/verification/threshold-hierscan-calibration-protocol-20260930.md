# HierScan calibration protocol (predeclared, 2026-09-30)

Mote: `bd-01M2GTMY1KYB94RGJARBB1GBK8`, packet 4. This protocol is committed
before any calibration-profile run of the reference-aligned HierScan. It
extends the [packet 3 protocol](threshold-null-calibration-protocol-20260930.md),
which was run on the previous, terminal-only HierScan. That earlier protocol
and its results stay on record unchanged.

## Question

The reference-aligned HierScan reports every rejected node and spends alpha
with γ = 0.5, split by prior mass (`neurothresh::hier_descend`). Two questions
follow:

- **Complete null.** Does it control FWER at α?
- **Partial null.** Does it control region-level FWER, meaning the probability
  of reporting any region that contains no true signal?

## Data-generating process, null families and conditions

These are identical to the packet 3 protocol: 6×6×6 grid, subject scales
`exp(0.5 z)`, optional in-grid 3×3×3 mean smoothing of the noise before the
signal is added, a partial-null signal of δ = 1.2 on the 2×2×2 corner block,
and voxelwise one-sample t.

- **Null families.**
  - Exact n = 6: 64 flips, identity first.
  - Monte Carlo n = 10: 99 draws uniform with replacement from all 1,024
    flips.
- **Level.** α = 0.05, `HierScanConfig` defaults otherwise: kappas {1, 2, 4},
  minVoxels 1, uniform priors.
- **Condition order.** This order is part of the seed contract. Conditions
  are indexed 0–15 in the nesting {exact, Monte Carlo} ×
  {white, smooth} × {greater, two-sided} × {complete, partial}, with the last
  factor fastest.

## Error events per replicate

- **Complete null.** Any significant region.
- **Partial null.** Any significant region whose voxels are all outside the
  signal block. A region that contains at least one signal voxel is a true
  discovery at the region level, even if it also contains null voxels. This
  protocol makes no voxel-level claim for HierScan.

## Acceptance

1. **Every replicate completes.** Any `Left` fails the condition.
2. **Not liberal.** Fail if P(Binom(R, α) ≥ k) < 0.001.
3. **No over-conservatism gate.** None applies.
   - In the complete null, the design spends γα = 0.025 on the root test and
     at most (1 − γ)α below it. It can spend below-root budget only after a
     false root rejection, so the expected rate is at most about γα.
   - For exact two-sided conditions, the smallest attainable p-value is
     2/64 = 0.031 > γα. These conditions therefore cannot reject at all, and
     an observed 0 is expected, not a defect.
4. **Recorded, not gated.** For partial nulls, the fraction of replicates in
   which some significant region contains a signal voxel (power).

## Profiles and seeds

- **Pull request.** R = 200, development seed 20260931, on the JVM and on
  Scala.js.
- **Calibration** (`SCALAFIM_THRESHOLD_CALIBRATION=calibration`, JVM). R = 2000,
  held-out base seed 5550930, not used before this commit.

A condition's seed is `base + 1000 × conditionIndex`.

## Not covered

- Voxel-level FWER for HierScan.
- Non-uniform or adaptive priors.
- Parcel-level HierScan, which is not implemented.
- `hier_descend_alpha`, the alpha-spending variant without step-down, which
  is not implemented.
