# scalafim-fmri-threshold

Spatial inference over statistic maps for ScalaFIM.

This module is the Scala 3 home for the computational core inspired by
`~/code/neurothresh`: prior-weighted set scoring, maxT and Westfall-Young
correction, octree region primitives, and hierarchical scanning.
`ThresholdMethod` lists only methods with an implemented decision path
(`HierScan`, `MaxT`); TFCE, cluster-FDR, and RFT are not implemented.

Permutation inference is explicit about two conventions:

- **Orientation.** Observed statistics and null draws are supplied raw and
  oriented together by one `ThresholdAlternative`. A `MaxNullDistribution`
  carries the alternative that produced it, so observed values cannot be
  compared with a differently oriented null. Its cutoff is on the oriented
  scale.
- **Reference.** A `NullDraw` declares its `NullReference`. `MonteCarlo`
  draws exclude the identity action and use `(1 + #{null >= t}) / (B + 1)`.
  `ExactEnumeration` draws include it and use `#{null >= t} / B`; a set whose
  nulls never reach an observed statistic is refused as missing the identity.

A `StatisticMap` is labelled `Z`, `T` or `NegLog10P`. The label fixes only
the evidence orientation: `Z` and `T` maps are signed and admit every
alternative; a `NegLog10P` map is unsigned and admits only `Greater`. The
label carries no degrees of freedom and no p-value sidedness. Every admitted
procedure judges a map only against null draws of the same statistic, and
maxT and Westfall-Young p-values and rejections (not the value-scale
cutoff) are unchanged by a strictly increasing odd transform applied to both
the map and its null draws, such as an exact t-to-z conversion with one df
shared by all voxels. HierScan's set scores are not invariant this way, so
both procedures record the label as `statKind` in their `params`. Whether a
`NegLog10P` map is one- or two-sided is part of the caller's hypothesis,
and the null draws must match it. No scale conversion is implemented.

`MaxNull.reduce` streams draws into per-draw maxima without a
draws-by-voxels matrix. `MaxT.runMap` builds a voxelwise result from it whose
reject mask, adjusted p-value map and cutoff agree by construction.

Decision scale is explicit. Only voxelwise `MapThresholdResult`s carry a
`cutoff`. A `HierScanResult` rejects the union of its significant regions,
scored on set statistics, so it exposes no voxel threshold.

Every null draw is accounted for. A failed or invalid draw aborts the
procedure with `NullDrawFailed(index, cause)` rather than being dropped. A
draw that returns a different field when HierScan revisits it is refused as
`NondeterministicNullDraw(index)`.

The module is cross-compiled for JVM and Scala.js. Shared code depends on
`scalafim-image` for volumes, masks, spaces, and connected components, and on
`scalafim-linalg` for small matrix containers. It does not depend on
`scalafim-fmri-group`; group, fit, atlas, and surface workflows can feed maps
and regions into this layer.

```scala
import scalafim.fmri.threshold.*
import scalafim.image.*

val field =
  StatisticField.fromMap(StatisticMap.z(zMap), ThresholdAlternative.Greater)

val score =
  for
    statField <- field
    f = statField.evidence
    priors <- PriorWeights.uniform(statField.size)
    root <- Octree.root(f, priors)
    input <- ScoringInput(f, priors, root)
    kappa <- Kappa(1.0)
    s <- ScoreSet.softMax(input, kappa)
  yield s

val scan =
  HierScan.runMap(
    statistic = StatisticMap.z(zMap),
    nullDraw = permutationNulls,
    config = HierScanConfig(alpha = Alpha.unsafe(0.05), alternative = ThresholdAlternative.Greater)
  )
```

See `docs/plans/neurothresh.md` for the full design and rollout order.
