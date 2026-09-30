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

`MaxNull.reduce` streams draws into per-draw maxima without a
draws-by-voxels matrix.

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
