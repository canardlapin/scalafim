# scalafim-fmri-threshold

Spatial inference over statistic maps for ScalaFIM.

This module is the Scala 3 home for the computational core inspired by
`~/code/neurothresh`: prior-weighted set scoring, maxT and Westfall-Young
correction, octree region primitives, and later TFCE/cluster/RFT methods.

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
