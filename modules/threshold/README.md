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
  MaskedField.fromVolume(zMap, Tail.Positive)

val score =
  for
    f <- field
    priors <- PriorWeights.uniform(f.size)
    root <- Octree.root(f, priors)
    kappa <- Kappa(1.0)
    s <- ScoreSet.softMax(root.indices, f.valuesCopy, priors, kappa)
  yield s

val scan =
  HierScan.run(
    stat = zMap,
    nullDraw = permutationNulls,
    config = HierScanConfig(alpha = Alpha.unsafe(0.05), tail = Tail.Positive)
  )
```

See `docs/plans/neurothresh.md` for the full design and rollout order.
