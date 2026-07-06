# scalafim-image

Cross-compiled JVM/Scala.js neuroimaging core module for `scalafim`.

Package root:

```scala
import scalafim.image.*
import scalafim.image.Ops.*
```

This module contains image spaces, dense/sparse volumes, masks, affine
transforms, affine and dense-field spatial morphisms, morphism-aware resampling
plans, Jacobian modulation, dense transform-field materialization, lightweight
path execution plans, opt-in dense-field inverse approximation, clustering,
searchlights, and related numerical helpers.
JVM-specific image IO lives under:

```scala
import scalafim.image.io.*
```

Run it directly with:

```sh
sbt imageJVM/test
sbt imageJS/test
```
