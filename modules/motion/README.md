# scalafim-fmri-motion

Cross-compiled JVM/Scala.js fMRI motion-correction primitives for `scalafim`.

Package root:

```scala
import scalafim.fmri.motion.*
```

This module contains typed rigid poses and motion traces, framewise
displacement and DVARS metrics, motion QC records, a portable baseline rigid
estimator with optional pyramid levels, low-motion pose shrink, and temporal
regularization plus frame-mean nuisance residual removal, and one-pass
rigid-motion application over `NeuroVec[Double]`. Profile components report
currently implemented behavior; IC stencil, broader whitening, parallel
execution, spline acquisition timing, JVM NIfTI IO, CLI, and report writers are
staged later layers.

Run it directly with:

```sh
sbt motionJVM/test
sbt motionJS/test
```
