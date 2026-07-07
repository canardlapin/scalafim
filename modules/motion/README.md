# scalafim-fmri-motion

Cross-compiled JVM/Scala.js fMRI motion-correction primitives for `scalafim`.

Package root:

```scala
import scalafim.fmri.motion.*
```

This module contains typed rigid poses and motion traces, validated acquisition
timing descriptions, framewise displacement and DVARS metrics, motion QC
records, a portable baseline rigid estimator with optional pyramid levels,
low-motion pose shrink, temporal regularization plus frame-mean nuisance
residual removal, and one-pass rigid-motion application over `NeuroVec[Double]`.
`MotionCorrectionResult` bundles an estimate, optional corrected run, QC, and
the controls that produced them. Profiles expose typed active and planned
capabilities; IC stencil, broader whitening, parallel execution, packet-aware
application, JVM NIfTI IO, CLI, and report writers are staged later layers.

Run it directly with:

```sh
sbt motionJVM/test
sbt motionJS/test
```
