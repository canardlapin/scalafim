# scalafim-fmri-motion

Cross-compiled JVM/Scala.js fMRI motion-correction primitives for `scalafim`.

Package root:

```scala
import scalafim.fmri.motion.*
```

This module contains typed rigid poses and motion traces, validated acquisition
timing descriptions, framewise displacement and DVARS metrics, motion QC
records, a portable baseline rigid estimator with optional pyramid levels,
typed information-content stencil sampling, low-motion pose shrink, temporal
regularization plus frame-mean nuisance residual removal, deterministic
pose-spline primitives, and one-pass rigid-motion application over
`NeuroVec[Double]`, including packet-aware linear application for validated
slice/packet timing.
`MotionCorrectionResult` bundles an estimate, optional corrected run, QC, and
the controls that produced them. Profiles expose typed active and planned
capabilities; full IC template-mode whitening, parallel execution, richer
packet-aware estimator semantics, JVM NIfTI IO, CLI, and report writers are
staged later layers. `WhiteningPolicy.FrameMeanOnly` is supported as the typed
frame-mean residual path; `WhiteningPolicy.IcWhiten` is an explicit unsupported
control until the template-mode whitening contract is proven.

Run it directly with:

```sh
sbt motionJVM/test
sbt motionJS/test
```
