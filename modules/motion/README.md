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
the controls that produced them. JVM builds support deterministic ordered
parallel mapping for independent diagnostic and template-refresh frame work;
Scala.js rejects `ExecutionPolicy.ParallelFrames` as a typed unsupported
control. JVM builds also include lightweight NIfTI/sidecar IO, BIDS scan
discovery with TR/slice-timing metadata, motion/matrix/summary report bundle
writers, executable typed CLI commands, and an opt-in synthetic/external
benchmark harness. Profiles expose typed active and planned capabilities; full
IC template-mode whitening, richer packet-aware estimator semantics,
packaged shell-command distribution, and external real-data benchmark inputs
are staged later layers.
`WhiteningPolicy.FrameMeanOnly` is supported as the typed frame-mean residual
path; `WhiteningPolicy.IcWhiten` is an explicit unsupported control until the
template-mode whitening contract is proven.

Motion QC values can be projected into a renderer-neutral plot without a
reporting framework dependency:

```scala
import intaglio.*

final case class MotionQcPoint(frame: Double, displacement: Double)

val qcPlot = plot(qcPoints)
  .aes(_.frame, _.displacement)
  .geomLine()
  .hline(0.5)
  .axisTitles("Frame", "Framewise displacement (mm)")
  .build
```

Run it directly with:

```sh
sbt motionJVM/test
sbt motionJS/test
```

Run the opt-in JVM benchmark smoke harness with:

```sh
sbt "motionJVM/runMain scalafim.fmri.motion.benchmark.MotionBenchmarkCli --profile smoke --out modules/motion/jvm/target/motion-benchmark-smoke"
```

The volregger closeout audit and unsupported-feature ledger are in
[`docs/audits/motion-volregger-closeout.md`](../../docs/audits/motion-volregger-closeout.md).
