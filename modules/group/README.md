# scalafim-fmri-group

Cross-compiled JVM/Scala.js group-inference core for ScalaFIM. The module owns
typed group designs, responses, weighting, contrasts, statistics, multiple
comparison control, and result values. Plotting and concrete renderers remain
application-boundary concerns.

`EstimateGroup.prepare` compiles pinned estimate inputs into bounded group reads.
It joins catalog IDs, requires one row per identified participant and an explicit
`GroupEstimateAdmission` implementation for scientific and spatial admission.
`readBlock` opens one verified unit at a time, preserves sample order, and refuses
unavailable requested cells without shrinking the cohort. Its total cell budget
counts effects and variances; SE is squared exactly once for weighted group input.
The group runtime does not depend on `fit`. The older eager `TContrastResult`
convenience is now `scalafim.fmri.fit.estimates.FitGroupAdapter` in `fit-estimates`.

A group result can be projected into the shared renderer-neutral plotting DSL:

```scala
import intaglio.*

final case class GroupEstimate(index: Double, contrast: String, estimate: Double, lower: Double, upper: Double)

val effects = plot(estimates)
  .aes(_.index, _.estimate)
  .geomPoint()
  .geomErrorBar(_.lower, _.upper)
  .axisTitles("Contrast", "Effect")
  .build
```

The application chooses SVG, Canvas, Java2D, or JavaFX only after the program
has compiled to a shared `Scene`.

Run the portable suite directly with:

```sh
sbt groupJVM/test
sbt groupJS/test
```
