# scalafim-fmri-group

Cross-compiled JVM/Scala.js group-inference core for ScalaFIM. The module owns
typed group designs, responses, weighting, contrasts, statistics, multiple
comparison control, and result values. Plotting and concrete renderers remain
application-boundary concerns.

A group result can be projected into the shared renderer-neutral plotting DSL:

```scala
import scalafim.graphics.*

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
