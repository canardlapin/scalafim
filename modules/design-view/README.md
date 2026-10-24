# scalafim-fmri-design-view

Optional JVM/Scala.js graphics for fMRI designs, design reviews, response bases
and design-matrix rasters. Sources keep the `scalafim.fmri.design` package and
all existing APIs. The numerical models and review data stay in `design`.

Add `designViewJVM` or `designViewJS` to a source-build consumer that uses
`DesignGraphics`, `DesignReviewGraphics`, `ResponseBasisGraphics` or
`DesignMatrixRaster` (including its palette and error types). The corresponding
artifact is `scalafim-fmri-design-view`. A dependency on `design` alone no longer
includes these plotting APIs or Intaglio.

Renderer-neutral event plot export:

```scala
import scalafim.fmri.design.*
import intaglio.svg.*

val scene = DesignGraphics.eventModelScene(model, termName = Some("task")).toOption.get
val svg = SvgRenderer.render(scene).toOption.get
```

`DesignGraphics` itself is implemented with the public plotting DSL. A design
adapter can use the same surface directly:

```scala
import intaglio.*

val trained = plot(eventPlotData.points)
  .aes(_.time, _.response)
  .group(_.regressor)
  .scaleColorDiscrete(_.regressor, levels = eventPlotData.regressors, name = "regressor")
  .geomLine()
  .resolve
```

The dependency direction is `design-view -> design + Intaglio core`. Applications
select an Intaglio renderer separately; SVG is used only by the JVM integration
gallery. Analysis modules depend on `design` without graphics.

Run it directly with:

```sh
sbt designViewJVM/test
sbt designViewJS/test
tools/render_design_gallery.sh
```
