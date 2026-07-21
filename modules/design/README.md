# scalafim-fmri-design

Cross-compiled JVM/Scala.js fMRI design module for `scalafim`.

Package root:

```scala
import scalafim.fmri.design.*
```

This module contains event models, formula parsing, condition bases, baseline
models, contrast definitions, design-matrix metadata, and design export helpers.
It depends on `scalafim-fmri-hrf` for sampling/convolution, `scalafim-linalg`
for shared QR/rank primitives, and `scalafim-graphics` for renderer-neutral
plot and scene exports.

Preferred typed entry points:

- `EventModelBuilder.EventDesignRequest` collects formula, data, schedule,
  build options, and extension registries into one inspectable request value.
- `FormulaParser.parseEither` and `EventModelBuilder.buildEither` return
  `DesignError` values instead of throwing at API boundaries.
- `BaselinePlan`, `NuisanceInput`, and `NuisancePolicy` provide a validated
  baseline/nuisance model request surface over the legacy parameter list.
- `HrfSelection` distinguishes shared HRFs from per-event HRFs without
  union casts.
- `DesignColumnDescriptor` and typed export indices (`DesignColumnIndex`,
  `ScanIndex`, `RunIndex`, `BasisIndex`, `TermIndex`) sit beside the legacy
  `DesignColumnMeta` fields for safer downstream plotting/reporting.
- `DesignGraphics.eventPlot` and `DesignGraphics.eventScene` adapt
  `EventPlotData` into graphics `Plot` and `Scene` values without depending on
  SVG or any platform renderer.
- `ContrastExpr`, `CellSelector`, `BasisSelection`, and `CompiledContrast`
  provide a typed, total contrast path via `compileEither` while legacy
  `ContrastSpec`/`ContrastWeights` constructors remain available.

Renderer-neutral event plot export:

```scala
import scalafim.fmri.design.*
import scalafim.graphics.svg.*

val scene = DesignGraphics.eventModelScene(model, termName = Some("task")).toOption.get
val svg = SvgRenderer.render(scene).toOption.get
```

`DesignGraphics` itself is implemented with the public plotting DSL. A design
adapter can use the same surface directly:

```scala
import scalafim.graphics.*

val trained = plot(eventPlotData.points)
  .aes(_.time, _.response)
  .group(_.regressor)
  .scaleColorDiscrete(_.regressor, levels = eventPlotData.regressors, name = "regressor")
  .geomLine()
  .resolve
```

The dependency direction is `design -> graphics -> graphics-svg` at the
application boundary. `design` produces plot specs/scenes; SVG remains an
optional renderer adapter.

Run it directly with:

```sh
sbt designJVM/test
sbt designJS/test
```
