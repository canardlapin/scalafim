# scalafim-graphics

`graphics` owns the renderer-neutral plotting core for ScalaFIM.

It is not a Java2D, JavaFX, Canvas, SVG, or ggplot renderer. It is the shared
typed algebra those backends can interpret later:

- grammar-of-graphics plot, layer, geom, stat, coordinate, and aesthetic specs;
- `scales`-inspired transforms, trained ranges, out-of-bounds policies,
  palettes, breaks, labels, and discrete domains;
- `grid`-inspired immutable scene trees with units, viewports, graphical
  parameters, grob primitives, and axis/tick scene helpers.

The module has no internal dependencies and cross-compiles to JVM and Scala.js.
Domain modules should export plot specifications or scenes into this module;
platform renderers should consume those values at a boundary.

## Core laws

- Scene composition is a monoid: `Scene.empty` is identity and `++` is
  associative while preserving grob order.
- Trained continuous ranges are immutable unions over finite observations; a
  later training pass cannot shrink a range.
- Transforms define explicit domains and must round-trip through their inverse
  on valid values within tolerance.
- Continuous scales retain both raw data domains and transformed domains:
  palette mapping uses transformed coordinates, while breaks and labels remain
  in the raw data domain.
- Scale bindings are row-aware: each binding pairs an aesthetic, a row extractor,
  and a typed scale. Continuous scales consume `Double`, discrete scales consume
  `String`, and the aesthetic they bind to determines the rendered value type.
- Layer constructors for common geoms require their essential aesthetics in the
  Scala signature. Generic `fromMapping` allows inheritance; `Plot.addLayer`
  validates the effective layer mapping before a renderer ever sees the layer.
- Axes are scene helpers, not renderer features: `Axis` lowers to baseline
  segments, tick segments, and text labels that any backend can interpret.
