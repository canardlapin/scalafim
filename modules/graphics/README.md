# scalafim-graphics

`graphics` owns the renderer-neutral plotting core for ScalaFIM.

It is not a Java2D, JavaFX, Canvas, SVG, or ggplot renderer. It is the shared
typed algebra those backends can interpret later:

- grammar-of-graphics plot, layer, geom, stat, coordinate, and aesthetic specs;
- `scales`-inspired transforms with explicit open/closed domains, trained
  ranges, out-of-bounds policies, palettes, breaks, labels, and discrete
  domains;
- `grid`-inspired immutable scene trees with units, viewports, graphical
  parameters, grob primitives, and axis/tick scene helpers;
- a device-resolution layer (`DeviceContext`, `DeviceScene`) that flattens
  scenes into numeric, y-down device primitives any backend can serialize;
- a plot layout solver (`PlotLayoutSolver`, `LayoutPolicy`, `TextMetrics`)
  that allocates named panel, axis-strip, and legend regions;
- a renderer conformance contract (`RendererConformance`, `RendererHarness`)
  that backend modules run to prove deterministic, marker-preserving output.

The module has no internal dependencies and cross-compiles to JVM and Scala.js.
Domain modules should export plot specifications or scenes into this module;
platform renderers should consume `DeviceScene` values at a boundary.

## Core laws

- Scene composition is a monoid: `Scene.empty` is identity and `++` is
  associative while preserving grob order.
- Scene coordinates are y-up (the grid convention): npc and native y increase
  toward the top of the device. Orientation is explicit — a `Viewport` carries
  a `YDirection` (default `Up`; `Down` is available for raster-style spaces) —
  and the device lowering flips exactly once, so backends never guess.
- Lengths resolve to numbers before rendering: `LengthResolver` evaluates
  npc/native/absolute expressions (including `Add`/`Sub`/`Mul` mixtures and
  typed location-plus-extent offsets)
  against a `DeviceContext`, so backends emit numeric geometry only — no CSS
  `calc()`, no percentages.
- Trained continuous ranges are immutable unions over finite observations; a
  later training pass cannot shrink a range.
- Transforms define explicit open/closed domains and must round-trip through
  their inverse on valid values within tolerance.
- Continuous scales retain both raw data domains and transformed domains:
  palette mapping uses transformed coordinates, while breaks and labels remain
  in the raw data domain.
- Default continuous breaks use a deterministic zero-anchored 1/2/5 grid with
  an approximate target count. Use `Breaks.count` when an exact number of
  equally spaced breaks is part of the caller's contract.
- Aesthetic mappings are row-aware typed values: direct, constant, and scaled
  mappings share one `AesValue` algebra, and every `AesSpec` normalizes to a
  typed `AesEnv` keyed by the `Aesthetic[A]` enum. Continuous scales consume
  `Double`, discrete scales consume `String`, and the aesthetic they bind to
  determines the rendered value type.
- Layer constructors for common geoms require their essential aesthetics in the
  Scala signature. Generic `fromMapping` allows inheritance; `Plot.addLayer`
  validates the effective layer mapping before a renderer ever sees the layer.
- Scene sizes and radii use `ExtentExpr`, not raw `LengthExpr`, so negative
  extents cannot enter primitive grobs through checked constructors.
- Raster images use checked `RasterDimensions`, opaque packed `Rgba32` pixels,
  and immutable `RasterImage` storage. Pixel rows are top-to-bottom; image
  placement still follows the scene's y-up `Point`/`Anchor` rules. Nearest and
  smooth interpolation are explicit, and file IO or platform image objects
  never enter the shared grammar.
- Axes are scene helpers, not renderer features: `Axis` lowers to baseline
  segments, tick segments, and text labels that any backend can interpret.

## Compilation pipeline

`PlotCompiler` is a facade over explicit, independently testable phases
(`CompilerPhases.scala`): mapping resolution → scale registration → row
evaluation (with typed `DroppedRow` diagnostics) → group-aware geom lowering →
layout resolution → guide resolution. Guides follow a `GuidePolicy`:
`Derived` produces routine axes from trained scales (transform-aware breaks
positioned in mapped space) and legends from discrete color/fill palettes,
with explicit `GuideSpec` overrides; layout comes from an explicit
`PanelLayout`, an explicit `PanelFrame` plus derived data ranges, or the
`LayoutPolicy` solver. Compiler-derived panel ranges use a typed
`RangeExpansion` (5% by default) after guide derivation so point glyphs at
trained extrema remain inside the panel; `RangeExpansion.none` restores exact
edge-centered framing, and an explicit `PanelLayout` is always authoritative.

## Backends

A backend implements `RendererHarness` and runs `RendererConformance.check`
in its test suite; the shared `DeviceScene` lowering is the reference
implementation. `graphics-svg` inspects serialized markup while
`graphics-canvas` records deterministic Canvas commands; both must satisfy the
same primitive, style, text-placement, raster-image, clipping, and rotation
requirements.
`graphics-java2d` independently adopts the same contract and adds raster-level
`BufferedImage` assertions for JVM rendering behavior.
