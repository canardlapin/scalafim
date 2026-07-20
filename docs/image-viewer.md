# Image viewer infrastructure

ScalaFIM separates image geometry, view composition, interaction, and toolkit
rendering. A browser Canvas, Java2D surface, and JavaFX Canvas therefore display
the same sampled rasters and interpret pointer positions through the same panel
receipts.

## Coordinate contract

- World points use NIfTI RAS+: +x is right, +y anterior, +z superior.
- Voxel coordinates are continuous, zero-based voxel-center coordinates.
- NIfTI `sform` is preferred when present; otherwise `qform`, including `qfac`,
  is reconstructed. Header spacing/origin is only the no-form fallback.
- `AnatomicalPlane` names sagittal, coronal, and axial views.
- `LeftRightConvention.PatientLeftOnLeft` and `PatientRightOnLeft` state screen
  mirroring directly. Mirroring changes presentation, not world geometry.
- `SliceGrid` rows are top-to-bottom. Pixel `(0, 0)` is a center, and volume
  coverage is computed from the eight transformed voxel-cell boundary corners.

The `image` module owns this contract. `SlicePlane`, `SliceGrid`, and
`OrthogonalSliceGrids` are renderer-independent, while `SlicePlan` converts a
grid into two affine voxel-space increments. Nearest sampling is available for
any voxel type; linear and cubic sampling are `Double`-only at compile time.

## Minimal shared view

```scala
import scalafim.graphics.*
import scalafim.image.*
import scalafim.image.view.*

val anatomy: NeuroVol[Double] = ???

val layer = SliceLayer(
  id = LayerId.unsafe("anatomy"),
  volume = anatomy,
  sampling = SliceSampling.Linear(),
  colorizer = ScalarColorizer(DisplayWindow.unsafe(-100.0, 300.0))
)

val model = ViewerModel
  .fromLayers(layer)
  .fold(error => throw new IllegalArgumentException(error.message), identity)

val session = ViewerSession(
  state = ViewerState.centered(model.referenceSpace),
  device = DeviceContext.unsafe(1000.0, 800.0)
)

val frame = session.frame(model)
  .fold(error => throw new IllegalArgumentException(error.message), identity)
```

`frame.scene` is a normal `graphics.Scene`; `frame.panels` records the exact
world grid and fitted device rectangle for each anatomical view. Scalar,
integer-label, and Boolean-mask layers can coexist because each layer retains
its own sampling and colorization types. Layers with different affines are
sampled into the reference grid in world space.

## Interaction

`ViewerAction` and `ViewerReducer` form a pure state machine. Actions cover
world-coordinate picking, anatomical scrolling, display convention, pixel and
slice step, window, opacity, visibility, timepoint, resize, crosshair, and
orientation labels.

```scala
val action = ViewerEvents.pick(frame, deviceX = 240.0, deviceY = 160.0)
val next = action.flatMap(ViewerReducer.reduce(model, session, _))
```

Device input is y-down; panel receipts convert it to root NPC and then to the
top-to-bottom slice grid. Scrolling follows each plane's positive anatomical
normal and is independent of screen mirroring.

## Temporal, lazy, and mapped layers

`SliceLayer.series` displays an in-memory `NeuroVec`. `VolumeSource.lazyFrames`
adapts a lazy or IO-backed frame reader while declaring its stable
`VolumeSpace` and frame count. Models reject inconsistent temporal lengths;
time-invariant overlays remain visible at every timepoint.

A registered overlay can supply `LayerMapping.Pullback(morphism)`. The
`SpatialMorphism` maps reference-world query points to source-world points.
`MappedSlicePlan` materializes nonlinear pullback coordinates once, then uses
the same typed interpolation kernels as an affine slice.

## Cache and profiling receipts

Caching is explicit and immutable:

```scala
val cold = ViewerCompiler.compileCached(
  model,
  session.state,
  session.device,
  ViewerCache.empty(capacity = 24)
).toOption.get

val warm = ViewerCompiler.compileCached(
  model,
  session.state,
  session.device,
  cold.cache
).toOption.get
```

`ViewerProfile` reports panel count, layer requests, cache hits/misses, and
sampled pixel work. It deliberately excludes wall-clock time, keeping receipts
portable and deterministic. A cache is scoped to one stable `ViewerModel`.
`ViewLink` separately synchronizes declared cursor, timepoint, and convention
properties between viewer states.

## Platform hosts

- `CanvasViewerHost` compiles and draws through `graphics-canvas`; DOM code
  supplies canvas-relative pointer coordinates and owns listeners/lifecycle.
- `Java2DViewerHost` draws into a supplied `Graphics2D` and can produce a
  `BufferedImage` directly.
- `JavaFxViewerHost` draws through `JavaFxGraphicsContext` or a live JavaFX
  `GraphicsContext`; the application owns the JavaFX thread, stage, and canvas.

Hosts do not reinterpret anatomy, resample data, or mutate viewer state. They
only render a shared frame and translate device events into shared actions.
