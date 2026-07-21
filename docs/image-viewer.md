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
slice step, window, threshold, opacity, visibility, timepoint, resize,
crosshair, and orientation labels. Thresholding is distinct from windowing:
`DisplayThreshold.TransparentBand` makes only the strict interior of a finite
band transparent, matching neuroimjs, while `DisplayThreshold.Disabled`
explicitly preserves every finite value.

```scala
val action = ViewerEvents.pick(frame, deviceX = 240.0, deviceY = 160.0)
val next = action.flatMap(ViewerReducer.reduce(model, session, _))
```

Device input is y-down; panel receipts convert it to root NPC and then to the
top-to-bottom slice grid. Scrolling follows each plane's positive anatomical
normal and is independent of screen mirroring.

## Navigation, layouts, and readouts

Each anatomical panel has an independent, checked `PanelView`. Its `ZoomLevel`
is at least one and its normalized center is constrained so panning cannot move
outside the sampled image. `SetPanelView` and `ResetPanelView` are pure viewer
actions. Rendering, crosshairs, and inverse picking all use the same transform,
so a backend does not need to reimplement navigation geometry.

`OrthogonalLayout` supports the default L-shaped arrangement as well as checked
single-row and single-column arrangements. Physical slice aspect is preserved
inside every cell on every device size.

Every compiled `ViewerFrame` also carries a `PanelReadout` for each anatomical
panel. Readout values are typed as scalar, label, or mask samples and include
both world and reference-voxel coordinates. They are derived from the sampled
slice already used for rendering; a raster-cache hit never forces a new source
read merely to populate a readout.

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

`ViewerCache` retains sampled scalar slices separately from colorized rasters.
Changing a display window or threshold therefore recolorizes cached values
without reading or sampling the source volume. Raster cache identity includes
both presentation values, while sampling identity includes the plane-normal
position and covering-grid geometry but excludes in-plane cursor position, so
moving an axial cursor reuses sagittal and coronal rasters while their
crosshairs move.

`ViewerProfile` reports raster and sampled-slice hits/misses, sampled pixels,
colorized pixels, and source-frame reads. A compilation resolves each visible
layer/timepoint at most once even when all three anatomical planes miss the
slice cache. It deliberately excludes wall-clock time, keeping these work
receipts portable and deterministic. A cache is scoped to one stable
`ViewerModel`. `ViewLink` separately synchronizes declared cursor, timepoint,
and convention properties between viewer states.

The opt-in real-browser timing harness and its non-timing acceptance contract
are documented in [`benchmarks/image-view-browser.md`](benchmarks/image-view-browser.md).

## Platform hosts

- `CanvasViewerHost` compiles and draws through `graphics-canvas`; DOM code
  supplies canvas-relative pointer coordinates and owns listeners/lifecycle.
  Interactive clients should create one `CanvasViewerRuntime` with
  `CanvasViewerHost.runtime()` and retain it across frames. The runtime owns
  the immutable viewer-cache state plus a bounded browser-native raster cache;
  each render returns `ViewerProfile` and `CanvasDrawProfile` receipts,
  including uploaded bytes.
  `CanvasViewerHost.controller(...)` is the higher-level application contract:
  it owns one model/session/runtime, exposes reducer-backed pick, scroll, and
  dispatch bindings, supports exact session snapshot/restore, and rejects work
  after `close()`. It deliberately retains neither a DOM node nor a Canvas
  context, so applications still own listener installation and teardown.
  `CanvasScrollCoordinator` can coalesce wheel bursts on an application
  scheduler such as `CanvasTaskScheduler.AnimationFrame`, and
  `prefetchSlices` can warm only the immediately adjacent slices during idle
  time. Both paths still reduce through the synchronous reference state
  machine; prefetch never mutates visible state or uploads native rasters.
- `Java2DViewerHost` draws into a supplied `Graphics2D` and can produce a
  `BufferedImage` directly.
- `JavaFxViewerHost` draws through `JavaFxGraphicsContext` or a live JavaFX
  `GraphicsContext`; the application owns the JavaFX thread, stage, and canvas.

Hosts do not reinterpret anatomy, resample data, or mutate viewer state. They
only render a shared frame and translate device events into shared actions.
