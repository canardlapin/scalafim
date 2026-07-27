# Surface viewer infrastructure

ScalaFIM surface display is one typed scientific model with three interpreters:
a deterministic CPU raster on JVM and Scala.js, JavaFX Scene3D on JVM, and
Three.js/WebGL in the browser. The concrete backends consume the same
`SurfaceRenderPlan`; they do not own layer, threshold, orientation, projection,
or picking semantics.

## Architecture

```text
surface geometry + fields          image volume       connectivity result
             |                         |                       |
             +---------- surface-view model/compiler ----------+
                                      |
                              SurfaceRenderPlan
                         /             |              \
             reference raster      JavaFX Scene3D       Three.js/WebGL
                 JVM + JS                JVM                 Scala.js
```

`surface-view` depends on the existing `surface` core and standalone Intaglio.
It uses Intaglio for renderer-neutral chrome (`Scene`, `Grob`,
text, orientation marks, legends, and publication composition). Mesh triangles
are deliberately a separate 3D primitive stream: forcing them through the 2D
Grob algebra would discard depth, culling, lighting, and native picking.

The split is intentional:

- `surface-view` owns valid models, immutable state/reducer actions, compilation,
  resource identities, backend capabilities, serialization, and conformance;
- `surface-view-raster` is the deterministic pixel and pick oracle;
- `surface-view-javafx` owns only JavaFX resource interpretation and controller
  bindings;
- `surface-view-three` owns only injected Three.js/WebGL interpretation;
- `surface-view-connectivity` adapts connectivity edge spaces without making the
  core renderer depend on an estimator module.

## Minimal shared viewer

```scala
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

val geometry: SurfaceGeometry = ???
val values: Array[Double] = ???

val surfaceId = SurfaceId.unsafe("left-midthickness")
val layerId = SurfaceLayerId.unsafe("activation")

val layer = SurfaceLayer.scalar(
  layerId,
  surfaceId,
  geometry,
  values,
  ScalarColorizer(DisplayWindow.unsafe(-5.0, 5.0), ColorRamp.Heat)
).fold(error => throw new IllegalArgumentException(error.message), identity)

val model = SurfaceViewerModel.make(
  Vector(SurfaceAsset.make(surfaceId, geometry).toOption.get),
  Vector(layer)
).fold(error => throw new IllegalArgumentException(error.message), identity)

val threshold = DisplayThreshold.transparentBand(-2.0, 2.0)
  .fold(error => throw new IllegalArgumentException(error.message), identity)
val state = SurfaceViewer.reduce(
  model,
  SurfaceViewerState.initial(model),
  SurfaceViewerAction.SetLayerThreshold(layerId, threshold)
).fold(error => throw new IllegalArgumentException(error.message), identity)

val plan = SurfaceCompiler.compile(model, state)
  .fold(error => throw new IllegalArgumentException(error.message), identity)
```

Scalar, label, mask, packed-RGBA, curvature, projected-volume, annotation, and
connectivity-derived layers ultimately compile to primitive packed color buffers.
Backends therefore do not grow special scientific-data entry points.

## Coordinates and anatomical orientation

Surface coordinates are transformed by `SurfaceGeometry.surfaceToWorld` before
packing. World space follows the image contract: NIfTI RAS+, with +x right, +y
anterior, and +z superior. `SurfaceViewpoint` defines camera direction exactly:

| View | Camera position relative to target | What faces the observer |
| --- | --- | --- |
| left lateral | -x | left hemisphere lateral wall |
| left medial | +x | left hemisphere medial wall |
| right lateral | +x | right hemisphere lateral wall |
| right medial | -x | right hemisphere medial wall |
| anterior | +y | frontal/anterior surface |
| posterior | -y | posterior surface |
| dorsal | +z | superior surface |
| ventral | -z | inferior surface |

These names describe anatomy, not array order. A backend receives the compiled
view/projection matrices and must not introduce its own sign flip. Bilateral
layout validates that the left slot contains left-hemisphere geometry and the
right slot contains right-hemisphere geometry. `BilateralOrder` states screen
order explicitly, and publication chrome records an `L ... R` orientation mark.

Real hemisphere files retain their native RAS displacement. The compiler
therefore attaches a display-only world offset to each bilateral slot so each
independently stored hemisphere is centered in its own viewport. Raster,
JavaFX, and Three.js interpret the same offset. Mesh buffers, scientific world
coordinates, clipping inputs, readouts, and returned pick coordinates remain
unchanged. The production fsaverage5 gate exists because duplicating one tiny
mesh into both slots cannot reveal this centering error.

Winding remains the mesh's ordered triangle topology. Back-, front-, or
two-sided culling is explicit; a backend may not silently reverse indices to
make a surface appear. The asymmetric raster fixtures, native pick fixtures,
and browser/JavaFX conformance matrix catch swapped hemispheres, vertical flips,
reversed camera directions, and winding changes.

Render-plan revision 1 has no culling field, so its portable default is
two-sided. JavaFX uses `CullFace.NONE` and Three.js uses `DoubleSide` to match
the raster oracle. This matters for external GIFTI/FreeSurfer winding: inventing
a native front-side convention produced large anatomical holes while still
passing the tetrahedron fixture. A future plan revision may add a typed culling
policy; native back-face culling is not an implicit default today.

Camera pan values and projection scale are world-unit values. The compiler aims
the camera at the canonical geometry's transformed world-space bounds and puts
the eye outside that geometry's bounding sphere. Camera direction therefore has
the same anatomical meaning for unit fixtures and millimetre-space GIFTI meshes;
the camera cannot sit inside a cortex and expose the opposite wall. The center
and radius are part of the camera resource identity. The checked example
intentionally uses a `(10, 20, 30)` GIFTI transform and a dorsal view rather
than erasing the transform. Cross-platform metamorphic tests require a pure
world translation to preserve exact raster pixels and picks.

## Layers, thresholds, and time

Every layer is tied to one exact surface domain. Domain equality includes
hemisphere, vertex count/order, face count, ordered topology, and the coordinate
transform where the operation requires it. Invalid cross-surface reuse fails at
construction rather than being repaired by a renderer.

Layer presentation contains visibility, opacity, blend mode, and threshold.
Layer order is an explicit state vector and therefore deterministic. A
transparent-band threshold hides only values strictly inside the finite band;
boundary values remain visible, matching the image-view/neuroimjs contract.
Thresholding is independent from the display window.

Dynamic fields expose a validated time axis or a bounded frame source.
`SurfacePlayback` resolves nearest/linear samples through an immutable LRU and
materializes an ordinary scalar layer. Timepoint changes update only layer
resources: mesh keys remain stable and admission rejects geometry uploads.

Each `SurfaceAsset` may own a topology-locked `SurfaceSet` containing named
white, smooth-white, pial, midthickness, inflated, spherical, or custom
geometry. `SurfaceViewerState` stores either a fixed `SurfaceKind` or a typed
`Morphing(from, to, fraction)` presentation for every asset. State changes do
not replace the mesh domain: layers, thresholds, vertex ids, selection, and
readouts survive the transition. Starting a morph toward its source reverses
the active transition without a coordinate discontinuity; completing fraction
one settles to the target state.

The compiler interpolates directly into the packed float position buffer,
recomputes normals, and reuses the asset's cached index buffer. Topology and
coordinate revisions have separate resource keys. JavaFX mutates the retained
`TriangleMesh` point and normal buffers; Three.js mutates retained `position`
and `normal` attributes and refreshes its bounding sphere. Neither path uploads
indices, colors, materials, or texture atlases during a morph. Camera framing
is anchored to the family's canonical/default geometry, preventing per-frame
recentring without letting a distant inflated or spherical state displace the
folded anatomy before a transition starts.

### Geodesic reveal lens

`SurfaceGeodesicLens` is an immutable topology-domain value: a pinned
`VertexId`, validated opaque inner/outer radii, and one precomputed geodesic
weight per vertex. Construction runs the shared surface Dijkstra once. Every
animation frame then performs only allocation-controlled interpolation in the
compiler's existing position-buffer loop. The falloff is one inside the inner
radius, zero at and beyond the outer radius, and quintic smootherstep between
them.

The viewer's default `PinnedHarmonic` policy first removes the target vertex's
translation so the selected point remains fixed. Target displacements are hard
constraints inside the core, zero is the hard constraint outside the outer
radius, and a portable Jacobi solve computes a discrete harmonic displacement
through the collar. This field is precomputed once when the lens is pinned and
stored as a `SurfaceLensDeformation`; the direct per-vertex correspondence
policy remains available as an explicit diagnostic oracle.

`SurfaceGeometryPresentation.RevealLens(from, to, deformation, fraction)`
preserves the same mesh, layers, thresholds, selection, camera, and scientific
vertex identity as a whole-surface morph. `BeginGeometryLens` pins or retargets
a lens; `SetGeometryMorphFraction` opens it; `ClearGeometryLens` returns exactly
to the source geometry. A global morph cannot begin while a lens is active, so
two incompatible geometry semantics never coexist implicitly.

At render time, only a vertex's interpolation fraction changes:

```text
position = source + animation fraction * precomputed displacement
```

Topology and layer resource keys remain stable. JavaFX updates one retained
point/normal buffer and zero atlases per frame; the retained Three.js path uses
the same geometry revision boundary. The real-cortex parity plate pins a
strongly displaced suprathreshold lateral vertex and opens a 12--48 mm patch,
making the anatomical effect visible while fixed-seed data thresholded at
`-2.33, 2.33` remains attached. `SurfaceLensQuality` records triangle
inversions over the full continuous path, area ratios, and maximum/p95 edge
strain at full opening. Natural deformation construction returns a typed error
rather than admit a field whose path inverts a triangle. The QA
plate adds a projected pin plus warm core and cool outer-boundary rings; these
guides are presentation annotations and do not contaminate scientific colors.
See
[`visual-qa/surface-lens-parity.md`](visual-qa/surface-lens-parity.md).

## Curvature, parcels, clipping, and publication

Curvature is an ordinary scalar underlay with a pinned neutral-gray colorizer.
It may color corresponding inflated geometry only when the cortical hemisphere,
vertex order, and ordered triangle topology agree. Parcel and ROI boundaries
reuse the surface module's topology edge table.

The reference raster implements exact world-plane clipping. JavaFX and Three.js
currently reject world-plane plans and advertise that limitation in
`SurfaceBackendCapabilities`; callers can choose the reference path or omit the
unsupported feature. Near/far camera clipping remains available.

Publication presets pin dimensions, margins, font sizes, background, title,
orientation, and legend. 2D chrome is a normal Intaglio `Scene`, so the same
Grob tree can be composed over a surface snapshot and rendered with Canvas,
Java2D, JavaFX, or SVG.

## Volume projection and networks

`SurfaceVolumeProjection` is the portable CPU path. It delegates geometry and
sampling policy to existing `VolToSurfMorphism`, `VolumeSurfaceSamplingPlan`,
and `NeuroSpace` types, and records requested/accepted samples, mask/quality
decisions, bytes, and elapsed time. The result becomes a scalar surface layer.

The browser backend has an optional WebGL2 float-texture midpoint/nearest
projector. Feature detection is explicit and unsupported policies return typed
errors. Its acceptance fixture compares values, accepted counts, and quality
flags with the CPU oracle before timing is considered.

Network nodes carry stable identity, surface/vertex, optional region, and world
position. Threshold, top-N, sign, and region filters compile first; retained
edges then become ordinary signed-color triangle tubes or lines. The optional
connectivity adapter preserves source edge-space ordering and provenance.

## Picking and interaction

Reference-raster picks record surface, face, nearest vertex, barycentric
coordinates, and depth. Native JavaFX and Three.js picks are translated back to
the same typed identity. A pick outside all surfaces is `None`, not a sentinel.

At an exact projected triangle edge, CPU fill rules and a continuous native ray
may select adjacent incident faces. Production acceptance therefore requires
the same surface and nearest vertex and requires the native face to be incident
to that reference vertex; both face ids remain in the receipt. Interior fixtures
away from an edge continue to require exact face parity.

The shared `SurfaceViewer.reduce` state machine owns view, projection, orbit,
pan, zoom, layer presentation/order, timepoint, clipping, and selection.
Controllers translate toolkit events into those actions and then recompile.
They do not mutate scientific state independently.

## Backend lifecycle

### JavaFX

Create `JavaFxSurfaceBackend` on a JavaFX-capable process, render a plan, obtain
a `SubScene`, and retain one `JavaFxSurfaceController`. The application owns the
JavaFX thread, `Stage`, `Scene`, event loop, and OpenJFX packaging. On window
close, dispose the controller and backend. JavaFX dependencies are `Provided`
in the backend module; an application must add matching `javafx-base` and
`javafx-graphics` platform artifacts.

The adapter uses `PerspectiveCamera` for perspective plans and a real
`ParallelCamera` for orthographic plans. Orthographic view coordinates are
mapped explicitly into the retained `SubScene` pixel extent and viewport; they
are not approximated with a narrow perspective field of view. Projection
changes switch the camera on every retained `SubScene` without rebuilding mesh
or atlas resources. This distinction is required for depth-varying cortical
meshes and exact bilateral placement of world-transformed GIFTI geometry.

JavaFX Scene3D lacks a portable integer vertex attribute and programmable blend
pipeline. ScalaFIM packs per-layer colors into retained texture atlases and
updates them without rebuilding meshes. The backend validates atlas dimensions
against JavaFX limits and returns a typed incompatibility rather than truncating
data. Large layer counts/vertex counts may make the atlas path less attractive;
use Three.js for browser GPU work or the reference raster for deterministic
offline output.

### Scala.js / Three.js

The host owns the canvas, DOM listeners, animation scheduling, and injected
Three.js namespace. `ThreeJsRuntime.create(three, canvas)` does not impose an npm
layout. Retain one backend/runtime so mesh buffers survive camera and layer
updates; call `dispose()` when the canvas is removed. Canvas resize is an
explicit backend action. Snapshot and pick work is on demand rather than an
unconditional animation loop.

### Reference raster

`SurfaceRasterizer.render` is pure apart from timing receipt collection. It is
portable and deterministic, but its O(pixels + triangles) CPU work is intended
for testing, snapshots, and fallback—not high-frame-rate interaction.

## Serialization

`SurfaceSceneDocument` revision 1 records external URI/SHA-256 references,
exact asset/layer identity, layout, camera, lighting, clipping, timepoint,
selection, presentation/order, required neutral capabilities, and sorted
provenance. Payload arrays remain external. `SurfaceSceneCodec` emits canonical
JSON on JVM and JS, rejects duplicate/unknown fields by default, and never
serializes JavaFX or Three.js objects.

Restoration validates resolved reference digests and model identities, replays
the reducer actions, and can then reproduce camera matrices, resource keys,
packed colors, slots, and draw passes exactly.

## Performance and correctness gates

Run the shared and backend conformance suites with:

```sh
sbt surfaceViewConformance
sbt surfaceViewExamplesJVM/test surfaceViewExamplesJS/test
```

Run the live JVM matrix on a machine with JavaFX Scene3D:

```sh
sbt surfaceViewAdmissionJVM
sbt surfaceViewVisualQaJVM
```

Link the browser examples and admission harness with:

```sh
sbt surfaceViewExamplesJS/fastLinkJS surfaceViewThreeJS/fastLinkJS
```

The live JavaFX and browser examples are executable visual oracles, not smoke
tests. Both load the same transformed, bilateral, layered, thresholded GIFTI
fixture; render at 960 x 540; and compare native pixels with the same shared CPU
raster. They fail on insufficient left/right coverage, mask IoU below 0.90,
centroid displacement above three pixels, mean interior channel error above 24,
or disagreement in the interior landmark's surface, face, or nearest vertex.
The browser path reads the actual WebGL framebuffer and flips WebGL's bottom-up
rows. The JavaFX path snapshots a mounted `SubScene`. Shared adversarial tests
prove that horizontal flips, vertical flips, and red/blue corruption violate
the policy. The fixture itself must shade at least 10,000 pixels at 320 x 180,
which prevents a nearly empty image from passing by background agreement.

The pinned matrix covers 32,768 and 163,842 vertices, 1/4/8 layers, and cold,
camera, style, data, timepoint, pick, resize, and snapshot paths. Structural
admission is exact: camera updates upload no mesh/layer data; style/data/time
updates upload no meshes. Timings are hardware receipts, not universal limits.
See [`benchmarks/surface-viewer.md`](benchmarks/surface-viewer.md).

## Troubleshooting

- **Blank view:** confirm `surfaceToWorld`, camera direction, pan, projection
  field, and near/far range are expressed in one RAS+ world space. Do not erase
  a valid transform or flip triangle indices as a display workaround.
- **Wrong left/right or upside down:** inspect `Hemisphere`, `SurfaceViewpoint`,
  `BilateralOrder`, and the compiled camera direction. Backends must use the
  plan matrices unchanged.
- **Everything thresholded:** threshold and window are distinct. Inspect the
  selected timepoint and remember transparent-band boundaries are inclusive on
  the visible side.
- **JavaFX reports no 3D support:** run with a display and a working Scene3D
  pipeline. Use the raster backend for headless deterministic output.
- **JavaFX atlas rejected:** reduce simultaneously visible layers, split the
  view, or choose Three.js/reference raster. Rejection is preferable to silent
  color corruption.
- **Browser renderer fails:** require WebGL for surface display and WebGL2 plus
  floating-point render targets for GPU volume projection. The CPU projection
  path remains available.
- **Camera motion reallocates meshes:** retain the same backend instance and
  verify resource keys. The admission gate treats such uploads as a failure.
- **Scene restore fails:** resolve the exact URI/digest pairs and construct the
  same topology/layer domains before replay. Unknown-field tolerance must be
  opted into explicitly.
