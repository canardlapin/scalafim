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

Render-plan revisions 1 and 2 have no culling field, so their portable default is
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

Vertex constructors (`scalar`, `labels`, `mask`, `packedRgba`) retain their
existing behavior. For measurements on triangles, first construct an owned
`SurfaceFaceField` and use `faceScalar`, `faceLabels`, `faceMask`, or
`facePackedRgba`. A face field validates `faceCount * frameCount` values in
frame-major order and retains the exact ordered mesh association:

```scala
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

// geometry and surfaceId belong to the surface being displayed.
val layer = SurfaceFaceField.make(geometry, faceValues).map: field =>
  SurfaceLayer.faceScalar(
    SurfaceLayerId.unsafe("face-statistic"),
    surfaceId,
    field,
    ScalarColorizer(DisplayWindow.unsafe(-3.0, 3.0))
  )
```

Face samples remain constant within their triangles before lighting. They are
never averaged onto scientific vertices. The current compiler uses separate
render corners for any asset containing a face layer, including hidden layers,
so visibility and timepoint changes do not change render topology. This costs
three render vertices per face; `SurfaceProfile` counts the expanded buffers
and source-ID map. Original normals, scientific vertex IDs, and face order
survive expansion and coordinate morphs. JavaFX, Three.js, and the portable
raster consume the same compiled representation.

`SelectFace(surface, face, vertex)` requires the vertex to belong to the face.
Its readout reports the face sample and the selected vertex's world coordinates;
vertex layers retain nearest-vertex readouts. A vertex-only selection does not
invent a face value at a shared vertex. JavaFX clicks dispatch face selections;
browser hosts can dispatch the same action from the returned pick IDs. Native
Three.js picks include original barycentric coordinates. Rays follow the compiled
projection matrix for both orthographic and perspective views. For partitioned
surfaces, the native raycast uses original triangles with the same live position
buffer, avoiding artificial pick gaps at the display subdivision seams. Its CPU
index buffer uses twelve bytes per original face (plus any attached network faces).

Vertex constructors preserve their existing color-interpolated appearance by
default. To display discrete regions, pass an explicit policy:

```scala
SurfaceLayer.labels(id, surfaceId, geometry, labels, colorizer,
  interpolation = SurfaceVertexInterpolation.NearestSample)
```

`scalar`, `mask`, and `packedRgba` accept the same typed vertex policy. Nearest
sampling chooses the largest original barycentric weight. Exact ties in the
reference evaluator and picks choose the smallest original vertex ID. These
regions are a display convention, not inferred anatomical parcel boundaries.

All three backends use six render triangles per scientific face: edge midpoints
and the centroid divide each corner's region into two triangles. The shared
representation has eighteen separate render corners per face, interpolated
original normals, and original face provenance. `SurfaceProfile` includes all
expanded buffers. Time, visibility, and style changes retain this topology;
coordinate morphs update its positions and normals. Native rasterization may
antialias exact region boundaries; unlit interiors remain palette colors within
one 8-bit channel value.

Nearest layers can compose with face-constant, interpolated-color, and scalar
layers. The portable fragment evaluator and Three.js shaders evaluate each
layer's policy before ordered opacity and blending. Pure vertex-color scenes
retain their existing render-corner composition contract. Automatic legend
requests bind to these effective mappings and reserve measured publication space
beside the surface.

For continuous scalar fields, `SurfaceLayer.interpolatedScalar` accepts raw
vertex values and an inspectable Intaglio `ScalarMapping`. The portable raster
interpolates those values with perspective-correct barycentric coordinates,
then applies visibility, normalization, the color ramp, opacity, and blending.
Multiple layers and declared world-space directional lighting are supported.
Three.js lowers inspectable mappings to fragment shaders, with up to eight
layers and 1,024 ramp stops per scalar layer, subject to native device limits.
It rejects mappings whose distinct boundaries collapse at GPU float precision
before native uploads. The original scalar samples remain in Double precision
in the shared plan. Fragment attributes, precomposed colors, and world normals use centroid
interpolation so partially covered multisample pixels cannot extrapolate scalar
values or normals outside their triangle. Antialiasing can still mix colors from
different visible faces at a pixel; exact mapping boundaries remain reference
semantics rather than a claim of identical edge pixels across devices.

Runtime creation also checks native depth ordering with a small offscreen
fixture before uploading any surface. A pipeline that exposes the farther
triangle returns `ThreeSurfaceError.NativeDepthMismatch`. Chromium 151
SwiftShader with four-sample antialiasing fails this check; Apple M3 Max Metal
passes. The [independent depth receipt](benchmarks/receipts/surface-three-depth-admission-2026-09-08.json)
also verifies the non-antialiased software control. This is a measured capability
check, not a driver-name blacklist. The
reference raster is a fallback. Hosts can also create a fresh WebGL2 canvas
without antialiasing before passing it to `ThreeJsRuntime.create`; admission
still runs on that context. The final browser receipt records the tested
renderer and sample count.

Fractional fitted viewports retain their logical pixel positions in WebGL.
The adapter compensates the projection for integral device viewport bounds,
keeping native display coordinates aligned with reference pixels and picking.
This correction preserves homogeneous depth and the compiled camera used for
scientific rays.

The native renderer also expands the occupied geometry interval across the
available depth buffer. This improves separation of nearby cortical faces when
the requested camera range is much wider than the anatomy. All viewports share
one depth transform; screen coordinates, depth order, and the scientific camera
remain unchanged. Bounds include attached network geometry and refresh after
morphs. A conservative float envelope retains clipping planes reached by the
geometry; eye-plane crossings or unusable bounds keep the original projection.
This reduces depth quantization error without promising exact visibility for
arbitrarily close surfaces on every graphics device.

JavaFX supports this composition through the explicitly approximate
`JavaFxSurfaceBackend.createApproximate` factory. Its bounded geometric fallback
has declared color-error, subdivision, and memory limits; the default backend
continues to reject fragment plans. Large or steep maps can exceed these budgets.
The opt-in factory also subdivides varying legacy corner colors, including lit
facewise and nearest-sample maps, to avoid atlas filtering across unrelated
triangles when zoomed out. It preserves their existing corner composition and
lighting order. Coincident display corners share subdivision edges while keeping
their distinct colors and original scientific IDs. Lit legacy colors require an
error budget of at least two channel values: one is reserved for rounding shaded
corners, and the rest bounds subdivision error. Constant unlit categories retain
the existing mesh. The [cortical review](plans/surface-cortical-semantics-review.md)
records native comparisons, subpixel coverage checks, and measured memory costs.
See the [JavaFX implementation review](plans/surface-bounded-javafx-review.md) and
[Three.js shader review](plans/surface-three-fragments-review.md) for native
coverage, resource costs, and completed cortical-scale acceptance.

```scala
val layer = SurfaceLayer.interpolatedScalar(
  layerId, surfaceId, geometry, values, mapping
)
```

Compiled scalar packets retain owned `Double` samples indexed by original
vertex. Original faces and pick identities survive clipping. A nonfinite sample
invalidates an interior where it contributes; it has no effect on its exact
opposite edge. There is no renormalization over the remaining finite samples.
Hidden fragments reveal underlying layers or the base surface. Readouts retain
their existing nearest-vertex sample meaning. Appended network geometry has a
separate face-coverage domain, so missing scalar colors cannot leak onto tubes.
The [reference and JavaFX feasibility review](plans/surface-scalar-fragments-review.md)
records the earlier implementation and experimental lookup results.

Every layer is tied to one exact surface domain. Domain equality includes
hemisphere, vertex count/order, face count, ordered topology, and the coordinate
transform where the operation requires it. Invalid cross-surface reuse fails at
construction rather than being repaired by a renderer.

Layer presentation contains visibility, opacity, blend mode, and threshold.
Layer order is an explicit state vector and therefore deterministic. A
transparent-band threshold hides only values strictly inside the finite band;
boundary values remain visible, matching the image-view/neuroimjs contract.
Thresholding is independent from the display window.

Scalar layers expose an optional Intaglio `ScalarMapping`. It describes
sequential, continuous diverging, or split-tail scaling, piecewise ramp stops,
visibility endpoints, hidden/invalid colors, and clamp/hide behavior. Supply
`mapping.colorizer` to either the vertex or face scalar constructor. Existing
`ScalarColorizer` instances are inspectable through a compatibility adapter;
arbitrary callbacks retain only the adjustment capabilities they advertise.

`layer.effectiveScalarMapping(presentation)` resolves checked window and
threshold overrides. Split cutoffs and the diverging center remain absolute
scalar values, so a window that excludes them is rejected. A disabled threshold
disables separate visibility filtering but leaves a split scale's intrinsic gap.
Compiled layer packets carry the effective descriptor and resource keys include
its identity. Colors remain unlit mapping colors before layer composition and
lighting. See the [mapping implementation and comparison plate](plans/surface-scalar-mapping-review.md).

`SurfaceLegendRequest` provides `Continuous`, `Split`, `Categorical`, and
`Manual` alternatives. Automatic requests name one layer with
`SurfaceLegendLayers.one(id)`, or a validated set for a shared legend. They
accept an Intaglio `LegendTitle` (quantity and optional units), but no independently
editable limits or colors. `SurfaceLegend.bind(request, model, state)` resolves
the current window and threshold, validates custom ticks, and returns a typed
error for opaque callbacks or differing effective mappings. Empty ticks select
mapping-derived ticks. Continuous requests accept sequential and diverging
scales; split requests require a split mapping.

Categorical requests require an inspectable `LabelColorizer`, names for every
palette key, and face-constant or nearest-sample interpolation. The resulting
entries retain original category IDs, exact palette colors, and a separate
fallback key. Interpolated category colors cannot masquerade as discrete
categories. A requested layer must be visible and present in the current layout
and layer order. Manual requests remain explicit, independent color keys.

Bound legends carry an identity covering their mapping, metadata, source layers,
and interpolation. `legend.validateCurrent(model, state)` rejects a stale
snapshot; binding its request again produces the updated legend. Camera,
lighting, and opacity do not change its calibration identity. Automatic legends
describe unlit mapping colors; opacity and compositing can change displayed
pixels. Scalar legend content delegates ticks and bar sampling to Intaglio's
`ScalarLegend`, whose drawing API is available through `ScalarLegendDrawing`.
`SurfaceLegendPublication.prepare` places measured cards beside the surface;
existing `SurfacePublication.decorate` manual swatches retain their output.

Dynamic fields expose a validated time axis or a bounded frame source.
`SurfacePlayback` resolves nearest/linear samples through an immutable LRU and
materializes an ordinary scalar layer. Timepoint changes preserve source mesh keys. Exact retained paths update only
layer resources; the opt-in bounded JavaFX approximation may repartition its
display mesh and upload new geometry and atlases when values change.

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
coordinate revisions have separate resource keys. The exact JavaFX path mutates
retained `TriangleMesh` point and normal buffers; Three.js mutates retained
`position` and `normal` attributes and refreshes its bounding sphere. These
paths retain indices, colors, materials, and texture atlases during a morph.
The opt-in bounded JavaFX path may rebuild its display mesh and atlas to retain
its declared color error bound; native update receipts record those uploads.

The camera anchor remains at the family's canonical/default geometry, so an
ordinary morph does not recenter or refit the camera. Explicit `FitCamera` uses
the currently displayed geometry's transformed bounds, including intermediate
morph positions, while retaining that anchor and the viewing direction. Use it
to bring an expanded or translated presentation back into view.

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
unsupported feature. `SurfaceViewerAction.SetClipping(SurfaceClipping.NearFar(near, far))`
sets positive eye-space depth limits in both orthographic and perspective views.
The shared compiler applies these limits to the projection matrix; pixel coverage
and scientific picks use the same clipped depth interval. Changing the range
preserves mesh and layer resources.

JavaFX orthographic rendering maps the interval into `ParallelCamera`'s
viewport-sized depth volume and updates that transform on resize. The
[depth-clipping receipt](benchmarks/receipts/surface-depth-clipping-2026-09-07.json)
records 24 native near/far, resize, and restoration cases. It separately records
the legacy texture-color approximation; bounded scalar output stays within two
channel values of the reference in that fixture.

Publication presets pin dimensions, margins, font sizes, background, title,
orientation, and legend. 2D chrome is a normal Intaglio `Scene`, so the same
Grob tree can be composed over a surface snapshot and rendered with Canvas,
Java2D, JavaFX, or SVG.

For mapping-derived legends, prepare the page before rendering the surface:

```scala
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

val title = LegendTitle.make("Contrast", Some("percent signal change")).toOption.get
val request = SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(layerId), title)
val spec = SurfacePublicationSpec(
  SurfacePublicationPreset.ManuscriptSingleColumn,
  Some("Task response"),
  SurfaceOrientationMark.LeftLateral
)
val prepared = SurfaceLegendPublication.prepare(model, state, spec, Vector(request))
val artifact = prepared.toOption.get.renderWith: input =>
  SurfaceRasterizer.render(input.plan, input.dimensions,
    SurfaceRasterStyle(background = input.background)).map(_.image)
```

The returned artifact contains an Intaglio `Scene` and a receipt identifying the
surface resources, camera, timepoint, legend mappings, source interpolation
policies, and physical layout. `renderWith` supplies the prepared plan and exact
surface dimensions to the backend, propagates rendering errors, and rejects a
wrong-sized image. The backend remains responsible for rendering those inputs.
The page reserves a right-hand legend column; an empty legend list releases that
space. Excessively tall stacks return a typed error before pixel generation.

Continuous and split cards use Intaglio's scalar drawing; categorical and manual
cards use its wrapped swatch drawing. Long quantity names, units, and entry
labels wrap within their cards. Scalar bars use 512-sample nearest-neighbor
strips partitioned at mapping boundaries; this is a finite display approximation.
Fonts use estimated metrics by default, with a `TextMetrics` override available.
Legacy swatches supplied to `prepare` become an explicit manual card; the older
`decorate` API keeps its original layout. See the
[publication gallery and native checks](plans/surface-publication-legend-review.md).

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

The build defaults to OpenJFX 21.0.5. To select OpenJFX 24.0.2 for both JavaFX
hosts and the JVM surface examples, start sbt with:

```sh
sbt -Dscalafim.javafx.version=24.0.2 imageViewJavafxJVM/test surfaceViewJavafxJVM/test surfaceViewExamplesJVM/test
```

JavaFX 24 requires JDK 22 or newer. Keep all application OpenJFX artifacts on
the same version and platform classifier. The selection does not change the
published backends' `Provided` dependency scope. On JDK 24 or newer, enable
native access in the application JVM: `--enable-native-access=ALL-UNNAMED`
for a classpath launch, or `--enable-native-access=javafx.graphics` for a
module-path launch using these graphics modules. See the
[JavaFX 24 release notes](https://openjfx.io/highlights/24/).

Each logical viewport renders into its own retained 3D `SubScene`, using
`PerspectiveCamera` for perspective plans and `ParallelCamera` for orthographic
plans. The adapter clips that completed depth pass to its viewport, so anatomy
cannot spill into letterboxing or the other hemisphere's slot. Clipping the
rendered SubScene preserves depth testing between its mesh chunks; clipping the
3D Group directly would lose that ordering.

The returned outer `SubScene` uses a parallel camera to compose these images.
Change anatomical camera state through the controller; its projection action
switches the inner cameras without rebuilding mesh or atlas resources. Native
picks still reach the original mesh nodes through the nested scenes; empty
backgrounds remain pick misses. Perspective rendering uses an equivalent default-eye
coordinate transform so nested `localToScreen` queries remain valid on OpenJFX 21.
Resizing
updates the inner render targets, viewport rectangles, and transforms together;
rebuilding or disposing detaches their cameras and property listeners.

A backend owns one mounted SubScene. `snapshot` reuses that mount, temporarily
applies the requested size, antialiasing, and background, then restores the live
view. Without a mount it creates and releases a temporary scene. Asking the same
backend for a second simultaneous mount returns a typed incompatibility; use a
separate backend for another live view.

Inner targets currently retain the full canvas extent to preserve native pixel
alignment across fractional viewport boundaries. This uses one depth-enabled
render target per visible surface and adds GPU memory beyond the primitive
mesh/atlas byte receipts. Orthographic coordinates remain an exact parallel
projection, rather than a narrow perspective approximation.

JavaFX Scene3D lacks a portable integer vertex attribute and programmable blend
pipeline. ScalaFIM packs per-layer colors into texture atlases. Exact paths retain mesh
resources during color updates; bounded approximation paths may repartition the
display mesh and rebuild atlases as described above. The backend validates atlas dimensions
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

`SurfaceSceneDocument` revision 6 records external URI/SHA-256 references,
exact asset/layer identity, layout, camera, lighting, clipping, timepoint,
selection (including an optional face), vertex/face layer association and vertex interpolation policy,
presentation/order, required neutral capabilities, and sorted
provenance. Scalar layers also record their base mapping identity, checked
against the supplied model during restoration; presentation overrides remain
separate. Payload arrays and executable mapping definitions remain external. `SurfaceSceneCodec` emits canonical
JSON on JVM and JS, rejects duplicate/unknown fields by default, and never
serializes JavaFX or Three.js objects.

Revision-1 documents decode as vertex data with legacy color interpolation;
revision 2 retains face association and selection while using legacy vertex
interpolation. Revision 3 retains nearest-sample policies without the
mapping-identity check. Revision 4 adds mapping identity; revision 5 records
scalar interpolation and requires the corresponding backend capability. Face and nearest-sample documents infer `FacewiseData` and
`NearestVertexSampling` requirements respectively. The render-plan boundary is
revision 7; its color arrays remain indexed by render vertex, with scientific
association, interpolation policy, and partition provenance recorded separately.

Revision 6 adds legend requests and their effective identities. Pass
`legendRequests` to `SurfaceSceneDocument.capture`; restoration binds each
request against the restored presentation and rejects a stale identity, including
changed category palettes or fallback colors. The legend requests remain on the
document for subsequent publication or rebinding. Earlier revisions preserve
their JSON shape and contain no legend field.

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

The dedicated face-boundary gate runs with:

```sh
sbt "surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceFaceProbe"
```

The browser admission page includes `runScalafimThreeFaceProbe`, which reads
actual WebGL pixels, checks source vertex IDs, and verifies that a timepoint
change uploads colors without geometry. The JavaFX gate checks a mounted native
snapshot against the raster, plus translation of a JavaFX `PickResult` back to
the original face and vertex. Its pick input is constructed; the browser probe
uses real ray intersections.

See the [facewise validation receipt](benchmarks/receipts/surface-facewise-2026-09-07.json)
and its [JavaFX](visual-qa/surface-facewise-javafx.png) and
[WebGL](visual-qa/surface-facewise-three.png) images for the initial analytic
fixture. The receipt distinguishes these passing checks from the pre-existing
vertex-example checksum failure.

The [nearest-sample receipt](benchmarks/receipts/surface-nearest-2026-09-07.json)
records 455 passing conformance tests, final focused checks, and native images
([JavaFX](visual-qa/surface-nearest-javafx.png), [WebGL](visual-qa/surface-nearest-three.png)).
JavaFX checked 29,238 interior pixels with maximum channel error 1; WebGL checked
179 with error 0 and verified 174 oblique perspective picks by reprojection.
The native JavaFX pick input is constructed; WebGL uses actual ray intersections.
Both verified geometry reuse on timepoint changes. These are the initial analytic
checks. The [completion audit](plans/surface-map-completion.md) records subsequent
native JavaFX ray traversal, cortical updates, and repeated large-mesh admission.

The [fragment shader review](plans/surface-three-fragments-review.md) records
548 passing conformance tests and native Three.js checks for 64 composition
cases, 28 scalar edge cases, and four depth-clipping cases. The scalar checks
include split/diverging scales, invalid values, varying normals, and a narrow
range around a large offset with attached network geometry. Native comparisons
exclude declared mapping and silhouette boundary bands affected by antialiasing.

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

The legacy vertex-color matrix covers 32,768 and 163,842 vertices, 1/4/8 layers, and cold,
camera, style, data, timepoint, pick, resize, and snapshot paths. Structural
admission is exact: camera updates upload no mesh/layer data; style/data/time
updates upload no meshes. Timings are hardware receipts, not universal limits.
See [`benchmarks/surface-viewer.md`](benchmarks/surface-viewer.md).

The new map-policy matrix tests scalar, folding-layered, curvature-layered,
nearest and facewise modes at both sizes, with both projections and lighting
states, three repetitions, reference pixels and actual native picks. The
[32,768-vertex receipt](benchmarks/receipts/surface-map-benchmark-32768-2026-09-08.json)
records 516 rendered stages; the
[163,842-vertex receipt](benchmarks/receipts/surface-map-benchmark-163842-2026-09-08.json)
records 354 rendered stages and 18 checked rejections. The tested approximation
profile allows four channel values of error, three million generated triangles,
1 GiB of accounted generated resources, 24 subdivision levels and 64 cuts per
face. Its limits exclude JVM objects, driver allocations and render targets.

The larger synthetic grid's lit scalar modes exceed the subdivision-depth budget
on its final face and are rejected before native allocation. This is a measured
boundary for that fixture, not a vertex-count limit for every surface. Large lit
nearest updates can take several seconds. The receipts separate update, native
paint and pick times; rendering acceptance does not promise interactive latency.
These ellipsoid workloads complement the anatomical cortical fixtures.

Run this opt-in native matrix with:

```sh
sbt 'surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceMapBenchmark /tmp/surface-map-admission 3'
```

## Cortical WebGL validation

The host-local cortical probe uses the existing FreeSurfer readers to export the
pinned left fsaverage6 pial, white, inflated, and sulcal-depth inputs. It checks
five map modes at 384, 768, and 1024 pixels, both projections and lighting states,
then retained camera, frame, opacity, threshold, morph, geometry, and resize
updates. Each stage checks actual framebuffer pixels and 64 original-ID rays.
The synthetic overlay and category bands are display fixtures, not estimated
activation or anatomical parcels.

The [final native receipt](benchmarks/receipts/surface-cortical-webgl-2026-09-08.json)
passes all 252 stages and 16,128 picks on Chromium 151 / Apple M3 Max Metal.
Across 10.59 million strict interior pixels, maximum channel error is 3 and
minimum mask IoU is 0.9928 (required: at least 0.99). The
[image gallery](visual-qa/surface-cortical-webgl/README.md) retains pial and inflated
views of all five modes. Two focused non-antialiased SwiftShader checks also
pass; the receipt keeps that narrower scope distinct from the full Metal matrix.

Build the transport and classic-script browser bundle with:

```sh
sbt 'surfaceViewExamplesJVM/Test/runMain scalafim.examples.surfaceview.CorticalMapBrowserFixture CORPUS_DIRECTORY /tmp/cortical-fixture.json'
sbt 'set surfaceViewExamplesJS / scalaJSLinkerConfig ~= (_.withModuleKind(org.scalajs.linker.interface.ModuleKind.NoModule))' surfaceViewExamplesJS/fastLinkJS
```

Supply a project Playwright/Three.js installation and its installed browser:

```sh
PLAYWRIGHT_BROWSERS_PATH=BROWSER_INSTALL node examples/surface-view/browser/cortical-maps.mjs /tmp/cortical-fixture.json /tmp/cortical-webgl PROJECT_NODE_MODULES Scalar,Layered,CurvatureLayered,Nearest,Face '' all metal
```

`metal` selects ANGLE Metal on macOS; `default` uses Chromium's selected backend.
`no-antialias` creates a non-antialiased WebGL2 context before runtime admission.
Using `legacy` in place of the mode list runs the integrated 48-case admission,
face/nearest, composition, scalar-edge, and clipping harness. A nonempty
following argument selects one diagnostic stage and is recorded as a focused
check, not a complete matrix. Follow the browser ownership audits in `AGENTS.md`
before and after running the harness. The runner closes its own browser and
server on success or failure and records the renderer, source checksums,
linked-artifact hash, and resource disposal.

## Troubleshooting

- **Blank view:** confirm `surfaceToWorld`, camera direction, pan, projection
  field, and near/far range are expressed in one RAS+ world space. Do not erase
  a valid transform or flip triangle indices as a display workaround.
- **Wrong left/right or upside down:** inspect `Hemisphere`, `SurfaceViewpoint`,
  `BilateralOrder`, and the compiled camera direction. Backends must preserve the
  compiled camera coordinates and clipping semantics.
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
