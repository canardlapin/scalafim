# Surface viewer conformance and performance

The surface viewer admits backends against one versioned render-plan contract.
Correctness is established by shared semantic fixtures and the deterministic
reference raster; performance receipts describe what happened on a particular
runtime and device. A fast result is not treated as correctness evidence, and a
timing recorded on one machine is not a universal threshold.

## Deterministic gate

Run the focused cross-platform gate with:

```sh
sbt surfaceViewConformance
```

This executes the typed model/compiler, raster oracle, JavaFX interpreter, and
Three.js interpreter suites. Shared suites run on both JVM and Scala.js. The
semantic matrix covers asymmetric orientation, winding and culling, depth,
scalar/label/mask/RGBA layers, sparse fields, threshold boundaries, blend order,
lit and unlit modes, bilateral layout, face and vertex picking, resizing, and
disposal.

Every backend publishes a stable `SurfaceBackendCapabilities` value. Unsupported
behavior is a named caveat. The current JavaFX and Three.js backends explicitly
declare world clipping unsupported; the reference raster implements world
planes and declares lighting unsupported.

## Anatomical and publication contracts

Curvature is stored as an ordinary `SurfaceField[Double]`. The
`SurfaceLayer.curvatureUnderlay` constructor applies the pinned neutral-gray
encoding and allows a field computed on folded geometry to color an inflated
surface only when hemisphere, vertex order, and ordered triangle topology match
exactly. Sparse or topology-mismatched curvature is rejected.

`SurfaceParcels.boundaryEdges` derives parcel and ROI outlines from the existing
`MeshTopology` edge table plus `LabeledSurface`; it does not retain a duplicate
graph. World clipping uses normalized `WorldClipPlane` values with explicit
positive/negative keep-side semantics. The deterministic raster performs exact
polygon clipping. JavaFX and Three.js reject world-plane plans before
interpretation until their native implementations are admitted.

Publication presets carry fixed pixel dimensions, font sizes, margins,
orientation marks, and legend entries. `SurfacePublication.decorate` records
the camera key, layer keys, timepoint, and chrome count. The title, orientation,
and legend are graphics `Grob` values in the plan's `Scene`, and
`SurfacePublication.compose` places a raster snapshot beneath that same vector
chrome for Canvas, Java2D, JavaFX, or SVG rendering.
`JavaFxSnapshotConfig.publication` and `ThreeCanvasSize.publication` translate
the same preset into exact backend dimensions; the receipt also pins the title,
orientation, legend labels, and background color.

## Temporal, morphing, and linked-view contracts

Dynamic values use a validated `SurfaceTimeAxis` of typed `SurfaceSeconds`.
Times are finite and strictly increasing; nearest and linear sampling produce an
explicit `SurfaceFrameSample(lowerFrame, upperFrame, alpha)`. A
`SurfaceTemporalField` can expose a `SurfaceMatrix[Double]` column-by-column or
delegate to a lazy `SurfaceDoubleFrameSource`. Every read is checked against the
mesh vertex count before it enters rendering.

`SurfaceFrameCache` is an immutable, capacity-bounded LRU. A capacity of zero
means read-through with no retention. Sampling and prefetching return receipts
that distinguish cache hits, misses, source reads, evictions, and prefetched
frames. `SurfacePlayback.prepareScalar` materializes the selected sample as an
ordinary scalar `SurfaceLayer`, so thresholds, ordering, backend updates, and
resource receipts remain on the same rendering path. Its playback receipt also
states whether the sample changed and therefore requires a layer upload.

`SurfacePlayback` is a pure reducer over play, pause, seek, step, rate, looping,
and elapsed-time actions. Prefetch direction follows the signed playback rate;
the cache capacity remains the hard retention bound.

Morphing accepts only geometry with the same cortical hemisphere, vertex order,
ordered triangle topology, and `surfaceToWorld` transform. `SurfaceSet` now
enforces all four conditions at family construction, including key/kind
agreement and unique kinds, so an incompatible family cannot reach the viewer.
Fractions are typed and restricted to `[0, 1]`; exact endpoints return the
original geometry. Intermediate geometry uses the explicit
`RecomputeFromGeometry` normal policy.

The viewer compiler does not materialize an intermediate `SurfaceGeometry`.
It writes interpolated world positions directly into the render packet,
recomputes normals, and reuses one cached index buffer. Render packets carry a
stable topology resource key plus a coordinate revision key. JavaFX and
Three.js classify a changed coordinate revision with unchanged topology as an
in-place geometry update: position and normal bytes change, while index,
layer/color, material, and atlas upload counts remain zero. Family union bounds
are cached when an asset is constructed and keep the camera center stable
throughout the transition.

Image/surface linking never guesses a display convention. `SurfaceWorldLink`
applies `surfaceToWorld`, then uses `VolumeSpace.worldToVoxel`; reverse linking
finds a nearest world-space vertex within a typed maximum radius. Typed vertex
annotations materialize as ordinary packed-RGBA layers, and bounded
`SurfaceSelectionHistory` records deterministic monotonic sequence numbers.

## Volume projection and connectivity contracts

`SurfaceVolumeProjection` is the portable CPU oracle on both JVM and Scala.js.
It reuses `VolToSurfMorphism`, `VolumeSurfaceSamplingPlan`, and `SomeSampleSpace`
rather than embedding spatial policy in a renderer. White, pial, midpoint,
fractional-thickness, and normal-line paths therefore share the existing
surface-to-world and world-to-voxel transforms. Reducer, minimum-sample, mask,
fill, and per-vertex quality decisions are explicit in typed policy and result
values. The receipt records requested and accepted samples, source volume and
materialized bytes, elapsed time, path, and reducer. A projected result becomes
an ordinary scalar layer, so JavaFX and the raster backend need no special
volume API.

The Three.js backend additionally admits `GpuVolumeProjection` only when its
live renderer provides WebGL2 and floating-point render targets. Its initial GPU
kernel implements midpoint/nearest projection through a float `Data3DTexture`;
unsupported sampling policies fail explicitly and retain the CPU path. The
browser acceptance page differentially compares GPU values, counts, and quality
flags against the asymmetric CPU fixture and fails if the maximum absolute
error exceeds `1e-5`.

Connectivity remains source-neutral in `surface-view`. Typed nodes preserve
surface, vertex, world position, optional region, and stable identity; typed
edges preserve signed weights. Absolute threshold, stable top-N, sign, and
region filters are compiled before rendering. Line/tube style is converted to
ordinary triangle and packed-color resources, allowing the raster, JavaFX, and
Three.js backends to consume the identical plan. The optional
`surface-view-connectivity` module adapts `StaticConnectivity`/`EdgeVector`
results without creating a dependency from the renderer back into estimation.
Its receipt reports retained positive, negative, and zero edges, generated
vertices and triangles, primitive bytes, and compilation time; the shared gate
includes a 20,000-edge workload to keep the accounting exercised on JVM and
Scala.js.

## Reproducible scene documents

`SurfaceSceneDocument` is the versioned, renderer-neutral handoff format for a
scientific surface view. Revision 1 records external surface and layer URIs with
SHA-256 digests; exact hemisphere, surface kind, vertex/face counts, and ordered
topology identity; layer encoding and frame count; layout and bilateral order;
camera, lighting, clipping, timepoint, selection, layer presentation/order; a
set of required backend-neutral capabilities; and sorted provenance entries.
Mesh coordinates and vertex-value payloads are deliberately not embedded.

`SurfaceSceneCodec` produces canonical compact JSON with stable field and array
order on JVM and Scala.js. The default reader rejects unknown fields. A caller
must explicitly select `SurfaceUnknownFieldPolicy.Ignore` to consume an
extension while retaining the known revision-1 state; unsupported revisions
remain errors under either policy. Duplicate fields, non-finite numbers,
malformed external references, invalid display state, and inconsistent layer or
surface identities return typed `SurfaceSceneError` values.

Restoration requires both a `SurfaceViewerModel` and resolved external
references. URI/digest equality and exact model identity are checked before the
document is replayed through `SurfaceViewer.reduce`. The conformance fixture
then recompiles the restored view and compares camera matrices, slots, resource
keys, packed colors, and draw passes to the pre-serialization plan. Capability
admission uses only `SurfaceBackendFeature`; JavaFX and Three.js names or object
types never enter the schema.

## Live JVM receipts

Run the complete pinned JVM admission matrix with:

```sh
sbt surfaceViewAdmissionJVM
```

The command runs the CPU raster baseline and then JavaFX Scene3D. It emits one
newline-delimited JSON object per case. JavaFX requires a usable display and an
available Scene3D pipeline; this is intentionally a live job rather than a
headless CI gate.

Pass an explicit repetition count when invoking a runner directly:

```sh
sbt "surfaceViewRasterJVM/runMain scalafim.surface.view.raster.SurfaceRasterAdmissionBenchmark 5"
sbt "surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceAdmissionBenchmark 5"
```

JavaFX is a `Provided` library dependency for downstream packaging, so its live
runner intentionally uses the test runtime classpath where the platform modules
are present.

Run the live JavaFX pixel and controller probes separately from the timing
matrix:

```sh
sbt surfaceViewVisualQaJVM
```

The first correctness probe loads the exact transformed, bilateral, layered,
thresholded GIFTI fixture used by the browser example, snapshots a real 960 x
540 JavaFX `SubScene`, and compares it with the same deterministic raster and
interior pick. The second probe retains the smaller publication fixture as an
independent differential check. The interaction probe exercises the native
pick-to-action bridge, perspective/orthographic camera switching on a mounted
`SubScene`, and orbit, pan, zoom, and reset controller paths. These probes remain
distinct from the admission benchmark because timing is not correctness
evidence.

## Live browser receipts

After linking the Scala.js backend, a browser host calls:

```js
runScalafimThreeAdmissionMatrix(THREE, canvas, 5)
```

The exported function returns an array of plain JSON objects. It uses a real
Three.js WebGL renderer, performs real center-canvas ray picks, and includes PNG
canvas serialization in the snapshot path.

The browser example at `examples/surface-view/browser/` and
`JavaFxGiftiParityProbe` share the stricter visual gate. Each compares native
pixels against the CPU oracle at identical dimensions, checks left and right
foreground coverage separately, and requires exact surface/face/nearest-vertex
agreement for the same interior landmark. The policy also detects whole-image
horizontal or vertical flips and interior color drift; the corresponding
adversarial tests run on both JVM and Scala.js.

### Geometry-family morph gate

The live morph gate uses matching, checksum-pinned bilateral fsaverage5 white,
pial, and inflated GIFTI surfaces plus a deterministic spherical projection of
the same real topology. It checks white-to-pial, pial-to-inflated, and
inflated-to-sphere at fractions 0, .25, .5, .75, and 1. Every native frame is
compared with a freshly rendered raster oracle, including separate left/right
foreground coverage and the existing flip-sensitive orientation contract.

Run JavaFX with a usable display:

```sh
sbt "surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxCorticalMorphProbe"
```

Link `surfaceViewExamplesJS/fastLinkJS`, serve the repository's parent code
directory, and open `examples/surface-view/browser/morph.html` for the WebGL
gate. The page fails on runtime errors, pixel drift, a topology rebuild, or a
color upload during a coordinate-only transition.

The 2026-07-22 receipt recorded all 15 frames passing on both backends. JavaFX
updated both hemispheres in one command using exactly 491,616 position/normal
bytes per changing frame; WebGL reported two retained-buffer updates and the
same byte count. Neither backend rebuilt topology or uploaded colors after the
cold frame. The gate also exposed and fixed a bilateral WebGL regression where
the previous right-viewport scissor limited the next frame's clear and left
stale geometry in the left viewport.

Synthetic timing at 163,842 vertices recorded 0.156-0.168 ms JavaFX buffer
mutation across 1/4/8 layer cases. Chrome recorded a 24.7 ms median and 50.4 ms
p95 for the full compile, normal recomputation, buffer update, and draw path;
the JavaFX number covers native buffer mutation only, so the two are not a
like-for-like renderer comparison. Full machine-specific evidence is in
`docs/benchmarks/receipts/surface-viewer-morph-2026-07-22.json`.

### Production cortical gate

`JavaFxCorticalAcceptanceProbe` and `browser/cortical.html` extend that contract
to checksum-pinned bilateral fsaverage5 pial surfaces: 20,484 vertices and
40,960 faces in the compiled scene. They additionally verify real GIFTI DTD
handling, FreeSurfer's zlib payload variant, bilateral slot recentering without
scientific-coordinate mutation, two-sided winding semantics, real surface and
vertex picking, and camera-only cache retention.

The corpus remains external because the GIFTI metadata identifies FreeSurfer
fsaverage5 sources but does not state data-specific redistribution terms. The
manifest pins both SHA-256 digests and the SurfViewJS source commit; CI vendoring
must wait for an explicit redistribution decision. The local live gate accepts
an alternate root through `SCALAFIM_SURFACE_CORPUS` or the JVM probe argument.

The 2026-07-21 acceptance receipt on this Apple Silicon host recorded:

| Backend | Mask IoU | Centroid px | Mean channel error | Cold native render | Camera-only render | Camera uploads |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| JavaFX Scene3D | 0.987294 | 0.472824 | 2.832600 | 46.769 ms | 0.222 ms | 0 mesh, 0 layer |
| Three.js/WebGL | 0.993546 | 0.032341 | 0.046904 | 6.100 ms | 0.200 ms | 0 mesh, 0 layer |

The full machine-specific record is
`docs/benchmarks/receipts/surface-viewer-cortical-acceptance-2026-07-21.json`.
These timings are observations, not hardware-independent budgets.

## Pinned matrix and receipt contract

The matrix is the Cartesian product of:

- meshes: exactly 32,768 and 163,842 vertices;
- visible layers: 1, 4, and 8;
- paths: cold load, camera-only, style update, layer-data update, timepoint
  update, pick, resize, and snapshot.

`SurfaceBenchmarkFixture` supplies identical primitive buffers and resource-key
transitions to every backend. Camera-only cases must report zero mesh and layer
uploads. Style, layer-data, and timepoint paths must report zero mesh uploads.
Those are exact admission rules, not timing heuristics.

Receipts use schema `scalafim.surface-benchmark.v1` and include:

- backend id, accepted plan revision, capabilities, and caveats;
- runtime, OS, architecture, graphics API, and device metadata;
- mesh/layer upload events, byte counts, texture dimensions, cache hits,
  draw calls, triangle counts, resize events, and picks;
- per-observation phase timings and nearest-rank minimum/p50/p95/maximum
  summaries.

Commit baseline JSON only after confirming the hardware and runtime metadata.
Structural counters are compared exactly. Timing regression policy should
compare like-for-like metadata and use a relative budget around a recorded
baseline; it must not encode a hardware-independent millisecond promise.

## Checked receipts and example parity

The current machine-specific summaries are:

- [`receipts/surface-view-javafx-jdk25-2026-07-21.json`](receipts/surface-view-javafx-jdk25-2026-07-21.json)
- [`receipts/surface-view-browser-chrome150-2026-07-21.json`](receipts/surface-view-browser-chrome150-2026-07-21.json)

The cross-platform example under `examples/surface-view` uses one checked GIFTI
fixture and one shared model. Its shared JVM/Scala.js test pins the semantic
receipt, including transformed world-space camera direction, bilateral slot and
layer order, selection, deterministic raster hash, shaded-pixel count, and first
pick. JavaFX additionally verifies that its production GIFTI decoder yields the
exact shared geometry. Run it with:

```sh
sbt surfaceViewExamplesJVM/test surfaceViewExamplesJS/test
```

On the recorded host, the direct 960 x 540 fixture produced the following live
native-backend comparison:

| backend | mask IoU | centroid error (px) | mean interior channel error | exact pick |
| --- | ---: | ---: | ---: | --- |
| JavaFX Scene3D | 0.997230 | 0.224042 | 0.640985 | left / face 0 / vertex 0 |
| Three.js/WebGL | 0.997230 | 0.224042 | 0.001046 | left / face 0 / vertex 0 |

Both native masks contained 115,520 foreground pixels, split 57,760/57,760
between the display halves, against 115,200 and 57,600/57,600 in the CPU
reference. The JavaFX color difference reflects retained texture-atlas sampling;
geometry, orientation, coverage, and typed picking remain within the shared
policy.
