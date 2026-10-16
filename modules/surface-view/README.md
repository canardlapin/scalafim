# surface-view

Renderer-neutral, cross-platform surface-viewer model and compiler. It owns
typed surfaces and layers, display state, camera/layout/orientation semantics,
dynamic fields, projection and network primitives, scene documents, resource
identity, backend capabilities, and conformance/admission contracts.

The module emits `SurfaceRenderPlan`; it does not depend on JavaFX, Three.js, a
DOM, or a native windowing toolkit. See [`docs/surface-viewer.md`](../../docs/surface-viewer.md).

## Repeated world-coordinate picks

Call `SurfaceWorldLink.prepare(surfaceId, geometry)` once and reuse the returned
`SurfaceWorldIndex` with `index.nearestVertex(world, radius)`. It snapshots
transformed coordinates in original vertex order. Keep source geometry stable
during preparation. Coordinate/transform changes require explicit preparation
of a new snapshot; the old snapshot intentionally
continues to describe the old geometry. No cache reuse is inferred from mutable
mesh object identity or topology equality. The geometry-taking convenience
query remains a one-shot scan and observes current coordinates.

The index is a ScalaFIM world-placement adapter, not a new provider dependency.
It uses a balanced median-partition tree over primitive world-coordinate arrays,
retains no mesh reference, and returns ordinary `SurfaceSelection` values. It
uses full world Euclidean distance under scale/shear and chooses the lowest
original vertex id on ties; the radius boundary is inclusive. Affine validity
is established by the existing image4s constructor. Query stack/storage is
bounded by tree depth, although pathological coincident data can require a full
scan. There is no per-query mesh export or content hash.

Reproducible synthetic JVM measurement:
`sbt "surfaceViewJVM/Test/runMain scalafim.surface.view.SurfaceWorldIndexBenchmark"`.
The 2026-09-12 run used 1,000 independently checked queries per cardinality:

| Vertices | Build ms | Retained primitive bytes | Scan ms | Indexed ms | Setup crossover queries |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 10,449 | 9.09 | 292,572 | 8.83 | 2.74 | 1,495 |
| 163,842 | 48.84 | 4,587,576 | 220.09 | 17.23 | 241 |

Scan uses pretransformed primitive coordinates and no per-vertex allocation.
Index query allocation was approximately 112 bytes/query, independent of mesh
size (result and query state); preparation allocated 623,120 and 7,214,640 bytes.
These are one local warmed measurement, not stable latency gates or anatomical
fixture results. Retained bytes exclude JVM headers; no process-RSS claim is
made. Prefer the one-shot path when setup cannot amortize across repeated picks.

## Several views and display instances

`SurfaceCompositionCompiler.compile` accepts a shared presentation and a vector
of typed `SurfaceCompositionInputView`s. Each view has its own camera pose,
viewport, lighting, and instances. Each instance carries a `SurfaceDisplayFrame`
witness and an optional display translation. Four views can reuse one uploaded
mesh; several surfaces in one view share its depth buffer. Scientific geometry
and selections stay in their original coordinate frame.

`SurfaceCompositionState` applies orbit, pan, zoom, and resize operations.
Camera links apply deltas in each camera's own basis, preserving distinct medial
and lateral poses. A resize changes only the selected panel's aspect ratio.
`SurfaceCompositionPlan.fromLegacy` lowers an existing render plan, preserving
its viewport fit, offsets, lighting, clipping, and fragment policy.

`SurfaceCompositionDocument.capture` and `SurfaceCompositionCodec` save the
composition around the existing asset-checked scene document. Restore requires
external asset bindings and caller-supplied display witnesses with matching
persistent frame keys. It preserves fixed/global-morph geometry, camera links,
time, selection, and layer presentation. Local reveal-lens deformation is
explicitly refused by this document format. Existing V1–V7 scenes keep their
original codec and framing behavior.

Mesh resource keys identify topology; geometry revisions identify positions and
normals. Native updates retain topology while accepting a new geometry revision,
and reject changed bytes under an unchanged revision before issuing commands.
Layer keys identify their immutable payloads.
The plan constructor also checks viewport ownership, rigid camera/projection
forms, resource domains, and required fragment composition. Renderers report a
`SurfaceDisplayOrigin(view, instance)` alongside the original scientific pick.

## Contour strokes

`SurfaceContourStyle.mode` defaults to `SideBands`, preserving the existing
opaque border geometry. Opt into `SurfaceContourMode.RoundedStrokes` for
pixel-width capsule strokes with round endpoint caps. The core must be opaque;
the optional halo may carry alpha. A halo's full width is the core width plus
`halo.width`, and halos composite after all cores in declared edge order.

The lowerer binds these meshes to their source edge, camera, and output size.
Re-lower after changing the camera or canvas dimensions. Rounded strokes require
positive clip-W endpoints and opaque scientific surfaces; unsupported inputs
return an explicit refusal. Three.js and the raster backend implement this
policy. Native JavaFX refuses it so callers can use `JavaFxRasterSurfaceHost`.
Decorations are not currently persisted by the general scene codec.

The [contour comparison gallery](../../docs/visual-qa/surfviewjs-parity/rounded-contours-20261015/index.html)
records the supported interior geometry/blending checks and remaining edge,
performance, and full-matrix qualification gaps.
