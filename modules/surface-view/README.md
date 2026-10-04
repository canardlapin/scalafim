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
transformed coordinates in original vertex order. Keep source geometry stable during preparation. Coordinate/transform changes
require explicit preparation of a new snapshot; the old snapshot intentionally
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
