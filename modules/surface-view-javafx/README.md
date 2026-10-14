# surface-view-javafx

JVM JavaFX Scene3D interpreter and reducer-backed controller for the shared
surface-view plan. Meshes and packed layer atlases are retained across camera
updates; snapshots and picks return typed receipts. OpenJFX remains `Provided`
for published consumers, so applications choose and package their platform
modules explicitly.

The default atlas remains `LegacyTriangle` with native Phong lighting. Recovered
affine encodings are diagnostic candidates: production backend creation returns
`SamplerUnqualified` because no stock JavaFX runtime has been admitted. The
explicit `WorldVertexLambert` policy shades original world-space vertex colors
before interpolation and has a separate contract from native Phong lighting.
See the [sampler capability proposal](../../docs/plans/javafx-affine-sampler-capability.md)
for the required per-map filtering behavior and qualification boundary.

See [`docs/surface-viewer.md`](../../docs/surface-viewer.md) for lifecycle,
atlas-size limits, packaging, and fallback behavior.

`JavaFxSurfaceComposition` hosts one depth-enabled native `SubScene` per
composition view. Add its `root` to the caller-owned scene, call `render` with a
`JavaFxSnapshotConfig`, and call `resize` when the host changes. Every operation
runs on the JavaFX Application Thread; the caller owns the Stage lifecycle.
Occurrences share the production backend's meshes and atlases. `pick` accepts
an actual JavaFX `PickResult` and returns both its display origin and original
scientific face/vertex/barycentrics. `dispose` detaches the host and releases its
owned resources.

This path uses explicit world-space vertex lighting. Shared atlases currently
require the same lighting, mesh, and ordered layer stack for repeated instances
of a surface. Fragment layers and world clipping return typed refusals. These
bounds remain visible in admission; native multi-view success does not qualify
those other display features.
