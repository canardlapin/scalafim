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
