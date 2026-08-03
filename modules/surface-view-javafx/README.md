# surface-view-javafx

JVM JavaFX Scene3D interpreter and reducer-backed controller for the shared
surface-view plan. Meshes and packed layer atlases are retained across camera
updates; snapshots and picks return typed receipts. OpenJFX remains `Provided`
for published consumers, so applications choose and package their platform
modules explicitly.

See [`docs/surface-viewer.md`](../../docs/surface-viewer.md) for lifecycle,
atlas-size limits, packaging, and fallback behavior.
