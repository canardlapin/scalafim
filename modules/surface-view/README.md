# surface-view

Renderer-neutral, cross-platform surface-viewer model and compiler. It owns
typed surfaces and layers, display state, camera/layout/orientation semantics,
dynamic fields, projection and network primitives, scene documents, resource
identity, backend capabilities, and conformance/admission contracts.

The module emits `SurfaceRenderPlan`; it does not depend on JavaFX, Three.js, a
DOM, or a native windowing toolkit. See [`docs/surface-viewer.md`](../../docs/surface-viewer.md).
