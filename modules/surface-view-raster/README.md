# surface-view-raster

Deterministic CPU reference interpreter for `SurfaceRenderPlan`, shared by JVM
and Scala.js. It provides exact depth buffering, culling, clipping, compositing,
native-equivalent picking receipts, and reproducible pixels. It is the semantic
oracle for concrete GPU/toolkit backends, not the preferred interactive path.

`SurfaceRasterizer.renderStrips` streams render-only images in increasing row order using
the same native plan, full-frame camera fitting and global pixel coordinates. It omits pick
buffers. Each callback receives an immutable `SurfaceRasterStrip` with its first row and full
dimensions. Consume/release that image before returning; retaining every strip defeats the
streaming memory bound. Only a successful final receipt admits a complete output. Discard
partial output after cancellation, a refused sink or a rendering failure.

`preflightStrips` checks conservative primitive-buffer headroom before allocation. For a
3GiB application, `SurfaceStripConfig.forHeap(3L << 30, reservedBytes)` subtracts the caller's
explicit reservation for the resident scene, accumulated output, upload buffers, JVM/object
overhead and other application state. This is an allocation preflight, not a claim that arbitrary
application heap use fits. Default strips are64 rows and a256MiB primitive-buffer budget.
The preflight allows two pixel/depth generations plus composed vertex colors; no full-frame
image or picking arrays are allocated by this path. It does not introduce supersampling or
change the existing reference renderer's transparency, lighting or scientific interpolation.

`stripCapabilities` declares a separate render-only backend without native picking. Image
upload, navigation/controller policy, actual off-gate timing and real-scene3GiB qualification
belong to subsequent consumer admission. Shared full-frame/strip equivalence and cancellation
tests must pass on JVM and Scala.js before provider adoption.
