# Typed SurfaceRenderPlan affine fixture producer

Prepared in an isolated checkout at base `bc981ddf09016c1e29b30810f53cff4517df95d3`;
provider pins and root production source are unchanged. The sole source addition
is the approved JVM test main `SurfacePlanAffineFixtureProbe.scala`.

The producer admits all 48 frozen opaque, unlit v23 inputs, constructs typed
surface geometry/assets/layers/model, calls SurfaceCompiler.compile, and checks
every original VertexId coordinate raw bit and RGBA plus every original FaceId
ordered corner. It rejects expanded/remapped geometry, layers or passes, and
checks public resource addresses and receipt/profile shape. It writes positions
and indices from public mesh buffers and packed RGBA from public layer buffers.
The source boundary IDs are retained as oracle metadata after identity checks.

Every output binary matches both the frozen input and its original archive
member byte-for-byte, SHA-256-for-SHA-256. Header counts and v23 byte lengths
were independently checked in Python. The manifest lists all 48 hashes.

Surface-view tests passed 68 JVM and 68 Scala.js; the producer main passed all
48 cases. No compiler warnings/errors occurred. Exact commands and full logs
are retained. A separate four-gigabyte worktree base was used; final shutdown
returned0 and reported no server running.

The existing native probe must retain its known fixed pixel-parallel projection.
This fixture handoff excludes application camera fitting/matrices, lighting,
normals, cortical occlusion, multilayer alpha, fragment-scalar semantics, picking,
DPR, export and backend lifecycle/performance. It does not implement a complete
JOGL SurfaceRenderPlan provider.

`surface-plan-affine-fixture-producer.patch` applies the sole source addition.
`plan-inputs.tar.gz` carries 48 binaries and the field/identity/hash manifest.
`logs-and-source.tar.gz` retains source, runner and full gated logs. `receipt.json`
hashes the source, archive, artifacts and all cases. Root owns promotion and
native replay after source review. No Mote or publication changes were made.
