# Geodesic correctness before extraction — 2026-10-20

ScalaFIM source `78acb531d9d805d679f546db29060818ac2b49d0` is locally qualified.
It uses published mesh4s `b9e0553b85113d52fc758747d62ddc0fa343e033` and the
still-local atlas override `7cfe1369c0c098ab7539d6a4d60646b80eb08d00`.
This bounded repair does not finish `eco-u3-kernels` or the eco epic.

## Repair

Geodesic matrix filling previously converted target vertices to a map, so a
repeated target retained only its last column and earlier columns stayed at
infinity. Matrix filling now writes every requested column in order, retaining
repeated sources and targets.

The cache previously keyed by vertex/edge counts and a 32-bit weight hash.
It could return another mesh's result or mistake distinct weights for equal
ones. The key now compares the full immutable mesh (connectivity and coordinates)
and the complete weight vector, alongside metric and ordered row/column ids.
Hashes only accelerate lookup. Equal mesh values restored with fresh runtime
owners still reuse an entry. Disabled caching avoids constructing a key.

The key retains references to the immutable mesh and weights until the cache is
cleared; it does not copy their buffers. The public `GeodesicCache.Key` record
now carries mesh/weights instead of counts/hash. Matrix, metric and cache-policy
entry points are unchanged. Existing validation—including finite nonnegative
costs, at least one positive geodesic cost, and positive chunk sizes—is preserved.
The solver and its numerical operations are unchanged.

## Independent evidence

`regressions-before-01.log` retains all ten previous tests passing and five
new courts failing on the old production implementation:

- repeated target columns remain infinity instead of finite distances;
- equal-sized meshes with different connectivity reuse 2 instead of 1;
- changed coordinates reuse a stale direct distance;
- distinct finite weights with the same legacy bit-fold hash reuse 1 instead
  of `1.0000009536743166`;
- an independent Floyd–Warshall oracle exposes missing repeated columns.

The final tests also exercise spherical coordinate changes, equal restored mesh
cache reuse, source/target order, zero-cost paths, disconnected infinity results,
and chunk sizes 1 and 2. The Floyd–Warshall court covers 12 deterministic cost
sets on each of three mesh topologies. Its all-pairs dynamic program is
independent of the production priority-queue algorithm. Existing generic graph
and scientific fixture checks remain intact.

`regressions-after-02.log` is retained adverse evidence: JVM 19 tests passed;
JS 18 passed and one fixture failed because boxed Double hashing was assumed to
match the JVM. The corrected fixture explicitly checks the old cache's exact
bit fold. No production check or numerical tolerance was weakened.

## Final gates

| Gate | Passed | Skipped |
| --- | ---: | ---: |
| surface JVM | 330 | 0 |
| surface JS | 287 | 2 |
| surface-view JVM | 256 | 0 |
| surface-view JS | 256 | 0 |
| Total | 1,129 | 2 |

`qualified-surface-view-03.log` exits zero with no compiler warnings. The two
existing JS placement/performance skips remain unqualified. Changed code and
its direct view consumers compile and test on both platforms; the earlier
all-module compile receipt remains bound to its earlier source revision.
No hosted CI, new native visual acceptance or timing/allocation result is claimed.

[qualification.json](qualification.json) records the command, log hash and all
19 loaded source builds. Raw logs and metadata remain under the common Git
directory's `verification/geodesic-correctness-20261020/`.

## Next solver seam

Generic prepared single-source shortest paths with finite nonnegative costs
belong in graph4s. Its pinned `DirectedRoutes.shortest` exposes a single-target
route; calling that repeatedly would replace one source traversal with many.
Strict mesh intrinsic metrics also reject some valid legacy custom costs, so
they cannot silently replace this API.

Add and independently qualify the reusable distance capability upstream, then
adapt the mesh vertex/edge owners without casts or renumbering. Coordinate one
graph4s revision across all consumers before default-pin acceptance. Preserve
these cache and Floyd–Warshall witnesses through that cutover. Complete remaining
U3 input-admission acceptance and required independent review before closure.
