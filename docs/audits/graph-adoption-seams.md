# Graph Adoption Seam Audit

Date: 2026-07-11  
Tracker: `bd-01KX92VRXXB3WJQRR5Z4YE0EJZ`

Status: historical. The audited in-repository graph substrate was subsequently
extracted to standalone graph4s; numerical graph consumers now use its optional
`graph4s-gale` module.

## Outcome

The reusable `graph` module is a sound semantic oracle for the surveyed
algorithms, but it is not a drop-in replacement for every current storage or
neighborhood kernel.

Adopt it in three different ways:

1. `pipeline` is a direct delegation candidate once duplicate dependency edges
   and the existing cycle diagnostic are preserved.
2. `spatial` needs a policy-specific adapter because the domain admits parallel
   morphisms and identity morphisms while graph v1 is simple and loopless.
3. `atlas` and `surface` should expose or internally use graph values only where
   that removes a representation. Implicit voxel scans, filtered mesh traversals,
   and multi-target geodesic kernels remain specialized until graph offers
   equivalent non-materializing operations.

The audit adds test-only graph dependencies and differential suites. Production
dependency edges are deliberately deferred to the migration beads.

## Executable Evidence

| Domain | Differential fixture | Independent comparison |
| --- | --- | --- |
| Atlas | `modules/atlas/.../GraphDifferentialSuite.scala` | `RegionGraph.adjacency` contact counts versus a full 26-neighborhood oracle for Connect6/18/26; region edges then lower to canonical simple topology. |
| Surface | `modules/surface/.../GraphDifferentialSuite.scala` | Thresholded mesh components versus induced graph components; default/custom geodesics versus graph Dijkstra; unreachable pairs versus `NoPath`. |
| Spatial | `modules/spatial/.../GraphDifferentialSuite.scala` | Route costs, reconstructed morphism ids, policy filtering, geometric inverses, parallel-edge collapse, and identity-path behavior. |
| Pipeline | `modules/pipeline/.../GraphDifferentialSuite.scala` | Stable pipeline stages versus `Dag` layers; repeated dependency collapse; generic cycle witness versus the domain remaining-node diagnostic. |

All four suites run from shared sources on JVM and Scala.js.

## Atlas

### Live seam

`RegionGraph.adjacency` scans an implicit voxel neighborhood and accumulates
region-pair contact counts. It never constructs a voxel graph. This is the right
algorithm: materializing one vertex and several edges per voxel would increase
memory dramatically without improving the region result.

The returned `Vector[RegionEdge]` is already deterministic and endpoint-
canonical. It can lower to:

```text
UndirectedGraph[RegionId, Region, Int]
```

without changing `RegionGraph`, `RegionEdge`, or atlas syntax. The graph basis
must include every atlas region, including regions with no selected adjacency.

### Decision

- Keep the voxel contact scanner specialized.
- Add graph conversion/result APIs only where a consumer needs traversal or
  `graph4s-gale`.
- Do not make the scanner construct a voxel graph internally.
- Keep `SpaceTransforms.shortestRoute` as a separate later seam: its weighted
  status/confidence policy is not part of region adjacency and is outside the
  current migration bead.

## Surface

### Live seams

`MeshTopology` already stores explicit canonical edges, neighbor rows, and edge
lengths. `SurfaceComponents` and `SurfaceParcels` each implement filtered BFS.
`SurfaceGeodesics` implements weighted Dijkstra and currently rebuilds weighted
adjacency inside each source run.

The differential fixtures show that graph components and shortest-path costs
match current behavior, including custom edge weights and disconnected meshes.
They do not by themselves justify retaining both `MeshTopology` and a second
full graph value.

### Storage and performance limits

These are hard migration gates:

- At most one persistent full topology representation per mesh. A migrated
  `MeshTopology` may wrap graph as its sole edge/adjacency store, or expose a
  transient adapter, but must not retain both complete representations.
- No graph materialization per thresholded field, parcel, source vertex, or
  target vertex.
- Component traversal remains `O(V + E)` and allocates `O(V)` working state.
- A geodesic matrix builds/interprets weighted adjacency at most once per call,
  not once per source-target pair. Per-source search remains
  `O((V + E) log V)` or better.
- Shared JVM/JS tests must include disconnected meshes, zero/nonuniform edge
  weights where valid, and exact route reconstruction.

Before changing `MeshTopology` storage, benchmark representative approximately
32k-vertex and 164k-vertex hemisphere meshes. The candidate must use no more
than 1.25 times the retained topology memory of the current representation and
must not regress median component/geodesic runtime by more than 15 percent over
five warmed runs. These are migration acceptance limits, not unit-test timing
assertions.

### Decision

- Do not implement thresholded components as `inducedByKeys` per call; that
  allocates a new graph and edge table. Keep the filtered-neighbor BFS until
  graph supports a non-materializing filtered view.
- Do not implement distance matrices as one `shortestPath` call per target.
  Keep the surface kernel until graph exposes a single-source distance result or
  an equivalent reusable traversal.
- Remove the duplicated component implementations in `SurfaceComponents` and
  `SurfaceParcels` by sharing one surface-local filtered traversal now; graph
  delegation can follow when the filtered-view gate is met.
- A graph interop adapter remains useful for downstream topology and
  `graph4s-gale` consumers, provided it is cached once or transient and
  measured.

## Spatial

### Live seam

`SpatialGraph.path` builds policy-specific route edges and runs Dijkstra.
Domain behavior includes:

- anatomical/functional filtering;
- generated geometric inverse morphisms and inverse penalties;
- multiple morphisms with the same ordered domain endpoints;
- identity paths represented by an explicit domain morphism;
- `MorphismPath` validation, inverse flags, and quality.

Graph v1 is simple and loopless. Therefore the adapter must first filter by
policy and inverse availability, drop self identity edges from the search
topology, and collapse parallel endpoint pairs to the cheapest eligible
morphism. Equal-cost collapse must retain original morphism order. The public
identity case remains `MorphismPath.identity`; generic zero-edge paths must not
leak into the domain API.

The ordered basis must be explicit. `SpatialGraph.domains` is a `Map`, so its
iteration order is not a numerical or tie-breaking contract. Migration should
store or derive a deterministic domain order and add an explicit equal-cost
route fixture before delegation.

### Decision

- Keep all morphism/domain validation and routing policy in `spatial`.
- Delegate only the validated simple route search.
- Preserve reconstructed `Morphism` payloads, inverse ids, cost, and
  `usedInverses`.
- Do not pretend the reusable graph owns a stable morphism edge identity.

## Pipeline

### Live seam

`ExecutionPlan` validates node uniqueness, dependency existence, artifact-kind
compatibility, cycles, and insertion-stable stages. Generic `Dag` reproduces the
successful stage order when its `VertexBasis` follows `PipelineGraph.nodes`.

Two translation details are mandatory:

- expression dependencies may repeat the same upstream node, so dependency
  pairs must be deduplicated before simple graph construction;
- domain validation must run before graph construction so unknown dependencies
  and artifact-kind errors retain their current typed errors.

On failure, generic graph returns a concrete cycle witness. The current
`CyclicGraph` error reports every node left after maximal staging, which may
include downstream nodes not on the cycle. The migration must preserve this
public diagnostic or deliberately revise it in a separate API change. A cycle
witness may be added as extra detail, not substituted silently.

### Decision

- Retain `PipelineGraph`, `ExecutionPlan`, `PipelineStage`, artifact validation,
  and insertion order.
- Use a graph adapter with node payloads and unit dependency edges.
- Delegate successful layering through `Dag`.
- Preserve the existing remaining-node cycle error; add a graph-core progress
  result if necessary rather than duplicating a second Kahn implementation in
  pipeline.

## Specialized Algorithms That Remain Separate

- atlas voxel contact scanning for Connect6/18/26;
- image/grid connected components over implicit neighborhoods;
- surface filtered components until a borrowed/filtered graph view exists;
- surface multi-target geodesics until a reusable single-source distance API
  exists;
- rectangular connectivity, which remains an `EdgeSpace`/operator or future
  relation rather than a directed graph.

These are not exceptions to the unified architecture. They share stable keyed
bases and can emit graph-compatible results, while retaining algorithms whose
implicit or domain-specific representation is the source of their efficiency.

## Migration Gates

1. Keep the differential suites green before and after each delegation.
2. Add production `graph` dependencies only in the module being migrated.
3. Preserve public domain result types and error semantics.
4. Do not retain parallel full topology stores.
5. Run every affected suite on JVM and Scala.js.
6. For surface storage changes, satisfy the explicit memory/runtime benchmark
   limits before deleting the old representation.
