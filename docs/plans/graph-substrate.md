# Unified Graph Substrate Plan

Status: planned  
Mote epic: `bd-01KX92SV3T5RA26J9QWGX79W2F`

## Decision

ScalaFIM will add two reusable cross-platform modules:

- `graph`: dependency-free ordered bases, explicit simple topology, alignment,
  traversal, components, paths, cycles, and DAG algorithms;
- `graph-linalg`: basis-carrying numerical operators built from `graph` and the
  existing `linalg` storage and solver contracts.

`connectivity` remains the scientific measurement domain. It continues to own
node/time/run axes, complete `EdgeSpace` coordinate systems, connectivity
measures, dense matrices, dynamics, inference, and projection provenance.

The central distinction is:

```text
ConnectivityMatrix = measurement over a complete coordinate space
Graph              = explicit set of relationships selected as present
```

This plan does not make every matrix a graph, treat a zero measurement as an
absent edge, turn a matrix diagonal into loops, or reinterpret rectangular
connectivity as an ordinary directed graph.

## Dependency Target

```text
graph                    no internal dependencies
graph-linalg             graph, linalg
connectivity             graph, linalg
atlas                    graph, image, surface
surface                  graph, image
spatial                  graph, linalg, image, surface
pipeline                 graph

downstream spectral/connectome analysis
                         connectivity, graph-linalg, multivar as needed
```

`connectivity` will not depend on `graph-linalg` initially. Connectivity-to-
graph projection produces a typed graph; downstream analysis composes that
graph with `graph-linalg` explicitly.

## Version-One Scope

Version one supports only:

- immutable simple graphs;
- loopless construction;
- directed or undirected topology;
- stable domain keys separated from graph-local dense coordinates;
- deterministic canonical edge order;
- edge payloads whose semantic capabilities are supplied contextually;
- shared JVM and Scala.js implementations and law tests.

Multiplicity, loops, public persistent edge identifiers, graph isomorphism,
rectangular relations, mutable builders as public APIs, and graph-specific
storage backends are deferred until a concrete consumer requires them.

## Core Model

### Identity and coordinates

`K` is stable domain identity. A dense integer is only a coordinate in one
ordered basis:

```scala
opaque type VertexIx = Int
```

It must not be named `VertexId`. Induced subgraphs and reindexing may change a
`VertexIx` while preserving `K`.

The reusable basis is:

```scala
VertexBasis[K, V]
```

It stores observable entries in a `Vector[(K, V)]` and a private key-to-index
map. Its public compatibility levels are:

- `sameKeyOrderAs`: numerically coordinate-compatible;
- `sameKeySetAs`: alignable by permutation;
- `sameMetadataAs`: equal keys, order, and values;
- validated permutation/alignment values rather than untyped index arrays.

An induced basis preserves relative source ordering and returns its mapping to
the source basis. A reindexed basis returns the forward and inverse
permutations.

### Graph and edge identity

The graph direction is encoded in the type. Multiplicity and loop policy are
not type parameters in version one.

For simple graphs, endpoint pairs are edge identity:

- directed: ordered `(from, to)`;
- undirected: canonical unordered `(minIx, maxIx)`.

Any storage `EdgeIx` remains graph-local and non-semantic. Domains that possess
stable edge keys carry them in the edge payload.

Construction must:

1. resolve endpoint keys through the supplied basis;
2. reject unknown keys, self-edges, and duplicate simple edges;
3. canonicalize undirected endpoints;
4. sort the edge table lexicographically;
5. construct private outgoing and incoming adjacency caches.

`Graph` is not a case class. Observable storage uses `Vector`; hot adjacency
caches may use private arrays. Structural equality and hashing depend only on
the direction, ordered basis, canonical edge table, and edge payloads, never on
construction order or cache representation.

Ordinary graph equality is basis-order-sensitive. Graph isomorphism is a
separate future operation.

### Semantic edge capabilities

Algorithms request the meaning they need rather than accepting an arbitrary
`Double` conversion:

```scala
trait AdjacencyWeight[-E]:
  def weight(edge: E): Double

trait NonNegativeAdjacencyWeight[-E] extends AdjacencyWeight[E]
trait StrictlyPositiveAdjacencyWeight[-E]
    extends NonNegativeAdjacencyWeight[E]

trait EdgeCost[-E]:
  def cost(edge: E): Double
```

Validated wrappers should distinguish at least signed affinity, nonnegative
affinity, and distance/cost. Distance does not receive an adjacency instance by
default; affinity does not receive a path-cost instance by default. Conversions
such as RBF distance-to-affinity or inverse-affinity path cost are explicit
policies.

`EdgeCombine` is not part of simple graph algorithms. It belongs only at an
explicit boundary that collapses repeated observations or future multiedges.

## Graph Algorithms

The first generic algorithms are:

- undirected connected components and connectivity;
- directed weak and strong components;
- nonnegative-cost shortest paths with path reconstruction;
- cycle detection with a concrete cycle witness;
- topological order and deterministic topological layers;
- a validated `Dag` refinement;
- induced subgraphs and reindexing.

Components use explicit topology and ignore edge payloads. Weighted paths use
`EdgeCost[E]`. Domain wrappers retain their own validation and terminology.

## Graph-Linalg Boundary

`graph-linalg` reuses:

- `scalafim.linalg.LinearMap`;
- `SparseTriplets`;
- `CsrMatrix`;
- `DoubleMatrix` and `DoubleVector`;
- existing and future eigensolver capabilities owned by `linalg`.

It must not define parallel sparse-matrix or eigensolver families.

Public results carry their coordinate meaning:

```scala
VertexSignal[K, V]
VertexOperator[K, V]
EdgeBasis[D, K]
IncidenceOperator[D, K, V]
VertexSpectrum[K, V]
SpectralEmbedding[K, V]
```

The raw `LinearMap`, CSR representation, or decomposition result remains
accessible, but is not the sole public result.

### Topology and numerical support

The API distinguishes:

- `topologyAdjacency`: one for each present edge;
- `weightedAdjacency`: interpreted numerical edge weights;
- `degree`: count of present neighbors;
- `strength`: sum of adjacency weights.

`CsrMatrix` may drop explicit numerical zeros. Therefore its nonzero structure
must never be used to reconstruct graph topology.

For weighted Laplacians, weighted-zero support is explicit:

```text
nullity(L) = component count after dropping zero-weight edges
```

This equals topological component count only when every present edge has
strictly positive affinity. Normalized-Laplacian isolates are zero-strength
vertices, not merely vertices with no topological neighbors. Every normalized
operator requires an explicit isolate policy.

Incidence uses a deterministic orientation for undirected edges. Reversing any
incidence column must leave `B W B^T` unchanged.

### Spectral solvers

Partial eigensolver contracts belong in `linalg`, preferably over `LinearMap`
or a narrower symmetric-operator capability rather than CSR specifically.
`graph-linalg` requests smallest/largest eigenpairs and wraps results with the
vertex basis.

Spectral tests compare:

- eigenvalues under permutation similarity;
- eigenvectors up to sign for isolated eigenvalues;
- subspaces or projection matrices for repeated eigenspaces;
- embedding pairwise geometry rather than byte-identical coordinates.

## Connectivity Integration

### NodeAxis migration

`NodeAxis` will delegate storage and indexing to:

```scala
VertexBasis[NodeId, NodeSpec]
```

It remains a connectivity public type and adds:

- `sameKeyOrderAs`;
- `sameKeySetAs`;
- `sameMetadataAs`;
- `sameScientificBasisAs`.

`sameScientificBasisAs` incorporates connectivity-specific axis provenance,
such as parcellation identity and version. `sameIdentityAs` keeps its current
semantics during migration, then becomes deprecated only after every call site
has selected a precise replacement.

Existing Scala-native and Ariadne-compatible `EdgeSpace` ordering and fixtures
must remain unchanged.

### Matrix-to-graph projection

Projection has separate selection and output stages:

```text
1. Determine eligible EdgeSpace coordinates.
2. Validate and ignore the measurement diagonal.
3. Compute selection scores.
4. Select coordinates with deterministic tie behavior.
5. Transform selected measurements into typed edge payloads.
6. Apply post-transform zero policy.
7. Construct the canonical simple graph.
```

The policy independently records:

- eligibility;
- selection score: raw, absolute, positive, negative, or distance ordering;
- selection: all, nonzero tolerance, threshold, density, or mask;
- deterministic density tie policy;
- output transformation;
- diagonal treatment;
- post-transform zero policy.

This permits operations such as selecting by absolute correlation while
preserving the original signed correlation.

Rectangular `EdgeSpace` is rejected by graph projection in version one. A
future `Relation` or basis-carrying rectangular operator may reuse
`VertexBasis`; it is not forced through ordinary graph topology.

Projection returns:

```scala
ProjectedConnectivityGraph[D, K, V, W]
```

containing:

- the canonical graph;
- a deterministic `ProjectionReceipt`;
- one source `EdgeSpaceIx` for each graph edge in canonical edge order.

The correspondence is injective for a simple graph and supports mapping graph
statistics back to edge vectors, reporting original matrix coordinates, and
reconstructing masks.

The receipt records the source measure, ordered scientific basis identity,
EdgeSpace ordering convention, requested and realized selection parameters,
tie handling, transformation, diagonal and zero policies, selected edge count,
zero-strength vertices, projection version, and any versioned portable basis
digest. Scala `hashCode` is never persistent provenance.

## Domain Migration Policy

Shared graph adoption must preserve domain APIs:

- `RegionGraph` remains atlas vocabulary;
- `MeshTopology`, surface components, and geodesics remain surface vocabulary;
- `SpatialGraph`, routing policy, morphism validation, and reconstructed paths
  remain spatial vocabulary;
- `PipelineGraph`, artifact-kind validation, `ExecutionPlan`, and stages remain
  pipeline vocabulary.

Each migration begins with a differential harness against the current
implementation. Deletion of domain traversal code is a later step and requires
matching results plus memory/performance evidence.

Specialized implicit-neighborhood algorithms remain separate. In particular,
voxel-grid connected components and other kernels that avoid materializing a
huge graph are not rewritten merely to use the generic graph API.

Surface adoption requires an explicit memory gate: a generic graph must not
silently duplicate the existing mesh edge and neighbor tables at production
mesh sizes. The implementation may make `MeshTopology` own a graph or expose a
zero-copy/internal view after measurement; temporary dual storage is acceptable
only in differential tests.

## Laws and Test Oracles

### Basis

- keys are unique;
- `size == entries.size`;
- `indexOf(keyAt(ix)) == Some(ix)`;
- key lookup round-trips;
- metadata changes do not change key-order compatibility;
- metadata equality implies key-order compatibility;
- equal-key-set permutations are invertible;
- alignment composition is associative;
- induced bases preserve relative source order.

### Graph values

- all endpoints belong to the basis;
- loops and duplicate simple edges are rejected;
- undirected endpoints are canonical;
- input edge order does not affect equality or hash code;
- identity vertex/edge mappings preserve the graph;
- induced-by-all-keys returns the original graph;
- nested induced subgraphs obey intersection;
- permutation followed by its inverse restores the graph.

### Algorithms

- components form a disjoint partition of the basis;
- same-component membership is equivalent to path existence;
- shortest paths use present edges and report the summed cost;
- Dijkstra matches Floyd-Warshall on deterministic small random graphs;
- DAG layers contain every vertex once and respect every dependency edge;
- cycle rejection returns a valid cycle witness.

### Projection

- output key order equals the source `NodeAxis` key order;
- every graph edge maps to exactly one selected source coordinate;
- no unselected or diagonal coordinate becomes an edge;
- source correspondence is injective;
- selection and output transformation remain independent;
- density ties are deterministic and recorded;
- replaying the same matrix and policy reproduces graph, correspondence, and
  receipt;
- current EdgeSpace vectorization roundtrips remain byte-for-byte unchanged.

### Linear algebra

- topology adjacency exactly represents present edges;
- undirected adjacency and Laplacians are symmetric;
- weighted adjacency uses only explicit `AdjacencyWeight` evidence;
- combinatorial `L = D - A`, `L 1 = 0`, and positive semidefiniteness hold for
  nonnegative affinity;
- weighted Laplacian nullity matches positive-weight support components;
- with strictly positive weights, weighted and topological component counts
  agree;
- `L = B W B^T` and incidence orientation invariance hold;
- reindexing produces `P A P^T` and `P L P^T`;
- spectral eigenvalues and embedding geometry are reindexing-invariant.

Tests live in shared source sets and run on both JVM and Scala.js. Randomized
tests use deterministic seeds and independent small-graph or dense-matrix
oracles rather than the implementation under test.

## Execution Phases

### Phase 0: architecture contract

Deliver this plan, final module boundaries, build/test commands, tracker
dependencies, and non-goals. No implementation.

Gate: the plan is consistent with `vision.md`, `AGENTS.md`, `build.sbt`, and
`docs/module-relations.md`, and does not duplicate `linalg` ownership.

### Phase 1: graph foundation

Add the `graph` crossProject, `VertexIx`, `VertexBasis`, permutations,
alignments, canonical simple directed/undirected graph values, errors, and law
tests. Update root aggregates, aliases, README, and module relations.

Gate:

```sh
sbt graphJVM/test graphJS/test
```

### Phase 2: graph algorithms

Add components, shortest paths, directed algorithms, cycle witnesses,
topological layers, and `Dag`.

Gate: shared oracle and randomized tests pass on JVM and JS.

### Phase 3: connectivity basis integration

Make `NodeAxis` delegate to `VertexBasis`; add precise compatibility and
scientific provenance APIs while preserving legacy semantics and vectorization.

Gate:

```sh
sbt graphJVM/test graphJS/test connectivityJVM/test connectivityJS/test
```

### Phase 4: graph-linalg operators

Add the `graph-linalg` crossProject and basis-carrying topology adjacency,
weighted adjacency, degree/strength, incidence, and Laplacian APIs over existing
`linalg` types.

Gate:

```sh
sbt linalgJVM/test linalgJS/test graphJVM/test graphJS/test graphLinalgJVM/test graphLinalgJS/test
```

### Phase 5: connectivity projection

Add typed projection policies, weight transformations, receipts, source edge
correspondence, and adversarial projection laws. Rectangular projection remains
an explicit unsupported boundary.

Gate: graph and connectivity tests pass on both platforms, including replay,
tie, signed-weight, distance, zero, diagonal, and source-mapping cases.

### Phase 6: independent domain migration audit

Build differential fixtures and performance/memory probes before changing
domain implementations. This is a separate blocking work item so implementation
does not certify itself.

Gate: expected equivalence, intentional differences, and performance budgets
are documented for atlas, surface, spatial, and pipeline.

### Phase 7: domain migrations

Migrate atlas/surface first, then spatial; migrate pipeline independently after
the audit. Preserve public types and scenario contracts. Remove duplicate
algorithms only after differential and performance gates pass.

Gate: affected module JVM/JS suites plus relevant scenario parity suites pass.

### Phase 8: operator eigensolvers and graph spectra

Add operator-oriented partial eigensolver capabilities to `linalg`, then add
basis-carrying graph spectra and embeddings. Signed connectivity does not enter
ordinary nonnegative Laplacian APIs without an explicit transformation.

Gate: linalg backend contracts, graph-linalg laws, permutation/subspace tests,
and both-platform suites pass.

### Phase 9: similarities

Add `GraphSimilarity` or feature-producing spectral summaries only after the
projection and spectral contracts are stable. Only mathematically justified PSD
constructions receive adapters to `multivar.Kernel`.

Gate: symmetry, basis/alignment, relabeling, and PSD tests match each
implementation's documented mathematical claims.

### Final integration gate

After build aggregation and all migrations:

```sh
sbt compileAll
sbt testAll
```

The build must remain warning-clean on Scala 3.7.4 for JVM and Scala.js.

## Mote Work Graph

- `bd-01KX92T189GCS09PYKWKWDNKX1`: architecture contract and roadmap;
- `bd-01KX92VR0G0D73MEFPTM20JPC0`: graph basis/alignment/core values;
- `bd-01KX92VR6SRN3WQSHPSR29XV38`: graph algorithms and DAG refinement;
- `bd-01KX92VRCNVBX0V90ZDRQKZVDP`: `NodeAxis` basis integration;
- `bd-01KX92VRJHJ8YGGJF1TJ53SK73`: graph-linalg operators;
- `bd-01KX92VRR7MWR37PM40HZAC10J`: connectivity projection;
- `bd-01KX92VRXXB3WJQRR5Z4YE0EJZ`: independent migration audit/harness;
- `bd-01KX92VS3M8HVCYHX0MHA3YRWQ`: atlas/surface migration;
- `bd-01KX92VS9Q9SH7AC4BTQPYSFTG`: spatial migration;
- `bd-01KX92VSFAY26X8BCGN9CQ56NF`: pipeline migration;
- `bd-01KX92VSMWNZD2FT13YN2DHKWK`: linalg operator eigensolver capability;
- `bd-01KX92VSTP6G1S8ZV3YJK0ZXH2`: graph spectra and embeddings;
- `bd-01KX92VT0WDQJX07Y2K7Y6S8D9`: similarities and PSD adapters.

The epic is complete only when the terminal spatial, pipeline, and similarity
branches close and the full integration gate passes.

## Explicit Non-Goals

- replacing `EdgeSpace` with graph edges;
- moving connectivity measures or inference into graph;
- treating rectangular connectivity as a directed graph;
- adding a second sparse matrix or eigensolver hierarchy;
- making `graph-linalg` depend on connectivity or multivar;
- replacing domain public APIs with bare `Graph` values;
- forcing grid algorithms to materialize voxel graphs;
- claiming arbitrary graph similarities are PSD kernels;
- preserving dense coordinates as stable identity across induced/reindexed
  graphs;
- introducing multigraph or loop type parameters before a real consumer exists.
