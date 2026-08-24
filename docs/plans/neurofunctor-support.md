# Neurofunctor-Style Spatial Support

This plan maps the durable ideas in `~/code/neurofunctor` into ScalaFIM without
porting the R/S4 surface. The target is a typed, modular Scala 3 layer for
domains, morphism graphs, sampled linear operators, provenance, QC, and lazy
views.

## What neurofunctor contributes

The useful architecture is:

```text
Domain = Space + Geometry
MorphismGraph = domains + morphisms + route policy
OperatorCompiler = path -> sampled linear operator
FieldView = root data + lazy operator application
```

The core semantics worth preserving are:

- spaces cover native volumes, surfaces, templates, hybrids/grayordinates, and
  latent coefficient spaces;
- geometries define sampled elements: voxel grids, meshes, and ordered hybrid
  parts;
- morphisms are pullback coordinate maps: a path from source to target is
  evaluated at target coordinates to find source samples;
- a complete path should compile to one sampled-domain operator, avoiding
  repeated interpolation;
- operators are target-by-source maps with first-class adjoints for
  backprojection;
- provenance records the selected path, hashes, inverse quality, ROI, coverage,
  and sampling policy;
- QC checks identity, composition, adjoint, round-trip, coverage, and
  commutativity;
- caches are keyed by immutable descriptions, not by incidental runtime state.

The parts not worth carrying over directly are S4 classes, mutable environment
caches on graph objects, `igraph` as a public dependency, one-based row/column
conventions, R callback lists as the main abstraction, and compilation hidden
inside `to()`.

## ScalaFIM boundary

Do not put this into `image`, `surface`, or `atlas` as one-off helpers. The
clean boundary is:

- `linalg`: dependency-free numerical operator algebra.
- `spatial`: domain graph, morphism descriptions, operator compilation,
  provenance, QC, and field views.
- `image`: volume spaces, data containers, affine math, and low-level volume
  sampling helpers.
- `surface`: mesh geometry, topology, vertex fields, and surface sampling paths.
- `atlas`: atlas metadata, route descriptors, parcel operations, and later
  adapters into spatial domains.
- `dataset`: data backends and series adapters that can feed or consume spatial
  field views.
- JVM-only IO packages: NIfTI, CIFTI, FreeSurfer, fMRIPrep, ANTs/FSL/AFNI,
  and on-disk operator caches. GIFTI surface ingestion is available through
  platform-specific JVM and Scala.js entry points.

The first new module should be:

```scala
lazy val spatial =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/spatial"))
    .dependsOn(linalg, image, surface)
```

`spatial` can start as one module with strict package boundaries:

```text
scalafim.spatial.domain
scalafim.spatial.morphism
scalafim.spatial.graph
scalafim.spatial.operator
scalafim.spatial.qc
scalafim.spatial.field
```

If warp IO and coordinate kernels grow large, split a later
`scalafim-transform` module out of `scalafim.spatial.morphism`. Do not split it
before the toy path is stable.

## Core model

Use closed algebraic types and opaque ids rather than strings at API
boundaries:

```scala
opaque type DomainId = String
opaque type MorphismId = String
opaque type OperatorId = String

enum SpaceRef:
  case Volume(subject: SubjectId, session: Option[SessionId], modality: Modality)
  case Surface(subject: SubjectId, hemisphere: Hemisphere, kind: SurfaceKind)
  case Template(name: TemplateName, resolution: Resolution, kind: TemplateKind)
  case Latent(dim: Int, basisName: Option[String], support: Option[DomainId])

enum SamplingGeometry:
  case Volume(space: SomeSampleSpace, mask: Option[NeuroVol[Boolean]])
  case Surface(geometry: SurfaceGeometry, mask: Option[SurfaceRoi[Boolean]])
  case Hybrid(parts: Vector[DomainPart])

final case class Domain(id: DomainId, space: SpaceRef, geometry: SamplingGeometry)
```

`Hybrid` should be an ordered vector of parts with stable offsets. It should not
be a special string id such as `"hybrid"` because block-diagonal operators need
real source and target domain identities.

Morphisms should distinguish geometric inverse from operator adjoint:

```scala
enum MorphismKind:
  case Identity, Affine3D, Warp3D, VolumeToSurface, SurfaceToSurface, Functional, Filter

enum Inverse:
  case Exact(method: String)
  case Provided(method: String, quality: Double)
  case Approximate(method: String, quality: Double)
  case AdjointOnly
  case None

final case class Morphism(
  id: MorphismId,
  source: DomainId,
  target: DomainId,
  kind: MorphismKind,
  routeTag: RouteTag,
  cost: Double,
  inverse: Inverse,
  map: CoordMapRef,
  provenance: MorphismProvenance
)
```

Reverse routing may use `Exact`, `Provided`, and `Approximate`. It must not use
`AdjointOnly` as a geometric inverse. Backprojection is an operator-level
operation via `adjoint`.

## Operator algebra

Add the numeric core to `linalg`, without spatial types:

```scala
trait LinearMap:
  def rows: Int
  def cols: Int
  def forward(x: DoubleMatrix): DoubleMatrix
  def adjoint: LinearMap

final case class SparseTriplets(rows: Int, cols: Int, i: Array[Int], j: Array[Int], x: Array[Double])
final class CsrMatrix private (...) extends LinearMap
final class ComposedLinearMap private (left: LinearMap, right: LinearMap) extends LinearMap
final class RestrictedLinearMap private (...) extends LinearMap
```

`spatial.operator` then wraps these maps with domain and provenance:

```scala
final case class SpatialOperator(
  source: DomainId,
  target: DomainId,
  map: LinearMap,
  path: MorphismPath,
  qc: OperatorQc,
  provenance: OperatorProvenance
)
```

Required operations:

- `forward`: source samples by observations -> target samples by observations;
- `adjoint`: target samples by observations -> source samples by observations;
- `compose`: `P2 * P1`, with domain and dimension checks;
- `restrict`: ROI rows and optional source columns;
- `blockDiag`: hybrid parts with explicit offsets and provenance merge;
- `toTriplets`: stable serialization format for cache and fixtures.

Use zero-based indices throughout Scala. Any R parity fixture must convert at
the boundary.

## Compiler shape

The compiler should be pure from descriptions to compiled operators:

```scala
final case class CompileRequest(
  source: DomainId,
  target: DomainId,
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  roi: Option[Vector[Int]],
  allowInverses: Boolean
)

trait OperatorCompiler:
  def compile(graph: SpatialGraph, request: CompileRequest): Either[SpatialError, SpatialOperator]
```

Implementation order:

1. Resolve a `MorphismPath`.
2. Enumerate target sample coordinates, optionally ROI-only.
3. Compose target-to-source coordinate maps for the whole path.
4. Build interpolation rows against the source geometry.
5. Assemble a sparse target-by-source operator.
6. Compute coverage and path quality.
7. Return a typed operator plus provenance.

Sampling kernels:

- volume source: nearest and trilinear weights;
- volume-to-surface: midpoint first, ribbon second, both using existing
  `SurfaceGeometryPair`;
- surface source: nearest vertex first, barycentric later;
- hybrid target/source: compile per part and combine with block diagonal or
  offset-aware rows.

The existing `surface.VolumeSurfaceSampler` can remain as an eager convenience
API, but the operator compiler should own reusable projector weights.

## Graph and cache

`SpatialGraph` should be immutable:

```scala
final case class SpatialGraph(
  domains: Map[DomainId, Domain],
  morphisms: Vector[Morphism]
):
  def add(domain: Domain): Either[SpatialError, SpatialGraph]
  def add(morphism: Morphism): Either[SpatialError, SpatialGraph]
  def path(source: DomainId, target: DomainId, policy: RoutingPolicy): Either[SpatialError, MorphismPath]
```

Path finding can be a small Dijkstra implementation over typed edges. A public
graph-library object should not leak into the API.

Cache should be an interpreter service, not a field on `SpatialGraph`:

```scala
trait OperatorCache[F[_]]:
  def get(key: OperatorCacheKey): F[Option[SpatialOperator]]
  def put(key: OperatorCacheKey, operator: SpatialOperator): F[Unit]
```

Shared code can have an in-memory cache. JVM code can add file-backed triplet
caches later. Cache keys should include source, target, path morphism ids and
hashes, sampling policy, ROI, inverse policy, and compiler version.

## Field runtime

Keep field views thin and inspectable:

```scala
final case class Field[A](
  data: FieldDataRef[A],
  domain: DomainId,
  root: DomainId,
  pending: Option[OperatorId],
  provenance: FieldProvenance
)

trait FieldRuntime[F[_]]:
  def view[A](field: Field[A], target: DomainId, request: CompileRequest): F[Either[SpatialError, Field[A]]]
  def data(field: Field[Double]): F[Either[SpatialError, DoubleMatrix]]
```

The runtime may compile/load an operator and apply it, but `Field` itself should
not hide mutable caches or file handles. Dataset/image adapters can create
`FieldDataRef` values for `NeuroVol`, `NeuroVec`, archive payloads, or backend
blocks.

## First implementation slice

1. Add `LinearMap`, `SparseTriplets`, CSR apply, transpose, compose, restrict,
   block diagonal, and tests to `linalg`.
2. Add `modules/spatial` with `DomainId`, `SpaceRef`, `SamplingGeometry`,
   `Domain`, `Morphism`, `SpatialGraph`, and route-policy tests.
3. Implement identity and affine morphisms over volume domains.
4. Compile volume-to-volume trilinear operators on toy `SomeSampleSpace` fixtures.
5. Add functor and adjoint tests:
   - identity compiles to identity;
   - direct affine path matches composed affine path;
   - `<P x, y> == <x, P^T y>`;
   - ROI restriction preserves selected rows;
   - coverage is explicit for out-of-bounds samples.
6. Add volume-to-surface midpoint weights using existing `SurfaceGeometryPair`.
7. Add ribbon weights and hybrid block diagonal support.
8. Add an in-memory `OperatorCache`.
9. Add JVM-only fMRIPrep/transform ingest and file-backed triplet cache.

Do not start with real warp files. The shared toy graph should prove the API and
operator algebra first.

## R parity path

Use `neurofunctor` as a fixture generator, not as an API template.

Initial fixtures:

- tiny affine volume-to-volume projector triplets;
- tiny midpoint volume-to-surface projector triplets;
- ribbon weights for a small hand-built white/pial pair;
- adjoint outputs for fixed input matrices;
- path provenance examples with inverse-quality flags.

Fixture export should store zero-based triplets for Scala tests. If generated
from R, the exporter should subtract one from row/column indices and record the
source package commit hash.

## Acceptance checklist

- Shared JVM and JS tests pass for `linalg`, `image`, `surface`, and `spatial`.
- No JVM-only IO dependency enters shared code.
- Domain, morphism, path, compile request, operator, cache key, and field view
  are separately inspectable values.
- A graph never owns mutable operator caches.
- Operator adjoints are available even when geometric inverses are not.
- Volume-to-surface support compiles to a reusable sparse operator rather than
  only eager sampled values.
- The first public API is typed Scala 3, not a list-shaped R compatibility
  surface.
