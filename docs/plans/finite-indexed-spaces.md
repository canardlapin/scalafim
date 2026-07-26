# Finite Indexed Spaces for Regions, Parcellations, and Searchlights

Status: ratified architecture and implementation plan

Date: 2026-07-26

Mote epic: `bd-01KYFTRA5BVGEBJN3VKBA256B4`

Planning bead: `bd-01KYFTRWJ168EAKS91N8PM99KR`

Scope: `locus-kernel`, `locus-data`, `locus-laws`, and migrations across
`graph`, `image`, `surface`, `atlas`, `spatial`, `dataset`, `mvpa-spatial`,
`connectivity`, `threshold`, `latent`, and narrow multivar adapters

## 1. Executive Decision

ScalaFIM will add one shared calculus for finite, addressable domains. It will
sit below `graph`, `image`, `surface`, and `atlas` rather than belonging to any
one of them.

The production artifacts are:

```text
locus-kernel
  FiniteSpace, Point, Region, Selection, TotalMap, Relation

locus-data
  IndexedField, Section, Parcellation, Searchlight, aggregation

locus-laws
  exhaustive finite reference models and reusable JVM/Scala.js law suites
```

Their public packages are `scalafim.locus` and `scalafim.locus.laws`.
Artifact names are `scalafim-locus-kernel`, `scalafim-locus-data`, and
`scalafim-locus-laws`.

The central identifications are:

```text
binary ROI       = Region[X]
ordered extract  = Selection[X]
parcellation     = a supported quotient X -> P
network          = a quotient factorization P -> N
searchlight      = a center domain plus an endorelation X -> X
extraction       = field restriction
ROI summary      = a fiberwise commutative fold
adjacency        = an endorelation with additional validated evidence
```

This is not authorization for a universal `ROI` hierarchy. Names, atlas
metadata, geometry, storage, interpolation, statistical analysis, and
probabilistic membership remain separate.

Existing APIs will migrate incrementally. A live type is not replaced merely
because its name resembles a locus type. Each overlapping type in this plan is
classified as one of:

- reuse unchanged;
- adapt to a locus value;
- back with a locus implementation while preserving its public API;
- rename because its current name denotes a different concept;
- retain as an explicitly domain-specific value;
- deprecate and remove after downstream migration.

No implementation phase may add a second generic region, point, finite-space,
relation, parcellation, or searchlight representation.

## 2. Why This Work Is Needed

ScalaFIM already has strong local implementations, but the same underlying
ideas are represented several times:

- `image.VoxelRegion` is an unordered, grid-bound set, while
  `spatial.VoxelRegion` is an axis-aligned box;
- `image.VoxelSelection`, `dataset.VoxelSelection`, `spatial.RowSelection`,
  `multivar.IndexSet`, and `latent.RadialVoxelSelection` each carry a different
  mix of ordering, bounds, and domain semantics;
- atlas `Region` is metadata, not an extensional region;
- volume searchlights materialize `ROIVolWindow` values, while
  `mvpa-spatial` defines another center and window-set model;
- `SurfaceRoi` bundles membership, geometry, values, and a label;
- volume and surface label fields encode partitions without a shared quotient
  contract;
- `graph.VertexBasis`, connectivity axes, dataset voxel domains, and spatial
  domains each maintain ordered finite axes;
- `spatial.Field` is a lazy execution and provenance value, while the desired
  mathematical field is a pure random-access function.

These are not all mistakes. Several represent different layers and should
remain different. The defect is the absence of a neutral layer that states
which parts are the same, supplies the shared laws, and makes conversions
explicit.

Without that layer:

1. region Boolean laws are tested only for particular volume code;
2. exact transport and interpolation are easy to conflate;
3. atlas aggregation rescans or allocates by parcel instead of lowering to one
   shared fiberwise operation;
4. volume and surface searchlights can disagree on boundary semantics;
5. parcel adjacency has no shared semantic reference model;
6. downstream modules revalidate raw integer selections without retaining a
   common ambient-space identity;
7. type names suggest equivalence where semantics differ.

## 3. Goals

### G1. One neutral finite-space kernel

Provide one portable representation of semantic finite spaces, points,
regions, ordered selections, exact total maps, and finite relations.

### G2. Static and runtime space safety

Use a phantom type to prevent ordinary cross-space composition and a runtime
`SpaceKey` to catch erased, existential, deserialized, or incorrectly reused
phantom types.

### G3. Explicit ordering

Keep extensional regions unordered. Require `Selection[X]` or ambient-domain
order wherever row order is observable.

### G4. Quotients rather than vectors of masks

Represent a parcellation as a supported label map into a typed parcel space.
Derive fibers, networks, refinement, and one-pass aggregation from that map.

### G5. Relations rather than materialized searchlight windows

Represent a searchlight scheme as an endorelation plus its allowed centers.
Materialize data windows only at the image, surface, MVPA, or execution
boundary that needs them.

### G6. Separate exact and approximate transport

Reserve `TotalMap` for exact discrete point maps. Use `Relation` for
multi-valued crisp transport. Interpolation and weighted membership will use a
later weighted-kernel sibling rather than masquerading as exact maps.

### G7. Reuse live structures

Keep `graph.VertexBasis` as the keyed metadata basis, `spatial.Field` as the
lazy execution value, and image/surface containers as concrete storage and
geometry values. Adapt them instead of cloning their useful behavior.

### G8. Law-driven optimization

Make Boolean, indexed-logic, relation, quotient, restriction, and aggregation
laws executable on JVM and Scala.js. Test optimized representations against a
small dense reference model.

### G9. Incremental adoption

Keep current volume, surface, atlas, dataset, and MVPA workflows operational
during migration. Breaking semantic changes require named compatibility
adapters and a removal ledger.

## 4. Non-Goals

This epic does not authorize:

- a base `ROI` trait with mask, atlas, network, and searchlight subclasses;
- replacing every type named `Space`, `Region`, `Field`, or `Selection`;
- moving geometry, coordinates, topology, or connectivity into `Region`;
- replacing `graph.Graph` with a Boolean relation;
- making loops legal in the current simple-graph API;
- treating interpolation or resampling as a `TotalMap`;
- an implicit conversion between equal-shaped volume grids;
- storing labels, names, colors, or provenance in extensional region equality;
- representing a parcellation as `Vector[NamedRegion]`;
- treating an overlapping or probabilistic atlas as a parcellation;
- Boolean operations on arbitrary weighted masks;
- an ordered `Foldable` instance for `Region`;
- a second lazy execution system beside `spatial.Field`;
- NIfTI, GIFTI, CIFTI, BIDS, atlas registry, or file-resource concerns in
  locus;
- type-level arithmetic for domain sizes learned at runtime;
- immediate removal of every legacy raw-index API;
- weighted relations or stochastic kernels in the crisp-kernel release.

Weighted relations and stochastic atlases are a planned extension point, not a
hidden mode of the crisp types.

## 5. Governing Semantic Model

### 5.1 Finite spaces and points

A finite space is an ordered, addressable semantic domain. Its order gives
points stable local ordinals; it does not by itself imply geometry.

```scala
opaque type SpaceKey = String
opaque type Point[S] = Int

final class FiniteSpace[S] private (
  val key: SpaceKey,
  val size: Int
):
  def point(ordinal: Int): Option[Point[S]]
  def points: Iterator[Point[S]]
```

The kernel permits an empty finite space. Domain adapters may require a
positive size.

`SpaceKey` identifies the ordered semantic elements, not just their shape. Two
91 by 109 by 91 grids with the same affine are not automatically the same
space. A volume adapter must receive or derive a key from explicit domain or
acquisition identity. Geometry-only compatibility remains a separate check.

A useful identity record distinguishes:

```text
semantic identity   SpaceKey: which ordered elements these are
geometry identity   grid, affine, topology, or coordinate fingerprint
provenance          where the identity and geometry came from
```

No core constructor silently equates semantic identity with geometry
fingerprinting.

The phantom parameter `S` prevents ordinary accidental mixing. Runtime-loaded
spaces are packaged existentially:

```scala
trait SomeFiniteSpace:
  type S
  val value: FiniteSpace[S]
```

Objects that must share a dynamic space retain the same package or an explicit
checked re-identification witness. Public constructors never accept a bare
`Point[S]` without checking it against the supplied `FiniteSpace[S]`.

### 5.2 Regions and ordered selections

`Region[S]` is an extensional subset of one `FiniteSpace[S]`.

```scala
final class Region[S] private (
  val space: FiniteSpace[S],
  private val members: OrdinalSet
):
  def contains(point: Point[S]): Boolean
  def cardinality: Int
  def subsetOf(that: Region[S]): Either[SpaceMismatch, Boolean]
  def union(that: Region[S]): Either[SpaceMismatch, Region[S]]
  def intersect(that: Region[S]): Either[SpaceMismatch, Region[S]]
  def diff(that: Region[S]): Either[SpaceMismatch, Region[S]]
  def complement: Region[S]
```

Equality uses the runtime space identity and member ordinals. Construction
history, labels, metadata, iteration history, and storage representation do
not affect equality.

`Selection[S]` is an ordered, duplicate-free sequence of points in one space:

```scala
final class Selection[S] private (
  val space: FiniteSpace[S],
  private val ordered: IArray[Point[S]],
  val region: Region[S]
)
```

The distinction is permanent:

- use `Region` for membership and Boolean algebra;
- use `Selection` when extraction order, matrix row order, display order, or
  serialization order is observable;
- ambient-domain order is an allowed operational order, but it is never part
  of region equality.

Both values may be empty. A consumer that requires non-empty input validates
that policy at its boundary.

### 5.3 Exact maps and indexed Boolean logic

`TotalMap[X, Y]` is a total point map represented by a dense target-ordinal
array:

```scala
final class TotalMap[X, Y] private (
  val from: FiniteSpace[X],
  val to: FiniteSpace[Y],
  private val targets: IArray[Int]
):
  def apply(point: Point[X]): Point[Y]
  def andThen[Z](that: TotalMap[Y, Z]): Either[SpaceMismatch, TotalMap[X, Z]]
  def pullback(region: Region[Y]): Either[SpaceMismatch, Region[X]]
  def existsAlong(region: Region[X]): Either[SpaceMismatch, Region[Y]]
  def forallAlong(region: Region[X]): Either[SpaceMismatch, Region[Y]]
  def forallAlongOnImage(region: Region[X]): Either[SpaceMismatch, Region[Y]]
  def asRelation: Relation[X, Y]
```

`forallAlong` follows ordinary vacuous truth, so target points with empty
fibers are included. `forallAlongOnImage` intersects that result with the image
of the whole source.

Validated evidence wrappers are separate values:

```text
Injection[X, Y]
Surjection[X, Y]
Bijection[X, Y]
```

They are produced by checked constructors. A caller does not set an
`isBijective` Boolean.

`TotalMap` never denotes:

- nearest-neighbor interpolation;
- trilinear interpolation;
- a partially covered transform;
- a many-candidate correspondence;
- a probabilistic assignment.

### 5.4 Relations and searchlights

`Relation[X, Y]` is a finite Boolean relation with sparse rows:

```scala
final class Relation[X, Y] private (
  val from: FiniteSpace[X],
  val to: FiniteSpace[Y]
):
  def row(point: Point[X]): Region[Y]
  def andThen[Z](that: Relation[Y, Z]): Either[SpaceMismatch, Relation[X, Z]]
  def converse: Relation[Y, X]
  def union(that: Relation[X, Y]): Either[SpaceMismatch, Relation[X, Y]]
  def image(region: Region[X]): Either[SpaceMismatch, Region[Y]]
  def allRelatedInside(region: Region[Y]): Either[SpaceMismatch, Region[X]]
```

The kernel supplies identity, composition, converse, union, relational image,
and the image/erosion adjunction. It may use CSR internally, but CSR is not the
public contract.

Evidence such as reflexivity or symmetry is validated:

```text
ReflexiveRelation[S]
SymmetricRelation[S]
```

`graph.Graph` remains a loop-free, keyed, metadata-bearing simple graph.
`Relation` permits identity loops and carries no edge values. Graph conversion
therefore requires an explicit policy for loops, direction, keys, metadata, and
edge weights.

A searchlight scheme is not only a type alias because its set of centers may
be restricted:

```scala
final class Searchlight[S] private (
  val centers: Region[S],
  val neighborhoods: Relation[S, S]
):
  def regionAt(center: Point[S]): Option[Region[S]]
```

Construction requires rows outside `centers` to be empty. A separately
validated `CenteredSearchlight[S]` proves that every allowed center belongs to
its own neighborhood. Symmetry is separate evidence and is not assumed for
k-nearest-neighbor searchlights.

Metric-ball constructors use closed balls:

```text
K_r(x) = { y | d(x, y) <= r }
```

Radius zero is valid. For a proper metric and all-point center domain,
`K_0` is the identity relation. Existing volume and surface compatibility
methods may retain stricter legacy validation temporarily, but the new
constructor and law suite use non-negative radii and a closed boundary.

Value filtering such as “nonzero voxels only” is field restriction after the
geometric neighborhood is constructed. It is not part of metric geometry.

### 5.5 Parcellations as supported quotients

A parcellation has a typed parcel codomain. It does not use arbitrary labels as
parcel identity:

```scala
final class Parcellation[X, P] private (
  val ambient: FiniteSpace[X],
  val parcels: FiniteSpace[P],
  private val parcelOrdinalAt: IArray[Int]
):
  def parcelAt(point: Point[X]): Option[Point[P]]
  def support: Region[X]
  def fiber(parcel: Point[P]): Region[X]
  def quotientRelation: Relation[X, P]
  def coarsen[Q](
    mapping: Surjection[P, Q]
  ): Either[SpaceMismatch, Parcellation[X, Q]]
```

`-1` is an internal background sentinel only. It never crosses the public API.
Every point in the support has exactly one parcel. Every point in the parcel
space has a non-empty fiber; a constructor rejects unused parcel ordinals.

The quotient codomain gives three distinct equalities:

- label-field equality, including parcel identities;
- region equality of individual fibers;
- partition equality up to a bijective relabeling.

Names, integer atlas ids, colors, networks, ontology terms, display order, and
release provenance are metadata indexed by `Point[P]`.

A network assignment is a validated surjection `P -> N`. Network regions are
fibers of the composite quotient. The atlas layer must not build independent
network masks and hope that they agree.

An overlapping atlas is a relation `L -> X`, not a `Parcellation[X, L]`.
Probabilistic membership is a future stochastic-kernel type with normalization
evidence.

### 5.6 Indexed fields and restriction

The pure mathematical field is named `IndexedField` to avoid collision with
the existing lazy `spatial.Field`:

```scala
trait IndexedField[S, +A]:
  def space: FiniteSpace[S]
  def apply(point: Point[S]): A
  def map[B](f: A => B): IndexedField[S, B]
  def restrict(region: Region[S]): Either[SpaceMismatch, Section[S, A]]

final class Section[S, +A] private (
  val field: IndexedField[S, A],
  val support: Region[S]
):
  def apply(point: Point[S]): Option[A]
  def restrict(region: Region[S]): Either[SpaceMismatch, Section[S, A]]
  def valuesInDomainOrder: Iterator[A]
  def valuesIn(selection: Selection[S]): Either[SpaceMismatch, Iterator[A]]
```

Image, surface, dataset, connectivity, and spatial modules provide adapters.
The locus core does not own storage, chunks, caches, handles, or interpolation.

`spatial.Field` remains the inspectable lazy plan and provenance value for
samples by observations. It can expose an `IndexedField` view when materialized
or provide a restriction-aware source adapter. It is not renamed or replaced
by this epic.

Product and coproduct spaces are deferred until concrete CIFTI or
spatiotemporal APIs require them. The first release may represent a spatial
point whose value is an observation vector without pretending that this is
already a type-level `X × T`.

### 5.7 Fiberwise aggregation

Generic aggregation belongs in `locus-data`:

```scala
def foldMapBy[X, P, A, M](
  parcellation: Parcellation[X, P],
  field: IndexedField[X, A]
)(
  contribution: A => M
)(using cats.kernel.CommutativeMonoid[M]): Either[SpaceMismatch, IndexedField[P, M]]
```

The implementation makes one pass over the supported source points. It does
not construct every parcel mask and rescan the field.

`locus-kernel` remains free of external dependencies. Commutative folding
extensions live in `locus-data`, which uses Cats Kernel rather than defining a
new monoid type.

Means accumulate a mergeable state such as `(sum, count)` and divide only at
the presentation boundary. Missing and non-finite values require an explicit
policy.

Ordinary IEEE `Double` addition is not associative. Exact fusion laws are
tested with lawful exact monoids. Floating-point reducers use a documented
canonical order or a reproducible accumulator and are tested with explicit
tolerances. The API and documentation must not claim bitwise hierarchy fusion
for ordinary unordered `Double` addition.

## 6. Module and Dependency Architecture

The target internal dependency shape is:

```text
locus-kernel
+-- locus-data
|   +-- image
|   |   +-- surface       also depends on graph
|   |   |   +-- atlas     also depends directly on locus-data and graph
|   |   +-- spatial       also depends on graph, linalg, surface
|   |   +-- dataset       existing additional dependencies remain
|   +-- mvpa-spatial      through image, surface, and atlas
+-- graph
    +-- connectivity
    +-- surface
    +-- atlas
    +-- spatial

locus-laws
  test-only dependency from locus and migrating consumer test configurations
```

`locus-kernel` has no internal or external production dependencies.
`locus-data` depends only on `locus-kernel` internally and Cats Kernel
externally. `locus-laws` depends on both locus artifacts and test libraries.

Modules whose public APIs mention a locus type declare a direct dependency;
they do not rely only on a transitive edge.

### 6.1 `locus-kernel`

Owns:

- `SpaceKey`, `FiniteSpace`, existential space packaging;
- `Point`;
- `Region`, `Selection`;
- `TotalMap`, injection/surjection/bijection evidence;
- `Relation`, reflexive/symmetric evidence;
- dense reference-free production representations and typed errors.

Does not own:

- coordinates, metrics, meshes, affines, or interpolation;
- keyed graph metadata;
- fields, parcellations, atlases, or reductions;
- files or provenance catalogs.

### 6.2 `locus-data`

Owns:

- `IndexedField` and `Section`;
- `Parcellation`;
- `Searchlight` and centered-searchlight evidence;
- fiberwise commutative aggregation;
- partition comparison and coarsening;
- adapters among locus values, not neuroimaging containers.

Does not own:

- volume/surface constructors;
- atlas metadata or registry identity;
- graph algorithms;
- lazy field execution;
- statistical analysis.

### 6.3 `locus-laws`

Owns:

- exhaustive finite reference representations;
- reusable law groups;
- ScalaCheck generators where random testing adds value;
- differential helpers for optimized consumers;
- no production API.

### 6.4 Existing modules

`graph` continues to own keyed vertex bases, simple graphs, graph algorithms,
and graph-specific errors. `VertexBasis[K, V]` is reused as the keyed metadata
axis. Locus will not introduce a competing generic keyed-axis type.

`image` owns volume geometry, dense and sparse image storage, masks, coordinate
transforms, interpolation, and volumetric searchlight constructors.

`surface` owns mesh identity, coordinates, topology, geodesics, labels,
fragmentation policies, and surface searchlight constructors.

`atlas` owns atlas identity, parcel metadata, ontology, release provenance,
loaders, lookup, and volume/surface wrappers over generic parcellations.

`spatial` owns continuous morphisms, route selection, sampled operators,
weighted interpolation, lazy field plans, and explicit lowering into an exact
map, crisp relation, or future weighted kernel when justified.

Downstream computational modules own their algorithms and consume locus values
through narrow adapters.

## 7. Existing Abstraction Inventory and Required Disposition

The inventory below is part of the acceptance contract. Implementers must
update it if new overlapping types appear before their phase begins.

| Existing value | Actual meaning | Disposition |
| --- | --- | --- |
| `graph.VertexBasis[K,V]` and `VertexIx` | Ordered keyed vertices plus metadata and graph-local ordinals | Reuse. Add checked locus adapters; do not create another keyed-axis abstraction. Keep graph-local public compatibility. |
| `image.NeuroSpace` and `VolumeSpace` | Volume shape, affine, axes, and coordinate geometry | Retain. Add an adapter that requires an explicit semantic `SpaceKey`; geometry alone does not mint semantic identity. |
| `image.VoxelIndexSet` | Validated integer membership or ordering, depending on constructor | Migrate its two meanings to `Region` and `Selection`. Keep a compatibility wrapper while image consumers move. |
| `image.VoxelRegion` | Unordered set on an exact volume grid | Back with `Region[V]`; preserve checked cross-grid operations through compatibility methods. |
| `image.VoxelSelection` | Ordered unique voxel extraction | Back with `Selection[V]`; preserve requested order. |
| `image.VoxelRoi` and `ROICoords` | Legacy ordered coordinate/index ROI input | Compatibility-only after adapters exist. No new core code consumes them directly. |
| `image.ROIVol`, `RoiValues`, and `RoiSeries` | Data restricted to an ordered voxel support | Adapt to `IndexedField`, `Section`, and explicit `Selection`; retain concrete image storage APIs where useful. |
| `image.ROIVolWindow` | Materialized searchlight data plus center | Keep as an image compatibility/materialization result. It is not the searchlight definition. |
| `image.Mask.MaskVol` | Dense Boolean volume | Retain as storage. Add lossless `Region` conversion with explicit space identity. |
| `image.Searchlight*` | Volume neighborhood construction and immediate window materialization | Split into relation construction and optional materialization. Standardize non-negative radius and closed-ball semantics in the new API. |
| `surface.SurfaceDomain` | Hemisphere plus vertex count | Do not use as exact semantic identity. Migrate exact adapters to `SurfaceMeshDomain`; deprecate exact-space uses of the weaker type. |
| `surface.SurfaceMeshDomain` | Hemisphere plus ordered topology identity | Reuse as the geometry/topology source for a locus space key. |
| `surface.SurfaceField[A]` | Geometry-bound sparse or dense vertex data | Retain as a surface container and adapt to `IndexedField`. |
| `surface.SurfaceRoi[A]` | Geometry, membership, values, and label bundled together | Split semantics into locus `Region`, field/section data, and optional annotation. Keep a compatibility constructor/view. |
| `surface.LabeledSurface` | Vertex label field plus label table | Adapt to `Parcellation`; keep label table as metadata. |
| `surface.ParcelUnit` | A topology-aware parcel or connected fragment | Retain as geometry output. A disconnected parcellation fiber remains valid; fragmentation policy is not a core quotient invariant. |
| `atlas.Region` and `RegionIndex` | Parcel metadata and lookup | Rename to `AtlasRegionMetadata` and `ParcelMetadataIndex` or equivalent. Provide deprecated aliases during migration. Never use them for extensional equality. |
| `atlas.VolumeAtlas` and `SurfaceAtlas` | Atlas identity, metadata, provenance, and concrete label payload | Retain as neuroimaging wrappers over typed parcellations. |
| `atlas.AtlasReduce` | Per-parcel reductions with temporary allocations | Rebase on one-pass `foldMapBy`; preserve reducer convenience through explicit missing-value policies. |
| `atlas.AtlasOverlap` | Pairwise overlap with optional implicit nearest resampling | Remove the Boolean resample switch from the new API. Require same-space evidence or an explicit alignment plan and receipt. |
| `atlas.RegionGraph` | Weighted parcel contact scan and graph construction | Keep the optimized contact-count scan. Differentially test its Boolean support against quotient-relation composition after removing self edges. |
| `spatial.Domain` and `SamplingGeometry` | Runtime semantic domain, sampled geometry, masks, and hybrid parts | Retain. Every finite sampled domain exposes a locus space package. Surface compatibility must use topology identity, not vertex count alone. |
| `spatial.VoxelRegion` | Half-open axis-aligned box | Rename to `VoxelBox`. It is geometry, not a general region. |
| `spatial.SpatialDemand` and `RowSelection` | Lazy physical/logical demand over field rows and time | Retain planning semantics. Lower locus `Region` and `Selection` explicitly; retire duplicate raw ROI/mask row variants after migration. |
| `spatial.Field` | Lazy field root, view plan, execution state, and provenance | Retain unchanged in role. Use `IndexedField` only as a pure view or adapter, never as a replacement runtime. |
| `dataset.VoxelIndex`, `VoxelSelection`, and `VoxelDomain` | Dataset read policy, requested order, and active/full spatial availability | Retain policy distinctions. Back resolved points/order with locus values once `DatasetShape` owns an explicit finite-space identity. |
| `dataset.VoxelSampleMap` | Exact injection from active sample rows to full-grid voxels plus partial reverse lookup | Re-express with `Selection` and an injection/lookup adapter; preserve active/full-grid semantics. |
| `mvpa.FeatureSet` and `FeatureSetPlan` | Algorithm-level feature groups and searchlight execution plans | Retain in `mvpa`. Add constructors from locus regions, selections, parcellations, and searchlights. |
| `mvpa-spatial.SearchlightCenter`, `SearchlightWindow`, and `SearchlightWindowSet` | A second spatial searchlight representation | Deprecate after locus-to-MVPA adapters land. No new code constructs it directly. |
| `multivar.IndexSet` and `RoiPlan` | Ordered, non-empty feature-coordinate blocks with multivar axis policy | Retain as multivar inputs. Add a narrow adapter from locus `Selection`; do not force multivar core to own spatial identity. |
| `connectivity.NodeAxis` | Keyed node metadata and scientific provenance | Retain and continue reusing `VertexBasis`. Optionally expose a locus finite-space package for node-indexed operations. |
| `connectivity.EdgeSpace` and `EdgeMask` | Ordered connectivity edges and Boolean edge selection | Adapt `EdgeSpace` to a locus finite space and `EdgeMask` to a region where useful. Preserve vectorization-order provenance. |
| `threshold.MaskedField` | Compacted active statistic field plus full-grid mapping | Back its support and mapping with locus values while retaining threshold-specific arrays. |
| `threshold.Region` | Scored scan candidate with id, bounding box, and prior mass | Rename to `ScanRegion` or `ThresholdRegion` and contain a locus region. It is not the neutral region type. |
| `latent.RadialMaskOrder` and radial selections | Representation layout, active/full-grid mapping, and decode policy | Retain layout policy; use locus selection and exact-map adapters for indexing. |
| `group.GroupSpace.ParcelAxis` | Group-result sample-axis description | Keep as a group-layer axis; add an atlas/locus adapter only when a concrete workflow requires it. |
| `zarr.CopyRegion`, graphics fields/domains, and unrelated statistical “spaces” | Physical slices, rendering values, or mathematical model spaces | Out of scope. Similar names do not make them finite neuroimaging regions. |

## 8. Public API and Failure Rules

### 8.1 Checked construction

Public constructors return typed `Either[LocusError, A]` values. Convenience
throwing constructors may exist only when clearly named or consistent with an
existing module's established `unsafe` convention.

Package-private constructors may accept owned primitive arrays after a checked
boundary. Public array inputs and exports copy unless ownership transfer is
explicit and inaccessible to ordinary callers.

### 8.2 Space mismatch

Operations that combine separately held values check `SpaceKey` and size even
when their phantom parameter is the same. They return `SpaceMismatch` rather
than relying on `require` or silently comparing ordinals.

Hot loops may use a checked `SameSpace` witness or package-private unsafe
method after one boundary check.

### 8.3 Raw integers

Raw ordinals are private implementation details. Adapters may accept legacy
integer APIs, but they validate once and convert to `Point`, `Region`, or
`Selection`. A new public API does not exchange `Vector[Int]` when its values
mean points in a known finite space.

### 8.4 Identity and metadata

`SpaceKey`, atlas identity, parcel identity, label text, and provenance are
different values. Relabeling a parcellation can preserve its blocks. Two atlas
releases can use the same display labels and still be different atlases.

### 8.5 Exact versus approximate operations

Every transport API states one of:

- exact total map;
- exact injection, surjection, or bijection;
- crisp relation;
- continuous coordinate transform;
- sampled/interpolated operator;
- future weighted or stochastic kernel.

No Boolean `resample` flag chooses among these semantics.

## 9. Detailed Migration Plan

Each phase is a separate mote child. A phase includes shared JVM and Scala.js
tests unless it is explicitly a JVM-only IO adapter.

### Phase 0. Ratify ownership and duplicate-abstraction disposition

Deliver this document from a live source and tracker inventory. Record module
ownership, non-goals, migration dispositions, dependencies, risks, and release
gates. Lodge the implementation phases under one epic.

Exit criteria:

- the document names every known overlapping abstraction;
- `docs/module-relations.md`, `README.md`, and `build.sbt` are not modified
  while their unrelated dirty changes are unowned;
- the mote epic and children point back to this document;
- no implementation is claimed.

### Phase 1. Establish finite spaces, points, regions, selections, and maps

Add `locus-kernel` with:

- `SpaceKey`, `FiniteSpace`, `SomeFiniteSpace`, and `Point`;
- `Region` and `Selection`;
- typed error ADTs;
- `TotalMap` and validated injection/surjection/bijection evidence;
- identity, composition, pullback, existential image, universal image, and
  image-supported universal image;
- immutable storage with defensive public ownership.

Before finalizing storage, benchmark a sorted primitive representation and
Scala immutable `BitSet` on representative sparse ROIs, dense masks, Boolean
operations, and Scala.js. Keep the representation private so this choice can
change.

Exit criteria:

- no production dependency;
- exhaustive Boolean and exact-map tests on JVM and Scala.js;
- direct tests of both adjunctions, Frobenius reciprocity, and vacuous truth;
- public construction cannot create an out-of-bounds point or mismatched
  region;
- the module README states identity and ordering semantics.

### Phase 2. Add relations and graph interop

Add sparse `Relation`, identity, composition, converse, union, image, erosion,
and validated relation evidence.

Add explicit adapters between:

- `VertexBasis` and a locus finite-space package;
- directed/undirected simple graphs and relations;
- graph adjacency and reflexive or loop-free relation policies.

Do not replace `VertexBasis`, `VertexIx`, or `Graph`. Do not discard keyed
metadata or edge values during conversion without an explicit function.

Exit criteria:

- category, dagger, union-distributivity, and image/erosion adjunction laws;
- optimized sparse results match a dense Boolean reference;
- identity relations retain loops while graph conversion handles them
  explicitly;
- a graph can reuse a locus space without a second keyed-axis implementation.

### Phase 3. Add parcellations, fields, sections, searchlights, and aggregation

Add `locus-data` with:

- `IndexedField` and `Section`;
- `Parcellation[X, P]` with typed parcel codomain and background support;
- partition equality up to relabeling;
- surjective coarsening and network composition;
- `Searchlight[S]` and centered evidence;
- one-pass `foldMapBy`;
- exact and tolerant aggregation test policies.

The first release includes common refinement if it can be implemented through
compact observed pairs without complicating the public type. Common
coarsening may remain a later operation within this phase if its union-find
implementation and identity semantics are fully tested.

Exit criteria:

- fibers are disjoint and cover support;
- every parcel point has a non-empty fiber;
- coarsening is functorial and fibers of a coarsening equal unions of source
  fibers;
- restriction identity, intersection, and map-commutation laws pass;
- exact aggregation fusion passes for lawful monoids;
- one-pass aggregation does not allocate a source-sized temporary array per
  parcel.

### Phase 4. Publish the reusable law and reference-model gate

Add `locus-laws` as a cross-platform test-support artifact.

The suite exhaustively enumerates:

- all regions for small finite spaces;
- all total maps where a total map exists;
- all bounded small relations;
- supported parcellations and coarsenings;
- small fields and sections.

ScalaCheck supplements exhaustive cases for larger sparse layouts and
constructor sequences. Consumer modules use the same law groups for their
adapters.

Exit criteria:

- law definitions do not depend on production representations;
- every optimized locus implementation is differentially checked;
- JVM and Scala.js execute the shared laws;
- generator bounds prevent combinatorial test explosions;
- numeric laws distinguish exact algebra from approximate floating behavior.

### Phase 5. Migrate image regions and volumetric searchlights

Give each exact volume domain an explicit locus space package. Back
`VoxelRegion` and `VoxelSelection` with locus membership and order while
preserving compatibility.

Split volumetric searchlights into:

1. construction of a `Searchlight[V]`;
2. optional restriction/materialization into `ROIVolWindow`;
3. MVPA or local-statistic execution over the relation rows.

Standardize the new metric-ball constructor on `distance <= radius`, permit
radius zero, and move nonzero-value support into field restriction. Preserve
legacy behavior only through named deprecated methods where source
compatibility requires it.

Exit criteria:

- existing image region algebra remains behaviorally compatible where its
  semantics are unchanged;
- exact grid/space mismatch remains a typed failure;
- `K_0 = I`, radius monotonicity, symmetry, and
  `K_r ; K_s subsetOf K_(r+s)` pass on synthetic metric grids;
- materialized windows match relation-row extraction;
- image JVM and JS suites and downstream adapter suites pass.

### Phase 6. Migrate surface domains, labels, parcels, and searchlights

Use `SurfaceMeshDomain` as the exact topology/order source. A surface locus
space key includes hemisphere and ordered topology identity; coordinate
surface kind remains geometry, not point identity.

Adapt:

- `SurfaceField` to `IndexedField`;
- `SurfaceRoi` to region plus section plus annotation;
- `LabeledSurface` to `Parcellation`;
- geodesic neighborhoods to `Searchlight`.

Keep fragmentation, centroids, medoids, boundaries, and contact counts in
`surface`. They are geometric operations over parcellation fibers.

Exit criteria:

- equal vertex counts with different ordered topology cannot share a space;
- volume and surface metric balls use the same closed-boundary convention;
- radius zero returns the center for a proper metric;
- disconnected label fibers remain valid core parcels;
- each fragmentation policy is an explicit derived surface operation;
- surface JVM and JS suites pass.

### Phase 7. Rebase atlas on typed parcellations

Rename atlas metadata values so `Region` no longer denotes metadata in the new
API. Keep deprecated aliases while downstream callers migrate.

Back `VolumeAtlas` and `SurfaceAtlas` with:

- a typed `Parcellation[X, P]`;
- parcel metadata as `IndexedField[P, AtlasRegionMetadata]`;
- explicit display order as `Selection[P]`;
- optional validated parcel-to-network surjection;
- atlas identity and release provenance.

Rework:

- reductions through one-pass aggregation;
- overlap through exact same-space evidence or an explicit alignment plan;
- parcel adjacency through the relational semantic reference;
- optimized boundary/contact counts as a distinct weighted result.

Exit criteria:

- atlas labels and metadata no longer define extensional region equality;
- volume and surface atlases expose the same quotient-level operations;
- hidden nearest resampling is absent from the new overlap API;
- network masks are derived from composition;
- optimized parcel adjacency matches the relation reference on fixtures;
- existing atlas parity fixtures pass on JVM and Scala.js where applicable.

### Phase 8. Integrate spatial and dataset selection boundaries

In `spatial`:

- expose a locus space package from every finite sampled `Domain`;
- strengthen surface mask compatibility to exact topology/order;
- rename the bounding-box `VoxelRegion` to `VoxelBox`;
- lower `Region` and `Selection` into field demands;
- distinguish exact `TotalMap`, crisp `Relation`, and sampled operator
  lowering;
- retain `spatial.Field` as the lazy runtime.

In `dataset`:

- attach explicit semantic space identity to `DatasetShape` or its resolved
  acquisition domain;
- preserve `All` versus `AllSpatial` as dataset availability policy;
- back resolved ordered reads with `Selection`;
- express active-to-full-grid mapping as an injection plus checked reverse
  lookup;
- preserve requested row order in returned data.

Exit criteria:

- no geometry-only check silently establishes semantic identity;
- structured demands cannot be applied to the wrong domain;
- dataset selection and image selection adapters agree on order and bounds;
- lazy field planning remains inspectable and does not materialize merely to
  create a region;
- spatial and dataset JVM and JS suites pass.

### Phase 9. Remove duplicate MVPA and multivar spatial adapters

Add direct conversions from:

- `Region` or `Selection` to `mvpa.FeatureSet`;
- `Parcellation` to regional `FeatureSetPlan`;
- `Searchlight` to searchlight `FeatureSetPlan`;
- locus selections to multivar feature `IndexSet` and `RoiPlan`.

Deprecate `mvpa-spatial.SearchlightCenter`, `SearchlightWindow`, and
`SearchlightWindowSet`. Keep `FeatureSet`, `FeatureSetPlan`, `RoiAnalysis`, and
multivar feature plans because they carry algorithm-level semantics not owned
by locus.

Exit criteria:

- MVPA has one spatial searchlight source of truth;
- feature order is supplied by an explicit selection or ambient-domain order;
- center inclusion is validated once by centered-searchlight evidence;
- no MVPA classifier depends on volume or surface geometry construction;
- MVPA and multivar focused JVM and JS suites pass.

### Phase 10. Adapt connectivity, threshold, and latent consumers

In `connectivity`, expose node and edge finite spaces without replacing
`VertexBasis`, scientific axis provenance, or vectorization order. Adapt
`EdgeMask` to a region where Boolean set operations are useful.

In `threshold`, rename its scored `Region`, retain bounding box and prior mass
as algorithmic metadata, and use locus membership plus the compact-to-volume
exact map.

In `latent`, reuse locus selection and exact maps for active/full-grid
transport while retaining representation layout and decode policies.

Exit criteria:

- adapters preserve scientific axis and layout provenance;
- no raw index is interpreted in a different ambient domain;
- threshold candidate scoring remains unchanged;
- connectivity vectorization order remains explicit;
- latent active/mask order remains exact and round-trippable;
- focused JVM and JS suites pass.

### Phase 11. Compatibility removal, performance, and release gate

Complete the deprecation ledger, dependency audit, documentation, examples,
benchmarks, and full repository verification.

Remove a compatibility type only after all repository consumers use the new
source of truth. If external compatibility requires keeping a wrapper for one
release, document its delegation and removal condition.

Exit criteria:

- no duplicate generic region, parcellation, relation, or searchlight remains;
- similarly named domain-specific values have unambiguous names and docs;
- `README.md`, `docs/module-relations.md`, build aggregates, and aliases match
  the implemented module graph;
- public arrays retain defensive ownership;
- focused benchmarks show no asymptotic regression and record allocations;
- `sbt compileAll` and `sbt testAll` pass warning-clean;
- both JVM and Scala.js evidence is recorded for every migrated feature;
- the epic closes only after all blocking child beads are closed.

## 10. Law and Test Strategy

### 10.1 Region laws

For regions in one space:

```text
A union empty = A
A intersect whole = A
A union A = A
A intersect A = A
A intersect (B union C) = (A intersect B) union (A intersect C)
A union complement(A) = whole
A intersect complement(A) = empty
A subsetOf B iff A intersect B = A
```

Construction history and storage representation do not affect equality.
Cross-space operations return a mismatch.

### 10.2 Map laws

```text
pullback(identity) = identity
pullback(g compose f) = pullback(f) compose pullback(g)
pullback preserves intersection, union, and complement
exists_f(A) subsetOf B iff A subsetOf pullback_f(B)
pullback_f(B) subsetOf A iff B subsetOf forall_f(A)
exists_f(A intersect pullback_f(B)) = exists_f(A) intersect B
```

The suite includes target points with empty fibers.

### 10.3 Relation laws

```text
(K ; L) ; M = K ; (L ; M)
I ; K = K = K ; I
converse(K ; L) = converse(L) ; converse(K)
converse(converse(K)) = K
composition distributes over union
image_K(A) subsetOf B iff A subsetOf allRelatedInside_K(B)
```

Dense Boolean matrices are the reference, not the production API.

### 10.4 Parcellation laws

```text
distinct fibers are disjoint
union of all fibers = support
every parcel has a non-empty fiber
coarsen(identity) = original
coarsen(f).coarsen(g) = coarsen(g compose f)
fiber of a coarsening = union of source fibers mapped to it
sameBlocksAs is invariant under bijective relabeling
```

### 10.5 Field and aggregation laws

```text
restrict(whole) = field
restrict(A).restrict(B) = restrict(A intersect B)
map(f).restrict(A) = restrict(A).map(f)
fold(A union B) = fold(A) combine fold(B), when A and B are disjoint
foldBy(n compose p) = foldBy(n) compose foldBy(p), for a lawful monoid
```

Floating-point tests state tolerances and accumulation policy.

### 10.6 Searchlight constructor laws

For closed metric balls:

```text
r <= s implies K_r subsetOf K_s
K_0 = identity
converse(K_r) = K_r
K_r ; K_s subsetOf K_(r+s)
```

k-nearest-neighbor constructors are tested for size and deterministic tie
policy, not symmetry.

### 10.7 Adapter differential tests

At minimum:

- legacy `VoxelRegion` versus locus-backed membership and Boolean results;
- legacy volume searchlight windows versus relation-row materialization;
- surface label fibers versus generic parcellation fibers;
- `AtlasReduce` results versus one-pass aggregation;
- `RegionGraph` Boolean support versus quotient adjacency;
- dataset resolved order versus locus selection order;
- MVPA feature plans versus parcellation/searchlight adapters;
- threshold compact/full-grid mapping round trips.

## 11. Representation and Performance Plan

The public algebra is representation-independent.

Candidate production representations are:

| Value | Initial candidate | Required property |
| --- | --- | --- |
| `Region[S]` | private sorted primitive array or immutable bit set, selected by benchmark | Fast membership and Boolean operations without exposing order |
| `Selection[S]` | owned primitive ordinal array plus derived region | Preserve requested order and reject duplicates |
| `TotalMap[X,Y]` | dense primitive target-ordinal array | O(1) lookup and one-pass fiber indexing |
| `Relation[X,Y]` | CSR sparse Boolean rows | O(1) row boundaries and O(nnz) traversal |
| `Parcellation[X,P]` | dense primitive parcel ordinals with `-1` internal background | O(1) label lookup and one-pass aggregation |
| `IndexedField[S,A]` | interface over existing storage | No mandatory materialization |
| weighted future | sparse numerical rows | Not part of crisp release |

Performance gates use representative sparse ROIs, dense masks, volume and
surface searchlights, atlas time-series aggregation, and parcel adjacency.

Required implementation discipline:

- validate once before hot loops;
- use primitive arrays and `while` loops in shared numeric/index kernels;
- avoid per-point `Option` allocation inside aggregation and relation
  composition;
- do not materialize every parcellation fiber during one-pass folds;
- keep row ordering deterministic for serialization and differential tests;
- measure JVM and Scala.js separately;
- retain specialized contact-count scans when they carry weights that a
  Boolean relation does not.

## 12. Compatibility and Removal Policy

Migration uses adapters before removals.

### 12.1 Compatibility wrappers

The first locus-backed releases keep:

- `image.VoxelRegion` and `VoxelSelection`;
- existing image ROI containers;
- `VolumeAtlas` and `SurfaceAtlas`;
- `mvpa.FeatureSet` and `FeatureSetPlan`;
- `spatial.Field`;
- graph and connectivity public axes.

Wrappers delegate to locus semantics or expose checked conversions. They do not
copy the algebra into a second implementation.

### 12.2 Required renames

The plan requires unambiguous names for:

- atlas metadata currently named `Region`;
- the spatial half-open box currently named `VoxelRegion`;
- threshold scored candidates currently named `Region`.

Deprecated aliases may bridge source compatibility, but new signatures and
documentation use the corrected names.

### 12.3 Semantic changes

The following changes are deliberate and require release notes:

- new metric searchlights accept radius zero;
- new metric balls use an inclusive boundary on volume and surface;
- value-based support filtering is separate from geometry;
- atlas overlap no longer resamples through a Boolean default;
- exact space identity is stricter than equal shape or vertex count;
- parcel/network identity is distinct from display label text.

### 12.4 Prohibited compatibility shortcuts

Do not add:

- global implicit conversions from `Int` to `Point`;
- implicit space-key derivation from grid dimensions;
- implicit image resampling;
- equality that ignores runtime space identity;
- deprecated wrappers with independent mutable or cached membership;
- a label-based fallback when parcel points do not align.

## 13. Risks and Decision Gates

### R1. Dynamic phantom-type ergonomics

Path-dependent dynamic spaces can make APIs awkward. Phase 1 must include
compile-only usage tests for loading a runtime volume, retaining its space
package, constructing regions, and passing them through atlas and MVPA
adapters. Ergonomics cannot be “fixed” by erasing `S` to `Any`.

### R2. Identity migration

Existing `NeuroSpace` values do not carry semantic acquisition identity.
Adapters must require a key from a domain, dataset, atlas, or explicit caller.
A temporary structural-grid key is allowed only in a named compatibility API
whose return value records that weaker basis.

### R3. Representation performance

Immutable `BitSet` may be good for dense masks and poor for sparse selections;
sorted arrays may show the reverse. Phase 1 benchmarks decide the private
default. A later roaring backend may be added only if JVM and Scala.js behavior
and ownership remain coherent.

### R4. Relation composition cost

Generic sparse Boolean multiplication can allocate heavily. The reference
implementation establishes correctness first. Specialized builders and
composition use row workspaces or sorted merges after differential tests are
green.

### R5. Floating-point fusion

Mathematical monoid laws do not make IEEE addition associative. Numeric
reducers document order, missing-value policy, and tolerance. Reproducible
summation may become a separate primitive if benchmarks and scientific
requirements justify it.

### R6. Migration breadth

The repository contains many domain-specific “selection” and “space” values.
This epic migrates only values that cross the finite spatial/parcel support
boundary. It does not force graphics, Zarr slicing, statistical model spaces,
or every temporal index into locus.

### R7. Dirty shared files

At plan ratification, `README.md`, `build.sbt`, and
`docs/module-relations.md` contain unrelated changes. Implementation phases
must preflight and reserve exact paths, preserve unrelated hunks, and defer a
shared file if ownership cannot be isolated.

### R8. Weighted extension pressure

Interpolation and probabilistic atlases may tempt callers to encode weights as
Boolean support. The crisp API exposes no weight slot. Weighted relations and
stochastic kernels require a separate plan with composition and normalization
laws.

## 14. Epic Acceptance Contract

The epic is complete only when all of the following are true:

1. `locus-kernel`, `locus-data`, and `locus-laws` exist as cross-projects.
2. The shared finite-space, region, selection, map, relation, parcellation,
   field, section, searchlight, and aggregation contracts are implemented.
3. The law suite passes on JVM and Scala.js against optimized and reference
   representations.
4. Image and surface construct neighborhoods through the same relation
   semantics.
5. Volume and surface atlas payloads expose typed parcellations.
6. Atlas metadata is not named or compared as an extensional region.
7. Atlas reduction is one-pass and atlas overlap requires explicit alignment.
8. Graph adjacency and locus relations interoperate without weakening graph
   invariants.
9. Spatial and dataset boundaries preserve exact space identity and requested
   order.
10. MVPA has no separate spatial searchlight model.
11. Connectivity, threshold, and latent adapters preserve their
    domain-specific provenance and policies.
12. The compatibility ledger identifies every remaining wrapper and its
    removal condition.
13. Module documentation and build aliases match the implemented graph.
14. Focused benchmarks and allocation receipts are recorded.
15. `sbt compileAll` and `sbt testAll` pass warning-clean, including JVM and
    Scala.js.

## 15. Tracker Mapping

The authoritative implementation order and dependency edges are stored in
mote under epic `bd-01KYFTRA5BVGEBJN3VKBA256B4`. The umbrella epic is blocked
by phase 11, so it cannot become ready for closure while a blocking delivery
phase remains. The child graph is:

| Phase | Mote bead | Blocking predecessors |
| --- | --- | --- |
| 0. Ratify plan | `bd-01KYFTRWJ168EAKS91N8PM99KR` | none |
| 1. Finite spaces, regions, selections, maps | `bd-01KYFV8XSG61M781294YNAPCY4` | phase 0 |
| 2. Relations and graph interop | `bd-01KYFV8ZHHZJGF5YSADJSZ8P3G` | phase 1 |
| 3. Parcellations, fields, searchlights, aggregation | `bd-01KYFV8ZYAAN0SZX2398AC2K8Q` | phase 2 |
| 4. Reusable laws and reference model | `bd-01KYFV909RDY5C72MDTHCQCYDF` | phase 3 |
| 5. Image migration | `bd-01KYFV90MHQNCMC88JX6VT7YH0` | phase 4 |
| 6. Surface migration | `bd-01KYFV90Z579EBDCDZGR3EYJ1A` | phases 4 and 5 |
| 7. Atlas migration | `bd-01KYFV91A05PSWFC3QPDRZ52DN` | phases 5 and 6 |
| 8. Spatial and dataset integration | `bd-01KYFV91N5EPKMCD57Q3G25T3G` | phases 5 and 6 |
| 9. MVPA and multivar adapters | `bd-01KYFV9208NQQ34JD7G6R11K0S` | phases 7 and 8 |
| 10. Connectivity, threshold, latent adapters | `bd-01KYFV92BKSA03J17S46A7H4N6` | phases 7 and 8 |
| 11. Release gate and compatibility removal | `bd-01KYFV92Q4NXAS8S61T507FT3D` | phases 9 and 10 |

Mote issue bodies summarize the deliverable and acceptance criteria. This
document remains the governing source when a short tracker body omits detail.
