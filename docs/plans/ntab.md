# Dynamic Neuro Tables (`ntab`) Plan

Status: planned
Mote epic: not yet created

## Decision

ScalaFIM should add a cross-platform `ntab` module for dynamic, row-oriented
analysis of neuroimaging features. Its public package is `scalafim.ntab`, its
artifact name is `scalafim-ntab`, and the primary user-facing value is
`NTable`.

The primary use case is interactive analysis in which columns and features are
discovered at runtime. The module therefore must not require users to declare a
single static metadata case class before exploring a table. Instead, a runtime
schema is inspected once and columns are bound to typed handles:

```scala
val group = table.meta[Group]("group").orThrow
val age = table.meta[Years]("age").orThrow
val site = table.meta[SiteId]("site").orThrow
val stat = table.volume[Double]("stat_map").orThrow
```

After successful binding, expressions and operations are typed:

```scala
table
  .where(group === Group.Patient && age >= Years(40.0))
  .groupBy(group, site)
  .summarize(stat, VolumeReducer.Mean)
```

The compiler should reject nonsensical combinations such as comparing `Years`
with `Group`, applying a numeric reducer to text, or applying a surface reducer
to a volume feature. Runtime validation remains responsible for facts that
arrive from data: whether a named column exists, whether values decode as the
requested type, and whether concrete grids, topologies, or parcel bases are
compatible.

The explicit `.orThrow` above is the notebook convenience. Library and pipeline
code uses the underlying `Either[NTabError, A]`; binding never fails through an
unmarked exception path.

Runtime code generation is not part of the initial module. A future JVM-only
tool may compile a discovered `NTableSchema` into case classes and phantom
support tags, but `ntab` must remain complete and ergonomic without a Scala
compiler at runtime.

No public identifier or file format in this design adopts the previously
discussed manifest acronym associated with non-fungible tokens. The R project
under `~/code/neurotabs` remains a source of workflow ideas and parity fixtures,
not a naming, storage, or API contract.

## Motivation

Neuroimaging analyses commonly manipulate collections whose rows are subjects,
sessions, contrasts, estimates, or derived maps. Each row carries ordinary
design metadata and one or more large feature values such as:

- a `NeuroVol[Double]`;
- a sparse or masked volume;
- a `SurfaceField[Double]`;
- parcel values indexed by an atlas;
- a vector or matrix on another explicitly described support.

ScalaFIM already owns the feature values and their geometry. What is missing is
the relational layer that can express:

```text
filter -> group -> resolve -> reduce -> compare -> drill
```

without flattening images into scalar table columns or discarding their spatial
support.

The reference R implementation demonstrates the value of separating logical
features from physical storage, retaining observation identity, filtering
without resolving feature payloads, grouping by arbitrary factors, and tracing
summaries back to contributing rows. It also shows what ScalaFIM should improve:
stringly feature kinds, runtime expression evaluation, nullable sentinels,
storage-specific branches in computation, and per-row resolution should not be
the center of the Scala API.

## Relationship to Existing Modules

`ntab` is a new leaf analysis module, not a replacement for an existing table
or dataset abstraction.

### `image`, `surface`, and `atlas`

These modules own concrete feature values and exact support:

- `image` owns volume values, spaces, masks, and coordinate semantics;
- `surface` owns geometry, topology, vertex-indexed fields, and surface ROIs;
- `atlas` owns parcel identity, region order, and parcel values.

`ntab` composes these types. It must not introduce a universal `NeuroArray`, a
parallel volume/surface hierarchy, or string descriptions that replace their
existing invariants.

Volume support must reuse the narrowest existing `image` value:

- dense `NeuroVol[A]` uses `VolumeSpace`;
- sparse `SparseNeuroVol[A]` uses its existing `VoxelIndexSet`;
- aligned `ROIVol[A]` uses the `VoxelIndexSet` obtained from its `VoxelRoi`;
- masks remain `NeuroVol[Boolean]` and lower through `Mask.indexSet` when an
  indexed support is required.

`VoxelIndexSet` already couples an exact `VolumeSpace` to validated ordered
unique voxel indices. `ntab` must not introduce a parallel `VoxelSupport` or
reimplement mask/ROI validation. `NeuroImageView[A, D]` may help concrete
dense-image adapters read values, but it is not the universal feature
abstraction: sparse volumes, surface fields, and parcels have distinct support
and reconstruction contracts.

### `dataset`

`dataset` owns run-oriented fMRI data, timepoint/voxel selection, lazy response
sources, and `FmriSeries`. An `NTable` instead represents a collection of
observations whose features may be volumes, surface fields, or parcels. It does
not become a new `DatasetBackend`, and `dataset` must not acquire a dependency
on `ntab`.

Adapters from a `DatasetIndex` or fit results into an `NTable` may live in
downstream integration modules once concrete workflows require them.

### `group`

`NTable.groupBy` is a descriptive relational operation: it partitions rows by
one or more typed keys so reducers can compute summaries. The `group` module is
inferential: it owns subjects-by-samples effect matrices, second-level design
matrices, variance-aware estimators, group contrasts, and reference
distributions. A metadata factor must not become a `GroupDesign` term merely
because it appeared in `groupBy`.

The core modules remain independent. `ntab` does not depend on `group`, and
`group` does not depend on `ntab`. A separate `group-ntab` adapter above both
modules prepares typed `GroupData[V]` from selected table rows and reconstructs
group result maps as derived `NTable` features. This preserves optionality and
avoids pulling `dataset`, `design`, `fit`, and `linalg` into the table core.

The bridge must preserve the `group` module's variance capability type. An
effects-only table prepares `GroupData[VarianceCapability.EffectsOnly]`; a
table with aligned positive variance features prepares
`GroupData[VarianceCapability.WithVariances]`. It must not erase this
distinction to `Option` before model construction.

### MVPA `PatternTable`

`PatternTable` is a dense sample-by-feature numerical matrix with a feature
mapping. It is already the right type for MVPA kernels. `NTable` is a
heterogeneous observation relation with deferred feature payloads. A future
adapter may resolve an `NTable` feature into a `PatternTable`, but the two types
must not merge.

### `spatial.Field`

`spatial.Field` represents a numerical samples-by-observations field on one
domain, including lazy spatial operators. It is an appropriate lowering target
for an aligned numerical `NTable` feature column, not the container for
metadata queries or grouped provenance. A bridge is deferred until a concrete
cross-domain workflow requires it.

### `multivar`

`multivar` consumes row-aligned matrices and owns their mathematical geometry.
It does not own metadata filtering or feature resolution. `ntab` may prepare
aligned materialized inputs for `multivar`; `multivar` remains independent.

## Dependency Target

The initial module is a shared JVM/Scala.js `crossProject`:

```scala
lazy val ntab =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/ntab"))
    .dependsOn(image, surface, atlas)
    .settings(commonSettings)
    .settings(
      name := "scalafim-ntab"
    )
    .jsSettings(jsSettingsBase)
```

The direct dependencies are intentional because the public module supplies
typed feature bindings and reducers for all three domains. It does not depend
on `dataset`, `spatial`, `group`, `fit`, `mvpa`, `multivar`, `pipeline`, or a
Scala compiler.

The first group-inference integration is a separate cross-platform leaf:

```scala
lazy val groupNtab =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/group-ntab"))
    .dependsOn(group, ntab)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-group-ntab"
    )
    .jsSettings(jsSettingsBase)
```

Its package is `scalafim.fmri.group.ntab`. This follows the existing
algorithm-plus-adapter shape of `mvpa-dataset` and `mvpa-spatial`; no dependency
points from a lower computational module back to an optional relational front
end.

`ntabJVM` owns JVM-only resource resolution and file adapters. Shared query
plans contain only immutable descriptions, never paths opened as handles,
mutable decoder registries, threads, or scheduler objects.

The initial source layout should be:

```text
modules/ntab/
  README.md
  shared/src/main/scala/scalafim/ntab/
    Ids.scala
    Error.scala
    ScalarType.scala
    Schema.scala
    Column.scala
    ColumnStore.scala
    Expression.scala
    NTable.scala
    Query.scala
    Grouping.scala
    Feature.scala
    NumericFeature.scala
    Resolution.scala
    Reduction.scala
    Provenance.scala
    VolumeFeatures.scala
    SurfaceFeatures.scala
    ParcelFeatures.scala
  shared/src/test/scala/scalafim/ntab/
  jvm/src/main/scala/scalafim/ntab/io/
  jvm/src/test/scala/scalafim/ntab/io/

modules/group-ntab/
  README.md
  shared/src/main/scala/scalafim/fmri/group/ntab/
    GroupNTableError.scala
    GroupDesignSpec.scala
    GroupInput.scala
    GroupOutput.scala
  shared/src/test/scala/scalafim/fmri/group/ntab/
```

The exact file split may shrink during implementation. Public concepts should
not be collapsed into a single large dispatcher.

## Core Model

### Identity

Use opaque types for identities and names:

```scala
opaque type TableId = String
opaque type RowId = String
opaque type ColumnId = String
opaque type ColumnName = String
opaque type FeatureId = String
opaque type ResourceId = String
```

Smart constructors validate non-empty canonical forms. `ColumnId` and
`FeatureId` are stable identities; display names may be renamed without
changing lineage. Every root row has a unique `RowId`. A derived row has its own
`RowId` plus explicit contributor lineage.

An internal dense `RowIx` may be used for local indexing but is never stable
identity and is never serialized as one.

### Dynamic schema

`NTableSchema` is an immutable ordered schema:

```scala
final case class NTableSchema private (
  tableId: TableId,
  metadata: Vector[MetaColumnSchema],
  features: Vector[FeatureColumnSchema],
  rowId: ColumnId
)
```

Construction must reject:

- duplicate column or feature identities;
- duplicate names within one namespace;
- a row-id column with an invalid scalar type or missing values;
- invalid feature shape/support combinations;
- references to unknown resources or selectors when those are declared in an
  in-memory construction request.

Metadata and feature namespaces are separate. If a single lookup syntax is
later offered, ambiguity must be reported rather than resolved by precedence.

### Scalar types and missingness

The first scalar type algebra should be closed and portable:

```scala
enum ScalarType:
  case Text
  case Int32
  case Int64
  case Float64
  case Bool
  case Category(domain: CategoryDomain)
```

Dates, timestamps, JSON, decimals, and unsigned widths are deferred until a
consumer demonstrates their required semantics on both JVM and Scala.js.

Storage is columnar. Primitive columns use primitive arrays plus a validity
bitmap; missingness must not require one `Option` allocation per cell. Public
columns are immutable even if their private representation uses arrays.

Custom domain values bind through a contextual codec:

```scala
trait ScalarCodec[A]:
  def scalarType: ScalarType
  def decode(value: ScalarValue): Either[NTabError, A]
  def encode(value: A): ScalarValue
```

Opaque types such as `Years`, `SiteId`, and `SubjectId` receive explicit
instances. A user may bind a categorical column to an enum only when the
codec validates every declared level. Merely observing a finite set of values
does not make the domain closed.

### Typed metadata handles

A successful runtime binding returns a typed handle:

```scala
final class MetaCol[A] private[ntab] (
  owner: TableId,
  id: ColumnId,
  codec: ScalarCodec[A]
)
```

The primary binding API is:

```scala
def meta[A](name: String)(using ScalarCodec[A]): Either[NTabError, MetaCol[A]]
```

Binding validates existence, declared scalar compatibility, missingness
policy, and categorical domain compatibility. A handle retains its table owner
and column identity. Using a handle from an unrelated table is a typed runtime
error rather than accidental name-based capture.

Handle binding is the trust boundary. Query evaluation should not repeatedly
decode or revalidate the schema for every row.

### Feature schemas and handles

Feature payload type and support type remain explicit:

```scala
final class FeatureCol[A, S] private[ntab] (
  owner: TableId,
  id: FeatureId,
  support: S,
  codec: FeatureCodec[A, S]
)
```

`S` is an exact support descriptor or a narrow wrapper over one. Initial
bindings are:

```scala
FeatureCol[NeuroVol[A], VolumeSpace]
FeatureCol[SparseNeuroVol[A], VoxelIndexSet]
FeatureCol[ROIVol[A], VoxelIndexSet]
FeatureCol[SurfaceField[A], SurfaceSupport]
FeatureCol[ParcelData[A], ParcelSupport]
```

`SurfaceSupport` contains the `SurfaceGeometry` plus the ordered vertex indices
present in the field. Geometry alone is insufficient because `SurfaceField`
may represent an indexed subset of vertices. Full and indexed fields therefore
cannot be reduced together accidentally. `VolumeSpace` is sufficient for the
initial dense `NeuroVol`. Sparse volumes and ROI values reuse
`image.VoxelIndexSet`, including its exact volume space and ordered unique voxel
indices. A table column of aligned ROI values has one shared `VoxelIndexSet`;
arbitrary row-varying ROI shapes are set-valued features and require explicit
union/intersection semantics outside the first numerical reduction slice.

`ParcelSupport` should contain the atlas reference and ordered region identity
needed to validate a `ParcelData[A]`. It must not duplicate the atlas payload.

The capability tying values to support is:

```scala
trait FeatureCodec[A, S]:
  def supportOf(value: A): Either[NTabError, S]
  def validate(value: A, support: S): Either[NTabError, Unit]
  def equivalent(left: S, right: S): Boolean
```

`supportOf` delegates to the owning feature type; it does not infer support
from array length. `validate` checks the extracted support against the bound
column support and may also validate payload-specific invariants.

Numerical reduction and downstream matrix adapters require one additional,
narrow capability:

```scala
trait NumericFeature[A, S] extends FeatureCodec[A, S]:
  def size(support: S): Int

  def copyTo(
    value: A,
    destination: Array[Double],
    offset: Int
  ): Either[NTabError, Unit]

  def build(
    support: S,
    values: Array[Double],
    label: String
  ): Either[NTabError, A]
```

Concrete instances adapt `NeuroVol[Double]`, `SparseNeuroVol[Double]`,
`ROIVol[Double]`, `SurfaceField[Double]`, and `ParcelData[Double]`. They reuse
the constructors, spaces, index sets, geometry, and region order owned by
`image`, `surface`, and `atlas`. `copyTo` and `build` are the common
flatten/reconstruct boundary required by grouped reducers and the `group-ntab`
bridge; they are not a new universal neuroimaging container.

Equivalent Scala value types do not imply equivalent support. Two
`NeuroVol[Double]` columns may still have different grids. The initial grouped
reduction policy requires exact support. Resampling, surface registration, or
parcel correspondence must be requested through explicit future plans outside
the reducer.

### Feature sources

Feature payloads are per-row sources rather than scalar cells:

```scala
enum FeatureSource:
  case Materialized(ref: MaterializedFeatureRef)
  case External(resource: ResourceId, selector: FeatureSelector)
  case Derived(derivation: DerivationId)
  case Missing(reason: MissingReason)
```

The shared representation is descriptive. It does not store an open file or a
decoder closure. Materialized values live in an immutable table-local store
behind stable references. External resources are interpreted by an explicit
resolver capability.

Ordered fallback encodings are not part of the initial core. Ingest adapters
must normalize each row to one selected `FeatureSource` or return a typed
ambiguity/missing error. This keeps storage policy out of query and reduction.

## Expression Algebra

### Typed expressions

Filtering and derived metadata use a small typed expression tree:

```scala
sealed trait Expr[A]

object Expr:
  final case class Literal[A](value: A) extends Expr[A]
  final case class Column[A](column: MetaCol[A]) extends Expr[A]
  final case class Equal[A](left: Expr[A], right: Expr[A])
      extends Expr[Boolean]
  final case class Compare[A](
    left: Expr[A],
    right: Expr[A],
    order: ScalarOrder[A],
    op: ComparisonOp
  ) extends Expr[Boolean]
  final case class And(left: Expr[Boolean], right: Expr[Boolean])
      extends Expr[Boolean]
```

The implementation may use a closed enum or private subclasses, but public
construction is through extension methods:

```scala
group === Group.Patient
age >= Years(40.0)
site.isIn(SiteId("a"), SiteId("b"))
group.isMissing
```

Only operators justified by contextual capabilities are available. Ordering
operators require a `ScalarOrder[A]` with a stable descriptor and comparison
kernel; numeric arithmetic requires a narrower numeric capability. An
arbitrary application `Ordering[A]` is admitted only through an explicitly
local expression, because an opaque comparator cannot be explained or
interpreted portably. There is no universal `Any` comparison and no string
parser in the shared query API.

Expression evaluation carries a validity bit alongside scalar values. Boolean
composition follows Kleene three-valued logic. `where` retains only `true` and
drops both `false` and missing predicates, matching ordinary interactive data
analysis behavior; `isMissing` itself always produces a non-missing Boolean.

### Local escape hatch

Arbitrary Scala predicates are useful during exploration but cannot be
inspected, serialized, or interpreted remotely. If offered, they must be named
to expose the boundary:

```scala
table.filterLocal(row => ...)
```

`filterLocal` is an explicitly local terminal or materializing operation. It
must not masquerade as a portable query-plan node. It is deferred from the
first slice unless typed expressions prove insufficient for basic examples.

### Group keys

Grouping accepts typed metadata handles or expressions. Common interactive
forms should be concise:

```scala
table.groupBy(group)
table.groupBy(group, site)
table.groupBy(group, site, condition)
```

The public implementation may use heterogeneous tuples and match types
internally, but tuple machinery must not appear in error messages or ordinary
documentation. The inferred group key types are respectively:

```scala
Group
(Group, SiteId)
(Group, SiteId, Condition)
```

An explicit constructor maps grouping columns to a named key when desired:

```scala
table.groupBy(group, site).mapKey(Cohort.apply)
```

The overloads construct a typed `GroupSpec[K]`. `GroupedQuery[K]`, group keys,
summary rows, and contrast selectors retain that same `K`; they do not erase a
multi-factor key to `Vector[Any]`. A programmatic dynamic grouping route uses a
distinct `DynamicGroupKey` containing ordered `ScalarValue`s and column
identities. The first release may provide static arities one through three plus
this dynamic route. General tuple derivation is optional, not a blocker.

## Query and Execution

### Plan values

Queries are immutable inspectable values:

```scala
final case class NQuery private (
  source: NTable,
  steps: Vector[QueryStep]
)

enum QueryStep:
  case Filter(predicate: Expr[Boolean])
  case Arrange(order: OrderPlan)
  case Take(count: Int)
```

`OrderPlan` is a smart-constructed non-empty head/tail value. The public
`arrangeBy(first, rest*)` API cannot construct an empty ordering step.

Grouping changes the type and API:

```scala
GroupedQuery[K]
```

It is not represented by a Boolean flag on `NQuery`. Only a grouped query
offers `summarize`, `compareGroups`, and `groupKeys`. `ungroup` returns an
ordinary query without silently materializing it.

The first executor is local and deterministic. The plan types stay free of
scheduler vocabulary, captured closures, mutable caches, and open resources.
This leaves room for future block or distributed interpreters without making
them an initial dependency.

### Metadata execution

Filters, ordering, grouping, and row counts operate only on metadata columns.
They must not resolve feature payloads. The executor carries a dense row
selection or permutation over root row indices until a feature operation
requires materialization.

Repeated filters may be evaluated sequentially in version one. A general query
optimizer is deferred. A narrow filter-fusion pass is acceptable only after
behavior is locked by laws.

### Resolution plans

Feature resolution compiles selected rows into a pure plan:

```scala
final case class ResolvePlan[A, S](
  feature: FeatureCol[A, S],
  rows: RowSelection,
  tasks: Vector[ResolveTask],
  support: S
)
```

Planning must:

1. preserve the selected observation order;
2. reject missing values according to an explicit `MissingPolicy`;
3. group tasks by materialized store or external resource;
4. validate that every row declares support compatible with the bound feature;
5. produce enough information for batched reads without opening resources.

The interpreter capability is explicit:

```scala
trait FeatureResolver:
  def resolve[A, S](plan: ResolvePlan[A, S])
      (using FeatureCodec[A, S]): Either[NTabError, ResolvedFeature[A, S]]
```

An in-memory resolver exists in shared code. JVM-only resolvers may batch NIfTI
or surface reads, but they orchestrate the existing owning-module readers. A
NIfTI resolver delegates decoding and image construction to
`scalafim.image.io.Nifti`; it never implements a second NIfTI parser. If the
owning reader exposes only throwing entry points, add a narrow total
`readVolEither`/`readVecEither` API in `image` and map its error into
`NTabError`. Apply the same rule to surface readers. The module does not use a
mutable global backend registry. Applications assemble an immutable resolver
set and pass it explicitly.

## Reduction and Comparison

### Reducers

Reducers are typed values, not operation names:

```scala
trait FeatureReducer[A, S, B]:
  def reduce(
    values: ResolvedFeature[A, S],
    groups: GroupIndex,
    missing: MissingPolicy
  ): Either[NTabError, ReducedFeature[B, S]]
```

The initial numerical reducers are:

- mean;
- sum;
- variance;
- standard deviation;
- standard error.

Each concrete feature adapter first proves support extraction, flattening, and
reconstruction independently. Once those laws pass, reducers may share a
primitive-array kernel through `NumericFeature[A, S]`; support validation and
result construction remain in the concrete adapter. This avoids both copied
inner loops and a universal spatial value hierarchy.

Reducers must validate support once before entering primitive-array loops. Hot
loops use primitive arrays and `while`; they do not allocate one object per
voxel, vertex, parcel, observation, or group contribution.

### Missingness

Missingness policy is explicit:

```scala
enum MissingPolicy:
  case Error
  case Drop
  case Propagate
```

`Error` is the default. `Drop` records the effective contributor count for each
group. `Propagate` produces a missing derived feature rather than a numerical
sentinel. `Double.NaN` is never the public representation of a missing feature.

Reducers whose statistics become undefined after dropping values return a
typed error or missing result according to the policy; they do not silently
divide by zero.

### Summary results

Summaries return an object that exposes both the derived table and the typed
derived feature handle:

```scala
final case class SummaryResult[K, A, S](
  key: GroupSpec[K],
  table: NTable,
  feature: FeatureCol[A, S],
  keys: Vector[K],
  contributors: ContributorIndex,
  receipt: SummaryReceipt
)
```

A summary exposes typed `keys`, `rowFor(key: K)`, and contrast selection by
`K`, while its `table` retains the materialized key columns for dynamic
inspection. Users never have to rebuild a multi-factor key as strings or look
up the generated feature by an untyped name.

`SummaryReceipt` records reducer, missingness policy, source feature identity,
support fingerprint, group count, selected row count, and effective contributor
counts. It contains descriptions, not the feature payload itself.

### Comparisons

Group comparisons are typed plans over already-aligned summary values:

```scala
enum PointwiseContrast:
  case Subtract
  case Ratio
  case PercentChange
```

Ratio-like contrasts must define zero-denominator policy explicitly. A future
standardized contrast may carry its variance/scale requirements as a separate
type rather than being added as an under-specified enum member.

Arbitrary binary functions may be offered only as an explicitly local
operation. Built-in contrast plans remain inspectable and portable.

## Provenance and Drill-Down

Every root row has:

```scala
final case class RootLineage(table: TableId, row: RowId)
```

Every derived row has a non-empty ordered contributor set plus a non-empty
derivation path. Contributor identity is not serialized into a magic metadata
column.

```scala
final case class DerivedLineage(
  contributors: ContributorSet,
  path: DerivationPath
)
```

`ContributorSet` and `DerivationPath` are private-constructor head/tail domain
records with validated smart constructors. `ntab` does not reuse the generic
`NonEmptyVector` currently located in `atlas`, because table lineage should not
depend on an atlas-local utility by accident.

`drill` uses lineage directly:

```scala
summary.drill(summaryRow)
```

It returns the contributing root-row selection in deterministic source order.
If several source tables were concatenated, lineage retains their original
`TableId`s and does not assume globally unique row names.

## Concatenation and Compatibility

Concatenation is a metadata and logical-feature operation, not a physical
storage merge.

Version one supports strict row concatenation when:

- metadata columns have the same stable identities and scalar types;
- feature columns have the same stable identities and payload types;
- feature supports are exact-equivalent;
- row and table lineage can be retained without identity collision.

Safe scalar widening may be added later through an explicit coercion plan.
Columns are not aligned by display-name coincidence. Different external
resource layouts may concatenate because compatibility is defined over the
bound logical feature and support, not the resource location.

Union-by-name, joins, pivots, and wide-to-long reshaping are deferred. They
should be driven by concrete analysis workflows rather than an attempt to
recreate a full general-purpose dataframe library.

## Group Inference Bridge

The `group-ntab` module is an explicit bidirectional adapter, not a method
hidden behind ordinary `NTable.groupBy`:

```text
NQuery + typed columns
  -> group-ntab preparation
  -> GroupData[V] + GroupDesign + GroupSupportReceipt[S]
  -> GroupModel / GroupEngine
  -> GroupResult
  -> derived NTable + contributor lineage
```

### Preparing group inputs

The primary input is a long-form selected query with one effect feature per
subject and first-level contrast:

```scala
GroupNTable.effectsOnly(
  query = selected,
  subject = subjectId,
  firstLevel = contrastName,
  effect = estimate
)

GroupNTable.withVariances(
  query = selected,
  subject = subjectId,
  firstLevel = contrastName,
  effect = estimate,
  variance = estimateVariance
)
```

The adapter supplies `ScalarCodec` instances for `dataset.SubjectId` and the
opaque names owned by `group`; those codecs do not belong in `ntab` core.
Preparation must:

1. retain deterministic subject order;
2. reject duplicate or missing subject-by-contrast cells;
3. require metadata used by the design to be constant within subject across
   repeated contrast rows;
4. resolve effect and variance features in the same row order;
5. require exact-equivalent effect and variance support;
6. use `NumericFeature[A, S]` to construct row-major
   subjects-by-samples `DoubleMatrix` values;
7. call `GroupData.effectsOnly` or `GroupData.withVariances` so variance
   availability remains visible in the result type;
8. retain a `ContributorIndex` from group subjects back to root `NTable` rows.

A wide convenience constructor may accept explicitly named effect/variance
feature pairs, but it lowers to the same validated representation. Neither
route aligns cells by incidental row position or display-name coincidence.

### Constructing the second-level design

`group-ntab` builds a design from the prepared subject axis, not from a raw
snapshot of `design.data.DataTable`. The first design algebra supports:

- intercept-only models;
- numeric covariates selected through typed `MetaCol[A]` values with an
  explicit numerical encoder;
- two-level treatment coding with an explicit reference level;
- already-encoded matrices with typed term names and an explicit subject axis.

Categorical reference levels, contrast coding, and interactions are scientific
policy. They must be present in `GroupDesignSpec`; the adapter must not choose a
reference from first-observed row order. The resulting matrix is validated by
`GroupDesign.fromTypedMatrix`. General formula parsing and automatic
interaction expansion are deferred.

Relational grouping and inferential design remain visibly different:

```scala
table.groupBy(site).summarize(effect, VolumeReducer.Mean) // descriptive

GroupNTable
  .withVariances(selected, subject, contrast, effect, variance)
  .design(GroupDesignSpec.treatment(group, reference = Group.Control))
  .model(GroupWeighting.RandomEffects())                  // inferential
```

A future `fitEach` operation may deliberately stratify a query and build one
`GroupModel` per `GroupSpec[K]`, but it must be named as model execution rather
than overloaded onto `summarize`.

### Sample support and `GroupSpace`

Before the voxel bridge lands, `group.GroupSpace.VoxelAxis` should reuse
`image.VoxelIndexSet` rather than storing a raw `NeuroSpace` plus
`Vector[Int]`:

```scala
final case class VoxelAxis(voxels: VoxelIndexSet) extends GroupSpace:
  def nSamples: Int = voxels.size
```

This is a narrow cleanup inside `group`, which already depends on `image`, and
removes duplicate bounds/order/uniqueness validation. Dense volumes use a full
grid `VoxelIndexSet`; sparse and ROI features use their existing index set.

The current `GroupSpace` has no exact surface or atlas case. The first bridge
uses `GroupSpace.SampleAxis` for the numerical engine while retaining the exact
`S` in `GroupSupportReceipt[S]`. The receipt proves sample count and order and
is required to reconstruct outputs. Do not add `surface` and `atlas`
dependencies to `group` merely for this adapter. Native surface/parcel group
spaces should be introduced only when group-level spatial inference consumes
their geometry directly.

### Reconstructing group outputs

`GroupNTable.fromResult` creates one derived row per first-level-contrast by
design-term or evaluated group contrast. Initial metadata includes:

- first-level contrast identity;
- design term or group contrast identity;
- weighting and reference statistic;
- residual degrees of freedom where applicable;
- source model/receipt identity.

Typed feature columns reconstruct estimate, standard-error, statistic,
p-value, and optionally adjusted-p-value maps through the original
`NumericFeature[A, S]`. Meta-analytic fits may additionally expose `tau2`, Q,
and I2 maps. Every derived row records the contributing subject rows and the
group model receipt; support is never inferred from result-vector length.

The adapter has its own error ADT:

```scala
enum GroupNTableError:
  case Table(error: NTabError)
  case Group(error: GroupError)
  case DuplicateCell(subject: SubjectId, contrast: FirstLevelContrastName)
  case MissingCell(subject: SubjectId, contrast: FirstLevelContrastName)
  case SubjectMetadataConflict(subject: SubjectId, column: ColumnId)
  case EffectVarianceSupportMismatch(feature: FeatureId)
  case ResultSupportMismatch(expected: Int, actual: Int)
```

Ordinary adapter failures remain values. The bridge never catches a group
failure and converts it to a missing output row.

## Static Views and Optional Code Generation

A stable analysis may request a typed record view:

```scala
final case class StudyMetadata(
  group: Group,
  age: Years,
  site: SiteId
)
```

An eventual `asMetadata[StudyMetadata]` may use a `Mirror`-derived or explicit
row codec. This is a convenience over the dynamic schema, not the primary data
model.

Runtime source generation belongs in a separate future JVM-only artifact such
as `scalafim-ntab-codegen`. That tool may consume `NTableSchema`, emit stable
Scala source, compile it, and expose it to later notebook cells. Requirements
before accepting such a tool are:

- deterministic source keyed by schema fingerprint;
- generated source retained for inspection;
- compiler version pinned to the session version;
- sanitized identifiers and no manifest-provided code execution;
- bounded classloader/cache lifecycle;
- runtime binding that still validates actual payloads;
- no compiler dependency in `ntab` or its shared API.

Code generation is not required for any version-one acceptance criterion.

## Version-One Scope

Version one includes:

- validated dynamic metadata schema;
- immutable columnar metadata storage;
- typed metadata handle binding;
- typed filter expressions;
- deterministic arrange and row selection;
- grouping by one to three metadata expressions;
- materialized dense-volume, sparse-volume, aligned-ROI, surface-field, and
  parcel feature columns;
- in-memory feature resolution;
- concrete `NumericFeature[A, S]` instances for those numerical feature kinds;
- exact-support mean, sum, variance, standard deviation, and standard error;
- explicit missingness policy;
- summary receipts, contributor lineage, and drill-down;
- strict row concatenation;
- shared JVM and Scala.js tests;
- an end-to-end in-memory example covering dense/sparse/ROI volume, surface,
  and parcel values.

Version one does not include:

- a public manifest or interchange format;
- a mutable global backend registry;
- NIfTI, GIFTI, HDF5, Zarr, Arrow, or database readers;
- arbitrary joins, pivots, windows, or SQL parsing;
- runtime Scala compilation;
- a general query optimizer;
- distributed execution;
- implicit resampling or cross-topology correspondence;
- arbitrary user closures inside portable plans;
- replacement of `PatternTable`, `FmriSeries`, `spatial.Field`, or existing
  image/surface/atlas values;
- second-level group-model execution inside `ntab` core.

The first version intentionally proves the relational and spatial algebra with
materialized feature values. `group-ntab` is the first integration milestone;
external resolution is a separate follow-on once the public grammar is stable.

## Implementation Plan

### Milestone 0: API spike and fixtures

Create a compile-only API spike in tests before committing the full internal
model. The spike must demonstrate:

```scala
val controls =
  table.where(group === Group.Control && age >= Years(40.0))

val means =
  controls.groupBy(site, condition).summarize(statMap, VolumeReducer.Mean)

val members =
  means.drill(means.rows.head)
```

Add tiny deterministic fixtures:

- four observations with two categorical factors and one numeric covariate;
- `2 x 2 x 2` volumes on one exact grid;
- sparse and ROI values on one ordered `VoxelIndexSet`;
- a tetrahedral surface with one field per observation;
- three-region parcel values with a fixed region order;
- deliberately mismatched grid, topology, parcel order, missing value, and
  duplicate row-id variants.

Exit criteria:

- the intended API is concise without runtime code generation;
- negative compile tests demonstrate invalid comparison and reducer pairing;
- fixture semantics are written independently of implementation results.

### Milestone 1: schema, metadata store, and typed expressions

Implement:

- identity opaque types and `NTabError`;
- `ScalarType`, `ScalarValue`, `ScalarCodec[A]`, and category domains;
- `NTableSchema` and immutable columnar stores;
- root `NTable` construction with row-id validation;
- `MetaCol[A]` binding;
- `Expr[A]`, equality, ordering, Boolean composition, missingness checks;
- `NQuery.where`, `arrangeBy`, `take`, `count`, `describe`, and `explain`;
- the deterministic local metadata interpreter.

Tests:

- duplicate names/ids and invalid row-id schemas;
- typed binding success and scalar mismatch errors;
- handle-owner mismatch;
- category-domain mismatch;
- three-valued/missing predicate behavior as explicitly specified;
- stable filter and sort order;
- filter composition laws;
- metadata queries prove feature stores are untouched;
- JVM/JS parity.

Exit criteria:

- dynamic columns are bound once and used through typed expressions;
- no feature operation is required to exercise the metadata grammar;
- `ntabJVM/test` and `ntabJS/test` pass warning-clean.

### Milestone 2: feature catalog and exact support

Implement:

- `FeatureColumnSchema`, `FeatureSource`, materialized feature store;
- `FeatureCodec[A, S]` and `FeatureCol[A, S]`;
- `NumericFeature[A, S]` with independently validated concrete adapters;
- `VolumeSpace`, `VoxelIndexSet`, `SurfaceSupport`, and `ParcelSupport`
  bindings;
- version-one feature construction from already-loaded values;
- `ResolvePlan`, `ResolvedFeature`, and the shared in-memory resolver;
- `MissingPolicy`.

Tests:

- payload type and support binding errors;
- missing, duplicate, and out-of-order per-row feature sources;
- exact volume grid validation including affine/space semantics;
- exact sparse/ROI voxel-index order and uniqueness validation;
- exact surface geometry, topology, and ordered vertex-domain validation;
- exact atlas and region-order validation;
- selected-row order preserved through resolution;
- repeated metadata-only queries perform zero resolutions;
- JVM/JS parity.

Exit criteria:

- each feature type resolves through one typed public surface;
- dense, sparse, ROI, surface, and parcel numerical features flatten and
  reconstruct without losing support or element order;
- support mismatch fails before arithmetic;
- shared plans contain no IO or mutable registry state.

### Milestone 3: grouped reduction, comparison, and drill

Implement:

- `GroupedQuery[K]` and deterministic `GroupIndex[K]`;
- group-by arities one through three;
- dense-volume, sparse-volume, ROI, surface, and parcel reducers using the
  shared numerical kernel only after concrete adapter validation;
- `SummaryResult[K, A, S]`, `SummaryReceipt`, and derived feature handles;
- contributor lineage and `drill`;
- strict concatenation;
- typed pointwise subtraction and ratio with explicit denominator policy.

Tests:

- independent numerical oracles for every reducer;
- singleton, unbalanced, empty-after-filter, and all-missing groups;
- `Drop` contributor counts and `Propagate` missing results;
- group order independent of hash-map iteration;
- dense/sparse/ROI/surface/parcel equivalence on the same aligned synthetic
  values;
- drill returns exactly the contributing root rows in source order;
- concatenation preserves lineage across repeated row-name strings;
- primitive result buffers have the expected shape and order;
- JVM/JS parity.

Exit criteria:

- the canonical filter/group/summarize/compare/drill workflow is executable;
- result features never need to be rediscovered by untyped name;
- known support and missingness failures have structured errors.

### Milestone 4: ergonomics, documentation, and performance receipt

Add:

- `modules/ntab/README.md` with an interactive quick start;
- an `examples/ntab-jvm` example only if it adds value beyond shared tests;
- table/schema/plan descriptions suitable for a notebook display layer;
- a benchmark main for metadata filtering/grouping and numerical reduction;
- a performance receipt documenting allocations and scaling;
- README, `docs/module-relations.md`, root aggregate, `compileAll`, and
  `testAll` wiring.

The representative benchmark should include at least:

- 10,000 metadata rows with mixed numeric and categorical predicates;
- 1,000 materialized moderate-sized numerical feature values;
- grouped mean with several unbalanced groups;
- a proof that metadata filtering resolves no features;
- an allocation comparison against a deliberately boxed reference path.

Exit criteria:

- the example reads like an interactive analysis rather than a builder demo;
- JVM and JS tests pass;
- `sbt compileAll` and `sbt testAll` pass;
- the benchmark records actual timings and allocations without making an
  unsupported absolute performance claim.

### Milestone 5: `group-ntab` inference bridge

After the in-memory feature grammar is stable, implement the separate
`group-ntab` cross-project.

Implement:

- `ScalarCodec` instances for `SubjectId` and group-owned opaque names;
- long-form effects-only and effects-plus-variance preparation;
- `GroupDesignSpec` for intercept, numeric covariates, explicit-reference
  treatment coding, and already-encoded matrices;
- `PreparedGroup[V, A, S]` with typed variance capability, exact support,
  contributor index, and preparation receipt;
- `GroupSpace.VoxelAxis` migration to `VoxelIndexSet`;
- `GroupResult` and `GroupContrastResult` reconstruction as derived tables;
- `GroupNTableError` with lossless wrapping of table and group errors;
- README, `docs/module-relations.md`, root aggregate, `compileAll`, and
  `testAll` wiring for `group-ntab`.

Tests:

- shuffled input rows still produce deterministic subject order;
- missing and duplicate subject-by-contrast cells are rejected;
- conflicting repeated subject metadata is rejected;
- effect and variance support/order mismatches fail before matrix creation;
- effects-only input cannot construct variance-required models;
- explicit factor reference level is invariant to row order;
- dense, sparse, ROI, surface, and parcel inputs agree with independent
  subjects-by-samples matrix fixtures;
- reconstructed result maps preserve exact support and numerical values;
- output lineage contains exactly the contributing subject rows;
- JVM/JS parity for the bridge and its group-engine scenario.

Exit criteria:

- no dependency edge exists from `ntab` to `group` or from `group` to `ntab`;
- the bridge runs intercept-only, two-level, and covariate-adjusted scenarios;
- variance capability remains visible in `PreparedGroup` at compile time;
- a result map round-trips through `NumericFeature[A, S]` without support loss;
- `groupNtabJVM/test`, `groupNtabJS/test`, `compileAll`, and `testAll` pass.

### Follow-on A: JVM external resource resolution

After the grammar stabilizes, add JVM-only resource descriptions and resolvers.
The first target should be selected from an actual consumer workflow rather
than assumed in advance. Likely candidates are NIfTI volumes and GIFTI surface
fields.

Acceptance requires:

- pure shared read plans;
- delegation to the existing owning-module reader rather than duplicated
  NIfTI/GIFTI parsing;
- total reader entry points whose errors are mapped into `NTabError`;
- batched tasks grouped by resource;
- one checksum/integrity validation per unique resource;
- bounded caches with explicit ownership;
- requested row order preserved after batched execution;
- identical results to materialized in-memory fixtures;
- unsupported resource kinds reported directly.

### Follow-on B: other integration adapters

Add only adapters justified by live use:

- fit result bundles to `NTable`;
- resolved numerical features to `spatial.Field`;
- resolved sample-by-feature values to MVPA `PatternTable`;
- `NTable` summaries to graphics/viewer data;
- persisted schema source generation for notebooks.

Each adapter belongs above both source modules and must not create a dependency
from lower computational modules back to `ntab`.

## Error Model

Use one top-level error ADT with structured sub-errors or categories:

```scala
enum NTabError:
  case Schema(error: SchemaError)
  case Binding(error: BindingError)
  case Expression(error: ExpressionError)
  case Resolution(error: ResolutionError)
  case Support(error: SupportError)
  case Reduction(error: ReductionError)
  case Provenance(error: ProvenanceError)
```

Public entry points return `Either[NTabError, A]`. Concise throwing helpers may
exist as explicitly named `unsafe` or `orThrow` conveniences at interactive
boundaries. Internal numerical loops may use validated unsafe constructors but
must not throw for an ordinary user input error.

Important distinct failures include:

- unknown metadata column or feature;
- scalar type/codec mismatch;
- handle from another table;
- malformed category domain;
- missing non-null row identity;
- duplicate row identity;
- feature payload type mismatch;
- volume grid mismatch;
- surface topology mismatch;
- parcel basis/order mismatch;
- unsupported resource resolver;
- missing feature under the selected policy;
- empty group or insufficient contributors;
- undefined ratio denominator;
- lineage source unavailable for drill.

## Laws and Test Strategy

In addition to example tests, encode laws that apply across feature kinds.

### Relational laws

- filtering by `true` is identity;
- filtering by `false` yields an empty row selection without corrupting schema;
- sequential filters equal conjunction, modulo receipt structure;
- stable arrange is idempotent;
- group membership partitions the selected row set exactly once;
- concatenation is associative when schemas and identities are compatible;
- metadata operations never resolve features.

### Resolution laws

- resolving `All` preserves source observation order;
- resolving a selection preserves requested order;
- materialized and external interpretations agree on the same fixture;
- binding plus resolution validates support before returning a value;
- a missing source is never confused with a materialized numerical sentinel.

### Numerical feature laws

- `supportOf` returns the exact owning-module support, never a length-derived
  surrogate;
- `build(support, flatten(value))` preserves values and support;
- flattening preserves the declared voxel, vertex, or parcel order;
- dense volumes use `VolumeSpace`; sparse and ROI values use the existing
  `VoxelIndexSet`;
- incompatible support fails before copying into a shared numerical buffer;
- concrete adapters map owning-module construction errors into `NTabError`.

### Reduction laws

- group sum equals an independent scalar-loop oracle at every support element;
- mean times count equals sum within declared floating-point tolerance;
- singleton mean is the original feature;
- variance and standard error state their denominator convention explicitly;
- reducers are invariant to row permutation within a group up to floating-point
  tolerance;
- support mismatch fails before entering the hot loop;
- dense, sparse, ROI, surface, and parcel implementations agree when fed
  identical aligned synthetic values.

### Group bridge laws

- prepared subject order is deterministic and independent of input row order;
- every prepared subject-by-contrast cell has exactly one effect and, for
  variance-carrying inputs, exactly one variance;
- effect and variance matrices share subject order, sample order, and support;
- treatment coding is invariant to row order when the reference is explicit;
- `PreparedGroup[EffectsOnly, A, S]` cannot enter a variance-required builder;
- reconstructing group vectors uses the retained `GroupSupportReceipt[S]`, not
  vector length alone;
- result lineage contains all and only contributing subject rows.

### Compile-time negative tests

Use Scala 3 compile-error assertions to prove that:

- text cannot use ordering without a supplied ordering;
- `MetaCol[Years]` cannot compare with `Group`;
- a portable comparison cannot capture an arbitrary application `Ordering`;
- a volume reducer cannot accept a surface feature;
- a parcel contrast cannot accept a volume reference;
- only grouped queries expose grouped summarization.
- effects-only prepared group data cannot call variance-required model
  constructors.

### Cross-platform gate

Every shared milestone finishes with:

```sh
sbt ntabJVM/test
sbt ntabJS/test
```

Final integration also requires:

```sh
sbt compileAll
sbt testAll
```

## Performance Invariants

- Metadata is stored column-wise; numeric filters do not box every cell.
- Missing scalar values use validity storage rather than `Option` per cell.
- Metadata filtering and grouping never resolve feature payloads.
- Resolution plans batch by resource and preserve requested row order.
- Numerical features copy directly to primitive buffers without per-element
  wrapper allocation.
- Reducers validate support outside their inner loops.
- Numerical inner loops use primitive arrays and explicit allocation.
- Group membership is compiled once per grouped execution, not rediscovered per
  feature element.
- Result buffers are allocated once when shape is known.
- Public APIs remain immutable even when private caches or arrays are used.

No performance claim is accepted without a runnable benchmark receipt.

## Documentation Requirements

The README and examples should lead with interactive use:

1. inspect a table;
2. bind several metadata variables;
3. bind a volume, surface, or parcel feature;
4. filter without loading features;
5. group by two variables;
6. summarize and compare;
7. inspect the receipt;
8. drill to contributors.

Documentation must explain the compile-time/runtime boundary plainly:

- column names and external payloads are discovered and validated at runtime;
- a successfully bound handle has a static Scala type;
- expressions built from handles are type checked;
- exact support is validated from data before computation;
- optional source generation does not replace runtime validation.

The group integration documentation must also distinguish descriptive
`NTable.groupBy(...).summarize(...)` from inferential
`GroupNTable.prepare(...).model(...)`, show explicit categorical reference
coding, and demonstrate an effects-plus-variance random-effects path.

Avoid presenting `ntab` as a full dataframe, SQL engine, or storage standard.
Its purpose is typed relational analysis over neuroimaging feature values.

## Candidate Evaluation

The following screen records the main design options considered. Impact,
effort, and risk use a 1--5 scale.

| # | Candidate | Impact | Effort | Risk | Confidence | Evidence | Verdict |
|---:|---|---:|---:|---:|---:|---|---|
| 1 | Make runtime schema plus typed handles the primary API. | 5 | 3 | 2 | 97% | Interactive analyses are largely dynamic. | Keep |
| 2 | Require one generated metadata case class before any query. | 2 | 4 | 4 | 30% | Adds friction to exploration. | Reject |
| 3 | Offer optional typed record views over the dynamic core. | 4 | 3 | 2 | 84% | Useful for stable pipelines. | Maybe after v1 |
| 4 | Put row identity in an opaque `RowId`. | 5 | 2 | 1 | 98% | Group/drill requires stable identity. | Keep |
| 5 | Store metadata column-wise with primitive arrays and validity bits. | 5 | 4 | 2 | 91% | Filtering/grouping are hot metadata paths. | Keep |
| 6 | Store each row as `Map[String, Any]`. | 2 | 2 | 5 | 15% | Defeats typed binding and primitive storage. | Reject |
| 7 | Use a closed portable scalar ADT for the first release. | 5 | 2 | 1 | 94% | JVM/JS and runtime discovery require an inspectable schema. | Keep |
| 8 | Bind custom domain scalars through `ScalarCodec[A]`. | 5 | 3 | 2 | 92% | Opaque domain types should remain usable interactively. | Keep |
| 9 | Build filters as typed `Expr[A]` plans. | 5 | 4 | 2 | 94% | Supports type checking, explainability, and later interpreters. | Keep |
| 10 | Accept arbitrary closures as the only filter API. | 3 | 2 | 4 | 35% | Closures are local and uninspectable. | Reject |
| 11 | Put group inference in a leaf `group-ntab` bridge. | 5 | 4 | 2 | 94% | Preserves both module boundaries and typed variance capability. | Keep after core |
| 12 | Represent grouping as a separate `GroupedQuery[K]`. | 5 | 3 | 1 | 95% | Prevents invalid summarize states. | Keep |
| 13 | Support concise grouping of one to three heterogeneous columns. | 5 | 3 | 2 | 92% | Covers common multifactor analyses. | Keep |
| 14 | Expose general tuple-level schema programming in v1. | 2 | 5 | 4 | 28% | Risks obscure APIs under Scala 3.4.2. | Reject |
| 15 | Type feature handles by payload and owning-module support. | 5 | 4 | 2 | 97% | Existing volume, voxel-index, surface, and parcel invariants should be reused. | Keep |
| 16 | Replace existing spatial values with a universal array. | 1 | 5 | 5 | 10% | Existing modules already own stronger types. | Reject |
| 17 | Require exact support for initial reductions. | 5 | 3 | 1 | 96% | Implicit alignment would be scientifically unsafe. | Keep |
| 18 | Implicitly resample mismatched supports during summarize. | 2 | 5 | 5 | 12% | Hides consequential spatial policy. | Reject |
| 19 | Normalize each row to one feature source at ingest. | 4 | 3 | 2 | 88% | Keeps fallback policy out of compute. | Keep |
| 20 | Carry ordered fallback encodings through every query. | 2 | 4 | 4 | 25% | Couples storage policy to analysis. | Reject |
| 21 | Use explicit immutable resolver capabilities. | 5 | 4 | 2 | 91% | Avoids global registries and keeps plans interpretable. | Keep after core |
| 22 | Build JVM file resolvers before the in-memory grammar. | 3 | 5 | 4 | 33% | Storage would dictate an unsettled API. | Reject |
| 23 | Implement separate explicit reducers before generalizing pointwise algebra. | 5 | 3 | 1 | 93% | Preserves clarity and measurable allocation behavior. | Keep |
| 24 | Add a narrow `NumericFeature` flatten/rebuild capability after concrete adapters. | 5 | 3 | 2 | 91% | Reducers and the group bridge need one checked primitive-buffer boundary. | Keep |
| 25 | Preserve contributor lineage as structured data. | 5 | 3 | 1 | 96% | Drill-down is a core workflow, not display metadata. | Keep |
| 26 | Encode contributor IDs in a magic JSON metadata column. | 1 | 2 | 5 | 10% | Repeats a dynamic implementation limitation. | Reject |
| 27 | Add a lazy linear query plan without a general optimizer. | 4 | 3 | 2 | 88% | Enables explainability without speculative complexity. | Keep |
| 28 | Build a general optimizer and distributed runner in v1. | 2 | 5 | 5 | 20% | No current performance evidence requires it. | Reject |
| 29 | Add optional deterministic JVM schema code generation later. | 3 | 5 | 4 | 62% | Helpful for stable notebooks but not required dynamically. | Maybe |
| 30 | Lock every milestone with JVM/JS laws, negative type tests, and receipts. | 5 | 4 | 1 | 97% | Required by ScalaFIM's completion contract. | Keep |

## Top Three Priorities

1. **Dynamic schema and typed metadata expressions.** This proves that the
   interactive experience works without generated code or feature IO.
2. **Typed feature handles with exact support and in-memory resolution.** This
   establishes the scientific safety boundary across volumes, surfaces, and
   parcels.
3. **Grouped reduction with structured lineage.** This completes the defining
   filter/group/summarize/drill workflow before storage adapters broaden the
   system.

This order isolates the highest-risk API question first, then the highest-risk
scientific invariant, then the primary user workflow. External IO, integration
adapters, and code generation remain additive once those foundations are
proven.

The first integration priority is `group-ntab`: it exercises typed metadata,
exact feature support, numerical flatten/rebuild, variance capability, and
lineage together without weakening the core dependency graph.

## Open Questions Before Implementation

The following choices can be resolved during the milestone-zero spike without
changing the architecture:

1. Should the primary type be spelled `NTable`, `NTab`, or `NeuroTable` while
   retaining module/package name `ntab`? The current recommendation is
   `NTable`.
2. Should `CategoryDomain` distinguish ordered and unordered categories in
   version one? The current recommendation is yes, because ordering affects
   comparisons and sorting.
3. Should grouping arity stop at three or include four? Three covers the
   canonical examples; a `GroupSpec[DynamicGroupKey]` covers programmatic cases.
4. Should `SummaryResult` represent multi-column keys as Scala tuples by
   default, or map them immediately to a small named record? It will expose a
   typed `K` either way; the spike should decide based on inference quality and
   error messages.
5. Should version one include `filterLocal`, or should it wait until a missing
   expression operation is demonstrated? The current recommendation is to
   wait.
6. Which external resource resolver should come first after version one? Choose
   from a live consumer workflow, not from format popularity alone.
7. Should the first `GroupDesignSpec` support interactions, or stop at explicit
   treatment coding plus numeric covariates? The current recommendation is to
   stop there until a real analysis fixes the desired interaction grammar.
