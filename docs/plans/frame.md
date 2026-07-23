# Frame architecture

Status: accepted implementation contract for the ScalaFIM incubation phase.

Frame is an immutable, typed local dataframe library. It is intentionally
incubated inside ScalaFIM under ScalaFIM names:

- package: `scalafim.frame`
- core artifact: `scalafim-frame`
- source module: `modules/frame`

`frame4s` is a possible extracted-project name, not a name used by the
incubating code or published artifacts. Extraction may change repository,
package, and artifact names mechanically. It must not require an architecture
rewrite. No `org.typelevel` coordinates or Typelevel project status are claimed
in advance; the intended path is an affiliate-first proposal after the API,
laws, maintenance model, and production evidence are credible.

## Thesis

The central value is:

```scala
Frame[Schema] // immutable typed logical plan
Expr[A]       // typed column expression
Table[Schema] // materialized columnar data
```

Selecting, adding columns, filtering, joining, grouping, aggregating, sorting,
and limiting construct a pure plan. They do not read, mutate, or materialize a
table. Execution is a separate interpretation step:

```scala
frame.explain
frame.stream[F]       // fs2.Stream[F, RowBatch[Schema]]
frame.collect[F]      // Resource[F, Table[Schema]]
```

This split makes query construction referentially transparent, permits plan
inspection and normalization before execution, keeps backend choice out of the
public query algebra, and gives the semantic reference interpreter a small,
deterministic testing surface.

## Module boundaries

### `scalafim-frame`

The cross-built core has no external runtime dependency. It owns:

- Scala 3 named-tuple schema derivation and field lookup;
- `DynamicFrame` runtime-schema binding and promotion to `Frame[Schema]`;
- explicit `Option[A]` nullability in expression and output types;
- the expression algebra and immutable logical-plan ADT;
- Arrow-compatible logical types and immutable column/table contracts;
- pure plan explanation and normalization;
- a small semantic in-memory interpreter used for laws and tests.

It does not own filesystem or network IO, FS2, Cats Effect, Apache Arrow
allocators, Polars, DuckDB, or a production query optimizer.

### `scalafim-frame-fs2`

This cross-built adapter depends on the core, Cats Effect, and FS2. It
owns effectful scan/sink boundaries, streaming execution, cancellation, and
resource-safe collection. A backend that lends buffers or holds file/native
handles must expose a `Resource`; a `Table` obtained through `collect` is valid
only within that resource scope. The reference backend may implement collection
with `Resource.pure`, but the public API does not weaken the lifetime contract.

Format- and engine-specific integrations remain optional adapters. CSV,
Arrow, Parquet, Polars, and SQL engines must not leak their types into the core
algebra. Polars is a gated candidate backend or collaboration, not the assumed
execution engine. DuckDB, Parquet, and Gale integrations remain separate
decisions.

## Typed schema contract

A typed schema is a Scala 3.7 named tuple:

```scala
type People = (
  id: Int,
  name: String,
  score: Option[Double]
)
```

Field names are literal singleton types and field values carry their Scala
types. `SchemaDescriptor[People]` derives the ordered runtime schema. A dynamic
source is promoted only after exact ordered name, logical type, and nullability
validation:

```scala
dynamic.typed[People] // Either[FrameError, Frame[People]]
```

Typed lookup rejects missing names and incompatible expression types during
compilation. `Option[A]` is the only nullable field representation in the typed
surface. A nullable boolean cannot be used as a filter predicate until it is
made total, for example with `isTrue`. A left outer join maps every right-side
field to `Option`, without nesting an already optional field.

Projection and aggregation derive new named-tuple schemas. `withColumn` in 0.1
adds a fresh field and rejects accidental replacement; an explicit replacement
operation can be added later with its own type-level contract. Joins require
disjoint output names in 0.1 so ownership is never resolved by implicit suffixes.

The compile contract is tested with both narrow and 32-column schemas. This is
not a claim of unbounded compile-time performance; compile-cost regression gates
should be added before a standalone release.

The initial physical-layout and allocation smoke receipt is recorded in
[`../benchmarks/frame-storage.md`](../benchmarks/frame-storage.md). It is scoped
to the semantic storage core and deliberately makes no production-engine or
universal zero-copy claim.

## Logical plan and errors

The closed 0.1 logical plan contains source, project, filter, inner/left join,
aggregate, sort, and limit nodes. Convenience operations lower to these nodes.
Each node carries its validated output schema, making an invalid internal plan
unconstructable through the public typed API.

Resolved fields and expressions carry distinct stable `ColumnId` and `ExprId`
values; display names and left/right/current qualifiers remain separate. Scan
and literal-values nodes refer to immutable `SourceRef` identities rather than
capturing a table, cursor, backend, or closure. Public consumers can inspect a
plan through its output, node name, children, and deterministic explanation,
but the resolved node constructors remain internal. Typed erasure to
`DynamicFrame` and successful rebinding preserve the exact plan and schema
objects without copying data or executing the plan.

Failures are structured:

- `SchemaError` covers malformed runtime schemas;
- `BindingIssue` records ordered field count/name/type/nullability mismatches;
- `FrameError` covers binding and planning-boundary failures;
- the execution foundations will add a separate `ExecutionError` ADT rather
  than throwing backend exceptions through the public boundary.

Internal `unsafe` constructors are permitted only after public validation or
compile-time derivation. They do not form part of the public API.

## Semantic constitution

The reference interpreter defines semantics independently of optional engines:

- Boolean expressions use SQL three-valued logic. `AND` and `OR` follow their
  SQL truth tables; filters retain only `true` and discard `false` and null.
- Ordinary equality with either operand null returns null. `nullSafeEq` is total:
  two nulls are equal, one null is unequal, and two values use logical equality.
- Null grouping keys form one group. Dictionary values compare by decoded
  logical value rather than dictionary index identity.
- NaN is a non-null floating value. IEEE equality applies (`NaN != NaN`); total
  ordering places NaNs after finite/infinite values and treats NaNs as one sort
  equivalence class. Floating reductions preserve logical input order, use
  ordinary IEEE arithmetic, and propagate NaN; any backend reordering tolerance
  must be declared in its receipt.
- `Int32` and `Int64` arithmetic is checked. Overflow and integral division by
  zero are structured `ExecutionError` values, never wrapping arithmetic.
- There are no implicit casts. The current closed expression algebra accepts
  only same-typed operators; future explicit cast nodes must define overflow,
  precision, and null propagation per source/target pair.
- Timestamps are signed counts since the Unix epoch in their declared unit.
  Units do not compare or convert implicitly, and timestamps carry no hidden
  session timezone.
- UTF-8 strings use unsigned binary UTF-8 collation. Locale-sensitive collation
  requires a future explicit expression/capability.
- There is no hidden row index. `Values` sources are stable; scans are unordered
  unless their `SourceRef` declares an order. Project, filter, and limit preserve
  their input guarantee; aggregate and join do not; sort establishes an explicit
  key guarantee and is stable for equal keys.

`ReferenceInterpreter` is the always-available semantic oracle. Project,
withColumn/project lowering, filter, and limit transform record batches
incrementally. `ReferenceExecution.physicalExplain` is separate from pure
logical explain and reports streaming/blocking nodes, row estimates, and
`fallback=none`. It is deliberately not a production optimizer, spill engine,
SIMD framework, or performance competitor.

`scalafim-frame-fs2` brackets the execution cursor and each emitted batch.
`FrameRuntime.stream` releases both on completion, failure, early termination,
or cancellation. `FrameRuntime.collect` retains output batches into a
`Resource[F, Table[Schema]]`; failed or canceled acquisition closes every
retained batch, and the resource finalizer closes the materialized table.
Owning CSV and Arrow IPC sources are also acquired through `Resource`; their
decoded or native buffers cannot escape an unbracketed source lifetime.

Normalization is observationally error-preserving as well as value-preserving.
Rewrites that reorder expression evaluation, including filter fusion and
project pushdown, are applied only when the expressions moved across the
boundary are total. Checked integral arithmetic therefore cannot begin failing,
or stop failing, merely because a plan was normalized.

## 0.1 delivery boundary

The first usable release includes:

- immutable Arrow-compatible column storage and one semantic reference backend;
- typed `select`, fresh-field `withColumn`, and `filter`;
- inner and left outer joins;
- group-by with count, sum, mean, population variance, min, and max;
- sort and limit;
- explicit missing-value semantics;
- FS2 streaming scan and resource-safe collection;
- in-memory and CSV sources/sinks on JVM and Scala.js;
- Apache Arrow IPC stream ingestion/writing through the JVM adapter;
- pure plan display and normalization.

Right/full joins, union, distinct, windows, user-defined aggregate functions,
Parquet, distributed execution, and a pandas-sized convenience surface are
later work.
Most importantly, 0.1 does not build a production columnar execution engine.
