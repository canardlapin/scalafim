# Spatial lazy pull-through contract

Status: implemented normative contract for `bd-01KXYN7JAQEKRWH7RQN0VZVSWD`.

## Purpose

The spatial API should let a caller describe a value in another domain, chain
further spatial operations, and finally ask for data without eagerly moving the
value at every step. A chain should remain an inspectable view rooted in one
source value. At demand time the runtime pulls the request back through the
chain, reads only the required source support, and performs at most one spatial
resampling.

This is the useful form of “laziness” borrowed from neuromorphism and
neurotransform: transformations compose as descriptions, while data movement is
delayed until a terminal demand makes it necessary.

## Vocabulary

- **Root**: the original field identity, domain, shape, and data reference.
- **View**: an immutable description of how a root should be observed.
- **Demand**: a terminal request such as full data, selected rows, or a bounded
  target region.
- **Pullback**: propagation of target demand through the view to determine the
  necessary source support.
- **Materialization**: reading root values and producing concrete output data.

## Normative semantics

### Descriptive purity

Constructing or composing a view must not read source data, compile a spatial
operator, resample values, or write a materialized-result cache. A view is an
immutable value that can be inspected before it runs.

### Single-root identity

Every view retains the stable identity of exactly one root. Composition changes
the current domain and target intent, not the root identity or root data
reference. Independently constructed roots are distinct unless the caller
supplies the same explicit root identity.

### Root-first normalization

A chain `root -> a -> b -> target` has one semantic intent: observe `root` in
`target`. Intermediate steps remain available for explanation and provenance,
but they are not instructions to materialize each intermediate field. Planning
and compilation normalize from the root domain to the final demand.

### One spatial resampling

For one terminal materialization, values are spatially interpolated no more than
once. Coordinate maps and index transformations may be composed, inverted, or
evaluated during pullback; those operations do not themselves resample field
values. This rule matters because two lossy interpolations generally differ from
one interpolation through a composed transform.

### Demand equivalence

For every valid demand, lazy materialization must equal the corresponding
root-first eager reference within the declared numerical tolerance. A partial
demand must produce the same selected values as full root-first materialization,
while permitting the runtime to read a strict subset of source support.

### Cache identity

A compiled-plan or materialized-result cache key must include every semantic
input capable of changing the result: root identity or root revision, root and
target domains, normalized transform intent, target shape or geometry, routing,
sampling policy, row/region demand, inverse policy, compiler/backend identity,
and any boundary or missing-data policy. Equal semantic requests may share work;
requests differing in any such input must not alias.

### Materialization boundary

Only an explicit terminal operation may trigger source reads, compilation,
resampling, or result-cache writes. Repeated demand may reuse valid cached work,
but observing metadata, printing a view, or composing another view remains
effect-free.

### Typed failure

Invalid domains, shapes, selections, routes, inverses, unsupported transform
families, unavailable root data, and compilation failures cross public API
boundaries as typed errors. They must not surface as `null`, incidental index
errors, or partially materialized fields.

### Provenance and explanation

Before execution, a caller can inspect the root, current domain, target intent,
selection, policies, and descriptive steps. After planning or materialization,
the explanation additionally reports normalization, pullback/source support,
compiler/backend choice, cache decisions, and the number of value-resampling
passes. Provenance describes what was requested and what ran; it does not force
execution merely to become available.

### Platform boundary

The semantic model, planner contracts, law tests, and portable reference path
live in shared code and behave identically on JVM and Scala.js. Platform IO and
accelerated kernels may implement capabilities behind that contract without
changing its meaning.

## Compatibility boundary

The existing `Field.pending: Vector[OperatorCacheKey]` loop is legacy eager-style
execution deferred in time: it still applies cached operators sequentially and
can therefore resample more than once. It may remain temporarily as an explicit
compatibility path, but it is not the conforming implementation of this
contract. New semantic APIs must build a root-first `ViewPlan`; later compiler
and runtime slices replace the compatibility loop with pullback planning and one
root-to-demand execution.

## Executable laws

`SpatialLazyContractLaws` is a reusable adapter suite. Each conforming runtime
must instantiate it on JVM and Scala.js and establish:

1. view construction performs no eager work and preserves root identity;
2. chained views equal one direct root-first fused view and use one resampling;
3. partial demand equals selecting from full demand and restricts source support;
4. cache identity changes with every result-affecting semantic input;
5. only terminal materialization performs reads, compilation, resampling, or writes;
6. invalid requests produce typed failures without partial execution; and
7. explanations retain root, normalized intent, demand, and execution facts.

## Epic completion evidence

The epic is complete only when the public `Field` API constructs semantic plans,
affine and supported nonlinear chains compile root-first, partial demand is
pulled back, JVM/Scala.js materialization conforms to the same laws, external
ingest and plugins preserve root identity, explanations are inspectable, and the
full spatial suites pass on both platforms with fixture or differential evidence
for numerical behavior.

## Implemented architecture

`Field` owns a `FieldDataRef`, stable root identity/revision, and immutable
`ViewPlan`. The public `.to` / `.in` / demand methods change only the plan. They
do not call a runtime.

At an explicit terminal boundary, `LazyFieldRuntime`:

1. builds an `EvaluationCacheKey` from root/source revision, graph, final
   demand, policies, compiler registry, and backend;
2. resolves one root-to-target route and lowers every morphism through the
   registry into an ordered `ProgramStage` trace;
3. validates coordinate/value/barrier ordering and compiles one root operator;
4. derives exact or conservative source support from that operator;
5. reads only the demanded root rows and observations;
6. applies the spatial operator once, executes ordered value transforms, and
   caches the result; and
7. records an `EvaluationTrace` used by explanation and materialization
   lineage.

Coordinate stages include affine, dense warp, volume-surface, and
surface-surface pullbacks. Value stages include Gale-backed observation maps,
row maps, hybrid projections, and pointwise fusion barriers. The standard
compiler fuses coordinate stages but rejects an algebraic ordering it cannot
prove safe.

## Explanation phases

- `field.explain` is always descriptive and effect-free.
- `runtime.explain(field)` returns descriptive information plus any already
  recorded plan/evaluation; it never creates one.
- `runtime.plan(field)` explicitly compiles and predicts support without
  validating or reading a source.
- after `.value` or `.materialize`, the explanation reports the normalized
  route, inverse qualities, compiler and stage order, fusion barriers, support,
  cache key/dispositions, backend/operator signature, resampling count, and
  coverage/path QC.

`FieldQcPolicy` validates recorded coverage and path quality with typed
`FieldQcFailure` results. A materialized root keeps the corresponding
`FieldMaterializationLineage`, so freezing a value does not erase how it was
produced.

## Acceptance evidence

| Evidence | Contract protected |
| --- | --- |
| `SpatialLazyContractSuite` | reusable purity, identity, fusion, demand, cache, terminal-boundary, failure, and explanation laws |
| `SpatialLazyAcceptanceSuite` | affine + nonlinear + volume-to-surface + target/time selection + repeated evaluation + materialization on JVM and Scala.js |
| R 4.5.1 `stats::approx` fixture | pullback direction and one-pass `[4, 8, 0]` versus sequential `[4, 4, 3]` |
| `SpatialLazyExternalScenarioSuite` | no eager NIfTI IO, exact physical support reads, reuse, and channel closure |
| `TransformAssetLoaderSuite` | ANTs/FSL/AFNI direction and coordinate-convention normalization, dense inverse claims, malformed assets |
| `ItkHdf5TransformReaderSuite` | independent SimpleITK point oracles for ordered affine/displacement composites, oriented LPS grids, float/double and legacy aliases, executable inverse pairs, and typed failures |
| `SpatialLazyPerformanceSuite` | view-construction cost, JVM allocated bytes, compilation/execution reuse, cache sizes, and source bytes |

The benchmark is diagnostic rather than a machine-independent speed budget.
Its invariant gates are one compilation, one execution, repeated result-cache
hits, a strict partial source read, and positive allocation/timing observations.
The latest local receipt and reproduction command are in
[`docs/benchmarks/spatial-lazy.md`](../benchmarks/spatial-lazy.md).

## Known boundaries

- Row-linear value stages mixed with coordinate stages require an explicit
  backend fusion contract and are rejected by the standard backend.
- Target demand is terminal; selection followed by another spatial transform
  is intentionally invalid.
- Random-access NIfTI sources currently require an uncompressed file.
- The in-process jHDF adapter supports ITK 3D composite markers, affine or
  matrix-offset components, and displacement-field components. Other ITK
  transform families fail with `UnsupportedItkTransformType` rather than being
  guessed.
- A forward nonlinear HDF5 composite requires a supplied inverse HDF5 asset;
  only affine-only composites are inverted analytically.
- The legacy `CachedFieldRuntime` / `Field.pending` path remains available but
  is outside this root-first contract.

The HDF5 oracle fixtures and their SimpleITK point results are generated by
[`tools/spatial/generate_itk_hdf5_fixtures.py`](../../tools/spatial/generate_itk_hdf5_fixtures.py).
Run it with `uv run --with h5py --with numpy --with SimpleITK==2.5.4 ...
--check` to verify that the committed semantic dataset hashes are current. The
Scala test run consumes only the committed fixtures and does not depend on
Python or SimpleITK.
