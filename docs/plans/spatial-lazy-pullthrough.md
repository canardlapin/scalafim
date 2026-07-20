# Spatial Lazy Pull-Through Contract

Status: normative contract for
`bd-01KXYN7JAQEKRWH7RQN0VZVSWD`.

This document freezes the semantics of a lazy spatial `Field` before the
runtime representation changes. The intended experience is that a field may be
described in another spatial domain, narrowed to a demand, inspected, and
composed without reading or resampling its data. Numerical work happens only at
an explicit terminal operation.

The existing spatial algebra remains the foundation: domains and morphisms are
typed values, routes are selected by `SpatialGraph`, and compiled operators are
target-by-source pullbacks. This contract governs how those pieces are exposed
as a lazy field rather than changing their mathematical direction.

## Vocabulary

- **root**: the immutable data identity from which a view will be evaluated. A
  root is either materialized data or an external source capability plus its
  metadata.
- **view**: an immutable description sharing a root. A view has a current
  domain, requested operations, policies, and a demand, but no transformed
  field data.
- **demand**: the target samples and observations requested by a terminal
  operation. Full-field evaluation is one possible demand, not a separate
  execution model.
- **pullback program**: the normalized executable description obtained by
  resolving the final target through the spatial graph back to the root.
- **spatial resampling**: interpolation of root field values at source
  coordinates determined from target samples. Coordinate-map evaluation and
  sparse-operator construction are planning work, not additional resampling.
- **materialization**: evaluation followed by creation of a new root in the
  view's current domain. It is the only operation that deliberately changes the
  root boundary.

## Normative model

The public representation may use more precise Scala types, but it must retain
this information:

```text
FieldRoot
  id
  domain
  shape
  data reference or source capability

ViewPlan
  root identity
  current domain
  requested transform and selection steps
  demand
  sampling policy
  inverse policy
  provenance inputs
```

The implementation must not represent a chained view as a list of already
compiled operators to apply in sequence. Compilation starts from the root and
the final view description:

```text
root Field
    |  descriptive to / in / rows / roi / slice / time operations
    v
immutable ViewPlan
    |  terminal value or materialize
    v
resolve root -> final target route
    v
pull target demand back to required root support
    v
compile one root-support -> target-demand operator
    v
read root support, resample once, return or materialize
```

Value-transform stages such as filters may form explicit execution barriers in
later compiler plugins. They must not silently turn a composable coordinate
route into repeated spatial interpolation. An explanation must identify every
barrier.

## Required laws

### 1. Descriptive purity

Creating a root, calling `to` or `in`, and adding selections must perform no
source read, operator compilation, spatial resampling, or result-cache write.
The operations return new immutable descriptions and leave their inputs valid.

### 2. Single-root identity

Every non-materialized descendant retains the exact root identity of its
ancestor. Its current domain may change, but `root`, `domain`, and `target` are
not interchangeable concepts.

### 3. Root-first normalization

If the graph resolves the same root-to-target route, these descriptions are
execution-equivalent:

```scala
root.to(mid).to(target)
root.to(target)
```

They may retain different requested-step provenance, but they normalize to the
same pullback program and execution cache key. This is route equivalence, not a
claim that all possible paths between two domains are interchangeable.

### 4. One spatial resampling

A terminal evaluation of a purely spatial route samples root field values at
most once. A route with two affine or nonlinear coordinate edges composes the
coordinate pullbacks before interpolation. Applying one sampled operator per
edge is non-conforming even when the resulting dimensions happen to match.

The regression fixture must contain values for which two half-voxel linear
interpolations differ from one whole-voxel interpolation; otherwise a
sequential implementation can pass accidentally.

### 5. Demand equivalence

Evaluating a valid target demand must equal selecting the same samples from a
full evaluation, within the numerical tolerance of the sampling policy.
Demand may reduce target rows, root support, IO, allocation, and computation;
it may not alter selected values or their ordering.

Source-support planning distinguishes exact support from conservative support.
Both are correct only if every required source sample is included. Unsupported
demand narrowing must fall back explicitly to a broader support request rather
than silently omit samples.

### 6. Cache identity

An execution cache key is determined by immutable semantic inputs, including:

- root identity and root geometry;
- normalized route and morphism fingerprints;
- final target and demand;
- sampling and boundary policy;
- inverse policy and selected inverse qualities;
- compiler and plugin versions.

Equivalent normalized descriptions share a key. A change to any value that can
change the result must produce a different key. Failed evaluations do not
populate result caches.

### 7. Materialization boundary

`materialize` evaluates the view and creates a new materialized root in the
current domain. The new field has no pending spatial plan, descendants refer to
the new root, and lineage records the old root and evaluated plan. The original
root and views remain unchanged.

### 8. Typed failure

The API represents at least these failures explicitly:

- no compatible route or compiler;
- a route requiring inverse quality disallowed by policy;
- unavailable, stale, or shape-incompatible root data;
- invalid or domain-incompatible demand;
- compilation, cache, and numerical execution failure.

No descriptive operation may open an external source merely to discover a
failure that can be established from metadata.

### 9. Provenance and explanation

Before evaluation, a view exposes its root, current domain, requested steps,
demand, and policies. After normalization or evaluation, provenance also
records the selected route, inverse qualities, compiler versions, fusion
decisions or barriers, source support, cache identity, and coverage or QC.
Requested-step provenance is preserved even when execution is fused.

### 10. Platform boundary

The plan algebra, laws, route lowering, in-memory evaluation, and cache-key
semantics live in shared code and run on JVM and Scala.js. File formats and
external transform loaders remain JVM adapters behind shared capabilities.

## Terminal operations

The intended distinction is explicit:

- `value`: evaluate the requested demand and return data, reusing successful
  compilation and result caches where allowed;
- `materialize`: evaluate and establish a new root;
- `explain`: inspect the plan without IO or numerical execution;
- source preflight: optional explicit metadata or availability check, distinct
  from `explain` and descriptive composition.

Naming may evolve, but an API must not make ordinary inspection an accidental
terminal operation.

## Executable specification

`SpatialLazyContractLaws` is a reusable shared-test harness. It partitions the
contract into targeted law families:

| Failure mode | Executable law |
| --- | --- |
| eager reads or compilation | descriptive purity |
| root identity lost through views | single-root identity |
| sequential edge evaluation | root-first equivalence plus resampling count |
| incorrect ROI or row pushdown | demand equivalence and source-support check |
| cache collision or missed reuse | semantic cache identity |
| materialized views still depend on an old root | materialization boundary |
| ambient exceptions or invented inverses | typed failure cases |
| fusion erases history | provenance preservation |

The initial suite runs the laws against a small independent reference adapter.
That adapter uses a one-dimensional signal where sequential half-sample linear
interpolation disagrees with a single fused whole-sample interpolation. The
production `ViewPlan` and evaluator must be bound to the same harness in their
implementation children; passing only the reference adapter is not evidence
that the runtime is complete.

## Migration consequence

The current `Field.pending: Vector[OperatorCacheKey]` and
`CachedFieldRuntime.data` loop apply one compiled operator after another. They
are legacy behavior under this contract. The migration must replace that
execution meaning with a root-first `ViewPlan`; retaining the vector only as
requested-step provenance is acceptable, but treating it as the execution
program is not.

## Epic completion evidence

The epic is complete only when:

1. the production field implementation passes the shared contract laws on JVM
   and Scala.js;
2. independent coordinate and resampling oracles validate affine, nonlinear,
   and mixed-domain routes;
3. instrumented external sources prove descriptive purity and demand-aware IO;
4. repeated evaluation proves safe cache reuse;
5. documentation examples use the public composable API without manual
   operator compilation.
