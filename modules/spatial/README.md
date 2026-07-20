# scalafim-spatial

`scalafim-spatial` is ScalaFIM's typed, lazy spatial layer. A `Field` keeps one
root value and accumulates an immutable `ViewPlan`; `.value` or `.materialize`
then pulls the final demand back through the complete route, reads the required
root support, and performs no more than one spatial interpolation.

This is the useful part of the neuromorphism/neurotransform model: transforms
compose like descriptions, values move only at an explicit terminal boundary,
and the apparently magical execution remains inspectable.

```scala
import scalafim.spatial.*

given SpatialGraph = graph

val requested =
  nativeBold
    .to(anatomical)
    .flatMap(_.to(template))
    .flatMap(_.to(leftSurface))
    .flatMap(_.vertices(40, 12, 7))
    .flatMap(_.timeBlock(start = 20, length = 10))

val runtime = LazyFieldRuntime(summon[SpatialGraph])

val before: Either[FieldApiError, FieldExplanation] =
  requested.map(_.explain)                 // descriptive and effect-free

val planned: Either[FieldApiError, FieldExplanation] =
  requested.flatMap { view =>
    runtime.plan(view).left.map(FieldApiError.Spatial.apply)
  } // compile/support plan, no source read

given FieldRuntime = runtime

val values = requested.flatMap(_.value)       // terminal execution
val frozen = requested.flatMap(_.materialize) // fresh root with retained lineage
```

Normal application code should handle the `Either` directly; the expanded
`planned` expression above only makes the two error domains visible. Calling
`field.explain` or `runtime.explain(field)` never plans, reads, or evaluates.
`runtime.plan(field)` is the explicit compile-and-support-planning boundary and
still performs no source IO.

## Mental model

The execution path is root-first, not stepwise:

```text
Field root + immutable ViewPlan
  -> final target demand
  -> graph route and typed ProgramStage lowering
  -> coordinate fusion / value-barrier validation
  -> source-support pullback
  -> one compact root read
  -> one spatial operator application
  -> ordered value stages
  -> result cache or materialized root with lineage
```

Intermediate `.to` calls remain in `ViewPlan.steps` for explanation. They are
not instructions to create intermediate images. Target selections are terminal:
select rows, voxels, vertices, masks, regions, slices, ROIs, or a time block only
after the final spatial re-expression.

## Public pieces

- `Domain`, `SamplingGeometry`, and typed ids describe volume, surface, hybrid,
  template, and latent sampled spaces.
- `Morphism` and `SpatialGraph` describe immutable routes with explicit
  direction, cost, route tag, and geometric inverse quality.
- `Field.fromMatrix` creates an in-memory root. `Field.fromSource` creates an
  externally backed root without reading it.
- `Field.to` / `Field.in` compose a root-preserving view. `rows`, `voxels`,
  `vertices`, `roi`, `slice`, `mask`, `region`, and `timeBlock` refine the final
  demand.
- `LazyFieldRuntime` compiles a normalized `PullbackProgram`, predicts source
  support, memoizes plans/results, and executes the root-to-demand operator.
- `FieldExplanation` reports descriptive intent before execution and the actual
  route, inverse qualities, compilers, stages, fusion barriers, support,
  backend, cache decisions, resampling count, coverage, and path QC after
  planning/evaluation.
- `FieldQcPolicy` turns coverage and path-quality requirements into typed
  `FieldQcFailure` values.

`materialize` returns a fresh root in the current domain. Its
`FieldProvenance.materializations` retains the parent root/revision, demand,
descriptive steps, and recorded execution explanation.

## Supported execution families

Coordinate pullbacks currently cover identity, affine volume transforms, dense
coordinate warps, volume-to-surface sampling, and surface vertex mappings.
Mixed affine/nonlinear/volume-surface routes compile into one root operator.

Value plugins make non-coordinate morphisms explicit:

- functional observation transforms;
- row-linear filters;
- hybrid row projections; and
- pointwise affine fusion barriers.

These stages are ordered with coordinate stages in `PullbackProgram.stageTrace`.
Coordinate stages fuse into at most one value resampling. Noncommuting value
barriers remain visible; unsupported orderings fail with
`SpatialError.UnsupportedPluginComposition` rather than silently changing the
math.

New linear algebra uses Gale. Plugin matrices are `gale.linalg.DMat`, Gale row
maps execute through `GaleLinearMap`, and JVM transform ingestion uses Gale
factorization. `GaleMatrixBridge` is the explicit compatibility boundary to the
older `scalafim.linalg.DoubleMatrix`. Spatial image geometry still exposes its
current `scalafim.image.DMat` ABI; conversions at that boundary are deliberate,
not a second linear-algebra implementation.

## JVM sources and transform assets

`scalafim.spatial.io.NiftiFieldSource` validates an uncompressed NIfTI lazily
and reads compact row/observation windows. Its revision participates in cache
identity, and stale or geometry-mismatched files produce typed failures.

The JVM transform loader normalizes executable ANTs ITK affine/displacement and
HDF5 composite, FSL FLIRT and dense-warp, and AFNI affine/warp assets to the
canonical RAS pullback convention. The HDF5 path is in-process through jHDF: it
reads the ordered `/TransformGroup`, decodes affine and displacement components,
applies ITK's last-component-first composition rule, converts LPS geometry and
vectors to RAS, and lowers the result to a portable `CompositeCoordinateMap`.
Affine algebra and validation use Gale.

ANTs HDF5 defaults to a stored target-to-source pullback. A caller may declare a
forward affine-only file, which is inverted exactly. A forward composite with a
nonlinear component needs an explicit inverse HDF5 asset; the loader never
pretends that reversing a displacement field is an inverse. Native direction,
coordinate convention, ordered component types, canonical or historical
`Tranform*` dataset names, optional ITK/HDF metadata, inverse asset, and content
fingerprint remain in provenance. Float parameter datasets stay in float
storage while decoding, avoiding a full-size double copy before construction of
the runtime field.

## Extension protocol

To add a morphism family:

1. Put its invariants in a typed `MorphismKind`, `CoordinateMap`, or
   `MorphismPlugin` payload.
2. Implement `MorphismPullbackCompiler` and register it in a
   `MorphismCompilerRegistry`; do not add kind switches to the central planner.
3. Lower to `ProgramStage.Coordinate` for pullbacks or a typed `ValueStage` with
   `StageSemantics.ValueTransform` / `FusionBarrier`.
4. Use Gale `DMat`/linear maps for algebraic transforms. Keep JVM-only adapters
   and file IO under `jvm`.
5. Include payload content and compiler identity in fingerprints, return typed
   failures for unsupported compositions, and add shared JVM+Scala.js tests.
6. Add an independent fixture when direction, convention, interpolation, or
   statistical parity matters.

## Deliberate limitations

- A row-linear stage interleaved with coordinate pullbacks needs a backend that
  declares and implements the fusion rule; the standard backend rejects it.
- A selection cannot be followed by another spatial `.to`; demand remains
  terminal so support pullback is unambiguous.
- Compressed NIfTI must be staged uncompressed before random-access reads.
- The built-in ITK HDF5 semantic adapter accepts composite markers, affine or
  matrix-offset transforms, and 3D displacement-field transforms. B-splines,
  velocity fields, and other transform families fail as
  `UnsupportedItkTransformType` until a typed decoder is added.
- A nonlinear HDF5 mapping is reversible only when its inverse HDF5 asset is
  supplied. Numerical field inversion is deliberately outside ingestion.
- `CachedFieldRuntime` and `Field.pending` remain a legacy compatibility path.
  New code should use semantic `Field.to` views with `LazyFieldRuntime`.

## Verification

Run both platforms:

```text
sbt spatialJVM/test spatialJS/test
uv run --with h5py --with numpy --with SimpleITK==2.5.4 \
  tools/spatial/generate_itk_hdf5_fixtures.py --check
```

`SpatialLazyContractSuite`, `SpatialLazyAcceptanceSuite`, and the focused
compiler/runtime/source suites cover descriptive purity, root identity, demand
narrowing, cache identity, one-resampling behavior, mixed routes, plugins,
external NIfTI IO, explanation, lineage, and typed failures. The frozen
acceptance oracle was generated independently with R 4.5.1 `stats::approx` and
distinguishes direct root-first values `[4, 8, 0]` from sequentially resampled
values `[4, 4, 3]`.

`ItkHdf5TransformReaderSuite` uses compact files emitted by SimpleITK/ITK and
point outputs computed by SimpleITK, not by ScalaFIM. It protects noncommuting
component order, affine centers, oriented displacement grids, LPS-to-RAS
conversion, voxel-interleaved displacement parameters, float/double datasets,
historical aliases, executable inverse pairs, and explicit malformed or
unsupported failures. The generator's `--check` compares semantic HDF5 dataset
hashes because byte-for-byte HDF container layout is not deterministic; Python
with the pinned SimpleITK 2.5.4, h5py, and NumPy is needed only to regenerate or
verify fixtures, not to run the Scala adapter.

See [the normative contract](../../docs/plans/spatial-lazy-pullthrough.md) and
[the benchmark receipt](../../docs/benchmarks/spatial-lazy.md).
