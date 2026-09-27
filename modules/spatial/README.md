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

Linear algebra uses Gale directly. Plugin matrices are `gale.linalg.DMat`;
sampled maps use Gale `DoubleLinearOperator`, `COO`, and `CSR`. Spatial image
geometry still exposes its current Gale `DMat` ABI, so conversions at that domain boundary are
deliberate rather than a second generic linear-algebra implementation.

## JVM sources and transform assets

`scalafim.spatial.io.NiftiFieldSource` validates an uncompressed NIfTI lazily
and reads compact row/observation windows. Its revision participates in cache
identity, and stale or geometry-mismatched files produce typed failures.

`TransformDescriptor.load` (through `TransformAssetLoader`) is a graph adapter
over the `transform` module. `TransformFiles.load` reads, gunzips, detects and
decodes the file (ITK text/MATLAB/HDF5, ANTs and AFNI 3dQwarp fields, FLIRT,
FNIRT fields, AFNI `.aff12.1D`, LTA, `.xfm`, `register.dat`, X5) and records
its SHA-256. The format's interpretation then yields a `WorldTransform` between
the descriptor's volume frames, taking any geometry it needs (FSL scaled-voxel
geometry, FreeSurfer volume geometry, AFNI obliquity) from the domains. Its
pullback becomes the morphism's provider map: affines through
`CoordinateMap.affineBetween`, dense fields through `CoordinateMap.dense`, and
other provider maps (e.g. ITK composites) under an identity built from their
asset digests and load options.

Coordinate conventions and storage direction are intrinsic to each format, so
descriptors carry no convention or direction flags. The one routing choice is
`TransformFileEndpoints`: `AsFile` when the descriptor's source and target are
the file's moving and fixed spaces, `Reversed` when the graph edge runs the
other way. A reversed affine uses its exact inverse. A reversed dense map needs
an inverse asset (`TransformAssetSpec`, e.g. ANTs `InverseWarp`); without one
the load fails with `NoForwardMap`. `TransformLoadOptions` holds only choices
that change values: the dense boundary policy (default `Reject`; ITK's own
behaviour is `PreserveSource`) and an explicit FNIRT relative/absolute
definition when detection should not decide.

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

## neurofunctor parity

`tools/r-parity/generate_neurofunctor_law_fixtures.R` exports neurofunctor's
functor, QC, hybrid and backprojection laws as (input, operation, expected)
triplets under `jvm/src/test/resources/scalafim/spatial/neurofunctor-laws/`,
with a manifest recording R, the package versions and the neurofunctor source
commit. `NeurofunctorLawParitySuite` replays every triplet on the JVM and
Scala.js. Indices are zero-based, and element-indexed values are exported in
ScalaFIM's volume order (z fastest), not neurofunctor's (x fastest).

Declared deviations, each kept as a triplet; where the fixture can observe
the difference, the replay asserts it:

- `allPaths` ranks routes by cost before capping at `maxPaths`; neurofunctor
  truncates its depth-first listing.
- Projection metrics summarise every row; neurofunctor samples up to 1000.
- Backprojection compiles with the view's own inverse setting; neurofunctor
  always allows inverses. Forward-first routing makes the values agree.
- Trilinear sampling renormalises the in-grid corners of a point less than
  one voxel outside the grid and reports fractional row coverage;
  neurofunctor drops the row.
- ROI operators hold only the ROI rows; neurofunctor keeps full height.

## Deliberate limitations

- A row-linear stage interleaved with coordinate pullbacks needs a backend that
  declares and implements the fusion rule; the standard backend rejects it.
- A selection cannot be followed by another spatial `.to`; demand remains
  terminal so support pullback is unambiguous.
- Compressed NIfTI must be staged uncompressed before random-access reads.
- Transform families the codecs refuse (ITK B-splines, FNIRT DCT
  coefficients, multi-volume affine series as a single route) fail as typed
  `TransformRead`, `TransformInterpretation` or `UnsupportedTransformAsset`
  errors.
- A dense mapping is reversible only when its inverse asset is supplied.
  Numerical field inversion is deliberately outside ingestion.
- `CachedFieldRuntime` and `Field.pending` remain a legacy compatibility path.
  New code should use semantic `Field.to` views with `LazyFieldRuntime`.

## Verification

Run both platforms:

```text
sbt spatialJVM/test spatialJS/test
```

`SpatialLazyContractSuite`, `SpatialLazyAcceptanceSuite`, and the focused
compiler/runtime/source suites cover descriptive purity, root identity, demand
narrowing, cache identity, one-resampling behavior, mixed routes, plugins,
external NIfTI IO, explanation, lineage, and typed failures. The frozen
acceptance oracle was generated independently with R 4.5.1 `stats::approx` and
distinguishes direct root-first values `[4, 8, 0]` from sequentially resampled
values `[4, 4, 3]`.

`TransformIngestSuite` checks the graph adapter on oracle files: SimpleITK
HDF5 composites and a constant displacement pair (with its inverse asset), and
a FLIRT matrix against fslpy's world mapping. The files are byte-identical
copies of transform-module oracles; each directory's `manifest.json` names its
canonical source. Codec and convention coverage (component order, centres,
oriented fields, LPS-to-RAS, float and legacy `Tranform*` datasets, refusals)
lives in the `transform` module's oracle suites.

See [the normative contract](../../docs/plans/spatial-lazy-pullthrough.md) and
[the benchmark receipt](../../docs/benchmarks/spatial-lazy.md).
