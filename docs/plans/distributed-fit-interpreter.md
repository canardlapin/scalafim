# Distributed Fit Interpreter Boundary

ScalaFIM first-level fitting should be distributable in principle without
making Spark, a scheduler, or a cluster file system part of the shared core.
The durable boundary is a pure `FitPlan` plus small numerical kernels, with
execution handled by interpreters.

This is a design note, not an implementation request. The current local
executor should remain the only runtime path until the fit contracts have
enough surface area to justify a JVM-only distributed adapter.

## Design position

Keep these layers separate:

- `design`, `model`, `fit`, `linalg`, and `dataset` stay cross-compiled and
  dependency-light.
- `FitPlan` remains an immutable, inspectable description of what to fit.
- Numerical estimators operate on matrix blocks and return typed results.
- Runtime placement, partitioning, retries, persistence, and broadcast mechanics
  belong in an interpreter module outside shared core.

The future distributed module should be a JVM adapter over these values, not a
new modeling API. Spark is one possible interpreter, not a type that should leak
into `FitPlan`, fit configs, estimators, or result models.

## Future interpreter shape

A minimal future abstraction is:

```scala
trait FitInterpreter[F[_]]:
  def fit(plan: FitPlan, selection: DataSelection): F[Either[FitError, FmriFitResult]]
```

`LocalFitInterpreter` can wrap the current `FitPlanExecutor`. A future
`SparkFitInterpreter` can live in a JVM-only module and interpret the same
plans by distributing voxel blocks.

The important contract is not the exact trait signature. It is that a fit plan
does not own execution. Interpreters choose how to materialize response data,
where to run kernels, and how to assemble results.

## Data movement model

First-level design matrices are small relative to whole-brain response data.
For OLS, GLS, and LSS, the distributed shape should therefore be:

1. Build and validate the design locally.
2. Broadcast the design payload, column metadata, options, and small
   precomputed decompositions when useful.
3. Partition response data by voxel, vertex, parcel, or component columns while
   preserving the full time axis inside each block.
4. Run the same pure estimator kernel on every response block.
5. Merge block results by stable sample ids and voxel ids.

This keeps distribution along the spatial/sample axis and avoids splitting the
time axis, where run structure, filtering, censoring, autocorrelation, and
trial design semantics live.

## LSS distribution

Core LSS is naturally block parallel over response columns:

- Trial and fixed design matrices are design-level state.
- QR residualization or equivalent fixed-design projections are small and can
  be broadcast.
- Each worker receives `Y: timepoints x voxelsInBlock`.
- Each worker runs `LeastSquaresSeparate.fit` with the same trial and fixed
  design.
- The result block has `trials x voxelsInBlock` beta values plus diagnostics.

Diagnostics should be split into design diagnostics and block diagnostics. Trial
rank and fixed rank are properties of the design and should be identical across
blocks. Voxel-wise non-finite data, missing columns, or block-specific failures
belong in block diagnostics and should not mutate the plan.

This is why the LSS kernel should stay a function of explicit matrices and
options. It is already the right shape for a Spark adapter: broadcast small
values, map partitions over response blocks, reduce metadata.

## OLS and GLS

OLS follows the same pattern as LSS:

- Broadcast the validated design and its decomposition.
- Partition response data along spatial columns.
- Run the OLS kernel for each response block.
- Assemble coefficient, residual, and contrast blocks by stable ids.

GLS is distributable, but it has a stricter boundary. Any run-wise covariance,
AR parameter estimation, censoring, whitening, or prewhitening state must be
explicit in the plan or in a serializable fit preparation value. Estimating AR
parameters per voxel is still block parallel. Estimating global or pooled AR
state requires an explicit reducer before the final fit stage.

## Result assembly

The local result types are convenient because they can hold all output in
memory. A distributed interpreter should add an internal result sink boundary:

```scala
trait FitResultSink[F[_]]:
  def writeBlock(block: FitResultBlock): F[Unit]
  def finish(): F[Either[FitError, FmriFitResult]]
```

The sink may assemble an in-memory result for small analyses, write arrays to a
store for large analyses, or emit a manifest for lazy loading. The public result
model should still expose typed row, column, trial, term, and voxel metadata.

## Constraints for current code

Current and near-term fit code should preserve these constraints:

- Do not introduce Spark types outside a dedicated JVM adapter module.
- Do not store file handles, mutable registries, sessions, or closures inside
  `FitPlan`.
- Keep fit configs as serializable product types and sealed enums.
- Keep estimator kernels deterministic and free of hidden global state.
- Preserve stable ids for timepoints, terms, trials, runs, and spatial samples.
- Keep design validation separate from response materialization.
- Prefer explicit block contracts over implicit iterator side effects.

These constraints are sufficient for future distributed execution without
making the present local implementation heavier.

## Deferred work

1. Define a small `ResponseBlockSource` or `VoxelBlockSource` contract in the
   dataset or fit boundary once more storage backends exist.
2. Add a `FitInterpreter` facade only when there are at least two real
   interpreters to choose between.
3. Add a JVM-only Spark adapter that depends on Spark and translates
   `ResponseBlockSource` partitions into RDDs or DataFrames.
4. Add result sink implementations for in-memory, archive-backed, and manifest
   backed outputs.
5. Add distributed parity tests that compare local and partitioned execution on
   the same synthetic OLS and LSS fixtures.

## Acceptance checklist

- The design boundary is documented without adding a Spark dependency.
- The current local `FitPlanExecutor` path is unchanged.
- LSS remains expressible as a pure block kernel over explicit matrices.
- Future distributed work has clear module and API constraints.
