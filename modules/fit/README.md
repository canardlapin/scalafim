# scalafim-fmri-fit

Portable fitting kernels for fMRI models.

The first engine is full-rank ordinary least squares over timepoints-by-voxels
response blocks. It is cross-built for the JVM and Scala.js and uses the
Gale's portable matrix and factor APIs.

The low-level `Ols` kernel is matrix-only. During `0.1-development`,
`FitPlanExecutor` is the canonical fMRI-aware execution facade: it adapts
`FitPlan` plus an in-memory
timepoints-by-voxels dataset into typed coefficient and residual-variance
results while preserving column names, voxel indices, and timepoint indices.
Engine-specific behavior is routed through typed `FitInterpreter` instances, so
new engines can define preparation, chunk execution, and merge semantics without
growing a string-dispatched executor. `FitEngine.RunwiseLeastSquares` executes
the same OLS kernel independently per sampling-frame block and returns per-run
coefficient surfaces.

Dense OLS results carry normalized coefficient covariance, coefficient standard
errors, residual diagnostics, and t-contrast evaluation by design-column name.

## Selected estimates without full fit products

Follow the [selected-estimates guide](../../docs/selected-estimates.md) for
executable coefficient, contrast, FIR and optional-product examples.

`SelectedEstimates.prepare` is the common entry point for the supported shared
OLS and runwise inverse-covariance fixed-effects routes. It preserves the
scientific engine declared by the fit plan and returns a typed refusal for other
engines. Preparation reads no responses. The resulting `description` reports
identified outputs, actual selected rows, per-fit axes/rank diagnostics, bounded
read dimensions, computations and planned product dispositions. For example,
pooled estimates with uncertainty off still report internal residual variance
and joint precision. A planned retained product is not evidence that execution
completed; use the returned execution outcome and native block results.

`PreparedSelectedEstimates.foreachBlock` delivers `SelectedEstimateBlock.Shared`
or `.Pooled`, retaining each native uncertainty and exclusion interpretation.
The caller owns the sink and publishes only after successful completion and any
source-revision checks. Cancelled and failed streams are partial outputs. No
cross-request cache is implied: a prepared object binds its exact realized design
and selected axes, while the caller verifies the response content revision.
Fitted/residual series are explicitly unavailable through this executor; bounded
inspection is a separate provider capability.


`FirstLevelEstimates.prepare` compiles identified coefficients and structural
contrasts once, then `foreachBlock` reads bounded voxel blocks and delivers only
the requested output matrices. The default request computes no residual variance,
standard errors, covariance, or unrequested nuisance coefficient maps. Nuisance
terms still enter the design and its rank checks. Selecting a nuisance column
explicitly retains its estimate.

```scala
import gale.backend.Backend
import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.design.ColumnId
import scalafim.fmri.fit.*
import scalafim.fmri.model.FitPlan

def estimateSelected(
    model: FitPlan,
    reader: DatasetSeriesReader,
    columns: Vector[ColumnId],
    write: FirstLevelEstimateBlock => Either[FitError, Unit]
)(using Backend): Either[FitError, EstimateExecutionOutcome] =
  for
    request <- FirstLevelEstimateRequest.make(columns.map(EstimateOutput.Coefficient.apply))
    size <- ChunkSize(1024)
    prepared <- FirstLevelEstimates.prepare(model, request, size)
    outcome <- prepared.foreachBlock(reader, write)
  yield outcome
```

This example compiles in `SelectedEstimatesExample` on JVM and Scala.js. The caller
owns the reader, backend and sink; the executor retains no delivered blocks.
`EstimateOutput.Contrast` accepts `StructuralTContrast`, including response
functionals with declared units. Output order, structural column identity,
physical FIR bin coordinates, selected scans and voxel identities are preserved.
Preparation reads no response data. Cancellation is checked before each read and
delivery; sink failure prevents subsequent reads. Block size bounds each dataset
request, not memory allocated internally by a reader, backend or accumulating sink.

This compiled route currently admits full-rank shared-design OLS with finite
selected responses and no temporal weighting or other response transforms.
Unsupported engines and preparation policies return typed errors before image
reads. Existing `FitPlanExecutor` routes remain available for other scientific
models. The request never silently changes whitening, run coefficient scope or
inverse-covariance pooling.

Use `EstimateUncertaintyRequest.Marginal` or `Joint` to request selected uncertainty
products. Their computation uses full-model residual degrees of freedom; the
estimate-only route also admits exactly determined full-rank designs. Low-level
matrix callers can use `Ols.prepareEstimates` with a `CoefficientReadout`. This compiles
the dual of a pivoted QR factorization, avoiding a square Q, inverse,
pseudoinverse or response-sized nuisance projection. Execution forwards the
caller's Gale backend to the selected matrix product. Measured throughput and
end-to-end application acceptance are separate from this API's correctness.

### Selected outputs after run pooling

For `FitStrategy.SeparateRunsThenFixedEffects`, use
`FirstLevelFixedEffectsEstimates.prepare` with the same output request and bounded
sink pattern. It prepares one run-local QR factor and precision readout per run,
then combines each voxel's full shared coefficient precision before selecting
outputs. Run-local nuisance terms enter each fit but produce no coefficient maps.
Residual variance is required internally even when output uncertainty is absent.
The executor does not construct runwise full-fit objects or pooled full-fit maps.

```scala
def estimatePooled(
    model: FitPlan,
    reader: DatasetSeriesReader,
    request: FirstLevelEstimateRequest,
    write: FirstLevelFixedEffectsEstimateBlock => Either[FitError, Unit]
)(using Backend): Either[FitError, EstimateExecutionOutcome] =
  for
    size <- ChunkSize(1024)
    prepared <- FirstLevelFixedEffectsEstimates.prepare(model, request, size)
    outcome <- prepared.foreachBlock(reader, write)
  yield outcome
```

This example also compiles on both platforms. `runPreparations` exposes actual
retained scans, local coefficient axes, rank diagnostics and residual degrees of
freedom. `selection` preserves requested coefficient/contrast and FIR identities.
Each block has a `Selected` result with exclusions, or an explicit `Excluded`
result when none of its voxels qualifies. Completion counts processed input
voxels; callers must inspect the retained identities before downstream analysis.
Cancellation, source validation and sink ownership follow the shared OLS route.

For existing runwise fits, `FixedEffectsEstimates.combine` shares the ordinary
native fitter's projection, availability and exclusion rules while retaining only
selected pooled maps. `request.compile(schema)` supplies its identified selection.
`FixedEffectsEstimates.estimate` accepts existing sufficient statistics directly.
These two adapters preserve input ownership; they do not retroactively remove
maps already constructed by their caller. Selected covariance is propagated only
when requested, and scalar contrasts are never pooled in place of the full
joint coefficient geometry.

## Numerical and inference behavior

OLS defaults to a rank-revealing QR solve with strict full-rank semantics. The
previous normal-equations path remains available as an explicit
`OlsSolvePolicy.NormalEquations`, but it is no longer the default. Rank-deficient
designs are rejected before fitting; ridge and minimum-norm rank policies are
represented in the public ADT but currently report typed unsupported-policy
errors rather than silently changing the estimator.

Dense `DesignMatrix` and `ResponseBlock` constructors reject non-finite values at
the boundary. Use the public constructors when accepting external data; reserve
the `.unsafe` constructors for already-validated internal paths and tests.

`MissingDataPolicy` keeps that finite-block invariant intact. `Error` rejects a
selected response containing any non-finite value; `ExcludeVoxel` removes a
whole affected response column; `Propagate` is the compatibility name for that
same whole-column behavior. `OmitRowsPerVoxel` scans before `ResponseBlock`
construction, groups voxels by their exact finite-row mask, and fits each group
with its own selected design. Unlike masks produce `PatternedFmriFitResult`,
whose children retain their own timepoints, rank/covariance geometry, ordinary
residual degrees of freedom, AR plan, and contrast result. All-missing,
insufficient-df, and rank-deficient patterns become typed per-voxel exclusions;
healthy patterns still complete. `fitDense` succeeds only when the retained
voxels genuinely share one observation geometry.

Row omission is available for OLS, GLS, runwise OLS, and separate-run fixed
effects, including sequential/future voxel chunking. A missing timepoint is a
real gap on the source time axis, so AR whitening and estimation reset across
it rather than compacting adjacent observations. Estimated shared/global AR is
estimated independently within each observation pattern; ScalaFIM does not
invent a pooled AR estimator across incompatible masks. Fixed selected-row
weights are subset by original row position. Response-derived DVARS weights and an
explicit nuisance-projection matrix are rejected for this policy until their
cross-pattern estimation/alignment contracts are defined.

Response preparation is represented explicitly by `ResponsePreparationPlan`.
The plan records missing-data policy, censoring, volume weights, nuisance
projection, whitening/autocorrelation preparation, and robust weighting as typed
steps. Preparing a block returns a provenance-bearing `PreparedFitBlockInput`.
The default plan is identity-preserving for current OLS/GLS/LSS behavior;
non-default transforms that still need engine-specific implementation are
retained as deferred provenance instead of disappearing into loose config flags.

OLS volume weighting is executable rather than advisory. Fixed weights declare
whether they align to the full acquisition series or the selected response rows;
estimated weights use a typed DVARS estimator with an explicit transform and
within-run or across-selection normalization scope. Both paths apply `sqrt(w)`
to the design and response. Exact zero weights remove their rows before QR, so
rank and residual degrees of freedom describe the fitted weighted system rather
than counting observations with no influence. The result provenance records the
weight source, normalization, input and retained timepoints, zero/excluded rows,
partition scope, and the resolved weights. Chunked OLS resolves response-derived
weights once from the complete selected response and reuses that immutable
temporal geometry for every voxel chunk. Other fit engines reject volume
weighting when the plan is built until they have an equally explicit execution
contract; response-independent canonical geometry likewise cannot estimate
DVARS without a response.

Prepared LSS designs expose a response-independent `TrialReadout`: a checked
Gale linear operator from timepoints to a named `TrialCoefficientAxis`. The
operator folds fixed-design residualization into the trial estimator, applies to
all voxel columns as one batch, and exposes the true transpose action from trial
scores back to timepoints. `LeastSquaresSeparate.fit` uses this same readout, so
ordinary beta estimation and future joint MVPA objectives cannot drift into two
different LSS implementations. A trial absorbed by the fixed design remains on
the coefficient axis with `TrialEstimability.ZeroRegressor` and an exactly zero
readout row; response-dependent preparation such as robust or voxelwise nuisance
estimation is intentionally not represented as a fixed `TrialReadout`.

GLS normalizes legacy AR options through typed `AutocorrelationConfig` before
execution. Impossible strategy states such as simultaneous global and voxelwise
estimation are rejected at construction. Shared/run-level AR estimation supports
multiple GLS re-estimation passes (`iterations > 1`) while keeping the prepared
whitening plan immutable and chunk-safe. Voxelwise AR estimation prepares one
whitening plan per selected voxel on the full selected response block, then
chunks select those plans by voxel id so parallel chunking cannot change the AR
model. Dense results now carry a typed normalized coefficient covariance payload:
either one shared matrix or exactly one matrix per selected voxel. Voxelwise GLS
therefore keeps per-voxel covariance through chunk merge and named t/F contrasts
use the correct covariance surface for each voxel. The fixed AR(1) path is
anchored to an `fmrireg` whitening and GLS fixture, including censor-reset
behavior.

`FitEngine.RobustLeastSquares` is a real interpreter-backed engine. It uses
iteratively reweighted least squares with typed `RobustPsi` choices for Huber and
bisquare weighting, plus typed scale scopes (`Global`, `Run`, `Voxel`), final
weights, scale estimates, iteration count, convergence state, and coefficient
delta diagnostics. Following fmrireg's GLM robust path, weights are row-wise:
each timepoint receives one robust weight from the cross-voxel median
standardized residual, while scale components retain the configured scope.
`Voxel` scope therefore means per-voxel scale components feeding one row-weight
vector, not independent voxel-specific weight streams.

Chunked robust execution is parallelizable after a preparation barrier. The
interpreter estimates the full-selection robust weight vector once, stores it in
an immutable prepared context, and each voxel chunk applies that same row-weight
vector. This preserves global row order and avoids chunk-local robust weights
silently changing the fitted model. Robust-triggered AR re-estimation is a typed
robust autocorrelation policy: the interpreter estimates a shared/run-pooled AR
whitening plan from robust residuals, stores that immutable plan with the prepared
context, and applies the same whitening plus row-weight vector in each chunk.
Voxelwise robust AR is rejected explicitly until per-voxel robust covariance
bundles are represented.

Robust standard errors are computed per voxel from the final weighted design.
Because row-wise robust weighting gives every voxel the same weighted design,
dense robust fits retain a shared normalized-covariance matrix and can use the
ordinary dense contrast path.

Contrast inference treats zero residual variance and zero contrast variance as
non-estimable. These paths now return `FitError.NonEstimableContrast` instead of
emitting non-finite t or F statistics, so callers should handle the typed error
case rather than interpreting `NaN` or infinity as a valid statistic.

`ResponsePreparationPlan.prepareManova` compiles an `FContrast` into a
`PreparedManovaGeometry`. The contrast covariance is Cholesky-certified and the
resulting multi-column effect basis is normalized in the prepared design
metric. Dependent contrast rows are therefore rejected as non-estimable, while
any nonsingular change of basis within the same contrast subspace preserves the
hypothesis projector.

## Result artifacts

Result export is modeled in shared code without binding the fit module to a file
format. `StatMap` is the checked scalar-map primitive: it couples finite values
to selected voxels, dataset shape, statistic kind, provenance, and export intent.
`ParameterMap` and `ContrastMap` specialize that primitive for coefficient,
standard-error, estimate, t, and F surfaces. `CoefficientCovarianceArtifact`
captures shared or voxelwise normalized coefficient covariance with parameter
names, selected voxels, shape, provenance, and export intent. `ResultManifest`
groups those artifacts with `AnalysisProvenance` so downstream JVM adapters can
write NIfTI, HDF5, GDS, or BIDS-style derivatives without reaching back into fit
internals.

Dense results separate fit diagnostics from coefficient inference.
`residualVariance` remains the full-model diagnostic surface, while the typed
`CoefficientInference` payload owns its own variance scale, covariance,
standard errors, degrees of freedom, method, and allowed coefficient scope.
Manifests and image-map adapters export coefficients for every modeled column
but export standard errors and covariance only for inferable columns. The JSON
sidecar records the inference method, scope label, and inferable column names.

The existing `FitImageMaps` API remains compatible. It now also accepts typed
parameter and contrast maps as adapter inputs, which keeps image placement logic
centralized while giving future engines a richer artifact contract than loose
string labels.

JVM export is a thin adapter over the shared manifest. `ResultManifestWriter`
can write a BIDS-style derivative directory with NIfTI scalar maps, coefficient
covariance TSV, and a JSON sidecar. `BidsNiftiMapLayout.Bundled` is the default
and preserves the compact multi-map parameter and contrast NIfTI files.
`BidsNiftiMapLayout.Individual` instead writes one single-map NIfTI for every
named coefficient, standard error, contrast estimate, and contrast statistic.
The sidecar records the selected layout and lists every physical artifact with
its original unsanitized map label. Individual-map export preflights all paths
and rejects filename collisions caused by BIDS entity sanitization before any
file is written. HDF5 and GDS are represented as typed export formats but
currently report explicit unsupported-format errors until their stores are
wired.

## Chunked execution

`FitChunkPlan` represents a finite, ordered `Iterable` of voxel chunks over a
resolved data selection. Each `FitChunkSpec` carries a typed ordinal plus the
selected timepoint and voxel indices. `FitChunkProgram` lowers those fit chunks
into a generic `ChunkProgram[FitChunkWork]`, so executors can run chunks
independently, wrap failures with chunk identity, and merge results back in
selection order.

`ChunkedFitExecutor` runs the plan sequentially. `FutureChunkedFitExecutor` uses a
caller-supplied `ExecutionContext` and typed `FitParallelism` to bound concurrent
chunk work. The public `FitPlanExecutor.fitChunked` and `fitChunkedFuture` methods
cover OLS, GLS, LSS, and runwise OLS when chunking by voxel count; arbitrary
time-splitting is intentionally not exposed as a valid fit chunking strategy.

Low-rank fmrireg engines are represented by typed model strategies rather than
free-form engine arguments. `LatentSketch` carries a checked component request and
sketch method; `IdentityResponse` is valid only when the component request covers
the full voxel selection, while compressed sketches must opt into
`ContiguousVoxelAveraging` or the data-derived `PrincipalComponents` method.
Principal-component sketches also require an explicit fixed component count, so a
compressed PCA fit cannot be confused with the full identity response path.
`ReducedRankGls` carries a checked component request plus typed autocorrelation
config. Full-rank identity `LatentSketch` executes exactly through the
OLS-equivalent path. Fixed-component averaging and principal-component sketches
both prepare one global orthonormal loading basis for the resolved selection, fit
the latent responses once, and let each voxel chunk decode only its own
coefficient and voxelwise covariance columns. That keeps compressed sketch
execution scheduler-neutral: changing chunk boundaries does not change the fitted
sketch.

Compressed `ReducedRankGls` now has the same preparation barrier for shared GLS
fits. It prepares the full selected GLS fit once, applies the shared whitening to
the resolved response, and derives a typed design partition from model column
sources: event columns are reduced-rank targets, while baseline/nuisance columns
are residualized out before the target subspace fit and then estimated back on
the remaining signal. The compressed target fit uses the fmrireg/classical QR-SVD
construction: decompose the leading residualized `Q^T Y` task score matrix, solve
the truncated score targets back through the residualized target-design QR, and
decode voxel chunks from that immutable basis. Shared-whitening full-rank
requests use the same path so they retain the `ReducedRankGls` event-only
inference contract while remaining coefficient-equivalent to GLS. Rank
selection is typed: `Full`, fixed component counts, retained singular-value
energy, and residual sum-of-squares budgets are construction-validated before
the interpreter resolves an execution rank from the prepared task score
spectrum, matching the fmrireg `rank_mode = "fixed" | "energy" | "rss_budget"`
surface without stringly engine arguments. Every reduced-rank result carries an
event/target-only inference scope: nuisance and baseline coefficients remain
available as estimates but reject T/F inference and do not produce exported SE
or covariance maps.

Reduced-rank inference is explicit. `Conditional` matches fmrireg's default:
the residualized task-design covariance is paired with the voxelwise conditional
variance obtained by projecting latent residual covariance through the retained
voxel basis. `Bootstrap` carries validated replicate count, block size, and seed,
and produces a full task-coefficient covariance per voxel so arbitrary post-hoc
T/F contrasts remain supported. Bootstrap resampling uses a documented
Park-Miller stream rather than platform RNG state, making the prepared result
byte-stable across JVM, Scala.js, direct, and chunked execution. The fixture
generator is `tools/r-parity/generate_fmrireg_rrr_fixtures.R`.

Voxelwise-AR compressed reduced-rank GLS remains explicit unsupported behavior.
Per-voxel whitening removes the single shared response geometry required by the
current QR-SVD subspace, so that extension is tracked as a separate design task
rather than treated as a routine backend branch. Full-rank voxelwise-AR fallback
remains available with event-only inference scope.

Chunked execution asks the selected `FitInterpreter` to prepare engine-specific
context once for the resolved selection and then applies that immutable context
to each voxel chunk. This is especially important for estimated-AR GLS: the
AR/whitening plan is estimated on the full selected response block before voxel
chunks run, so chunking does not silently change the autocorrelation model.

The reusable scheduler-neutral boundary is `ChunkProgram`: a validated ordered
work stream, `CompletedChunk` values keyed by chunk ordinal, and a `ChunkReducer`
contract for deterministic merge semantics. `FitChunkProgram` is the fMRI fit
specialization: a resolved chunk stream plus the prepared interpreter context
needed to execute each work item. The shared module provides local sequential and
bounded `Future` interpreters over chunk programs. Future distributed runtimes
should consume the same prepared program contract from JVM-only adapters rather
than adding scheduler concepts to shared code.
