# Request selected first-level estimates

Use `SelectedEstimates` when you need identified betas, FIR coefficients or
contrasts from a shared OLS or runwise fixed-effects model. The request selects
what is returned. The fit plan still determines the scientific model, nuisance
terms, retained scans and run combination.

Start with the `FitPlan` and matching `DatasetSeriesReader` from your
[first-level model](first-level-analysis.md). Choose structural column IDs from
`plan.model.designSchema` so the request retains condition, basis and run
identity. Column display names alone are insufficient identifiers.

The helpers below belong to one [executable example](../modules/fit/shared/src/test/scala/scalafim/examples/SelectedEstimatesExample.scala).
Its [portable suite](../modules/fit/shared/src/test/scala/scalafim/examples/SelectedEstimatesExampleSuite.scala)
executes the helpers on JVM and Scala.js, including a mixed-TR R reference.
They illustrate ordinary API composition; applications can call the same APIs
directly.

<!-- BEGIN extracted:selected-imports -->
```scala
import gale.backend.Backend
import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.design.ColumnId
import scalafim.fmri.fit.*
import scalafim.fmri.hrf.{Hrf, ResponseFunctional}
import scalafim.fmri.model.FitPlan
```
<!-- END extracted:selected-imports -->

## Select betas and a named contrast

Given the structural IDs for conditions A and B, request their coefficients and
A minus B in that order. To request only the contrast, supply only the
`EstimateOutput.Contrast` entry. No optional uncertainty is requested by default.

<!-- BEGIN extracted:selected-condition -->
```scala
def conditionRequest(a: ColumnId, b: ColumnId): Either[FitError, FirstLevelEstimateRequest] =
  FirstLevelEstimateRequest.make(Vector(
    EstimateOutput.Coefficient(a),
    EstimateOutput.Coefficient(b),
    EstimateOutput.Contrast(StructuralTContrast.fromIds(
      ContrastId.unsafe("a-minus-b"), "A minus B", Map(a -> 1.0, b -> -1.0)))
  ))
```
<!-- END extracted:selected-condition -->

Preparation checks the requested IDs against the realized design. Unknown,
duplicate or unsupported output identities return typed errors. A contrast's
weights refer to model coefficients, including any declared basis and scaling;
they do not turn t-statistics into effect estimates.

## Request FIR coefficients

Build the scientific model with its FIR basis first, then select its identified
columns in the desired output order. The request mechanism is the same as for
condition amplitudes.

<!-- BEGIN extracted:selected-fir -->
```scala
def firRequest(firColumns: Vector[ColumnId]): Either[FitError, FirstLevelEstimateRequest] =
  FirstLevelEstimateRequest.make(firColumns.map(EstimateOutput.Coefficient.apply))
```
<!-- END extracted:selected-fir -->

Output metadata retains the original structural columns and basis identities.
Keep that metadata with the maps: FIR coordinates carry physical response-time
meaning. An arbitrary HRF-basis coefficient is not automatically a time sample,
and a coefficient index is not a substitute for its original bin coordinates.

## Reconstruct a response from a derivative basis

A derivative coefficient describes a basis coordinate. To request the fitted
response at a physical time, or a mean or integral over a window, use a native
semantic cell and `ResponseFunctional`. ScalaFIM derives the complete linear
readout and transports it through the realized column scaling.

<!-- BEGIN extracted:selected-response -->
```scala
def responseRequest(
    cell: StructuralHypothesisDsl.CellRef,
    basis: Hrf,
    functional: ResponseFunctional
): Either[FitError, FirstLevelEstimateRequest] =
  for
    contrast <- cell.response(basis, functional)
      .named("response", "Reconstructed response for the selected cell")
      .toStructural
    request <- FirstLevelEstimateRequest.make(Vector(EstimateOutput.Contrast(contrast)))
  yield request
```
<!-- END extracted:selected-response -->

The named expression's `toStructural` conversion retains its selectors,
functional, units and typed errors. Pass the resulting contrast to the same
selected-estimate request used for ordinary coefficients. Requested uncertainty
is propagated through the full readout, including covariance between basis
coefficients. A point or mean has response-value units; a window integral has
response-integral units. Exact window functionals require an analytic primitive
for the chosen basis; unsupported requests remain explicit errors.

## Choose optional products

`None` returns estimates without optional uncertainty maps. `Marginal` requests
selected standard errors; `Joint` also requests covariance between the selected
outputs. Both retain the same requested scientific estimates.

<!-- BEGIN extracted:selected-uncertainty -->
```scala
def withUncertainty(
    request: FirstLevelEstimateRequest,
    uncertainty: EstimateUncertaintyRequest
): Either[FitError, FirstLevelEstimateRequest] =
  FirstLevelEstimateRequest.make(request.outputs, uncertainty, request.retainRunCoefficients)
```
<!-- END extracted:selected-uncertainty -->

Runwise inverse-covariance pooling still computes residual variance and joint
run precision internally when uncertainty is `None`. Selecting fewer returned
maps must not change the pooling rule. Shared OLS without requested uncertainty
can use the compiled time readout without computing residual variance.

To retain nuisance coefficients from a shared OLS design, explicitly append
their structural IDs. Unselected nuisance terms already participate fully in
the fit and its rank checks.

<!-- BEGIN extracted:selected-nuisance -->
```scala
def withNuisance(
    request: FirstLevelEstimateRequest,
    nuisanceColumns: Vector[ColumnId]
): Either[FitError, FirstLevelEstimateRequest] =
  FirstLevelEstimateRequest.make(
    request.outputs ++ nuisanceColumns.map(EstimateOutput.Coefficient.apply),
    request.uncertainty, request.retainRunCoefficients)
```
<!-- END extracted:selected-nuisance -->

For a runwise fixed-effects model, request run coefficients as a separate
product. The source IDs can name nuisance terms or other coefficients. Each
selected run returns the requested columns that occur in its design, in request
order. An empty vector turns this retention off.

<!-- BEGIN extracted:selected-run-coefficients -->
```scala
def withRunCoefficients(
    request: FirstLevelEstimateRequest,
    runColumns: Vector[ColumnId]
): Either[FitError, FirstLevelEstimateRequest] =
  FirstLevelEstimateRequest.make(request.outputs, request.uncertainty,
    retainRunCoefficients = runColumns)
```
<!-- END extracted:selected-run-coefficients -->

The pooled block's `runCoefficients` is `NotRequested` by default or `Retained`
with identified run blocks. Each block carries original `sourceColumnIds`, its
run-local `coefficientAxis`, exact `voxelIndices`, raw estimates and per-voxel
statuses. FIR columns retain physical response-time coordinates and HRF scaling.
Run blocks include input voxels excluded from precision pooling, so inspect
statuses even when a raw coefficient is zero. A fully excluded pooled block can
still contain requested run coefficients.

Primary uncertainty applies to the pooled outputs; retained run coefficients
have no standard errors or covariance. Their readouts reuse the existing run
factorizations and response coordinates, with no additional response reads.
The request leaves the pooled estimand, joint precision and exclusions unchanged.
These run coefficients do not create additional independent participants.

Run-local nuisance terms are absent from the pooled coefficient axis. Asking
for them as primary pooled outputs returns a typed error before response reads.
Shared OLS also refuses `retainRunCoefficients`: select its nuisance coefficients
as ordinary outputs with `withNuisance`. The lower-level pooling-only
`FixedEffectsEstimates.combine` and `.estimate` APIs refuse a run-retention
request because their result contains only pooled products.

## Inspect before reading images

Prepare against the exact fit plan with a maximum response-block size. This
compiles factors and requested readouts from design metadata without reading
responses.

<!-- BEGIN extracted:selected-prepare -->
```scala
def prepareSelected(
    model: FitPlan,
    request: FirstLevelEstimateRequest,
    maximumVoxelsPerRead: Int
): Either[FitError, PreparedSelectedEstimates] =
  for
    size <- ChunkSize(maximumVoxelsPerRead)
    prepared <- SelectedEstimates.prepare(model, request, size)
  yield prepared
```
<!-- END extracted:selected-prepare -->

Inspect `prepared.description` before execution:

| Field | What it tells you |
| --- | --- |
| `outputs` | Ordered coefficient or contrast identities |
| `topology` | Shared OLS or runwise inverse-covariance pooling |
| `diagnostics`, `fittedAxes`, `timepoints` | Rank, fitted columns and actual selected rows |
| `computations` | Required work, including internal variance and pooling |
| `products` | Retained, internal-only, not-requested or unavailable products |
| `maximumVoxelsPerRead` | Largest declared response request |

These are planned capabilities. They do not prove execution completed or that a
source file still matches its original revision. A prepared object may be reused
only with the same scientific design and a reader whose input binding has been
verified by its owner. No automatic cross-request cache is implied.

## Execute through a bounded sink

The common entry point preserves the engine declared by the fit plan. This
helper prepares and executes in blocks of at most 1,024 voxels.

<!-- BEGIN extracted:selected-execute -->
```scala
def estimateWithDeclaredEngine(
    model: FitPlan,
    reader: DatasetSeriesReader,
    request: FirstLevelEstimateRequest,
    write: SelectedEstimateBlock => Either[FitError, Unit]
)(using Backend): Either[FitError, EstimateExecutionOutcome] =
  for
    size <- ChunkSize(1024)
    prepared <- SelectedEstimates.prepare(model, request, size)
    outcome <- prepared.foreachBlock(reader, write)
  yield outcome
```
<!-- END extracted:selected-execute -->

Each callback receives `SelectedEstimateBlock.Shared` or `.Pooled` with the
native result, voxel identities and uncertainty interpretation. Pooled blocks
also report excluded voxels. A completed outcome counts processed input voxels;
inspect exclusions and retained identities before assembling participant data.

The reader, backend and sink belong to the caller. The executor retains no
previous output blocks. A sink that collects every block can still consume
whole-result memory; the block size alone is not a process memory budget.
Use `prepared.foreachBlock(reader, write, cancelled)` when cancellation is needed.
Cancellation is checked before reads and deliveries, and a failed sink stops
later reads. Treat cancelled or failed output as partial and publish only after
successful completion and any required source-revision checks.

The compiled routes currently require finite selected responses and full-rank
admitted designs, with no temporal weighting or other response transforms.
Other engines and policies return typed unsupported errors before image reads.
Fitted and residual series are unavailable through this executor; bounded trace
inspection is a separate capability. The existing full-fit API remains available
for other scientific models.

For numerical and performance details, continue to the [fit module](../modules/fit/README.md)
and [measured performance guide](benchmarks/selective-estimation.md). The compiled
selected path is currently a local development capability; an archived benchmark
or successful source build is not a published artifact or complete application
release.

Maintain the exact snippet/source link with:

```sh
python3 -S tools/docs/check_first_level_docs.py --check
sbt \
  'fitJVM/testOnly scalafim.examples.SelectedEstimatesExampleSuite' \
  'fitJS/testOnly scalafim.examples.SelectedEstimatesExampleSuite'
```
