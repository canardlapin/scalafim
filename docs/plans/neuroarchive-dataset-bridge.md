# NeuroArchive–Dataset Bridge

Status: implemented and verified
Tracker: `bd-01KY6EJNE45WFSAFMNS14V900E`

Verification at the implemented tip:

- `sbt compileAll` passed warning-clean;
- `sbt testAll` passed across the complete JVM and Scala.js matrix;
- focused bridge gates passed with dataset 75 JVM / 49 JS tests and
  dataset-zarr 15 JVM / 7 JS tests;
- downstream model, fit, MVPA-dataset, and group JVM/JS gates passed;
- the source-blind LNA and Zarr reads and the cross-session
  coordinate-window workflow all have executable coverage.

## Decision

NeuroArchive and `FmriDataset` connect by composing the storage adapters that
already exist. The bridge does not introduce a parallel error algebra,
selection model, study index, acquisition wrapper, provenance type parameter,
or read-capability hierarchy.

The target is deliberately small:

```text
NeuroArchive Zarr
  -> OpenedCanonicalBold
  -> ZarrResponseBlockSource
  -> ResponseBlockDatasetBackend
  -> FmriDataset

LNA dataset
  -> selected LnaArchive run
  -> LatentResponseDatasetBackend | LatentArchiveDatasetBackend
  -> FmriDataset
```

Both paths return:

```scala
Either[DatasetError, FmriDataset]
```

After opening, reading is source-blind:

```scala
dataset.seriesEither(selection)
```

The bridge adds only three new capabilities:

1. small source composers that fold native failures into `DatasetError`;
2. provenance carried as a value with dataset metadata and therefore into
   `FmriSeries`;
3. a segmented result algebra built from `DatasetIndex`, `RunKey`, and
   `DatasetRunPartition`.

## 1. Reuse the Existing Dataset Algebra

The following existing types remain canonical:

- `DatasetError` for every dataset-facing failure;
- `FmriDataset` for an attached backend, sampling frame, events, and derived
  time axis;
- `DataSelection`, `TimepointSelection`, and `VoxelSelection` for reads;
- `DatasetIndex`, `DatasetRun`, `DatasetRunQuery`, and
  `DatasetFieldCriterion` for study-scale lookup;
- `DatasetTimeAxis` and `DatasetRunPartition` for multi-run temporal
  structure;
- `DatasetMetadata` for information that follows the dataset into selected
  series.

The bridge must not add equivalents named `LoadError`, `FmriAcquisition`,
`DatasetHandle[P]`, `OpenedFmriDataset[P]`, `DatasetReadCapability`,
`StudyIndex`, `DatasetUnit`, `TemporalSelection`, `SpatialSelection`,
`DatasetProjection`, or `DatasetLevel`.

### 1.1 One Error Channel

Source adapters translate native errors at their boundary:

```text
StoreError / NeuroArchiveZarrError -> DatasetError.StorageFailure
ArchiveError                       -> DatasetError.ArchiveFailure
LatentError                        -> DatasetError.LatentFailure
LnaDatasetLookupError              -> existing DatasetError cases
SamplingFrameError                 -> DatasetError.InvalidTimeAxis
```

The exact mapping is tested. Callers never repeat procedural
`.left.map(...)` plumbing for each open step.

No new generic `DatasetOpenError[E]` is introduced.

### 1.2 Checked `FmriDataset` Construction Without an Intermediate

Checked construction remains necessary, but `FmriAcquisition` does not.
`FmriDataset.open` accepts the irreducible inputs and derives
`DatasetTimeAxis` internally:

```scala
final class FmriDataset private (
    val backend: DatasetBackend,
    val samplingFrame: SamplingFrame,
    val events: DatasetEvents,
    val timeAxis: DatasetTimeAxis
)

object FmriDataset:
  def open(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId],
      events: DatasetEvents = DatasetEvents.Empty
  ): Either[DatasetError, FmriDataset]

  def unsafe(
      backend: DatasetBackend,
      samplingFrame: SamplingFrame,
      runIds: Vector[RunId],
      events: DatasetEvents = DatasetEvents.Empty
  ): FmriDataset
```

There is no public case-class constructor, generated `copy`, independently
supplied `DatasetTimeAxis`, or separate attachment-error ADT.

`open`:

1. derives `DatasetTimeAxis.fromSamplingFrame(samplingFrame, runIds)`;
2. validates event run referential integrity;
3. validates backend timepoints against the frame;
4. returns the existing appropriate `DatasetError`.

`unsafe` delegates to `open` and throws only at an explicit trust boundary.

Built-in archive-path backends receive checked `make` factories so evaluating
their frozen shape and voxel domain cannot unexpectedly throw during the
normal open path. `open` is total for attachment compatibility once it receives
a well-formed backend value; it does not attempt to catch arbitrary exceptions
thrown by a third-party `DatasetBackend` implementation while evaluating its
own members.

### 1.3 Regular Sampling Sugar

Add only the common single-block convenience:

```scala
object SamplingFrame:
  def regular(
      tr: Double,
      nScans: Int
  ): Either[SamplingFrameError, SamplingFrame]

  def regular(
      tr: Double,
      nScans: Int,
      startTime: Double,
      precision: Double = 0.1
  ): Either[SamplingFrameError, SamplingFrame]
```

Both overloads delegate to `SamplingFrame.validated` with one block. The
two-argument form preserves the established `TR / 2` default; the explicit
origin form is used by storage refinements, including an exact Zarr origin of
zero. Neither creates a second timing representation.

## 2. Source Composers

### 2.1 NeuroArchive Zarr

The public JVM composer lives in `dataset-zarr`, the lowest module that can see
both `dataset` and `archive-zarr`.

An extension on the existing factory preserves the concise call:

```scala
import scalafim.dataset.zarr.*

FmriDataset.openZarr(
  root = root,
  run = RunId("run-01")
)
```

Normative signature:

```scala
extension (factory: FmriDataset.type)
  def openZarr(
      root: Path,
      run: RunId,
      options: ZarrDatasetOpenOptions = ZarrDatasetOpenOptions()
  ): Either[DatasetError, FmriDataset]
```

Options expose existing operational controls without changing the common
dataset interface:

```scala
final case class ZarrDatasetOpenOptions(
    precision: SamplingPrecisionPolicy =
      SamplingPrecisionPolicy.Default,
    voxelDomain: Option[VoxelDomain] = None,
    metadata: DatasetMetadata = DatasetMetadata.Empty,
    readLimits: ReadLimits = ReadLimits()
)
```

The composer:

1. opens `JvmFileStore`;
2. validates `OpenedCanonicalBold`;
3. refines regular manifest timing into one `SamplingFrame`;
4. opens `ZarrResponseBlockSource` with `ReadLimits`;
5. creates a checked `ResponseBlockDatasetBackend`;
6. attaches it with `FmriDataset.open`;
7. adds Zarr provenance to dataset metadata.

No timing argument is accepted because NeuroArchive Zarr owns normative
acquisition timing.

#### Timing refinement

The shared pure refinement returns `SamplingFrame`, not an acquisition wrapper:

```scala
object CanonicalBoldSampling:
  def refine(
      timing: AcquisitionTiming,
      policy: SamplingPrecisionPolicy
  ): Either[DatasetError, SamplingFrame]
```

It:

- accepts regular timing only;
- normalizes seconds and milliseconds before validation;
- preserves origin zero exactly rather than invoking the `TR / 2` default;
- rejects negative origins, non-positive steps, and count overflow;
- applies the explicit serializable precision policy;
- maps refinement failures to `DatasetError.InvalidTimeAxis`.

The synchronous composer remains JVM-only. Browser Zarr stays asynchronous and
does not receive a fake blocking facade.

### 2.2 LNA

The public JVM composer lives in `dataset`, where the existing LNA integration
already lives:

```scala
for
  query <- LnaDatasetQuery.fromStrings(
    subject = "01",
    session = Some("01"),
    task = Some("rest"),
    run = Some("01"),
    space = Some("MNI152NLin2009cAsym")
  )
  timing <- SamplingFrame.regular(tr = 0.8, nScans = 600)
  dataset <- FmriDataset.openLna(
    root = root,
    query = query,
    timing = timing
  )
yield dataset
```

Normative signatures:

```scala
extension (factory: FmriDataset.type)
  def openLna(
      root: Path,
      query: LnaDatasetQuery,
      timing: SamplingFrame,
      events: DatasetEvents = DatasetEvents.Empty
  ): Either[DatasetError, FmriDataset]

  def openLna(
      root: Path,
      query: LnaDatasetQuery,
      timing: SamplingFrame,
      archiveRun: RunLabel,
      events: DatasetEvents
  ): Either[DatasetError, FmriDataset]
```

The overload without `archiveRun` requires the selected archive to contain
exactly one internal run. The overload with `archiveRun` requires an exact
label. Neither overload silently selects `RunLabel.indexed(0)`.

The external dataset run id comes from the resolved LNA file entity for the
normal single-block path. A multi-block timing frame requires an explicit
`runIds: Vector[RunId]` overload; no ids are fabricated.

LNA requires `SamplingFrame` because its archive schema has no normative
acquisition timing. This asymmetry is visible in the signatures:

```text
openZarr(root, run)          // timing derived; supplying it is impossible
openLna(root, query, timing) // timing required; omission is impossible
```

The composer chooses the best existing backend:

- `LatentResponseDatasetBackend` when selection-aware reconstruction is
  available;
- `LatentArchiveDatasetBackend` when reconstruction necessarily falls back to
  the selected archive run;
- materialization only through a separately named explicit operation.

The common read API does not expose a new capability enum. Source-specific
diagnostics and post-read accounting remain source metadata or source receipts,
not downstream control types.

## 3. Provenance as a Value

Provenance must follow a dataset into every selected `FmriSeries`, but it does
not parameterize the dataset or index types.

Extend `DatasetMetadata` with one optional value:

```scala
trait DatasetProvenance:
  def source: String

final class DatasetMetadata private (
    val typedValues: Map[DatasetFieldId, DatasetValue],
    val provenance: Option[DatasetProvenance]
)
```

`DatasetProvenance` is intentionally an extensible value contract because the
module graph prevents `dataset` from naming Zarr-specific classes:

```text
dataset-zarr -> dataset
dataset      -X-> dataset-zarr
```

Concrete values live beside their adapters:

```scala
final case class LnaDatasetProvenance(
    archivePath: String,
    archiveRun: RunLabel,
    codecFamily: String,
    externalBasis: Option[String]
) extends DatasetProvenance:
  val source = "lna"

final case class NeuroArchiveZarrProvenance(
    acquisitionId: String,
    payloadId: String,
    contentRevision: String,
    logicalPayloadHash: String,
    precisionPolicy: SamplingPrecisionPolicy
) extends DatasetProvenance:
  val source = "neuroarchive-zarr"
```

This is ordinary value polymorphism, not a type parameter threaded through
every consumer. `DatasetMetadata` already flows backend -> `FmriDataset` ->
`FmriSeries`, so no `OpenedDataset` wrapper is required.

In-memory datasets may use a small `MemoryDatasetProvenance` value or leave
provenance absent. Built-in archive composers always attach provenance.

Zarr's existing `ExecutionReceipt` remains the authority for actual read
accounting. It is not duplicated as a speculative dataset capability.

## 4. Keep `DatasetIndex`, Fix Its Run Invariant

`DatasetIndex` remains the study query mechanism. No `StudyId`, `StudyIndex`,
`DatasetUnit`, `DatasetUnitQuery`, or projection hierarchy is introduced.

One existing seam must be made explicit: `DatasetRun` is named and keyed as one
run, while an arbitrary `FmriDataset` may contain several time blocks.

The checked invariant is:

```text
DatasetRun(key, dataset) identifies exactly one DatasetTimeAxis block,
and key.run equals that block's RunId.
```

The representation therefore becomes a checked view rather than an unchecked
pair:

```scala
final class DatasetRun private (
    val key: RunKey,
    val dataset: FmriDataset,
    val block: DatasetTimeBlock
)
```

For a single-run dataset, construction is direct. For a multi-run dataset,
`DatasetRun.fromDataset(datasetKey, dataset)` expands its time-axis blocks into
multiple lightweight `DatasetRun` views sharing the same underlying dataset.
No response data is copied.

Each run view translates run-local `DataSelection.time` indices to the
underlying dataset's global indices before reading. Its resulting
`DatasetRunPartition` records output rows, global timepoints, and run-local
timepoints.

`DatasetIndex.fromRuns` validates the invariant in addition to its existing
empty-index and duplicate-key checks.

The existing query vocabulary remains unchanged:

```scala
val query = DatasetRunQuery(
  subject = Some(SubjectId("01")),
  session = DatasetFieldCriterion.Any,
  task = DatasetFieldCriterion.Is(TaskId("rest")),
  space = DatasetFieldCriterion.Is(
    SpaceId("MNI152NLin2009cAsym")
  )
)

index.resolveEither(query)
```

## 5. Selection Extensions, Not a Parallel Selection Model

### 5.1 Coordinate Selection

Extend the existing spatial selection:

```scala
enum VoxelSelection:
  case All
  case AllSpatial
  case Indices(values: Vector[VoxelIndex])
  case Coords(values: Vector[VoxelCoord])
```

`DataSelection` remains the only read-selection record.

For one dataset, `Coords` resolves through its `NeuroSpace` and voxel domain.
For an indexed cross-run read, coordinate selection has exact-grid semantics:
it additionally requires identical geometry across selected runs and readable
membership for every requested coordinate. Equal `SpaceId` labels or equal
integer triples alone are not enough.

Non-exact alignment is deferred until it can accept an explicit transform or
resampling plan from `spatial`; no one-case alignment enum is introduced in
advance of that capability.

### 5.2 Windows

Windows remain selections rather than hierarchy nodes. Add a checked range to
the existing temporal selection:

```scala
enum TimepointSelection:
  case All
  case Indices(values: Vector[TimepointIndex])
  case Window(start: TimepointIndex, length: Int)
```

`Window` validates positive length and bounds when resolved. On a
`DatasetRun` view it is run-local, just like `Indices`; the run view performs
the global translation. Repeating a window across sessions means applying the
same `DataSelection` to several resolved `DatasetRun` values.

Windows may overlap. No window owns timepoints or crosses a run boundary.

## 6. Segmented Results

Add one result type over existing run keys and partitions:

```scala
final class FmriSeriesSegment private (
    val key: RunKey,
    val partition: DatasetRunPartition,
    val series: FmriSeries
)

final class SegmentedFmriSeries private (
    val segments: Vector[FmriSeriesSegment]
)
```

`DatasetIndex.read` is the single study-scale composition:

```scala
def read(
    query: DatasetRunQuery,
    selection: DataSelection = DataSelection.All
): Either[DatasetError, SegmentedFmriSeries]
```

It resolves the existing query, validates cross-run coordinate compatibility,
reads each run view, and preserves canonical index order.

The result owns three deliberately distinct operations.

### 6.1 Reorganize

Use ordinary typed functions rather than a new hierarchy-level enum:

```scala
def groupBy[K](
    key: FmriSeriesSegment => K
): Map[K, SegmentedFmriSeries]
```

Example:

```scala
series.groupBy(_.key.dataset.session)
```

No values or segment boundaries change.

### 6.2 Relayout

```scala
def blockConcatenate:
  Either[DatasetError, BlockConcatenatedFmriSeries]

final class BlockConcatenatedFmriSeries private (
    val series: FmriSeries,
    val segments: Vector[FmriSeriesSegment],
    val boundaries: Vector[BlockSegmentBoundary]
)
```

Concatenation changes physical layout but retains every observation, source
segment, run-local coordinate, and provenance value. It never fabricates a
continuous elapsed-time axis across runs or sessions. The concatenated
`FmriSeries` uses a physical row axis `0 .. nRows - 1`; `boundaries` and the
original segment partitions remain the authority for run/session coordinates.

### 6.3 Change Values

Reduction is explicitly caller-supplied:

```scala
def reduceBy[K, A](
    key: FmriSeriesSegment => K
)(
    reduce: SegmentedFmriSeries => Either[DatasetError, A]
): Either[DatasetError, Map[K, A]]
```

Example:

```scala
series.reduceBy(_.key.dataset.session): session =>
  SessionSummary.mean(session)
```

The dataset module does not invent a universal `Mean` policy for potentially
misaligned time series.

## 7. End-to-end Examples

### 7.1 Source-blind selection

```scala
def selectedSignal(
    dataset: FmriDataset
): Either[DatasetError, FmriSeries] =
  dataset.seriesEither(
    DataSelection(
      time = TimepointSelection.indices(100, 101, 102, 103),
      voxels = VoxelSelection.indices(4811, 4812, 9007)
    )
  )
```

The same function accepts a Zarr-backed, LNA-backed, or in-memory dataset.

### 7.2 One voxel across sessions

```scala
val selected =
  index.read(
    query = DatasetRunQuery(
      subject = Some(SubjectId("01")),
      session = DatasetFieldCriterion.Any,
      task = DatasetFieldCriterion.Is(TaskId("rest")),
      space = DatasetFieldCriterion.Is(
        SpaceId("MNI152NLin2009cAsym")
      )
    ),
    selection = DataSelection(
      voxels = VoxelSelection.Coords(
        Vector(VoxelCoord(32, 41, 18))
      )
    )
  )

val bySession =
  selected.map(_.groupBy(_.key.dataset.session))

val algorithmInput =
  selected.flatMap(_.blockConcatenate)
```

The default remains segmented. Flattening is requested afterward and retains
its segment table.

### 7.3 Same window across runs

```scala
index.read(
  query = query,
  selection = DataSelection(
    time = TimepointSelection.Window(
      start = TimepointIndex.unsafe(40),
      length = 20
    ),
    voxels = VoxelSelection.Coords(
      Vector(VoxelCoord(32, 41, 18))
    )
  )
)
```

The window is interpreted locally by each selected `DatasetRun` view.

## 8. Tests

### 8.1 Shared dataset tests on JVM and Scala.js

- `FmriDataset.open` derives exactly one time axis from frame and run ids;
- backend/frame mismatch and invalid events return `DatasetError`;
- no public case-class constructor or `copy` remains;
- `SamplingFrame.regular` delegates to existing validation;
- `DatasetRun` rejects a key that does not match its time-axis block;
- multi-run datasets expand into zero-copy indexed run views;
- run-local selections translate to the correct global indices;
- coordinate selections validate bounds and voxel-domain membership;
- windows validate length and bounds;
- segmented reads preserve index order, run keys, partitions, and provenance;
- `groupBy` preserves values and segments;
- `blockConcatenate` preserves all observations and mandatory boundaries;
- `reduceBy` invokes the supplied reduction per exact group.

### 8.2 LNA JVM tests

- unique archive run opens without a label;
- multiple archive runs require an exact label;
- no run-zero default remains;
- timing is mandatory and preserved;
- missing or ambiguous file queries fold into `DatasetError`;
- selection-aware and whole-run existing backends both expose the same
  source-blind `seriesEither` contract;
- LNA provenance reaches the selected `FmriSeries`.

### 8.3 Zarr shared and JVM tests

- seconds and milliseconds refine identically;
- zero and nonzero origins are preserved;
- negative origin, explicit timing, overflow, and invalid precision fail as
  `DatasetError`;
- `ReadLimits` reach `ZarrResponseBlockSource`;
- selected read order remains stable;
- Zarr provenance reaches the selected `FmriSeries`;
- existing `ExecutionReceipt` remains authoritative for physical read
  accounting.

### 8.4 Downstream gates

```text
datasetJVM/test
datasetJS/test
datasetZarrJVM/test
datasetZarrJS/test
modelJVM/test modelJS/test
fitJVM/test fitJS/test
mvpaDatasetJVM/test mvpaDatasetJS/test
groupJVM/test groupJS/test
testAll
```

## 9. Implementation Slices

### Slice A: checked construction

- private non-case `FmriDataset`;
- direct checked `open` and named `unsafe`;
- derived `DatasetTimeAxis`;
- checked built-in backend factories;
- `SamplingFrame.regular`;
- constructor-site migration.

### Slice B: source composers and provenance

- provenance value in `DatasetMetadata`;
- JVM LNA composer in `dataset`;
- shared Zarr timing refinement;
- JVM Zarr composer in `dataset-zarr`;
- adapter error folding into `DatasetError`.

### Slice C: indexed run views and selection extensions

- enforce the `DatasetRun` single-block invariant;
- zero-copy expansion of multi-run datasets;
- run-local selection translation;
- `VoxelSelection.Coords`;
- `TimepointSelection.Window`.

### Slice D: segmented algebra

- `DatasetIndex.read`;
- `SegmentedFmriSeries`;
- function-based `groupBy`;
- boundary-preserving `blockConcatenate`;
- caller-supplied `reduceBy`.

### Slice E: scenarios and verification

- LNA and Zarr source-blind read scenarios;
- cross-session coordinate/window scenario;
- documentation and module relationship updates;
- downstream cross-platform gates;
- `testAll`.

Each slice requires a fresh mote preflight and exact-path reservation.

## 10. Implemented Decisions

The implementation follows these choices:

1. All public openers return `Either[DatasetError, FmriDataset]`.
2. `FmriDataset.open` derives its time axis directly; there is no
   `FmriAcquisition`.
3. Provenance is a value in `DatasetMetadata`, not a type parameter.
4. There is no common read-capability ADT.
5. LNA requires timing and never defaults its internal archive run.
6. Zarr derives timing and does not accept caller timing.
7. `DatasetIndex` remains the study query mechanism.
8. An indexed `DatasetRun` identifies one time-axis block; multi-run datasets
   expand into zero-copy run views.
9. `DataSelection` remains the only selection record.
10. Segmentation is the default; grouping, block concatenation, and reduction
    remain three different operations.
