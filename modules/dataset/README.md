# scalafim-dataset

Cross-compiled JVM/Scala.js dataset module for `scalafim`.

Package root:

```scala
import scalafim.dataset.*
```

This module is the rewrite target for `fmridataset`. It defines pure dataset
descriptions, typed identities, spatial/temporal selections, checked response
attachment, and fMRI series. It deliberately does not emulate R S3 dispatch.

## Archive and dataset roles

`FmriDataset` is a pure scientific description and query authority. It owns
shape, sampling, run hierarchy, events, metadata, and voxel-domain identity,
but it does not own an effect or a storage backend.

```text
FmriDataset + DatasetRunQuery + DataSelection
  -> pure run-local response selections

FmriDataset + ResponseSource[F] + AcquisitionContext
  -> checked OpenedDataset[F]
  -> effectful DatasetReadResult
```

`OpenedDataset[F]` validates timing, schema, geometry or topology references,
mask identity, sample ordering, calibration, and non-finite policy before it
can read. It lowers only to output-domain `ResolvedResponseSelection`; dataset
code never addresses latent coefficients. `DatasetReadResult` retains the
source `ReadResult`, including physical-read evidence. Its response provenance
retains every source node and derives one attachment root that directly
references every source root, so dataset adaptation is visible without
discarding the archive or representation history.

`SynchronousFmriDataset` is the explicit facade for an actual
synchronous `DatasetBackend`. Asynchronous and Scala.js sources have no
blocking adapter. New synchronous consumers should accept a
`DatasetSeriesReader`; effectful consumers should accept `OpenedDataset[F]`.

Pure checked construction is available without a reader:

```scala
val description: Either[DatasetError, FmriDataset] =
  FmriDataset.describe(
    id = datasetId,
    shape = shape,
    voxelDomain = voxelDomain,
    metadata = metadata,
    samplingFrame = samplingFrame,
    runIds = Vector(RunId("run-01")),
    events = events
  )
```

For a synchronous backend, `FmriDataset.open` returns a
`SynchronousFmriDataset`. `FmriDataset.unsafe` is its explicit trusted-input
constructor.

Default voxel selection is backend-readable, not blindly full-volume. A backend
exposes a typed `VoxelDomain`: dense full-volume backends make every spatial
voxel readable, while masked latent backends expose only active mask voxels by
default and preserve their global voxel indices in `FmriSeries`. Use
`VoxelSelection.AllSpatial` only when the caller is intentionally requesting the
full spatial volume and is prepared for masked backends to reject inactive
voxels.

Multi-run structure is represented in shared code with typed keys and an index:
`DatasetKey` captures subject/session/task/space identity, `RunKey` adds the
run, and `DatasetIndex` validates unique `DatasetRun` views. One
`FmriDataset` may contain several temporal blocks; `DatasetRun.fromDataset`
expands them into zero-copy, run-local views. `DatasetRunQuery`
resolves runs by typed criteria instead of raw string filters; file-format and
BIDS/LNA discovery logic should adapt into these keys at IO boundaries.
Within each `FmriDataset`, `DatasetTimeAxis` maps global timepoints to typed
run ids and run-local indices. `FmriDataset.runPartitions(...)` returns
dataset-backed selected-row partitions for runwise engines and censored
subselections without reconstructing run structure from raw `blockLens`.

Study hierarchy is therefore represented without a second tree API:

```text
study (DatasetIndex)
  -> subject/session/task/space (DatasetKey fields)
    -> run (DatasetRun)
      -> window (run-local TimepointSelection)
        -> timepoint (selected series row)
```

Indexed reads preserve each matching `(dataset key, run)` as a segment:

```scala
import scalafim.dataset.*
import scalafim.image.VoxelCoord

val selected: Either[DatasetError, SegmentedFmriSeries] =
  index.read(
    readers = synchronousReaders,
    query = DatasetRunQuery(
      subject = Some(SubjectId("01")),
      session = DatasetFieldCriterion.Any,
      task = DatasetFieldCriterion.Is(TaskId("rest")),
      space = DatasetFieldCriterion.Is(SpaceId("MNI152NLin2009cAsym"))
    ),
    selection = DataSelection(
      time = TimepointSelection.unsafeWindow(start = 40, length = 20),
      voxels = VoxelSelection.coords(VoxelCoord(32, 41, 18))
    )
  )

val bySession =
  selected.map(_.groupBy(_.key.dataset.session)) // reorganize only

val algorithmRows =
  selected.flatMap(_.blockConcatenate)           // relayout, keep boundaries

val summaries =
  selected.flatMap(
    _.reduceBy(_.key.dataset.session)(summarize)  // caller changes values
  )
```

`DatasetIndex` remains pure. Its read methods require an explicit immutable
`SynchronousDatasetReaders` registry; constructing an index does not smuggle a
reader into each run description.

Segmentation is the default. `blockConcatenate` never claims that observations
from different runs or sessions form one elapsed-time axis; its `boundaries`
retain the original run keys, partitions, and local timepoints.

Event and metadata ingestion keeps untyped string maps at the boundary but stores
typed values in shared code. `DatasetEventRow` parses reserved fields such as
`onset`, `duration`, `run`, `session`, and `condition` into typed accessors,
`DatasetEvents` validates rectangular event tables and run labels against the
dataset `DatasetTimeAxis`, and model-building code consumes typed event values
instead of reparsing `Map[String, String]` rows.

Core backends include `InMemoryDatasetBackend`, bounded NIfTI response sources,
and caller-defined implementations of the neutral reader contracts.

LNA discovery, archive-backed and latent-response backends, and
archive-receipt adaptation now live physically in
`interop-archived-response`. Their `scalafim.dataset` package names remain
available to applications that depend on that interop artifact, but the
dataset artifact itself neither imports nor dispatches on archive or
representation types.

Study-scale readers can implement `ResponseBlockSource`, a smaller bounded-read
contract over resolved timepoint and voxel selections.
`CompositeResponseBlockSource` combines run sources without concatenating their
payloads and preserves requested global timepoint and voxel order.
`ResponseBlockDatasetBackend` adapts that seam back into the existing model/fit
path.

On the JVM, `NiftiResponseBlockSource` performs positional reads from
uncompressed NIfTI data. Selected voxels are sorted into bounded byte windows;
contiguous spans and small gaps are coalesced, each window is read once per
requested timepoint into a reusable buffer, and decoded values are scattered
back into the caller's requested order. The response matrix adopts its final
primitive row-major buffer without boxing or copying through nested vectors.
Compressed `.nii.gz` inputs are never treated as random-access: callers must
provide a `NiftiStagingCache`, which expands them to a metadata-keyed cache entry
through a temporary file and atomic finalization. Failed staging leaves no
discoverable partial artifact.

The deterministic planner tests guard window/read counts and buffer bounds. An
opt-in representative 200-TR by 50k-voxel benchmark is available with:

```sh
sbt "datasetJVM/Test/runMain scalafim.dataset.io.NiftiReadBenchmark"
```

Run it directly with:

```sh
sbt datasetJVM/test
sbt datasetJS/test
```
