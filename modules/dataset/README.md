# scalafim-dataset

Cross-compiled JVM/Scala.js dataset module for `scalafim`.

Package root:

```scala
import scalafim.dataset.*
```

This module is the rewrite target for `fmridataset`. It defines dataset shapes,
typed identifiers, spatial/temporal selections, fMRI series, and storage-backend
contracts. It deliberately does not emulate R S3 dispatch; new backends implement
a small typed `DatasetBackend` trait.

## Archive and dataset roles

`FmriDataset` is the readable fMRI view; NeuroArchive is a family of durable
storage formats. They compose rather than inherit from one another:

```text
LNA archive -> latent dataset backend ----+
                                         +-> FmriDataset -> FmriSeries
Zarr revision -> response-block backend --+
```

Once opened, ordinary consumers use `seriesEither` and do not branch on the
storage source. Source provenance remains available through
`dataset.metadata.provenance` and follows every selected `FmriSeries`.

Checked construction is canonical:

```scala
val dataset: Either[DatasetError, FmriDataset] =
  FmriDataset.open(
    backend = backend,
    samplingFrame = samplingFrame,
    runId = RunId("run-01"),
    events = events
  )
```

`FmriDataset.unsafe` is the explicit trusted-input compatibility path.

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

Segmentation is the default. `blockConcatenate` never claims that observations
from different runs or sessions form one elapsed-time axis; its `boundaries`
retain the original run keys, partitions, and local timepoints.

Event and metadata ingestion keeps legacy string maps at the boundary but stores
typed values in shared code. `DatasetEventRow` parses reserved fields such as
`onset`, `duration`, `run`, `session`, and `condition` into typed accessors,
`DatasetEvents` validates rectangular event tables and run labels against the
dataset `DatasetTimeAxis`, and model-building code consumes typed event values
instead of reparsing `Map[String, String]` rows.

Backends currently include:

- `InMemoryDatasetBackend` for dense time-by-voxel matrices.
- `LatentArchiveDatasetBackend` for existing archive-backed LNA payloads.
- `LatentResponseDatasetBackend` for direct `LatentResponse` reads; selected
  timepoints and voxels are decoded through the latent response without
  materializing a whole run.
- JVM-only `LnaDataset` directory adapter for neuroarchive-style
  `derivatives/lna` trees: subject discovery, metadata tables, shared-basis
  registries, exact parsed-entity `.lna.h5` lookup over
  subject/session/task/run/acq/space/desc, archive-backed backend creation,
  selection-aware latent reads for explicit, temporal, transport, BOLDZip, and
  shared-basis archives, and opt-in materialized reads that resolve external
  shared-basis archives through the dataset root.

The high-level LNA composer requires the timing that LNA does not store and
never silently chooses an internal archive run:

```scala
import scalafim.dataset.*
import scalafim.dataset.io.*
import scalafim.fmri.hrf.design.SamplingFrame

val opened =
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

val signal =
  opened.flatMap(
    _.seriesEither(
      DataSelection(
        time = TimepointSelection.indices(100, 101, 102, 103),
        voxels = VoxelSelection.indices(4811, 4812, 9007)
      )
    )
  )
```

If the selected LNA contains several internal runs, use the overload with an
explicit `archiveRun: RunLabel`. A multi-block sampling frame likewise requires
explicit external `runIds`.

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
