# scalafim-dataset-zarr

`dataset-zarr` adapts immutable NeuroArchive Zarr revisions to Scalafim's
bounded `ResponseBlockSource`. Ordered timepoint/voxel selections lower to
rank-four canonical points, execute through the generic Zarr planner, apply
stored-scalar calibration, and return `FmriSeries` without materializing the
whole run.

On the JVM, the high-level bridge derives regular acquisition timing from the
NeuroArchive manifest and returns the same `FmriDataset` interface used by LNA
and in-memory backends:

```scala
import scalafim.dataset.*
import scalafim.dataset.zarr.*

val opened: Either[DatasetError, FmriDataset] =
  FmriDataset.openZarr(
    root = revision,
    run = RunId("run-01"),
    options = ZarrDatasetOpenOptions(
      metadata = DatasetMetadata(Map("study" -> "demo"))
    )
  )

val selected =
  opened.flatMap(
    _.seriesEither(
      DataSelection(
        time = TimepointSelection.indices(100, 101, 102, 103),
        voxels = VoxelSelection.indices(4811, 4812, 9007)
      )
    )
  )
```

The opener accepts `ReadLimits` through `ZarrDatasetOpenOptions`. It accepts no
caller-supplied timing: Zarr timing is normative, while irregular timing and
negative regular origins are rejected as typed `DatasetError`s. The synchronous
composer is JVM-only; browser-backed Zarr remains honestly asynchronous.
Publication identity is carried as `NeuroArchiveZarrProvenance` in dataset
metadata, while Zarr execution receipts remain the authority for physical read
accounting.

The JVM side provides streaming raw-scalar NIfTI import, direct or measured
start-indexed sharded publication, and raw-scalar- and affine-preserving
BIDS/NIfTI export within the documented NeuroArchive 0.1 subset. The shared
selection and BIDS identity logic remains available on Scala.js; file-format
IO stays at the JVM boundary.

The extraction matrix covers `uint8`, `int16`, `int32`, `float32`, and
`float64`. Zarr-Python opens each published canonical array and verifies raw
`t,z,y,x` values before nibabel verifies the BIDS/NIfTI export. An independent
hard gate runs the official BIDS validator over every exported tree; the
internal BIDS loader is not treated as compliance evidence. Full preservation
of qform/qfac/intent and arbitrary NIfTI header provenance remains explicitly
outside the current 0.1 claim.

NIfTI repetition times expressed in seconds, milliseconds, or microseconds
are normalized to canonical seconds. Frequency-domain temporal unit codes are
refused for BOLD acquisitions instead of being silently interpreted as time.
